package Regexp::Grammars;

use warnings;
use strict;
use 5.010;

use Scalar::Util qw< blessed >;
use Data::Dumper qw< Dumper  >;

our $VERSION = 1.001_005;

# Load the module...
sub import {
    # Signal lexical scoping (active, unless something was exported)...
    $^H{'Regexp::Grammars::active'} = 1;

    # Process any regexes in module's active lexical scope...
    use overload;
    overload::constant(
        qr => sub {
            my ($raw, $cooked, $type) = @_;

            # In active scope and really a regex...
            if (_module_is_active() && $type =~ /qq?/) {
                return bless \$raw, 'Regexp::Grammars::Precursor';
            }
            # Ignore everything else...
            else {
                return $cooked;
            }
        }
    );
}

# Deactivate module's regex effect when it is "anti-imported" with 'no'...
sub unimport {
    # Signal lexical (non-)scoping...
    $^H{'Regexp::Grammars::active'} = 0;
}

# Tidy up the hoopy user-defined pragma interface...
sub _module_is_active {
    return (caller 1)[10]->{'Regexp::Grammars::active'};
}


#=====[ COMPILE-TIME INTERIM REPRESENTATION OF GRAMMARS ]===================
{
    package Regexp::Grammars::Precursor;

    # Only translate precursors once...
    state %grammar_cache;

    use overload (
        # Concatenation/interpolation just concatenates to the precursor...
        q{.} => sub {
            my ($x, $y, $reversed) = @_;
            if (ref $x) { $x = ${$x} }
            if (ref $y) { $y = ${$y} }
            if ($reversed) { ($y,$x) = ($x,$y); }
            $x .= $y//q{};
            return bless \$x, 'Regexp::Grammars::Precursor';
        },

        # Using as a string (i.e. matching) preprocesses the precursor...
        q{""} => sub {
            my ($obj) = @_;
            use Scalar::Util qw< refaddr >;
            return $grammar_cache{ refaddr($obj) }
                //= Regexp::Grammars::_build_grammar( ${$obj} );
        },

        # Everything else, as usual...
        fallback => 1,
    );
}


#=====[ SUPPORT FOR THE INTEGRATED DEBUGGER ]=========================

# All messages go to STDERR by default...
*Regexp::Grammars::LOGFILE = *STDERR{IO};

# Debugging levels indicate where to stop...
our %DEBUG_LEVEL = (
    off  => 0,                               # No more debugging
    run  => 1,   continue  => 1,             # Run to completion of regex match
                 match     => 2,   on => 2,  # Run to next successful submatch
    step => 3,   try       => 3,             # Run to next reportable event
);

# Debugging levels can be abbreviated to one character during interactions...
@DEBUG_LEVEL{ map {substr($_,0,1)} keys %DEBUG_LEVEL } = values %DEBUG_LEVEL;
$DEBUG_LEVEL{o} = $DEBUG_LEVEL{off};

# Width of leading context field in debugging messages is constrained...
my $MAX_CONTEXT_WIDTH = 20;
my $MIN_CONTEXT_WIDTH = 6;

# Rewrite a string currently being matched, to make \n and \t visible
sub _show_metas {
    my $context_str = shift // q{};

    # Quote newlines (\n -> \\n, without using a regex)...
    my $index = index($context_str,"\n");
    while ($index >= 0) {
        substr($context_str, $index, 1, '\\n');
        $index = index($context_str,"\n",$index+2);
    }

    # Quote tabs (\t -> \\t, without using a regex)...
    $index = index($context_str,"\t");
    while ($index >= 0) {
        substr($context_str, $index, 1, '\\t');
        $index = index($context_str,"\t",$index+2);
    }

    return $context_str;
}

# Minimize whitespace in a string...
sub _squeeze_ws {
    my ($str) = @_;

    $str =~ tr/\n\t/ /;

    my $index = index($str,q{  });
    while ($index >= 0) {
        substr($str, $index, 2, q{ });
        $index = index($str,q{  },$index);
    }

    return $str;
}

# Prepare for debugging...
sub _init_try_stack {
    our (@try_stack, $last_try_pos, $last_context_str);

    # Start with a representation of the entire grammar match...
    @try_stack = ({
        subrule => '<grammar>',
        height  => 0,
        errmsg  => ' \\FAIL <grammar>',
    });

    # Initialize tracking of location and context...
    $last_try_pos     = -1;
    $last_context_str = q{};

    # Report...
    say {*Regexp::Grammars::LOGFILE} _debug_context('=>')
                . 'Trying <grammar> from position ' . pos();
}

# Create a "context string" showing where the regex is currently matching...
sub _debug_context {
    my ($fill_chars) = @_;

    # Determine minimal sufficient width for context field...
    my $field_width = length(_show_metas($_));
    if ($field_width > $MAX_CONTEXT_WIDTH) {
        $field_width = $MAX_CONTEXT_WIDTH;
    }
    elsif ($field_width < $MIN_CONTEXT_WIDTH) {
        $field_width = $MIN_CONTEXT_WIDTH;
    }

    # Get current matching position (and some additional trailing context)...
    my $context_str
        = substr(_show_metas(substr($_//q{},pos(),$field_width)),0,$field_width);

    # Build the context string, handling special cases...
    our $last_context_str;
    if ($fill_chars) {
        # If caller supplied a 1- or 2-char fill sequence, use that instead...
        my $last_fill_char = length($fill_chars) > 1
                                ? substr($fill_chars,-1,1,q{})
                                : $fill_chars
                                ;
        $context_str = $fill_chars x ($field_width-1) . $last_fill_char;
    }
    else { 
        # Make end-of-string visible in empty context string...
        if ($context_str eq q{}) {
            $context_str = '[eos]';
        }

        # Don't repeat consecutive identical context strings...
        if ($context_str eq $last_context_str) {
            $context_str = q{ } x $field_width;
        }
        else {
            # If not repeating, remember for next time...
            $last_context_str = $context_str;
        }
    }

    # Left justify and return context string...
    return sprintf("%-*s ",$field_width,$context_str);
}

# Show a debugging message (mainly used for compile-time errors and info)...
sub _debug_notify {
    # Single arg is a line to be printed with a null severity...
    my ($severity, @lines) = @_==1 ? (q{},@_) : @_;
    chomp @lines;

    # Formatting string for all lines...
    my $format = qq{%*s | %s\n};

    # Track previous severity and avoid repeating the same level...
    state $prev_severity = q{};
    if ($severity eq $prev_severity) {
        $severity = q{};
    }
    else {
        $prev_severity = $severity;
    }

    # Display first line with severity indicator (unless same as previous)...
    printf {*Regexp::Grammars::LOGFILE} $format, $MIN_CONTEXT_WIDTH, $severity, shift @lines;

    # Display first line without severity indicator
    for my $next_line (@lines) {
        printf {*Regexp::Grammars::LOGFILE} $format, $MIN_CONTEXT_WIDTH, q{}, $next_line;
    }
}

# Handle user interactions during runtime debugging...
sub _debug_interact {
    my ($stack_height, $leader, $curr_frame_ref, $min_debug_level) = @_;

    our $DEBUG; # ...stores current debug level within regex

    # Only interact with terminals, and if debug level is appropriate...
    if (-t *Regexp::Grammars::LOGFILE
    && defined $DEBUG
    && ($DEBUG_LEVEL{$DEBUG}//0) >= $DEBUG_LEVEL{$min_debug_level}) { 
        INPUT:
        while (1) {
            my $cmd = <>;
            chomp $cmd;

            # Input of 'd' means 'display current result frame'...
            if ($cmd eq 'd') {
                print {*Regexp::Grammars::LOGFILE} join "\n",
                    map { $leader . ($stack_height?'|   ':q{})
                        . '       : ' . $_
                        }
                        split "\n", q{ }x8 . substr(Dumper($curr_frame_ref),8);
                print "\t";
            }
            # Any other (valid) input changes debugging level and continues...
            else {
                if (exists $DEBUG_LEVEL{$cmd}) { $DEBUG = $cmd; }
                last INPUT;
            }
        }
    }
    # When interaction not indicated, just complete the debugging line...
    else {
        print {*Regexp::Grammars::LOGFILE} "\n";
    }
}
 
# Handle reporting of unsuccessful match attempts...
sub _debug_handle_failures {
    my ($stack_height, $subrule, $in_match) = @_;
    our @try_stack;

    # Unsuccessful match attempts leave "leftovers" on the attempt stack...
    CLEANUP:
    while (@try_stack && $try_stack[-1]{height} >= $stack_height) {
        # Grab record of (potentially) unsuccessful attempt...
        my $error_ref = pop @try_stack;

        # If attempt was the one whose match is being reported, go and report...
        last CLEANUP if $in_match 
                     && $error_ref->{height} == $stack_height
                     && $error_ref->{subrule} eq $subrule;

        # Otherwise, report the match failure...
        say {*Regexp::Grammars::LOGFILE} _debug_context(q{ }) . $error_ref->{errmsg};
    }
}

# Handle attempts to call non-existent subrules...
sub _debug_fatal {
    my ($naughty_construct) = @_;

    print {*Regexp::Grammars::LOGFILE} 
        "_________________________________________________________________\n",
        "Fatal error: Entire parse terminated prematurely while attempting\n",
        "             to call non-existent rule: $naughty_construct\n",
        "_________________________________________________________________\n";
    $@ = "Entire parse terminated prematurely while attempting to call non-existent rule: $naughty_construct";
}

# Print a <log:...> message in context...
sub _debug_logmsg {
    my ($stack_height, @msg) = @_;

    # Determine indent for messages...
    my $leader = _debug_context() . q{|   } x ($stack_height-1) . '|';
       
    # Report the attempt...
    print {*Regexp::Grammars::LOGFILE} map { "$leader$_\n" } @msg;
}

# Print a message indicating a (sub)match attempt...
sub _debug_trying {
    my ($stack_height, $curr_frame_ref, $subrule) = @_;

    # Clean up after any preceding unsuccessful attempts...
    _debug_handle_failures($stack_height, $subrule);

    # Determine indent for messages...
    my $leader = _debug_context() . q{|   } x ($stack_height-2);
       
    # Detect and report any backtracking prior to this attempt...
    our $last_try_pos;  #...Stores the pos() of the most recent match attempt?
    my $backtrack_distance = $last_try_pos - pos();
    if ($backtrack_distance > 0) {
        say {*Regexp::Grammars::LOGFILE} ' <' . q{~} x (length(_debug_context(q{ }))-3) . q{ }
                    . q{|   } x ($stack_height-2)
                    . qq{|...Backtracking $backtrack_distance char}
                    . ($backtrack_distance > 1 ? q{s} : q{})
                    . q{ and trying new match}
                    ;
    }

    # Report the attempt...
    print {*Regexp::Grammars::LOGFILE} $leader, "|...Trying $subrule\t";

    # Handle user interactions during debugging...
    _debug_interact($stack_height, $leader, $curr_frame_ref, 'step');

    # Record the attempt, for later error handling in _debug_matched()...
    our @try_stack;
    push @try_stack, {
        height  => $stack_height,
        subrule => $subrule,
        # errmsg should align under:              |...Trying $subrule\t
        errmsg  => q{|   } x ($stack_height-2) . "|    \\FAIL $subrule",
    };
    $last_try_pos = pos();
}

# Print a message indicating a successful (sub)match...
sub _debug_matched {
    my ($stack_height, $curr_frame_ref, $subrule, $text) = @_;

    # Clean up any intervening unsuccessful attempts...
    _debug_handle_failures($stack_height, $subrule, 'in match');

    # Build debugging message...
    my $debug_context = _debug_context();
    my $leader  = $debug_context . q{|   } x ($stack_height-2);
    my $message = ($stack_height ? '|   ' : q{})
                . " \\_____$subrule matched '";
    my $filler  = $stack_height
                    ? '|   ' . q{ } x (length($message)-4)
                    :          q{ } x  length($message);

    # Split multi-line match texts and indent them correctly...
    $text = join "\n$leader$filler", split "\n", $text;

    our $last_try_pos;  #...Stores the pos() of the most recent match attempt?

    # Report if match required backtracking...
    my $backtrack_distance = $last_try_pos - pos();
    if ($backtrack_distance > 0) {
        say {*Regexp::Grammars::LOGFILE} ' <' . q{~} x (length(_debug_context(q{ }))-3) . q{ }
                    . q{|   } x ($stack_height-2)
                    . qq{|...Backtracking $backtrack_distance char}
                    . ($backtrack_distance > 1 ? q{s} : q{})
                    . qq{ and rematching $subrule}
                    ;
    }
    $last_try_pos = pos();

    # Print match message...
    print {*Regexp::Grammars::LOGFILE} $leader . $message . $text . qq{'\t}; 

    # Check for user interaction...
    _debug_interact($stack_height, $leader, $curr_frame_ref,
                    $stack_height ?  'match' : 'run');
}

# Print a message indicating a successful (sub)match...
sub _debug_require {
    my ($stack_height, $condition, $succeeded) = @_;

    # Build debugging message...
    my $debug_context = _debug_context();
    my $leader  = $debug_context . q{|   } x ($stack_height-1);
    my $message1 = ($stack_height ? '|...' : q{})
                 . "Testing condition: $condition"
                 ;
    my $message2 = ($stack_height ? '|   ' : q{})
                 . " \\_____"
                 . ($succeeded ? 'Satisified' : 'FAILED')
                 ;

    # Report if match required backtracking...
    our $last_try_pos;
    my $backtrack_distance = $last_try_pos - pos();
    if ($backtrack_distance > 0) {
        say {*Regexp::Grammars::LOGFILE} ' <' . q{~} x (length(_debug_context(q{ }))-3) . q{ }
                    . q{|   } x ($stack_height-1)
                    . qq{|...Backtracking $backtrack_distance char}
                    . ($backtrack_distance > 1 ? q{s} : q{})
                    . qq{ and rematching}
                    ;
    }

    # Remember where the condition was tried...
    $last_try_pos = pos();

    # Print match message...
    say {*Regexp::Grammars::LOGFILE} $leader . $message1;
    say {*Regexp::Grammars::LOGFILE} $leader . $message2;
}

# Print a message indicating a successful store-result-of-code-block...
sub _debug_executed {
    my ($stack_height, $curr_frame_ref, $subrule, $value) = @_;

    # Build message...
    my $leader   = _debug_context() . q{|   } x ($stack_height-2);
    my $message  = "|...Action $subrule\n";
    my $message2 = "|   saved value: '";
    $message    .= $leader . $message2;
    my $filler   = q{ } x length($message2);

    # Split multiline results over multiple lines (properly indented)...
    $value = join "\n$leader$filler", split "\n", $value;

    # Report the action...
    print {*Regexp::Grammars::LOGFILE} $leader . $message . $value . qq{'\t}; 

    # Check for user interaction...
    _debug_interact($stack_height, $leader, $curr_frame_ref, 'match');
}

# Create the code to be inserted into the regex to facilitate debugging...
sub _build_debugging_statements {
    my ($debugging_active, $subrule, $extra_pre_indent) = @_;

    return (q{}, q{}) if ! $debugging_active;;

    $extra_pre_indent //= 0;

    $subrule = "q{$subrule}";

    return (
      qq{ Regexp::Grammars::_debug_trying(\@Regexp::Grammars::RESULT_STACK+$extra_pre_indent, \$Regexp::Grammars::RESULT_STACK[-2+$extra_pre_indent], $subrule)
            if \$Regexp::Grammars::DEBUG_LEVEL{\$Regexp::Grammars::DEBUG};
        },
      qq{ Regexp::Grammars::_debug_matched(\@Regexp::Grammars::RESULT_STACK+1, \$Regexp::Grammars::RESULT_STACK[-1], $subrule, \$^N)
            if \$Regexp::Grammars::DEBUG_LEVEL{\$Regexp::Grammars::DEBUG};
        },
    );
}


#=====[ SUPPORT FOR UPDATING THE RESULT STACK ]=========================

# Create a clone of the current result frame with an new key/value...
sub _extend_current_result_frame_with_scalar {
    my ($stack_ref, $key, $value) = @_;

    # Autovivify null stacks (only occur when grammar invokes no subrules)...
    if (!@{$stack_ref}) {
        $stack_ref = [{}];
    }

    # Copy existing frame, appending new value so it overwrites any old value...
    my $cloned_result_frame = {
        %{$stack_ref->[-1]},
        $key => $value,
    };

    # Make the copy into an object, if the original was one...
    if (my $class = blessed($stack_ref->[-1])) {
        bless $cloned_result_frame, $class;
    }

    return $cloned_result_frame;
}

# Create a clone of the current result frame with an additional key/value
# (As above, but preserving the "listiness" of the key being added to)...
sub _extend_current_result_frame_with_list {
    my ($stack_ref, $key, $value) = @_;

    # Copy existing frame, appending new value to appropriate element's list...
    my $cloned_result_frame = {
        %{$stack_ref->[-1]},
        $key => [
            @{$stack_ref->[-1]{$key}//[]},
            $value,
        ],
    };

    # Make the copy into an object, if the original was one... 
    if (my $class = blessed($stack_ref->[-1])) {
        bless $cloned_result_frame, $class;
    }

    return $cloned_result_frame;
}

# Pop current result frame and add it to a clone of previous result frame
# (flattening it if possible, and preserving any blessing)...
sub _pop_current_result_frame {
    my ($stack_ref, $key, $value) = @_;

    # Where are we in the stack?
    my $curr_frame   = $stack_ref->[-1];
    my $caller_frame = $stack_ref->[-2];

    # Track which frames are objects...
    my $is_blessed_curr   = blessed($curr_frame);
    my $is_blessed_caller = blessed($caller_frame);

    # Remove "private" captures (i.e. those starting with _)...
    delete @{$curr_frame}{grep {substr($_,0,1) eq '_'} keys %{$curr_frame} };

    # Nest a clone of current frame inside a clone of the caller frame...
    my $cloned_caller_frame = {
        %{$caller_frame},
        $key => exists $curr_frame->{'='}
                    ? $curr_frame->{'='}
                    : $is_blessed_curr || length(join(q{}, keys %{$curr_frame}))
                        ? { q{} => $value, %{$curr_frame} }
                        : keys %{$curr_frame}
                            ? $curr_frame->{q{}}
                            : $value
    };

    # Make the copies into objects, if the originals were... 
    if ($is_blessed_curr && !exists $curr_frame->{'='} ) {
        bless $cloned_caller_frame->{$key}, $is_blessed_curr;
    }
    if ($is_blessed_caller) {
        bless $cloned_caller_frame, $is_blessed_caller;
    }

    return $cloned_caller_frame;
}

# Pop current result frame and add it to a clone of previous result frame
# (flattening it if possible, and preserving any blessing)
# (As above, but preserving listiness of key being added to)...
sub _pop_current_result_frame_with_list {
    my ($stack_ref, $key, $value) = @_;

    # Where are we in the stack?
    my $curr_frame   = $stack_ref->[-1];
    my $caller_frame = $stack_ref->[-2];

    # Track which frames are objects...
    my $is_blessed_curr = blessed($curr_frame);
    my $is_blessed_caller = blessed($caller_frame);

    # Remove "private" captures (i.e. those starting with _)...
    delete @{$curr_frame}{grep {substr($_,0,1) eq '_'} keys %{$curr_frame} };

    # Append a clone of current frame inside a clone of the caller frame...
    my $cloned_caller_frame = {
            %{$caller_frame},
            $key => [
                @{$caller_frame->{$key}//[]},
                exists $curr_frame->{'='}
                    ? $curr_frame->{'='}
                    : $is_blessed_curr || length(join(q{}, keys %{$curr_frame}))
                        ? {
                                q{} => $value,
                                %{$curr_frame}
                        }
                        : keys %{$curr_frame}
                            ? $curr_frame->{q{}}
                            : $value
            ],
        };

    # Make the copies into objects, if the originals were... 
    if ($is_blessed_curr && !exists $curr_frame->{'='} ) {
        bless $cloned_caller_frame->{$key}[-1], $is_blessed_curr;
    }
    if ($is_blessed_caller) {
        bless $cloned_caller_frame, $is_blessed_caller;
    }

    return $cloned_caller_frame;
}


#=====[ MISCELLANEOUS CONSTANTS ]=========================

# This code inserted at the start of every grammar regex
#    (initializes the result stack cleanly and backtrackably, via local)...
my $PROLOGUE = q{((?{; @! = () if !pos;
                       local @Regexp::Grammars::RESULT_STACK
                           = (@Regexp::Grammars::RESULT_STACK, {});
                       local $Regexp::Grammars::DEBUG = 'off' }) };

# This code inserted at the end of every grammar regex
#    (grabs final result and stores it in %/. Also defines default <ws> rule)...
my $EPILOGUE = q{
    )(?{; $Regexp::Grammars::RESULT_STACK[-1]{""} //= $^N;
         local $Regexp::Grammars::match_frame = pop @Regexp::Grammars::RESULT_STACK;
         delete @{$Regexp::Grammars::match_frame}{
                    grep {substr($_,0,1) eq '_'} keys %{$Regexp::Grammars::match_frame}
                };
         if (@Regexp::Grammars::RESULT_STACK) {
            $Regexp::Grammars::RESULT_STACK[-1]{'(?R)'} = $Regexp::Grammars::match_frame;
         }
         */ = $Regexp::Grammars::match_frame;
    })(?(DEFINE)
        (?<ws>(?:\\s*))
        (?<hk>(?:\\S+))
    )
};


#=====[ MISCELLANEOUS PATTERNS THAT MATCH USEFUL THINGS ]========

# Match an identifier...
my $IDENT = q{[^\W\d]\w*+};

# Match balanced parentheses, taking into account \-escapes and []-escapes...
my $PARENS = qr{
    (?&PARENS)
    (?(DEFINE)
        (?<PARENS> \( (?: \\. | (?&PARENS) | (?&CHARSET) | [^][()\\]++)*+ \) )
        (?<CHARSET> \[ \^?+ \]?+ [^]]*+ \] )
    )
}xms;


#=====[ SUPPORT FOR TRANSLATING GRAMMAR-ENHANCED REGEX TO NATIVE REGEX ]====

my %REPETITION_DESCRIPTION_FOR = (
    '+'  => 'once or more',
    '*'  => 'any number of times',
    '?'  => 'if possible',
    '+?' => 'as few times as possible',
    '*?' => 'as few times as possible',
    '??' => 'if necessary',
    '++' => 'as many times as possible',
    '*+' => 'as many times as possible',
    '?+' => 'if possible',
);

sub _translate_raw_regex {
    my ($regex, $debug_build) = @_;

    my $is_comment = substr($regex, 0, 1) eq q{#};
    my $visible_regex = _squeeze_ws($regex);

    # Report how regex was interpreted, if requested to...
    if ($debug_build && $visible_regex ne q{} && $visible_regex ne q{ }) {
        _debug_notify( info =>
                           "   |",
                           "   |...Treating '$visible_regex' as:",
            ($is_comment ? "   |       \\ a comment (which will be ignored)"
                         : "   |       \\ normal Perl regex syntax"
            ),
        );
    }

    return $is_comment ? q{} : $regex;
}

# Report and convert a debugging directive...
sub _translate_debug_directive {
    my ($construct, $cmd, $debug_build) = @_;

    # Report how directive was interpreted, if requested to...
    if ($debug_build) {
        _debug_notify( info =>
            "   |",
            "   |...Treating $construct as:",
            "   |       \\ Change run-time debugging mode to '$cmd'",
        );
    }

    return qq{(?{; local \$Regexp::Grammars::DEBUG = q{$cmd}; }) };
}

# Report and convert a <require:...> directive...
sub _translate_require_directive {
    my ($construct, $condition, $debug_build) = @_;

    $condition = substr($condition, 3, -2);

    # Report how directive was interpreted, if requested to...
    if ($debug_build) {
        _debug_notify( info =>
            "   |",
            "   |...Treating $construct as:",
            "   |       \\ Require that {$condition} is true",
        );
    }

    my $quoted_condition = $condition;
    $quoted_condition =~ s{\$}{}xms;

    return qq{(?(?{;$condition})
        (?{;Regexp::Grammars::_debug_require(
            scalar \@Regexp::Grammars::RESULT_STACK, q{$quoted_condition}, 1)
                if \$Regexp::Grammars::DEBUG_LEVEL{\$Regexp::Grammars::DEBUG}})
      | (?{;Regexp::Grammars::_debug_require(
            scalar \@Regexp::Grammars::RESULT_STACK, q{$quoted_condition}, 0)
                if \$Regexp::Grammars::DEBUG_LEVEL{\$Regexp::Grammars::DEBUG}})(?!))
    };
}

# Report and convert a debugging directive...
sub _translate_error_directive {
    my ($construct, $type, $msg, $debug_build) = @_;

    # Determine severity...
    my $severity = ($type eq 'error') ? 'fatal' : 'non-fatal';

    # Unpack message...
    if (substr($msg,0,3) eq '(?{') {
        $msg = 'do'. substr($msg,2,-1);
    }
    elsif ($type ne 'log'
    &&     (lc(substr($msg,0,9) ) eq 'expected '
    ||      lc(substr($msg,0,10)) eq 'expecting ')) {
        $msg = qq{q{$msg, but found '}.\$CONTEXT.q{' instead}};
    }
    else {
        $msg = qq{q{$msg}};
    }

    # Report how directive was interpreted, if requested to...
    if ($debug_build) {
        _debug_notify( info => "   |",
                               "   |...Treating $construct as:",
            ( $type eq 'log' ? "   |       \\ Log a message to the logfile"
                             : "   |       \\ Append a $severity error message to \@!"
            ),
        );
    }

    # Generate the regex...
    return $type eq 'log'
        ? qq{(?{Regexp::Grammars::_debug_logmsg(scalar \@Regexp::Grammars::RESULT_STACK,$msg)
                if \$Regexp::Grammars::DEBUG_LEVEL{\$Regexp::Grammars::DEBUG}
          })}

        : qq{(?:(?{;local \$Regexp::Grammar::_memopos=pos();})
              (?>\\s*+((?-s).{0,$MAX_CONTEXT_WIDTH}+))
              (?{; pos() = \$Regexp::Grammar::_memopos; push @!, $msg }) (?!)|}
        . ($severity eq 'fatal' ? q{(?!)} : q{})
        . q{)}
        ;
}

sub _translate_subpattern {
    my ($construct, $alias, $subpattern, $savemode, $postmodifier, $debug_build, $debug_runtime)
        = @_;

    # Determine save behaviour...
    my $is_noncapturing   = $savemode eq 'noncapturing';
    my $is_listifying     = $savemode eq 'list';
    my $is_codeblock      = substr($subpattern,0,3) eq '(?{'
                         || substr($subpattern,0,4) eq '(??{';
    my $value_saved       = $is_codeblock  ? '$^R'                    : '$^N';
    my $do_something_with = $is_codeblock  ? 'execute the code block' : 'match the pattern';
    my $result            = $is_codeblock  ? 'result'                 : 'matched substring';
    my $description       = $is_codeblock  ? substr($subpattern,2,-1) : $subpattern;
    my $debug_construct
        = $is_codeblock ?  '<' . substr($alias,1,-1) . '= (?{;' . substr($subpattern,3,-2) . '})>'
        :                  $construct
        ;

    # Report how construct was interpreted, if requested to...
    my $repeatedly = $REPETITION_DESCRIPTION_FOR{$postmodifier} // q{};
    my $results  = $is_listifying && $postmodifier    ? "each $result"
                 : substr($postmodifier,0,1) eq '?'   ? "any $result"
                 : $postmodifier && !$is_noncapturing ? "only the final $result"
                 :                                      "the $result"
                 ;
    if ($debug_build) {
        _debug_notify( info =>
                                 "   |",
                                 "   |...Treating $construct as:",
                                 "   |      |  $do_something_with $description $repeatedly",
            ( $is_noncapturing ? "   |       \\ but don't save $results"
            : $is_listifying   ? "   |       \\ appending $results to \$MATCH{$alias}"
            :                    "   |       \\ saving $results in \$MATCH{$alias}"
            )
        );
    }

    # Generate run-time debugging code (if any)...
    my ($debug_pre, $debug_post)
        = _build_debugging_statements($debug_runtime,$debug_construct, +1);

    # Generate post-match result-capturing code, if match captures...
    my $post_action = $is_noncapturing
        ? q{}
        : qq{local \@Regexp::Grammars::RESULT_STACK = (
                \@Regexp::Grammars::RESULT_STACK[0..\@Regexp::Grammars::RESULT_STACK-2],
                Regexp::Grammars::_extend_current_result_frame_with_$savemode(
                    \\\@Regexp::Grammars::RESULT_STACK, $alias, $value_saved
                ),
            );}
        ;

    # Translate to standard regex code...
    return qq{(?{;local \@Regexp::Grammars::RESULT_STACK
                    = \@Regexp::Grammars::RESULT_STACK;$debug_pre})
                (?:($subpattern)(?{;$post_action$debug_post}))$postmodifier};
}


sub _translate_hashmatch {
    my ($construct, $alias, $hashname, $savemode, $postmodifier, $debug_build, $debug_runtime)
        = @_;

    # Determine save behaviour...
    my $is_noncapturing   = $savemode eq 'noncapturing';
    my $is_listifying     = $savemode eq 'list';

    # Convert hash to hash lookup...
    my $hash_lookup = '$' . substr($hashname, 1). '{$^N}';

    # Report how construct was interpreted, if requested to...
    my $repeatedly = $REPETITION_DESCRIPTION_FOR{$postmodifier} // q{};
    my $results  = $is_listifying && $postmodifier    ? 'each matched key'
                 : substr($postmodifier,0,1) eq '?'   ? 'any matched key'
                 : $postmodifier && !$is_noncapturing ? 'only the final matched key'
                 :                                      'the matched key'
                 ;
    if ($debug_build) {
        _debug_notify( info =>
                                 "   |",
                                 "   |...Treating $construct as:",
                                 "   |      |  match a key from the hash $hashname $repeatedly",
            ( $is_noncapturing ? "   |       \\ but don't save $results"
            : $is_listifying   ? "   |       \\ appending $results to \$MATCH{$alias}"
            :                    "   |       \\ saving $results in \$MATCH{$alias}"
            )
        );
    }

    # Generate run-time debugging code (if any)...
    my ($debug_pre, $debug_post)
        = _build_debugging_statements($debug_runtime,$construct, +1);

    # Generate post-match result-capturing code, if match captures...
    my $post_action = $is_noncapturing
        ? q{}
        : qq{local \@Regexp::Grammars::RESULT_STACK = (
                \@Regexp::Grammars::RESULT_STACK[0..\@Regexp::Grammars::RESULT_STACK-2],
                Regexp::Grammars::_extend_current_result_frame_with_$savemode(
                    \\\@Regexp::Grammars::RESULT_STACK, $alias, \$^N
                ),
            );}
        ;

    # Translate to standard regex code...
    return qq{(?:(?{;local \@Regexp::Grammars::RESULT_STACK
                    = \@Regexp::Grammars::RESULT_STACK;$debug_pre})
                (?:((?&hk))(??{exists $hash_lookup ? q{} : q{(?!)}})(?{;$post_action$debug_post})))$postmodifier};
}


# Convert a "<rule> ** <rule>" construct to pure Perl 5.10...
sub _translate_separated_list {
    my ($term, $separator, $term_trans, $sep_trans,
        $ws, $debug_build, $debug_runtime) = @_;

    # Translate meaningful whitespace...
    $ws = length($ws) ? q{(?&ws)} : q{};

    # Report how construct was interpreted, if requested to...
    if ($debug_build) {
        _debug_notify( info =>
            "   |",
            "   |...Treating $term ** $separator as:",
            "   |      |  repeatedly match the subrule $term",
            "   |       \\ as long as the matches are separated by matches of $separator",
        );
    }

    # Translate to list-matching pattern...
    state $checkpoint
        = q{(?{;@Regexp::Grammars::RESULT_STACK = @Regexp::Grammars::RESULT_STACK;})};
    return qq{(?:$ws$checkpoint$sep_trans$ws$term_trans)*};
}

sub _translate_subrule_call {
    my ( $construct, $alias, $subrule, $savemode, $postmodifier,
         $debug_build, $debug_runtime, $valid_subrule_names_ref)
        = @_;

    # Shortcircuit if unknown subrule invoked...
    if (!$valid_subrule_names_ref->{$subrule}) {
        _debug_notify( error =>
            qq{Found call to $construct, but no <rule: $subrule> or},
            qq{<token: $subrule> was defined in the grammar},
            qq{(Did you misspell the rule name or forget to define the rule?)},
            q{},
        );
        return "(?{Regexp::Grammars::_debug_fatal('$construct')})(*COMMIT)(*FAIL)";
    }

    # Determine save behaviour...
    my $is_noncapturing = $savemode eq 'noncapturing';
    my $is_listifying   = $savemode eq 'list';

    my $save_code = 
       $is_noncapturing?
          q{ @Regexp::Grammars::RESULT_STACK[0..@Regexp::Grammars::RESULT_STACK-2] }
     : $is_listifying?
         qq{ \@Regexp::Grammars::RESULT_STACK[0..\@Regexp::Grammars::RESULT_STACK-3],
              Regexp::Grammars::_pop_current_result_frame_with_list(
                  \\\@Regexp::Grammars::RESULT_STACK, $alias, \$^N
              ),
         }
     :
         qq{ \@Regexp::Grammars::RESULT_STACK[0..\@Regexp::Grammars::RESULT_STACK-3],
              Regexp::Grammars::_pop_current_result_frame(
                   \\\@Regexp::Grammars::RESULT_STACK, $alias, \$^N
              ),
        }
     ; 

    # Report how construct was interpreted, if requested to...
    my $repeatedly = $REPETITION_DESCRIPTION_FOR{$postmodifier} // q{};
    my $results  = $is_listifying && $postmodifier    ? 'each match'
                 : substr($postmodifier,0,1) eq '?'   ? 'any match'
                 :                                      'the match'
                 ;
    if ($debug_build) {
        _debug_notify( info =>
                                 "   |",
                                 "   |...Treating $construct as:",
                                 "   |      |  match the subrule <$subrule> $repeatedly",
            ( $is_noncapturing ? "   |       \\ but don't save anything"
            : $is_listifying   ? "   |       \\ appending $results to \$MATCH{$alias}"
            :                    "   |       \\ saving $results in \$MATCH{$alias}"
            ),
        );
    }

    # Generate post-match result-capturing code, if match captures...
    my ($debug_pre, $debug_post)
        = _build_debugging_statements($debug_runtime, $construct);

    # Translate to standard regex code...
    return qq{(?:(?{;
            local \@Regexp::Grammars::RESULT_STACK = (\@Regexp::Grammars::RESULT_STACK, {});
            $debug_pre})((?&$subrule))(?{;
                local \@Regexp::Grammars::RESULT_STACK = (
                    $save_code
                );$debug_post
    }))$postmodifier};
}

sub _translate_rule_def {
    my ($type, $qualifier, $name, $body, $objectify) = @_;;

    # Return object if requested...
    my $objectification =
        $objectify ? qq{(?{; local \@Regexp::Grammars::RESULT_STACK = \@Regexp::Grammars::RESULT_STACK;
                            bless \$Regexp::Grammars::RESULT_STACK[-1], '$qualifier$name'
                        })}
                    : q{};

    # Each rule or token becomes a DEFINE'd Perl 5.10 named capture...
    return qq{
        (?(DEFINE)
            (?<$name> (?{\$Regexp::Grammars::RESULT_STACK[-1]{'!'}=\$#{!};})
                (?:$body) $objectification
                (?{;\$#{!}=delete \$Regexp::Grammars::RESULT_STACK[-1]{'!'};})
            )
        )
    };
}


# Locate any valid <...> sequences and replace with native regex code...
sub _translate_subrule_calls {
    my ($grammar_spec,
        $compiletime_debugging_requested,
        $runtime_debugging_requested,
        $pre_match_debug,
        $post_match_debug,
        $expectation,
        $subrule_names_ref,
    ) = @_;

    # Remember the preceding construct, so as to implement the ** operator...
    my $prev_construct   = q{};
    my $prev_translation = q{};

    # Translate all other calls...
    $grammar_spec =~ s{
      (?<list_marker> (?<ws1> \s*+)  \*\* (?<ws2> \s*+) )?
      (?<construct>
        <
        (?:
            (?<self_subrule_scalar_nocap> 
                   \.                            \s* (?<subrule>(?&IDENT))  \s*      
            )
          | (?<self_subrule_scalar> 
                                                 \s* (?<subrule>(?&IDENT))  \s*      
            )
          | (?<self_subrule_list> 
                   \[                            \s* (?<subrule>(?&IDENT))  \s* \]
            )
          | (?<alias_subrule_scalar>   
                       (?<alias>(?&IDENT)) \s* = \s* (?<subrule>(?&IDENT))  \s*    
            )
          | (?<alias_subrule_list>
                   \[  (?<alias>(?&IDENT)) \s* = \s* (?<subrule>(?&IDENT))  \s* \] 
            )
          | (?<alias_parens_scalar_nocap> 
                   \.  (?<alias>(?&IDENT)) \s* = \s* (?<pattern>(?&PARENCODE)|(?&PARENS)) \s*    
            )
          | (?<alias_parens_scalar> 
                       (?<alias>(?&IDENT)) \s* = \s* (?<pattern>(?&PARENCODE)|(?&PARENS)) \s*    
            )
          | (?<alias_parens_list>
                   \[  (?<alias>(?&IDENT)) \s* = \s* (?<pattern>(?&PARENCODE)|(?&PARENS)) \s* \]  
            )
          | (?<alias_hash_scalar_nocap> 
                                                     (?<varname>(?&HASH)) \s*    
            )
          | (?<alias_hash_scalar> 
                       (?<alias>(?&IDENT)) \s* = \s* (?<varname>(?&HASH)) \s*    
            )
          | (?<alias_hash_list>
                   \[  (?<alias>(?&IDENT)) \s* = \s* (?<varname>(?&HASH)) \s* \]  
            )
          |
            (?<require_directive>
                    require \s* : \s* (?<condition> (?&PARENCODE) ) \s*
            )
          |
            (?<debug_directive>
                    debug \s* : \s* (?<cmd> run | match | step | try | off | on) \s*
            )
          |
            (?<autoerror_directive>
                    error \s*+ : \s*+ 
            )
          |
            (?<error_directive>
                    (?<error_type> log | error | warning )
                    \s*+ : \s*+
                    (?<msg> (?&PARENCODE) | .+? )
                    \s*+
            )
        )
        > (?<modifier> \s* (?! \*\* ) [?+*][?+]? | )
      |
        (?<raw_regex>  \\. | (?&PARENS) | (?&CHARSET) | \# [^\n]*+ | [^][<>#\\]++ )
    )

    (?(DEFINE)
        (?<PARENS>    \(     (?: \\. | (?&PARENS) | (?&CHARSET) | [^][()\\<>]++ )*+  \)   )
        (?<BRACES>    \{     (?: \\. | (?&BRACES) |               [^{}\\]++   )*+    \}   )
        (?<PARENCODE> \(\?\{ (?: \\. | (?&BRACES) |               [^{}\\]++   )*+    \}\) )
        (?<HASH>      \% (?&IDENT) (?: :: (?&IDENT) )*                                    )
        (?<CHARSET>   \[              \^?+ \]?+ [^]]*+                               \]   ) 
        (?<IDENT>     [^\W\d]\w*+                                                         )
    )
    }{ 
        my $curr_construct = $+{construct};
        my $alias          = ($+{alias}//'MATCH') eq 'MATCH' ? q{'='} : qq{'$+{alias}'};

        # Determine and remember the necessary translation...
        my $curr_translation = do{
            if ($+{alias_parens_scalar}) {
                _translate_subpattern(
                    $curr_construct, $alias, $+{pattern}, 'scalar', $+{modifier},
                    $compiletime_debugging_requested, $runtime_debugging_requested
                );
            }
            elsif ($+{alias_parens_scalar_nocap}) {
                _translate_subpattern(
                    $curr_construct, $alias, $+{pattern}, 'noncapturing', $+{modifier},
                    $compiletime_debugging_requested, $runtime_debugging_requested
                );
            }
            elsif ($+{alias_parens_list}) {
                _translate_subpattern(
                    $curr_construct, $alias, $+{pattern}, 'list', $+{modifier},
                    $compiletime_debugging_requested, $runtime_debugging_requested
                );
            }
            elsif ($+{alias_hash_scalar}) {
                _translate_hashmatch(
                    $curr_construct, $alias, $+{varname}, 'scalar', $+{modifier},
                    $compiletime_debugging_requested, $runtime_debugging_requested
                );
            }
            elsif ($+{alias_hash_scalar_nocap}) {
                _translate_hashmatch(
                    $curr_construct, $alias, $+{varname}, 'noncapturing', $+{modifier},
                    $compiletime_debugging_requested, $runtime_debugging_requested
                );
            }
            elsif ($+{alias_hash_list}) {
                _translate_hashmatch(
                    $curr_construct, $alias, $+{varname}, 'list', $+{modifier},
                    $compiletime_debugging_requested, $runtime_debugging_requested
                );
            }
            elsif ($+{alias_subrule_scalar}) {
                _translate_subrule_call(
                    $curr_construct, $alias, $+{subrule}, 'scalar', $+{modifier},
                    $compiletime_debugging_requested,
                    $runtime_debugging_requested,
                    $subrule_names_ref,
                );
            }
            elsif ($+{alias_subrule_list}) {
                _translate_subrule_call(
                    $curr_construct, $alias, $+{subrule}, 'list', $+{modifier},
                    $compiletime_debugging_requested,
                    $runtime_debugging_requested,
                    $subrule_names_ref,
                );
            }
            elsif ($+{self_subrule_scalar_nocap}) {
                _translate_subrule_call(
                    $curr_construct, qq{'$+{subrule}'}, $+{subrule}, 'noncapturing', $+{modifier},
                    $compiletime_debugging_requested,
                    $runtime_debugging_requested,
                    $subrule_names_ref,
                );
            }
            elsif ($+{self_subrule_scalar}) {
                _translate_subrule_call(
                    $curr_construct, qq{'$+{subrule}'}, $+{subrule}, 'scalar', $+{modifier},
                    $compiletime_debugging_requested,
                    $runtime_debugging_requested,
                    $subrule_names_ref,
                );
            }
            elsif ($+{self_subrule_list}) {
                _translate_subrule_call(
                    $curr_construct, qq{'$+{subrule}'}, $+{subrule}, 'list', $+{modifier},
                    $compiletime_debugging_requested,
                    $runtime_debugging_requested,
                    $subrule_names_ref,
                );
            }
            elsif ($+{raw_regex}) {
                _translate_raw_regex(
                    $+{raw_regex}, $compiletime_debugging_requested
                );
            }
            elsif ($+{require_directive}) {
                _translate_require_directive(
                    $curr_construct, $+{condition}, $compiletime_debugging_requested
                );
            }
            elsif ($+{debug_directive}) {
                _translate_debug_directive(
                    $curr_construct, $+{cmd}, $compiletime_debugging_requested
                );
            }
            elsif ($+{error_directive}) {
                _translate_error_directive(
                    $curr_construct, $+{error_type}, $+{msg},
                    $compiletime_debugging_requested
                );
            }
            elsif ($+{autoerror_directive}) {
                _translate_error_directive(
                    $curr_construct, 'error', "Expected $expectation",
                    $compiletime_debugging_requested
                );
            }
            else {
                die qq{Internal error: this shouldn't happen!\nNear $curr_construct};
            }
        };

        # Handle the ** operator...
        if ($+{list_marker}) {
            my $ws = $+{ws1} . $+{ws2};

            $curr_translation = _translate_separated_list(
                $prev_construct,   $curr_construct,
                $prev_translation, $curr_translation, $ws,
                $compiletime_debugging_requested, $runtime_debugging_requested
            );
            $curr_construct = qq{$prev_construct ** $curr_construct};
        }

        # Finally, remember this latest translation, and return it...
        $prev_construct   = $curr_construct;
        $prev_translation = $curr_translation;;
    }exmsg;

    # Translate magic hash accesses...
    $grammar_spec =~ s{\$MATCH (?= \s*\{) }  # ...Access named entry in hash
                      {\$Regexp::Grammars::RESULT_STACK[-1]}xmsg;

    # Translate magic scalars and hashes...
    state $translate_scalar = {
        q{%$MATCH}  => q{%{$Regexp::Grammars::RESULT_STACK[-1]{q{=}}}},
        q{@$MATCH}  => q{@{$Regexp::Grammars::RESULT_STACK[-1]{q{=}}}},
        q{$MATCH}   => q{$Regexp::Grammars::RESULT_STACK[-1]{q{=}}},
        q{$CAPTURE} => q{$^N},
        q{$CONTEXT} => q{$^N},
        q{$DEBUG}   => q{$Regexp::Grammars::DEBUG},
        q{$INDEX}   => q{${\\pos()}},
        q{%MATCH}   => q{$Regexp::Grammars::RESULT_STACK[-1]},
    };
    state $translatable_scalar
        = join '|', map {quotemeta $_}
                        sort {length $b <=> length $a}
                             keys %{$translate_scalar};

    $grammar_spec =~ s{ ($translatable_scalar) (?! \s* (?: \[ | \{) ) }
                      {$translate_scalar->{$1}}oxmsg;


    return $grammar_spec;
}

# Generate a "decimal timestamp" and insert in a template...
sub _timestamp {
    my ($template) = @_;

    # Generate and insert any timestamp...
    if ($template =~ /%t/) {
        my ($sec, $min, $hour, $day, $mon,   $year) = localtime;
                                     $mon++; $year+=1900;
        my $timestamp = sprintf("%04d%02d%02d.%02d%02d%02d",
                                $year, $mon, $day, $hour, $min, $sec);
        $template =~ s{%t}{$timestamp}xms;;
    }

    return $template;
}

# Open (or re-open) the requested log file...
sub _autoflush {
    my ($fh) = @_;
    my $originally_selected = select $fh;
    $|=1;
    select $originally_selected;
}

sub _open_log {
    my ($mode, $filename) = @_;

    # Special case: '-' --> STDERR
    if ($filename eq q{-}) {
        return *STDERR{IO};
    }
    # Otherwise, just open the named file...
    elsif (open my $fh, $mode, $filename) {
        _autoflush($fh);
        return $fh;
    }
    # Otherwise, generate a warning and default to STDERR...
    else {
        local *Regexp::Grammars::LOGFILE = *STDERR{IO};
        _debug_notify( warn =>
            qq{Unable to open log file '$filename'},
            qq{($!)},
            qq{Defaulting to STDERR instead.},
        );
        _debug_notify( q{} => q{} );
        return *STDERR{IO};
    }
}

# Transform grammar-augmented regex into pure Perl 5.10 regex...
sub _build_grammar {
    my ($grammar_spec) = @_;
    $grammar_spec .= q{};

    # Check for dubious repeated <SUBRULE> constructs that throw away captures...
    my @dubious
        = $grammar_spec
            =~ m{ < (?! \[ ) ( $IDENT (?: = [^>]*)? ) > \s* ([+*][?+]?|\{.*\}[?+]?) }gxms;

    # Report dubiousities...
    while (@dubious) {
        my ($rule, $qual) = splice @dubious, 0, 2;
        _debug_notify( warn =>
            qq{Repeated subrule <$rule>$qual will only capture its final match},
            qq{(Did you mean <[$rule]>$qual instead?)},
            q{},
        )
    }

    # Check for dubious non-backtracking <SUBRULE> constructs...
    @dubious
        = $grammar_spec
            =~ m{ < ( [^>]+ ) > \s* ([?+*][+]|\{.*\}[+]) }gxms;

    # Report dubiousities...
    while (@dubious) {
        my ($rule, $qual) = splice @dubious, 0, 2;
        my $safe_qual = substr($qual,0,-1);
        _debug_notify( warn =>
            qq{Non-backtracking subrule <$rule>$qual not fully supported yet},
            qq{(If grammar does not work try <$rule>$safe_qual instead)},
            q{},
        )
    }

    # Check whether a log file was specified...
    my $compiletime_debugging_requested;
    local *Regexp::Grammars::LOGFILE = *Regexp::Grammars::LOGFILE;
    my $logfile = q{-};

    $grammar_spec =~ s{ ^ [^#]* < logfile: \s* ([^>]+?) \s* > }{
        $logfile = _timestamp($1);

        # Presence of <logfile:...> implies compile-time logging...
        $compiletime_debugging_requested = 1;
        *Regexp::Grammars::LOGFILE = _open_log('>',$logfile);

        # Delete <logfile:...> directive...
        q{};
    }gexms;

    # Look ahead for any run-time debugging requests...
    my $runtime_debugging_requested
        = $grammar_spec =~ m{
              ^ [^#]* < debug: \s* (run | match | step | try | on | off) \s* >
            | \$DEBUG (?! \s* (?: \[ | \{) )
        }xms;

    # Standard actions set up and clean up any regex debugging...
    # Before entire match, set up a stack of attempt records and report...
    my $pre_match_debug
        = $runtime_debugging_requested
            ? qq{(?{; *Regexp::Grammars::LOGFILE
                        = Regexp::Grammars::_open_log('>>','$logfile');
                      Regexp::Grammars::_init_try_stack(); })}
            : qq{(?{; *Regexp::Grammars::LOGFILE
                        = Regexp::Grammars::_open_log('>>','$logfile'); })}
            ;

    # After entire match, report whether successful or not...
    my $post_match_debug
        = $runtime_debugging_requested
            ? qq{(?{;Regexp::Grammars::_debug_matched(0,\\%/,'<grammar>',\$^N)})
                |(?>(?{;Regexp::Grammars::_debug_handle_failures(0,'<grammar>'); }) (?!))
                }
            : q{}
            ;

    # Subdivide into rule and token definitions, preparing to process each...
    my @defns = split m{
            < (obj|)(rule|token) \s*+ : \s*+ ((?:${IDENT}::)*+)($IDENT) \s* >
        }xms, $grammar_spec;

    # Extract up list of names of defined rules/tokens...
    # (Name is every 4th item out of every five, skipping the first item)
    my %subrule_names = map { $_ => 1 }
                            @defns[ map { $_ * 5 + 4 } 0 .. ((@defns-1)/5-1) ];

    # Report how main regex was interpreted, if requested to...
    if ($compiletime_debugging_requested) {
        _debug_notify( info =>
            "Processing the main regex before any rule definitions",
        );
    }

    # Any actual regex is processed first...
    my $regex = _translate_subrule_calls(
        shift @defns,
        $compiletime_debugging_requested,
        $runtime_debugging_requested,
        $pre_match_debug,
        $post_match_debug,
        'valid input',         # Expected...what?
        \%subrule_names,
    );

    # Report how construct was interpreted, if requested to...
    if ($compiletime_debugging_requested) {
        _debug_notify( q{} =>
            q{   |},
            q{    \\___End of main regex},
            q{},
        );
    }

    #  Then iterate any following rule definitions...
    while (@defns) {
        # Grab details of each rule defn (as extracted by previous split)...
        my ($objectify, $type, $qualifier, $name, $body) = splice(@defns, 0, 5);

        # Report how construct was interpreted, if requested to...
        if ($compiletime_debugging_requested) {
            _debug_notify( info =>
                "Defining a rule: <$name>",
                "   |...Returns: " . ($objectify ? "an object of class '$qualifier$name'" : "a hash"),
            );
        }

        # Translate any nested <...> constructs...
        $body = _translate_subrule_calls(
            $body,
            $compiletime_debugging_requested,
            $runtime_debugging_requested,
            $pre_match_debug,
            $post_match_debug,
            lc($name),                # Expected...what?
            \%subrule_names,
        );

        # Report how construct was interpreted, if requested to...
        if ($compiletime_debugging_requested) {
            _debug_notify( q{} =>
                q{   |},
                q{    \\___End of rule definition},
                q{},
            );
        }

        # Rules make non-code literal whitespace match textual whitespace...
        if ($type eq 'rule') {
            state $CODE_OR_SPACE = qr{
                  \( \?\?? (?&BRACED) \)
                | (?<! \A) \s++ (?! \| | (?: \) \s* )? \z | \(\(?\?\&ws\) | \(\?\??\{ | \\s )
                (?(DEFINE) (?<BRACED> \{ (?: \\. | (?&BRACED) | [^{}] )* \} ) )
            }xms;
            $body =~ s{($CODE_OR_SPACE)}
                      [  substr($1,0,3) eq '(?{'
                      || substr($1,0,4) eq '(??{' ? $1 : '(?&ws)']exmsg;  #}
        }

        $regex .= _translate_rule_def( $type, $qualifier, $name, $body, $objectify );
    }

    # Insert checkpoints into any user-defined code block...
    $regex =~ s{ \( \?\?? \{ \K (?!;) }{
        local \@Regexp::Grammars::RESULT_STACK = \@Regexp::Grammars::RESULT_STACK; 
    }xmsg;
    
    # Check for any suspicious left-overs from the start of the regex...
    pos $regex = 0;

    # Report anything that starts like a subrule, but isn't...
    my %seen;
    while ($regex =~ m{( (?<! \(\? | q\{ ) (?<! \\) < [[.]* $IDENT \s* (:?) .*? [\n>] )}gxms) {
        my $construct = $1;
        my $something = $2 ? 'directive' : 'subrule call';

        # Only report potential problems once...
        next if $seen{$construct}++;

        # Also explain how to indicate the construct is intentional...
        _debug_notify( warn =>
            qq{Possible invalid $something:},
            qq{    $construct},
            qq{(To silence this warning, use: \\$construct},
        );
        _debug_notify( q{} => q{} );
    }

    # Aggregrate the final grammar...
    _complete_regex($regex, $pre_match_debug, $post_match_debug);
}

sub _complete_regex {
    my ($regex, $pre_match_debug, $post_match_debug) = @_;

    return qq{$pre_match_debug$PROLOGUE$regex$EPILOGUE$post_match_debug};
}

1; # Magic true value required at end of module

__END__

=head1 NAME

Regexp::Grammars - Add grammatical parsing features to Perl 5.10 regexes


=head1 VERSION

This document describes Regexp::Grammars version 1.001_005


=head1 SYNOPSIS

    use Regexp::Grammars;

    my $parser = qr{
        (?:
            <Verb>               # Parse and save a Verb in a scalar
            <.ws>                # Parse but don't save whitespace
            <Noun>               # Parse and save a Noun in a scalar

            <type={ rand > 0.5 ? 'VN' : 'VerbNoun' }>
                                 # Save result of expression in a scalar
        |
            (?:
                <[Noun]>         # Parse a Noun and save result in a list
                                     (saved under the key 'Noun')
                <[PostNoun=ws]>  # Parse whitespace, save it in a list
                                 #   (saved under the key 'PostNoun')
            )+                   

            <Verb>               # Parse a Verb and save result in a scalar
                                     (saved under the key 'Verb')

            <type={ 'VN' }>      # Save a literal in a scalar
        |
            <debug: match>       # Turn on the integrated debugger here
            <.Cmd= (?: mv? )>    # Parse but don't capture a subpattern
                                     (name it 'Cmd' for debugging purposes)
            <[File]>+            # Parse 1+ Files and save them in a list
                                     (saved under the key 'File')
            <debug: off>         # Turn off the integrated debugger here
            <Dest=File>          # Parse a File and save it in a scalar
                                     (saved under the key 'Dest')
        )

        ################################################################

        <token: File>              # Define a subrule named File
            <.ws>                  #  - Parse but don't capture whitespace
            <MATCH= ([\w-]+) >     #  - Parse the subpattern and capture 
                                   #    matched text as the result of the
                                   #    subrule

        <token: Noun>              # Define a subrule named Noun
            cat | dog | fish       #  - Match an alternative (as usual) 

        <rule: Verb>               # Define a whitespace-sensitive subrule
            eats                   #  - Match a literal (after any space)
            <Object=Noun>?         #  - Parse optional subrule Noun and
                                   #    save result under the key 'Object'
        |                          #  Or else...
            <AUX>                  #  - Parse subrule AUX and save result
            <part= (eaten|seen) >  #  - Match a literal, save under 'part'

        <token: AUX>               # Define a whitespace-insensitive subrule
            (has | is)             #  - Match an alternative and capture
            (?{ $MATCH = uc $^N }) #  - Use captured text as subrule result

    };

    # Match the grammar against some text...
    if ($text =~ $parser) {
        # If successful, the hash %/ will have the hierarchy of results...
        process_data_in( %/ );
    }



=head1 QUICKSTART CHEATSHEET

=head2 In your program...

    use Regexp::Grammars;    Allow enhanced regexes in lexical scope
    %/                       Result-hash for successful grammar match

=head2 Defining rules in your grammar...

    <rule:     IDENTIFIER>   Define rule with magic whitespace
    <token:    IDENTIFIER>   Define rule without magic whitespace
    <objrule:  IDENTIFIER>   Define rule returning blessed result-hash
    <objtoken: IDENTIFIER>   Define token returning blessed result-hash

=head2 Matching rules in your grammar...

    <RULENAME>               Call named subrule,
                             save result to $MATCH{RULENAME}

    <%HASH>                  Match longest possible key of hash

    <ALIAS= RULENAME>        Call subrule, save result in $MATCH{ALIAS}
    <ALIAS= %HASH>           Match a hash key, save in $MATCH{ALIAS}
    <ALIAS= ( PATTERN )>     Match pattern, save match in $MATCH{ALIAS}
    <ALIAS= (?{ CODE })>     Execute code, save value in $MATCH{ALIAS}

    <.SUBRULE>               Call any kind of subrule (as above),
                             but don't save the result in %MATCH

    <[SUBRULE]>              Call any kind of subrule (as above) but
                             append result instead of overwriting it

    <SUBRULE1> ** <SUBRULE2> Match one or more repetitions of SUBRULE1
                             as long as they're separated by SUBRULE2

=head2 In your grammar's code blocks...

    $CAPTURE    Alias for $^N (the most recent paren capture)
    $CONTEXT    Another alias for $^N
    $INDEX      Current index of next matching position in string
    %MATCH      Current rule's result-hash
    $MATCH      Magic override value (returned instead of result-hash)
    $DEBUG      Current match-time debugging mode

=head2 Debugging support...

    <require: (?{ CODE })>   Fail if code evaluates false
    <debug: COMMAND >        Change match-time debugging mode
    <error: TEXT|CODEBLOCK>  Queue text or value as an error message
    <logfile: LOGFILE>       Change debugging log file (default: STDERR)
    <log: (?{ CODE })  >     Explicitly add a message to the log



=head1 DESCRIPTION

This module adds a small number of new regex constructs that can be used
within Perl 5.10 patterns to implement complete recursive-descent parsing.

Perl 5.10 already supports recursive=descent I<matching>, via the new
C<< (?<name>...) >> and C<< (?&name) >> constructs. For example, here is
a simple matcher for a subset of the LaTeX markup language:

    $matcher = qr{
        (?&File)

        (?(DEFINE)
            (?<File>     (?&Element)* )

            (?<Element>  \s* (?&Command)
                      |  \s* (?&Literal)
            )

            (?<Command>  \\ \s* (?&Literal) \s* (?&Options)? \s* (?&Args)? )

            (?<Options>  \[ \s* (?:(?&Option) (?:\s*,\s* (?&Option) )*)? \s* \])

            (?<Args>     \{ \s* (?&Element)* \s* \}  )

            (?<Option>   \s* [^][\$&%#_{}~^\s,]+     )

            (?<Literal>  \s* [^][\$&%#_{}~^\s]+      )
        )
    }xms

This technique makes it possible to use regexes to recognize complex,
hierarchical--and even recursive--textual structures. The problem is
that Perl 5.10 doesn't provide any support for extracting that
hierarchical data into nested data structures. In other words, using
Perl 5.10 you can I<match> complex data, but not I<parse> it into an
internally useful form.

An additional problem when using Perl 5.10 regexes to match complex data
formats is that you have to make sure you remember to insert
whitespace-matching constructs (such as C<\s*>) at every possible position
where the data might contain ignorable whitespace. This reduces the
readability of such patterns, and increases the chance of errors (typically
caused by overlooking a location where whitespace might appear).

The Regexp::Grammars module solves both those problems.

If you import the module into a particular lexical scope, it
preprocesses any regex in that scope, so as to implement a number of
extensions to the standard Perl 5.10 regex syntax. These extensions
simplify the task of defining and calling subrules within a grammar, and
allow those subrule calls to capture and retain the components of they
match in a proper hierarchical manner.

For example, the above LaTeX matcher could be converted to a full LaTeX parser
(and considerably tidied up at the same time), like so:

    use Regexp::Grammars;
    $parser = qr{
        <File>

        <rule: File>       <[Element]>*

        <rule: Element>    <Command> | <Literal>

        <rule: Command>    \\  <Literal>  <Options>?  <Args>?

        <rule: Options>    \[  <[Option]> ** (,)  \]
        
        <rule: Args>       \{  <[Element]>*  \}

        <rule: Option>     [^][\$&%#_{}~^\s,]+

        <rule: Literal>    [^][\$&%#_{}~^\s]+
    }xms

Note that there is no need to explicitly place C<\s*> subpatterns throughout
the rules; that is taken care of automatically.

If the Regexp::Grammars version of this regex were successfully matched
against some appropriate LaTeX document, each rule would call the
subrules specified within it, and then return a hash containing whatever
result each of those subrules returned, with each result indexed by the
subrule's name.

That is, if the rule named C<Command> were invoked, it would first try
to match a backslash, then it would call the three subrules
C<< <Literal> >>, C<< <Options> >>, and C<< <Args> >> (in that sequence). If
they all matched successfully, the C<Command> rule would then return a
hash with three keys: C<'Literal'>, C<'Options'>, and C<'Args'>. The value
for each of those hash entries would be whatever result-hash the
subrules themselves had returned when matched.

In this way, each level of the hierarchical regex can generate hashes
recording everything its own subrules matched, so when the entire pattern
matches, it produces a tree of nested hashes that represent the
structured data the pattern matched.

For example, if the previous regex grammar were matched against a string
containing:

    \documentclass[a4paper,11pt]{article}
    \author{D. Conway}

it would automatically extract the following data structure:

    {
        'file' => {
            'element' => [
                {
                    'command' => {
                        'literal' => 'documentclass',
                        'options' => {
                            'option'  => [ 'a4paper', '11pt' ],
                        },
                        'args'    => {
                            'element' => [ 'article' ],
                        }
                    }
                },
                {
                    'command' => {
                        'literal' => 'author',
                        'args' => {
                            'element' => [
                                {
                                    'literal' => 'D.',
                                },
                                {
                                    'literal' => 'Conway',
                                }
                            ]
                        }
                    }
                }
            ]
        }
    }

The data structure that Regexp::Grammars produces from a regex match 
is available to the surrounding program in the magic variable C<%/>.

Regexp::Grammars provides many features that simplify the extraction of
hierarchical data via a regex match, and also some features that can
simplify the processing of that data once it has been extracted. The
following sections explain each of those features, and some of the
parsing techniques they support.


=head2 Setting up the module

Just add:
    
    use Regexp::Grammars;

to any lexical scope. Any regexes within that scope will automatically now
implement the new parsing constructs:

    use Regexp::Grammars;

    my $parser = qr/ regex with $extra <chocolatey> grammar bits /x;

Note that you will need to use the C</x> modifier when declaring a regex
grammar. Otherwise, the default I<"a whitespace character matches exactly
that whitespace character"> behaviour of Perl regexes will mess up your
grammar's parsing.

Once the grammar has been processed, you can then match text against the
extended regexes, in the usual manner (i.e. via a C<=~> match):

    if ($input_text =~ $parser) {
        ...
    }
    
After a successful match, the variable C<%/> will contain a series of
nested hashes representing the structured hierarchical data captured
during the parse.

=head2 Structure of a Regexp::Grammars grammar

A Regexp::Grammars specification consists of a pattern (which may
include both standard Perl 5.10 regex syntax, as well as special
Regexp::Grammars directives), followed by one or more rule or token
definitions.

For example:

    use Regexp::Grammars;
    my $balanced_brackets = qr{

        # Pattern...
        <paren_pair> | <brace_pair>

        # Rule definition...
        <rule: paren_pair> 
            \(  (?: <escape> | <paren_pair> | <brace_pair> | [^()] )*  \)

        # Rule definition...
        <rule: brace_pair>
            \{  (?: <escape> | <paren_pair> | <brace_pair> | [^{}] )*  \}

        # Token definition...
        <token: escape>
            \\ . 
    }xms;

The initial pattern acts like the "top" rule of the grammar, and must be
matched completely for the grammar to match.

The rules and tokens are declarations only and they are not directly matched.
Instead, they act like subroutines, and are invoked by name from the
initial pattern (or from within a rule or token).

Each rule or token extends from the directive that introduces it up to either
the next rule or token directive, or (in the case of the final rule or token)
to the end of the grammar.


=head2 Tokens vs rules (whitespace handling)

The difference between a token and a rule is that a token treats any
whitespace within it exactly as a normal Perl regular expression would.
That is, a sequence of whitespace in a token is ignored if the C</x>
modifier is in effect, or else matches the same literal sequence of
whitespace characters (if C</x> is not in effect).

In a rule, any sequence of whitespace (except those at the very start and the
very end of the rule) is treated as matching the implicit subrule C<< <.ws> >>,
which is automatically predefined to match optional whitespace (i.e. C<\s*>).

You can explicitly define a C<< <ws> >> token to change that default
behaviour. For example, you could alter the definition of "whitespace" to
include Perlish comments, by adding an explicit C<< <token: ws> >>:

    <token: ws> 
        (?: \s+ | #[^\n]* )*

But be careful not to define C<< <ws> >> as a rule, as this will lead to 
all kinds of infinitely recursive unpleasantness.


=head2 Calling subrules

To invoke a rule to match at any point, just enclose the rule's name in angle
brackets (like in Perl 6). There must be no space between the opening bracket
and the rulename. For example::

    qr{ 
        file:             # Match literal sequence 'f' 'i' 'l' 'e' ':'
        <name>            # Call <rule: name>
        <options>?        # Call <rule: options> (it's okay if it fails)

        <rule: name>
            # etc.
    }x;

If you need to match a literal pattern that would otherwise look like a
subrule call, just backslash-escape the leading angle:

    qr{ 
        file:             # Match literal sequence 'f' 'i' 'l' 'e' ':'
        \<name>           # Match literal sequence '<' 'n' 'a' 'm' 'e' '>' 
        <options>?        # Call <rule: options> (it's okay if it fails)

        <rule: name>
            # etc.
    }x;


=head2 Subrule results

If a subrule call successfully matches, the result of that match is a
reference to a hash. That hash reference is stored in the current rule's
own result-hash, under the name of the subrule that was invoked. The
hash will, in turn, contain the results of any more deeply nested
subrule calls, each stored under the name by which the nested
subrule was invoked.

In other words, if the rule C<sentence> is defined:

    <rule: sentence>
        <noun> <verb> <object>

then successfully calling the rule:

    <sentence>

causes a new hash entry at the current nesting level. That entry's key will be
C<'sentence'> and its value will be a reference to a hash, which in turn will
have keys: C<'noun'>, C<'verb'>, and C<'object'>.

In addition each result-hash has one extra key: the empty string. The
value for this key is whatever string the entire subrule call matched.
So, for example, a successful call to C<< <sentence> >> might add
something like the following to the current result-hash:

    sentence => {
        ""     => 'I saw a dog',
        noun   => 'I',
        verb   => 'saw',
        object => {
            ""      => 'a dog',
            article => 'a',
            noun    => 'dog',
        },
    }

Note, however, that if the result-hash at any level contains I<only>
the empty-string key (i.e. the subrule did not call any sub-subrules or
save any of their nested result-hashes), then the hash is "unpacked"
and just the matched substring itself if returned.

For example, if C<< <rule: sentence> >> had been defined:

    <rule: sentence>
        I see dead people

then a successful call to the rule would only add:

    sentence => 'I see dead people'

to the current result-hash.

This is a useful feature because it prevents a series of nested subrule
calls from producing very unwieldy data structures. For example, without
this automatic unpacking, even the simple earlier example:

    <rule: sentence>
        <noun> <verb> <object>

would produce something needlessly complex, such as:

    sentence => {
        ""     => 'I saw a dog',
        noun   => {
            "" => 'I',
        },
        verb   => {
            "" => 'saw',
        },
        object => {
            ""      => 'a dog',
            article => {
                "" => 'a',
            },
            noun    => {
                "" => 'dog',
            },
        },
    }



=head2 Renaming subrule results

It is not always convenient to have subrule results stored under the
same name as the rule itself. Rule names should be optimized for
understanding the behaviour of the parser, whereas result names should
be optimized for understanding the structure of the data. Often those
two goals are identical, but not always; sometimes rule names need to
describe what the data looks like, while result names need to describe
what the data means.

For example, sometimes you need to call the same rule twice, to match
two syntactically identical components whose positions give then semantically
distinct meanings:

    <rule: copy_cmd>
        copy <file> <file>

The problem here is that, if the second call to C<< <file> >> succeeds, its
result-hash will be stored under the key C<'file'>, clobbering the data that
was returned from the first call to C<< <file> >>.

To avoid such problems, Regexp::Grammars allows you to I<alias> any subrule
call, so that it is still invoked by the original name, but its result-hash is
stored under a different key. The syntax for that is:
C<<< <I<alias>=I<rulename>> >>>. For example:

    <rule: copy_cmd>
        copy <from=file> <to=file>

Here, C<< <rule: file> >> is called twice, with the first result-hash being
stored under the key C<'from'>, and the second result-hash being stored under
the key C<'to'>.

Note, however, that the alias before the C<=> must be a proper
identifier (i.e. a letter or underscore, followed by letters, digits,
and/or underscores). Aliases that start with an underscore and aliases named
C<MATCH> have special meaning (see L<Private subrule calls> and
L<Result distillation> respectively).

Aliases can also be useful for normalizing data that may appear in different
formats and sequences. For example:

    <rule: copy_cmd>
        copy <from=file>        <to=file>
      | dup    <to=file>  as  <from=file>
      |      <from=file>  ->    <to=file>
      |        <to=file>  <-  <from=file>

Here, regardless of which order the old and new files are specified, the
result-hash always gets:

    copy_cmd => {
        from => 'oldfile',
          to => 'newfile',
    }


=head2 List-like subrule calls

If a subrule call is quantified with a repetition specifier:

    <rule: file_sequence>
        <file>+

then each repeated match overwrites the corresponding entry in the
surrounding rule's result-hash, so only the result of the final
repetition will be retained. That is, if the above example matched
the string C<S<"foo.pl bar.py baz.php">>, then the result-hash would contain:

    file_sequence {
        ""   => 'foo.pl bar.py baz.php',
        file => 'baz.php',
    }

Usually, that's not the desired outcome, so Regexp::Grammars provides
another mechanism by which to call a subrule; one that saves I<all>
repetitions of its results.

A regular subrule call consists of the rule's name surrounded by angle
brackets. If, instead, you surround the rule's name with C<< <[...]> >>
(angle I<and> square brackets) like so:

    <rule: file_sequence>
        <[file]>+

then the rule is invoked in exactly the same way, but the result of that
submatch is pushed onto an array nested inside the appropriate result-hash
entry. In other words, if the above example matched the same
C<S<"foo.pl bar.py baz.php">> string, the result-hash would contain:

    file_sequence {
        ""   => 'foo.pl bar.py baz.php',
        file => [ 'foo.pl', 'bar.py', 'baz.php' ],
    }

This "listifying subrule call" can also be useful for non-repeated subrule
calls, if the same subrule is invoked in several places in a grammar. For
example if a cmdline option could be given either one or two values, you
might parse it:

    <rule: size_option>   
        -size <[size]> (?: x <[size]> )?

The result-hash entry for C<'size'> would then always contain an array,
with either one or two elements, depending on the input being parsed.

Listifying subrules can also be given L<aliases|"Renaming subrule results">,
just like ordinary subrules. The alias is always specified inside the square
brackets:

    <rule: size_option>   
        -size <[size=pos_integer]> (?: x <[size=pos_integer]> )?

Here, the sizes are parsed using the C<pos_integer> rule, but saved in the
result-hash in an array under the key C<'size'>.


=head2 Pseudo-subrules

Aliases can also be given to standard Perl subpatterns, as well as to 
code blocks within a regex. The syntax for subpatterns is:

    <ALIAS= (SUBPATTERN) >

In other words, the syntax is exactly like an aliased subrule call, except
that the rule name is replaced with a set of parentheses containing the
subpattern. Any parentheses--capturing or non-capturing--will do.

The effect of aliasing a standard subpattern is to cause whatever that
subpattern matches to be saved in the result-hash, using the alias as
its key. For example:

    <rule: file_command>

        <cmd=(mv|cp|ln)>  <from=file>  <to=file>

Here, the C<< <cmd=(mv|cp|ln)> >> is treated exactly like a regular
C<(mv|cp|ln)>, but whatever substring it matches is saved in the result-hash
under the key C<'cmd'>.

The syntax for aliasing code blocks is:

    <ALIAS= (?{ your($code->here) }) >

Note, however, that the code block must be specified in the standard Perl 5.10
regex notation: C<(?{...})>. A common mistake is to write:

    <ALIAS= { your($code->here } >

instead, which will attempt to interpolate C<$code> before
the regex is even compiled, as such variables are only "protected" from
interpolation inside a C<< (?{...}) >>.

When correctly specified, this construct executes the code in the block
and saves the result of that execution in the result-hash, using the
alias as its key. Aliased code blocks are useful for adding semantic
information based on which branch of a rule is executed. For example,
consider the C<copy_cmd> alternatives shown earlier:

    <rule: copy_cmd>
        copy <from=file>        <to=file>
      | dup    <to=file>  as  <from=file>
      |      <from=file>  ->    <to=file>
      |        <to=file>  <-  <from=file>

Using aliased code blocks, you could add an extra field to the result-
hash to describe which form of the command was detected, like so:

    <rule: copy_cmd>
        copy <from=file>        <to=file>  <type=(?{ 'std' })> 
      | dup    <to=file>  as  <from=file>  <type=(?{ 'rev' })> 
      |      <from=file>  ->    <to=file>  <type=(?{ 'fwd' })> 
      |        <to=file>  <-  <from=file>  <type=(?{ 'bwd' })> 

Now, if the rule matched, the result-hash would contain something like:

    copy_cmd => {
        from => 'oldfile',
          to => 'newfile',
        type => 'fwd',
    }


Note that, in addition to the semantics described above, aliased
subpatterns and code blocks also become visible to Regexp::Grammars'
integrated debugger (see L<Debugging>).


=head2 Amnesiac subrule calls

By default, every subrule call saves its result into the result-hash, either
under its own name, or under an alias.

However, sometimes you may want to refactor some literal part of a rule
into one or more subrules, without having those submatches added to the
result-hash. The syntax for calling a subrule, but ignoring its return value
is:

    <.SUBRULE>

(which is stolen directly from Perl 6).

For example, you may prefer to rewrite a rule such as:

    <rule: paren_pair> 

        \( 
            (?: <escape> | <paren_pair> | <brace_pair> | [^()] )*
        \)

without any literal matching, like so:

    <rule: paren_pair> 

        <.left_paren>
            (?: <escape> | <paren_pair> | <brace_pair> | <.non_paren> )*
        <.right_paren>
    
    <token: left_paren>   \(
    <token: right_paren>  \)
    <token: non_paren>    [^()]

Moreover, as the individual components inside the parentheses probably
aren't being captured for any useful purpose either, you could further
optimize that to:

    <rule: paren_pair> 

        <.left_paren>
            (?: <.escape> | <.paren_pair> | <.brace_pair> | <.non_paren> )*
        <.right_paren>
    

Note that you can also use the dot modifier on an aliased subpattern:

    <.Alias= (SUBPATTERN) >

This seemingly contradictory behaviour (of giving a subpattern a name,
then deliberately ignoring that name) actually does make sense in one
situation. Providing the alias makes the subpattern visible to the
debugger, while using the dot stops it from affecting the result-hash.
See L<"Debugging non-grammars"> for an example of this usage.


=head2 Private subrule calls

If a rule name (or an alias) begins with an underscore:

     <_RULENAME>       <_ALIAS=RULENAME>  
    <[_RULENAME]>     <[_ALIAS=RULENAME]>

then matching proceeds as normal, and any result that is returned is
stored in the current result-hash in the usual way.

However, when any rule finishes (and just before it returns) it first
filters its result-hash, removing any entries whose keys begin with an
underscore. This means that any subrule with an underscored name (or
with an underscored alias) remembers its result, but only until the end
of the current rule. Its results are effectively private to the current
rule.

This is especially useful in conjunction with
L<result distillation|"Result distillation">.


=head2 Matching separated lists

One of the commonest tasks in text parsing is to match a list of unspecified
length, in which items are separated by a fixed token. Things like:

    1, 2, 3 , 4 ,13, 91        # Numbers separated by commas and spaces

    g-c-a-g-t-t-a-c-a          # Bases separated by dashes

    /usr/local/bin             # Names separated by directory markers

    /usr:/usr/local:bin        # Directories separated by colons

The usual construct required to parse these kinds of structures is either:

    <rule: list>

        <item> <separator> <list               # recursive definition
      | <item>                                 # base case

Or, more efficiently, but less prettily:

    <rule: list>

        <[item]> (?: <separator> <[item]> )*   # iterative definition

Because this is such a common requirement, Regexp::Grammars provides a cleaner
way to specify the iterative version. The syntax is taken from Perl 6:

    <rule: list>

        <[item]> ** <separator>                # iterative definition

This is a repetition specifier on the first subrule (hence the use of C<**>
as the marker, to reflect the repetitive behaviour of C<*>). However, the
number of repetitions is controlled by the second subrule: the first subrule
will be repeatedly matched for as long as the second subrule matches
immediately after it.

So, for example, you can match a sequence of numbers separated by commas with:

    <[number]> ** <comma>

    <token: number>  \d+
    <token: comma>   \s* , \s*
    
Note that it's important to use the C<< <[...]> >> form for the items being
matched, so that all of them are saved in the result hash. You can also save
all the separators (if that's important):

    <[number]> ** <[comma]>

The repeated item I<must> be specified as a subrule call fo some
kind, but the separators may be specified either as a subrule or a 
bracketed pattern. For example:

    <[number]> ** ( , )

The separator must always be specified in matched delimiters of some
kind: either matching C<< <...> >> or matching C<< (...) >>. A common
error is to write:

    <[number]> ** ,

You can also use a pattern as the item matcher, but it must be aliased into
a subrule:

    <[item=(\d+)]> ** ( , )


=head2 Matching hash keys

In some situations a grammar may need a rule that matches dozens,
hundreds, or even thousands of one-word alternatives. For example, when
matching command names, or valid userids, or English words. In such
cases it is often impractical (and always inefficient) to list all the
alternatives between C<|> alterators:

    <rule: shell_cmd>
        a2p | ac | apply | ar | automake | awk | ...
        # ...and 400 lines later
        ... | zdiff | zgrep | zip | zmore | zsh

    <rule: valid_word>
        a | aa | aal | aalii | aam | aardvark | aardwolf | aba | ...
        # ...and 40,000 lines later... 
        ... | zymotize | zymotoxic | zymurgy | zythem | zythum

To simplify such cases, Regexp::Grammars provides a special construct
that allows you to specify all the alternatives as the keys of a normal
hash. The syntax for that construct is simply to put the hash name
inside angle brackets (with no space between the angles and the hash name).

Which means that the rules in the previous example could also be written:

    <rule: shell_cmd>
        <%cmds>

    <rule: valid_word>
        <%dict>

provided that the two hashes (C<%cmds> and C<%dict>) are visible in the scope
where the grammar is created.

Internally, the construct is converted to something equivalent to:

    <rule: shell_cmd>
        (<.hk>)  <require: exists $cmds{$CAPTURE}>

    <rule: valid_word>
        (<.hk>)  <require: exists $dict{$CAPTURE}>

The special C<< <hk> >> rule is created automatically, and defaults to
C<\S+>, but you can also define it explicitly to handle other kinds of 
keys. For example:

    <rule: hk>
        .+            # Key may be any number of chars on a single line

    <rule: hk>
        [ACGT]{10,}   # Key is a base sequence of at least 10 pairs

Matching a hash key in this way is typically I<significantly> faster
than matching a full set of alternations. Specifically, it is
I<O(length of longest potential key)>, instead of I<O(number of keys)>.


=head1 Common parsing techniques

=head2 Result distillation

Normally, calls to subrules produce nested result-hashes within the
current result-hash. Those nested hashes always have at least one
automatically supplied key (C<"">), whose value is the entire substring
that the subrule matched.

If there are no other nested captures within the subrule, there will be
no other keys in the result-hash. This would be annoying as a typical
nested grammar would then produce results consisting of hashes of
hashes, with each nested hash having only a single key (C<"">). This in
turn would make postprocessing the result-hash (in C<%/>) far more
complicated than it needs to be.

To avoid this behaviour, if a subrule's result-hash doesn't contain any keys
except C<"">, the module "flattens" the result-hash, by replacing it with 
the value of its single key.

So, for example, the grammar:

    mv \s* <from> \s* <to>

    <rule: from>   [\w/.-]+
    <rule: to>     [\w/.-]+
 
I<doesn't> return a result-hash like this:

    {
        ""     => 'mv /usr/local/lib/libhuh.dylib  /dev/null/badlib',
        'from' => { "" => '/usr/local/lib/libhuh.dylib' },
        'to'   => { "" => '/dev/null/badlib'            },
    }

Instead, it returns:

    {
        ""     => 'mv /usr/local/lib/libhuh.dylib  /dev/null/badlib',
        'from' => '/usr/local/lib/libhuh.dylib',
        'to'   => '/dev/null/badlib',
    }

That is, because the C<'from'> and C<'to'> subhashes each have only a single
entry, they are each "flattened" to the value of that entry.

This flattening also occurs if a result-hash contains only "private" keys
(i.e. keys starting with underscores). For example:

    mv \s* <from> \s* <to>

    <rule: from>   <_dir=path>? <_file=filename>
    <rule: to>     <_dir=path>? <_file=filename>
    
    <token: path>      [\w/.-]*/
    <token: filename>  [\w.-]+

Here, the C<from> rule produces a result like this:

    from => {
          "" => '/usr/local/bin/perl',
        _dir => '/usr/local/bin/',
       _file => 'perl',
    }

which is automatically stripped of "private" keys, leaving:
        
    from => {
          "" => '/usr/local/bin/perl',
    }

which is then automatically flattened to:

    from => '/usr/local/bin/perl'


=head3 Manual result distillation

Regexp::Grammars also offers full manual control over the distillation
process. If you use the reserved word C<MATCH> as the alias for
a subrule call:

    <MATCH=filename>

or a subpattern match:

    <MATCH=( \w+ )>

or a code block:

    <MATCH=(?{ 42 })>

then the current rule will treat the return value of that subrule,
pattern, or code block as its complete result, and return that value
instead of the usual result-hash it constructs. This is the case even if
the result has other entries that would normally also be returned.

For example, in a rule like:

    <rule: term>
          <MATCH=literal>
        | <left_paren> <MATCH=expr> <right_paren>

The use of C<MATCH> aliases causes the rule to return either whatever
C<< <literal> >> returns, or whatever C<< <expr> >> returns (provided
it's between left and right parentheses).

Note that, in this second case, even though C<< <left_paren> >> and
C<< <right_paren> >> I<are> captured to the result-hash, they are
not returned, because the C<MATCH> alias overrides the normal "return
the result-hash" semantics and returns only what its associated
subrule (i.e. C<< <expr> >>) produces.


=head3 Programmatic result distillation

It's also possible to control what a rule returns from within a code block.
Regexp::Grammars provides a set of reserved variables that give direct
access to the result-hash.

The result-hash itself can be accessed as C<%MATCH> within any code block
inside a rule. For example:

    <rule: sum> 
        <X=product> \+ <Y=product>
            <MATCH=(?{ $MATCH{X} + $MATCH{Y} })>

Here, the rule matches a product (aliased C<'X'> in the result-hash),
then a literal C<'+'>, then another product (aliased to C<'Y'> in the
result-hash). The rule then executes the code block, which accesses the two
saved values (as C<$MATCH{X}> and C<$MATCH{Y}>), adding them together.
Because the block is itself aliased to C<MATCH>, the sum produced by the block
becomes the (only) result of the rule.

It is also possible to set the rule result from within a code block (instead
of aliasing it). The special "override" return value is represented by the
special variable C<$MATCH>. So the previous example could be rewritten:

    <rule: sum> 
        <X=product> \+ <Y=product>
            (?{ $MATCH = $MATCH{X} + $MATCH{Y} })

Both forms are identical in effect. Any assignment to C<$MATCH> overrides the
normal "return all subrule results" behaviour.

Assigning to C<$MATCH> directly is particularly handy if the result
may not always be "distillable", for example:

    <rule: sum> 
        <X=product> \+ <Y=product>
            (?{ if (!ref $MATCH{X} && !ref $MATCH{Y}) {
                    # Reduce to sum, if both terms are simple scalars...
                    $MATCH = $MATCH{X} + $MATCH{Y};
                }
                else {
                    # Return full syntax tree for non-simple case...
                    $MATCH{op} = '+';
                }
            })

Note that you can also partially override the subrule return behaviour.
Normally, the subrule returns the complete text it matched under the "empty
key" of its result-hash. That is, of course, C<$MATCH{""}>, so you can
override just that behaviour by directly assigning to that entry.

For example, if you have a rule that matches key/value pairs from a
configuration file, you might prefer that any trailing comments not be
included in the "matched text" entry of the rule's result-hash. You could
hide such comments like so:

    <rule: config_line>
        <key> : <value>  <comment>?
            (?{
                # Edit trailing comments out of "matched text" entry...
                $MATCH = "$MATCH{key} : $MATCH{value}";
            })

Some more examples of the uses of C<$MATCH>:

    <rule: FuncDecl>
      # Keyword  Name               Keep return the name (as a string)...
        func     <Identifier> ;     (?{ $MATCH = $MATCH{'Identifier'} })


    <rule: NumList>
      # Numbers in square brackets...
        \[ 
            ( \d+ (?: , \d+)* )
        \]

      # Return only the numbers...
        (?{ $MATCH = $CAPTURE })


    <token: Cmd>
      # Match standard variants then standardize the keyword...
        (?: mv | move | rename )      (?{ $MATCH = 'mv'; })


=head2 Parse-time data processing

Using code blocks in rules, it's often possible to fully process data as
you parse it. For example, the C<< <sum> >> rule shown in the previous section
might be part of a simple calculator, implemented entirely in a single
grammar. Such a calculator might look like this:


    my $calculator = do{
        use Regexp::Grammars;
        qr{
            <Answer>

            <rule: Answer>
                <X=Mult> \+ <Y=Answer>
                    <MATCH= (?{ $MATCH{X} + $MATCH{Y} })>
              | <X=Mult> - <Y=Answer>
                    <MATCH= (?{ $MATCH{X} - $MATCH{Y} })>
              |
                    <MATCH=Mult>

            <rule: Mult>
                <X=Pow> \* <Y=Mult>
                    <MATCH= (?{ $MATCH{X} * $MATCH{Y} })>
              | <X=Pow>  / <Y=Mult>
                    <MATCH= (?{ $MATCH{X} / $MATCH{Y} })>
              | <X=Pow>  % <Y=Mult>
                    <MATCH= (?{ $MATCH{X} % $MATCH{Y} })>
              |
                    <MATCH=Pow>

            <rule: Pow>
                <X=Term> \^ <Y=Pow>
                    <MATCH= (?{ $MATCH{X} ** $MATCH{Y}; })>
              |
                    <MATCH=Term>

            <rule: Term>
                    <MATCH=Literal>
              | \(  <MATCH=Answer>  \)

            <token: Literal>
                    <MATCH= ( [+-]? \d++ (?: \. \d++ )?+ )>
        }xms
    };

    while (my $input = <>) {
        if ($input =~ $calculator) {
            say "--> $/{Answer}";
        }
    }

Because every rule computes a value using the results of the subrules
below it, and aliases that result to its C<MATCH>, each rule returns a
complete evaluation of the subexpression it matches, passing that back
to higher-level rules, which then do the same.

Hence, the result returned to the very top-level rule (i.e. to C<<
<Answer> >>) is the complete evaluation of the entire expression that
was matched. That means that, in the very process of having matched a
valid expression, the calculator has also computed the value of that
expression, which can then simply be printed directly.

It is often possible to have a grammar fully (or sometimes at least
partially) evaluate or transform the data it is parsing, and this
usually leads to very efficient and easy-to-maintain implementations.

The main limitation of this technique is that the data has to be in a
well-structured form, where subsets of the data can be evaluated using
only local information. In cases where the meaning of the data is
distributed through that data non-hierarchically, or relies on global
state, or on external information, it is often better to have the grammar
simply construct a complete syntax tree for the data first, and then evaluate
that syntax tree separately, after parsing is complete. The following section
describes a feature of Regexp::Grammars that can make this second style of
data processing simpler and more maintainable.


=head2 Object-oriented parsing

When a grammar has parsed successfully, the C<%/> variable will contain a
series of nested hashes (and possibly arrays) representing the hierarchical
structure of the parsed data.

Typically, the next step is to walk that tree, extracting or
converting or otherwise processing that information. If the tree has nodes of
many different types, it can be difficult to build a recursive subroutine that
can navigate it easily.

A much cleaner solution is possible if the nodes of the tree are proper
objects.  In that case, you just define a C<process()> or C<traverse()> method
for eah of the classes, and have every node call that method on each of its
children. For example, if the parser were to return a tree of nodes
representing the contents of a LaTeX file, then you could define the following
methods:

    sub Latex::file::explain
    {
        my ($self, $level) = @_;
        for my $element (@{$self->{element}}) {
            $element->explain($level);
        }
    }

    sub Latex::element::explain {
        my ($self, $level) = @_;
        (  $self->{command} || $self->{literal})->explain($level)
    }

    sub Latex::command::explain {
        my ($self, $level) = @_;
        say "\t"x$level, "Command:";
        say "\t"x($level+1), "Name: $self->{name}";
        if ($self->{options}) {
            say "\t"x$level, "\tOptions:";
            $self->{options}->explain($level+2)
        }
        
        for my $arg (@{$self->{arg}}) {
            say "\t"x$level, "\tArg:";
            $arg->explain($level+2)
        }
    }

    sub Latex::options::explain {
        my ($self, $level) = @_;
        $_->explain($level) foreach @{$self->{option}};
    }

    sub Latex::literal::explain {
        my ($self, $level, $label) = @_;
        $label //= 'Literal';
        say "\t"x$level, "$label: ", $self->{q{}};
    }

and then simply write:

    if ($text =~ $LaTeX_parser) {
        $/{LaTeX_file}->explain();
    }

and the chain of C<explain()> calls would cascade down the nodes of the tree,
each one invoking the appropriate C<explain()> method according to the type of
node encountered.

The only problem is that, by default, Regexp::Grammars returns a tree of
plain-old hashes, not LaTeX::whatever objects. Fortunately, it's easy to
request that the result hashes be automatically blessed into the appropriate
classes, using the C<< <objrule:...> >> and C<< <objtoken:...> >> directives.

These directives are identical to the C<< <rule:...> >> and C<<
<token:...> >> directives (respectively), except that the rule or
token they create will also bless the hash it normally returns,
converting it to an object of a class whose name is the same as the
rule or token itself.

For example:

    <objrule: Element>
        # ...Defines a rule that can be called as <Element>
        # ...and which returns a hash-based Element object

The C<IDENTIFIER> of the rule or token may also be fully qualified. In
such cases, the rule or token is defined using only the final "short name",
but the result object is blessed using the fully qualified "long name".
For example:

    <objrule: LaTeX::Element> 
        # ...Defines a rule that can be called as <Element>
        # ...and which returns a hash-based LaTeX::Element object

This can be useful to ensure that returned objects don't collide with
other namespaces in your program.

Note that you can freely mix object-returning and plain-old-hash-returning
rules and tokens within a single grammar, though you have to be careful
not to subsequently try to call a method on any of the unblessed nodes.


=head1 Debugging

Regexp::Grammars provides a number of features specifically designed to help
debug both grammars and the data they parse.

All debugging messages are written to a log file (which, by default, is
just STDERR). However, you can specify a disk file explicitly by placing a
C<< <logfile:...> >> directive at the start of your grammar:

    $grammar = qr{

        <logfile: LaTeX_parser_log >

        \A <LaTeX_file> \Z    # Pattern to match

        <rule: LaTeX_file>
            # etc.
    }x;

You can also explicitly specify that messages go to the terminal:

        <logfile: - >


=head2 Debugging grammar creation with C<< <logfile:...> >>

Whenever a log file has been directly specified,
Regexp::Grammars automatically does verbose static analysis of your grammar.
That is, whenever it compiles a grammar containing an explicit
C<< <logfile:...> >> directive it logs a series of messages explaining how it
has interpreted the various components of that grammar. For example, the
following grammar:

    <logfile: parser_log > 

    <cmd>

    <rule: cmd>
        mv <from=file> <to=file>
      | cp <source> <[file]>  <.comment>?

would produce the following analysis in the 'parser_log' file:

    info | Processing the main regex before any rule definitions
         |    |
         |    |...Treating <cmd> as:
         |    |      |  match the subrule <cmd> 
         |    |       \ saving the match in $MATCH{'cmd'}
         |    |
         |     \___End of main regex
         | 
    info | Defining a rule: <cmd>
         |    |...Returns: a hash
         |    |
         |    |...Treating ' mv ' as:
         |    |       \ normal Perl regex syntax
         |    |
         |    |...Treating <from=file> as:
         |    |      |  match the subrule <file> 
         |    |       \ saving the match in $MATCH{'from'}
         |    |
         |    |...Treating <to=file> as:
         |    |      |  match the subrule <file> 
         |    |       \ saving the match in $MATCH{'to'}
         |    |
         |    |...Treating ' | cp ' as:
         |    |       \ normal Perl regex syntax
         |    |
         |    |...Treating <source> as:
         |    |      |  match the subrule <source> 
         |    |       \ saving the match in $MATCH{'source'}
         |    |
         |    |...Treating <[file]> as:
         |    |      |  match the subrule <file> 
         |    |       \ appending the match to $MATCH{'file'}
         |    |
         |    |...Treating <.comment>? as:
         |    |      |  match the subrule <comment> if possible
         |    |       \ but don't save anything
         |    |
         |     \___End of rule definition

This kind of static analysis is a useful starting point in debugging a
miscreant grammar, because it enables you to see what you actually
specified (as opposed to what you I<thought> you'd specified).


=head2 Debugging grammar execution with C<< <debug:...> >>

Regexp::Grammars also provides a simple interactive debugger, with which you
can observe the process of parsing and the data being collected in any
result-hash.

To initiate debugging, place a C<< <debug:...> >> directive anywhere in your
grammar. When parsing reaches that directive the debugger will be activated,
and the command specified in the directive immediately executed. The available
commands are:

    <debug: on>    - Enable debugging, stop when entire grammar matches
    <debug: match> - Enable debugging, stope when a rule matches
    <debug: try>   - Enable debugging, stope when a rule is tried
    <debug: off>   - Disable debugging and continue parsing silently

    <debug: continue> - Synonym for <debug: on>
    <debug: run>      - Synonym for <debug: on>
    <debug: step>     - Synonym for <debug: try>

These directives can be placed anywhere within a grammar and take effect
when that point is reached in the parsing. Hence, adding a
C<< <debug:step> >> directive is very much like setting a breakpoint at that
point in the grammar. Indeed, a common debugging strategy is to turn
debugging on and off only around a suspect part of the grammar:

    <rule: tricky>   # This is where we think the problem is...
        <debug:step>
        <preamble> <text> <postscript>
        <debug:off>

Once the debugger is active, it steps through the parse, reporting rules
that are tried, matches and failures, backtracking and restarts, and the
parser's location within both the grammar and the text being matched. That
report looks like this:

    ===============> Trying <grammar> from position 0
    > cp file1 file2 |...Trying <cmd>   
                     |   |...Trying <cmd=(cp)>  
                     |   |    \FAIL <cmd=(cp)>
                     |    \FAIL <cmd>
                      \FAIL <grammar>
    ===============> Trying <grammar> from position 1
     cp file1 file2  |...Trying <cmd>   
                     |   |...Trying <cmd=(cp)>  
     file1 file2     |   |    \_____<cmd=(cp)> matched 'cp' 
    file1 file2      |   |...Trying <[file]>+   
     file2           |   |    \_____<[file]>+ matched 'file1'   
                     |   |...Trying <[file]>+   
    [eos]            |   |    \_____<[file]>+ matched ' file2'  
                     |   |...Trying <[file]>+   
                     |   |    \FAIL <[file]>+
                     |   |...Trying <target>    
                     |   |   |...Trying <file>
                     |   |   |    \FAIL <file>
                     |   |    \FAIL <target>
     <~~~~~~~~~~~~~~ |   |...Backtracking 5 chars and trying new match
    file2            |   |...Trying <target>    
                     |   |   |...Trying <file>
                     |   |   |    \____ <file> matched 'file2'
    [eos]            |   |    \_____<target> matched 'file2'    
                     |    \_____<cmd> matched ' cp file1 file2' 
                      \_____<grammar> matched ' cp file1 file2' 

The first column indicates the point in the input at which the parser is
trying to match, as well as any backtracking or forward searching it may
need to do. The remainder of the columns track the parser's hierarchical
traversal of the grammar, indicating which rules are tried, which
succeed, and what they match.

Provided the logfile is a terminal (as it is by default), the debugger
also pauses at various points in the parsing process--before trying a
rule, after a rule succeeds, or at the end of the parse--according to
the most recent command issued. When it pauses, you can issue a new
command by entering a single letter:

    m       - to continue until the next subrule matches
    t or s  - to continue until the next subrule is tried
    r or c  - to continue to the end of the grammar
    o       - to switch off debugging

Note that these are the first letters of the corresponding
C<< <debug:...> >> commands, listed earlier. Just hitting ENTER while the
debugger is paused repeats the previous command.

While the debugger is paused you can also type a 'd', which will display
the result-hash for the current rule. This can be useful for detecting
which rule isn't returning the data you expected.


=head2 User-defined logging with C<< <log:...> >>

Both static and interactive debugging send a series of predefined log messages
to whatever log file you have specified. It is also possible to send
additional, user-defined messages to the log, using the C<< <log:...> >>
directive.

This directive expects either a simple text or a codeblock as its single
argument. If the argument is a code block, that code is expected to
return the text of the message; if the argument is anything else, that
something else I<is> the literal message. For example:

    <rule: ListElem>

        <Elem=   ( [a-z]\d+) >
            <log: Checking for a suffix, too...>

        <Suffix= ( : \d+   ) >?
            <log: (?{ "ListElem: $MATCH{Elem} and $MATCH{Suffix}" })>

User-defined log messages implemented using a codeblock can also specify
a severity level. If the codeblock of a C<< <log:...> >> directive
returns two or more values, the first is treated as a log message
severity indicator, and the remaining values as separate lines of text
to be logged. For example:

    <rule: ListElem>
        <Elem=   ( [a-z]\d+) >
        <Suffix= ( : \d+   ) >?

            <log: (?{
                warn => "Elem was: $MATCH{Elem}", 
                        "Suffix was $MATCH{Suffix}",
            })>

When they are encountered, user-defined log messages are interspersed
between any automatic log messages (i.e. from the debugger), at the correct
level of nesting for the current rule.


=head2 Debugging non-grammars

It is possible to use Regexp::Grammars without creating I<any> subrule
definitions, simply to debug a recalcitrant regex. For example, if the
following regex wasn't working as expected:

    my $balanced_brackets = qr{
        \(             # left delim
        (?:
            \\         # escape or
        |   (?R)       # recurse or
        |   .          # whatever
        )*
        \)             # right delim
    }xms;

you could instrument it with aliased subpatterns and then debug it
step-by-step, using Regexp::Grammars:

    use Regexp::Grammars;

    my $balanced_brackets = qr{
        <debug:step>

        <.left_delim=  (  \(  )>
        (?:
            <.escape=  (  \\  )>
        |   <.recurse= ( (?R) )>
        |   <.whatever=(  .   )>
        )*
        <.right_delim= (  \)  )>
    }xms;

    while (<>) {
        say 'matched' if /$balanced_brackets/;
    }

Note the use of L<amnesiac aliased subpatterns|"Amnesiac subrule calls">
to avoid needlessly building a result-hash. Alternatively, you could use
listifying aliases to preserve the matching structure as an additional
debugging aid:

    use Regexp::Grammars;

    my $balanced_brackets = qr{
        <debug:step>

        <[left_delim=  (  \(  )]>
        (?:
            <[escape=  (  \\  )]>
        |   <[recurse= ( (?R) )]>
        |   <[whatever=(  .   )]>
        )*
        <[right_delim= (  \)  )]>
    }xms;

    if ( '(a(bc)d)' =~ /$balanced_brackets/) {
        use Data::Dumper 'Dumper';
        warn Dumper \%/;
    }


=head1 Handling errors when parsing

Assuming you have correctly debugged your grammar, the next source of problems
will likely be invalid input (especially if that input is being provided
interactively). So Regexp::Grammars also provides some support for detecting
when a parse is likely to fail...and informing the user why.

=head2 Requirements

The C<< <require:...> >> directive is useful for testing conditions
that it's not easy (or even possible) to check within the syntax of the
the regex itself. For example:

    <rule: IPV4_Octet_Decimal>
        # Up three digits...
        <MATCH= ( \d{1,3}+ )>
        
        # ...but less that 256...
        <require: (?{ $MATCH <= 255 })>

A require expects a regex codeblock as its argument and succeeds if the final
value of that codeblock is true. If the final value is false, the directive
fails and the rule starts backtracking.

Note, in this example that the digits are matched with C< \d{1,3}+ >. The
trailing C<+> prevents the C<{1,3}> repetition from backtracking to a smaller
number of digits if the C<< <require:...> >> fails.


=head2 Error messages

The module has limited support for error reporting from within a grammar,
in the form of the C<< <error:...> >> and C<< <warning:...> >> directives.

The C<< <error: MSG> >> directive queues a I<conditional> error message
within C<@!> and then fails to match (that is, it is equivalent to a
C<(?!)> when matching). For example:

    <rule: ListElem>
        <SerialNumber>
      | <ClientName>
      | <error: (?{ $errcount++ . ': Missing list element' })>

The error message is conditional in the sense that, if any surrounding rule
subsequently matches, the message is automatically removed from C<@!>. This
implies that you can queue up as many error messages as you wish, but they
will only remain in C<@!> if the match ultimately fails. Moreover, only those
error messages originating from rules that actually contributed to the
eventual failure-to-match will remain in C<@!>.

If a code block is specified as the argument, the error message is whatever
final value is produced when the block is executed. Note that this final value
does not have to be a string (though it does have to be a scalar).

    <rule: ListElem>
        <SerialNumber>
      | <ClientName>
      | <error: (?{
            # Return a hash, with the error information...
            { errnum => $errcount++, msg => 'Missing list element' }
        })>

If anything else is specified as the argument, it is treated as a
literal error string (and may not contain an unbalanced C<< '<' >>
or C<< '>' >>, nor any interpolated variables).

However, if the literal error string begins with "Expected " or
"Expecting ", then the error string automatically has the following
"context suffix" appended:

    , but found '$CONTEXT' instead

For example:

    qr{ <Arithmetic_Expression>                # ...Match arithmetic expression
      |                                        # Or else
        <error: Expected a valid expression>   # ...Report error, and fail

        # Rule definitions here...
    }xms;

On an invalid input this example might produce an error message like:

    "Expected a valid expression, but found '(2+3]*7/' instead"

The value of the special $CONTEXT variable is found by looking ahead in
the string being matched against, to locate the next sequence of non-blank
characters after the current parsing position. This variable may also be
explicitly used within the C<< <error: (?{...})> >> form of the directive.

As a special case, if you omit the message entirely from the directive,
it is supplied automatically, derived from the name of the current rule.
For example, if the following rule were to fail to match:

    <rule: Arithmetic_expression>
          <Multiplicative_Expression> ** ([+-])
        | <error:>

the error message queued would be:

    "Expected arithmetic expression, but found 'one plus two' instead"

Note however, that it is still essential to include the colon in the
directive. A common mistake is to write:

    <rule: Arithmetic_expression>
          <Multiplicative_Expression> ** ([+-])
        | <error>

which merely attempts to call C<< <rule: error> >> if the first
alternative fails.


=head2 Warning messages

Sometimes, you want to detect problems, but not invalidate the entire
parse as a result. For those occasions, the module provides a "less stringent"
form of error reporting: the C<< <warning:...> >> directive.

This directive is exactly the same as an C<< <error:...> >> in every respect
except that it does not induce a failure to match at the point it appears.

The directive is, therefore, useful for reporting I<non-fatal> problems
in a parse. For example:

    qr{ \A            # ...Match only at start of input
        <ArithExpr>   # ...Match a valid arithmetic expression

        (?:
            # Should be at end of input...
            \s* \Z                            
          |
            # If not, report the fact but don't fail...
            <warning: Expected end-of-input> 
            <warning: (?{ "Extra junk at index $INDEX: $CONTEXT" })>
        )

        # Rule definitions here...
    }xms;

Note that, because they do not induce failure, two or more
C<< <warning:...> >> directives can be "stacked" in sequence,
as in the previous example.



=head1 Scoping considerations

If you intend to use a grammar as part of a larger program that contains
other (non-grammatical) regexes, it is more efficient--and less
error-prone--to avoid having Regexp::Grammars process those regexes as
well. So it's often a good idea to declare your grammar in a C<do>
block, thereby restricting the scope of the module's effects.

For example:

    my $grammar = do {
        use Regexp::Grammars;
        qr{
            <file>

            <rule: file>
                <prelude>
                <data>
                <postlude>

            <rule: prelude>
                # etc.
        }x;
    };

Because the effects of Regexp::Grammars are lexically scoped, any regexes
defined outside that C<do> block will be unaffected by the module.



=head1 INTERFACE 

=head2 Perl API

=over 4

=item C<use Regexp::Grammars;>

Causes all regexes in the current lexical scope to be compile-time processed
for grammar elements.

=item C<$str =~ $grammar>

=item C<$str =~ /$grammar/>

Attempt to match the grammar against the string, building a nested data
structure from it.

=item C<%/>

This hash is assigned the nested data structure created by any successful
match of a grammar regex.

=item C<@!>

This array is assigned the queue of error messages created by any
unsuccessful match attempt of a grammar regex.

=back


=head2 Grammar syntax

=head3 Directives

=over 4

=item C<< <rule: IDENTIFIER> >>

Define a rule whose name is specified by the supplied identifier.

Everything following the C<< <rule:...> >> directive
(up to the next C<< <rule:...> >> or C<< <token:...> >> directive) is
treated as part of the rule being defined.

Any whitespace in the rule is replaced by a call to the C<< <.ws> >>
subrule (which defaults to matching C<\s*>, but may be explicitly redefined).


=item C<< <token: IDENTIFIER> >>

Define a rule whose name is specified by the supplied identifier.

Everything following the C<< <token:...> >> directive (up to the next
C<< <rule:...> >> or C<< <token:...> >> directive) is treated as part
of the rule being defined.

Any whitespace in the rule is ignored (under the C</x> modifier), or
explicitly matched (if C</x> is not used).

=item C<< <objrule:  IDENTIFIER> >>

=item C<< <objtoken: IDENTIFIER> >>

Identical to a C<< <rule: IDENTIFIER> >> or C<< <token: IDENTIFIER> >>
declaration, except that the rule or token will also bless the hash it
normally returns, converting it to an object of a class whose name is
the same as the rule or token itself.


=item C<< <require: (?{ CODE })  > >>

The code block is executed and if its final value is true, matching continues
from the same position. If the block's final value is false, the match fails at
that point and starts backtracking.


=item C<< <error: (?{ CODE })  > >>

=item C<< <error: LITERAL TEXT > >>

=item C<< <error: > >>

This directive queues a I<conditional> error message within C<@!> and
then fails to match (that is, it is equivalent to a C<(?!)> when
matching). 

=item C<< <warning: (?{ CODE })  > >>

=item C<< <warning: LITERAL TEXT > >>

This directive is exactly the same as an C<< <error:...> >> in every
respect except that it does not induce a failure to match at the point
it appears. That is, it is equivalent to a C<(?=)> ["succeed and
continue matching"], rather than a C<(?!)> ["fail and backtrack"].


=item C<< <debug: COMMAND > >>

During the matching of grammar regexes send debugging and warning
information to the specified log file (see C<< <logfile: LOGFILE> >>).

The available C<COMMAND>'s are:

    <debug: continue>    ___ Debug until end of complete parse
    <debug: run>         _/

    <debug: on>          ___ Debug until next subrule match
    <debug: match>       _/

    <debug: try>         ___ Debug until next subrule call or match
    <debug: step>        _/

    <debug: off>         ___ No debugging

See also the C<$DEBUG> special variable.


=item C<< <logfile: LOGFILE> >>

=item C<< <logfile:    -   > >>

During the compilation of grammar regexes, send debugging and warning
information to the specified LOGFILE (or to C<*STDERR> if C<-> is
specified).

If the specified LOGFILE name contains a C<%t>, it is replaced with a
(sortable) "YYYYMMDD.HHMMSS" timestamp. For example:

    <logfile: test-run-%t >

executed at around 9.30pm on the 21st of March 2009, would generate a
log file named: C<test-run-20090321.213056>


=item C<< <log: (?{ CODE })  > >>

=item C<< <log: LITERAL TEXT > >>

Append a message to the log file. If the argument is a code block,
that code is expected to return the text of the message; if the
argument is anything else, that something else I<is> the literal
message. 

If the block returns two or more values, the first is treated as a log
message severity indicator, and the remaining values as separate lines
of text to be logged.

=back


=head3 Subrule calls

=over 4

=item C<< <IDENTIFIER> >>

Call the subrule whose name is IDENTIFIER.

If it matches successfully, save the hash it returns in the current
scope's result-hash, under the key C<'IDENTIFIER'>.


=item C<< <IDENTIFIER_1=IDENTIFIER_2> >>

Call the subrule whose name is IDENTIFIER_1.

If it matches successfully, save the hash it returns in the current
scope's result-hash, under the key C<'IDENTIFIER_2'>.

In other words, the C<IDENTIFIER_1=> prefix changes the key under which the
result of calling a subrule is stored.


=item C<< <.IDENTIFIER> >>

Call the subrule whose name is IDENTIFIER.
Don't save the hash it returns.

In other words, the "dot" prefix disables saving of subrule results.


=item C<< <IDENTIFIER= ( PATTERN )> >>

Match the subpattern PATTERN.

If it matches successfully, capture the substring it matched and save
that substring in the current scope's result-hash, under the key
'IDENTIFIER'.


=item C<< <.IDENTIFIER= ( PATTERN )> >>

Match the subpattern PATTERN.
Don't save the substring it matched.


=item C<< <IDENTIFIER= %HASH> >>

Match a sequence of non-whitespace then verify that the sequence is a
key in the specified hash

If it matches successfully, capture the sequence it matched and save
that substring in the current scope's result-hash, under the key
'IDENTIFIER'.


=item C<< <%HASH> >>

Match a key from the hash.
Don't save the substring it matched.


=item C<< <IDENTIFIER= (?{ CODE })> >>

Execute the specified CODE.

Save the result (of the final expression that the CODE evaluates) in the
current scope's result-hash, under the key C<'IDENTIFIER'>.


=item C<< <[IDENTIFIER]> >>

Call the subrule whose name is IDENTIFIER.

If it matches successfully, append the hash it returns to a nested array
within the current scope's result-hash, under the key <'IDENTIFIER'>.


=item C<< <[IDENTIFIER_1=IDENTIFIER_2]> >>

Call the subrule whose name is IDENTIFIER_1.

If it matches successfully, append the hash it returns to a nested array
within the current scope's result-hash, under the key C<'IDENTIFIER_2'>.


=item C<< <ANY_SUBRULE> ** <ANY_OTHER_SUBRULE> >>

=item C<< <ANY_SUBRULE> ** (PATTERN) >>

Repeatedly call the first subrule.
Keep matching as long as the subrule matches, provided successive
matches are separated by matches of the second subrule or the pattern.

In other words, match a list of ANY_SUBRULE's separated by
ANY_OTHER_SUBRULE's or PATTERN's.

Note that, if a pattern is used to specify the separator, it must be
specified in some kind of matched parentheses. These may be capturing
[C<(...)>], non-capturing [C<(?:...)>], non-backtracking [C<< (?>...) >>],
or any other construct enclosed by an opening and closing paren.

=back

=head2 Special variables within grammar actions

=over 4

=item C<$CAPTURE>

=item C<$CONTEXT>

These are both aliases for the built-in read-only C<$^N> variable, which
always contains the substring matched by the nearest preceding C<(...)>
capture. C<$^N> still works perfectly well, but these are provided to
improve the readability of code blocks and error messages respectively.

=item C<$INDEX>

This variable contains the index at which the next match will be attempted
within the string being parsed. It is most commonly used in C<< <error:...> >>
or C<< <log:...> >> directives:

    <rule: ListElem>
        <log: (?{ "Trying words at index $INDEX" })>
        <MATCH=( \w++ )>
      |
        <log: (?{ "Trying digits at index $INDEX" })>
        <MATCH=( \d++ )>
      |
        <error: (?{ "Missing ListElem near index $INDEX" })>



=item C<%MATCH>

This variable contains all the saved results of any subrules called from the
current rule. In other words, subrule calls like:

    <ListElem>  <Separator= (,)>

stores their respective match results in C<$MATCH{'ListElem'}> and
C<$MATCH{'Separator'}>.


=item C<$MATCH>

This variable is an alias for C<$MATCH{"="}>. This is the C<%MATCH>
entry for the special "override value". If this entry is defined, its
value overrides the usual "return \%MATCH" semantics of a successful
rule.


=item C<$_>

At the start of any code blocks inside any regex, the variable C<$_> contains
the complete string being matched against. The current matching position
within that string is given by: C<pos($_)>.

=item C<$DEBUG>

This variable stores the current debugging mode (which may be any of:
C<'off'>, C<'on'>, C<'run'>, C<'continue'>, C<'match'>, C<'step'>, or
C<'try'>). It is set automatically by the C<< <debug:...> >> command, but may
also be set manually in a code block (which can be useful for conditional
debugging). For example:

    <rule: ListElem>
        <Identifier>

        # Conditionally debug if 'foobar' encountered...
        (?{ $DEBUG = $MATCH{Identifier} eq 'foobar' ? 'step' : 'off' })

        <Modifier>?

See also: the C<< <log: LOGFILE> >> and C<< <debug: DEBUG_CMD> >> directives.

=back


=head1 IMPORTANT CONSTRAINTS AND LIMITATIONS

=over 4

=item *

The additional regex constructs this module provides are implemented by
rewriting regular expressions. This is a (safer) form of source
filtering, but still subject to all the same limitations and
fallibilities of any other macro-based solution.

=item *

In particular, rewriting the macros involves the insertion of (a lot of)
extra capturing parentheses. This means you can no longer assume that
particular capturing parens correspond to particular numeric variables:
i.e. to C<$1>, C<$2>, C<$3> etc. If you want to capture directly use
Perl 5.10's named capture construct:

    (?<name> [^\W\d]\w* )

Better still, capture the data in its correct hierarchical context
using the module's "named subpattern" construct:

    <name= ([^\W\d]\w*) >
    

=item * 

No recursive descent parser--including those created with
Regexp::Grammars--can directly handle left-recursive grammars with rules
of the form:

    <rule: List>
        <List> , <ListElem>
        
If you find yourself attempting to write a left-recursive grammar (which
Perl 5.10 may or may not complain about, but will never successfully
parse with), then you probably need to use the "separated list"
construct instead:

    <rule: List>
        <[ListElem]> ** (,)

=item *

Grammatical parsing with Regexp::Grammars can fail if your grammar
places "non-backtracking" directives (i.e. the C<< (?>...) >> block or
the C<?+>, C<*+>, or C<++> repetition specifiers) around a subrule call.
The problem appears to be that preventing the regex from backtracking
through the in-regex actions that Regexp::Grammars adds causes the
module's internal stack to fall out of sync with the regex match.

For the time being, you need to make sure that grammar rules don't appear
inside a "non-backtracking" directive.

=back

=head1 DIAGNOSTICS

Note that (because the author cannot find a way to throw exceptions from
within a regex) none of the following diagnostics actually throws an
exception.

Instead, these messages are simply written to the specified parser logfile
(or to *STDERR, if no logfile is specified).

However, any fatal match-time message will immediately terminate the
parser matching and will still set C<$@> (as if an exception had been
thrown and caught at that point in the code). You then have the option
to check C<$@> immediately after matching with the grammar, and rethrow if
necessary:

    if ($input =~ $grammar) {
        process_data_in(\%/);
    }
    else {
        die if $@;
    }

=over

=item C<< Found call to %s, but no %s was defined in the grammar >>

You specified a call to a subrule for which there was no definition in
the grammar. Typically that's either because you forget to define the
rule, or because you misspelled either the definition or the subrule
call. For example:

    <file>

    <rule: fiel>            <---- misspelled rule
        <lines>             <---- used but never defined

Regexp::Grammars converts any such subrule call attempt to an instant
catastrophic failure of the entire parse, so if your parser ever
actually tries to perform that call, Very Bad Things will happen.


=item C<< Entire parse terminated prematurely while attempting to call non-existent rule: %s >>

You ignored the previous error and actually tried to call to a subrule
for which there was no definition in the grammar. Very Bad Things are
now happening. The parser got very upset, took its ball, and went home.
See the preceding diagnostic for remedies.

This diagnostic should throw an exception, but can't. So it sets C<$@>
instead, allowing you to trap the error manually if you wish.


=item C<< Possible invalid subrule call %s >>

Your grammar contained something of the form:
    
    <identifier
    <.identifier
    <[identifier

which you might have intended to be a subrule call, but which didn't
correctly parse as one. If it was supposed to be a Regexp::Grammars
subrule call, you need to check the syntax you used. If it wasn't
supposed to be a subrule call, you can silence the warning by rewriting
it and quoting the leading angle:

    \<identifier
    \<.identifier
    \<[identifier


=item C<< Possible invalid directive: %s >>

Your grammar contained something of the form:
    
    <identifier:

but which wasn't a known directive like C<< <rule:...> >>
or C<< <debug:...> >>. If it was supposed to be a Regexp::Grammars
directive, check the spelling of the directive name. If it wasn't
supposed to be a directive, you can silence the warning by rewriting it
and quoting the leading angle:

    \<identifier:


=item C<< Repeated subrule %s will only capture its final match >>

You specified a subrule call with a repetition qualifier, such as:

    <ListElem>*

or:

    <ListElem>+

Because each subrule call saves its result in a hash entry of the same name, 
each repeated match will overwrite the previous ones, so only the last match
will ultimately be saved. If you want to save all the matches, you need to
tell Regexp::Grammars to save the sequence of results as a nested array within
the hash entry, like so:

    <[ListElem]>*

or:

    <[ListElem]>+

If you really did intend to throw away every result but the final one, you can
silence the warning by placing the subrule call inside any kind of
parentheses. For example:

    (<ListElem>)*

or:

    (?: <ListElem> )+

=back


=head1 CONFIGURATION AND ENVIRONMENT

Regexp::Grammars requires no configuration files or environment variables.


=head1 DEPENDENCIES

This module only works under Perl 5.10 or later.


=head1 INCOMPATIBILITIES

This module is likely to be incompatible with any other module that
automagically rewrites regexes. For example it may conflict with
Regexp::DefaultFlags, Regexp::DeferredExecution, or Regexp::Extended.


=head1 BUGS

No bugs have been reported.

Please report any bugs or feature requests to
C<bug-regexp-grammars@rt.cpan.org>, or through the web interface at
L<http://rt.cpan.org>.


=head1 AUTHOR

Damian Conway  C<< <DCONWAY@CPAN.org> >>


=head1 LICENCE AND COPYRIGHT

Copyright (c) 2009, Damian Conway C<< <DCONWAY@CPAN.org> >>. All rights reserved.

This module is free software; you can redistribute it and/or
modify it under the same terms as Perl itself. See L<perlartistic>.


=head1 DISCLAIMER OF WARRANTY

BECAUSE THIS SOFTWARE IS LICENSED FREE OF CHARGE, THERE IS NO WARRANTY
FOR THE SOFTWARE, TO THE EXTENT PERMITTED BY APPLICABLE LAW. EXCEPT WHEN
OTHERWISE STATED IN WRITING THE COPYRIGHT HOLDERS AND/OR OTHER PARTIES
PROVIDE THE SOFTWARE "AS IS" WITHOUT WARRANTY OF ANY KIND, EITHER
EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE. THE
ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE SOFTWARE IS WITH
YOU. SHOULD THE SOFTWARE PROVE DEFECTIVE, YOU ASSUME THE COST OF ALL
NECESSARY SERVICING, REPAIR, OR CORRECTION.

IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING
WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MAY MODIFY AND/OR
REDISTRIBUTE THE SOFTWARE AS PERMITTED BY THE ABOVE LICENCE, BE
LIABLE TO YOU FOR DAMAGES, INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL,
OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE USE OR INABILITY TO USE
THE SOFTWARE (INCLUDING BUT NOT LIMITED TO LOSS OF DATA OR DATA BEING
RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD PARTIES OR A
FAILURE OF THE SOFTWARE TO OPERATE WITH ANY OTHER SOFTWARE), EVEN IF
SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF
SUCH DAMAGES.
