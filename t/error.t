use 5.010;
use warnings;
use Test::More 'no_plan';

use List::Util qw< reduce >;

my $calculator = do{
    use Regexp::Grammars;
    qr{ 
        \A 
        <Answer>
        (?:
            \Z
        |
            <warning: (?{ "Extra junk after expression at index $INDEX: '$CONTEXT'" })>
            <warning: Expected end of input>
            <error:>
        )

        <rule: Answer>  
            <[_Operand=Mult]> ** <[_Op=(\+|\-)]>
                (?{ $MATCH = shift @{$MATCH{_Operand}};
                    for my $term (@{$MATCH{_Operand}}) {
                        my $op = shift @{$MATCH{_Op}};
                        if ($op eq '+') { $MATCH += $term; }
                        else            { $MATCH -= $term; }
                    }
                })
          |
            <error: Expected valid arithmetic expression>

        <rule: Mult> 
        (?:
            <[_Operand=Pow]> ** <[_Op=(\*|/|%)]>
                (?{ $MATCH = reduce { eval($a . shift(@{$MATCH{_Op}}) . $b) }
                                    @{$MATCH{_Operand}};
                })
        )

        <rule: Pow> 
        (?:
            <[_Operand=Term]> ** <_Op=(\^)> 
                (?{ $MATCH = reduce { $b ** $a } reverse @{$MATCH{_Operand}}; })
        )

        <rule: Term>
        (?:
               <MATCH=Literal>
          | \( <MATCH=Answer> \)
        )

        <token: Literal>
            <error:>
        |
            <MATCH=( [+-]? \d++ (?: \. \d++ )?+ )>

    }xms
};

local $/ = "";
while (my $input = <DATA>) {
    chomp $input;
    my ($text, $expected) = split /\s+/, $input, 2;
    if ($text =~ $calculator) {
        is $/{Answer}, $expected => "Input $.: $text"; 
    }
    else {
        is_deeply \@!, eval($expected), => "Input $.: $text";
    }
}

__DATA__
2           2

2*3+4       10

2zoo        [
             "Extra junk after expression at index 1: 'zoo'",
             "Expected end of input, but found 'zoo' instead",
             "Expected valid input, but found 'zoo' instead",
             "Expected valid arithmetic expression, but found '2zoo' instead",
            ]

zoo         [
             "Expected literal, but found 'zoo' instead",
             "Expected valid arithmetic expression, but found 'zoo' instead",
            ]
