#! /usr/bin/perl -w
use strict;
use 5.010;

use Data::Show;
use Test::More 'no_plan';

my $test_grammar = do {
    use Regexp::Grammars;
    qr{
        <keyword=(\w+)>
            <content=(.+?)>
        <dekeyword( delim => 'fo+/')>
      |
        <keyword=(\w+)>
            <content=(.+?)>
        <unkeyword(:keyword, prefix=>'end')>

      | <keyword=(\w+)>
            <content=(.+?)>
        <[revkeyword=unkeyword(?{ keyword => scalar reverse $MATCH{keyword} })]>

        <rule: unkeyword>
            (??{ quotemeta( ($ARG{prefix}//q{}) . $ARG{keyword} ) })

        <token: dekeyword>
            <terminator=:delim>
    }xms;
};


ok 'fooxdaa' !~ $test_grammar => 'Fail';

ok 'fooxoof' =~ $test_grammar     => 'Match reverse';
is $/{keyword}, 'foo'             => 'Keyword as expected';
is $/{content}, 'x'               => 'Content as expected';
is_deeply $/{revkeyword}, ['oof'] => 'Revkeyword as expected';

ok 'fooxendfoo' =~ $test_grammar => 'Match end';
is $/{keyword}, 'foo'            => 'Keyword as expected';
is $/{content}, 'x'              => 'Content as expected';
is $/{unkeyword}, 'endfoo'       => 'Unkeyword as expected';

ok 'fooxfoo/' =~ $test_grammar => 'Match /';
is $/{keyword}, 'foo'          => 'Keyword as expected';
is $/{content}, 'x'            => 'Content as expected';
is_deeply $/{dekeyword}, { "" =>'foo/', 'terminator'=>'foo/' }
                               => 'Dekeyword as expected';
