#! /opt/local/bin/perl5.10.0
use v5.10;
use warnings;

use List::Util   qw< reduce >;
use Data::Dumper qw< Dumper >;
use Regexp::Grammars;

my $calculator = qr{
    \A
    <Answer>

    <objrule: Answer>
        <[Operand=Mult]> ** <[Operator=(\+|\-)]>

    <objrule: Mult>
        <[Operand=Pow]> ** <[Operator=( [*/%] )]>

    <objrule: Pow>
        <[Operand=Term]> ** <Operator=(\^)> 

    <objrule: Term>
                              <MATCH=Literal>
      | <.OpenParen=(  \( )>  <MATCH=Answer>  <.CloseParen=( \) )>

    <token: Literal>
        <MATCH=( [+-]? \d++ (?: \. \d++ )?+ )>
}xms;

while (my $input = <>) {
    if ($input =~ $calculator) {
        warn Dumper $/{Answer};
    }
}
