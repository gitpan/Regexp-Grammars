use v5.10;
use warnings;

my $calculator = do{
    use Regexp::Grammars;
    qr{
        <Answer>

        <rule: Answer>
            <X=Mult> \+ <Y=Answer>
                (?{ $MATCH = $MATCH{X} + $MATCH{Y}; })
          | <X=Mult> - <Y=Answer>
                (?{ $MATCH = $MATCH{X} - $MATCH{Y}; })
          | <MATCH=Mult>

        <rule: Mult>
            <X=Pow> \* <Y=Mult>
                (?{ $MATCH = $MATCH{X} * $MATCH{Y}; })
          | <X=Pow>  / <Y=Mult>
                (?{ $MATCH = $MATCH{X} / $MATCH{Y}; })
          | <X=Pow>  % <Y=Mult>
                (?{ $MATCH = $MATCH{X} % $MATCH{Y}; })
          | <MATCH=Pow>

        <rule: Pow>
            <X=Term> \^ <Y=Pow>
                (?{ $MATCH = $MATCH{X} ** $MATCH{Y}; })
          | <MATCH=Term>

        <rule: Term>
               <MATCH=Literal>
          | \( <MATCH=Answer> \)

        <token: Literal>
            <MATCH=( [+-]? \d++ (?: \. \d++ )?+ )>
    }xms
};

while (my $input = <>) {
    if ($input =~ $calculator) {
        say '--> ', $/{Answer};
    }
}
