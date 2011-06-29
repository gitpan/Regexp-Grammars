use 5.010;
use warnings;
use Test::More 'no_plan';

#=====[ Test zero timeout ]=============================================

my %AcceptableVersions = (
    '0.95' => 1,
    '0.98' => 1,
    '1.01' => 1,
);

my $version_checker = do{
    use Regexp::Grammars;
    qr{
        <LangVersion>

        <rule: LangVersion>
              vers = <%AcceptableVersions>
            |
              vers =
              <warning: (?{ "Cannot parse language version $CONTEXT" })>
              <timeout: 0>
    }xms;
};

ok 'vers = 0.95' =~ $version_checker => 'Matched version 0.95';
ok @! == 0                           => 'with no error messages';

ok 'vers = 0.96' !~ $version_checker           => 'Correctly failed to match version 0.96';
ok @! == 1                                     => 'with correct number of error messages';
is $![0], 'Cannot parse language version 0.96' => 'with correct error message';


#=====[ Test regular timeouts ]=============================================

my $calculator = do{
    use Regexp::Grammars;
    qr{
        <timeout: 5>
        <Answer>

        <rule: Answer>
            ( <.Mult> ** <.Op=([+-])> )
                <MATCH= (?{ eval $CAPTURE })>

        <rule: Mult>
            ( <.Pow> ** <.Op=([*/%])> )
                <MATCH= (?{ eval $CAPTURE })>

        <rule: Pow>
            <X=Term> \^ <Y=Pow>
                <MATCH= (?{ $MATCH{X} ** $MATCH{Y}; })>
            |
                <MATCH=Term>

        <rule: Term>
                <MATCH=Literal> (?{ sleep 1 })
            | \(  <MATCH=Answer>  \)

        <token: Literal>
                <MATCH= ( [+-]? \d++ (?: \. \d++ )?+ )>
    }xms
};

ok '2*2*2' !~ $calculator => 'Correctly failed to match 2*2*2';
ok @! == 1                => 'with single error message';
is $![0], 'Internal error: Timed out after 5 seconds (as requested)'
                          => 'and the correct error message';

ok '2*2' =~ $calculator   => 'Matched 2*2';
is $/{Answer}, '4'        => 'with correct result';
ok @! == 0                => 'and without error message';

ok '2' =~ $calculator     => 'Matched 2';
is $/{Answer}, '2'        => 'with correct result';
ok @! == 0                => 'and without error message';
