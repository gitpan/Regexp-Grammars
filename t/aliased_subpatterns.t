use 5.010;
use warnings;
use Test::More 'no_plan';

my $parser = do{
    use Regexp::Grammars;
    qr{
        <num=(\d++)>
      | <_pat='".*"'> <str=(??{ $MATCH{_pat} })>
      | <bool=(?{'true or false'})>
    }xms
};

#use Data::Show;
#show  $parser; exit;

ok +('"abc"' =~ $parser) => 'Matched <str>';
is $/{str}, '"abc"'      => 'Captured correctly';

ok +(42 =~ $parser) => 'Matched <num>';
is $/{num}, 42      => 'Captured correctly';

ok +('true' =~ $parser)      => 'Matched <bool>';
is $/{bool}, 'true or false' => 'Pseudo-captured correctly';
