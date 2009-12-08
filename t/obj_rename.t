use 5.010;
use warnings;

use Test::More tests=>4;

use Regexp::Grammars;

my $list_nonempty = qr{
    <List>

    <objrule: SaveAs=List>
        \(  <[Value]> ** (,)  \)

    <objtoken: Some::Other=Value>
        \d+
}xms;

no Regexp::Grammars;

ok +('(1,2)' =~ $list_nonempty)           => 'Matched non-empty list';

is ref($/{List}), 'SaveAs'                => 'Class naming worked at top level';

is ref($/{List}{Value}[0]), 'Some::Other' => 'Class naming worked at 2nd level';
is ref($/{List}{Value}[1]), 'Some::Other' => 'Class naming again at 2nd level';
