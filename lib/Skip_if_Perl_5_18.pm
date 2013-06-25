package # Hide this from CPAN
    Skip_if_Perl_5_18;

use Test::More;

if ($] >= 5.018) {
    plan skip_all => 'This feature of Regexp::Grammars known to be incompatible with 5.18';
}


1; # Magic true value required at end of module
