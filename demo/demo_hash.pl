#! /opt/local/bin/perl5.10.0
use v5.10;
use warnings;

my %hash = (
    do => 'a deer',
    re => 'a drop of golden sun',
    me => 'a name I call myself',
    fa => 'a long long way to run',
);


my $other = 1;

my $grammar = do {
    use Regexp::Grammars;
    qr{ <[WORD=%hash]>+ }xms;
};

while (my $line = <>) {
    if ($line =~ $grammar) {
        use Data::Dumper 'Dumper';
        say Dumper \%/;
    }
}

say {*STDERR} 'done!';
