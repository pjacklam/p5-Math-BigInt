#!perl

BEGIN {
    unless ($ENV{AUTHOR_TESTING}) {
        require Test::More;
        Test::More::plan(skip_all =>
                         'these tests are for testing by the author');
    }
}

use strict;
use warnings;

use Test::More tests => 4393;

###############################################################################
# Read and load configuration file and backend library.

my $conffile = 't/author-lib-meta-config.conf';
open CONFFILE, $conffile or die "$conffile: can't open file for reading: $!";
my $confdata = do { local $/ = undef; <CONFFILE>; };
close CONFFILE or die "$conffile: can't close file after reading: $!";

our ($LIB, $REF);
eval $confdata;
die $@ if $@;

eval "require $LIB";
die $@ if $@;

###############################################################################

can_ok($LIB, '_alen');

use lib 't';
use Math::BigInt::Lib::TestUtil qw< randstr >;

# Generate test data.

my @data;

# Small integers.

for my $x (0 .. 99) {
    push @data, [ $x ];
}

# Random large integers.

for (3 .. 1000) {
    my $nx = 2 + int rand 35;           # number of digits in $x
    my $x  = randstr($nx, 10);          # generate $a
    push @data, [ $x ];
}

# List context.

for (my $i = 0 ; $i <= $#data ; ++ $i) {
    my ($in0) = @{ $data[$i] };

    my ($x, @got);

    my $test = qq|\$x = $LIB->_new("$in0"); |
             . qq|\@got = $LIB->_alen(\$x);|;

    eval $test;
    is($@, "", "'$test' gives emtpy \$\@");

    subtest "_alen() in list context: $test", sub {
        plan tests => 5,

        cmp_ok(scalar @got, '==', 1,
               "'$test' gives one output arg");

        is(ref($got[0]), "",
           "'$test' output arg is a Perl scalar");

        isnt($got[0], undef,
             "'$test' output arg is defined");

        like($got[0], qr/^[+-]?(\d+(\.\d*)?|\.\d+)([Ee][+-]?\d+)?\z/,
             "'$test' output arg looks like a number");

        is($got[0], int($got[0]),
           "'$test' output arg is an integer");
    };
}

# Scalar context.

for (my $i = 0 ; $i <= $#data ; ++ $i) {
    my ($in0) = @{ $data[$i] };

    my ($x, $got);

    my $test = qq|\$x = $LIB->_new("$in0"); |
             . qq|\$got = $LIB->_alen(\$x);|;

    eval $test;
    is($@, "", "'$test' gives emtpy \$\@");

    subtest "_alen() in scalar context: $test", sub {
        plan tests => 4,

        is(ref($got), "",
           "'$test' output arg is a Perl scalar");

        isnt($got, undef,
             "'$test' output arg is defined");

        like($got, qr/^[+-]?(\d+(\.\d*)?|\.\d+)([Ee][+-]?\d+)?\z/,
             "'$test' output arg looks like a number");

        is($got, int($got),
           "'$test' output arg is an integer");
    };
}
