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

use Test::More tests => 4485;

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

use lib 't';
use Math::BigInt::Lib::TestUtil qw< randstr >;

can_ok($LIB, '_acmp');

# Generate test data.

my @data;

# Small integers.

for my $a (0 .. 10) {
    for my $b (0 .. 10) {
        push @data, [ $a, $b, $a <=> $b ];
    }
}

# Random large integers.

for (1 .. 1000) {
    my $na  = 2 + int rand 35;      # number of digits in $a
    my $nb  = 2 + int rand 35;      # number of digits in $a
    my $a   = randstr($na, 10);     # generate $a
    my $b   = randstr($na, 10);     # generate $b
    my $cmp = length($a) <=> length($b) || $a cmp $b;
    push @data, [ $a, $b, $cmp ];
}

# List context.

for (my $i = 0 ; $i <= $#data ; ++ $i) {
    my ($in0, $in1, $out0) = @{ $data[$i] };

    my ($x, $y, @got);

    my $test = qq|\$x = $LIB->_new("$in0"); |
             . qq|\$y = $LIB->_new("$in1"); |
             . qq|\@got = $LIB->_acmp(\$x, \$y);|;

    eval $test;
    is($@, "", "'$test' gives emtpy \$\@");

    subtest "_new() in list context: $test", sub {
        plan tests => 3,

        cmp_ok(scalar @got, "==", 1,
               "'$test' one output arg");

        is(ref($got[0]), "",
           "'$test' output arg is a Perl scalar");

        is($got[0], $out0,
           "'$test' output arg has the right value");
    };
}

# Scalar context.

for (my $i = 0 ; $i <= $#data ; ++ $i) {
    my ($in0, $in1, $out0) = @{ $data[$i] };

    my ($x, $y, $got);

    my $test = qq|\$x = $LIB->_new("$in0"); |
             . qq|\$y = $LIB->_new("$in1"); |
             . qq|\$got = $LIB->_acmp(\$x, \$y);|;

    eval $test;
    is($@, "", "'$test' gives emtpy \$\@");

    subtest "_new() in scalar context: $test", sub {
        plan tests => 2,

        is(ref($got), "",
           "'$test' output arg is a Perl scalar");

        is($got, $out0,
           "'$test' output arg has the right value");
    };
}
