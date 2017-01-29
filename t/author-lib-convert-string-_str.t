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

use Test::More tests => 11877;

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

can_ok($LIB, '_str');

use lib "t";
use Math::BigInt::Lib::TestUtil qw< randstr >;

# Generate test data.

my @data;

push @data, 0 .. 250;                   # small integers

for (my $n = 3 ; $n <= 250 ; ++ $n) {
    push @data, "1" . ("0" x $n);       # powers of 10
}

for (my $n = 4 ; $n <= 250 ; ++ $n) {
    for (1 .. 10) {
        push @data, randstr($n, 10);    # random $n-digit integer
    }
}

# List context.

for (my $i = 0 ; $i <= $#data ; ++ $i) {
    my ($in0) = $data[$i];
    my $out0 = $in0;

    my ($x, @got);

    my $test = qq|\$x = $LIB->_new("$in0"); |
             . qq|\@got = $LIB->_str(\$x);|;

    eval $test;
    is($@, "", "'$test' gives emtpy \$\@");

    subtest "_str() in list context: $test", sub {
        plan tests => 3;

        cmp_ok(scalar @got, '==', 1,
               "'$test' gives one output arg");

        is(ref($got[0]), "",
           "'$test' output arg is a scalar");

        is($got[0], $out0,
           "'$test' output arg has the right value");
    };
}

# Scalar context.

for (my $i = 0 ; $i <= $#data ; ++ $i) {
    my ($in0) = $data[$i];
    my $out0 = $in0;

    my ($x, $got);

    my $test = qq|\$x = $LIB->_new("$in0"); |
             . qq|\$got = $LIB->_str(\$x);|;

    eval $test;
    is($@, "", "'$test' gives emtpy \$\@");

    subtest "_str() in scalar context: $test", sub {
        plan tests => 2;

        is(ref($got), "",
           "'$test' output arg is a scalar");

        is($got, $out0,
           "'$test' output arg has the right value");
    };
}
