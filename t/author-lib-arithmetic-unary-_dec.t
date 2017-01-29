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

use Test::More tests => 2601;

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

can_ok($LIB, '_dec');

my @data;

# Small numbers (625 tests).

for (my $x = 1; $x <= 500 ; ++ $x) {
    push @data, [ $x, $x - 1 ];
}

# 11 - 1, 101 - 1, 1001 - 1, 10001 - 1, ...

for (my $p = 1; $p <= 50 ; ++ $p) {
    my $x = "1" . ("0" x ($p - 1) . "1");
    my $y = "1" . ("0" x $p);
    push @data, [ $x, $y ];
}

# 10 - 1, 100 - 1, 1000 - 1, 10000 - 1, ...

for (my $p = 1; $p <= 50 ; ++ $p) {
    my $x = "1" . ("0" x $p);
    my $y = ("9" x $p);
    push @data, [ $x, $y ];
}

# 9 - 1, 99 - 1, 999 - 1, 9999 - 1, ...

for (my $p = 1; $p <= 50 ; ++ $p) {
    my $x = "9" x $p;
    my $y = "9" x ($p - 1) . "8";
    push @data, [ $x, $y ];
}

# List context.

for (my $i = 0 ; $i <= $#data ; ++ $i) {
    my ($in0, $out0) = @{ $data[$i] };

    my ($x, @got);

    my $test = qq|\$x = $LIB->_new("$in0"); |
             . qq|\@got = $LIB->_dec(\$x);|;

    eval $test;
    is($@, "", "'$test' gives emtpy \$\@");

    subtest "_dec() in list context: $test", sub {
        plan tests => 6,

        cmp_ok(scalar @got, "==", 1,
               "'$test' gives one output arg");

        is(ref($got[0]), $REF,
           "'$test' output arg is a $REF");

        is($LIB->_check($got[0]), 0,
           "'$test' output is valid");

        is($LIB->_str($got[0]), $out0,
           "'$test' output arg has the right value");

        is(ref($x), $REF,
           "'$test' first input arg is still a $REF");

        ok($LIB->_str($x) eq $out0 || $LIB->_str($x) eq $in0,
           "'$test' input arg has the correct value");
    };
}

# Scalar context.

for (my $i = 0 ; $i <= $#data ; ++ $i) {
    my ($in0, $out0) = @{ $data[$i] };

    my ($x, $got);

    my $test = qq|\$x = $LIB->_new("$in0"); |
             . qq|\$got = $LIB->_dec(\$x);|;

    eval $test;
    is($@, "", "'$test' gives emtpy \$\@");

    subtest "_dec() in scalar context: $test", sub {
        plan tests => 5,

        is(ref($got), $REF,
           "'$test' output arg is a $REF");

        is($LIB->_check($got), 0,
           "'$test' output is valid");

        is($LIB->_str($got), $out0,
           "'$test' output arg has the right value");

        is(ref($x), $REF,
           "'$test' first input arg is still a $REF");

        ok($LIB->_str($x) eq $out0 || $LIB->_str($x) eq $in0,
           "'$test' input arg has the correct value");
    };
}
