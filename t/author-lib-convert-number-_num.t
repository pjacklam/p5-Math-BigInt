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

use Test::More tests => 3397;

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

can_ok($LIB, "_num");

use lib "t";
use Math::BigInt::Lib::TestUtil qw< randstr >;

# Generate test data.

my @data;

push @data, 0 .. 250;                   # small integers

for (my $n = 3 ; $n <= 300 ; ++ $n) {
    push @data, "1" . ("0" x $n);       # powers of 10
}

for (my $n = 1 ; $n <= 300 ; ++ $n) {
    push @data, randstr($n, 10);        # random big integers
}

# Tolerance for floating point number comparisons.

my $tol = 2e-15;

# List context.

for (my $i = 0 ; $i <= $#data ; ++ $i) {
    my $str = $data[$i];

    my ($x, @got);

    my $test = qq|\$x = $LIB->_new("$str"); |
             . qq|\@got = $LIB->_num(\$x);|;

    eval $test;
    is($@, "", "'$test' gives emtpy \$\@");

    subtest "_num() in list context: $test", sub {
        plan tests => 3,

        cmp_ok(scalar @got, "==", 1,
               "'$test' gives one output arg");

        is(ref($got[0]), "",
           "'$test' output arg is a Perl scalar");

        # If output does not use floating point notation, compare the
        # values exactly ...

        if ($got[0] =~ /^\d+\z/) {
            cmp_ok($got[0], "==", $str,
                   "'$test' output value is exactly right");
        }

        # ... otherwise compare them approximatly.

        else {
            my $rel_err = abs($got[0] - $str) / $str;
            cmp_ok($rel_err, "<", $tol,
                   "'$test' output value is correct within" .
                   " a relative error of $tol");
        }
    };
}

# Scalar context.

for (my $i = 0 ; $i <= $#data ; ++ $i) {
    my $str = $data[$i];

    my ($x, $got);

    my $test = qq|\$x = $LIB->_new("$str"); |
             . qq|\$got = $LIB->_num(\$x);|;

    eval $test;
    is($@, "", "'$test' gives emtpy \$\@");

    subtest "_num() in scalar context: $test", sub {
        plan tests => 2,

        is(ref($got), "",
           "'$test' output arg is a Perl scalar");

        # If output does not use floating point notation, compare the
        # values exactly ...

        if ($got =~ /^\d+\z/) {
            cmp_ok($got, "==", $str,
                   "'$test' output value is exactly right");
        }

        # ... otherwise compare them approximatly.

        else {
            my $rel_err = abs($got - $str) / $str;
            cmp_ok($rel_err, "<", $tol,
                   "'$test' output value is correct within" .
                   " a relative error of $tol");
        }
    };
}
