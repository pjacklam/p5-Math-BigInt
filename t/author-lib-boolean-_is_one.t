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

use Test::More tests => 81;

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

can_ok($LIB, '_is_one');

# Generate test data.

my @data;

for (my $x = 0 ; $x <= 10 ; ++ $x) {
    push @data, [ $x, $x == 1 ];
}

for (my $e = 2 ; $e <= 10 ; ++ $e) {
    my $x = "1" . ("0" x $e);
    push @data, [ $x, $x == 1 ];
}

# List context.

for (my $i = 0 ; $i <= $#data ; ++ $i) {
    my ($in0, $out0) = @{ $data[$i] };

    my ($x, @got);

    my $test = qq|\$x = $LIB->_new("$in0"); |
             . qq|\@got = $LIB->_is_one(\$x);|;

    eval $test;
    is($@, "", "'$test' gives emtpy \$\@");

    subtest "_is_one() in list context: $test", sub {
        plan tests => 3,

        cmp_ok(scalar @got, "==", 1,
               "'$test' gives one output arg");

        is(ref($got[0]), "",
           "'$test' output arg is a scalar");

        ok($got[0] && $out0 || !$got[0] && !$out0,
           "'$test' output arg has the right value");
    };
}

# Scalar context.

for (my $i = 0 ; $i <= $#data ; ++ $i) {
    my ($in0, $out0) = @{ $data[$i] };

    my ($x, $got);

    my $test = qq|\$x = $LIB->_new("$in0"); |
             . qq|\$got = $LIB->_is_one(\$x);|;

    eval $test;
    is($@, "", "'$test' gives emtpy \$\@");

    subtest "_is_one() in scalar context: $test", sub {
        plan tests => 2,

        is(ref($got), "",
           "'$test' output arg is a scalar");

        ok($got && $out0 || !$got && !$out0,
           "'$test' output arg has the right value");
    };
}
