#!/usr/bin/perl

BEGIN {
    unless ($ENV{AUTHOR_TESTING}) {
        print "1..0 # SKIP these tests are for testing by the author";
        exit;
    }
}

# Test how accuracy and precision are set in the constructors. Currently, only
# new() is tested, but all of the constructors need to be tested:
#
#     new(), bzero(), bone(), binf(), bnan(), and the from_*() methods.

use strict;
use warnings;

use Test::More tests => 20;

use Math::BigInt;
use Math::BigFloat;

my $x;

###############################################################################
# new(value, a, p)
###############################################################################

for my $class ('Math::BigInt', 'Math::BigFloat') {

    # set class accuracy

    $class -> accuracy(4);
    $class -> precision(undef);

    # instance precision overrides class accuracy

    $x = $class->new(5, undef, -3);

    is($x -> bdstr(), 5,
       'nan("x", a, p) instance "x"');

    is($x -> accuracy(), undef,
       'nan("x", a, p) instance accuracy (via method)');
    is($x -> {_a}, undef,
       'nan("x", a, p) instance accuracy (hash value)');

    is($x -> precision(), -3,
       'nan("x", a, p) instance precision (via method)');
    is($x -> {_p}, -3,
       'nan("x", a, p) instance precision (hash value)');

    # set class precision

    $class -> accuracy(undef);
    $class -> precision(-4);

    # instance accuracy overrides precision

    $x = $class->new(5, 3, undef);

    is($x -> bdstr(), 5,
       'nan("x", a, p) instance value');

    is($x -> accuracy(), 3,
       'nan("x", a, p) instance accuracy (via method)');
    is($x -> {_a}, 3,
       'nan("x", a, p) instance accuracy (hash value)');

    is($x -> precision(), undef,
       'nan("x", a, p) instance precision (via method)');
    is($x -> {_p}, undef,
       'nan("x", a, p) instance precision (hash value)');
}
