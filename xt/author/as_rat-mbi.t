# -*- mode: perl; -*-

use strict;
use warnings;

use Test::More tests => 2;
use Scalar::Util qw< refaddr >;

use Math::BigInt;

my ($x, $y);

note("as_rat() as a class method");

$x = Math::BigInt -> as_rat("2");
subtest '$x = Math::BigInt -> as_rat("2");' => sub {
    plan tests => 2;
    is(ref($x), 'Math::BigRat', '$x is a Math::BigRat');
    cmp_ok($x, "==", 2, '$x == 2');
};

note("as_rat() as an instance method");

$x = Math::BigInt -> new("2"); $y = $x -> as_rat();
subtest '$x = Math::BigInt -> new("2"); $y = $x -> as_rat();' => sub {
    plan tests => 4;
    is(ref($x), 'Math::BigInt', '$x is a Math::BigInt');
    is(ref($y), 'Math::BigRat', '$y is a Math::BigRat');
    cmp_ok($y, "==", 2, '$y == 2');
    isnt(refaddr($x), refaddr($y), '$x and $y are different objects');
};
