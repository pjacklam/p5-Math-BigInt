#!perl

use strict;
use warnings;

use Test::More tests => 24;

use Math::BigInt;

my $x;
my $y;

# Finite numbers.

$x = Math::BigInt -> new("123");
isa_ok($x, 'Math::BigInt');
$y = $x -> bdigitsum();
isa_ok($y, 'Math::BigInt');
is($x, "6");
is($y, "6");

$x = Math::BigInt -> new("0");
isa_ok($x, 'Math::BigInt');
$y = $x -> bdigitsum();
isa_ok($y, 'Math::BigInt');
is($x, "0");
is($y, "0");

$x = Math::BigInt -> new("-123");
isa_ok($x, 'Math::BigInt');
$y = $x -> bdigitsum();
isa_ok($y, 'Math::BigInt');
is($x, "6");
is($y, "6");

# Infinity

$x = Math::BigInt -> binf("+");
isa_ok($x, 'Math::BigInt');
$y = $x -> bdigitsum();
isa_ok($y, 'Math::BigInt');
ok($x -> is_inf("+"));
ok($y -> is_inf("+"));

$x = Math::BigInt -> binf("-");
isa_ok($x, 'Math::BigInt');
$y = $x -> bdigitsum();
isa_ok($y, 'Math::BigInt');
ok($x -> is_inf("+"));
ok($y -> is_inf("+"));

# NaN

$x = Math::BigInt -> bnan();
isa_ok($x, 'Math::BigInt');
$y = $x -> bdigitsum();
isa_ok($y, 'Math::BigInt');
ok($x -> is_nan());
ok($y -> is_nan());
