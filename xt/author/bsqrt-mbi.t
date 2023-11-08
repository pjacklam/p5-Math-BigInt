# -*- mode: perl; -*-

use strict;
use warnings;

use Test::More tests => 2;

use Math::BigInt;

# The default is to truncate to integer, and since sqrt(3) = 1.732..., the
# output should be 1.

is(Math::BigInt -> new(3) -> bsqrt(), "1",
   "Math::BigInt -> new(3) -> bsqrt() = 1");

# When the user has specified an accuracy of 1, the output should be rounded to
# the nearest integer, and since sqrt(3) = 1.732..., the output should be 2.

is(Math::BigInt -> new(3) -> bsqrt(1), "2",
   "Math::BigInt -> new(3) -> bsqrt(1) = 2");
