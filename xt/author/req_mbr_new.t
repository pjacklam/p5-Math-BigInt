# -*- mode: perl; -*-

# check that requiring Math::BigRat and then calling new() works

use strict;
use warnings;

use Test::More tests => 1;

require Math::BigRat;

my $x = Math::BigRat -> new(2);
is($x, '2', '$x is 2');
