#!/usr/bin/perl

# Test use Math::BigFloat with => 'Math::BigInt::SomeSubclass';

use strict;
use warnings;

use Test::More tests => 2367 + 1;

use Math::BigFloat with => 'Math::BigInt::Subclass',
                   lib  => 'Calc';

our ($CLASS, $CALC);
$CLASS = "Math::BigFloat";
$CALC  = "Math::BigInt::Calc";          # backend

# the with argument is ignored
is(Math::BigFloat->config()->{with}, 'Math::BigInt::Calc',
   'Math::BigFloat->config()->{with}');

require 't/bigfltpm.inc';	# all tests here for sharing
