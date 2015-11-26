#!/usr/bin/perl -w

use strict;             # restrict unsafe constructs

use Test::More tests => 2;

BEGIN {
    use_ok('Math::BigInt');
    use_ok('Math::BigFloat');
};

diag("Testing Math::BigInt $Math::BigInt::VERSION");
diag("==> Perl $], $^X");

# As long as Math::BigInt defaults to using Math::BigInt::FastCalc as its
# library (back-end), so see if it is installed, and if it is, display the
# version number.

eval { require Math::BigInt::FastCalc; };
if ($@) {
    diag("==> Math::BigInt::FastCalc (not installed)");
} else {
    diag("==> Math::BigInt::FastCalc $Math::BigInt::FastCalc::VERSION");
}
