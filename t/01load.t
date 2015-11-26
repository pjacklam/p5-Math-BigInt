#!/usr/bin/perl -w

use strict;             # restrict unsafe constructs

use Test::More tests => 2;

BEGIN {
    use_ok('Math::BigInt');
    use_ok('Math::BigFloat');
};

diag("Testing Math::BigInt $Math::BigInt::VERSION");
diag("==> Perl $], $^X");
