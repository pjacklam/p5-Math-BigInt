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

use Test::More tests => 19;

use Math::BigFloat;

my $pi = {
          16 => '3.141592653589793',
          40 => '3.141592653589793238462643383279502884197',
         };

# Called as class method without argument.

{
    my $x = Math::BigFloat -> bpi();
    isa_ok($x, 'Math::BigFloat');
    is($x, $pi -> {40}, 'Math::BigFloat -> bpi()');
}

# Called as class method with scalar argument.

{
    my $x = Math::BigFloat -> bpi(16);
    isa_ok($x, 'Math::BigFloat');
    is($x, $pi -> {16}, '$x = Math::BigFloat->bpi(16)');
}

# Called as class method with class argument.

{
    my $n = Math::BigFloat -> new("16");
    my $x = Math::BigFloat -> bpi($n);
    isa_ok($x, 'Math::BigFloat');
    is($x, $pi -> {16},
       '$n = Math::BigFloat->new("16"); $x = Math::BigFloat->bpi($n)');
}

# Called as instance method without argument.

{
    my $x = Math::BigFloat -> bnan();
    $x -> bpi();
    isa_ok($x, 'Math::BigFloat');
    is($x, $pi -> {40}, '$x = Math::BigFloat -> bnan(); $x->bpi()');
}

# Called as instance method with scalar argument.

{
    my $x = Math::BigFloat -> bnan();
    $x -> bpi(16);
    isa_ok($x, 'Math::BigFloat');
    is($x, $pi -> {16}, '$x = Math::BigFloat -> bnan(); $x->bpi(16)');
}

# Called as instance method with instance argument.

{
    my $n = Math::BigFloat -> new("16");
    my $x = Math::BigFloat -> bnan();
    $x -> bpi($n);
    isa_ok($x, 'Math::BigFloat');
    is($x, $pi -> {16}, '$n = Math::BigFloat->new("16"); $x -> bpi($n)');
}

# Called as function without argument.

{
    my $x = Math::BigFloat::bpi();
    isa_ok($x, 'Math::BigFloat');
    is($x, $pi -> {40}, '$x = Math::BigFloat::bpi()');
}

# Called as function with scalar argument.

{
    my $x = Math::BigFloat::bpi(16);
    isa_ok($x, 'Math::BigFloat');
    is($x, $pi -> {16}, '$x = Math::BigFloat::bpi(16)');
}

# Called as function with instance argument.
#
# This is an ambiguous case. The argument list to bpi() is ($n), which is
# assumed to mean $n->bpi(), since we favour the OO-style. So in the test
# below, $n is assigned the value of pi with the default number of digits, and
# then $n is assigned to $x.

{
    my $n = Math::BigFloat -> new("16");
    my $x = Math::BigFloat::bpi($n);
    isa_ok($x, 'Math::BigFloat');
    is($x, $pi -> {40},
       '$n = Math::BigFloat->new("16"); $x = Math::BigFloat::bpi($n)');
    is($n, $pi -> {40},
       '$n = Math::BigFloat->new("16"); $x = Math::BigFloat::bpi($n)');
}
