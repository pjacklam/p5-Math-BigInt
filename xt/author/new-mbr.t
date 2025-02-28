# -*- mode: perl; -*-

use strict;
use warnings;

use Test::More tests => 11;
use Scalar::Util qw< refaddr >;

my $class;

BEGIN { $class = 'Math::BigRat'; }
BEGIN { use_ok($class); }

# CPAN RT #132712.

my $q1 = $class -> new("-1/2");
my ($n, $d) = $q1 -> parts();

my $n_orig = $n -> copy();
my $d_orig = $d -> copy();
my $q2 = $class -> new($n, $d);

cmp_ok($n, "==", $n_orig,
       "The value of the numerator hasn't changed");
cmp_ok($d, "==", $d_orig,
       "The value of the denominator hasn't changed");

isnt(refaddr($n), refaddr($n_orig),
     "The addresses of the numerators have changed");
isnt(refaddr($d), refaddr($d_orig),
     "The addresses of the denominators have changed");

###############################################################################

# new() as a class method:
#
# $y = $class -> new()

{
    my $y = $class -> new();
    subtest qq|\$y = $class -> new();|, => sub {
        plan tests => 2;

        is(ref($y), $class, "output arg is a $class");
        is($y, "0", 'output arg has the right value');
    };
}

# new() as an instance method:
#
# $y = $x -> new()

{
    my $x = $class -> new("999");
    my $y = $x -> new();
    subtest qq|\$x = $class -> new("999"); \$y = \$x -> new();|, => sub {
        plan tests => 3;

        is(ref($y), $class, "output arg is a $class");
        is($y, "0", 'output arg has the right value');
        isnt(refaddr($x), refaddr($y), "output is not the invocand");
    };
}

###############################################################################

# new() as a class method:
#
# $class -> new("")

{
    my $y = $class -> new("");
    subtest qq|\$y = $class -> new("");|, => sub {
        plan tests => 2;

        is(ref($y), $class, "output arg is a $class");
        is($y, "NaN", 'output arg has the right value');
   };
}

# new() as an instance method:
#
# $x -> new("")

{
    my $x = $class -> new("999");
    my $y = $x -> new("");
    subtest qq|\$x = $class -> new("999"); \$y = \$x -> new("");|, => sub {
        plan tests => 3;

        is(ref($y), $class, "output arg is a $class");
        is($y, "NaN", 'output arg has the right value');
        isnt(refaddr($x), refaddr($y), "output is not the invocand");
    };
}

###############################################################################

# new() as a class method:
#
# $class -> new(undef)

{
    my $y = $class -> new(undef);
    subtest qq|\$y = $class -> new(undef);|, => sub {
        plan tests => 2;

        is(ref($y), $class, "output arg is a $class");
        is($y, "0", 'output arg has the right value');
    };
}

# new() as an instance method
#
# $x -> new(undef)

{
    my $x = $class -> new("999");
    my $y = $x -> new(undef);
    subtest qq|\$x = $class -> new("999"); \$y = \$x -> new(undef);|, => sub {
        plan tests => 3;

        is(ref($y), $class, "output arg is a $class");
        is($y, "0", 'output arg has the right value');
        isnt(refaddr($x), refaddr($y), "output is not the invocand");
    };
}
