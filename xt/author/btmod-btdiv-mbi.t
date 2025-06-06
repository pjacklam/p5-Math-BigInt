# -*- mode: perl; -*-

use strict;
use warnings;

use Test::More tests => 41301;

use Math::BigInt;

use Math::Complex ();

my $inf = $Math::Complex::Inf;
my $nan = $inf - $inf;

my $scalar_util_ok = eval { require Scalar::Util; };
Scalar::Util -> import('refaddr') if $scalar_util_ok;

diag "Skipping some tests since Scalar::Util is not installed."
  unless $scalar_util_ok;

# Return 1 if the input argument is +inf or -inf, and "" otherwise.

sub isinf {
    my $x = shift;
    return $x == $inf || $x == -$inf;
}

# Return 1 if the input argument is a nan (Not-a-Number), and "" otherwise.

sub isnan {
    my $x = shift;
    return $x != $x;
}

# Convert a Perl scalar to a Math::BigInt object. This function is used for
# consistent comparisons. For instance, a Not-a-Number might be stringified to
# 'nan', but Math::BigInt uses 'NaN'.

sub pl2mbi {
    my $x = shift;
    return Math::BigInt -> binf('+') if $x == $inf;
    return Math::BigInt -> binf('-') if $x == -$inf;
    return Math::BigInt -> bnan()    if isnan($x);
    return Math::BigInt -> new($x);
}

# Does a truncated division (T-division).

sub tdiv {
    die "Usage: fdiv X Y\n" if @_ != 2;

    #no integer;

    my $x = shift;              # numerator
    my $y = shift;              # denominator

    # Convert Perl strings representing nan, +inf, and -inf into Perl numbers.

    if ($x =~ /^\s*nan\s*$/i) {
        $x = $nan;
    } elsif ($x =~ /^\s*([+-]?)inf(inity)?\s*$/i) {
        $x = $1 eq '-' ? -$inf : $inf;
    }

    if ($y =~ /^\s*nan\s*$/i) {
        $y = $nan;
    } elsif ($y =~ /^\s*([+-]?)inf(inity)?\s*$/i) {
        $y = $1 eq '-' ? -$inf : $inf;
    }

    # If any input is nan, the output is nan.

    if (isnan($x) || isnan($y)) {
        return wantarray ? ($nan, $nan) : $nan;
    }

    # Divide by zero and modulo zero.

    if ($y == 0) {

        # Core Perl gives an "Illegal division by zero" error whenever the
        # denominator is zero. Math::BigInt, however, has a different
        # convention.

        my $q = $x < 0 ? -$inf
              : $x > 0 ?  $inf
              :           $nan;
        my $r = $x;
        return wantarray ? ($q, $r) : $q;
    }

    # Numerator is +/-infinity, and denominator is finite and non-zero.

    if (isinf($x)) {
        my $q = int($x / $y);
        my $r = $x - $y * $q;
        return wantarray ? ($q, $r) : $q;

        if (isinf($y)) {
            return wantarray ? ($nan, $nan) : $nan;
        } else {
            if (($x <=> 0) == ($y <=> 0)) {
                return wantarray ? ($inf, $nan) : $inf;
            } else {
                return wantarray ? (-$inf, $nan) : -$inf;
            }
        }
    }

    # Denominator is +/- infinity, and the numerator is finite.
    #
    # Core Perl:    5 %  Inf =    5
    #              -5 % -Inf =   -5
    #              -5 %  Inf =   -5
    #               5 % -Inf =    5

    if (isinf($y)) {
        return wantarray ? (0, $x) : 0;
    }

    # Do a truncated division.

    my $q = int($x / $y);
    my $r = $x - $y * $q;

    return wantarray ? ($q, $r) : $q;
}

# Tests where the invocand and the argument are two different objects.

#for my $num (-20 .. 20) {
#    for my $den (-20 .. -1, 1 .. 20) {
for my $num (-$inf, -20 .. 20, $inf, $nan) {
    for my $den (-$inf, -20 .. 20, $inf, $nan) {

        #######################################################################
        # btdiv() in list context.
        #######################################################################

        {
            # Compute expected output values.

            my ($quo, $rem) = tdiv($num, $den);

            note(qq|\n(\$quo, \$rem) = | .
                 qq|Math::BigInt -> new("$num") -> btdiv("$den")\n\n|);

            # Input values as objects.

            my $mbi_num = Math::BigInt -> new("$num");
            my $mbi_den = Math::BigInt -> new("$den");

            # Get addresses for later tests.

            my ($mbi_num_addr, $mbi_den_addr);
            $mbi_num_addr = refaddr($mbi_num) if $scalar_util_ok;
            $mbi_den_addr = refaddr($mbi_den) if $scalar_util_ok;

            # Compute actual output values.

            my ($mbi_quo, $mbi_rem) = $mbi_num -> btdiv($mbi_den);

            # Check classes.

            is(ref($mbi_num), 'Math::BigInt',
               "class of numerator is still Math::BigInt");
            is(ref($mbi_den), 'Math::BigInt',
               "class of denominator is still Math::BigInt");

            is(ref($mbi_quo), 'Math::BigInt',
               "class of quotient is Math::BigInt");
            is(ref($mbi_rem), 'Math::BigInt',
               "class of remainder is Math::BigInt");

            # Check values.

            is($mbi_quo, pl2mbi($quo), "$num / $den = $quo");
            is($mbi_rem, pl2mbi($rem), "$num % $den = $rem");

            is($mbi_den, pl2mbi($den), "value of denominator has not change");

            # Check addresses.

            my ($mbi_quo_addr, $mbi_rem_addr);
            $mbi_quo_addr = refaddr($mbi_quo) if $scalar_util_ok;
            $mbi_rem_addr = refaddr($mbi_rem) if $scalar_util_ok;

          SKIP: {
                skip "Scalar::Util not available", 2 unless $scalar_util_ok;

                is($mbi_quo_addr, $mbi_num_addr,
                   "the quotient object is the numerator object");

                ok($mbi_rem_addr != $mbi_num_addr &&
                   $mbi_rem_addr != $mbi_den_addr &&
                   $mbi_rem_addr != $mbi_quo_addr,
                   "the remainder object is neither the numerator," .
                   " denominator, nor quotient object");
            }
        }

        #######################################################################
        # btdiv() in scalar context.
        #######################################################################

        {
            # Compute expected output values.

            my $quo = tdiv($num, $den);

            note(qq|\n\$quo = | .
                 qq|Math::BigInt -> new("$num") -> btdiv("$den")\n\n|);

            # Input values as objects.

            my $mbi_num = Math::BigInt -> new("$num");
            my $mbi_den = Math::BigInt -> new("$den");

            # Get addresses for later tests.

            my ($mbi_num_addr, $mbi_den_addr);
            $mbi_num_addr = refaddr($mbi_num) if $scalar_util_ok;
            $mbi_den_addr = refaddr($mbi_den) if $scalar_util_ok;

            # Compute actual output values.

            my $mbi_quo = $mbi_num -> btdiv($mbi_den);

            # Check classes.

            is(ref($mbi_num), 'Math::BigInt',
               "class of numerator is still Math::BigInt");
            is(ref($mbi_den), 'Math::BigInt',
               "class of denominator is still Math::BigInt");

            is(ref($mbi_quo), 'Math::BigInt',
               "class of quotient is Math::BigInt");

            # Check values.

            is($mbi_quo, pl2mbi($quo), "$num / $den = $quo");

            is($mbi_den, pl2mbi($den), "value of numerator has not change");

            # Check addresses.

            my $mbi_quo_addr;
            $mbi_quo_addr = refaddr($mbi_quo) if $scalar_util_ok;;

          SKIP: {
                skip "Scalar::Util not available", 1 unless $scalar_util_ok;

                is($mbi_quo_addr, $mbi_num_addr,
                   "the quotient object is the numerator object");
            }
        }

        #######################################################################
        # btmod() (scalar context only).
        #######################################################################

        {
            # Compute expected output values.

            my (undef, $rem) = tdiv($num, $den);

            note(qq|\n\$quo = | .
                 qq|Math::BigInt -> new("$num") -> btmod("$den")\n\n|);

            # Input values as objects.

            my $mbi_num = Math::BigInt -> new("$num");
            my $mbi_den = Math::BigInt -> new("$den");

            # Get addresses for later tests.

            my ($mbi_num_addr, $mbi_den_addr);
            $mbi_num_addr = refaddr($mbi_num) if $scalar_util_ok;
            $mbi_den_addr = refaddr($mbi_den) if $scalar_util_ok;

            # Compute actual output values.

            my $mbi_rem = $mbi_num -> btmod($mbi_den);

            # Check classes.

            is(ref($mbi_num), 'Math::BigInt',
               "class of numerator is still Math::BigInt");
            is(ref($mbi_den), 'Math::BigInt',
               "class of denominator is still Math::BigInt");

            is(ref($mbi_rem), 'Math::BigInt',
               "class of remainder is Math::BigInt");

            # Check values.

            is($mbi_rem, pl2mbi($rem), "$num % $den = $rem");

            is($mbi_den, pl2mbi($den), "value of denominator has not change");

            # Check addresses.

            my $mbi_rem_addr;
            $mbi_rem_addr = refaddr($mbi_rem) if $scalar_util_ok;

          SKIP: {
                skip "Scalar::Util not available", 1 unless $scalar_util_ok;

                is($mbi_rem_addr, $mbi_num_addr,
                   "the remainder object is the numerator object");
            }
        }
    }
}

# Tests where the invocand and the argument is the same object.

for my $num (-$inf, -20 .. -1, 1 .. 20, $inf, $nan) {

    #######################################################################
    # btdiv() in list context.
    #######################################################################

    {
        # Compute expected output values.

        my ($quo, $rem) = tdiv($num, $num);

        note(qq|\n\$x = Math::BigInt -> new("$num"); | .
             qq|(\$quo, \$rem) = \$x -> btdiv("\$x")\n\n|);

        # Input values as objects.

        my $mbi_num = Math::BigInt -> new("$num");

        # Get addresses for later tests.

        my $mbi_num_addr;
        $mbi_num_addr = refaddr($mbi_num) if $scalar_util_ok;

        # Compute actual output values.

        my ($mbi_quo, $mbi_rem) = $mbi_num -> btdiv($mbi_num);

        # Check classes.

        is(ref($mbi_num), 'Math::BigInt',
           "class of numerator is still Math::BigInt");

        is(ref($mbi_quo), 'Math::BigInt',
           "class of quotient is Math::BigInt");
        is(ref($mbi_rem), 'Math::BigInt',
           "class of remainder is Math::BigInt");

        # Check values.

        is($mbi_quo, pl2mbi($quo), "$num / $num = $quo");
        is($mbi_rem, pl2mbi($rem), "$num % $num = $rem");

        # Check addresses.

        my ($mbi_quo_addr, $mbi_rem_addr);
        $mbi_quo_addr = refaddr($mbi_quo) if $scalar_util_ok;
        $mbi_rem_addr = refaddr($mbi_rem) if $scalar_util_ok;

        is($mbi_quo_addr, $mbi_num_addr,
           "the quotient object is the numerator object");

      SKIP: {
            skip "Scalar::Util not available", 1 unless $scalar_util_ok;

            ok($mbi_rem_addr != $mbi_num_addr &&
               $mbi_rem_addr != $mbi_quo_addr,
               "the remainder object is neither the numerator," .
               " denominator, nor quotient object");
        }
    }

    #######################################################################
    # btdiv() in scalar context.
    #######################################################################

    {
        # Compute expected output values.

        my $quo = tdiv($num, $num);

         note(qq|\n\$x = Math::BigInt -> new("$num"); | .
             qq|\$quo = \$x -> btdiv(\$x)\n\n|);

        # Input values as objects.

        my $mbi_num = Math::BigInt -> new("$num");

        # Get addresses for later tests.

        my $mbi_num_addr;
        $mbi_num_addr = refaddr($mbi_num) if $scalar_util_ok;

        # Compute actual output values.

        my $mbi_quo = $mbi_num -> btdiv($mbi_num);

        # Check classes.

        is(ref($mbi_num), 'Math::BigInt',
           "class of numerator is still Math::BigInt");

        is(ref($mbi_quo), 'Math::BigInt',
           "class of quotient is Math::BigInt");

        # Check values.

        is($mbi_quo, pl2mbi($quo), "$num / $num = $quo");

        # Check addresses.

        my $mbi_quo_addr;
        $mbi_quo_addr = refaddr($mbi_quo) if $scalar_util_ok;

      SKIP: {
            skip "Scalar::Util not available", 1 unless $scalar_util_ok;

            is($mbi_quo_addr, $mbi_num_addr,
               "the quotient object is the numerator object");
        }
    }

    #######################################################################
    # btmod() (scalar context only).
    #######################################################################

    {
        # Compute expected output values.

        my (undef, $rem) = tdiv($num, $num);

         note(qq|\n\$x = Math::BigInt -> new("$num") | .
             qq|\$quo = \$x -> btmod(\$x)\n\n|);

        # Input values as objects.

        my $mbi_num = Math::BigInt -> new("$num");

        # Get addresses for later tests.

        my $mbi_num_addr;
        $mbi_num_addr = refaddr($mbi_num) if $scalar_util_ok;

        # Compute actual output values.

        my $mbi_rem = $mbi_num -> btmod($mbi_num);

        # Check classes.

        is(ref($mbi_num), 'Math::BigInt',
           "class of numerator is still Math::BigInt");

        is(ref($mbi_rem), 'Math::BigInt',
           "class of remainder is Math::BigInt");

        # Check values.

        is($mbi_rem, pl2mbi($rem), "$num % $num = $rem");

        # Check addresses.

        my $mbi_rem_addr;
        $mbi_rem_addr = refaddr($mbi_rem) if $scalar_util_ok;

      SKIP: {
            skip "Scalar::Util not available", 1 unless $scalar_util_ok;

            is($mbi_rem_addr, $mbi_num_addr,
               "the remainder object is the numerator object");
        }
    }
}
