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

use Test::More tests => 18001;

###############################################################################
# Read and load configuration file and backend library.

my $conffile = 't/author-lib-meta-config.conf';
open CONFFILE, $conffile or die "$conffile: can't open file for reading: $!";
my $confdata = do { local $/ = undef; <CONFFILE>; };
close CONFFILE or die "$conffile: can't close file after reading: $!";

our ($LIB, $REF);
eval $confdata;
die $@ if $@;

eval "require $LIB";
die $@ if $@;

###############################################################################

can_ok($LIB, '_log_int');

my $scalar_util_ok = eval { require Scalar::Util; };
Scalar::Util -> import('refaddr') if $scalar_util_ok;

diag "Skipping some tests since Scalar::Util is not installed."
  unless $scalar_util_ok;

my @data;

# Small numbers.

for (my $x = 1; $x <= 1000 ; ++ $x) {
    for (my $y = 2; $y <= 10 ; ++ $y) {
        push @data, [ $x, $y ];
    }
}

# List context.

for (my $i = 0 ; $i <= $#data ; ++ $i) {
    my ($in0, $in1) = @{ $data[$i] };

    my ($x, $y, @got);

    my $test = qq|\$x = $LIB->_new("$in0"); |
             . qq|\$y = $LIB->_new("$in1"); |
             . qq|\@got = $LIB->_log_int(\$x, \$y);|;

    eval $test;
    is($@, "", "'$test' gives emtpy \$\@");

    subtest "_log_int() in list context: $test", sub {
        plan tests => $scalar_util_ok ? 8 : 7;

        # Number of input arguments.

        cmp_ok(scalar @got, '==', 2,
               "'$test' gives two output args");

        # First output argument.

        is(ref($got[0]), $REF,
           "'$test' first output arg is a $REF");

        is($LIB->_check($got[0]), 0,
           "'$test' first output is valid");

        isnt(refaddr($got[0]), refaddr($y),
             "'$test' first output arg is not the second input arg")
          if $scalar_util_ok;

        is(ref($x), $REF,
           "'$test' first input arg is still a $REF");

        # Second output argument.

        is(ref($got[1]), "",
           "'$test' second output arg is a scalar");

        if(!defined($got[1]) || $got[1] == 0 || $got[1] == 1) {
            pass("'$test' second output arg is valid");
        } else {
            fail("'$test' second output arg is valid");
            diag("         got: $got[1]");
            diag("    expected: 0, 1, or undef");
        }

        # How to validate the first output argument depends on the second
        # output agument.

        if (!defined($got[1])) {

            # The output might be truncated, which means that it is smaller
            # than the exact result, or it might be exact.

            my $base    = $LIB->_new("$in1");

            my $expo_lo = $got[0];
            my $powr_lo = $LIB->_pow($LIB->_copy($base), $expo_lo);

            my $expo_hi = $LIB->_inc($LIB->_copy($got[0]));
            my $powr_hi = $LIB->_pow($LIB->_copy($base), $expo_hi);

            my $powr    = $LIB->_new("$in0");

            my $desc = "'$test' gave a value within the expected limits";
            if ($LIB->_acmp($powr_lo, $powr)    <=  0 &&
                $LIB->_acmp($powr,    $powr_hi) == -1)
            {
                pass($desc);
            } else {
                my $str_base    = $LIB->_str($base);
                my $str_expo_lo = $LIB->_str($expo_lo);
                my $str_expo_hi = $LIB->_str($expo_hi);
                my $str_powr_lo = $LIB->_str($powr_lo);
                my $str_powr_hi = $LIB->_str($powr_hi);
                fail($desc);
                diag("    The output value is '", $LIB->_str($got[0]),
                     "' which is either exact or truncated,",
                     " according to the status.");
                diag("      $str_base ** $str_expo_lo = $str_powr_lo",
                     " (lower limit)");
                diag("      $str_base ** $str_expo_hi = $str_powr_hi",
                     " (upper limit)");
                diag("    The follwing is NOT true:",
                     " $str_powr_lo <= $in0 < $str_powr_hi");
            }

        } elsif ($got[1] == 0) {

            # The output is truncated, which means that it is smaller than
            # the exact result.

            my $base    = $LIB->_new("$in1");

            my $expo_lo = $got[0];
            my $powr_lo = $LIB->_pow($LIB->_copy($base), $expo_lo);

            my $expo_hi = $LIB->_inc($LIB->_copy($got[0]));
            my $powr_hi = $LIB->_pow($LIB->_copy($base), $expo_hi);

            my $powr    = $LIB->_new("$in0");

            my $desc = "'$test' gave a value within the expected limits";
            if ($LIB->_acmp($powr_lo, $powr)    == -1 &&
                $LIB->_acmp($powr,    $powr_hi) == -1)
            {
                pass($desc);
            } else {
                my $str_base    = $LIB->_str($base);
                my $str_expo_lo = $LIB->_str($expo_lo);
                my $str_expo_hi = $LIB->_str($expo_hi);
                my $str_powr_lo = $LIB->_str($powr_lo);
                my $str_powr_hi = $LIB->_str($powr_hi);
                fail($desc);
                diag("    The output value is '", $LIB->_str($got[0]),
                     "' which is truncated, according to the status.");
                diag("      $str_base ** $str_expo_lo = $str_powr_lo",
                     " (lower limit");
                diag("      $str_base ** $str_expo_hi = $str_powr_hi",
                     " (upper limit)");
                diag("    The follwing is NOT true:",
                     " $str_powr_lo < $in0 < $str_powr_hi");
            }

        } elsif ($got[1] == 1) {

            # The output is exact.

            my $base    = $LIB->_new("$in1");
            my $expo_ex = $got[0];
            my $powr_ex = $LIB->_pow($LIB->_copy($base), $expo_ex);

            my $powr    = $LIB->_new("$in0");

            my $desc = "'$test' gave an exact value";
            if ($LIB->_acmp($powr_ex, $powr) == 0)
            {
                pass($desc);
            } else {
                my $str_base    = $LIB->_str($base);
                my $str_expo_ex = $LIB->_str($expo_ex);
                my $str_powr_ex = $LIB->_str($powr_ex);
                fail($desc);
                diag("    The output value is '", $LIB->_str($got[0]),
                     "' which is exact, according to the status.");
                diag("      $str_base ** $str_expo_ex = $str_powr_ex");
                diag("    The follwing is NOT true:",
                     " $str_powr_ex == $in0");
            }
        }

    };
}
