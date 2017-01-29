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

use Test::More tests => 10413;

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

can_ok($LIB, '_gcd');

my $scalar_util_ok = eval { require Scalar::Util; };
Scalar::Util -> import('refaddr') if $scalar_util_ok;

diag "Skipping some tests since Scalar::Util is not installed."
  unless $scalar_util_ok;

my @data;

# Add data in data file.

(my $datafile = $0) =~ s/\.t/.dat/;
open DATAFILE, $datafile or die "$datafile: can't open file for reading: $!";
while (<DATAFILE>) {
    s/\s+\z//;
    next if /^#/ || ! /\S/;
    push @data, [ split /:/ ];
}
close DATAFILE or die "$datafile: can't close file after reading: $!";

# List context.

for (my $i = 0 ; $i <= $#data ; ++ $i) {
    my ($in0, $in1, $out0) = @{ $data[$i] };

    my ($x, $y, @got);

    my $test = qq|\$x = $LIB->_new("$in0"); |
             . qq|\$y = $LIB->_new("$in1"); |
             . qq|\@got = $LIB->_gcd(\$x, \$y);|;

    eval $test;
    is($@, "", "'$test' gives emtpy \$\@");

    subtest "_gcd() in list context: $test", sub {
        plan tests => $scalar_util_ok ? 9 : 8;

        cmp_ok(scalar @got, "==", 1,
               "'$test' gives one output arg");

        is(ref($got[0]), $REF,
           "'$test' output arg is a $REF");

        is($LIB->_check($got[0]), 0,
           "'$test' output is valid");

        is($LIB->_str($got[0]), $out0,
           "'$test' output arg has the right value");

        isnt(refaddr($got[0]), refaddr($y),
             "'$test' output arg is not the second input arg")
          if $scalar_util_ok;

        is(ref($x), $REF,
           "'$test' first input arg is still a $REF");

        ok($LIB->_str($x) eq $out0 || $LIB->_str($x) eq $in0,
           "'$test' first input arg has the right value");

        is(ref($y), $REF,
           "'$test' second input arg is still a $REF");

        is($LIB->_str($y), $in1,
           "'$test' second input arg is unmodified");
    };
}

# Scalar context.

for (my $i = 0 ; $i <= $#data ; ++ $i) {
    my ($in0, $in1, $out0) = @{ $data[$i] };

    my ($got);
    my ($x, $y);
    my ($xo, $yo);

    my $test = qq|\$x = $LIB->_new("$in0"); |
             . qq|\$y = $LIB->_new("$in1"); |
             . qq|\$got = $LIB->_gcd(\$x, \$y);|;

    eval $test;
    is($@, "", "'$test' gives emtpy \$\@");

    subtest "_gcd() in scalar context: $test", sub {
        plan tests => $scalar_util_ok ? 8 : 7;

        is(ref($got), $REF,
           "'$test' output arg is a $REF");

        is($LIB->_check($got), 0,
           "'$test' output is valid");

        is($LIB->_str($got), $out0,
           "'$test' output arg has the right value");

        isnt(refaddr($got), refaddr($y),
             "'$test' output arg is not the second input arg")
          if $scalar_util_ok;

        is(ref($x), $REF,
           "'$test' first input arg is still a $REF");

        ok($LIB->_str($x) eq $out0 || $LIB->_str($x) eq $in0,
           "'$test' first input arg has the right value");

        is(ref($y), $REF,
           "'$test' second input arg is still a $REF");

        is($LIB->_str($y), $in1,
           "'$test' second input arg is unmodified");
    };
}
