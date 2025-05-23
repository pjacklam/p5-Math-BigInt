# -*- mode: perl; -*-

use strict;
use warnings;

use Test::More tests => 4112;
use Scalar::Util qw< refaddr >;

use Math::BigFloat;

# ceil(log(x) / log(2))

sub clog2 {
    my $x = shift;
    my $y = int(log($x) / log(2));

    my $trial = 2 ** $y;
    return $y if $trial == $x;

    while ($trial > $x) {
        $y--;
        $trial = 2 ** $y;
    }

    while ($trial < $x) {
        $y++;
        $trial = 2 ** $y;
    }

    return $y;
}

my @cases =
  (
   [ "NaN", "NaN" ],
   [ "-1",  "NaN" ],
   [ "0",  "-inf" ],
  );

for (my $x = 1 ; $x <= 1025 ; $x++) {
    my $y = clog2($x);
    push @cases, [ $x, $y ];
}

note("\nbclog2() as a class method");

for my $case (@cases) {

    my ($test, $y, @y);
    my ($in0, $out0) = @$case;

    # Scalar context.

    $test = qq|\$y = Math::BigFloat -> bclog2("$in0");|;
    note "\n", $test, "\n\n";
    eval $test;
    die $@ if $@;

    subtest $test => sub {
        plan tests => 2;

        is(ref($y), 'Math::BigFloat', '$y is a Math::BigFloat');
        is($y, $out0, 'value of $y');
    };

    # List context.

    $test = qq|\@y = Math::BigFloat -> bclog2("$in0");|;
    note "\n", $test, "\n\n";
    eval $test;
    die $@ if $@;

    subtest $test => sub {
        plan tests => 3;

        is(scalar(@y), 1, 'number of output arguments');
        is(ref($y[0]), 'Math::BigFloat', '$y[0] is a Math::BigFloat');
        is($y[0], $out0, 'value of $y[0]');
    };
}

note("\nbclog2() as an instance method");

for my $case (@cases) {

    my ($test, $x, $y, @y);
    my ($in0, $out0) = @$case;

    # Scalar context.

    $test = qq|\$x = Math::BigFloat -> new("$in0"); |
          . qq|\$y = \$x -> bclog2();|;
    note "\n", $test, "\n\n";
    eval $test;
    die $@ if $@;

    subtest $test => sub {
        plan tests => 2;

        is(ref($y), 'Math::BigFloat', '$y is a Math::BigFloat');
        is($y, $out0, 'value of $y');
    };

    # List context.

    $test = qq|\@y = Math::BigFloat -> bclog2("$in0");|;
    note "\n", $test, "\n\n";
    eval $test;
    die $@ if $@;

    subtest $test => sub {
        plan tests => 3;

        is(scalar(@y), 1, 'number of output arguments');
        is(ref($y[0]), 'Math::BigFloat', '$y[0] is a Math::BigFloat');
        is($y[0], $out0, 'value of $y[0]');
    };
}
