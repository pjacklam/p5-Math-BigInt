# -*- mode: perl; -*-

use strict;
use warnings;

use Test::More tests => 121;

my $class = "Math::BigRat";
use_ok($class);

note("bfac() as a class method");

while (<DATA>) {
    s/^\s+//;
    next if /^#/ || !/\S/;

    my ($x, $want) = split;

    # bfac() as an instance method

    {
        my $y;
        my $test = qq|\$y = $class -> new("$x") -> bfac();|;
        note("\n$test\n\n");
        eval $test;
        die $@ if $@;
        is($y, $want);
    }

    # bfac() as a class method

    {
        my $y;
        my $test = qq|\$y = $class -> bfac("$x");|;
        note("\n$test\n\n");
        eval $test;
        die $@ if $@;
        is($y, $want);
    }

    # bfac() as a function does not work, since objectify() converts the scalar
    # to a Math::BigInt, which is the name of the package in which objectify()
    # is defined.

#    {
#        my ($y, $test);
#        $test = qq|\$y = $ {class}::bfac("$x");|;
#        note("\n$test\n\n");
#        eval $test;
#        die $@ if $@;
#        is($y, $want);
#    }

}

__DATA__

# Tests only for Math::BigRat

-3/2 NaN
-1/2 NaN
3/2 NaN
5/2 NaN

# Common tests for Math::BigInt, Math::BigFloat, and Math::BigRat:

NaN NaN
-inf NaN
-2 NaN
-1 NaN
0 1
1 1
2 2
3 6
4 24
5 120
6 720
7 5040
8 40320
9 362880
10 3628800
11 39916800
12 479001600
13 6227020800
14 87178291200
15 1307674368000
16 20922789888000
17 355687428096000
18 6402373705728000
19 121645100408832000
20 2432902008176640000
21 51090942171709440000
22 1124000727777607680000
23 25852016738884976640000
24 620448401733239439360000
25 15511210043330985984000000
26 403291461126605635584000000
27 10888869450418352160768000000
28 304888344611713860501504000000
29 8841761993739701954543616000000
30 265252859812191058636308480000000
31 8222838654177922817725562880000000
32 263130836933693530167218012160000000
33 8683317618811886495518194401280000000
34 295232799039604140847618609643520000000
35 10333147966386144929666651337523200000000
36 371993326789901217467999448150835200000000
37 13763753091226345046315979581580902400000000
38 523022617466601111760007224100074291200000000
39 20397882081197443358640281739902897356800000000
40 815915283247897734345611269596115894272000000000
41 33452526613163807108170062053440751665152000000000
42 1405006117752879898543142606244511569936384000000000
43 60415263063373835637355132068513997507264512000000000
44 2658271574788448768043625811014615890319638528000000000
45 119622220865480194561963161495657715064383733760000000000
46 5502622159812088949850305428800254892961651752960000000000
47 258623241511168180642964355153611979969197632389120000000000
48 12413915592536072670862289047373375038521486354677760000000000
49 608281864034267560872252163321295376887552831379210240000000000
50 30414093201713378043612608166064768844377641568960512000000000000
inf inf
