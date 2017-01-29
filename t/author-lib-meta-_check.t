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

use Test::More tests => 137;

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

can_ok($LIB, '_check');

# Generate test data.

my @data;

push @data, ([ "$LIB->_zero()", 1 ],      # valid objects
             [ "$LIB->_one()",  1 ],
             [ "$LIB->_two()",  1 ],
             [ "$LIB->_ten()",  1 ]);

for (my $n = 0 ; $n <= 24 ; ++ $n) {
    push @data, [ qq|$LIB->_new("1| . ("0" x $n) . qq|")|, 1 ];
}

push @data, ([ "undef",         0 ],      # invalid objects
             [ "''",            0 ],
             [ "1",             0 ],
             [ "[]",            0 ],
             [ "{}",            0 ]);

# List context.

for (my $i = 0 ; $i <= $#data ; ++ $i) {
    my ($in0, $out0) = @{ $data[$i] };

    my ($x, @got);

    my $test = qq|\$x = $in0; |
             . qq|\@got = $LIB->_check(\$x);|;

    eval $test;
    is($@, "", "'$test' gives emtpy \$\@");

    subtest "_check() in list context: $test", sub {
        plan tests => 3,

        cmp_ok(scalar @got, "==", 1,
               "'$test' gives one output arg");

        is(ref($got[0]), "",
           "'$test' output arg is a scalar");

        if ($out0) {                    # valid object
            is($got[0], 0,
               "'$test' output arg has the right value");
        } else {                        # invalid object
            isnt($got[0], "",
               "'$test' output arg is a non-empty string");
        }
    };
}

# Scalar context.

for (my $i = 0 ; $i <= $#data ; ++ $i) {
    my ($in0, $out0) = @{ $data[$i] };

    my ($x, $got);

    my $test = qq|\$x = $in0; |
             . qq|\$got = $LIB->_check(\$x);|;

    eval $test;
    is($@, "", "'$test' gives emtpy \$\@");

    subtest "_check() in scalar context: $test", sub {
        plan tests => 2,

        is(ref($got), "",
           "'$test' output arg is a scalar");

        if ($out0) {                    # valid object
            is($got, 0,
               "'$test' output arg has the right value");
        } else {                        # invalid object
            isnt($got, "",
               "'$test' output arg is a non-empty string");
        }
    };
}
