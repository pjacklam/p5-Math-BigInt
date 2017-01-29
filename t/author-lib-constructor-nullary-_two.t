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

use Test::More tests => 5;

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

can_ok($LIB, '_two');

my $out0 = '2';

# List context.

{
    my @got;

    my $test = qq|\@got = $LIB->_two(); |;

    eval $test;
    is($@, "", "'$test' gives emtpy \$\@");

    subtest "_two() in list context: $test", sub {
        plan tests => 4,

        cmp_ok(scalar @got, '==', 1,
               "'$test' gives one output arg");

        is(ref($got[0]), $REF,
           "'$test' output arg is a $REF");

        is($LIB->_check($got[0]), 0,
           "'$test' output is valid");

        is($LIB->_str($got[0]), $out0,
           "'$test' output arg has the right value");
    };
}

# Scalar context.

{
    my $got;

    my $test = qq|\$got = $LIB->_two(); |;

    eval $test;
    is($@, "", "'$test' gives emtpy \$\@");

    subtest "_two() in scalar context: $test", sub {
        plan tests => 3,

        is(ref($got), $REF,
           "'$test' output arg is a $REF");

        is($LIB->_check($got), 0,
           "'$test' output is valid");

        is($LIB->_str($got), $out0,
           "'$test' output arg has the right value");
    };
}
