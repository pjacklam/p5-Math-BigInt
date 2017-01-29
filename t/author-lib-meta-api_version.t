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

can_ok($LIB, 'api_version');

# List context.

{
    my @got;

    my $test = qq|\@got = $LIB->api_version();|;

    eval $test;
    is($@, "", "'$test' gives emtpy \$\@");

    subtest "api_version() in list context: $test", sub {
        plan tests => 3,

        cmp_ok(scalar @got, '==', 1,
               "'$test' gives one output arg");

        is(ref($got[0]), "",
           "'$test' output is a Perl scalar");

        like($got[0], qr/^[1-9]\d*(\.\d+)?$/,
             "'$test' output is a decimal number'");
    };
}

# Scalar context.

{
    my $got;

    my $test = qq|\$got = $LIB->api_version();|;

    eval $test;
    is($@, "", "'$test' gives emtpy \$\@");

    subtest "api_version() in scalar context: $test", sub {
        plan tests => 2,

        is(ref($got), "",
           "'$test' output is a Perl scalar");

        like($got, qr/^[1-9]\d*(\.\d+)?$/,
             "'$test' output is a decimal number'");
    };
}
