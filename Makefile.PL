#!perl

use strict;
use warnings;
use ExtUtils::MakeMaker;

WriteMakefile
  (
   'NAME'               => 'Math::BigInt',
   'VERSION_FROM'       => 'lib/Math/BigInt.pm',
   'AUTHOR'             => 'Peter John Acklam <pjacklam@gmail.com>',
   'MIN_PERL_VERSION'   => 5.006001,
   'PREREQ_PM'          => {
                            #'ExtUtils::MakeMaker' => 6.58,
                            'Test::More'          => 0.94,
                            'Math::Complex'       => 1.39,
                           },
   'LICENSE'            => 'perl_5',
   'LIBS'               => [''],        # e.g., '-lm'
   'DEFINE'             => '',          # e.g., '-DHAVE_SOMETHING'
   'INC'                => '',          # e.g., '-I/usr/include/other'
   'SIGN'               => 1,
  );
