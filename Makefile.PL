# We need at least Perl 5.6
require 5.006002;
use strict;

# Load the Module::Install bundled in ./inc/
use inc::Module::Install;

# The name of your distribution
name            'Math-BigInt';

# Get most of the details from the primary module
all_from        'lib/Math/BigInt.pm';

# Required versions.
requires        'perl'              => 5.006002;
requires        'Math::Complex'     => 1.39,
test_requires   'Test::More'        => 0.62;

install_as_core();

license         'perl';

author          'Peter John Acklam <pjacklam@online.no>';

# Do not index these
no_index        directory       => 'examples';

sign;

# Generate the Makefile
WriteAll;

1;
