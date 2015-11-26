
# We need at least Perl 5.6
require 5.006002;
use strict;

# Load the Module::Install bundled in ./inc/
use inc::Module::Install;

# The name of your distribution
name            'Math-BigInt';

# Get most of the details from the primary module
all_from        'lib/Math/BigInt.pm';

# As long as Math::BigInt defaults to using Math::BigInt::FastCalc as its
# library (back-end), we require it (see RT #49569).
requires        'perl'                          => 5.006002;

# See if old versions are installed, and if so, tell the user that
# upgrading them would make little fluffy kittens much happier:

my @checks;
check_minimum_version ('Math::BigInt::FastCalc' => 0.25);
check_minimum_version ('Math::BigInt::GMP'      => 1.35);
check_minimum_version ('Math::BigInt::Pari'     => 1.15);
check_minimum_version ('Math::BigRat'           => 0.2602);
check_minimum_version ('bignum'                 => 0.22);

if (@checks > 0)
  {
  print <<"EOF";
**********************************************************************
 Attention: After installing this package, you should also update:

EOF
  for my $n (@checks)
    {
    my $name = $n->[0]; while (length($name) < 24) { $name .= ' '; }
    print "  $name to at least v$n->[1]\t (you have v$n->[2])\n";
    }
  print "\n";
  sleep(5);
  };

build_requires  'Test::More'    => 0.62;

install_as_core();

license         'perl';

author          'Tels <nospam-abuse@bloodgate.com>';

# Do not index these
no_index        directory       => 'examples';

sign;

# Generate the Makefile
WriteAll;

1;

sub check_minimum_version
  {
  my ($module, $version) = @_;

  my $v; eval " require $module; \$v = \$${module}::VERSION;";

  recommends    $module, $version;

  # if the module is installed, but outdated, add it to the requirements
  if (defined $v && $v < $version)
    {
    push @checks, [ $module, $version, $v ];
    }
  }
