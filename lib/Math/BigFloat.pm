package Math::BigFloat;

#
# Mike grinned. 'Two down, infinity to go' - Mike Nostrus in 'Before and After'
#

# The following hash values are used internally:
#
#          sign : "+", "-", "+inf", "-inf", or "NaN"
#            _m : absolute value of mantissa ($LIB thingy)
#           _es : sign of exponent ("+" or "-")
#            _e : absolute value of exponent ($LIB thingy)
#      accuracy : accuracy (scalar)
#     precision : precision (scalar)

use 5.006001;
use strict;
use warnings;

use Carp          qw< carp croak >;
use Scalar::Util  qw< blessed >;
use Math::BigInt  qw< >;

our $VERSION = '2.005003';
$VERSION =~ tr/_//d;

require Exporter;
our @ISA        = qw< Math::BigInt >;
our @EXPORT_OK  = qw< bpi >;

use overload

  # overload key: with_assign

  '+'     =>      sub { $_[0] -> copy() -> badd($_[1]); },

  '-'     =>      sub { my $c = $_[0] -> copy();
                        $_[2] ? $c -> bneg() -> badd($_[1])
                              : $c -> bsub($_[1]); },

  '*'     =>      sub { $_[0] -> copy() -> bmul($_[1]); },

  '/'     =>      sub { $_[2] ? ref($_[0]) -> new($_[1]) -> bdiv($_[0])
                              : $_[0] -> copy() -> bdiv($_[1]); },

  '%'     =>      sub { $_[2] ? ref($_[0]) -> new($_[1]) -> bmod($_[0])
                              : $_[0] -> copy() -> bmod($_[1]); },

  '**'    =>      sub { $_[2] ? ref($_[0]) -> new($_[1]) -> bpow($_[0])
                              : $_[0] -> copy() -> bpow($_[1]); },

  '<<'    =>      sub { $_[2] ? ref($_[0]) -> new($_[1]) -> bblsft($_[0])
                              : $_[0] -> copy() -> bblsft($_[1]); },

  '>>'    =>      sub { $_[2] ? ref($_[0]) -> new($_[1]) -> bbrsft($_[0])
                              : $_[0] -> copy() -> bbrsft($_[1]); },

  # overload key: assign

  '+='    =>      sub { $_[0] -> badd($_[1]); },

  '-='    =>      sub { $_[0] -> bsub($_[1]); },

  '*='    =>      sub { $_[0] -> bmul($_[1]); },

  '/='    =>      sub { scalar $_[0] -> bdiv($_[1]); },

  '%='    =>      sub { $_[0] -> bmod($_[1]); },

  '**='   =>      sub { $_[0] -> bpow($_[1]); },

  '<<='   =>      sub { $_[0] -> bblsft($_[1]); },

  '>>='   =>      sub { $_[0] -> bbrsft($_[1]); },

#  'x='    =>      sub { },

#  '.='    =>      sub { },

  # overload key: num_comparison

  '<'     =>      sub { $_[2] ? ref($_[0]) -> new($_[1]) -> blt($_[0])
                              : $_[0] -> blt($_[1]); },

  '<='    =>      sub { $_[2] ? ref($_[0]) -> new($_[1]) -> ble($_[0])
                              : $_[0] -> ble($_[1]); },

  '>'     =>      sub { $_[2] ? ref($_[0]) -> new($_[1]) -> bgt($_[0])
                              : $_[0] -> bgt($_[1]); },

  '>='    =>      sub { $_[2] ? ref($_[0]) -> new($_[1]) -> bge($_[0])
                              : $_[0] -> bge($_[1]); },

  '=='    =>      sub { $_[0] -> beq($_[1]); },

  '!='    =>      sub { $_[0] -> bne($_[1]); },

  # overload key: 3way_comparison

  '<=>'   =>      sub { my $cmp = $_[0] -> bcmp($_[1]);
                        defined($cmp) && $_[2] ? -$cmp : $cmp; },

  'cmp'   =>      sub { $_[2] ? "$_[1]" cmp $_[0] -> bstr()
                              : $_[0] -> bstr() cmp "$_[1]"; },

  # overload key: str_comparison

#  'lt'    =>      sub { $_[2] ? ref($_[0]) -> new($_[1]) -> bstrlt($_[0])
#                              : $_[0] -> bstrlt($_[1]); },
#
#  'le'    =>      sub { $_[2] ? ref($_[0]) -> new($_[1]) -> bstrle($_[0])
#                              : $_[0] -> bstrle($_[1]); },
#
#  'gt'    =>      sub { $_[2] ? ref($_[0]) -> new($_[1]) -> bstrgt($_[0])
#                              : $_[0] -> bstrgt($_[1]); },
#
#  'ge'    =>      sub { $_[2] ? ref($_[0]) -> new($_[1]) -> bstrge($_[0])
#                              : $_[0] -> bstrge($_[1]); },
#
#  'eq'    =>      sub { $_[0] -> bstreq($_[1]); },
#
#  'ne'    =>      sub { $_[0] -> bstrne($_[1]); },

  # overload key: binary

  '&'     =>      sub { $_[2] ? ref($_[0]) -> new($_[1]) -> band($_[0])
                              : $_[0] -> copy() -> band($_[1]); },

  '&='    =>      sub { $_[0] -> band($_[1]); },

  '|'     =>      sub { $_[2] ? ref($_[0]) -> new($_[1]) -> bior($_[0])
                              : $_[0] -> copy() -> bior($_[1]); },

  '|='    =>      sub { $_[0] -> bior($_[1]); },

  '^'     =>      sub { $_[2] ? ref($_[0]) -> new($_[1]) -> bxor($_[0])
                              : $_[0] -> copy() -> bxor($_[1]); },

  '^='    =>      sub { $_[0] -> bxor($_[1]); },

#  '&.'    =>      sub { },

#  '&.='   =>      sub { },

#  '|.'    =>      sub { },

#  '|.='   =>      sub { },

#  '^.'    =>      sub { },

#  '^.='   =>      sub { },

  # overload key: unary

  'neg'   =>      sub { $_[0] -> copy() -> bneg(); },

#  '!'     =>      sub { },

  '~'     =>      sub { $_[0] -> copy() -> bnot(); },

#  '~.'    =>      sub { },

  # overload key: mutators

  '++'    =>      sub { $_[0] -> binc() },

  '--'    =>      sub { $_[0] -> bdec() },

  # overload key: func

  'atan2' =>      sub { $_[2] ? ref($_[0]) -> new($_[1]) -> batan2($_[0])
                              : $_[0] -> copy() -> batan2($_[1]); },

  'cos'   =>      sub { $_[0] -> copy() -> bcos(); },

  'sin'   =>      sub { $_[0] -> copy() -> bsin(); },

  'exp'   =>      sub { $_[0] -> copy() -> bexp($_[1]); },

  'abs'   =>      sub { $_[0] -> copy() -> babs(); },

  'log'   =>      sub { $_[0] -> copy() -> blog(); },

  'sqrt'  =>      sub { $_[0] -> copy() -> bsqrt(); },

  'int'   =>      sub { $_[0] -> copy() -> bint(); },

  # overload key: conversion

  'bool'  =>      sub { $_[0] -> is_zero() ? '' : 1; },

  '""'    =>      sub { $_[0] -> bstr(); },

  '0+'    =>      sub { $_[0] -> numify(); },

  '='     =>      sub { $_[0] -> copy(); },

  ;

##############################################################################
# global constants, flags and assorted stuff

# the following are public, but their usage is not recommended. Use the
# accessor methods instead.

# class constants, use Class->constant_name() to access
# one of 'even', 'odd', '+inf', '-inf', 'zero', 'trunc' or 'common'

our $accuracy   = undef;
our $precision  = undef;
our $round_mode = 'even';
our $div_scale  = 40;

our $upgrade    = undef;
our $downgrade  = undef;

our $_trap_nan  = 0;            # croak on NaNs?
our $_trap_inf  = 0;            # croak on Infs?

my $nan = 'NaN';                                # constant for easier life

my $LIB = Math::BigInt -> config('lib');        # math backend library

# Has import() been called yet? This variable is needed to make "require" work.

my $IMPORT = 0;

# some digits of accuracy for blog(undef, 10); which we use in blog() for speed
my $LOG_10 =
 '2.3025850929940456840179914546843642076011014886287729760333279009675726097';
my $LOG_10_A = length($LOG_10)-1;
# ditto for log(2)
my $LOG_2 =
 '0.6931471805599453094172321214581765680755001343602552541206800094933936220';
my $LOG_2_A = length($LOG_2)-1;
my $HALF = '0.5';                       # made into an object if nec.

##############################################################################
# the old code had $rnd_mode, so we need to support it, too

our $rnd_mode;
our $AUTOLOAD;

sub TIESCALAR {
    my ($class) = @_;
    bless \$round_mode, $class;
}

sub FETCH {
    return $round_mode;
}

sub STORE {
    $rnd_mode = (ref $_[0]) -> round_mode($_[1]);
}

BEGIN {
    *objectify = \&Math::BigInt::objectify;

    # when someone sets $rnd_mode, we catch this and check the value to see
    # whether it is valid or not.
    $rnd_mode   = 'even';
    tie $rnd_mode, 'Math::BigFloat';

    *as_number = \&as_int;
}

sub DESTROY {
    # going through AUTOLOAD for every DESTROY is costly, avoid it by empty sub
}

sub AUTOLOAD {

    # Make fxxx() work by mapping fxxx() to Math::BigFloat::bxxx().

    my $name = $AUTOLOAD;
    $name =~ s/^(.*):://;               # strip package name
    my $class = $1 || __PACKAGE__;

    $class -> import() if $IMPORT == 0;

    # E.g., "fabs" -> "babs", but "is_neg" -> "is_neg"

    my $bname = $name;
    $bname =~ s/^f/b/;

    # Map, e.g., Math::BigFloat::fabs() to Math::BigFloat::babs()

    if ($bname ne $name && Math::BigFloat -> can($bname)) {
        no strict 'refs';
        return &{"Math::BigFloat::$bname"}(@_);
    }

    # Map, e.g., Math::BigFloat::babs() to Math::BigInt::babs()

    elsif (Math::BigInt -> can($bname)) {
        no strict 'refs';
        return &{"Math::BigInt::$bname"}(@_);
    }

    else {
        croak("Can't call $class->$name(), not a valid method");
    }
}

##############################################################################

# Compare the following function with @ISA above. This inheritance mess needs a
# clean up. When doing so, also consider the BEGIN block and the AUTOLOAD code.
# Fixme!

sub isa {
    my ($self, $class) = @_;
    return if $class =~ /^Math::BigInt/; # we aren't one of these
    UNIVERSAL::isa($self, $class);
}

sub config {
    my $self  = shift;
    my $class = ref($self) || $self || __PACKAGE__;

    # Getter/accessor.

    if (@_ == 1 && ref($_[0]) ne 'HASH') {
        my $param = shift;
        return $class if $param eq 'class';
        return $LIB   if $param eq 'with';
        return $self -> SUPER::config($param);
    }

    # Setter.

    my $cfg = $self -> SUPER::config(@_);

    # We need only to override the ones that are different from our parent.

    unless (ref($self)) {
        $cfg->{class} = $class;
        $cfg->{with}  = $LIB;
    }

    $cfg;
}

###############################################################################
# Constructor methods
###############################################################################

sub new {
    # Create a new Math::BigFloat object from a string or another Math::BigInt,
    # Math::BigFloat, or Math::BigRat object. See hash keys documented at top.

    my $self    = shift;
    my $selfref = ref $self;
    my $class   = $selfref || $self;

    # Make "require" work.

    $class -> import() if $IMPORT == 0;

    # Calling new() with no input arguments has been discouraged for more than
    # 10 years, but people apparently still use it, so we still support it.

    return $class -> bzero() unless @_;

    my ($wanted, @r) = @_;

    if (!defined($wanted)) {
        #if (warnings::enabled("uninitialized")) {
        #    warnings::warn("uninitialized",
        #                   "Use of uninitialized value in new()");
        #}
        return $class -> bzero(@r);
    }

    if (!ref($wanted) && $wanted eq "") {
        #if (warnings::enabled("numeric")) {
        #    warnings::warn("numeric",
        #                   q|Argument "" isn't numeric in new()|);
        #}
        #return $class -> bzero(@r);
        return $class -> bnan(@r);
    }

    # Initialize a new object.

    $self = bless {}, $class;

    # See if $wanted is an object that is a Math::BigFloat or can convert
    # itself to a Math::BigFloat.

    if (defined(blessed($wanted)) && $wanted -> can('as_float')) {
        my $tmp = $wanted -> as_float(@r);
        for my $attr ('sign', '_m', '_es', '_e') {
            $self -> {$attr} = $tmp -> {$attr};
        }
        return $self -> round(@r);
    }

    # From now on we only work on the stringified version of $wanted, so
    # stringify it once and for all.

    $wanted = "$wanted";

    # Shortcut for simple forms like '123' that have no trailing zeros.
    # Trailing zeros would require a non-zero exponent.

    if ($wanted =~
        / ^
          \s*                           # optional leading whitespace
          ( [+-]? )                     # optional sign
          0*                            # optional leading zeros
          ( [1-9] (?: [0-9]* [1-9] )? ) # significand
          \s*                           # optional trailing whitespace
          $
        /x)
    {
        my $dng = $class -> downgrade();
        return $dng -> new($1 . $2) if $dng && $dng ne $class;
        $self->{sign} = $1 || '+';
        $self->{_m}   = $LIB -> _new($2);
        $self->{_es}  = '+';
        $self->{_e}   = $LIB -> _zero();
        $self -> round(@r)
          unless @r >= 2 && !defined $r[0] && !defined $r[1];
        return $self;
    }

    # Handle Infs.

    if ($wanted =~ / ^
                     \s*
                     ( [+-]? )
                     inf (?: inity )?
                     \s*
                     \z
                   /ix)
    {
        my $sgn = $1 || '+';
        return $class -> binf($sgn, @r);
    }

    # Handle explicit NaNs (not the ones returned due to invalid input).

    if ($wanted =~ / ^
                     \s*
                     ( [+-]? )
                     nan
                     \s*
                     \z
                   /ix)
    {
        return $class -> bnan(@r);
    }

    my @parts;

    if (
        # Handle hexadecimal numbers. We auto-detect hexadecimal numbers if
        # they have a "0x", "0X", "x", or "X" prefix, cf. CORE::oct().

        $wanted =~ /^\s*[+-]?0?[Xx]/ and
        @parts = $class -> _hex_str_to_flt_lib_parts($wanted)

          or

        # Handle octal numbers. We auto-detect octal numbers if they have a
        # "0o", "0O", "o", "O" prefix, cf. CORE::oct().

        $wanted =~ /^\s*[+-]?0?[Oo]/ and
        @parts = $class -> _oct_str_to_flt_lib_parts($wanted)

          or

        # Handle binary numbers. We auto-detect binary numbers if they have a
        # "0b", "0B", "b", or "B" prefix, cf. CORE::oct().

        $wanted =~ /^\s*[+-]?0?[Bb]/ and
        @parts = $class -> _bin_str_to_flt_lib_parts($wanted)

          or

        # At this point, what is left are decimal numbers that aren't handled
        # above and octal floating point numbers that don't have any of the
        # "0o", "0O", "o", or "O" prefixes. First see if it is a decimal
        # number.

        @parts = $class -> _dec_str_to_flt_lib_parts($wanted)
          or

        # See if it is an octal floating point number. The extra check is
        # included because _oct_str_to_flt_lib_parts() accepts octal numbers
        # that don't have a prefix (this is needed to make it work with, e.g.,
        # from_oct() that don't require a prefix). However, Perl requires a
        # prefix for octal floating point literals. For example, "1p+0" is not
        # valid, but "01p+0" and "0__1p+0" are.

        $wanted =~ /^\s*[+-]?0_*\d/ and
        @parts = $class -> _oct_str_to_flt_lib_parts($wanted))
    {
        ($self->{sign}, $self->{_m}, $self->{_es}, $self->{_e}) = @parts;

        $self -> round(@r)
          unless @r >= 2 && !defined($r[0]) && !defined($r[1]);

        $self -> _dng() if ($self -> is_int() ||
                            $self -> is_inf() ||
                            $self -> is_nan());

        return $self;
    }

    # If we get here, the value is neither a valid decimal, binary, octal, or
    # hexadecimal number. It is not an explicit Inf or a NaN either.

    return $class -> bnan(@r);
}

sub from_dec {
    my $self    = shift;
    my $selfref = ref $self;
    my $class   = $selfref || $self;

    # Make "require" work.

    $class -> import() if $IMPORT == 0;

    # Don't modify constant (read-only) objects.

    return $self if $selfref && $self -> modify('from_dec');

    my $str = shift;
    my @r = @_;

    if (my @parts = $class -> _dec_str_to_flt_lib_parts($str)) {

        # If called as a class method, initialize a new object.

        unless ($selfref) {
            $self = bless {}, $class;
            #$self -> _init();
        }

        ($self->{sign}, $self->{_m}, $self->{_es}, $self->{_e}) = @parts;

        $self -> round(@r)
          unless @r >= 2 && !defined($r[0]) && !defined($r[1]);

        $self -> _dng() if ($self -> is_int() ||
                            $self -> is_inf() ||
                            $self -> is_nan());

        return $self;
    }

    return $self -> bnan(@r);
}

sub from_hex {
    my $self    = shift;
    my $selfref = ref $self;
    my $class   = $selfref || $self;

    # Make "require" work.

    $class -> import() if $IMPORT == 0;

    # Don't modify constant (read-only) objects.

    return $self if $selfref && $self -> modify('from_hex');

    my $str = shift;
    my @r = @_;

    if (my @parts = $class -> _hex_str_to_flt_lib_parts($str)) {

        # If called as a class method, initialize a new object.

        unless ($selfref) {
            $self = bless {}, $class;
            #$self -> _init();
        }

        ($self->{sign}, $self->{_m}, $self->{_es}, $self->{_e}) = @parts;

        $self -> round(@r)
          unless @r >= 2 && !defined($r[0]) && !defined($r[1]);

        $self -> _dng() if ($self -> is_int() ||
                            $self -> is_inf() ||
                            $self -> is_nan());
        return $self;
    }

    return $self -> bnan(@r);
}

sub from_oct {
    my $self    = shift;
    my $selfref = ref $self;
    my $class   = $selfref || $self;

    # Make "require" work.

    $class -> import() if $IMPORT == 0;

    # Don't modify constant (read-only) objects.

    return $self if $selfref && $self -> modify('from_oct');

    my $str = shift;
    my @r = @_;

    if (my @parts = $class -> _oct_str_to_flt_lib_parts($str)) {

        # If called as a class method, initialize a new object.

        unless ($selfref) {
            $self = bless {}, $class;
            #$self -> _init();
        }

        ($self->{sign}, $self->{_m}, $self->{_es}, $self->{_e}) = @parts;

        $self -> round(@r)
          unless @r >= 2 && !defined($r[0]) && !defined($r[1]);

        $self -> _dng() if ($self -> is_int() ||
                                                    $self -> is_inf() ||
                                                    $self -> is_nan());
        return $self;
    }

    return $self -> bnan(@r);
}

sub from_bin {
    my $self    = shift;
    my $selfref = ref $self;
    my $class   = $selfref || $self;

    # Make "require" work.

    $class -> import() if $IMPORT == 0;

    # Don't modify constant (read-only) objects.

    return $self if $selfref && $self -> modify('from_bin');

    my $str = shift;
    my @r = @_;

    if (my @parts = $class -> _bin_str_to_flt_lib_parts($str)) {

        # If called as a class method, initialize a new object.

        unless ($selfref) {
            $self = bless {}, $class;
            #$self -> _init();
        }

        ($self->{sign}, $self->{_m}, $self->{_es}, $self->{_e}) = @parts;

        $self -> round(@r)
          unless @r >= 2 && !defined($r[0]) && !defined($r[1]);

        $self -> _dng() if ($self -> is_int() ||
                            $self -> is_inf() ||
                            $self -> is_nan());
        return $self;
    }

    return $self -> bnan(@r);
}

sub from_bytes {
    my $self    = shift;
    my $selfref = ref $self;
    my $class   = $selfref || $self;

    # Make "require" work.

    $class -> import() if $IMPORT == 0;

    # Don't modify constant (read-only) objects.

    return $self if $selfref && $self -> modify('from_bytes');

    my $str = shift;
    my @r = @_;

    # If called as a class method, initialize a new object.

    $self = $class -> bzero(@r) unless $selfref;

    $self -> {sign} = "+";
    $self -> {_m}   = $LIB -> _from_bytes($str);
    $self -> {_es}  = "+";
    $self -> {_e}   = $LIB -> _zero();
    $self -> bnorm();

    $self -> _dng();
    return $self;
}

sub from_ieee754 {
    my $self    = shift;
    my $selfref = ref $self;
    my $class   = $selfref || $self;

    # Make "require" work.

    $class -> import() if $IMPORT == 0;

    # Don't modify constant (read-only) objects.

    return $self if $selfref && $self -> modify('from_ieee754');

    my $in     = shift;     # input string (or raw bytes)
    my $format = shift;     # format ("binary32", "decimal64" etc.)
    my $enc;                # significand encoding (applies only to decimal)
    my $k;                  # storage width in bits
    my $b;                  # base
    my @r = @_;             # rounding parameters, if any

    if ($format =~ /^binary(\d+)\z/) {
        $k = $1;
        $b = 2;
    } elsif ($format =~ /^decimal(\d+)(dpd|bcd)?\z/) {
        $k = $1;
        $b = 10;
        $enc = $2 || 'dpd';     # default is dencely-packed decimals (DPD)
    } elsif ($format eq 'half') {
        $k = 16;
        $b = 2;
    } elsif ($format eq 'single') {
        $k = 32;
        $b = 2;
    } elsif ($format eq 'double') {
        $k = 64;
        $b = 2;
    } elsif ($format eq 'quadruple') {
        $k = 128;
        $b = 2;
    } elsif ($format eq 'octuple') {
        $k = 256;
        $b = 2;
    } elsif ($format eq 'sexdecuple') {
        $k = 512;
        $b = 2;
    }

    if ($b == 2) {

        # Get the parameters for this format.

        my $p;                      # precision (in bits)
        my $t;                      # number of bits in significand
        my $w;                      # number of bits in exponent

        if ($k == 16) {             # binary16 (half-precision)
            $p = 11;
            $t = 10;
            $w =  5;
        } elsif ($k == 32) {        # binary32 (single-precision)
            $p = 24;
            $t = 23;
            $w =  8;
        } elsif ($k == 64) {        # binary64 (double-precision)
            $p = 53;
            $t = 52;
            $w = 11;
        } else {                    # binaryN (quadruple-precision and above)
            if ($k < 128 || $k != 32 * sprintf('%.0f', $k / 32)) {
                croak "Number of bits must be 16, 32, 64, or >= 128 and",
                  " a multiple of 32";
            }
            $p = $k - sprintf('%.0f', 4 * log($k) / log(2)) + 13;
            $t = $p - 1;
            $w = $k - $t - 1;
        }

        # The maximum exponent, minimum exponent, and exponent bias.

        my $emax = $class -> new(2) -> bpow($w - 1) -> bdec();
        my $emin = 1 - $emax;
        my $bias = $emax;

        # Undefined input.

        unless (defined $in) {
            carp("Input is undefined");
            return $self -> bzero(@r);
        }

        # Make sure input string is a string of zeros and ones.

        my $len = CORE::length $in;
        if (8 * $len == $k) {                   # bytes
            $in = unpack "B*", $in;
        } elsif (4 * $len == $k) {              # hexadecimal
            if ($in =~ /([^\da-f])/i) {
                croak "Illegal hexadecimal digit '$1'";
            }
            $in = unpack "B*", pack "H*", $in;
        } elsif ($len == $k) {                  # bits
            if ($in =~ /([^01])/) {
                croak "Illegal binary digit '$1'";
            }
        } else {
            croak "Unknown input -- $in";
        }

        # Split bit string into sign, exponent, and mantissa/significand.

        my $sign = substr($in, 0, 1) eq '1' ? '-' : '+';
        my $expo = $class -> from_bin(substr($in, 1, $w));
        my $mant = $class -> from_bin(substr($in, $w + 1));

        my $x;

        $expo -> bsub($bias);                   # subtract bias

        if ($expo < $emin) {                    # zero and subnormals
            if ($mant == 0) {                   # zero
                $x = $class -> bzero();
            } else {                            # subnormals
                # compute (1/$b)**(N) rather than ($b)**(-N)
                $x = $class -> new("0.5");      # 1/$b
                $x -> bpow($bias + $t - 1) -> bmul($mant);
                $x -> bneg() if $sign eq '-';
            }
        }

        elsif ($expo > $emax) {                 # inf and nan
            if ($mant == 0) {                   # inf
                $x = $class -> binf($sign);
            } else {                            # nan
                $x = $class -> bnan(@r);
            }
        }

        else {                                  # normals
            $mant = $class -> new(2) -> bpow($t) -> badd($mant);
            if ($expo < $t) {
                # compute (1/$b)**(N) rather than ($b)**(-N)
                $x = $class -> new("0.5");      # 1/$b
                $x -> bpow($t - $expo) -> bmul($mant);
            } else {
                $x = $class -> new(2);
                $x -> bpow($expo - $t) -> bmul($mant);
            }
            $x -> bneg() if $sign eq '-';
        }

        if ($selfref) {
            $self -> {sign} = $x -> {sign};
            $self -> {_m}   = $x -> {_m};
            $self -> {_es}  = $x -> {_es};
            $self -> {_e}   = $x -> {_e};
        } else {
            $self = $x;
        }

        $self -> round(@r);
        $self -> _dng() if ($self -> is_int() ||
                            $self -> is_inf() ||
                            $self -> is_nan());
        return $self;
    }

    croak("The format '$format' is not yet supported.");
}

sub from_fp80 {
    my $self    = shift;
    my $selfref = ref $self;
    my $class   = $selfref || $self;

    # Make "require" work.

    $class -> import() if $IMPORT == 0;

    # Don't modify constant (read-only) objects.

    return $self if $selfref && $self -> modify('from_fp80');

    my $in = shift;         # input string (or raw bytes)
    my @r = @_;             # rounding parameters, if any

    # Undefined input.

    unless (defined $in) {
        carp("Input is undefined");
        return $self -> bzero(@r);
    }

    # The parameters for this format.

    my $p = 64;                 # precision (in bits)
    my $w = 15;                 # number of bits in exponent

    # The maximum exponent, minimum exponent, and exponent bias.

    my $emax = $class -> new(2) -> bpow($w - 1) -> bdec();      # = 16383
    my $emin = 1 - $emax;                                       # = -16382
    my $bias = $emax;                                           # = -16383

    # Make sure input string is a string of zeros and ones.

    my $len = CORE::length $in;
    if (8 * $len == 80) {                       # bytes
        $in = unpack "B*", $in;
    } elsif (4 * $len == 80) {                  # hexadecimal
        if ($in =~ /([^\da-f])/i) {
            croak "Illegal hexadecimal digit '$1'";
        }
        $in = unpack "B*", pack "H*", $in;
    } elsif ($len == 80) {                      # bits
        if ($in =~ /([^01])/) {
            croak "Illegal binary digit '$1'";
        }
    } else {
        croak "Unknown input -- $in";
    }

    # Split bit string into sign, exponent, and mantissa/significand.

    my $sign = substr($in, 0, 1) eq '1' ? '-' : '+';
    my $expo = $class -> from_bin(substr($in, 1, $w));
    my $mant = $class -> from_bin(substr($in, $w + 1));

    my $x;

    $expo -> bsub($bias);                       # subtract bias

    # zero and subnormal numbers

    if ($expo < $emin) {
        if ($mant == 0) {                       # zero
            $x = $class -> bzero();
        } else {                                # subnormals
            # compute (1/2)**N rather than 2**(-N)
            $x = $class -> new("0.5");
            $x -> bpow(-$emin - 1 + $p) -> bmul($mant);
            $x -> bneg() if $sign eq '-';
        }
    }

    # inf and nan

    elsif ($expo > $emax) {

        # if fraction of mantissa is zero, i.e., if mantissa is
        # 0.000... or 1.000...

        if (substr($in, 16) =~ /^[01]0+$/) {
            $x = $class -> binf($sign);
        } else {
            $x = $class -> bnan();
        }
    }

    # normal numbers

    else {

        # downscale mantissa
        $mant -> blsft($p - 1, "0.5");      # brsft($p - 1, 2) does division

        if ($expo < 0) {
            # compute (1/2)**N rather than 2**(-N)
            $x = $mant -> blsft(-$expo, "0.5");
        } elsif ($expo > 0) {
            $x = $mant -> blsft($expo, "2");
        } else {
            $x = $mant;
        }

        $x -> bneg() if $sign eq '-';
    }

    if ($selfref) {
        $self -> {sign} = $x -> {sign};
        $self -> {_m}   = $x -> {_m};
        $self -> {_es}  = $x -> {_es};
        $self -> {_e}   = $x -> {_e};
    } else {
        $self = $x;
    }

    $self -> round(@r);
    $self -> _dng() if ($self -> is_int() ||
                        $self -> is_inf() ||
                        $self -> is_nan());
    return $self;
}

sub from_base {
    my $self    = shift;
    my $selfref = ref $self;
    my $class   = $selfref || $self;

    # Make "require" work.

    $class -> import() if $IMPORT == 0;

    # Don't modify constant (read-only) objects.

    return $self if $selfref && $self -> modify('from_base');

    my ($str, $base, $cs, @r) = @_;     # $cs is the collation sequence

    $base = $class -> new($base) unless ref($base);

    croak("the base must be a finite integer >= 2")
      if $base < 2 || ! $base -> is_int();

    # If called as a class method, initialize a new object.

    $self = $class -> bzero() unless $selfref;

    # If no collating sequence is given, pass some of the conversions to
    # methods optimized for those cases.

    unless (defined $cs) {
        return $self -> from_bin($str, @r) if $base == 2;
        return $self -> from_oct($str, @r) if $base == 8;
        return $self -> from_hex($str, @r) if $base == 16;
        return $self -> from_dec($str, @r) if $base == 10;
    }

    croak("from_base() requires a newer version of the $LIB library.")
      unless $LIB -> can('_from_base');

    my $base_lib = $LIB -> _lsft($LIB -> _copy($base->{_m}), $base->{_e}, 10);
    $self -> {sign} = '+';
    $self -> {_m}   = $LIB->_from_base($str, $base_lib,
                                       defined($cs) ? $cs : ());
    $self -> {_es}  = "+";
    $self -> {_e}   = $LIB->_zero();
    $self -> bnorm();

    $self -> bround(@r);
    $self -> _dng();
    return $self;
}

sub bzero {
    # create/assign '+0'

    # Class::method(...) -> Class->method(...)
    unless (@_ && (defined(blessed($_[0])) && $_[0] -> isa(__PACKAGE__) ||
                   $_[0] =~ /^[a-z]\w*(?:::[a-z]\w*)*$/i))
    {
        #carp "Using ", (caller(0))[3], "() as a function is deprecated;",
        #  " use is as a method instead";
        unshift @_, __PACKAGE__;
    }

    my $self    = shift;
    my $selfref = ref $self;
    my $class   = $selfref || $self;

    # Make "require" work.

    $class -> import() if $IMPORT == 0;

    # Don't modify constant (read-only) objects.

    return $self if $selfref && $self -> modify('bzero');

    my $dng = $class -> downgrade();
    if ($dng && $dng ne $class) {
        return $self -> _dng() -> bzero(@_) if $selfref;
        return $dng -> bzero(@_);
    }

    # Get the rounding parameters, if any.

    my @r = @_;

    # If called as a class method, initialize a new object.

    $self = bless {}, $class unless $selfref;

    $self -> {sign} = '+';
    $self -> {_m}   = $LIB -> _zero();
    $self -> {_es}  = '+';
    $self -> {_e}   = $LIB -> _zero();

    # If rounding parameters are given as arguments, use them. If no rounding
    # parameters are given, and if called as a class method initialize the new
    # instance with the class variables.

    #return $self -> round(@r);  # this should work, but doesnt; fixme!

    if (@r) {
        if (@r >= 2 && defined($r[0]) && defined($r[1])) {
            carp "can't specify both accuracy and precision";
            return $self -> bnan();
        }
        $self->{accuracy} = $r[0];
        $self->{precision} = $r[1];
    } else {
        unless($selfref) {
            $self->{accuracy} = $class -> accuracy();
            $self->{precision} = $class -> precision();
        }
    }

    return $self;
}

sub bone {
    # Create or assign '+1' (or -1 if given sign '-').

    # Class::method(...) -> Class->method(...)
    unless (@_ && (defined(blessed($_[0])) && $_[0] -> isa(__PACKAGE__) ||
                   $_[0] =~ /^[a-z]\w*(?:::[a-z]\w*)*$/i))
    {
        #carp "Using ", (caller(0))[3], "() as a function is deprecated;",
        #  " use is as a method instead";
        unshift @_, __PACKAGE__;
    }

    my $self    = shift;
    my $selfref = ref $self;
    my $class   = $selfref || $self;

    # Make "require" work.

    $class -> import() if $IMPORT == 0;

    # Don't modify constant (read-only) objects.

    return $self if $selfref && $self -> modify('bone');

    my $dng = $class -> downgrade();
    if ($dng && $dng ne $class) {
        return $self -> _dng() -> bone(@_) if $selfref;
        return $dng -> bone(@_);
    }

    # Get the sign.

    my $sign = '+';     # default is to return +1
    if (defined($_[0]) && $_[0] =~ /^\s*([+-])\s*$/) {
        $sign = $1;
        shift;
    }

    # Get the rounding parameters, if any.

    my @r = @_;

    # If called as a class method, initialize a new object.

    $self = bless {}, $class unless $selfref;

    $self -> {sign} = $sign;
    $self -> {_m}   = $LIB -> _one();
    $self -> {_es}  = '+';
    $self -> {_e}   = $LIB -> _zero();

    # If rounding parameters are given as arguments, use them. If no rounding
    # parameters are given, and if called as a class method initialize the new
    # instance with the class variables.

    #return $self -> round(@r);  # this should work, but doesnt; fixme!

    if (@r) {
        if (@r >= 2 && defined($r[0]) && defined($r[1])) {
            carp "can't specify both accuracy and precision";
            return $self -> bnan();
        }
        $self->{accuracy} = $_[0];
        $self->{precision} = $_[1];
    } else {
        unless($selfref) {
            $self->{accuracy} = $class -> accuracy();
            $self->{precision} = $class -> precision();
        }
    }

    return $self;
}

sub binf {
    # create/assign a '+inf' or '-inf'

    # Class::method(...) -> Class->method(...)
    unless (@_ && (defined(blessed($_[0])) && $_[0] -> isa(__PACKAGE__) ||
                   $_[0] =~ /^[a-z]\w*(?:::[a-z]\w*)*$/i))
    {
        #carp "Using ", (caller(0))[3], "() as a function is deprecated;",
        #  " use is as a method instead";
        unshift @_, __PACKAGE__;
    }

    my $self    = shift;
    my $selfref = ref $self;
    my $class   = $selfref || $self;

    {
        no strict 'refs';
        if (${"${class}::_trap_inf"}) {
            croak("Tried to create +-inf in $class->binf()");
        }
    }

    # Make "require" work.

    $class -> import() if $IMPORT == 0;

    # Don't modify constant (read-only) objects.

    return $self if $selfref && $self -> modify('binf');

    # Get the sign.

    my $sign = '+';     # default is to return positive infinity
    if (defined($_[0]) && $_[0] =~ /^\s*([+-])(inf|$)/i) {
        $sign = $1;
        shift;
    }

    # Get the rounding parameters, if any.

    my @r = @_;

    # Downgrade?

    my $dng = $class -> downgrade();
    if ($dng && $dng ne $class) {
        return $self -> _dng() -> binf($sign, @r) if $selfref;
        return $dng -> binf($sign, @r);
    }

    # If called as a class method, initialize a new object.

    $self = bless {}, $class unless $selfref;

    $self -> {sign} = $sign . 'inf';
    $self -> {_m}   = $LIB -> _zero();
    $self -> {_es}  = '+';
    $self -> {_e}   = $LIB -> _zero();

    # If rounding parameters are given as arguments, use them. If no rounding
    # parameters are given, and if called as a class method initialize the new
    # instance with the class variables.

    #return $self -> round(@r);  # this should work, but doesnt; fixme!

    if (@r) {
        if (@r >= 2 && defined($r[0]) && defined($r[1])) {
            carp "can't specify both accuracy and precision";
            return $self -> bnan();
        }
        $self->{accuracy} = $r[0];
        $self->{precision} = $r[1];
    } else {
        unless($selfref) {
            $self->{accuracy} = $class -> accuracy();
            $self->{precision} = $class -> precision();
        }
    }

    return $self;
}

sub bnan {
    # create/assign a 'NaN'

    # Class::method(...) -> Class->method(...)
    unless (@_ && (defined(blessed($_[0])) && $_[0] -> isa(__PACKAGE__) ||
                   $_[0] =~ /^[a-z]\w*(?:::[a-z]\w*)*$/i))
    {
        #carp "Using ", (caller(0))[3], "() as a function is deprecated;",
        #  " use is as a method instead";
        unshift @_, __PACKAGE__;
    }

    my $self    = shift;
    my $selfref = ref $self;
    my $class   = $selfref || $self;

    {
        no strict 'refs';
        if (${"${class}::_trap_nan"}) {
            croak("Tried to create NaN in $class->bnan()");
        }
    }

    # Make "require" work.

    $class -> import() if $IMPORT == 0;

    # Don't modify constant (read-only) objects.

    return $self if $selfref && $self -> modify('bnan');

    my $dng = $class -> downgrade();
    if ($dng && $dng ne $class) {
        return $self -> _dng() -> bnan(@_) if $selfref;
        return $dng -> bnan(@_);
    }

    # Get the rounding parameters, if any.

    my @r = @_;

    # If called as a class method, initialize a new object.

    $self = bless {}, $class unless $selfref;

    $self -> {sign} = $nan;
    $self -> {_m}   = $LIB -> _zero();
    $self -> {_es}  = '+';
    $self -> {_e}   = $LIB -> _zero();

    # If rounding parameters are given as arguments, use them. If no rounding
    # parameters are given, and if called as a class method initialize the new
    # instance with the class variables.

    #return $self -> round(@r);  # this should work, but doesnt; fixme!

    if (@r) {
        if (@r >= 2 && defined($r[0]) && defined($r[1])) {
            carp "can't specify both accuracy and precision";
            return $self -> bnan();
        }
        $self->{accuracy} = $r[0];
        $self->{precision} = $r[1];
    } else {
        unless($selfref) {
            $self->{accuracy} = $class -> accuracy();
            $self->{precision} = $class -> precision();
        }
    }

    return $self;
}

sub bpi {

    # Class::method(...) -> Class->method(...)
    unless (@_ && (defined(blessed($_[0])) && $_[0] -> isa(__PACKAGE__) ||
                   $_[0] =~ /^[a-z]\w*(?:::[a-z]\w*)*$/i))
    {
        #carp "Using ", (caller(0))[3], "() as a function is deprecated;",
        #  " use is as a method instead";
        unshift @_, __PACKAGE__;
    }

    # Called as                 Argument list
    # ---------                 -------------
    # Math::BigFloat->bpi()     ("Math::BigFloat")
    # Math::BigFloat->bpi(10)   ("Math::BigFloat", 10)
    # $x->bpi()                 ($x)
    # $x->bpi(10)               ($x, 10)
    # Math::BigFloat::bpi()     ()
    # Math::BigFloat::bpi(10)   (10)
    #
    # In ambiguous cases, we favour the OO-style, so the following case
    #
    #   $n = Math::BigFloat->new("10");
    #   $x = Math::BigFloat->bpi($n);
    #
    # which gives an argument list with the single element $n, is resolved as
    #
    #   $n->bpi();

    my $self    = shift;
    my $selfref = ref $self;
    my $class   = $selfref || $self;
    my @r       = @_;                   # rounding paramters

    # Make "require" work.

    $class -> import() if $IMPORT == 0;

    if ($selfref) {                     # bpi() called as an instance method
        return $self if $self -> modify('bpi');
    } else {                            # bpi() called as a class method
        $self = bless {}, $class;       # initialize new instance
    }

    ($self, @r) = $self -> _find_round_parameters(@r);

    # The accuracy, i.e., the number of digits. Pi has one digit before the
    # dot, so a precision of 4 digits is equivalent to an accuracy of 5 digits.

    my $n = defined $r[0] ? $r[0]
          : defined $r[1] ? 1 - $r[1]
          : $self -> div_scale();

    my $rmode = defined $r[2] ? $r[2] : $self -> round_mode();

    my $pi;

    if ($n <= 1000) {

        # 75 x 14 = 1050 digits

        my $all_digits = <<EOF;
314159265358979323846264338327950288419716939937510582097494459230781640628
620899862803482534211706798214808651328230664709384460955058223172535940812
848111745028410270193852110555964462294895493038196442881097566593344612847
564823378678316527120190914564856692346034861045432664821339360726024914127
372458700660631558817488152092096282925409171536436789259036001133053054882
046652138414695194151160943305727036575959195309218611738193261179310511854
807446237996274956735188575272489122793818301194912983367336244065664308602
139494639522473719070217986094370277053921717629317675238467481846766940513
200056812714526356082778577134275778960917363717872146844090122495343014654
958537105079227968925892354201995611212902196086403441815981362977477130996
051870721134999999837297804995105973173281609631859502445945534690830264252
230825334468503526193118817101000313783875288658753320838142061717766914730
359825349042875546873115956286388235378759375195778185778053217122680661300
192787661119590921642019893809525720106548586327886593615338182796823030195
EOF

        # Should we round up?

        my $round_up;

        # From the string above, we need to extract the number of digits we
        # want plus extra characters for the newlines.

        my $nchrs = $n + int($n / 75);

        # Extract the digits we want.

        my $digits = substr($all_digits, 0, $nchrs);

        # Find out whether we should round up or down. Rounding is easy, since
        # pi is trancendental. With directed rounding, it doesn't matter what
        # the following digits are. With rounding to nearest, we only have to
        # look at one extra digit.

        if ($rmode eq 'trunc') {
            $round_up = 0;
        } else {
            my $next_digit = substr($all_digits, $nchrs, 1);
            $round_up = $next_digit lt '5' ? 0 : 1;
        }

        # Remove the newlines.

        $digits =~ tr/0-9//cd;

        # Now do the rounding. We could easily make the regex substitution
        # handle all cases, but we avoid using the regex engine when it is
        # simple to avoid it.

        if ($round_up) {
            my $last_digit = substr($digits, -1, 1);
            if ($last_digit lt '9') {
                substr($digits, -1, 1) = ++$last_digit;
            } else {
                $digits =~ s{([0-8])(9+)$}
                            { ($1 + 1) . ("0" x CORE::length($2)) }e;
            }
        }

        # Convert to an object.

        $pi = bless {
                     sign => '+',
                     _m   => $LIB -> _new($digits),
                     _es  => CORE::length($digits) > 1 ? '-' : '+',
                     _e   => $LIB -> _new($n - 1),
                    }, $class;

    } else {

        # For large accuracy, the arctan formulas become very inefficient with
        # Math::BigFloat, so use Brent-Salamin (aka AGM or Gauss-Legendre).

        # Use a few more digits in the intermediate computations.
        $n += 8;

        $HALF = $class -> new($HALF) unless ref($HALF);
        my ($an, $bn, $tn, $pn)
          = ($class -> bone, $HALF -> copy() -> bsqrt($n),
             $HALF -> copy() -> bmul($HALF), $class -> bone);
        while ($pn < $n) {
            my $prev_an = $an -> copy();
            $an -> badd($bn) -> bmul($HALF, $n);
            $bn -> bmul($prev_an) -> bsqrt($n);
            $prev_an -> bsub($an);
            $tn -> bsub($pn * $prev_an * $prev_an);
            $pn -> badd($pn);
        }
        $an -> badd($bn);
        $an -> bmul($an, $n) -> bdiv(4 * $tn, $n);

        $an -> round(@r);
        $pi = $an;
    }

    if (defined $r[0]) {
        $pi -> accuracy($r[0]);
    } elsif (defined $r[1]) {
        $pi -> precision($r[1]);
    }

    $pi -> _dng() if ($pi -> is_int() ||
                      $pi -> is_inf() ||
                      $pi -> is_nan());

    %$self = %$pi;
    bless $self, ref($pi);
    return $self;
}

sub copy {
    my ($x, $class);
    if (ref($_[0])) {           # $y = $x -> copy()
        $x = shift;
        $class = ref($x);
    } else {                    # $y = Math::BigInt -> copy($y)
        $class = shift;
        $x = shift;
    }

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @_;

    my $copy = bless {}, $class;

    $copy->{sign} = $x->{sign};
    $copy->{_es}  = $x->{_es};
    $copy->{_m}   = $LIB->_copy($x->{_m});
    $copy->{_e}   = $LIB->_copy($x->{_e});

    $copy->{accuracy}  = $x->{accuracy} if exists $x->{accuracy};
    $copy->{precision} = $x->{precision} if exists $x->{precision};

    return $copy;
}

sub as_int {
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    # Temporarily disable upgrading and downgrading.

    my $upg = Math::BigInt -> upgrade();
    my $dng = Math::BigInt -> downgrade();
    Math::BigInt -> upgrade(undef);
    Math::BigInt -> downgrade(undef);

    my $y;
    if ($x -> isa("Math::BigInt")) {
        $y = $x -> copy();
    } else {
        if ($x -> is_inf()) {
            $y = Math::BigInt -> binf($x -> sign());
        } elsif ($x -> is_nan()) {
            $y = Math::BigInt -> bnan();
        } else {
            $y = Math::BigInt -> new($x -> copy() -> bint() -> bdstr());
        }

        # Copy the remaining instance variables.

        ($y->{accuracy}, $y->{precision}) = ($x->{accuracy}, $x->{precision});
    }

    $y -> round(@r);

    # Restore upgrading and downgrading.

    Math::BigInt -> upgrade($upg);
    Math::BigInt -> downgrade($dng);

    return $y;
}

sub as_rat {
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    # Temporarily disable upgrading and downgrading.

    require Math::BigRat;
    my $upg = Math::BigRat -> upgrade();
    my $dng = Math::BigRat -> downgrade();
    Math::BigRat -> upgrade(undef);
    Math::BigRat -> downgrade(undef);

    my $y;
    if ($x -> isa("Math::BigRat")) {
        $y = $x -> copy();
    } else {

        if ($x -> is_inf()) {
            $y = Math::BigRat -> binf($x -> sign());
        } elsif ($x -> is_nan()) {
            $y = Math::BigRat -> bnan();
        } else {
            $y = Math::BigRat -> new($x -> bfstr());
        }

        # Copy the remaining instance variables.

        ($y->{accuracy}, $y->{precision}) = ($x->{accuracy}, $x->{precision});
    }

    $y -> round(@r);

    # Restore upgrading and downgrading.

    Math::BigRat -> upgrade($upg);
    Math::BigRat -> downgrade($dng);

    return $y;
}

sub as_float {
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    # Disable upgrading and downgrading.

    require Math::BigFloat;
    my $upg = Math::BigFloat -> upgrade();
    my $dng = Math::BigFloat -> downgrade();
    Math::BigFloat -> upgrade(undef);
    Math::BigFloat -> downgrade(undef);

    my $y;
    if ($x -> isa("Math::BigFloat")) {
        $y = $x -> copy();
    } else {
        if ($x -> is_inf()) {
            $y = Math::BigFloat -> binf($x -> sign());
        } elsif ($x -> is_nan()) {
            $y = Math::BigFloat -> bnan();
        } else {
            if ($x -> isa("Math::BigRat")) {
                if ($x -> is_int()) {
                    $y = Math::BigFloat -> new($x -> bdstr());
                } else {
                    my ($num, $den) = $x -> fparts();
                    my $str = $num -> as_float() -> bdiv($den, @r) -> bdstr();
                    $y = Math::BigFloat -> new($str);
                }
            } else {
                $y = Math::BigFloat -> new($x -> bdstr());
            }
        }

        # Copy the remaining instance variables.

        ($y->{accuracy}, $y->{precision}) = ($x->{accuracy}, $x->{precision});
    }

    $y -> round(@r);

    # Restore upgrading and downgrading.

    Math::BigFloat -> upgrade($upg);
    Math::BigFloat -> downgrade($dng);

    return $y;
}

###############################################################################
# Boolean methods
###############################################################################

sub is_zero {
    # return true if arg (BFLOAT or num_str) is zero
    my (undef, $x) = ref($_[0]) ? (undef, @_) : objectify(1, @_);

    return 0 if $x->{sign} ne '+';
    return 1 if $LIB->_is_zero($x->{_m});
    return 0;
}

sub is_one {
    # return true if arg (BFLOAT or num_str) is +1 or -1 if signis given
    my (undef, $x, $sign) = ref($_[0]) ? (undef, @_) : objectify(1, @_);

    if (defined($sign)) {
        croak 'is_one(): sign argument must be "+" or "-"'
          unless $sign eq '+' || $sign eq '-';
    } else {
        $sign = '+';
    }

    return 0 if $x->{sign} ne $sign;
    $LIB->_is_zero($x->{_e}) && $LIB->_is_one($x->{_m}) ? 1 : 0;
}

sub is_odd {
    # return true if arg (BFLOAT or num_str) is odd or false if even
    my (undef, $x) = ref($_[0]) ? (undef, @_) : objectify(1, @_);

    return 0 unless $x -> is_finite();
    $LIB->_is_zero($x->{_e}) && $LIB->_is_odd($x->{_m}) ? 1 : 0;
}

sub is_even {
    # return true if arg (BINT or num_str) is even or false if odd
    my (undef, $x) = ref($_[0]) ? (undef, @_) : objectify(1, @_);

    return 0 unless $x -> is_finite();
    ($x->{_es} eq '+') &&                       # 123.45 isn't
      ($LIB->_is_even($x->{_m})) ? 1 : 0;       # but 1200 is
}

sub is_int {
    # return true if arg (BFLOAT or num_str) is an integer
    my (undef, $x) = ref($_[0]) ? (undef, @_) : objectify(1, @_);

    return 0 unless $x -> is_finite();
    return $x->{_es} eq '+' ? 1 : 0;            # 1e-1 => no integer
}

###############################################################################
# Comparison methods
###############################################################################

sub bcmp {
    # Compares 2 values.  Returns one of undef, <0, =0, >0. (suitable for sort)

    # set up parameters
    my ($class, $x, $y, @r) = ref($_[0]) && ref($_[0]) eq ref($_[1])
                            ? (ref($_[0]), @_)
                            : objectify(2, @_);

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    # Handle all 'nan' cases.

    return    if $x -> is_nan() || $y -> is_nan();

    # Handle all '+inf' and '-inf' cases.

    return  0 if ($x -> is_inf("+") && $y -> is_inf("+") ||
                  $x -> is_inf("-") && $y -> is_inf("-"));
    return +1 if $x -> is_inf("+"); # x = +inf and y < +inf
    return -1 if $x -> is_inf("-"); # x = -inf and y > -inf
    return -1 if $y -> is_inf("+"); # x < +inf and y = +inf
    return +1 if $y -> is_inf("-"); # x > -inf and y = -inf

    # Handle all cases with opposite signs.

    return +1 if $x->{sign} eq '+' && $y->{sign} eq '-'; # also does 0 <=> -y
    return -1 if $x->{sign} eq '-' && $y->{sign} eq '+'; # also does -x <=> 0

    # Handle all remaining zero cases.

    my $xz = $x -> is_zero();
    my $yz = $y -> is_zero();
    return  0 if $xz && $yz;             # 0 <=> 0
    return -1 if $xz && $y->{sign} eq '+'; # 0 <=> +y
    return +1 if $yz && $x->{sign} eq '+'; # +x <=> 0

    # Both arguments are now finite, non-zero numbers with the same sign.

    my $cmp;

    # The next step is to compare the exponents, but since each mantissa is an
    # integer of arbitrary value, the exponents must be normalized by the
    # length of the mantissas before we can compare them.

    my $mxl = $LIB->_len($x->{_m});
    my $myl = $LIB->_len($y->{_m});

    # If the mantissas have the same length, there is no point in normalizing
    # the exponents by the length of the mantissas, so treat that as a special
    # case.

    if ($mxl == $myl) {

        # First handle the two cases where the exponents have different signs.

        if ($x->{_es} eq '+' && $y->{_es} eq '-') {
            $cmp = +1;
        } elsif ($x->{_es} eq '-' && $y->{_es} eq '+') {
            $cmp = -1;
        }

        # Then handle the case where the exponents have the same sign.

        else {
            $cmp = $LIB->_acmp($x->{_e}, $y->{_e});
            $cmp = -$cmp if $x->{_es} eq '-';
        }

        # Adjust for the sign, which is the same for x and y, and bail out if
        # we're done.

        $cmp = -$cmp if $x->{sign} eq '-'; # 124 > 123, but -124 < -123
        return $cmp if $cmp;

    }

    # We must normalize each exponent by the length of the corresponding
    # mantissa. Life is a lot easier if we first make both exponents
    # non-negative. We do this by adding the same positive value to both
    # exponent. This is safe, because when comparing the exponents, only the
    # relative difference is important.

    my $ex;
    my $ey;

    if ($x->{_es} eq '+') {

        # If the exponent of x is >= 0 and the exponent of y is >= 0, there is
        # no need to do anything special.

        if ($y->{_es} eq '+') {
            $ex = $LIB->_copy($x->{_e});
            $ey = $LIB->_copy($y->{_e});
        }

        # If the exponent of x is >= 0 and the exponent of y is < 0, add the
        # absolute value of the exponent of y to both.

        else {
            $ex = $LIB->_copy($x->{_e});
            $ex = $LIB->_add($ex, $y->{_e}); # ex + |ey|
            $ey = $LIB->_zero();             # -ex + |ey| = 0
        }

    } else {

        # If the exponent of x is < 0 and the exponent of y is >= 0, add the
        # absolute value of the exponent of x to both.

        if ($y->{_es} eq '+') {
            $ex = $LIB->_zero(); # -ex + |ex| = 0
            $ey = $LIB->_copy($y->{_e});
            $ey = $LIB->_add($ey, $x->{_e}); # ey + |ex|
        }

        # If the exponent of x is < 0 and the exponent of y is < 0, add the
        # absolute values of both exponents to both exponents.

        else {
            $ex = $LIB->_copy($y->{_e}); # -ex + |ey| + |ex| = |ey|
            $ey = $LIB->_copy($x->{_e}); # -ey + |ex| + |ey| = |ex|
        }

    }

    # Now we can normalize the exponents by adding lengths of the mantissas.

    $ex = $LIB->_add($ex, $LIB->_new($mxl));
    $ey = $LIB->_add($ey, $LIB->_new($myl));

    # We're done if the exponents are different.

    $cmp = $LIB->_acmp($ex, $ey);
    $cmp = -$cmp if $x->{sign} eq '-'; # 124 > 123, but -124 < -123
    return $cmp if $cmp;

    # Compare the mantissas, but first normalize them by padding the shorter
    # mantissa with zeros (shift left) until it has the same length as the
    # longer mantissa.

    my $mx = $x->{_m};
    my $my = $y->{_m};

    if ($mxl > $myl) {
        $my = $LIB->_lsft($LIB->_copy($my), $LIB->_new($mxl - $myl), 10);
    } elsif ($mxl < $myl) {
        $mx = $LIB->_lsft($LIB->_copy($mx), $LIB->_new($myl - $mxl), 10);
    }

    $cmp = $LIB->_acmp($mx, $my);
    $cmp = -$cmp if $x->{sign} eq '-'; # 124 > 123, but -124 < -123
    return $cmp;

}

sub bacmp {
    # Compares 2 values, ignoring their signs.
    # Returns one of undef, <0, =0, >0. (suitable for sort)

    # set up parameters
    my ($class, $x, $y, @r) = ref($_[0]) && ref($_[0]) eq ref($_[1])
                            ? (ref($_[0]), @_)
                            : objectify(2, @_);

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    # handle +-inf and NaN
    if ($x->{sign} !~ /^[+-]$/ || $y->{sign} !~ /^[+-]$/) {
        return    if ($x -> is_nan() || $y -> is_nan());
        return  0 if ($x -> is_inf() && $y -> is_inf());
        return  1 if ($x -> is_inf() && !$y -> is_inf());
        return -1;
    }

    # shortcut
    my $xz = $x -> is_zero();
    my $yz = $y -> is_zero();
    return  0 if $xz && $yz;    # 0 <=> 0
    return -1 if $xz && !$yz;   # 0 <=> +y
    return  1 if $yz && !$xz;   # +x <=> 0

    # adjust so that exponents are equal
    my $lxm = $LIB->_len($x->{_m});
    my $lym = $LIB->_len($y->{_m});
    my ($xes, $yes) = (1, 1);
    $xes = -1 if $x->{_es} ne '+';
    $yes = -1 if $y->{_es} ne '+';
    # the numify somewhat limits our length, but makes it much faster
    my $lx = $lxm + $xes * $LIB->_num($x->{_e});
    my $ly = $lym + $yes * $LIB->_num($y->{_e});
    my $l = $lx - $ly;
    return $l <=> 0 if $l != 0;

    # lengths (corrected by exponent) are equal
    # so make mantissa equal-length by padding with zero (shift left)
    my $diff = $lxm - $lym;
    my $xm = $x->{_m};          # not yet copy it
    my $ym = $y->{_m};
    if ($diff > 0) {
        $ym = $LIB->_copy($y->{_m});
        $ym = $LIB->_lsft($ym, $LIB->_new($diff), 10);
    } elsif ($diff < 0) {
        $xm = $LIB->_copy($x->{_m});
        $xm = $LIB->_lsft($xm, $LIB->_new(-$diff), 10);
    }
    $LIB->_acmp($xm, $ym);
}

###############################################################################
# Arithmetic methods
###############################################################################

sub bneg {
    # (BINT or num_str) return BINT
    # negate number or make a negated number from string
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('bneg');

    # For +0 do not negate (to have always normalized +0).
    $x->{sign} =~ tr/+-/-+/
      unless $x->{sign} eq '+' && $LIB->_is_zero($x->{_m});

    $x -> round(@r);
    $x -> _dng() if ($x -> is_int() ||
                     $x -> is_inf() ||
                     $x -> is_nan());
    return $x;
}

sub bnorm {
    # bnorm() can't support rounding, because bround() and bfround() call
    # bnorm(), which would recurse indefinitely.

    # adjust m and e so that m is smallest possible
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    # inf and nan
    if ($x->{sign} !~ /^[+-]$/) {
        $x -> round(@r);
        $x -> _dng();
        return $x;
    }

    my $zeros = $LIB->_zeros($x->{_m}); # correct for trailing zeros
    if ($zeros != 0) {
        my $z = $LIB->_new($zeros);
        $x->{_m} = $LIB->_rsft($x->{_m}, $z, 10);
        if ($x->{_es} eq '-') {
            if ($LIB->_acmp($x->{_e}, $z) >= 0) {
                $x->{_e} = $LIB->_sub($x->{_e}, $z);
                $x->{_es} = '+' if $LIB->_is_zero($x->{_e});
            } else {
                $x->{_e} = $LIB->_sub($LIB->_copy($z), $x->{_e});
                $x->{_es} = '+';
            }
        } else {
            $x->{_e} = $LIB->_add($x->{_e}, $z);
        }
    } else {
        # $x can only be 0Ey if there are no trailing zeros ('0' has 0 trailing
        # zeros). So, for something like 0Ey, set y to 0, and -0 => +0
        if ($LIB->_is_zero($x->{_m})) {
            $x->{sign} = '+';
            $x->{_es}  = '+';
            $x->{_e}   = $LIB->_zero();
        }
    }

    # Inf and NaN was handled above, so no need to check for this.

    $x -> _dng() if $x -> is_int();
    return $x;
}

sub binc {
    # increment arg by one
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('binc');

    # Inf and NaN

    if ($x -> is_inf() || $x -> is_nan()) {
        $x -> round(@r);
        $x -> _dng();
        return $x
    }

    # Non-integer

    if ($x->{_es} eq '-') {
        return $x -> badd($class -> bone(), @r);
    }

    # If the exponent is non-zero, convert the internal representation, so
    # that, e.g., 12e+3 becomes 12000e+0 and we can easily increment the
    # mantissa.

    if (!$LIB->_is_zero($x->{_e})) {
        $x->{_m} = $LIB->_lsft($x->{_m}, $x->{_e}, 10); # 1e2 => 100
        $x->{_e} = $LIB->_zero();                       # normalize
        $x->{_es} = '+';
        # we know that the last digit of $x will be '1' or '9', depending on
        # the sign
    }

    # now $x->{_e} == 0
    if ($x->{sign} eq '+') {
        $x->{_m} = $LIB->_inc($x->{_m});
        return $x -> bnorm() -> bround(@r);
    } elsif ($x->{sign} eq '-') {
        $x->{_m} = $LIB->_dec($x->{_m});
        $x->{sign} = '+' if $LIB->_is_zero($x->{_m}); # -1 +1 => -0 => +0
        return $x -> bnorm() -> bround(@r);
    }

    $x -> _dng() if ($x -> is_int() ||
                     $x -> is_inf() ||
                     $x -> is_nan());
    return $x;
}

sub bdec {
    # decrement arg by one
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('bdec');

    # Inf and NaN

    if ($x -> is_inf() || $x -> is_nan()) {
        $x -> round(@r);
        $x -> _dng();
        return $x
    }

    # Non-integer

    if ($x->{_es} eq '-') {
        return $x -> badd($class -> bone('-'), @r);
    }

    # If the exponent is non-zero, convert the internal representation, so
    # that, e.g., 12e+3 becomes 12000e+0 and we can easily increment the
    # mantissa.

    if (!$LIB->_is_zero($x->{_e})) {
        $x->{_m} = $LIB->_lsft($x->{_m}, $x->{_e}, 10); # 1e2 => 100
        $x->{_e} = $LIB->_zero();                       # normalize
        $x->{_es} = '+';
    }

    # now $x->{_e} == 0
    my $zero = $x -> is_zero();
    if (($x->{sign} eq '-') || $zero) {           # x <= 0
        $x->{_m} = $LIB->_inc($x->{_m});
        $x->{sign} = '-' if $zero;                # 0 => 1 => -1
        $x->{sign} = '+' if $LIB->_is_zero($x->{_m}); # -1 +1 => -0 => +0
        $x -> bnorm();
    }
    elsif ($x->{sign} eq '+') {                   # x > 0
        $x->{_m} = $LIB->_dec($x->{_m});
        $x -> bnorm();
    }

    $x -> round(@r);
    $x -> _dng() if ($x -> is_int() ||
                     $x -> is_inf() ||
                     $x -> is_nan());
    return $x;
}

sub badd {
    # set up parameters
    my ($class, $x, $y, @r) = ref($_[0]) && ref($_[0]) eq ref($_[1])
                            ? (ref($_[0]), @_)
                            : objectify(2, @_);

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('badd');

    unless ($x -> is_finite() && $y -> is_finite()) {

        return $x -> bnan(@r) if $x -> is_nan() || $y -> is_nan();

        return $x -> is_inf("+") ? ($y -> is_inf("-") ? $x -> bnan(@r)
                                                      : $x -> binf("+", @r))
             : $x -> is_inf("-") ? ($y -> is_inf("+") ? $x -> bnan(@r)
                                                      : $x -> binf("-", @r))
             :                     ($y -> is_inf("+") ? $x -> binf("+", @r)
                                                      : $x -> binf("-", @r));
    }

    return $x -> _upg() -> badd($y, @r) if $class -> upgrade();

    $r[3] = $y;                 # no push!

    # for speed: no add for $x + 0
    if ($y -> is_zero()) {
        $x -> round(@r);
    }

    # for speed: no add for 0 + $y
    elsif ($x -> is_zero()) {
        # make copy, clobbering up x (modify in place!)
        $x->{_e} = $LIB->_copy($y->{_e});
        $x->{_es} = $y->{_es};
        $x->{_m} = $LIB->_copy($y->{_m});
        $x->{sign} = $y->{sign} || $nan;
        $x -> round(@r);
    }

    # both $x and $y are non-zero
    else {

        # take lower of the two e's and adapt m1 to it to match m2
        my $e = $y->{_e};
        $e = $LIB->_zero() if !defined $e; # if no BFLOAT?
        $e = $LIB->_copy($e);              # make copy (didn't do it yet)

        my $es;

        ($e, $es) = $LIB -> _ssub($e, $y->{_es} || '+', $x->{_e}, $x->{_es});

        my $add = $LIB->_copy($y->{_m});

        if ($es eq '-') {                       # < 0
            $x->{_m} = $LIB->_lsft($x->{_m}, $e, 10);
            ($x->{_e}, $x->{_es}) = $LIB -> _sadd($x->{_e}, $x->{_es}, $e, $es);
        } elsif (!$LIB->_is_zero($e)) {         # > 0
            $add = $LIB->_lsft($add, $e, 10);
        }

        # else: both e are the same, so just leave them

        if ($x->{sign} eq $y->{sign}) {
            $x->{_m} = $LIB->_add($x->{_m}, $add);
        } else {
            ($x->{_m}, $x->{sign}) =
              $LIB -> _sadd($x->{_m}, $x->{sign}, $add, $y->{sign});
        }

        # delete trailing zeros, then round
        $x -> bnorm() -> round(@r);
    }

    $x -> _dng() if ($x -> is_int() ||
                     $x -> is_inf() ||
                     $x -> is_nan());
    return $x;
}

sub bsub {
    # set up parameters
    my ($class, $x, $y, @r) = ref($_[0]) && ref($_[0]) eq ref($_[1])
                            ? (ref($_[0]), @_)
                            : objectify(2, @_);

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('bsub');

    $r[3] = $y;                 # no push!

    unless ($x -> is_finite() && $y -> is_finite()) {

        return $x -> bnan(@r) if $x -> is_nan() || $y -> is_nan();

        return $x -> is_inf("+") ? ($y -> is_inf("+") ? $x -> bnan(@r)
                                                      : $x -> binf("+", @r))
             : $x -> is_inf("-") ? ($y -> is_inf("-") ? $x -> bnan(@r)
                                                      : $x -> binf("-", @r))
             :                     ($y -> is_inf("+") ? $x -> binf("-", @r)
                                                      : $x -> binf("+", @r));
    }

    $x -> badd($y -> copy() -> bneg(), @r);
    return $x;
}

sub bmul {
    # multiply two numbers

    # set up parameters
    my ($class, $x, $y, @r) = ref($_[0]) && ref($_[0]) eq ref($_[1])
                            ? (ref($_[0]), @_)
                            : objectify(2, @_);

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('bmul');

    return $x -> bnan(@r) if $x -> is_nan() || $y -> is_nan();

    # inf handling
    if (($x->{sign} =~ /^[+-]inf$/) || ($y->{sign} =~ /^[+-]inf$/)) {
        return $x -> bnan(@r) if $x -> is_zero() || $y -> is_zero();
        # result will always be +-inf:
        # +inf * +/+inf => +inf, -inf * -/-inf => +inf
        # +inf * -/-inf => -inf, -inf * +/+inf => -inf
        return $x -> binf(@r) if ($x->{sign} =~ /^\+/ && $y->{sign} =~ /^\+/);
        return $x -> binf(@r) if ($x->{sign} =~ /^-/ && $y->{sign} =~ /^-/);
        return $x -> binf('-', @r);
    }

    return $x -> _upg() -> bmul($y, @r) if $class -> upgrade();

    # aEb * cEd = (a*c)E(b+d)
    $x->{_m} = $LIB->_mul($x->{_m}, $y->{_m});
    ($x->{_e}, $x->{_es})
      = $LIB -> _sadd($x->{_e}, $x->{_es}, $y->{_e}, $y->{_es});

    $r[3] = $y;                 # no push!

    # adjust sign:
    $x->{sign} = $x->{sign} ne $y->{sign} ? '-' : '+';
    $x -> bnorm -> round(@r);

    $x -> _dng() if ($x -> is_int() ||
                     $x -> is_inf() ||
                     $x -> is_nan());
    return $x;
}

*bdiv = \&bfdiv;
*bmod = \&bfmod;

sub bfdiv {
    # This does floored division (or floor division) where the quotient is
    # rounded towards minus infinity.
    #
    # ($q, $r) = $x -> btdiv($y) returns $q and $r so that $q is floor($x / $y)
    # and $q * $y + $r = $x.

    # Set up parameters.
    my ($class, $x, $y, @r) = ref($_[0]) && ref($_[0]) eq ref($_[1])
                            ? (ref($_[0]), @_)
                            : objectify(2, @_);

    ###########################################################################
    # Code for all classes that share the common interface.
    ###########################################################################

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('bfdiv');

    my $wantarray = wantarray;          # call only once

    # At least one argument is NaN. This is handled the same way as in
    # Math::BigInt -> bdiv(). See the comment in the code for Math::BigInt ->
    # bdiv() for further details.

    if ($x -> is_nan() || $y -> is_nan()) {
        return $wantarray ? ($x -> bnan(@r), $class -> bnan(@r))
                          : $x -> bnan(@r);
    }

    # Divide by zero and modulo zero. This is handled the same way as in
    # Math::BigInt -> bdiv(). See the comment in the code for Math::BigInt ->
    # bdiv() for further details.

    if ($y -> is_zero()) {
        my $rem;
        if ($wantarray) {
            $rem = $x -> copy() -> round(@r);
            $rem -> _dng() if $rem -> is_int();
        }
        if ($x -> is_zero()) {
            $x -> bnan(@r);
        } else {
            $x -> binf($x->{sign}, @r);
        }
        return $wantarray ? ($x, $rem) : $x;
    }

    # Numerator (dividend) is +/-inf. This is handled the same way as in
    # Math::BigInt -> bdiv(). See the comment in the code for Math::BigInt ->
    # bdiv() for further details.

    if ($x -> is_inf()) {
        my $rem;
        $rem = $class -> bnan(@r) if $wantarray;
        if ($y -> is_inf()) {
            $x -> bnan(@r);
        } else {
            my $sign = $x -> bcmp(0) == $y -> bcmp(0) ? '+' : '-';
            $x -> binf($sign, @r);
        }
        return $wantarray ? ($x, $rem) : $x;
    }

    # Denominator (divisor) is +/-inf. This is handled the same way as in
    # Math::BigInt -> bdiv(), with one exception: In scalar context,
    # Math::BigFloat does true division (although rounded), not floored
    # division (F-division), so a finite number divided by +/-inf is always
    # zero. See the comment in the code for Math::BigInt -> bdiv() for further
    # details.

    if ($y -> is_inf()) {
        my $rem;
        if ($wantarray) {
            if ($x -> is_zero() || $x -> bcmp(0) == $y -> bcmp(0)) {
                $rem = $x -> copy() -> round(@r);
                $rem -> _dng() if $rem -> is_int();
                $x -> bzero(@r);
            } else {
                $rem = $class -> binf($y -> {sign}, @r);
                $x -> bone('-', @r);
            }
        } else {
            $x -> bzero(@r);
        }
        return $wantarray ? ($x, $rem) : $x;
    }

    # At this point, both the numerator and denominator are finite, non-zero
    # numbers.

    # we need to limit the accuracy to protect against overflow
    my $fallback = 0;
    my (@params, $scale);
    ($x, @params) = $x->_find_round_parameters($r[0], $r[1], $r[2], $y);

    if ($x -> is_nan()) {       # error in _find_round_parameters?
        $x -> round(@r);
        return $wantarray ? ($x, $class -> bnan(@r)) : $x;
    }

    # no rounding at all, so must use fallback
    if (scalar @params == 0) {
        # simulate old behaviour
        $params[0] = $class -> div_scale(); # and round to it as accuracy
        $scale = $params[0]+4;            # at least four more for proper round
        $params[2] = $r[2];               # round mode by caller or undef
        $fallback = 1;                    # to clear a/p afterwards
    } else {
        # the 4 below is empirical, and there might be cases where it is not
        # enough...
        $scale = abs($params[0] || $params[1]) + 4; # take whatever is defined
    }

    # Temporarily disable downgrading

    my $dng = Math::BigFloat -> downgrade();
    Math::BigFloat -> downgrade(undef);

    my $rem;
    $rem = $class -> bzero() if $wantarray;

    $y = $class -> new($y) unless $y -> isa('Math::BigFloat');

    my $lx = $LIB -> _len($x->{_m});
    my $ly = $LIB -> _len($y->{_m});
    $scale = $lx if $lx > $scale;
    $scale = $ly if $ly > $scale;
    my $diff = $ly - $lx;
    $scale += $diff if $diff > 0; # if lx << ly, but not if ly << lx!

    # Are both operands the same object, i.e., like $x -> bdiv($x)? If so,
    # flipping the sign of $y also flips the sign of $x.

    my $xsign = $x -> {sign};
    my $ysign = $y -> {sign};

    $y -> {sign} =~ tr/+-/-+/;            # Flip the sign of $y, and see ...
    my $same = $xsign ne $x -> {sign};    # ... if that changed the sign of $x.
    $y -> {sign} = $ysign;                # Re-insert the original sign.

    if ($same) {                          # $x -> bdiv($x)
        $x -> bone();
    } else {
        # make copy of $x in case of list context for later remainder
        # calculation
        $rem = $x -> copy() if $wantarray;

        $x->{sign} = $x->{sign} ne $y->{sign} ? '-' : '+';

        # promote Math::BigInt and its subclasses (except when already a
        # Math::BigFloat)
        $y = $class -> new($y) unless $y -> isa('Math::BigFloat');

        # calculate the result to $scale digits and then round it
        # (a * 10 ** b) / (c * 10 ** d) => (a/c) * 10 ** (b-d)
        $x->{_m} = $LIB->_lsft($x->{_m}, $LIB->_new($scale), 10);   # scale up
        $x->{_m} = $LIB->_div($x->{_m}, $y->{_m});                  # divide

        # correct exponent of $x
        ($x->{_e}, $x->{_es})
          = $LIB -> _ssub($x->{_e}, $x->{_es}, $y->{_e}, $y->{_es});

        # correct for 10**scale
        ($x->{_e}, $x->{_es})
          = $LIB -> _ssub($x->{_e}, $x->{_es}, $LIB->_new($scale), '+');

        $x -> bnorm();          # remove trailing zeros
    }

    # shortcut to not run through _find_round_parameters again
    if (defined $params[0]) {
        $x->{accuracy} = undef;               # clear before round
        $x -> bround($params[0], $params[2]); # then round accordingly
    } else {
        $x->{precision} = undef;               # clear before round
        $x -> bfround($params[1], $params[2]); # then round accordingly
    }
    if ($fallback) {
        # clear a/p after round, since user did not request it
        $x->{accuracy} = undef;
        $x->{precision} = undef;
    }

    # Restore downgrading

    Math::BigFloat -> downgrade($dng);

    if ($wantarray) {
        $x -> bfloor();
        $rem -> bfmod($y, @params);      # copy already done
        if ($fallback) {
            # clear a/p after round, since user did not request it
            $rem->{accuracy} = undef;
            $rem->{precision} = undef;
        }
        $x -> _dng()   if $x -> is_int();
        $rem -> _dng() if $rem -> is_int();
        return $x, $rem;
    }

    $x -> _dng() if $x -> is_int();
    $x;         # rounding already done above
}

sub bfmod {
    # (dividend: BFLOAT or num_str, divisor: BFLOAT or num_str) return
    # remainder

    # set up parameters
    my ($class, $x, $y, @r) = ref($_[0]) && ref($_[0]) eq ref($_[1])
                            ? (ref($_[0]), @_)
                            : objectify(2, @_);

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('bfmod');

    # At least one argument is NaN. This is handled the same way as in
    # Math::BigInt -> bfmod().

    return $x -> bnan(@r) if $x -> is_nan() || $y -> is_nan();

    # Modulo zero. This is handled the same way as in Math::BigInt -> bfmod().

    if ($y -> is_zero()) {
        return $x -> round(@r);
    }

    # Numerator (dividend) is +/-inf. This is handled the same way as in
    # Math::BigInt -> bfmod().

    if ($x -> is_inf()) {
        return $x -> bnan(@r);
    }

    # Denominator (divisor) is +/-inf. This is handled the same way as in
    # Math::BigInt -> bfmod().

    if ($y -> is_inf()) {
        if ($x -> is_zero() || $x -> bcmp(0) == $y -> bcmp(0)) {
            return $x -> round(@r);
        } else {
            return $x -> binf($y -> sign(), @r);
        }
    }

    # Modulo is zero if $x is zero or if $x is an integer and $y is +/-1.

    return $x -> bzero(@r) if $x -> is_zero()
      || ($x -> is_int() &&
          # check that $y == +1 or $y == -1:
          ($LIB->_is_zero($y->{_e}) && $LIB->_is_one($y->{_m})));

    # Numerator (dividend) and denominator (divisor) are identical. Return
    # zero.

    my $cmp = $x -> bacmp($y);          # $x <=> $y
    if ($cmp == 0) {                    # $x == $y => result 0
        return $x -> bzero(@r);
    }

    # Compare the exponents of $x and $y.

    my $ecmp = $LIB->_scmp($x->{_e}, $x->{_es}, $y->{_e}, $y->{_es});

    my $ym = $y->{_m};          # mantissa of y, scaled if necessary

    if ($ecmp > 0) {

        # $x has a larger exponent than $y, so shift the mantissa of $x by the
        # difference between the exponents of $x and $y.
        #
        # 123e+2 % 456e+1 =>    1230 % 456 (+2 - +1 = 1)
        # 123e+2 % 456e-1 =>  123000 % 456 (+2 - -1 = 3)
        # 456e-1 % 123e-3 =>   12300 % 456 (-1 - -3 = 2)

        # get the difference between exponents; $ds is always "+" here
        my ($de, $ds) = $LIB->_ssub($LIB->_copy($x->{_e}), $x->{_es},
                                    $y->{_e}, $y->{_es});

        # adjust the mantissa of x by the difference between exponents
        $x->{_m} = $LIB->_lsft($x->{_m}, $de, 10);

        # compute the modulus
        $x->{_m} = $LIB->_mod($x->{_m}, $ym);

        # adjust the exponent of x to correct for the ajustment of the mantissa
        ($x->{_e}, $x->{_es}) = $LIB->_ssub($x->{_e}, $x->{_es}, $de, $ds);

    } elsif ($ecmp < 0) {

        # $x has a smaller exponent than $y, so shift the mantissa of $y by the
        # difference between the exponents of $x and $y.
        #
        # 123456e+1 % 78e+2 =>  123456 % 780   (+2 - +1 = 1)
        # 123456e-2 % 78e+1 =>  123456 % 78000 (+1 - -2 = 3)

        # get the difference between exponents; $ds is always "+" here
        my ($de, $ds) = $LIB->_ssub($LIB->_copy($y->{_e}), $y->{_es},
                                    $x->{_e}, $x->{_es});

        # adjust the mantissa of y by the difference between exponents
        $ym = $LIB->_lsft($LIB->_copy($ym), $de, 10);

        # compute the modulus
        $x->{_m} = $LIB->_mod($x->{_m}, $ym);

    } else {

        # $x has the same exponent as $y, so compute the modulus directly

        # compute the modulus
        $x->{_m} = $LIB->_mod($x->{_m}, $ym);
    }

    if ($LIB->_is_zero($x->{_m})) {
        $x->{sign} = '+';
    } else {
        # adjust for floored division/modulus
        $x->{_m} = $LIB->_sub($ym, $x->{_m}, 1)
          if $x->{sign} ne $y->{sign};
        $x->{sign} = $y->{sign};
    }

    $x -> bnorm();
    $x -> round($r[0], $r[1], $r[2], $y);
    $x -> _dng() if $x -> is_int();
    return $x;
}

sub btdiv {
    # This does truncated division, where the quotient is truncted, i.e.,
    # rounded towards zero.
    #
    # ($q, $r) = $x -> btdiv($y) returns $q and $r so that $q is int($x / $y)
    # and $q * $y + $r = $x.

    # Set up parameters
    my ($class, $x, $y, @r) = ref($_[0]) && ref($_[0]) eq ref($_[1])
                            ? (ref($_[0]), @_)
                            : objectify(2, @_);

    ###########################################################################
    # Code for all classes that share the common interface.
    ###########################################################################

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('btdiv');

    my $wantarray = wantarray;          # call only once

    # At least one argument is NaN. Return NaN for both quotient and the
    # modulo/remainder.

    if ($x -> is_nan() || $y -> is_nan()) {
        return $wantarray ? ($x -> bnan(@r), $class -> bnan(@r))
                          : $x -> bnan(@r);
    }

    # Divide by zero and modulo zero.
    #
    # Division: Use the common convention that x / 0 is inf with the same sign
    # as x, except when x = 0, where we return NaN. This is also what earlier
    # versions did.
    #
    # Modulo: In modular arithmetic, the congruence relation z = x (mod y)
    # means that there is some integer k such that z - x = k y. If y = 0, we
    # get z - x = 0 or z = x. This is also what earlier versions did, except
    # that 0 % 0 returned NaN.
    #
    #     inf / 0 =  inf                     inf % 0 =  inf
    #       5 / 0 =  inf                       5 % 0 =    5
    #       0 / 0 =  NaN                       0 % 0 =    0
    #      -5 / 0 = -inf                      -5 % 0 =   -5
    #    -inf / 0 = -inf                    -inf % 0 = -inf

    if ($y -> is_zero()) {
        my $rem;
        if ($wantarray) {
            $rem = $x -> copy(@r);
        }
        if ($x -> is_zero()) {
            $x -> bnan(@r);
        } else {
            $x -> binf($x -> {sign}, @r);
        }
        return $wantarray ? ($x, $rem) : $x;
    }

    # Numerator (dividend) is +/-inf, and denominator is finite and non-zero.
    # The divide by zero cases are covered above. In all of the cases listed
    # below we return the same as core Perl.
    #
    #     inf / -inf =  NaN                  inf % -inf =  NaN
    #     inf /   -5 = -inf                  inf %   -5 =  NaN
    #     inf /    5 =  inf                  inf %    5 =  NaN
    #     inf /  inf =  NaN                  inf %  inf =  NaN
    #
    #    -inf / -inf =  NaN                 -inf % -inf =  NaN
    #    -inf /   -5 =  inf                 -inf %   -5 =  NaN
    #    -inf /    5 = -inf                 -inf %    5 =  NaN
    #    -inf /  inf =  NaN                 -inf %  inf =  NaN

    if ($x -> is_inf()) {
        my $rem;
        $rem = $class -> bnan(@r) if $wantarray;
        if ($y -> is_inf()) {
            $x -> bnan(@r);
        } else {
            my $sign = $x -> bcmp(0) == $y -> bcmp(0) ? '+' : '-';
            $x -> binf($sign,@r );
        }
        return $wantarray ? ($x, $rem) : $x;
    }

    # Denominator (divisor) is +/-inf. The cases when the numerator is +/-inf
    # are covered above. In the modulo cases (in the right column) we return
    # the same as core Perl, which does floored division, so for consistency we
    # also do floored division in the division cases (in the left column).
    #
    #      -5 /  inf =    0                   -5 %  inf =  -5
    #       0 /  inf =    0                    0 %  inf =   0
    #       5 /  inf =    0                    5 %  inf =   5
    #
    #      -5 / -inf =    0                   -5 % -inf =  -5
    #       0 / -inf =    0                    0 % -inf =   0
    #       5 / -inf =    0                    5 % -inf =   5

    if ($y -> is_inf()) {
        my $rem;
        if ($wantarray) {
            $rem = $x -> copy() -> round(@r);
            $rem -> _dng() if $rem -> is_int();
        }
        $x -> bzero(@r);
        return $wantarray ? ($x, $rem) : $x;
    }

    # At this point, both the numerator and denominator are finite, non-zero
    # numbers.

    # we need to limit the accuracy to protect against overflow
    my $fallback = 0;
    my (@params, $scale);
    ($x, @params) = $x->_find_round_parameters($r[0], $r[1], $r[2], $y);

    if ($x -> is_nan()) {       # error in _find_round_parameters?
        $x -> round(@r);
        return $wantarray ? ($x, $class -> bnan(@r)) : $x;
    }

    # no rounding at all, so must use fallback
    if (scalar @params == 0) {
        # simulate old behaviour
        $params[0] = $class -> div_scale(); # and round to it as accuracy
        $scale = $params[0]+4;            # at least four more for proper round
        $params[2] = $r[2];               # round mode by caller or undef
        $fallback = 1;                    # to clear a/p afterwards
    } else {
        # the 4 below is empirical, and there might be cases where it is not
        # enough...
        $scale = abs($params[0] || $params[1]) + 4; # take whatever is defined
    }

    # Temporarily disable downgrading

    my $dng = Math::BigFloat -> downgrade();
    Math::BigFloat -> downgrade(undef);

    my $rem;
    $rem = $class -> bzero() if $wantarray;

    $y = $class -> new($y) unless $y -> isa('Math::BigFloat');

    my $lx = $LIB -> _len($x->{_m});
    my $ly = $LIB -> _len($y->{_m});
    $scale = $lx if $lx > $scale;
    $scale = $ly if $ly > $scale;
    my $diff = $ly - $lx;
    $scale += $diff if $diff > 0; # if lx << ly, but not if ly << lx!

    # Are both operands the same object, i.e., like $x -> bdiv($x)? If so,
    # flipping the sign of $y also flips the sign of $x.

    my $xsign = $x -> {sign};
    my $ysign = $y -> {sign};

    $y -> {sign} =~ tr/+-/-+/;            # Flip the sign of $y, and see ...
    my $same = $xsign ne $x -> {sign};    # ... if that changed the sign of $x.
    $y -> {sign} = $ysign;                # Re-insert the original sign.

    if ($same) {                          # $x -> bdiv($x)
        $x -> bone();
    } else {
        # make copy of $x in case of list context for later remainder
        # calculation
        $rem = $x -> copy() if $wantarray;

        $x->{sign} = $x->{sign} ne $y->{sign} ? '-' : '+';

        # promote Math::BigInt and its subclasses (except when already a
        # Math::BigFloat)
        $y = $class -> new($y) unless $y -> isa('Math::BigFloat');

        # calculate the result to $scale digits and then round it
        # (a * 10 ** b) / (c * 10 ** d) => (a/c) * 10 ** (b-d)
        $x->{_m} = $LIB->_lsft($x->{_m}, $LIB->_new($scale), 10);   # scale up
        $x->{_m} = $LIB->_div($x->{_m}, $y->{_m});                  # divide

        # correct exponent of $x
        ($x->{_e}, $x->{_es})
          = $LIB -> _ssub($x->{_e}, $x->{_es}, $y->{_e}, $y->{_es});

        # correct for 10**scale
        ($x->{_e}, $x->{_es})
          = $LIB -> _ssub($x->{_e}, $x->{_es}, $LIB->_new($scale), '+');

        $x -> bnorm();          # remove trailing zeros in mantissa
    }

    # shortcut to not run through _find_round_parameters again
    if (defined $params[0]) {
        $x->{accuracy} = undef;               # clear before round
        $x -> bround($params[0], $params[2]); # then round accordingly
    } else {
        $x->{precision} = undef;               # clear before round
        $x -> bfround($params[1], $params[2]); # then round accordingly
    }
    if ($fallback) {
        # clear a/p after round, since user did not request it
        $x->{accuracy} = undef;
        $x->{precision} = undef;
    }

    # Restore downgrading

    Math::BigFloat -> downgrade($dng);

    if ($wantarray) {
        $x -> bint();
        $rem -> btmod($y, @params);      # copy already done

        if ($fallback) {
            # clear a/p after round, since user did not request it
            $rem->{accuracy} = undef;
            $rem->{precision} = undef;
        }
        $x -> _dng()   if $x -> is_int();
        $rem -> _dng() if $rem -> is_int();
        return $x, $rem;
    }

    $x -> _dng() if $x -> is_int();
    $x;         # rounding already done above
}

sub btmod {
    # (dividend: BFLOAT or num_str, divisor: BFLOAT or num_str) return
    # remainder

    # set up parameters
    my ($class, $x, $y, @r) = ref($_[0]) && ref($_[0]) eq ref($_[1])
                            ? (ref($_[0]), @_)
                            : objectify(2, @_);

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('btmod');

    # At least one argument is NaN. This is handled the same way as in
    # Math::BigInt -> btmod().

    return $x -> bnan(@r) if $x -> is_nan() || $y -> is_nan();

    # Modulo zero. This is handled the same way as in Math::BigInt -> btmod().

    if ($y -> is_zero()) {
        return $x -> round(@r);
    }

    # Numerator (dividend) is +/-inf. This is handled the same way as in
    # Math::BigInt -> btmod().

    if ($x -> is_inf()) {
        return $x -> bnan(@r);
    }

    # Denominator (divisor) is +/-inf. This is handled the same way as in
    # Math::BigInt -> btmod().

    if ($y -> is_inf()) {
        return $x -> round(@r);
    }

    # Modulo is zero if $x is zero or if $x is an integer and $y is +/-1.

    return $x -> bzero(@r) if $x -> is_zero()
      || ($x -> is_int() &&
          # check that $y == +1 or $y == -1:
          ($LIB->_is_zero($y->{_e}) && $LIB->_is_one($y->{_m})));

    # Numerator (dividend) and denominator (divisor) are identical. Return
    # zero.

    my $cmp = $x -> bacmp($y);      # $x <=> $y
    if ($cmp == 0) {                # $x == $y => result 0
        return $x -> bzero(@r);
    }

    # Compare the exponents of $x and $y.

    my $ecmp = $LIB->_scmp($x->{_e}, $x->{_es}, $y->{_e}, $y->{_es});

    if ($ecmp > 0) {

        # $x has a larger exponent than $y, so shift the mantissa of $x by the
        # difference between the exponents of $x and $y.
        #
        # 123e+2 % 456e+1 =>    1230 % 456 (+2 - +1 = 1)
        # 123e+2 % 456e-1 =>  123000 % 456 (+2 - -1 = 3)
        # 456e-1 % 123e-3 =>   12300 % 456 (-1 - -3 = 2)

        # get the difference between exponents; $ds is always "+" here
        my ($de, $ds) = $LIB->_ssub($LIB->_copy($x->{_e}), $x->{_es},
                                    $y->{_e}, $y->{_es});

        # adjust the mantissa of x by the difference between exponents
        $x->{_m} = $LIB->_lsft($x->{_m}, $de, 10);

        # compute the modulus
        $x->{_m} = $LIB->_mod($x->{_m}, $y->{_m});

        # adjust the exponent of x to correct for the ajustment of the mantissa
        ($x->{_e}, $x->{_es}) = $LIB->_ssub($x->{_e}, $x->{_es}, $de, $ds);

    } elsif ($ecmp < 0) {

        # $x has a smaller exponent than $y, so shift the mantissa of $y by the
        # difference between the exponents of $x and $y.
        #
        # 123456e+1 % 78e+2 =>  123456 % 780   (+2 - +1 = 1)
        # 123456e-2 % 78e+1 =>  123456 % 78000 (+1 - -2 = 3)

        # get the difference between exponents; $ds is always "+" here
        my ($de, $ds) = $LIB->_ssub($LIB->_copy($y->{_e}), $y->{_es},
                                    $x->{_e}, $x->{_es});

        # adjust the mantissa of y by the difference between exponents
        my $ym = $LIB->_lsft($LIB->_copy($y->{_m}), $de, 10);

        # compute the modulus
        $x->{_m} = $LIB->_mod($x->{_m}, $ym);

    } else {

        # $x has the same exponent as $y, so compute the modulus directly

        # compute the modulus
        $x->{_m} = $LIB->_mod($x->{_m}, $y->{_m});
    }

    $x->{sign} = '+' if $LIB->_is_zero($x->{_m});       # fix sign for -0

    $x -> bnorm();
    $x -> round($r[0], $r[1], $r[2], $y);
    $x -> _dng() if $x -> is_int();
    return $x;
}

sub binv {
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('binv');

    # bone() might perform downgrading, so temporarily disable downgrading

    my $dng = Math::BigFloat -> downgrade();
    Math::BigFloat -> downgrade(undef);

    my $inv = $class -> bone() -> bdiv($x, @r);

    # Restore downgrading

    Math::BigFloat -> downgrade($dng);

    %$x = %$inv;

    $x -> round(@r);
    $x -> _dng() if ($x -> is_int() ||
                     $x -> is_inf() ||
                     $x -> is_nan());
    return $x;
}

sub bsqrt {
    # calculate square root
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('bsqrt');

    # Handle trivial cases.

    return $x -> bnan(@r)      if $x -> is_nan();
    return $x -> binf("+", @r) if $x -> is_inf("+");
    return $x -> round(@r)     if $x -> is_zero() || $x -> is_one();

    # We don't support complex numbers.

    if ($x -> is_neg()) {
        return $x -> _upg() -> bsqrt(@r) if $class -> upgrade();
        return $x -> bnan(@r);
    }

    # we need to limit the accuracy to protect against overflow
    my $fallback = 0;
    my (@params, $scale);
    ($x, @params) = $x->_find_round_parameters(@r);

    # error in _find_round_parameters?
    return $x -> bnan(@r) if $x -> is_nan();

    # no rounding at all, so must use fallback
    if (scalar @params == 0) {
        # simulate old behaviour
        $params[0] = $class -> div_scale(); # and round to it as accuracy
        $scale = $params[0]+4;            # at least four more for proper round
        $params[2] = $r[2];               # round mode by caller or undef
        $fallback = 1;                    # to clear a/p afterwards
    } else {
        # the 4 below is empirical, and there might be cases where it is not
        # enough...
        $scale = abs($params[0] || $params[1]) + 4; # take whatever is defined
    }

    # Shift the significand left or right to get the desired number of digits,
    # which is 2*$scale with possibly one extra digit to ensure that the
    # exponent is an even number.

    my $l = $LIB -> _len($x->{_m});
    my $n = 2 * $scale - $l;                    # how much should we shift?
    $n++ if ($l % 2 xor $LIB -> _is_odd($x->{_e}));
    my ($na, $ns) = $n < 0 ? (abs($n), "-") : ($n, "+");
    $na = $LIB -> _new($na);

    $x->{_m} = $ns eq "+" ? $LIB -> _lsft($x->{_m}, $na, 10)
                          : $LIB -> _rsft($x->{_m}, $na, 10);

    $x->{_m} = $LIB -> _sqrt($x->{_m});

    # Adjust the exponent by the amount that we shifted the significand. The
    # square root of the exponent is simply half of it: sqrt(10^(2*a)) = 10^a.

    ($x->{_e}, $x->{_es}) = $LIB -> _ssub($x->{_e}, $x->{_es}, $na, $ns);
    $x->{_e} = $LIB -> _div($x->{_e}, $LIB -> _new("2"));

    # Normalize to get rid of any trailing zeros in the significand.

    $x -> bnorm();

    # shortcut to not run through _find_round_parameters again
    if (defined $params[0]) {
        $x -> bround($params[0], $params[2]); # then round accordingly
    } else {
        $x -> bfround($params[1], $params[2]); # then round accordingly
    }

    if ($fallback) {
        # clear a/p after round, since user did not request it
        $x->{accuracy} = undef;
        $x->{precision} = undef;
    }

    $x -> round(@r);
    $x -> _dng() if $x -> is_int();
    $x;
}

sub bpow {
    # (BFLOAT or num_str, BFLOAT or num_str) return BFLOAT
    # compute power of two numbers, second arg is used as integer
    # modifies first argument

    # set up parameters
    my ($class, $x, $y, $a, $p, $r) = (ref($_[0]), @_);
    # objectify is costly, so avoid it
    if ((!ref($_[0])) || (ref($_[0]) ne ref($_[1]))) {
        ($class, $x, $y, $a, $p, $r) = objectify(2, @_);
    }

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('bpow');

    # $x and/or $y is a NaN
    return $x -> bnan() if $x -> is_nan() || $y -> is_nan();

    # $x and/or $y is a +/-Inf
    if ($x -> is_inf("-")) {
        return $x -> bzero()   if $y -> is_negative();
        return $x -> bnan()    if $y -> is_zero();
        return $x            if $y -> is_odd();
        return $x -> bneg();
    } elsif ($x -> is_inf("+")) {
        return $x -> bzero()   if $y -> is_negative();
        return $x -> bnan()    if $y -> is_zero();
        return $x;
    } elsif ($y -> is_inf("-")) {
        return $x -> bnan()    if $x -> is_one("-");
        return $x -> binf("+") if $x > -1 && $x < 1;
        return $x -> bone()    if $x -> is_one("+");
        return $x -> bzero();
    } elsif ($y -> is_inf("+")) {
        return $x -> bnan()    if $x -> is_one("-");
        return $x -> bzero()   if $x > -1 && $x < 1;
        return $x -> bone()    if $x -> is_one("+");
        return $x -> binf("+");
    }

    if ($x -> is_zero()) {
        return $x -> bone() if $y -> is_zero();
        return $x -> binf() if $y -> is_negative();
        return $x;
    }

    # We don't support complex numbers, so upgrade or return NaN.

    if ($x -> is_negative() && !$y -> is_int()) {
        return $x -> _upg() -> bpow($y, $a, $p, $r) if $class -> upgrade();
        return $x -> bnan();
    }

    if ($x -> is_one("+") || $y -> is_one()) {
        return $x;
    }

    if ($x -> is_one("-")) {
        return $x if $y -> is_odd();
        return $x -> bneg();
    }

    return $x -> _pow($y, $a, $p, $r) if !$y -> is_int();

    # We should NOT be looking at private variables of other objects. Fixme XXX
    my $y1 = $y -> as_int()->{value}; # make MBI part

    my $new_sign = '+';
    $new_sign = $LIB -> _is_odd($y1) ? '-' : '+' if $x->{sign} ne '+';

    # calculate $x->{_m} ** $y and $x->{_e} * $y separately (faster)
    $x->{_m} = $LIB -> _pow($x->{_m}, $y1);
    $x->{_e} = $LIB -> _mul($x->{_e}, $y1);

    $x->{sign} = $new_sign;
    $x -> bnorm();

    # x ** (-y) = 1 / (x ** y)

    if ($y->{sign} eq '-') {
        # modify $x in place!
        my $z = $x -> copy();
        $x -> bone();
        # round in one go (might ignore y's A!)
        return scalar $x -> bdiv($z, $a, $p, $r);
    }

    $x -> round($a, $p, $r, $y);

    $x -> _dng() if ($x -> is_int() ||
                     $x -> is_inf() ||
                     $x -> is_nan());
    return $x;
}

sub broot {
    # calculate $y'th root of $x

    # set up parameters
    my ($class, $x, $y, @r) = ref($_[0]) && ref($_[0]) eq ref($_[1])
                            ? (ref($_[0]), @_)
                            : objectify(2, @_);

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('broot');

    # Handle trivial cases.

    return $x -> bnan(@r) if $x -> is_nan() || $y -> is_nan();

    if ($x -> is_neg()) {
        # -27 ** (1/3) = -(27 ** (1/3)) = -3
        return $x -> broot($y -> copy() -> bneg(), @r) -> bneg()
          if ($x -> is_int() && $y -> is_int() &&
              $y -> is_neg() && $y -> is_odd());
        return $x -> _upg -> broot($y, @r) if $class -> upgrade();
        return $x -> bnan(@r);
    }

    # NaN handling: $x ** 1/0, x or y NaN, or y inf/-inf or y == 0
    return $x -> bnan(@r) if ($x->{sign} !~ /^\+/ || $y -> is_zero() ||
                              $y->{sign} !~ /^\+$/);

    # Trivial cases.
    return $x if ($x -> is_zero() || $x -> is_one() ||
                  $x -> is_inf()  || $y -> is_one());

    # we need to limit the accuracy to protect against overflow
    my $fallback = 0;
    my (@params, $scale);
    ($x, @params) = $x->_find_round_parameters(@r);

    return $x if $x -> is_nan();  # error in _find_round_parameters?

    # no rounding at all, so must use fallback
    if (scalar @params == 0) {
        # simulate old behaviour
        $params[0] = $class -> div_scale(); # and round to it as accuracy
        $scale = $params[0]+4;            # at least four more for proper round
        $params[2] = $r[2];               # round mode by caller or undef
        $fallback = 1;                    # to clear a/p afterwards
    } else {
        # the 4 below is empirical, and there might be cases where it is not
        # enough...
        $scale = abs($params[0] || $params[1]) + 4; # take whatever is defined
    }

    # When user set globals, they would interfere with our calculation, so
    # disable them and later re-enable them.

    my $ab = $class -> accuracy();
    my $pb = $class -> precision();
    $class -> accuracy(undef);
    $class -> precision(undef);

    # Disabling upgrading and downgrading is no longer necessary to avoid an
    # infinite recursion, but it avoids unnecessary upgrading and downgrading
    # in the intermediate computations.

    my $upg = $class -> upgrade();
    my $dng = $class -> downgrade();
    $class -> upgrade(undef);
    $class -> downgrade(undef);

    # We also need to disable any set A or P on $x (_find_round_parameters took
    # them already into account), since these would interfere, too.

    $x->{accuracy} = undef;
    $x->{precision} = undef;

    # remember sign and make $x positive, since -4 ** (1/2) => -2
    my $sign = 0;
    $sign = 1 if $x->{sign} eq '-';
    $x->{sign} = '+';

    my $is_two = 0;
    if ($y -> isa('Math::BigFloat')) {
        $is_two = $y->{sign} eq '+' && $LIB->_is_two($y->{_m})
                                    && $LIB->_is_zero($y->{_e});
    } else {
        $is_two = $y == 2;
    }

    # Normal square root if $y == 2

    if ($is_two) {
        $x -> bsqrt($scale + 4);
    }

    # Inverse: $x ** (-1) => 1 / $x

    elsif ($y -> is_one('-')) {
        $x -> binv($scale + 4);
    }

    # General case: calculate the broot() as integer result first, and if it
    # fits, return it rightaway (but only if $x and $y are integer).
    #
    # This code should be improved. XXX

    else {

        # Temporarily disable upgrading in Math::BigInt.

        my $mbi_upg = Math::BigInt -> upgrade();
        Math::BigInt -> upgrade(undef);

        my $done = 0;           # not yet
        if ($y -> is_int() && $x -> is_int()) {
            my $i = $LIB->_copy($x->{_m});
            $i = $LIB->_lsft($i, $x->{_e}, 10) unless $LIB->_is_zero($x->{_e});
            my $int = Math::BigInt -> bzero();
            $int->{value} = $i;
            $int -> broot($y -> as_int());
            # if ($exact)
            if ($int -> copy() -> bpow($y -> as_int()) == $x -> as_int()) {
                # found result, return it
                $x->{_m} = $int->{value};
                $x->{_e} = $LIB->_zero();
                $x->{_es} = '+';
                $x -> bnorm();
                $done = 1;
            }
        }

        if ($done == 0) {
            my $u = $class -> bone() -> bdiv($y, $scale+4);
            $u->{accuracy} = undef;
            $u->{precision} = undef;
            $x -> bpow($u, $scale+4);            # el cheapo
        }

        Math::BigInt -> upgrade($mbi_upg);
    }

    $x -> bneg() if $sign == 1;

    # shortcut to not run through _find_round_parameters again
    if (defined $params[0]) {
        $x -> bround($params[0], $params[2]); # then round accordingly
    } else {
        $x -> bfround($params[1], $params[2]); # then round accordingly
    }
    if ($fallback) {
        # clear a/p after round, since user did not request it
        $x->{accuracy} = undef;
        $x->{precision} = undef;
    }

    # Restore globals. We need to do it like this, because setting one
    # undefines the other.

    if (defined $ab) {
        $class -> accuracy($ab);
    } else {
        $class -> precision($pb);
    }

    $class -> upgrade($upg);
    $class -> downgrade($dng);

    $x -> round(@r);
    $x -> _dng() if ($x -> is_int() ||
                     $x -> is_inf() ||
                     $x -> is_nan());
    return $x;
}

sub bmuladd {
    # multiply two numbers and add the third to the result

    # set up parameters
    my ($class, $x, $y, $z, @r)
      = ref($_[0]) && ref($_[0]) eq ref($_[1]) && ref($_[1]) eq ref($_[2])
      ? (ref($_[0]), @_)
      : objectify(3, @_);

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('bmuladd');

    # At least one of x, y, and z is a NaN

    return $x -> bnan(@r) if ($x -> is_nan() ||
                              $y -> is_nan() ||
                              $z -> is_nan());

    # At least one of x, y, and z is an Inf

    if ($x -> is_inf("-")) {

        if ($y -> is_neg()) {                   # x = -inf, y < 0
            if ($z -> is_inf("-")) {
                return $x -> bnan(@r);
            } else {
                return $x -> binf("+", @r);
            }
        } elsif ($y -> is_zero()) {             # x = -inf, y = 0
            return $x -> bnan(@r);
        } else {                                # x = -inf, y > 0
            if ($z->{sign} eq "+inf") {
                return $x -> bnan(@r);
            } else {
                return $x -> binf("-", @r);
            }
        }

    } elsif ($x->{sign} eq "+inf") {

        if ($y -> is_neg()) {                   # x = +inf, y < 0
            if ($z->{sign} eq "+inf") {
                return $x -> bnan(@r);
            } else {
                return $x -> binf("-", @r);
            }
        } elsif ($y -> is_zero()) {             # x = +inf, y = 0
            return $x -> bnan(@r);
        } else {                                # x = +inf, y > 0
            if ($z -> is_inf("-")) {
                return $x -> bnan(@r);
            } else {
                return $x -> binf("+", @r);
            }
        }

    } elsif ($x -> is_neg()) {

        if ($y -> is_inf("-")) {                # -inf < x < 0, y = -inf
            if ($z -> is_inf("-")) {
                return $x -> bnan(@r);
            } else {
                return $x -> binf("+", @r);
            }
        } elsif ($y->{sign} eq "+inf") {        # -inf < x < 0, y = +inf
            if ($z->{sign} eq "+inf") {
                return $x -> bnan(@r);
            } else {
                return $x -> binf("-", @r);
            }
        } else {                                # -inf < x < 0, -inf < y < +inf
            if ($z -> is_inf("-")) {
                return $x -> binf("-", @r);
            } elsif ($z->{sign} eq "+inf") {
                return $x -> binf("+", @r);
            }
        }

    } elsif ($x -> is_zero()) {

        if ($y -> is_inf("-")) {                # x = 0, y = -inf
            return $x -> bnan(@r);
        } elsif ($y->{sign} eq "+inf") {        # x = 0, y = +inf
            return $x -> bnan(@r);
        } else {                                # x = 0, -inf < y < +inf
            if ($z -> is_inf("-")) {
                return $x -> binf("-", @r);
            } elsif ($z->{sign} eq "+inf") {
                return $x -> binf("+", @r);
            }
        }

    } elsif ($x -> is_pos()) {

        if ($y -> is_inf("-")) {                # 0 < x < +inf, y = -inf
            if ($z->{sign} eq "+inf") {
                return $x -> bnan(@r);
            } else {
                return $x -> binf("-", @r);
            }
        } elsif ($y->{sign} eq "+inf") {        # 0 < x < +inf, y = +inf
            if ($z -> is_inf("-")) {
                return $x -> bnan(@r);
            } else {
                return $x -> binf("+", @r);
            }
        } else {                                # 0 < x < +inf, -inf < y < +inf
            if ($z -> is_inf("-")) {
                return $x -> binf("-", @r);
            } elsif ($z->{sign} eq "+inf") {
                return $x -> binf("+", @r);
            }
        }
    }

    # At this point, we know that x, y, and z are finite numbers

    # Rather than copying $y and/or $z, perhaps we should assign the output to
    # a temporary $x value, and assign the final result to $x? XXX

    $y = $y -> copy() if refaddr($y) eq refaddr($x);
    $z = $z -> copy() if refaddr($z) eq refaddr($x);

    # aEb * cEd = (a*c)E(b+d)
    $x->{_m} = $LIB->_mul($x->{_m}, $y->{_m});
    ($x->{_e}, $x->{_es})
      = $LIB -> _sadd($x->{_e}, $x->{_es}, $y->{_e}, $y->{_es});

    $r[3] = $y;                 # no push!

    # adjust sign:
    $x->{sign} = $x->{sign} ne $y->{sign} ? '-' : '+';

    # take lower of the two e's and adapt m1 to it to match m2
    my $e = $z->{_e};
    $e = $LIB->_zero() if !defined $e; # if no BFLOAT?
    $e = $LIB->_copy($e);              # make copy (didn't do it yet)

    my $es;

    ($e, $es) = $LIB -> _ssub($e, $z->{_es} || '+', $x->{_e}, $x->{_es});

    my $add = $LIB->_copy($z->{_m});

    if ($es eq '-')             # < 0
    {
        $x->{_m} = $LIB->_lsft($x->{_m}, $e, 10);
        ($x->{_e}, $x->{_es}) = $LIB -> _sadd($x->{_e}, $x->{_es}, $e, $es);
    } elsif (!$LIB->_is_zero($e)) # > 0
    {
        $add = $LIB->_lsft($add, $e, 10);
    }
    # else: both e are the same, so just leave them

    if ($x->{sign} eq $z->{sign}) {
        # add
        $x->{_m} = $LIB->_add($x->{_m}, $add);
    } else {
        ($x->{_m}, $x->{sign}) =
          $LIB -> _sadd($x->{_m}, $x->{sign}, $add, $z->{sign});
    }

    # delete trailing zeros, then round
    $x -> bnorm() -> round(@r);

    $x -> _dng() if ($x -> is_int() ||
                     $x -> is_inf() ||
                     $x -> is_nan());
    return $x;
}

sub bmodpow {
    # takes a very large number to a very large exponent in a given very
    # large modulus, quickly, thanks to binary exponentiation. Supports
    # negative exponents.
    my ($class, $num, $exp, $mod, @r)
      = ref($_[0]) && ref($_[0]) eq ref($_[1]) && ref($_[1]) eq ref($_[2])
      ? (ref($_[0]), @_)
      : objectify(3, @_);

    # Don't modify constant (read-only) objects.

    return $num if $num -> modify('bmodpow');

    return $num -> bnan(@r)
      if $mod -> is_nan() || $exp -> is_nan() || $mod -> is_nan();

    # check modulus for valid values
    return $num -> bnan(@r) if $mod->{sign} ne '+' || $mod -> is_zero();

    # check exponent for valid values
    if ($exp->{sign} =~ /\w/) {
        # i.e., if it's NaN, +inf, or -inf...
        return $num -> bnan(@r);
    }

    $num -> bmodinv($mod, @r) if $exp->{sign} eq '-';

    # check num for valid values (also NaN if there was no inverse but $exp < 0)
    return $num -> bnan(@r) if $num->{sign} !~ /^[+-]$/;

    # $mod is positive, sign on $exp is ignored, result also positive

    # XXX TODO: speed it up when all three numbers are integers
    $num -> bpow($exp) -> bmod($mod);

    $num -> round(@r);
    $num -> _dng() if ($num -> is_int() ||
                       $num -> is_inf() ||
                       $num -> is_nan());
    return $num;
}

sub blog {
    # Return the logarithm of the operand. If a second operand is defined, that
    # value is used as the base, otherwise the base is assumed to be Euler's
    # constant.

    my ($class, $x, $base, @r);

    # Only objectify the base if it is defined, since an undefined base, as in
    # $x->blog() or $x->blog(undef) signals that the base is Euler's number =
    # 2.718281828...

    if (!ref($_[0]) && $_[0] =~ /^[A-Za-z]|::/) {
        # E.g., Math::BigFloat->blog(256, 2)
        ($class, $x, $base, @r) =
          defined $_[2] ? objectify(2, @_) : objectify(1, @_);
    } else {
        # E.g., $x->blog(2) or the deprecated Math::BigFloat::blog(256, 2)
        ($class, $x, $base, @r) =
          defined $_[1] ? objectify(2, @_) : objectify(1, @_);
    }

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('blog');

    # Handle all exception cases and all trivial cases. I have used Wolfram
    # Alpha (http://www.wolframalpha.com) as the reference for these cases.

    return $x -> bnan(@r) if $x -> is_nan();

    if (defined $base) {
        $base = $class -> new($base)
          unless defined(blessed($base)) && $base -> isa(__PACKAGE__);
        if ($base -> is_nan() || $base -> is_one()) {
            return $x -> bnan(@r);
        } elsif ($base -> is_inf() || $base -> is_zero()) {
            return $x -> bnan(@r) if $x -> is_inf() || $x -> is_zero();
            return $x -> bzero(@r);
        } elsif ($base -> is_negative()) {              # -inf < base < 0
            return $x -> bzero(@r) if $x -> is_one();   #     x = 1
            return $x -> bone('+', @r)  if $x == $base; #     x = base
            # we can't handle these cases, so upgrade, if we can
            return $x -> _upg() -> blog($base, @r) if $class -> upgrade();
            return $x -> bnan(@r);
        }
        return $x -> bone(@r) if $x == $base;       # 0 < base && 0 < x < inf
    }

    if ($x -> is_inf()) {                       # x = +/-inf
        my $sign = defined($base) && $base < 1 ? '-' : '+';
        return $x -> binf($sign, @r);
    } elsif ($x -> is_neg()) {                  # -inf < x < 0
        return $x -> _upg() -> blog($base, @r) if $class -> upgrade();
        return $x -> bnan(@r);
    } elsif ($x -> is_one()) {                  # x = 1
        return $x -> bzero(@r);
    } elsif ($x -> is_zero()) {                 # x = 0
        my $sign = defined($base) && $base < 1 ? '+' : '-';
        return $x -> binf($sign, @r);
    }

    # we need to limit the accuracy to protect against overflow
    my $fallback = 0;
    my ($scale, @params);
    ($x, @params) = $x->_find_round_parameters(@r);

    # no rounding at all, so must use fallback
    if (scalar @params == 0) {
        # simulate old behaviour
        $params[0] = $class -> div_scale(); # and round to it as accuracy
        $params[1] = undef;               # P = undef
        $scale = $params[0]+4;            # at least four more for proper round
        $params[2] = $r[2];               # round mode by caller or undef
        $fallback = 1;                    # to clear a/p afterwards
    } else {
        # the 4 below is empirical, and there might be cases where it is not
        # enough...
        $scale = abs($params[0] || $params[1]) + 4; # take whatever is defined
    }

    # When user set globals, they would interfere with our calculation, so
    # disable them and later re-enable them.

    my $ab = $class -> accuracy();
    my $pb = $class -> precision();
    $class -> accuracy(undef);
    $class -> precision(undef);

    # Disabling upgrading and downgrading is no longer necessary to avoid an
    # infinite recursion, but it avoids unnecessary upgrading and downgrading
    # in the intermediate computations.

    my $upg = $class -> upgrade();
    my $dng = $class -> downgrade();
    $class -> upgrade(undef);
    $class -> downgrade(undef);

    # We also need to disable any set A or P on $x (_find_round_parameters took
    # them already into account), since these would interfere, too.

    $x->{accuracy} = undef;
    $x->{precision} = undef;

    my $done = 0;

    # If both $x and $base are integers, try to calculate an integer result
    # first. This is very fast, and if the exact result was found, we are done.

    if (defined($base) && $base -> is_int() && $x -> is_int()) {
        my $x_lib = $LIB -> _new($x -> bdstr());
        my $b_lib = $LIB -> _new($base -> bdstr());
        ($x_lib, my $exact) = $LIB -> _log_int($x_lib, $b_lib);
        if ($exact) {
            $x->{_m} = $x_lib;
            $x->{_e} = $LIB -> _zero();
            $x -> bnorm();
            $done = 1;
        }
    }

    # If the integer result was not accurate, compute the natural logarithm
    # log($x) (using reduction by 10 and possibly also by 2), and if a
    # different base was requested, convert the result with log($x)/log($base).

    unless ($done) {
        $x -> _log_10($scale);
        if (defined $base) {
            # log_b(x) = ln(x) / ln(b), so compute ln(b)
            my $base_log_e = $base -> copy() -> _log_10($scale);
            $x -> bdiv($base_log_e, $scale);
        }
    }

    # shortcut to not run through _find_round_parameters again

    if (defined $params[0]) {
        $x -> bround($params[0], $params[2]); # then round accordingly
    } else {
        $x -> bfround($params[1], $params[2]); # then round accordingly
    }
    if ($fallback) {
        # clear a/p after round, since user did not request it
        $x->{accuracy} = undef;
        $x->{precision} = undef;
    }

    # Restore globals. We need to do it like this, because setting one
    # undefines the other.

    if (defined $ab) {
        $class -> accuracy($ab);
    } else {
        $class -> precision($pb);
    }

    $class -> upgrade($upg);
    $class -> downgrade($dng);

    $x -> round(@r);
    return $x -> _dng() if $x -> is_int();
    return $x;
}

sub bexp {
    # Calculate e ** X (Euler's number to the power of X)
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('bexp');

    return $x -> bnan(@r)  if $x -> is_nan();
    return $x -> binf(@r)  if $x -> is_inf("+");
    return $x -> bzero(@r) if $x -> is_inf("-");

    # Get the rounding parameters, if any.

    my $fallback = 0;
    my ($scale, @params);
    ($x, @params) = $x -> _find_round_parameters(@r);

    # Error in _find_round_parameters?
    return $x -> bnan(@r) if $x -> is_nan();

    return $x -> bone(@r) if $x -> is_zero();

    # If no rounding parameters are give, use fallback.

    if (!@params) {
        $params[0] = $class -> div_scale();     # fallback accuracy
        $params[1] = undef;                     # no precision
        $params[2] = $r[2];                     # rounding mode
        $scale = $params[0];
        $fallback = 1;                          # to clear a/p afterwards
    } else {
        if (defined($params[0])) {
            $scale = $params[0];
        } else {
            # We perform the computations below using accuracy only, not
            # precision, so when precision is given, we need to "convert" this
            # to accuracy. To do that, we need to know, at least approximately,
            # how many digits there will be in the final result.
            #
            #   log10(exp($x)) = log(exp($x)) / log(10) = $x / log(10)

            #$scale = 1 + int(log($ms) / log(10) + $es) - $params[1];
            my $ndig = $x -> numify() / log(10);
            $scale = 1 + int($ndig) - $params[1];
        }
    }

    # Add extra digits to reduce the consequence of round-off errors in the
    # intermediate computations.

    $scale += 4;

    if (!$x -> isa('Math::BigFloat')) {
        $x = Math::BigFloat -> new($x);
        $class = ref($x);
    }

    # When user set globals, they would interfere with our calculation, so
    # disable them and later re-enable them.

    my $ab = $class -> accuracy();
    my $pb = $class -> precision();
    $class -> accuracy(undef);
    $class -> precision(undef);

    # Disabling upgrading and downgrading is no longer necessary to avoid an
    # infinite recursion, but it avoids unnecessary upgrading and downgrading
    # in the intermediate computations.

    my $upg = $class -> upgrade();
    my $dng = $class -> downgrade();
    $class -> upgrade(undef);
    $class -> downgrade(undef);

    # We also need to disable any set A or P on $x (_find_round_parameters took
    # them already into account), since these would interfere, too.

    $x->{accuracy} = undef;
    $x->{precision} = undef;

    my $x_orig = $x -> copy();

    # We use the following Taylor series:

    #           x    x^2   x^3   x^4
    #  e = 1 + --- + --- + --- + --- ...
    #           1!    2!    3!    4!

    # The difference for each term is X and N, which would result in:
    # 2 copy, 2 mul, 2 add, 1 inc, 1 div operations per term

    # But it is faster to compute exp(1) and then raising it to the
    # given power, esp. if $x is really big and an integer because:

    #  * The numerator is always 1, making the computation faster
    #  * the series converges faster in the case of x == 1
    #  * We can also easily check when we have reached our limit: when the
    #    term to be added is smaller than "1E$scale", we can stop - f.i.
    #    scale == 5, and we have 1/40320, then we stop since 1/40320 < 1E-5.
    #  * we can compute the *exact* result by simulating bigrat math:

    #  1   1    gcd(3, 4) = 1    1*24 + 1*6    5
    #  - + -                  = ---------- =  --
    #  6   24                      6*24       24

    # We do not compute the gcd() here, but simple do:
    #  1   1    1*24 + 1*6   30
    #  - + -  = --------- =  --
    #  6   24       6*24     144

    # In general:
    #  a   c    a*d + c*b         and note that c is always 1 and d = (b*f)
    #  - + -  = ---------
    #  b   d       b*d

    # This leads to:         which can be reduced by b to:
    #  a   1     a*b*f + b    a*f + 1
    #  - + -   = --------- =  -------
    #  b   b*f     b*b*f        b*f

    # The first terms in the series are:

    # 1     1    1    1    1    1     1     1     13700
    # -- + -- + -- + -- + -- + --- + --- + ---- = -----
    # 1     1    2    6   24   120   720   5040   5040

    # Note that we cannot simply reduce 13700/5040 to 685/252, but must keep
    # the numerator and the denominator!

    if ($scale <= 75) {
        # set $x directly from a cached string form
        $x->{_m} = $LIB->_new("2718281828459045235360287471352662497757" .
                              "2470936999595749669676277240766303535476");
        $x->{sign} = '+';
        $x->{_es} = '-';
        $x->{_e} = $LIB->_new(79);
    } else {
        # compute A and B so that e = A / B.

        # After some terms we end up with this, so we use it as a starting
        # point:
        my $A = $LIB->_new("9093339520860578540197197" .
                           "0164779391644753259799242");
        my $F = $LIB->_new(42);
        my $step = 42;

        # Compute number of steps needed to get $A and $B sufficiently large.

        my $steps = _len_to_steps($scale - 4);
        #    print STDERR "# Doing $steps steps for ", $scale-4, " digits\n";

        while ($step++ <= $steps) {
            # calculate $a * $f + 1
            $A = $LIB -> _mul($A, $F);
            $A = $LIB -> _inc($A);
            # increment f
            $F = $LIB -> _inc($F);
        }

        # Compute $B as factorial of $steps (this is faster than doing it
        # manually)
        my $B = $LIB->_fac($LIB->_new($steps));

        #  print "A ", $LIB->_str($A), "\nB ", $LIB->_str($B), "\n";

        # compute A/B with $scale digits in the result (truncate, not round)
        $A = $LIB->_lsft($A, $LIB->_new($scale), 10);
        $A = $LIB->_div($A, $B);

        $x->{_m} = $A;
        $x->{sign} = '+';
        $x->{_es} = '-';
        $x->{_e} = $LIB->_new($scale);
    }

    # Now $x contains now an estimate of e, with some additional digits.

    if ($x_orig -> is_one()) {

        # else just round the already computed result

        $x->{accuracy} = undef;
        $x->{precision} = undef;

        # shortcut to not run through _find_round_parameters again

        if (defined $params[0]) {
            $x -> bround($params[0], $params[2]); # then round accordingly
        } else {
            $x -> bfround($params[1], $params[2]); # then round accordingly
        }

    } else {

        # Use the fact exp(x) = exp(x/n)**n. In our case, n = 2**i for some
        # integer i. We use this to compute exp(y) where y = x / (2**i) and
        # 1 <= |y| < 2.
        #
        # The code below is similar to the code found in to_ieee754().

        # We need to find the base 2 exponent. First make an estimate of the
        # base 2 exponent, before adjusting it below. We could skip this
        # estimation and go straight to the while-loops below, but the loops
        # are slow, especially when the final exponent is far from zero and
        # even more so if the number of digits is large. This initial
        # estimation speeds up the computation dramatically.
        #
        #   log2($m * 10**$e) = log10($m + 10**$e) * log(10)/log(2)
        #                     = (log10($m) + $e) * log(10)/log(2)
        #                     = (log($m)/log(10) + $e) * log(10)/log(2)

        my ($m, $e) = $x_orig -> nparts();
        my $ms = $m -> numify();
        my $es = $e -> numify();

        # We start off by initializing the exponent to zero and the mantissa to
        # the input value. Then we increase the mantissa and decrease the
        # exponent, or vice versa, until the mantissa is in the desired range
        # or we hit one of the limits for the exponent.

        my $mant = $x_orig -> copy() -> babs();
        my $expo;

        my $one  = $class -> bone();
        my $two  = $class -> new("2");
        my $half = $class -> new("0.5");

        my $expo_est = (log(abs($ms))/log(10) + $es) * log(10)/log(2);
        $expo_est = int($expo_est);

        # Don't multiply by a number raised to a negative exponent. This will
        # cause a division, whose result is truncated to some fixed number of
        # digits. Instead, multiply by the inverse number raised to a positive
        # exponent.

        $expo = $class -> new($expo_est);
        if ($expo_est > 0) {
            $mant -> bmul($half -> copy() -> bpow($expo));
        } elsif ($expo_est < 0) {
            my $expo_abs = $expo -> copy() -> bneg();
            $mant -> bmul($two -> copy() -> bpow($expo_abs));
        }

        # Final adjustment of the estimate above.

        while ($mant -> bcmp($two) >= 0) {      # $mant <= $two
            $mant -> bmul($half);
            $expo -> binc();
        }

        while ($mant -> bcmp($one) < 0) {       # $mant > $one
            $mant -> bmul($two);
            $expo -> bdec();
        }

        # Because of the upscaling, we need some additional digits.

        my $rescale = int($scale + abs($expo) * log(2) / log(10) + 1);
        $rescale = 4 if $rescale < 4;

        $x -> bpow($mant, $rescale);
        my $pow2 = $two -> bpow($expo, $rescale);
        $pow2 -> bneg() if $x_orig -> is_negative();

        # The bpow() below fails with the GMP and GMPz libraries if abs($pow2)
        # >= 2**30 = 1073741824. With the Pari library, it fails already when
        # abs($pow) >= 2**13 = 8192. With the Calc library, it is rediculously
        # slow when abs($pow2) is large. Fixme?

        croak "cannot compute bexp(); input value is too large"
          if $pow2 -> copy() -> babs() -> bcmp("1073741824") >= 0;

        $x -> bpow($pow2, $rescale);

        # Rounding parameters given as arguments currently don't override
        # instance variables, so accuracy (which is set in the computations
        # above) must be undefined before rounding. Fixme.

        $x->{accuracy} = undef;
        $x -> round(@params);
    }

    if ($fallback) {
        # clear a/p after round, since user did not request it
        $x->{accuracy} = undef;
        $x->{precision} = undef;
    }

    # Restore globals. We need to do it like this, because setting one
    # undefines the other.

    if (defined $ab) {
        $class -> accuracy($ab);
    } else {
        $class -> precision($pb);
    }

    $class -> upgrade($upg);
    $class -> downgrade($dng);

    # If downgrading, remember to preserve the relevant instance parameters.
    # There should be a more elegant way to do this. Fixme.

    $x -> round(@r);
    $x -> _dng() if $x -> is_int();
    $x;
}

sub bilog2 {
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('bilog2');

    return $x -> bnan(@r)        if $x -> is_nan();
    return $x -> binf("+", @r)   if $x -> is_inf("+");
    return $x -> binf("-", @r)   if $x -> is_zero();

    if ($x -> is_neg()) {
        return $x -> _upg() -> bilog2(@r) if $class -> upgrade();
        return $x -> bnan(@r);
    }

    if ($x->{_es} eq '-') {                     # exponent < 0
        $x->{_m} = $LIB->_rsft($x->{_m}, $x->{_e}, 10);
    } elsif (! $LIB->_is_zero($x->{_e})) {      # exponent > 0
        $x->{_m} = $LIB->_lsft($x->{_m}, $x->{_e}, 10);
    }

    $x->{_m} = $LIB -> _ilog2($x->{_m});
    $x->{_e} = $LIB -> _zero();
    $x -> bnorm() -> round(@r);
    $x -> _dng();
    return $x;
}

sub bilog10 {
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('bilog10');

    return $x -> bnan(@r)        if $x -> is_nan();
    return $x -> binf("+", @r)   if $x -> is_inf("+");
    return $x -> binf("-", @r)   if $x -> is_zero();

    if ($x -> is_neg()) {
        return $x -> _upg() -> bilog10(@r) if $class -> upgrade();
        return $x -> bnan(@r);
    }

    if ($x->{_es} eq '-') {                     # exponent < 0
        $x->{_m} = $LIB->_rsft($x->{_m}, $x->{_e}, 10);
    } elsif (! $LIB->_is_zero($x->{_e})) {      # exponent > 0
        $x->{_m} = $LIB->_lsft($x->{_m}, $x->{_e}, 10);
    }

    $x->{_m} = $LIB -> _ilog10($x->{_m});
    $x->{_e} = $LIB -> _zero();
    $x -> bnorm() -> round(@r);
    $x -> _dng();
    return $x;
}

sub bclog2 {
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('bclog2');

    return $x -> bnan(@r)        if $x -> is_nan();
    return $x -> binf("+", @r)   if $x -> is_inf("+");
    return $x -> binf("-", @r)   if $x -> is_zero();

    if ($x -> is_neg()) {
        return $x -> _upg() -> bclog2(@r) if $class -> upgrade();
        return $x -> bnan(@r);
    }

    if ($x->{_es} eq '-') {                     # exponent < 0
        $x->{_m} = $LIB->_rsft($x->{_m}, $x->{_e}, 10);
    } elsif (! $LIB->_is_zero($x->{_e})) {      # exponent > 0
        $x->{_m} = $LIB->_lsft($x->{_m}, $x->{_e}, 10);
    }

    $x->{_m} = $LIB -> _clog2($x->{_m});
    $x->{_e} = $LIB -> _zero();
    $x -> bnorm() -> round(@r);
    $x -> _dng();
    return $x;
}

sub bclog10 {
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('bclog10');

    return $x -> bnan(@r)        if $x -> is_nan();
    return $x -> binf("+", @r)   if $x -> is_inf("+");
    return $x -> binf("-", @r)   if $x -> is_zero();

    if ($x -> is_neg()) {
        return $x -> _upg() -> bclog10(@r) if $class -> upgrade();
        return $x -> bnan(@r);
    }

    if ($x->{_es} eq '-') {                     # exponent < 0
        $x->{_m} = $LIB->_rsft($x->{_m}, $x->{_e}, 10);
    } elsif (! $LIB->_is_zero($x->{_e})) {      # exponent > 0
        $x->{_m} = $LIB->_lsft($x->{_m}, $x->{_e}, 10);
    }

    $x->{_m} = $LIB -> _clog10($x->{_m});
    $x->{_e} = $LIB -> _zero();
    $x -> bnorm() -> round(@r);
    $x -> _dng();
    return $x;
}

sub bnok {
    # Calculate n over k (binomial coefficient or "choose" function) as
    # integer. set up parameters
    my ($class, $x, $y, @r) = ref($_[0]) && ref($_[0]) eq ref($_[1])
                            ? (ref($_[0]), @_)
                            : objectify(2, @_);

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('bnok');

    return $x -> bnan() if $x -> is_nan() || $y -> is_nan();
    return $x -> bnan() if (($x -> is_finite() && !$x -> is_int()) ||
                            ($y -> is_finite() && !$y -> is_int()));

    # This should be implemented without converting to Math::BigInt. XXX

    my $xint = $x -> as_int();          # to Math::BigInt
    my $yint = $y -> as_int();          # to Math::BigInt

    $xint -> bnok($yint);
    $xint -> round(@r);

    my $xflt = $xint -> as_float();
    $x -> {sign} = $xflt -> {sign};
    $x -> {_m}   = $xflt -> {_m};
    $x -> {_es}  = $xflt -> {_es};
    $x -> {_e}   = $xflt -> {_e};

    return $x -> _dng();
    return $x;
}

sub bperm {
    # Calculate n over k (binomial coefficient or "choose" function) as
    # integer. set up parameters
    my ($class, $x, $y, @r) = ref($_[0]) && ref($_[0]) eq ref($_[1])
                            ? (ref($_[0]), @_)
                            : objectify(2, @_);

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('bperm');

    return $x -> bnan() if $x -> is_nan() || $y -> is_nan();
    return $x -> bnan() if (($x -> is_finite() && !$x -> is_int()) ||
                            ($y -> is_finite() && !$y -> is_int()));

    # This should be implemented without converting to Math::BigInt. XXX

    my $xint = $x -> as_int();          # to Math::BigInt
    my $yint = $y -> as_int();          # to Math::BigInt

    $xint -> bperm($yint);
    $xint -> round(@r);

    my $xflt = $xint -> as_float();
    $x -> {sign} = $xflt -> {sign};
    $x -> {_m}   = $xflt -> {_m};
    $x -> {_es}  = $xflt -> {_es};
    $x -> {_e}   = $xflt -> {_e};

    return $x -> _dng();
    return $x;
}

sub bsin {
    # Calculate a sinus of x.
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    # First we apply range reduction to x. This is because if x is large, the
    # Taylor series converges slowly and requires higher accuracy in the
    # intermediate computation. The Taylor series is:
    #
    #                 x^3   x^5   x^7   x^9
    #    sin(x) = x - --- + --- - --- + --- ...
    #                  3!    5!    7!    9!

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('bsin');

    return $x -> bzero(@r) if $x -> is_zero();
    return $x -> bnan(@r)  if $x -> is_nan() || $x -> is_inf();

    # Get the rounding parameters, if any.

    my $fallback = 0;
    my ($scale, @params);
    ($x, @params) = $x -> _find_round_parameters(@r);

    # Error in _find_round_parameters?

    return $x -> bnan(@r) if $x -> is_nan();

    # If no rounding parameters are given, use fallback.

    if (!@params) {
        $params[0] = $class -> div_scale();     # fallback accuracy
        $params[1] = undef;                     # no precision
        $params[2] = $r[2];                     # rounding mode
        $scale = $params[0];
        $fallback = 1;                          # to clear a/p afterwards
    } else {
        if (defined($params[0])) {
            $scale = $params[0];
        } else {
            # We perform the computations below using accuracy only, not
            # precision, so when precision is given, we need to "convert" this
            # to accuracy.
            $scale = 1 - $params[1];
        }
    }

    # Add more digits to the scale if the magnitude of $x is large.

    my ($m, $e) = $x -> nparts();
    $scale += $e if $x >= 10;
    $scale = 4 if $scale < 4;

    # When user set globals, they would interfere with our calculation, so
    # disable them and later re-enable them

    my $ab = $class -> accuracy();
    my $pb = $class -> precision();
    $class -> accuracy(undef);
    $class -> precision(undef);

    # Disabling upgrading and downgrading is no longer necessary to avoid an
    # infinite recursion, but it avoids unnecessary upgrading and downgrading
    # in the intermediate computations.

    my $upg = $class -> upgrade();
    my $dng = $class -> downgrade();
    $class -> upgrade(undef);
    $class -> downgrade(undef);

    # We also need to disable any set A or P on $x (_find_round_parameters took
    # them already into account), since these would interfere, too.

    $x->{accuracy} = undef;
    $x->{precision} = undef;

    my $sin_prev;       # the previous approximation of sin(x)
    my $sin;            # the current approximation of sin(x)

    while (1) {

        # Compute constants to the current scale.

        my $pi     = $class -> bpi($scale);         # 𝜋
        my $twopi  = $pi -> copy() -> bmul("2");    # 2𝜋
        my $halfpi = $pi -> copy() -> bmul("0.5");  # 𝜋/2

        # Use the fact that sin(-x) = -sin(x) to reduce the range to the
        # interval to [0,∞).

        my $xsgn = $x < 0 ? -1 : 1;
        my $x = $x -> copy() -> babs();

        # Use the fact that sin(2𝜋x) = sin(x) to reduce the range to the
        # interval to [0, 2𝜋).

        $x -> bmod($twopi, $scale);

        # Use the fact that sin(x+𝜋) = -sin(x) to reduce the range to the
        # interval to [0,𝜋).

        if ($x -> bcmp($pi) > 0) {
            $xsgn = -$xsgn;
            $x -> bsub($pi);
        }

        # Use the fact that sin(𝜋-x) = sin(x) to reduce the range to the
        # interval [0,𝜋/2).

        if ($x -> bcmp($halfpi) > 0) {
            $x -> bsub($pi) -> bneg();     # 𝜋 - x
        }

        my $tol = $class -> new("1E-". ($scale-1));

        my $xsq  = $x -> copy() -> bmul($x, $scale) -> bneg();
        my $term = $x -> copy();
        my $fac  = $class -> bone();
        my $n    = $class -> bone();

        $sin = $x -> copy();    # initialize sin(x) to the first term

        while (1) {
            $n -> binc();
            $fac = $n -> copy();
            $n -> binc();
            $fac -> bmul($n);

            $term -> bmul($xsq, $scale) -> bdiv($fac, $scale);

            $sin -> badd($term, $scale);
            last if $term -> copy() -> babs() -> bcmp($tol) < 0;
        }

        $sin -> bneg() if $xsgn < 0;

        # Rounding parameters given as arguments currently don't override
        # instance variables, so accuracy (which is set in the computations
        # above) must be undefined before rounding. Fixme.

        $sin->{accuracy} = undef;
        $sin -> round(@params);

        # Compare the current approximation of sin(x) with the previous one,
        # and if they are identical, we're done.

        if (defined $sin_prev) {
            last if $sin -> bcmp($sin_prev) == 0;
        }

        # If the current approximation of sin(x) is different from the previous
        # approximation, double the scale (accuracy) and retry.

        $sin_prev = $sin;
        $scale *= 2;
    }

    # Assign the result to the invocand.

    %$x = %$sin;

    if ($fallback) {
        # clear a/p after round, since user did not request it
        $x->{accuracy} = undef;
        $x->{precision} = undef;
    }

    # Restore globals. We need to do it like this, because setting one
    # undefines the other.

    if (defined $ab) {
        $class -> accuracy($ab);
    } else {
        $class -> precision($pb);
    }

    $class -> upgrade($upg);
    $class -> downgrade($dng);

    # rounding has already been done
    $x -> _dng() if $x -> is_int();
    $x;
}

sub bcos {
    # Calculate a cosinus of x.
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    # Taylor:      x^2   x^4   x^6   x^8
    #    cos = 1 - --- + --- - --- + --- ...
    #               2!    4!    6!    8!

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('bcos');

    # we need to limit the accuracy to protect against overflow
    my $fallback = 0;
    my ($scale, @params);
    ($x, @params) = $x->_find_round_parameters(@r);

    # error in _find_round_parameters?
    return $x if $x -> is_nan();
    return $x -> bnan()   if $x -> is_inf();
    return $x -> bone(@r) if $x -> is_zero();

    # no rounding at all, so must use fallback
    if (scalar @params == 0) {
        # simulate old behaviour
        $params[0] = $class -> div_scale(); # and round to it as accuracy
        $params[1] = undef;                 # disable P
        $scale = $params[0] + 4;        # at least four more for proper round
        $params[2] = $r[2];             # round mode by caller or undef
        $fallback = 1;                  # to clear a/p afterwards
    } else {
        # the 4 below is empirical, and there might be cases where it is not
        # enough...
        $scale = abs($params[0] || $params[1]) + 4; # take whatever is defined
    }

    # When user set globals, they would interfere with our calculation, so
    # disable them and later re-enable them.

    my $ab = $class -> accuracy();
    my $pb = $class -> precision();
    $class -> accuracy(undef);
    $class -> precision(undef);

    # Disabling upgrading and downgrading is no longer necessary to avoid an
    # infinite recursion, but it avoids unnecessary upgrading and downgrading
    # in the intermediate computations.

    my $upg = $class -> upgrade();
    my $dng = $class -> downgrade();
    $class -> upgrade(undef);
    $class -> downgrade(undef);

    # We also need to disable any set A or P on $x (_find_round_parameters took
    # them already into account), since these would interfere, too.

    $x->{accuracy} = undef;
    $x->{precision} = undef;

    my $over = $x * $x;         # X ^ 2
    my $x2 = $over -> copy();     # X ^ 2; difference between terms
    my $sign = 1;               # start with -=
    my $below = $class -> new(2);
    my $factorial = $class -> new(3);
    $x -> bone();
    $x->{accuracy} = undef;
    $x->{precision} = undef;

    my $limit = $class -> new("1E-". ($scale-1));
    #my $steps = 0;
    while (3 < 5) {
        # we calculate the next term, and add it to the last
        # when the next term is below our limit, it won't affect the outcome
        # anymore, so we stop:
        my $next = $over -> copy() -> bdiv($below, $scale);
        last if $next -> bacmp($limit) <= 0;

        if ($sign == 0) {
            $x -> badd($next);
        } else {
            $x -> bsub($next);
        }
        $sign = 1-$sign;        # alternate
        # calculate things for the next term
        $over -> bmul($x2);                       # $x*$x
        $below -> bmul($factorial);              # n*(n+1)
        $factorial -> binc();
        $below -> bmul($factorial);              # n*(n+1)
        $factorial -> binc();
    }

    # shortcut to not run through _find_round_parameters again
    if (defined $params[0]) {
        $x -> bround($params[0], $params[2]); # then round accordingly
    } else {
        $x -> bfround($params[1], $params[2]); # then round accordingly
    }
    if ($fallback) {
        # clear a/p after round, since user did not request it
        $x->{accuracy} = undef;
        $x->{precision} = undef;
    }

    # Restore globals. We need to do it like this, because setting one
    # undefines the other.

    if (defined $ab) {
        $class -> accuracy($ab);
    } else {
        $class -> precision($pb);
    }

    $class -> upgrade($upg);
    $class -> downgrade($dng);

    $x -> round(@r);
    $x -> _dng() if $x -> is_int();
    $x;
}

sub batan {
    # Calculate a arcus tangens of x.
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    # taylor:       x^3   x^5   x^7   x^9
    #    atan = x - --- + --- - --- + --- ...
    #                3     5     7     9

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('batan');

    return $x -> bnan(@r) if $x -> is_nan();

    # We need to limit the accuracy to protect against overflow.

    my $fallback = 0;
    my ($scale, @params);
    ($x, @params) = $x->_find_round_parameters(@r);

    # Error in _find_round_parameters?

    return $x -> bnan(@r) if $x -> is_nan();

    if ($x->{sign} =~ /^[+-]inf\z/) {
        # +inf result is PI/2
        # -inf result is -PI/2
        # calculate PI/2
        my $pi = $class -> bpi(@r);
        # modify $x in place
        $x->{_m} = $pi->{_m};
        $x->{_e} = $pi->{_e};
        $x->{_es} = $pi->{_es};
        # -y => -PI/2, +y => PI/2
        $x->{sign} = substr($x->{sign}, 0, 1); # "+inf" => "+"
        $x -> {_m} = $LIB->_div($x->{_m}, $LIB->_new(2));
        return $x;
    }

    return $x -> bzero(@r) if $x -> is_zero();

    # no rounding at all, so must use fallback
    if (scalar @params == 0) {
        # simulate old behaviour
        $params[0] = $class -> div_scale(); # and round to it as accuracy
        $params[1] = undef;               # disable P
        $scale = $params[0]+4;            # at least four more for proper round
        $params[2] = $r[2];               # round mode by caller or undef
        $fallback = 1;                    # to clear a/p afterwards
    } else {
        # the 4 below is empirical, and there might be cases where it is not
        # enough...
        $scale = abs($params[0] || $params[1]) + 4; # take whatever is defined
    }

    # 1 or -1 => PI/4
    # inlined is_one() && is_one('-')
    if ($LIB->_is_one($x->{_m}) && $LIB->_is_zero($x->{_e})) {
        my $pi = $class -> bpi($scale - 3);
        # modify $x in place
        $x->{_m} = $pi->{_m};
        $x->{_e} = $pi->{_e};
        $x->{_es} = $pi->{_es};
        # leave the sign of $x alone (+1 => +PI/4, -1 => -PI/4)
        $x->{_m} = $LIB->_div($x->{_m}, $LIB->_new(4));
        return $x;
    }

    # When user set globals, they would interfere with our calculation, so
    # disable them and later re-enable them.

    my $ab = $class -> accuracy();
    my $pb = $class -> precision();
    $class -> accuracy(undef);
    $class -> precision(undef);

    # Disable upgrading and downgrading.

    my $upg = $class -> upgrade();
    my $dng = $class -> downgrade();
    $class -> upgrade(undef);
    $class -> downgrade(undef);

    # We also need to disable any set A or P on $x (_find_round_parameters took
    # them already into account), since these would interfere, too.

    $x->{accuracy} = undef;
    $x->{precision} = undef;

    # This series is only valid if -1 < x < 1, so for other x we need to
    # calculate PI/2 - atan(1/x):
    my $pi = undef;
    if ($x -> bacmp($x -> copy() -> bone) >= 0) {
        # calculate PI/2
        $pi = $class -> bpi($scale - 3);
        $pi->{_m} = $LIB->_div($pi->{_m}, $LIB->_new(2));
        # calculate 1/$x:
        my $x_copy = $x -> copy();
        # modify $x in place
        $x -> bone();
        $x -> bdiv($x_copy, $scale);
    }

    my $fmul = 1;
    foreach (0 .. int($scale / 20)) {
        $fmul *= 2;
        $x -> bdiv($x -> copy() -> bmul($x) -> binc() -> bsqrt($scale + 4) -> binc(),
                      $scale + 4);
    }

    my $over = $x * $x;   # X ^ 2
    my $x2 = $over -> copy();  # X ^ 2; difference between terms
    $over -> bmul($x);         # X ^ 3 as starting value
    my $sign = 1;               # start with -=
    my $below = $class -> new(3);
    my $two = $class -> new(2);
    $x->{accuracy} = undef;
    $x->{precision} = undef;

    my $limit = $class -> new("1E-". ($scale-1));
    #my $steps = 0;
    while (1) {
        # We calculate the next term, and add it to the last. When the next
        # term is below our limit, it won't affect the outcome anymore, so we
        # stop:
        my $next = $over -> copy() -> bdiv($below, $scale);
        last if $next -> bacmp($limit) <= 0;

        if ($sign == 0) {
            $x -> badd($next);
        } else {
            $x -> bsub($next);
        }
        $sign = 1 - $sign;              # alternatex
        # calculate things for the next term
        $over -> bmul($x2);             # $x*$x
        $below -> badd($two);           # n += 2
    }
    $x -> bmul($fmul);

    if (defined $pi) {
        my $x_copy = $x -> copy();
        # modify $x in place
        $x->{_m} = $pi->{_m};
        $x->{_e} = $pi->{_e};
        $x->{_es} = $pi->{_es};
        # PI/2 - $x
        $x -> bsub($x_copy);
    }

    # Shortcut to not run through _find_round_parameters again.
    if (defined $params[0]) {
        $x -> bround($params[0], $params[2]); # then round accordingly
    } else {
        $x -> bfround($params[1], $params[2]); # then round accordingly
    }
    if ($fallback) {
        # Clear a/p after round, since user did not request it.
        $x->{accuracy} = undef;
        $x->{precision} = undef;
    }

    # Restore globals. We need to do it like this, because setting one
    # undefines the other.

    if (defined $ab) {
        $class -> accuracy($ab);
    } else {
        $class -> precision($pb);
    }

    $class -> upgrade($upg);
    $class -> downgrade($dng);

    return $x -> _dng() if ($x -> is_int() ||
                            $x -> is_inf());
    $x;
}

sub batan2 {
    # $y -> batan2($x) returns the arcus tangens of $y / $x.

    # Set up parameters.
    my ($class, $y, $x, @r) = ref($_[0]) && ref($_[0]) eq ref($_[1])
                            ? (ref($_[0]), @_)
                            : objectify(2, @_);

    # Don't modify constant (read-only) objects.

    return $y if $y -> modify('batan2');

    # Handle all NaN cases.
    return $y -> bnan() if $x -> is_nan() || $y -> is_nan();

    # We need to limit the accuracy to protect against overflow.
    my $fallback = 0;
    my ($scale, @params);
    ($y, @params) = $y -> _find_round_parameters(@r);

    # Error in _find_round_parameters?
    return $y if $y -> is_nan();

    # No rounding at all, so must use fallback.
    if (scalar @params == 0) {
        # Simulate old behaviour
        $params[0] = $class -> div_scale(); # and round to it as accuracy
        $params[1] = undef;                 # disable P
        $scale = $params[0] + 4; # at least four more for proper round
        $params[2] = $r[2];      # round mode by caller or undef
        $fallback = 1;           # to clear a/p afterwards
    } else {
        # The 4 below is empirical, and there might be cases where it is not
        # enough ...
        $scale = abs($params[0] || $params[1]) + 4; # take whatever is defined
    }

    if ($x -> is_inf("+")) {                          # x = inf
        if ($y -> is_inf("+")) {                      #    y = inf
            $y -> bpi($scale) -> bmul("0.25");        #       pi/4
        } elsif ($y -> is_inf("-")) {                 #    y = -inf
            $y -> bpi($scale) -> bmul("-0.25");       #       -pi/4
        } else {                                      #    -inf < y < inf
            return $y -> bzero(@r);                   #       0
        }
    } elsif ($x -> is_inf("-")) {                     # x = -inf
        if ($y -> is_inf("+")) {                      #    y = inf
            $y -> bpi($scale) -> bmul("0.75");        #       3/4 pi
        } elsif ($y -> is_inf("-")) {                 #    y = -inf
            $y -> bpi($scale) -> bmul("-0.75");       #       -3/4 pi
        } elsif ($y >= 0) {                           #    y >= 0
            $y -> bpi($scale);                        #       pi
        } else {                                      #    y < 0
            $y -> bpi($scale) -> bneg();              #       -pi
        }
    } elsif ($x > 0) {                                # 0 < x < inf
        if ($y -> is_inf("+")) {                      #    y = inf
            $y -> bpi($scale) -> bmul("0.5");         #       pi/2
        } elsif ($y -> is_inf("-")) {                 #    y = -inf
            $y -> bpi($scale) -> bmul("-0.5");        #       -pi/2
        } else {                                      #   -inf < y < inf
            $y -> bdiv($x, $scale) -> batan($scale);  #       atan(y/x)
        }
    } elsif ($x < 0) {                                # -inf < x < 0
        my $pi = $class -> bpi($scale);
        if ($y >= 0) {                                #    y >= 0
            $y -> bdiv($x, $scale) -> batan()         #       atan(y/x) + pi
               -> badd($pi);
        } else {                                      #    y < 0
            $y -> bdiv($x, $scale) -> batan()         #       atan(y/x) - pi
               -> bsub($pi);
        }
    } else {                                          # x = 0
        if ($y > 0) {                                 #    y > 0
            $y -> bpi($scale) -> bmul("0.5");         #       pi/2
        } elsif ($y < 0) {                            #    y < 0
            $y -> bpi($scale) -> bmul("-0.5");        #       -pi/2
        } else {                                      #    y = 0
            return $y -> bzero(@r);                   #       0
        }
    }

    $y -> round(@r);

    if ($fallback) {
        $y->{accuracy} = undef;
        $y->{precision} = undef;
    }

    return $y;
}

sub bfac {
    # (BFLOAT or num_str, BFLOAT or num_str) return BFLOAT
    # compute factorial number, modifies first argument

    # set up parameters
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('bfac');

    return $x -> bnan(@r)      if $x -> is_nan()  || $x -> is_inf("-");
    return $x -> binf("+", @r) if $x -> is_inf("+");
    return $x -> bnan(@r)      if $x -> is_neg() || !$x -> is_int();
    return $x -> bone(@r)      if $x -> is_zero() || $x -> is_one();

    if ($x -> is_neg() || !$x -> is_int()) {
        return $x -> _upg() -> bfac(@r) if $class -> upgrade();
        return $x -> bnan(@r);
    }

    if (! $LIB->_is_zero($x->{_e})) {
        $x->{_m} = $LIB->_lsft($x->{_m}, $x->{_e}, 10); # change 12e1 to 120e0
        $x->{_e} = $LIB->_zero();           # normalize
        $x->{_es} = '+';
    }
    $x->{_m} = $LIB->_fac($x->{_m});       # calculate factorial

    $x -> bnorm();                      # norm again
    $x -> round(@r);
    $x -> _dng();
    return $x;
}

sub bdfac {
    # compute double factorial, modify $x in place
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('bdfac');

    return $x -> bnan(@r)      if $x -> is_nan() || $x -> is_inf("-");
    return $x -> binf("+", @r) if $x -> is_inf("+");
    return $x -> bnan(@r)      if $x <= -2 || !$x -> is_int();
    return $x -> bone(@r)      if $x <= 1;

    croak("bdfac() requires a newer version of the $LIB library.")
        unless $LIB -> can('_dfac');

    if (! $LIB->_is_zero($x->{_e})) {
        $x->{_m} = $LIB->_lsft($x->{_m}, $x->{_e}, 10); # change 12e1 to 120e0
        $x->{_e} = $LIB->_zero();           # normalize
        $x->{_es} = '+';
    }
    $x->{_m} = $LIB->_dfac($x->{_m});   # calculate factorial

    $x -> bnorm();                      # norm again
    $x -> round(@r);
    $x -> _dng();
    return $x;
}

sub btfac {
    # compute triple factorial

    # set up parameters
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('btfac');

    return $x -> bnan(@r)      if $x -> is_nan()  || $x -> is_inf("-");
    return $x -> binf("+", @r) if $x -> is_inf("+");

    if ($x <= -3 || !$x -> is_int()) {
        return $x -> _upg() -> btfac(@r) if $class -> upgrade();
        return $x -> bnan(@r);
    }

    my $k = $class -> new("3");
    return $x -> bnan(@r) if $x <= -$k;

    my $one = $class -> bone();
    return $x -> bone(@r) if $x <= $one;

    my $f = $x -> copy();
    while ($f -> bsub($k) > $one) {
        $x = $x -> bmul($f);
    }

    $x -> round(@r);
    $x -> _dng();
    return $x;
}

sub bmfac {
    my ($class, $x, $k, @r) = ref($_[0]) && ref($_[0]) eq ref($_[1])
                            ? (ref($_[0]), @_)
                            : objectify(2, @_);

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('bmfac');

    return $x -> bnan(@r)      if $x -> is_nan() || $x -> is_inf("-") ||
                                  !$k -> is_pos();
    return $x -> binf("+", @r) if $x -> is_inf("+");
    return $x -> bround(@r)    if $k -> is_inf("+");
    return $x -> bnan(@r)      if !$x -> is_int() || !$k -> is_int();
    return $x -> bnan(@r)      if $k < 1 || $x <= -$k;

    my $one = $class -> bone();
    return $x -> bone(@r) if $x <= $one;

    my $f = $x -> copy();
    while ($f -> bsub($k) > $one) {
        $x -> bmul($f);
    }

    $x -> round(@r);
    $x -> _dng();
    return $x;
}

sub bfib {
    # compute Fibonacci number(s)
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    croak("bfib() requires a newer version of the $LIB library.")
        unless $LIB -> can('_fib');

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('bfib');

    # List context.

    if (wantarray) {
        croak("bfib() can't return an infinitely long list of numbers")
          if $x -> is_inf();

        return if $x -> is_nan() || !$x -> is_int();

        # The following places a limit on how large $x can be. Should this
        # limit be removed? XXX

        my $n = $x -> numify();

        my @y;
        {
            $y[0] = $x -> copy() -> babs();
            $y[0]{_m} = $LIB -> _zero();
            $y[0]{_e} = $LIB -> _zero();
            last if $n == 0;

            $y[1] = $y[0] -> copy();
            $y[1]{_m} = $LIB -> _one();
            $y[1]{_e} = $LIB -> _zero();
            last if $n == 1;

            for (my $i = 2 ; $i <= abs($n) ; $i++) {
                $y[$i] = $y[$i - 1] -> copy();
                $y[$i]{_m} = $LIB -> _add($LIB -> _copy($y[$i - 1]{_m}),
                                                        $y[$i - 2]{_m});
            }

            # If negative, insert sign as appropriate.

            if ($x -> is_neg()) {
                for (my $i = 2 ; $i <= $#y ; $i += 2) {
                    $y[$i]{sign} = '-';
                }
            }

            # The last element in the array is the invocand.

            $x->{sign} = $y[-1]{sign};
            $x->{_m}   = $y[-1]{_m};
            $x->{_es}  = $y[-1]{_es};
            $x->{_e}   = $y[-1]{_e};
            $y[-1] = $x;
        }

        for (@y) {
            $_ -> bnorm();
            $_ -> round(@r);
        }

        return @y;
    }

    # Scalar context.

    else {
        return $x if $x -> is_inf('+');
        return $x -> bnan() if $x -> is_nan() || $x -> is_inf('-');

        if ($x -> is_int()) {

            $x->{sign}  = $x -> is_neg() && $x -> is_even() ? '-' : '+';
            $x->{_m} = $LIB -> _lsft($x->{_m}, $x -> {_e}, 10);
            $x->{_e} = $LIB -> _zero();
            $x->{_m} = $LIB -> _fib($x->{_m});
            $x -> bnorm();
        }

        return $x -> round(@r);
    }
}

sub blucas {
    # compute Lucas number(s)
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    croak("blucas() requires a newer version of the $LIB library.")
        unless $LIB -> can('_lucas');

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('blucas');

    # List context.

    if (wantarray) {
        croak("blucas() can't return an infinitely long list of numbers")
          if $x -> is_inf();

        return if $x -> is_nan() || !$x -> is_int();

        # The following places a limit on how large $x can be. Should this
        # limit be removed? XXX

        my $n = $x -> numify();

        my @y;
        {
            $y[0] = $x -> copy() -> babs();
            $y[0]{_m} = $LIB -> _two();
            $y[0]{_e} = $LIB -> _zero();
            last if $n == 0;

            $y[1] = $y[0] -> copy();
            $y[1]{_m} = $LIB -> _one();
            $y[1]{_e} = $LIB -> _zero();
            last if $n == 1;

            for (my $i = 2 ; $i <= abs($n) ; $i++) {
                $y[$i] = $y[$i - 1] -> copy();
                $y[$i]{_m} = $LIB -> _add($LIB -> _copy($y[$i - 1]{_m}),
                                                        $y[$i - 2]{_m});
            }

            # If negative, insert sign as appropriate.

            if ($x -> is_neg()) {
                for (my $i = 2 ; $i <= $#y ; $i += 2) {
                    $y[$i]{sign} = '-';
                }
            }

            # The last element in the array is the invocand.

            $x->{sign} = $y[-1]{sign};
            $x->{_m}   = $y[-1]{_m};
            $x->{_es}  = $y[-1]{_es};
            $x->{_e}   = $y[-1]{_e};
            $y[-1] = $x;
        }

        for (@y) {
            $_ -> bnorm();
            $_ -> round(@r);
        }

        return @y;
    }

    # Scalar context.

    else {
        return $x if $x -> is_inf('+');
        return $x -> bnan() if $x -> is_nan() || $x -> is_inf('-');

        if ($x -> is_int()) {

            $x->{sign}  = $x -> is_neg() && $x -> is_even() ? '-' : '+';
            $x->{_m} = $LIB -> _lsft($x->{_m}, $x -> {_e}, 10);
            $x->{_e} = $LIB -> _zero();
            $x->{_m} = $LIB -> _lucas($x->{_m});
            $x -> bnorm();
        }

        return $x -> round(@r);
    }
}

sub blsft {
    # shift left by $y in base $b, i.e., multiply by $b ** $y

    # set up parameters
    my ($class, $x, $y, $b, @r)
      = ref($_[0]) && ref($_[0]) eq ref($_[1]) && ref($_[1]) eq ref($_[2])
      ? (ref($_[0]), @_)
      : objectify(2, @_);

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('blsft');

    return $x -> bnan(@r) if $x -> is_nan() || $y -> is_nan();

    $b = 2 if !defined $b;
    $b = $class -> new($b)
      unless defined(blessed($b)) && $b -> isa(__PACKAGE__);
    return $x -> bnan(@r) if $b -> is_nan();

    # There needs to be more checking for special cases here. Fixme!

    # shift by a negative amount?
    return $x -> brsft($y -> copy() -> babs(), $b) if $y -> {sign} =~ /^-/;

    $x = $x -> bmul($b -> bpow($y), $r[0], $r[1], $r[2], $y);

    $x -> round(@r);
    $x -> _dng() if ($x -> is_int() ||
                     $x -> is_inf() ||
                     $x -> is_nan());
    return $x;
}

sub brsft {
    # shift right by $y in base $b, i.e., divide by $b ** $y

    # set up parameters
    my ($class, $x, $y, $b, @r)
      = ref($_[0]) && ref($_[0]) eq ref($_[1]) && ref($_[1]) eq ref($_[2])
      ? (ref($_[0]), @_)
      : objectify(2, @_);

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('brsft');

    return $x -> bnan(@r) if $x -> is_nan() || $y -> is_nan();

    # There needs to be more checking for special cases here. Fixme!

    $b = 2 if !defined $b;
    $b = $class -> new($b)
      unless defined(blessed($b)) && $b -> isa(__PACKAGE__);
    return $x -> bnan(@r) if $b -> is_nan();

    # shift by a negative amount?
    return $x -> blsft($y -> copy() -> babs(), $b) if $y -> {sign} =~ /^-/;

    # call bdiv()
    $x = $x -> bdiv($b -> bpow($y), $r[0], $r[1], $r[2], $y);

    $x -> round(@r);
    $x -> _dng() if ($x -> is_int() ||
                     $x -> is_inf() ||
                     $x -> is_nan());
    return $x;
}

###############################################################################
# Bitwise methods
###############################################################################

# Bitwise left shift.

sub bblsft {
    # We don't call objectify(), because the bitwise methods should not
    # upgrade, even when upgrading is enabled.

    my ($class, $x, $y, @r) = ref($_[0]) ? (ref($_[0]), @_) : @_;

    # Don't modify constant (read-only) objects.

    return $x if ref($x) && $x -> modify('bblsft');

    # Let Math::BigInt do the job.

    my $xint = Math::BigInt -> bblsft($x, $y, @r);

    # Temporarily disable downgrading.

    my $dng = $class -> downgrade();
    $class -> downgrade(undef);

    # convert to our class without downgrading.

    my $xflt = $class -> new($xint);

    # Reset downgrading.

    $class -> downgrade($dng);

    # If we are called as a class method, the first operand might not be an
    # object of this class, so check.

    if (defined(blessed($x)) && $x -> isa(__PACKAGE__)) {
        $x -> {sign} = $xflt -> {sign};
        $x -> {_m}   = $xflt -> {_m};
        $x -> {_es}  = $xflt -> {_es};
        $x -> {_e}   = $xflt -> {_e};
    } else {
        $x = $xflt;
    }

    # Now we might downgrade.

    $x -> round(@r);
    $x -> _dng();
    return $x;
}

# Bitwise right shift.

sub bbrsft {
    # We don't call objectify(), because the bitwise methods should not
    # upgrade, even when upgrading is enabled.

    my ($class, $x, $y, @r) = ref($_[0]) ? (ref($_[0]), @_) : @_;

    # Don't modify constant (read-only) objects.

    return $x if ref($x) && $x -> modify('bbrsft');

    # Let Math::BigInt do the job.

    my $xint = Math::BigInt -> bbrsft($x, $y, @r);

    # Temporarily disable downgrading.

    my $dng = $class -> downgrade();
    $class -> downgrade(undef);

    # Convert to our class without downgrading.

    my $xflt = $class -> new($xint);

    # Reset downgrading.

    $class -> downgrade($dng);

    # If we are called as a class method, the first operand might not be an
    # object of this class, so check.

    if (defined(blessed($x)) && $x -> isa(__PACKAGE__)) {
        $x -> {sign} = $xflt -> {sign};
        $x -> {_m}   = $xflt -> {_m};
        $x -> {_es}  = $xflt -> {_es};
        $x -> {_e}   = $xflt -> {_e};
    } else {
        $x = $xflt;
    }

    # Now we might downgrade.

    $x -> round(@r);
    $x -> _dng();
    return $x;
}

sub band {
    my ($class, $x, $y, @r) = ref($_[0]) && ref($_[0]) eq ref($_[1])
                            ? (ref($_[0]), @_)
                            : objectify(2, @_);

    # Don't modify constant (read-only) objects.

    return if $x -> modify('band');

    # If $x and/or $y is Inf or NaN, return NaN.

    return $x -> bnan(@r) if ($x -> is_nan() || $x -> is_inf() ||
                              $y -> is_nan() || $y -> is_inf());

    # This should be implemented without converting to Math::BigInt. XXX

    my $xint = $x -> as_int();          # to Math::BigInt
    my $yint = $y -> as_int();          # to Math::BigInt

    $xint -> band($yint);
    $xint -> round(@r);

    my $xflt = $xint -> as_float();
    $x -> {sign} = $xflt -> {sign};
    $x -> {_m}   = $xflt -> {_m};
    $x -> {_es}  = $xflt -> {_es};
    $x -> {_e}   = $xflt -> {_e};

    return $x -> _dng();
    return $x;
}

sub bior {
    my ($class, $x, $y, @r) = ref($_[0]) && ref($_[0]) eq ref($_[1])
                            ? (ref($_[0]), @_)
                            : objectify(2, @_);

    # Don't modify constant (read-only) objects.

    return if $x -> modify('bior');

    # If $x and/or $y is Inf or NaN, return NaN.

    return $x -> bnan(@r) if ($x -> is_nan() || $x -> is_inf() ||
                              $y -> is_nan() || $y -> is_inf());

    # This should be implemented without converting to Math::BigInt. XXX

    my $xint = $x -> as_int();          # to Math::BigInt
    my $yint = $y -> as_int();          # to Math::BigInt

    $xint -> bior($yint);
    $xint -> round(@r);

    my $xflt = $xint -> as_float();
    $x -> {sign} = $xflt -> {sign};
    $x -> {_m}   = $xflt -> {_m};
    $x -> {_es}  = $xflt -> {_es};
    $x -> {_e}   = $xflt -> {_e};

    return $x -> _dng();
    return $x;
}

sub bxor {
    my ($class, $x, $y, @r) = ref($_[0]) && ref($_[0]) eq ref($_[1])
                            ? (ref($_[0]), @_)
                            : objectify(2, @_);

    # Don't modify constant (read-only) objects.

    return if $x -> modify('bxor');

    # If $x and/or $y is Inf or NaN, return NaN.

    return $x -> bnan(@r) if ($x -> is_nan() || $x -> is_inf() ||
                              $y -> is_nan() || $y -> is_inf());

    # This should be implemented without converting to Math::BigInt. XXX

    my $xint = $x -> as_int();          # to Math::BigInt
    my $yint = $y -> as_int();          # to Math::BigInt

    $xint -> bxor($yint);
    $xint -> round(@r);

    my $xflt = $xint -> as_float();
    $x -> {sign} = $xflt -> {sign};
    $x -> {_m}   = $xflt -> {_m};
    $x -> {_es}  = $xflt -> {_es};
    $x -> {_e}   = $xflt -> {_e};

    return $x -> _dng();
    return $x;
}

sub bnot {
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    # Don't modify constant (read-only) objects.

    return if $x -> modify('bnot');

    return $x -> bnan(@r) if $x -> is_nan();

    # This should be implemented without converting to Math::BigInt. XXX

    my $xint = $x -> as_int();          # to Math::BigInt

    $xint -> bnot();
    $xint -> round(@r);

    my $xflt = $xint -> as_float();
    $x -> {sign} = $xflt -> {sign};
    $x -> {_m}   = $xflt -> {_m};
    $x -> {_es}  = $xflt -> {_es};
    $x -> {_e}   = $xflt -> {_e};

    return $x -> _dng();
    return $x;
}

###############################################################################
# Rounding methods
###############################################################################

sub bround {
    # accuracy: preserve $N digits, and overwrite the rest with 0's

    my ($class, $x, @a) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    if (($a[0] || 0) < 0) {
        croak('bround() needs positive accuracy');
    }

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('bround');

    my ($scale, $mode) = $x->_scale_a(@a);
    if (!defined $scale) {         # no-op
        $x -> _dng() if ($x -> is_int() ||
                         $x -> is_inf() ||
                         $x -> is_nan());
        return $x;
    }

    # Scale is now either $x->{accuracy}, $accuracy, or the input argument.
    # Test whether $x already has lower accuracy, do nothing in this case but
    # do round if the accuracy is the same, since a math operation might want
    # to round a number with A=5 to 5 digits afterwards again

    if (defined $x->{accuracy} && $x->{accuracy} < $scale) {
        $x -> _dng() if ($x -> is_int() ||
                         $x -> is_inf() ||
                         $x -> is_nan());
        return $x;
    }

    # scale < 0 makes no sense
    # scale == 0 => keep all digits
    # never round a +-inf, NaN

    if ($scale <= 0 || $x->{sign} !~ /^[+-]$/) {
        $x -> _dng() if ($x -> is_int() ||
                         $x -> is_inf() ||
                         $x -> is_nan());
        return $x;
    }

    # 1: never round a 0
    # 2: if we should keep more digits than the mantissa has, do nothing
    if ($x -> is_zero() || $LIB->_len($x->{_m}) <= $scale) {
        $x->{accuracy} = $scale if !defined $x->{accuracy} || $x->{accuracy} > $scale;
        $x -> _dng() if $x -> is_int();
        return $x;
    }

    # pass sign to bround for '+inf' and '-inf' rounding modes
    my $m = bless { sign => $x->{sign}, value => $x->{_m} }, 'Math::BigInt';

    $m = $m -> bround($scale, $mode);   # round mantissa
    $x->{_m} = $m->{value};             # get our mantissa back
    $x->{accuracy} = $scale;            # remember rounding
    $x->{precision} = undef;            # and clear P

    # bnorm() downgrades if necessary, so no need to check whether to
    # downgrade.
    $x -> bnorm();                # del trailing zeros gen. by bround()
}

sub bfround {
    # precision: round to the $Nth digit left (+$n) or right (-$n) from the '.'
    # $n == 0 means round to integer
    # expects and returns normalized numbers!

    my ($class, $x, @p) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('bfround'); # no-op

    my ($scale, $mode) = $x->_scale_p(@p);
    if (!defined $scale) {
        $x -> _dng() if ($x -> is_int() ||
                         $x -> is_inf() ||
                         $x -> is_nan());
        return $x;
    }

    # never round a 0, +-inf, NaN

    if ($x -> is_zero()) {
        $x->{precision} = $scale if !defined $x->{precision} || $x->{precision} < $scale; # -3 < -2
        $x -> _dng() if ($x -> is_int() ||
                         $x -> is_inf() ||
                         $x -> is_nan());
        return $x;
    }

    if ($x->{sign} !~ /^[+-]$/) {
        $x -> _dng() if ($x -> is_int() ||
                         $x -> is_inf() ||
                         $x -> is_nan());
        return $x;
    }

    # don't round if x already has lower precision
    if (defined $x->{precision} && $x->{precision} < 0 && $scale < $x->{precision}) {
        $x -> _dng() if ($x -> is_int() ||
                         $x -> is_inf() ||
                         $x -> is_nan());
        return $x;
    }

    $x->{precision} = $scale;           # remember round in any case
    $x->{accuracy} = undef;             # and clear A
    if ($scale < 0) {
        # round right from the '.'

        if ($x->{_es} eq '+') { # e >= 0 => nothing to round
            $x -> _dng() if ($x -> is_int() ||
                             $x -> is_inf() ||
                             $x -> is_nan());
            return $x;
        }

        $scale = -$scale;           # positive for simplicity
        my $len = $LIB->_len($x->{_m}); # length of mantissa

        # the following poses a restriction on _e, but if _e is bigger than a
        # scalar, you got other problems (memory etc) anyway
        my $dad = -(0+ ($x->{_es}.$LIB->_num($x->{_e}))); # digits after dot
        my $zad = 0;                                      # zeros after dot
        $zad = $dad - $len if (-$dad < -$len); # for 0.00..00xxx style

        # print "scale $scale dad $dad zad $zad len $len\n";
        # number  bsstr   len zad dad
        # 0.123   123e-3    3   0 3
        # 0.0123  123e-4    3   1 4
        # 0.001   1e-3      1   2 3
        # 1.23    123e-2    3   0 2
        # 1.2345  12345e-4  5   0 4

        # do not round after/right of the $dad

        if ($scale > $dad) { # 0.123, scale >= 3 => exit
            $x -> _dng() if ($x -> is_int() ||
                             $x -> is_inf() ||
                             $x -> is_nan());
            return $x;
        }

        # round to zero if rounding inside the $zad, but not for last zero like:
        # 0.0065, scale -2, round last '0' with following '65' (scale == zad
        # case)
        if ($scale < $zad) {
            $x -> _dng() if ($x -> is_int() ||
                             $x -> is_inf() ||
                             $x -> is_nan());
            return $x -> bzero();
        }

        if ($scale == $zad) {    # for 0.006, scale -3 and trunc
            $scale = -$len;
        } else {
            # adjust round-point to be inside mantissa
            if ($zad != 0) {
                $scale = $scale-$zad;
            } else {
                my $dbd = $len - $dad;
                $dbd = 0 if $dbd < 0; # digits before dot
                $scale = $dbd+$scale;
            }
        }
    } else {
        # round left from the '.'

        # 123 => 100 means length(123) = 3 - $scale (2) => 1

        my $dbt = $LIB->_len($x->{_m});
        # digits before dot
        my $dbd = $dbt + ($x->{_es} . $LIB->_num($x->{_e}));
        # should be the same, so treat it as this
        $scale = 1 if $scale == 0;
        # shortcut if already integer
        if ($scale == 1 && $dbt <= $dbd) {
            $x -> _dng() if ($x -> is_int() ||
                             $x -> is_inf() ||
                             $x -> is_nan());
            return $x;
        }
        # maximum digits before dot
        ++$dbd;

        if ($scale > $dbd) {
            # not enough digits before dot, so round to zero
            return $x -> bzero;
        } elsif ($scale == $dbd) {
            # maximum
            $scale = -$dbt;
        } else {
            $scale = $dbd - $scale;
        }
    }

    # pass sign to bround for rounding modes '+inf' and '-inf'
    my $m = bless { sign => $x->{sign}, value => $x->{_m} }, 'Math::BigInt';
    $m = $m -> bround($scale, $mode);
    $x->{_m} = $m->{value};     # get our mantissa back

    # bnorm() downgrades if necessary, so no need to check whether to
    # downgrade.
    $x -> bnorm();
}

sub bfloor {
    # round towards minus infinity
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('bfloor');

    return $x -> bnan(@r) if $x -> is_nan();

    if ($x -> is_finite()) {
        # if $x has digits after dot, remove them
        if ($x->{_es} eq '-') {
            $x->{_m} = $LIB->_rsft($x->{_m}, $x->{_e}, 10);
            $x->{_e} = $LIB->_zero();
            $x->{_es} = '+';
            # increment if negative
            $x->{_m} = $LIB->_inc($x->{_m}) if $x->{sign} eq '-';
        }
    }

    $x -> round(@r);
    $x -> _dng();
    return $x;
}

sub bceil {
    # round towards plus infinity
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('bceil');

    return $x -> bnan(@r) if $x -> is_nan();

    if ($x -> is_finite()) {
        # if $x has digits after dot, remove them
        if ($x->{_es} eq '-') {
            $x->{_m} = $LIB->_rsft($x->{_m}, $x->{_e}, 10);
            $x->{_e} = $LIB->_zero();
            $x->{_es} = '+';
            if ($x->{sign} eq '+') {
                $x->{_m} = $LIB->_inc($x->{_m});        # increment if positive
            } else {
                $x->{sign} = '+' if $LIB->_is_zero($x->{_m});   # avoid -0
            }
        }
    }

    $x -> round(@r);
    $x -> _dng();
    return $x;
}

sub bint {
    # round towards zero
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('bint');

    return $x -> bnan(@r) if $x -> is_nan();

    if ($x -> is_finite()) {
        # if $x has digits after the decimal point
        if ($x->{_es} eq '-') {
            $x->{_m} = $LIB->_rsft($x->{_m}, $x->{_e}, 10); # remove frac part
            $x->{_e} = $LIB->_zero();                       # truncate/normalize
            $x->{_es} = '+';                                # abs e
            $x->{sign} = '+' if $LIB->_is_zero($x->{_m});   # avoid -0
        }
    }

    $x -> round(@r);
    $x -> _dng();
    return $x;
}

###############################################################################
# Other mathematical methods
###############################################################################

sub bgcd {
    # GCD -- Euclid's algorithm, variant C (Knuth Vol 3, pg 341 ff)

    # Class::method(...) -> Class->method(...)
    unless (@_ && (defined(blessed($_[0])) && $_[0] -> isa(__PACKAGE__) ||
                   ($_[0] =~ /^[a-z]\w*(?:::[a-z]\w*)*$/i &&
                    $_[0] !~ /^(inf|nan)/i)))
    {
        #carp "Using ", (caller(0))[3], "() as a function is deprecated;",
        #  " use is as a method instead";
        unshift @_, __PACKAGE__;
    }

    my ($class, @args) = objectify(0, @_);

    # Pre-process list of operands.

    for my $arg (@args) {
        return $class -> bnan() unless $arg -> is_finite();
    }

    # Temporarily disable downgrading.

    my $dng = $class -> downgrade();
    $class -> downgrade(undef);

    my $x = shift @args;
    $x = $x -> copy();          # bgcd() and blcm() never modify any operands

    while (@args) {
        my $y = shift @args;

        # greatest common divisor
        while (! $y -> is_zero()) {
            ($x, $y) = ($y -> copy(), $x -> copy() -> bmod($y));
        }

        last if $x -> is_one();
    }
    $x -> babs();

    # Restore downgrading.

    $class -> downgrade($dng);

    $x -> _dng() if $x -> is_int();
    return $x;
}

sub blcm {
    # Least Common Multiple

    # Class::method(...) -> Class->method(...)
    unless (@_ && (defined(blessed($_[0])) && $_[0] -> isa(__PACKAGE__) ||
                   ($_[0] =~ /^[a-z]\w*(?:::[a-z]\w*)*$/i &&
                    $_[0] !~ /^(inf|nan)/i)))
    {
        #carp "Using ", (caller(0))[3], "() as a function is deprecated;",
        #  " use is as a method instead";
        unshift @_, __PACKAGE__;
    }

    my ($class, @args) = objectify(0, @_);

    # Pre-process list of operands.

    for my $arg (@args) {
        return $class -> bnan() unless $arg -> is_finite();
    }

    for my $arg (@args) {
        return $class -> bzero() if $arg -> is_zero();
    }

    my $x = shift @args;
    $x = $x -> copy();          # bgcd() and blcm() never modify any operands

    while (@args) {
        my $y = shift @args;
        my $gcd = $x -> copy() -> bgcd($y);
        $x -> bdiv($gcd) -> bmul($y);
    }

    $x -> babs();       # might downgrade
    return $x;
}

###############################################################################
# Object property methods
###############################################################################

sub length {
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    return 1 if $LIB->_is_zero($x->{_m});

    my $len = $LIB->_len($x->{_m});
    $len += $LIB->_num($x->{_e}) if $x->{_es} eq '+';
    if (wantarray()) {
        my $t = 0;
        $t = $LIB->_num($x->{_e}) if $x->{_es} eq '-';
        return $len, $t;
    }
    $len;
}

sub mantissa {
    # return a copy of the mantissa
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    # The following line causes a lot of noise in the test suits for
    # the Math-BigRat and bignum distributions. Fixme!
    #carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    return $x -> bnan(@r) if $x -> is_nan();

    if ($x->{sign} !~ /^[+-]$/) {
        my $s = $x->{sign};
        $s =~ s/^\+//;
        return Math::BigInt -> new($s, undef, undef); # -inf, +inf => +inf
    }
    my $m = Math::BigInt -> new($LIB->_str($x->{_m}), undef, undef);
    $m = $m -> bneg() if $x->{sign} eq '-';
    $m;
}

sub exponent {
    # return a copy of the exponent
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    # The following line causes a lot of noise in the test suits for
    # the Math-BigRat and bignum distributions. Fixme!
    #carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    return $x -> bnan(@r) if $x -> is_nan();

    if ($x->{sign} !~ /^[+-]$/) {
        my $s = $x->{sign};
        $s =~ s/^[+-]//;
        return Math::BigInt -> new($s, undef, undef); # -inf, +inf => +inf
    }
    Math::BigInt -> new($x->{_es} . $LIB->_str($x->{_e}), undef, undef);
}

sub parts {
    # return a copy of both the exponent and the mantissa
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    if ($x->{sign} !~ /^[+-]$/) {
        my $s = $x->{sign};
        $s =~ s/^\+//;
        my $se = $s;
        $se =~ s/^-//;
        # +inf => inf and -inf, +inf => inf
        return $class -> new($s), $class -> new($se);
    }
    my $m = Math::BigInt -> bzero();
    $m->{value} = $LIB->_copy($x->{_m});
    $m = $m -> bneg() if $x->{sign} eq '-';
    ($m, Math::BigInt -> new($x->{_es} . $LIB->_num($x->{_e})));
}

# Parts used for scientific notation with significand/mantissa and exponent as
# integers. E.g., "12345.6789" is returned as "123456789" (mantissa) and "-4"
# (exponent).

sub sparts {
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    # Not-a-number.

    if ($x -> is_nan()) {
        my $mant = $class -> bnan();            # mantissa
        return $mant unless wantarray;          # scalar context
        my $expo = $class -> bnan();            # exponent
        return $mant, $expo;                    # list context
    }

    # Infinity.

    if ($x -> is_inf()) {
        my $mant = $class -> binf($x->{sign});  # mantissa
        return $mant unless wantarray;          # scalar context
        my $expo = $class -> binf('+');         # exponent
        return $mant, $expo;                    # list context
    }

    # Finite number.

    my $mant = $class -> new($x);
    $mant->{_es} = '+';
    $mant->{_e}  = $LIB->_zero();
    $mant -> _dng();
    return $mant unless wantarray;

    my $expo = $class -> new($x -> {_es} . $LIB->_str($x -> {_e}));
    $expo -> _dng();
    return $mant, $expo;
}

# Parts used for normalized notation with significand/mantissa as either 0 or a
# number in the semi-open interval [1,10). E.g., "12345.6789" is returned as
# "1.23456789" and "4".

sub nparts {
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    # Not-a-number and Infinity.

    return $x -> sparts() if $x -> is_nan() || $x -> is_inf();

    # Finite number.

    my ($mant, $expo) = $x -> sparts();

    if ($mant -> bcmp(0)) {
        my ($ndigtot, $ndigfrac) = $mant -> length();
        my $expo10adj = $ndigtot - $ndigfrac - 1;

        if ($expo10adj > 0) {          # if mantissa is not an integer
            $mant = $mant -> brsft($expo10adj, 10);
            return $mant unless wantarray;
            $expo = $expo -> badd($expo10adj);
            return $mant, $expo;
        }
    }

    return $mant unless wantarray;
    return $mant, $expo;
}

# Parts used for engineering notation with significand/mantissa as either 0 or
# a number in the semi-open interval [1,1000) and the exponent is a multiple of
# 3. E.g., "12345.6789" is returned as "12.3456789" and "3".

sub eparts {
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    # Not-a-number and Infinity.

    return $x -> sparts() if $x -> is_nan() || $x -> is_inf();

    # Finite number.

    my ($mant, $expo) = $x -> nparts();

    my $c = $expo -> copy() -> bmod(3);
    $mant = $mant -> blsft($c, 10);
    return $mant unless wantarray;

    $expo = $expo -> bsub($c);
    return $mant, $expo;
}

# Parts used for decimal notation, e.g., "12345.6789" is returned as "12345"
# (integer part) and "0.6789" (fraction part).

sub dparts {
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    # Not-a-number.

    if ($x -> is_nan()) {
        my $int = $class -> bnan();
        return $int unless wantarray;
        my $frc = $class -> bzero();    # or NaN?
        return $int, $frc;
    }

    # Infinity.

    if ($x -> is_inf()) {
        my $int = $class -> binf($x->{sign});
        return $int unless wantarray;
        my $frc = $class -> bzero();
        return $int, $frc;
    }

    # Finite number.

    my $int = $x -> copy();
    my $frc;

    # If the input is an integer.

    if ($int->{_es} eq '+') {
        $frc = $class -> bzero();
    }

    # If the input has a fraction part

    else {
        $int->{_m} = $LIB -> _rsft($int->{_m}, $int->{_e}, 10);
        $int->{_e} = $LIB -> _zero();
        $int->{_es} = '+';
        $int->{sign} = '+' if $LIB->_is_zero($int->{_m});   # avoid -0
        return $int unless wantarray;
        $frc = $x -> copy() -> bsub($int);
        return $int, $frc;
    }

    $int -> _dng();
    return $int unless wantarray;
    return $int, $frc;
}

# Fractional parts with the numerator and denominator as integers. E.g.,
# "123.4375" is returned as "1975" and "16".

sub fparts {
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    # NaN => NaN/NaN

    if ($x -> is_nan()) {
        return $class -> bnan() unless wantarray;
        return $class -> bnan(), $class -> bnan();
    }

    # ±Inf => ±Inf/1

    if ($x -> is_inf()) {
        my $numer = $class -> binf($x->{sign});
        return $numer unless wantarray;
        my $denom = $class -> bone();
        return $numer, $denom;
    }

    # Finite number.

    # If we get here, we know that the output is an integer.

    $class = $downgrade if $class -> downgrade();

    my @flt_parts = ($x->{sign}, $x->{_m}, $x->{_es}, $x->{_e});
    my @rat_parts = $class -> _flt_lib_parts_to_rat_lib_parts(@flt_parts);
    my $numer = $class -> new($LIB -> _str($rat_parts[1]));
    $numer -> bneg() if $rat_parts[0] eq "-";
    return $numer unless wantarray;

    my $denom = $class -> new($LIB -> _str($rat_parts[2]));
    return $numer, $denom;
}

# Given "123.4375", returns "1975", since "123.4375" is "1975/16".

sub numerator {
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    return $class -> bnan()             if $x -> is_nan();
    return $class -> binf($x -> sign()) if $x -> is_inf();
    return $class -> bzero()            if $x -> is_zero();

    # If we get here, we know that the output is an integer.

    $class = $downgrade if $class -> downgrade();

    if ($x -> {_es} eq '-') {                   # exponent < 0
        my $numer_lib = $LIB -> _copy($x -> {_m});
        my $denom_lib = $LIB -> _1ex($x -> {_e});
        my $gcd_lib = $LIB -> _gcd($LIB -> _copy($numer_lib), $denom_lib);
        $numer_lib = $LIB -> _div($numer_lib, $gcd_lib);
        return $class -> new($x -> {sign} . $LIB -> _str($numer_lib));
    }

    elsif (! $LIB -> _is_zero($x -> {_e})) {    # exponent > 0
        my $numer_lib = $LIB -> _copy($x -> {_m});
        $numer_lib = $LIB -> _lsft($numer_lib, $x -> {_e}, 10);
        return $class -> new($x -> {sign} . $LIB -> _str($numer_lib));
    }

    else {                                      # exponent = 0
        return $class -> new($x -> {sign} . $LIB -> _str($x -> {_m}));
    }
}

# Given "123.4375", returns "16", since "123.4375" is "1975/16".

sub denominator {
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    return $class -> bnan() if $x -> is_nan();

    # If we get here, we know that the output is an integer.

    $class = $downgrade if $class -> downgrade();

    if ($x -> {_es} eq '-') {                   # exponent < 0
        my $numer_lib = $LIB -> _copy($x -> {_m});
        my $denom_lib = $LIB -> _1ex($x -> {_e});
        my $gcd_lib = $LIB -> _gcd($LIB -> _copy($numer_lib), $denom_lib);
        $denom_lib = $LIB -> _div($denom_lib, $gcd_lib);
        return $class -> new($LIB -> _str($denom_lib));
    }

    else {                                      # exponent >= 0
        return $class -> bone();
    }
}

###############################################################################
# String conversion methods
###############################################################################

sub bstr {
    # (ref to BFLOAT or num_str) return num_str
    # Convert number from internal format to (non-scientific) string format.
    # internal format is always normalized (no leading zeros, "-0" => "+0")
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    # Inf and NaN

    if ($x->{sign} ne '+' && $x->{sign} ne '-') {
        return $x->{sign} unless $x -> is_inf("+");     # -inf, NaN
        return 'inf';                                   # +inf
    }

    # Finite number

    my $es = '0';
    my $len = 1;
    my $cad = 0;
    my $dot = '.';

    # $x is zero?
    my $not_zero = !($x->{sign} eq '+' && $LIB->_is_zero($x->{_m}));
    if ($not_zero) {
        $es = $LIB->_str($x->{_m});
        $len = CORE::length($es);
        my $e = $LIB->_num($x->{_e});
        $e = -$e if $x->{_es} eq '-';
        if ($e < 0) {
            $dot = '';
            # if _e is bigger than a scalar, the following will blow your memory
            if ($e <= -$len) {
                my $r = abs($e) - $len;
                $es = '0.'. ('0' x $r) . $es;
                $cad = -($len+$r);
            } else {
                substr($es, $e, 0) = '.';
                $cad = $LIB->_num($x->{_e});
                $cad = -$cad if $x->{_es} eq '-';
            }
        } elsif ($e > 0) {
            # expand with zeros
            $es .= '0' x $e;
            $len += $e;
            $cad = 0;
        }
    }                           # if not zero

    $es = '-'.$es if $x->{sign} eq '-';
    # if set accuracy or precision, pad with zeros on the right side
    if ((defined $x->{accuracy}) && ($not_zero)) {
        # 123400 => 6, 0.1234 => 4, 0.001234 => 4
        my $zeros = $x->{accuracy} - $cad; # cad == 0 => 12340
        $zeros = $x->{accuracy} - $len if $cad != $len;
        $es .= $dot.'0' x $zeros if $zeros > 0;
    } elsif ((($x->{precision} || 0) < 0)) {
        # 123400 => 6, 0.1234 => 4, 0.001234 => 6
        my $zeros = -$x->{precision} + $cad;
        $es .= $dot.'0' x $zeros if $zeros > 0;
    }
    $es;
}

# Scientific notation with significand/mantissa and exponent as integers, e.g.,
# "12345.6789" is written as "123456789e-4".

sub bsstr {
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    # Inf and NaN

    if ($x->{sign} ne '+' && $x->{sign} ne '-') {
        return $x->{sign} unless $x -> is_inf("+");     # -inf, NaN
        return 'inf';                                   # +inf
    }

    # Upgrade?

    return $x -> _upg() -> bsstr(@r)
      if $class -> upgrade() && !$x -> isa(__PACKAGE__);

    # Round according to arguments or global settings, if any.

    $x = $x -> copy() -> round(@r);

    # Finite number

    ($x->{sign} eq '-' ? '-' : '') . $LIB->_str($x->{_m})
      . 'e' . $x->{_es} . $LIB->_str($x->{_e});
}

# Normalized notation, e.g., "12345.6789" is written as "1.23456789e+4".

sub bnstr {
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    # Inf and NaN

    if ($x->{sign} ne '+' && $x->{sign} ne '-') {
        return $x->{sign} unless $x -> is_inf("+");     # -inf, NaN
        return 'inf';                                   # +inf
    }

    # Upgrade?

    return $x -> _upg() -> bnstr(@r)
      if $class -> upgrade() && !$x -> isa(__PACKAGE__);

    # Finite number

    my $str = $x->{sign} eq '-' ? '-' : '';

    # Round according to arguments or global settings, if any.

    $x = $x -> copy() -> round(@r);

    # Get the mantissa and the length of the mantissa.

    my $mant = $LIB->_str($x->{_m});
    my $mantlen = CORE::length($mant);

    if ($mantlen == 1) {

        # Not decimal point when the mantissa has length one, i.e., return the
        # number 2 as the string "2", not "2.".

        $str .= $mant . 'e' . $x->{_es} . $LIB->_str($x->{_e});

    } else {

        # Compute new exponent where the original exponent is adjusted by the
        # length of the mantissa minus one (because the decimal point is after
        # one digit).

        my ($eabs, $esgn) = $LIB -> _sadd($LIB -> _copy($x->{_e}), $x->{_es},
                                      $LIB -> _new($mantlen - 1), "+");
        substr $mant, 1, 0, ".";
        $str .= $mant . 'e' . $esgn . $LIB->_str($eabs);

    }

    return $str;
}

# Engineering notation, e.g., "12345.6789" is written as "12.3456789e+3".

sub bestr {
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    # Inf and NaN

    if ($x->{sign} ne '+' && $x->{sign} ne '-') {
        return $x->{sign} unless $x -> is_inf("+");     # -inf, NaN
        return 'inf';                                   # +inf
    }

    # Upgrade?

    return $x -> _upg() -> bestr(@r)
      if $class -> upgrade() && !$x -> isa(__PACKAGE__);

    # Round according to arguments or global settings, if any.

    $x = $x -> copy() -> round(@r);

    # Finite number

    my $str = $x->{sign} eq '-' ? '-' : '';

    # Get the mantissa, the length of the mantissa, and adjust the exponent by
    # the length of the mantissa minus 1 (because the dot is after one digit).

    my $mant = $LIB->_str($x->{_m});
    my $mantlen = CORE::length($mant);
    my ($eabs, $esgn) = $LIB -> _sadd($LIB -> _copy($x->{_e}), $x->{_es},
                                  $LIB -> _new($mantlen - 1), "+");

    my $dotpos = 1;
    my $mod = $LIB -> _mod($LIB -> _copy($eabs), $LIB -> _new("3"));
    unless ($LIB -> _is_zero($mod)) {
        if ($esgn eq '+') {
            $eabs = $LIB -> _sub($eabs, $mod);
            $dotpos += $LIB -> _num($mod);
        } else {
            my $delta = $LIB -> _sub($LIB -> _new("3"), $mod);
            $eabs = $LIB -> _add($eabs, $delta);
            $dotpos += $LIB -> _num($delta);
        }
    }

    if ($dotpos < $mantlen) {
        substr $mant, $dotpos, 0, ".";
    } elsif ($dotpos > $mantlen) {
        $mant .= "0" x ($dotpos - $mantlen);
    }

    $str .= $mant . 'e' . $esgn . $LIB->_str($eabs);

    return $str;
}

# Decimal notation, e.g., "12345.6789" (no exponent).

sub bdstr {
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    # Inf and NaN

    if ($x->{sign} ne '+' && $x->{sign} ne '-') {
        return $x->{sign} unless $x -> is_inf("+");     # -inf, NaN
        return 'inf';                                   # +inf
    }

    # Upgrade?

    return $x -> _upg() -> bdstr(@r)
      if $class -> upgrade() && !$x -> isa(__PACKAGE__);

    # Round according to arguments or global settings, if any.

    $x = $x -> copy() -> round(@r);

    # Finite number

    my $mant = $LIB->_str($x->{_m});
    my $esgn = $x->{_es};
    my $eabs = $LIB -> _num($x->{_e});

    my $uintmax = ~0;

    my $str = $mant;
    if ($esgn eq '+') {

        croak("The absolute value of the exponent is too large")
          if $eabs > $uintmax;

        $str .= "0" x $eabs;

    } else {
        my $mlen = CORE::length($mant);
        my $c = $mlen - $eabs;

        my $intmax = ($uintmax - 1) / 2;
        croak("The absolute value of the exponent is too large")
          if (1 - $c) > $intmax;

        $str = "0" x (1 - $c) . $str if $c <= 0;
        substr($str, -$eabs, 0) = '.';
    }

    return $x->{sign} eq '-' ? '-' . $str : $str;
}

# Fractional notation, e.g., "123.4375" is written as "1975/16".

sub bfstr {
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), $_[0]) : objectify(1, @_);

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    # Inf and NaN

    if ($x->{sign} ne '+' && $x->{sign} ne '-') {
        return $x->{sign} unless $x -> is_inf("+");     # -inf, NaN
        return 'inf';                                   # +inf
    }

    # Upgrade?

    return $x -> _upg() -> bfstr(@r)
      if $class -> upgrade() && !$x -> isa(__PACKAGE__);

    # Finite number

    my $str = $x->{sign} eq '-' ? '-' : '';

    if ($x->{_es} eq '+') {
        $str .= $LIB -> _str($x->{_m}) . ("0" x $LIB -> _num($x->{_e}));
    } else {
        my @flt_parts = ($x->{sign}, $x->{_m}, $x->{_es}, $x->{_e});
        my @rat_parts = $class -> _flt_lib_parts_to_rat_lib_parts(@flt_parts);
        $str = $LIB -> _str($rat_parts[1]) . "/" . $LIB -> _str($rat_parts[2]);
        $str = "-" . $str if $rat_parts[0] eq "-";
    }

    return $str;
}

sub to_hex {
    # return number as hexadecimal string (only for integers defined)
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), $_[0]) : objectify(1, @_);

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    # Inf and NaN

    if ($x->{sign} ne '+' && $x->{sign} ne '-') {
        return $x->{sign} unless $x -> is_inf("+");     # -inf, NaN
        return 'inf';                                   # +inf
    }

    # Upgrade?

    return $x -> _upg() -> to_hex(@r)
      if $class -> upgrade() && !$x -> isa(__PACKAGE__);

    # Finite number

    return '0' if $x -> is_zero();

    return $nan if $x->{_es} ne '+';    # how to do 1e-1 in hex?

    my $z = $LIB->_copy($x->{_m});
    if (! $LIB->_is_zero($x->{_e})) {   # > 0
        $z = $LIB->_lsft($z, $x->{_e}, 10);
    }
    my $str = $LIB->_to_hex($z);
    return $x->{sign} eq '-' ? "-$str" : $str;
}

sub to_oct {
    # return number as octal digit string (only for integers defined)
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), $_[0]) : objectify(1, @_);

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    # Inf and NaN

    if ($x->{sign} ne '+' && $x->{sign} ne '-') {
        return $x->{sign} unless $x -> is_inf("+");     # -inf, NaN
        return 'inf';                                   # +inf
    }

    # Upgrade?

    return $x -> _upg() -> to_oct(@r)
      if $class -> upgrade() && !$x -> isa(__PACKAGE__);

    # Finite number

    return '0' if $x -> is_zero();

    return $nan if $x->{_es} ne '+';    # how to do 1e-1 in octal?

    my $z = $LIB->_copy($x->{_m});
    if (! $LIB->_is_zero($x->{_e})) {   # > 0
        $z = $LIB->_lsft($z, $x->{_e}, 10);
    }
    my $str = $LIB->_to_oct($z);
    return $x->{sign} eq '-' ? "-$str" : $str;
}

sub to_bin {
    # return number as binary digit string (only for integers defined)
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), $_[0]) : objectify(1, @_);

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    # Inf and NaN

    if ($x->{sign} ne '+' && $x->{sign} ne '-') {
        return $x->{sign} unless $x -> is_inf("+");     # -inf, NaN
        return 'inf';                                   # +inf
    }

    # Upgrade?

    return $x -> _upg() -> to_bin(@r)
      if $class -> upgrade() && !$x -> isa(__PACKAGE__);

    # Finite number

    return '0' if $x -> is_zero();

    return $nan if $x->{_es} ne '+';    # how to do 1e-1 in binary?

    my $z = $LIB->_copy($x->{_m});
    if (! $LIB->_is_zero($x->{_e})) {   # > 0
        $z = $LIB->_lsft($z, $x->{_e}, 10);
    }
    my $str = $LIB->_to_bin($z);
    return $x->{sign} eq '-' ? "-$str" : $str;
}

sub to_bytes {
    # return a byte string

    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    croak("to_bytes() requires a finite, non-negative integer")
        if $x -> is_neg() || ! $x -> is_int();

    return $x -> _upg() -> to_bytes(@r)
      if $class -> upgrade() && !$x -> isa(__PACKAGE__);

    croak("to_bytes() requires a newer version of the $LIB library.")
        unless $LIB -> can('_to_bytes');

    return $LIB->_to_bytes($LIB -> _lsft($x->{_m}, $x->{_e}, 10));
}

sub to_ieee754 {
    my ($class, $x, $format, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    my $enc;            # significand encoding (applies only to decimal)
    my $k;              # storage width in bits
    my $b;              # base

    if ($format =~ /^binary(\d+)\z/) {
        $k = $1;
        $b = 2;
    } elsif ($format =~ /^decimal(\d+)(dpd|bcd)?\z/) {
        $k = $1;
        $b = 10;
        $enc = $2 || 'dpd';     # default is dencely-packed decimals (DPD)
    } elsif ($format eq 'half') {
        $k = 16;
        $b = 2;
    } elsif ($format eq 'single') {
        $k = 32;
        $b = 2;
    } elsif ($format eq 'double') {
        $k = 64;
        $b = 2;
    } elsif ($format eq 'quadruple') {
        $k = 128;
        $b = 2;
    } elsif ($format eq 'octuple') {
        $k = 256;
        $b = 2;
    } elsif ($format eq 'sexdecuple') {
        $k = 512;
        $b = 2;
    }

    if ($b == 2) {

        # Get the parameters for this format.

        my $p;                      # precision (in bits)
        my $t;                      # number of bits in significand
        my $w;                      # number of bits in exponent

        if ($k == 16) {             # binary16 (half-precision)
            $p = 11;
            $t = 10;
            $w =  5;
        } elsif ($k == 32) {        # binary32 (single-precision)
            $p = 24;
            $t = 23;
            $w =  8;
        } elsif ($k == 64) {        # binary64 (double-precision)
            $p = 53;
            $t = 52;
            $w = 11;
        } else {                    # binaryN (quadruple-precition and above)
            if ($k < 128 || $k != 32 * sprintf('%.0f', $k / 32)) {
                croak "Number of bits must be 16, 32, 64, or >= 128 and",
                  " a multiple of 32";
            }
            $p = $k - sprintf('%.0f', 4 * log($k) / log(2)) + 13;
            $t = $p - 1;
            $w = $k - $t - 1;
        }

        # The maximum exponent, minimum exponent, and exponent bias.

        my $emax = $class -> new(2) -> bpow($w - 1) -> bdec();
        my $emin = 1 - $emax;
        my $bias = $emax;

        # Get numerical sign, exponent, and mantissa/significand for bit
        # string.

        my $sign = 0;
        my $expo;
        my $mant;

        if ($x -> is_nan()) {                   # nan
            $sign = 1;
            $expo = $emax -> copy() -> binc();
            $mant = $class -> new(2) -> bpow($t - 1);
        } elsif ($x -> is_inf()) {              # inf
            $sign = 1 if $x -> is_neg();
            $expo = $emax -> copy() -> binc();
            $mant = $class -> bzero();
        } elsif ($x -> is_zero()) {             # zero
            $expo = $emin -> copy() -> bdec();
            $mant = $class -> bzero();
        } else {                                # normal and subnormal

            $sign = 1 if $x -> is_neg();

            # Now we need to compute the mantissa and exponent in base $b.

            my $binv = $class -> new("0.5");
            my $b    = $class -> new(2);
            my $one  = $class -> bone();

            # We start off by initializing the exponent to zero and the
            # mantissa to the input value. Then we increase the mantissa and
            # decrease the exponent, or vice versa, until the mantissa is in
            # the desired range or we hit one of the limits for the exponent.

            $mant = $x -> copy() -> babs();

            # We need to find the base 2 exponent. First make an estimate of
            # the base 2 exponent, before adjusting it below. We could skip
            # this estimation and go straight to the while-loops below, but the
            # loops are slow, especially when the final exponent is far from
            # zero and even more so if the number of digits is large. This
            # initial estimation speeds up the computation dramatically.
            #
            #   log2($m * 10**$e) = log10($m + 10**$e) * log(10)/log(2)
            #                     = (log10($m) + $e) * log(10)/log(2)
            #                     = (log($m)/log(10) + $e) * log(10)/log(2)

            my ($m, $e) = $x -> nparts();
            my $ms = $m -> numify();
            my $es = $e -> numify();

            my $expo_est = (log(abs($ms))/log(10) + $es) * log(10)/log(2);
            $expo_est = int($expo_est);

            # Limit the exponent.

            if ($expo_est > $emax) {
                $expo_est = $emax;
            } elsif ($expo_est < $emin) {
                $expo_est = $emin;
            }

            # Don't multiply by a number raised to a negative exponent. This
            # will cause a division, whose result is truncated to some fixed
            # number of digits. Instead, multiply by the inverse number raised
            # to a positive exponent.

            $expo = $class -> new($expo_est);
            if ($expo_est > 0) {
                $mant = $mant -> bmul($binv -> copy() -> bpow($expo));
            } elsif ($expo_est < 0) {
                my $expo_abs = $expo -> copy() -> bneg();
                $mant = $mant -> bmul($b -> copy() -> bpow($expo_abs));
            }

            # Final adjustment of the estimate above.

            while ($mant >= $b && $expo <= $emax) {
                $mant = $mant -> bmul($binv);
                $expo = $expo -> binc();
            }

            while ($mant < $one && $expo >= $emin) {
                $mant = $mant -> bmul($b);
                $expo = $expo -> bdec();
            }

            # This is when the magnitude is larger than what can be represented
            # in this format. Encode as infinity.

            if ($expo > $emax) {
                $mant = $class -> bzero();
                $expo = $emax -> copy() -> binc();
            }

            # This is when the magnitude is so small that the number is encoded
            # as a subnormal number.
            #
            # If the magnitude is smaller than that of the smallest subnormal
            # number, and rounded downwards, it is encoded as zero. This works
            # transparently and does not need to be treated as a special case.
            #
            # If the number is between the largest subnormal number and the
            # smallest normal number, and the value is rounded upwards, the
            # value must be encoded as a normal number. This must be treated as
            # a special case.

            elsif ($expo < $emin) {

                # Scale up the mantissa (significand), and round to integer.

                my $const = $class -> new($b) -> bpow($t - 1);
                $mant = $mant -> bmul($const);
                $mant = $mant -> bfround(0);

                # If the mantissa overflowed, encode as the smallest normal
                # number.

                if ($mant == $const -> bmul($b)) {
                    $mant = $mant -> bzero();
                    $expo = $expo -> binc();
                }
            }

            # This is when the magnitude is within the range of what can be
            # encoded as a normal number.

            else {

                # Remove implicit leading bit, scale up the mantissa
                # (significand) to an integer, and round.

                $mant = $mant -> bdec();
                my $const = $class -> new($b) -> bpow($t);
                $mant = $mant -> bmul($const) -> bfround(0);

                # If the mantissa overflowed, encode as the next larger value.
                # This works correctly also when the next larger value is
                # infinity.

                if ($mant == $const) {
                    $mant = $mant -> bzero();
                    $expo = $expo -> binc();
                }
            }
        }

        $expo = $expo -> badd($bias);           # add bias

        my $signbit = "$sign";

        my $mantbits = $mant -> to_bin();
        $mantbits = ("0" x ($t - CORE::length($mantbits))) . $mantbits;

        my $expobits = $expo -> to_bin();
        $expobits = ("0" x ($w - CORE::length($expobits))) . $expobits;

        my $bin = $signbit . $expobits . $mantbits;
        return pack "B*", $bin;
    }

    croak("The format '$format' is not yet supported.");
}

sub to_fp80 {
    my ($class, $x, $format, @r) = ref($_[0]) ? (ref($_[0]), @_)
                                              : objectify(1, @_);

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    # The maximum exponent, minimum exponent, and exponent bias.

    my $emax = Math::BigFloat -> new("16383");
    my $emin = 1 - $emax;
    my $bias = $emax;

    # Get numerical sign, exponent, and mantissa/significand for bit string.

    my $sign = 0;
    my $expo;
    my $mant;

    if ($x -> is_nan()) {                   # nan
        $sign = 1;
        $expo = $emax -> copy() -> binc();
        $mant = $class -> new(2) -> bpow(64) -> bdec();

    } elsif ($x -> is_inf()) {              # inf
        $sign = 1 if $x -> is_neg();
        $expo = $emax -> copy() -> binc();
        $mant = $class -> bzero();

    } elsif ($x -> is_zero()) {             # zero
        $expo = $emin -> copy() -> bdec();
        $mant = $class -> bzero();

    } else {                                # normal and subnormal

        $sign = 1 if $x -> is_neg();

        # Now we need to compute the mantissa and exponent in base $b.

        my $binv = $class -> new("0.5");
        my $b    = $class -> new("2");
        my $one  = $class -> bone();

        # We start off by initializing the exponent to zero and the
        # mantissa to the input value. Then we increase the mantissa and
        # decrease the exponent, or vice versa, until the mantissa is in
        # the desired range or we hit one of the limits for the exponent.

        $mant = $x -> copy() -> babs();

        # We need to find the base 2 exponent. First make an estimate of
        # the base 2 exponent, before adjusting it below. We could skip
        # this estimation and go straight to the while-loops below, but the
        # loops are slow, especially when the final exponent is far from
        # zero and even more so if the number of digits is large. This
        # initial estimation speeds up the computation dramatically.
        #
        #   log2($m * 10**$e) = log10($m + 10**$e) * log(10)/log(2)
        #                     = (log10($m) + $e) * log(10)/log(2)
        #                     = (log($m)/log(10) + $e) * log(10)/log(2)

        my ($m, $e) = $x -> nparts();
        my $ms = $m -> numify();
        my $es = $e -> numify();

        my $expo_est = (log(abs($ms))/log(10) + $es) * log(10)/log(2);
        $expo_est = int($expo_est);

        # Limit the exponent.

        if ($expo_est > $emax) {
            $expo_est = $emax;
        } elsif ($expo_est < $emin) {
            $expo_est = $emin;
        }

        # Don't multiply by a number raised to a negative exponent. This
        # will cause a division, whose result is truncated to some fixed
        # number of digits. Instead, multiply by the inverse number raised
        # to a positive exponent.

        $expo = $class -> new($expo_est);
        if ($expo_est > 0) {
            $mant = $mant -> bmul($binv -> copy() -> bpow($expo));
        } elsif ($expo_est < 0) {
            my $expo_abs = $expo -> copy() -> bneg();
            $mant = $mant -> bmul($b -> copy() -> bpow($expo_abs));
        }

        # Final adjustment of the estimate above.

        while ($mant >= $b && $expo <= $emax) {
            $mant = $mant -> bmul($binv);
            $expo = $expo -> binc();
        }

        while ($mant < $one && $expo >= $emin) {
            $mant = $mant -> bmul($b);
            $expo = $expo -> bdec();
        }

        # This is when the magnitude is larger than what can be represented in
        # this format. Encode as infinity.

        if ($expo > $emax) {
            $mant = $class -> bzero();
            $expo = $emax -> copy() -> binc();
        }

        # This is when the magnitude is so small that the number is encoded as
        # a subnormal number.
        #
        # If the magnitude is smaller than that of the smallest subnormal
        # number, and rounded downwards, it is encoded as zero. This works
        # transparently and does not need to be treated as a special case.
        #
        # If the number is between the largest subnormal number and the
        # smallest normal number, and the value is rounded upwards, the value
        # must be encoded as a normal number. This must be treated as a special
        # case.

        elsif ($expo < $emin) {

            # Scale up the mantissa (significand), and round to integer.

            my $const = $class -> new($b) -> bpow(62);
            $mant -> bmul($const) -> bfround(0);

            # If the mantissa overflowed, encode as the smallest normal number.

            if ($mant == $const -> bmul($b)) {
                $expo -> binc();
            }
        }

        # This is when the magnitude is within the range of what can be encoded
        # as a normal number.

        else {

            # Remove implicit leading bit, scale up the mantissa (significand)
            # to an integer, and round.

            my $const = $class -> new($b) -> bpow(63);
            $mant -> bmul($const) -> bfround(0);

            # If the mantissa overflowed, encode as the next larger value. If
            # this caused the exponent to overflow, encode as infinity.

            if ($mant == $const -> copy() -> bmul($b)) {
                $expo -> binc();
                if ($expo > $emax) {
                    $mant = $class -> bzero();
                } else {
                    $mant = $const;
                }
            }
        }
    }

    $expo = $expo -> badd($bias);               # add bias

    my $signbit = "$sign";

    my $mantbits = $mant -> to_bin();
    $mantbits = ("0" x (64 - CORE::length($mantbits))) . $mantbits;

    my $expobits = $expo -> to_bin();
    $expobits = ("0" x (15 - CORE::length($expobits))) . $expobits;

    my $bin = $signbit . $expobits . $mantbits;
    return pack "B*", $bin;
}

sub as_hex {
    # return number as hexadecimal string (only for integers defined)

    my (undef, $x, @r) = ref($_[0]) ? (undef, @_) : objectify(1, @_);

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    return $x -> bstr() if $x->{sign} !~ /^[+-]$/; # inf, nan etc
    return '0x0' if $x -> is_zero();

    return $nan if $x->{_es} ne '+';    # how to do 1e-1 in hex?

    my $z = $LIB->_copy($x->{_m});
    if (! $LIB->_is_zero($x->{_e})) {   # > 0
        $z = $LIB->_lsft($z, $x->{_e}, 10);
    }
    my $str = $LIB->_as_hex($z);
    return $x->{sign} eq '-' ? "-$str" : $str;
}

sub as_oct {
    # return number as octal digit string (only for integers defined)

    my (undef, $x, @r) = ref($_[0]) ? (undef, @_) : objectify(1, @_);

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    return $x -> bstr() if $x->{sign} !~ /^[+-]$/; # inf, nan etc
    return '00' if $x -> is_zero();

    return $nan if $x->{_es} ne '+';    # how to do 1e-1 in octal?

    my $z = $LIB->_copy($x->{_m});
    if (! $LIB->_is_zero($x->{_e})) {   # > 0
        $z = $LIB->_lsft($z, $x->{_e}, 10);
    }
    my $str = $LIB->_as_oct($z);
    return $x->{sign} eq '-' ? "-$str" : $str;
}

sub as_bin {
    # return number as binary digit string (only for integers defined)

    my (undef, $x, @r) = ref($_[0]) ? (undef, @_) : objectify(1, @_);

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    return $x -> bstr() if $x->{sign} !~ /^[+-]$/; # inf, nan etc
    return '0b0' if $x -> is_zero();

    return $nan if $x->{_es} ne '+';    # how to do 1e-1 in binary?

    my $z = $LIB->_copy($x->{_m});
    if (! $LIB->_is_zero($x->{_e})) {   # > 0
        $z = $LIB->_lsft($z, $x->{_e}, 10);
    }
    my $str = $LIB->_as_bin($z);
    return $x->{sign} eq '-' ? "-$str" : $str;
}

sub numify {
    # Make a Perl scalar number from a Math::BigFloat object.

    my (undef, $x, @r) = ref($_[0]) ? (undef, @_) : objectify(1, @_);

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    if ($x -> is_nan()) {
        require Math::Complex;
        my $inf = $Math::Complex::Inf;
        return $inf - $inf;
    }

    if ($x -> is_inf()) {
        require Math::Complex;
        my $inf = $Math::Complex::Inf;
        return $x -> is_negative() ? -$inf : $inf;
    }

    # Create a string and let Perl's atoi()/atof() handle the rest.

    return 0 + $x -> bnstr();
}

###############################################################################
# Private methods and functions.
###############################################################################

sub import {
    my $class = shift;
    $IMPORT++;                  # remember we did import()
    my @a;                      # unrecognized arguments

    my @import = ();

    while (@_) {
        my $param = shift;

        # Enable overloading of constants.

        if ($param eq ':constant') {
            overload::constant

                integer => sub {
                    $class -> new(shift);
                },

                float   => sub {
                    $class -> new(shift);
                },

                binary  => sub {
                    # E.g., a literal 0377 shall result in an object whose
                    # value is decimal 255, but new("0377") returns decimal
                    # 377.
                    return $class -> from_oct($_[0]) if $_[0] =~ /^0_*[0-7]/;
                    $class -> new(shift);
                };
            next;
        }

        # Upgrading.

        if ($param eq 'upgrade') {
            $class -> upgrade(shift);
            next;
        }

        # Downgrading.

        if ($param eq 'downgrade') {
            $class -> downgrade(shift);
            next;
        }

        # Accuracy.

        if ($param eq 'accuracy') {
            $class -> accuracy(shift);
            next;
        }

        # Precision.

        if ($param eq 'precision') {
            $class -> precision(shift);
            next;
        }

        # Rounding mode.

        if ($param eq 'round_mode') {
            $class -> round_mode(shift);
            next;
        }

        # Fall-back accuracy.

        if ($param eq 'div_scale') {
            $class -> div_scale(shift);
            next;
        }

        # Backend library.

        if ($param =~ /^(lib|try|only)\z/) {
            push @import, $param;
            push @import, shift() if @_;
            next;
        }

        if ($param eq 'with') {
            # alternative class for our private parts()
            # XXX: no longer supported
            # $LIB = shift() || 'Calc';
            # carp "'with' is no longer supported, use 'lib', 'try', or 'only'";
            shift;
            next;
        }

        # Unrecognized parameter.

        push @a, $param;
    }

    Math::BigInt -> import(@import);

    # find out which library was actually loaded
    $LIB = Math::BigInt -> config('lib');

    $class -> SUPER::import(@a);                        # for subclasses
    $class -> export_to_level(1, $class, @a) if @a;     # need this, too
}

sub _len_to_steps {
    # Given D (digits in decimal), compute N so that N! (N factorial) is
    # at least D digits long. D should be at least 50.
    my $d = shift;

    # two constants for the Ramanujan estimate of ln(N!)
    my $lg2 = log(2 * 3.14159265) / 2;
    my $lg10 = log(10);

    # D = 50 => N => 42, so L = 40 and R = 50
    my $l = 40;
    my $r = $d;

    # Otherwise this does not work under -Mbignum and we do not yet have "no
    # bignum;" :(
    $l = $l -> numify if ref($l);
    $r = $r -> numify if ref($r);
    $lg2 = $lg2 -> numify if ref($lg2);
    $lg10 = $lg10 -> numify if ref($lg10);

    # binary search for the right value (could this be written as the reverse
    # of lg(n!)?)
    while ($r - $l > 1) {
        my $n = int(($r - $l) / 2) + $l;
        my $ramanujan
          = int(($n * log($n) - $n + log($n * (1 + 4*$n*(1+2*$n))) / 6 + $lg2)
                / $lg10);
        $ramanujan > $d ? $r = $n : $l = $n;
    }
    $l;
}

sub _log {
    # internal log function to calculate ln() based on Taylor series.
    # Modifies $x in place.
    my ($x, $scale) = @_;
    my $class = ref $x;

    # in case of $x == 1, result is 0
    return $x -> bzero() if $x -> is_one();

    # XXX TODO: rewrite this in a similar manner to bexp()

    # http://www.efunda.com/math/taylor_series/logarithmic.cfm?search_string=log

    # u = x-1, v = x+1
    #              _                               _
    # Taylor:     |    u    1   u^3   1   u^5       |
    # ln (x)  = 2 |   --- + - * --- + - * --- + ... |  x > 0
    #             |_   v    3   v^3   5   v^5      _|

    # This takes much more steps to calculate the result and is thus not used
    # u = x-1
    #              _                               _
    # Taylor:     |    u    1   u^2   1   u^3       |
    # ln (x)  = 2 |   --- + - * --- + - * --- + ... |  x > 1/2
    #             |_   x    2   x^2   3   x^3      _|

    # scale used in intermediate computations
    my $scaleup = $scale + 4;

    my ($v, $u, $numer, $denom, $factor, $f);

    $v = $x -> copy();
    $v = $v -> binc();                  # v = x+1
    $x = $x -> bdec();
    $u = $x -> copy();                  # u = x-1; x = x-1

    $x = $x -> bdiv($v, $scaleup);        # first term: u/v

    $numer = $u -> copy();              # numerator
    $denom = $v -> copy();              # denominator

    $u = $u -> bmul($u);                # u^2
    $v = $v -> bmul($v);                # v^2

    $numer = $numer -> bmul($u);        # u^3
    $denom = $denom -> bmul($v);        # v^3

    $factor = $class -> new(3);
    $f = $class -> new(2);

    while (1) {
        my $next = $numer -> copy() -> bround($scaleup)
          -> bdiv($denom -> copy() -> bmul($factor) -> bround($scaleup), $scaleup);

        $next->{accuracy} = undef;
        $next->{precision} = undef;
        my $x_prev = $x -> copy();
        $x = $x -> badd($next);

        last if $x -> bacmp($x_prev) == 0;

        # calculate things for the next term
        $numer  = $numer -> bmul($u);
        $denom  = $denom -> bmul($v);
        $factor = $factor -> badd($f);
    }

    $x = $x -> bmul($f);             # $x *= 2
    $x = $x -> bround($scale);
}

sub _log_10 {
    # Internal log function based on reducing input to the range of 0.1 .. 9.99
    # and then "correcting" the result to the proper one. Modifies $x in place.
    my ($x, $scale) = @_;
    my $class = ref $x;

    # Taking blog() from numbers greater than 10 takes a *very long* time, so
    # we break the computation down into parts based on the observation that:
    #  blog(X*Y) = blog(X) + blog(Y)
    # We set Y here to multiples of 10 so that $x becomes below 1 - the smaller
    # $x is the faster it gets. Since 2*$x takes about 10 times as long, we
    # make it faster by about a factor of 100 by dividing $x by 10.

    # The same observation is valid for numbers smaller than 0.1, e.g.
    # computing log(1) is fastest, and the further away we get from 1, the
    # longer it takes. So we also 'break' this down by multiplying $x with 10
    # and subtract the log(10) afterwards to get the correct result.

    # To get $x even closer to 1, we also divide by 2 and then use log(2) to
    # correct for this. For instance if $x is 2.4, we use the formula:
    #  blog(2.4 * 2) == blog(1.2) + blog(2)
    # and thus calculate only blog(1.2) and blog(2), which is faster in total
    # than calculating blog(2.4).

    # In addition, the values for blog(2) and blog(10) are cached.

    # Calculate the number of digits before the dot, i.e., 1 + floor(log10(x)):
    #   x = 123      => dbd =  3
    #   x = 1.23     => dbd =  1
    #   x = 0.0123   => dbd = -1
    #   x = 0.000123 => dbd = -3
    #   etc.

    my $dbd = $LIB->_num($x->{_e});
    $dbd = -$dbd if $x->{_es} eq '-';
    $dbd += $LIB->_len($x->{_m});

    # more than one digit (e.g. at least 10), but *not* exactly 10 to avoid
    # infinite recursion

    my $calc = 1;               # do some calculation?

    # No upgrading or downgrading in the intermediate computations.

    my $upg = $class -> upgrade();
    my $dng = $class -> downgrade();
    $class -> upgrade(undef);
    $class -> downgrade(undef);

    # disable the shortcut for 10, since we need log(10) and this would recurse
    # infinitely deep
    if ($x->{_es} eq '+' &&                     # $x == 10
        ($LIB->_is_one($x->{_e}) &&
         $LIB->_is_one($x->{_m})))
    {
        $dbd = 0;               # disable shortcut
        # we can use the cached value in these cases
        if ($scale <= $LOG_10_A) {
            $x = $x -> bzero();
            $x = $x -> badd($LOG_10); # modify $x in place
            $calc = 0;                      # no need to calc, but round
        }
        # if we can't use the shortcut, we continue normally
    } else {
        # disable the shortcut for 2, since we maybe have it cached
        if (($LIB->_is_zero($x->{_e}) &&        # $x == 2
             $LIB->_is_two($x->{_m})))
        {
            $dbd = 0;           # disable shortcut
            # we can use the cached value in these cases
            if ($scale <= $LOG_2_A) {
                $x = $x -> bzero();
                $x = $x -> badd($LOG_2); # modify $x in place
                $calc = 0;                     # no need to calc, but round
            }
            # if we can't use the shortcut, we continue normally
        }
    }

    # if $x = 0.1, we know the result must be 0-log(10)
    if ($calc != 0 &&
        ($x->{_es} eq '-' &&                    # $x == 0.1
         ($LIB->_is_one($x->{_e}) &&
          $LIB->_is_one($x->{_m}))))
    {
        $dbd = 0;               # disable shortcut
        # we can use the cached value in these cases
        if ($scale <= $LOG_10_A) {
            $x = $x -> bzero();
            $x = $x -> bsub($LOG_10);
            $calc = 0;          # no need to calc, but round
        }
    }

    return $x if $calc == 0;    # already have the result

    # default: these correction factors are undef and thus not used
    my $l_10;                   # value of ln(10) to A of $scale
    my $l_2;                    # value of ln(2) to A of $scale

    my $two = $class -> new(2);

    # $x == 2 => 1, $x == 13 => 2, $x == 0.1 => 0, $x == 0.01 => -1
    # so don't do this shortcut for 1 or 0
    if (($dbd > 1) || ($dbd < 0)) {
        # convert our cached value to an object if not already (avoid doing
        # this at import() time, since not everybody needs this)
        $LOG_10 = $class -> new($LOG_10, undef, undef) unless ref $LOG_10;

        # got more than one digit before the dot, or more than one zero after
        # the dot, so do:
        #  log(123)    == log(1.23) + log(10) * 2
        #  log(0.0123) == log(1.23) - log(10) * 2

        if ($scale <= $LOG_10_A) {
            # use cached value
            $l_10 = $LOG_10 -> copy(); # copy for mul
        } else {
            # else: slower, compute and cache result

            # shorten the time to calculate log(10) based on the following:
            # log(1.25 * 8) = log(1.25) + log(8)
            #               = log(1.25) + log(2) + log(2) + log(2)

            # first get $l_2 (and possible compute and cache log(2))
            $LOG_2 = $class -> new($LOG_2, undef, undef) unless ref $LOG_2;
            if ($scale <= $LOG_2_A) {
                # use cached value
                $l_2 = $LOG_2 -> copy(); # copy() for the mul below
            } else {
                # else: slower, compute and cache result
                $l_2 = $two -> copy();
                $l_2 = $l_2->_log($scale); # scale+4, actually
                $LOG_2 = $l_2 -> copy(); # cache the result for later
                # the copy() is for mul below
                $LOG_2_A = $scale;
            }

            # now calculate log(1.25):
            $l_10 = $class -> new('1.25');
            $l_10 = $l_10->_log($scale); # scale+4, actually

            # log(1.25) + log(2) + log(2) + log(2):
            $l_10 = $l_10 -> badd($l_2);
            $l_10 = $l_10 -> badd($l_2);
            $l_10 = $l_10 -> badd($l_2);
            $LOG_10 = $l_10 -> copy(); # cache the result for later
            # the copy() is for mul below
            $LOG_10_A = $scale;
        }
        $dbd-- if ($dbd > 1);       # 20 => dbd=2, so make it dbd=1
        $l_10 = $l_10 -> bmul($class -> new($dbd)); # log(10) * (digits_before_dot-1)
        my $dbd_sign = '+';
        if ($dbd < 0) {
            $dbd = -$dbd;
            $dbd_sign = '-';
        }
        ($x->{_e}, $x->{_es}) =
          $LIB -> _ssub($x->{_e}, $x->{_es}, $LIB->_new($dbd), $dbd_sign);
    }

    # Now: 0.1 <= $x < 10 (and possible correction in l_10)

    ### Since $x in the range 0.5 .. 1.5 is MUCH faster, we do a repeated div
    ### or mul by 2 (maximum times 3, since x < 10 and x > 0.1)

    $HALF = $class -> new($HALF) unless ref($HALF);

    my $twos = 0;               # default: none (0 times)
    while ($x -> bacmp($HALF) <= 0) { # X <= 0.5
        $twos--;
        $x = $x -> bmul($two);
    }
    while ($x -> bacmp($two) >= 0) { # X >= 2
        $twos++;
        $x = $x -> bdiv($two, $scale+4); # keep all digits
    }
    $x = $x -> bround($scale+4);
    # $twos > 0 => did mul 2, < 0 => did div 2 (but we never did both)
    # So calculate correction factor based on ln(2):
    if ($twos != 0) {
        $LOG_2 = $class -> new($LOG_2, undef, undef) unless ref $LOG_2;
        if ($scale <= $LOG_2_A) {
            # use cached value
            $l_2 = $LOG_2 -> copy(); # copy() for the mul below
        } else {
            # else: slower, compute and cache result
            $l_2 = $two -> copy();
            $l_2 = $l_2->_log($scale); # scale+4, actually
            $LOG_2 = $l_2 -> copy(); # cache the result for later
            # the copy() is for mul below
            $LOG_2_A = $scale;
        }
        $l_2 = $l_2 -> bmul($twos);      # * -2 => subtract, * 2 => add
    } else {
        undef $l_2;
    }

    $x = $x->_log($scale);       # need to do the "normal" way
    $x = $x -> badd($l_10) if defined $l_10; # correct it by ln(10)
    $x = $x -> badd($l_2) if defined $l_2;   # and maybe by ln(2)

    # Restore globals

    $class -> upgrade($upg);
    $class -> downgrade($dng);

    # all done, $x contains now the result
    $x;
}

sub _pow {
    # Calculate a power where $y is a non-integer, like 2 ** 0.3
    my ($x, $y, @r) = @_;
    my $class = ref($x);

    # if $y == 0.5, it is sqrt($x)
    $HALF = $class -> new($HALF) unless ref($HALF);
    return $x -> bsqrt(@r, $y) if $y -> bcmp($HALF) == 0;

    # Using:
    # a ** x == e ** (x * ln a)

    # u = y * ln x
    #                _                         _
    # Taylor:       |   u    u^2    u^3         |
    # x ** y  = 1 + |  --- + --- + ----- + ...  |
    #               |_  1    1*2   1*2*3       _|

    # we need to limit the accuracy to protect against overflow
    my $fallback = 0;
    my ($scale, @params);
    ($x, @params) = $x->_find_round_parameters(@r);

    return $x if $x -> is_nan();  # error in _find_round_parameters?

    # no rounding at all, so must use fallback
    if (scalar @params == 0) {
        # simulate old behaviour
        $params[0] = $class -> div_scale(); # and round to it as accuracy
        $params[1] = undef;               # disable P
        $scale = $params[0]+4;            # at least four more for proper round
        $params[2] = $r[2];               # round mode by caller or undef
        $fallback = 1;                    # to clear a/p afterwards
    } else {
        # the 4 below is empirical, and there might be cases where it is not
        # enough...
        $scale = abs($params[0] || $params[1]) + 4; # take whatever is defined
    }

    # When user set globals, they would interfere with our calculation, so
    # disable them and later re-enable them.

    my $ab = $class -> accuracy();
    my $pb = $class -> precision();
    $class -> accuracy(undef);
    $class -> precision(undef);

    # Disabling upgrading and downgrading is no longer necessary to avoid an
    # infinite recursion, but it avoids unnecessary upgrading and downgrading
    # in the intermediate computations.

    my $upg = $class -> upgrade();
    my $dng = $class -> downgrade();
    $class -> upgrade(undef);
    $class -> downgrade(undef);

    # We also need to disable any set A or P on $x (_find_round_parameters took
    # them already into account), since these would interfere, too.

    $x->{accuracy} = undef;
    $x->{precision} = undef;

    my ($limit, $v, $u, $below, $factor, $next, $over);

    $u = $x -> copy() -> blog(undef, $scale) -> bmul($y);
    my $do_invert = ($u->{sign} eq '-');
    $u = $u -> bneg()  if $do_invert;
    $v = $class -> bone();        # 1
    $factor = $class -> new(2);   # 2
    $x = $x -> bone();                 # first term: 1

    $below = $v -> copy();
    $over = $u -> copy();

    $limit = $class -> new("1E-". ($scale-1));
    while (3 < 5) {
        # we calculate the next term, and add it to the last
        # when the next term is below our limit, it won't affect the outcome
        # anymore, so we stop:
        $next = $over -> copy() -> bdiv($below, $scale);
        last if $next -> bacmp($limit) <= 0;
        $x = $x -> badd($next);
        # calculate things for the next term
        $over *= $u;
        $below *= $factor;
        $factor = $factor -> binc();

        last if $x->{sign} !~ /^[-+]$/;
    }

    if ($do_invert) {
        my $x_copy = $x -> copy();
        $x = $x -> bone -> bdiv($x_copy, $scale);
    }

    # shortcut to not run through _find_round_parameters again
    if (defined $params[0]) {
        $x = $x -> bround($params[0], $params[2]); # then round accordingly
    } else {
        $x = $x -> bfround($params[1], $params[2]); # then round accordingly
    }
    if ($fallback) {
        # clear a/p after round, since user did not request it
        $x->{accuracy} = undef;
        $x->{precision} = undef;
    }

    # Restore globals. We need to do it like this, because setting one
    # undefines the other.

    if (defined $ab) {
        $class -> accuracy($ab);
    } else {
        $class -> precision($pb);
    }

    $class -> upgrade($upg);
    $class -> downgrade($dng);

    $x;
}

# These functions are only provided for backwards compabibility so that old
# version of Math::BigRat etc. don't complain about missing them.

sub _e_add {
    my ($x, $y, $xs, $ys) = @_;
    return $LIB -> _sadd($x, $xs, $y, $ys);
}

sub _e_sub {
    my ($x, $y, $xs, $ys) = @_;
    return $LIB -> _ssub($x, $xs, $y, $ys);
}

1;

__END__

=pod

=head1 NAME

Math::BigFloat - arbitrary size floating point math package

=head1 SYNOPSIS

  use Math::BigFloat;

  # Configuration methods (may be used as class methods and instance methods)

  Math::BigFloat->accuracy($n);       # set accuracy
  Math::BigFloat->accuracy();         # get accuracy
  Math::BigFloat->precision($n);      # set precision
  Math::BigFloat->precision();        # get precision
  Math::BigFloat->round_mode($m);     # set rounding mode, must be
                                      # 'even', 'odd', '+inf', '-inf',
                                      # 'zero', 'trunc', or 'common'
  Math::BigFloat->round_mode();       # get class rounding mode
  Math::BigFloat->div_scale($n);      # set fallback accuracy
  Math::BigFloat->div_scale();        # get fallback accuracy
  Math::BigFloat->trap_inf($b);       # trap infinities or not
  Math::BigFloat->trap_inf();         # get trap infinities status
  Math::BigFloat->trap_nan($b);       # trap NaNs or not
  Math::BigFloat->trap_nan();         # get trap NaNs status
  Math::BigFloat->config($par, $val); # set configuration parameter
  Math::BigFloat->config($par);       # get configuration parameter
  Math::BigFloat->config();           # get hash with configuration
  Math::BigFloat->config("lib");      # get name of backend library

  # Generic constructor method (always returns a new object)

  $x = Math::BigFloat->new($str);               # defaults to 0
  $x = Math::BigFloat->new('256');              # from decimal
  $x = Math::BigFloat->new('0256');             # from decimal
  $x = Math::BigFloat->new('0xcafe');           # from hexadecimal
  $x = Math::BigFloat->new('0x1.cafep+7');      # from hexadecimal
  $x = Math::BigFloat->new('0o377');            # from octal
  $x = Math::BigFloat->new('0o1.3571p+6');      # from octal
  $x = Math::BigFloat->new('0b101');            # from binary
  $x = Math::BigFloat->new('0b1.101p+3');       # from binary

  # Specific constructor methods (no prefix needed; when used as
  # instance method, the value is assigned to the invocand)

  $x = Math::BigFloat->from_dec('234');         # from decimal
  $x = Math::BigFloat->from_hex('c.afep+3');    # from hexadecimal
  $x = Math::BigFloat->from_hex('cafe');        # from hexadecimal
  $x = Math::BigFloat->from_oct('1.3267p-4');   # from octal
  $x = Math::BigFloat->from_oct('377');         # from octal
  $x = Math::BigFloat->from_bin('0b1.1001p-4'); # from binary
  $x = Math::BigFloat->from_bin('0101');        # from binary
  $x = Math::BigFloat->from_bytes($bytes);      # from byte string
  $x = Math::BigFloat->from_base('why', 36);    # from any base
  $x = Math::BigFloat->from_ieee754($b, $fmt);  # from IEEE-754 bytes
  $x = Math::BigFloat->from_fp80($b);           # from x86 80-bit
  $x = Math::BigFloat->bzero();                 # create a +0
  $x = Math::BigFloat->bone();                  # create a +1
  $x = Math::BigFloat->bone('-');               # create a -1
  $x = Math::BigFloat->binf();                  # create a +inf
  $x = Math::BigFloat->binf('-');               # create a -inf
  $x = Math::BigFloat->bnan();                  # create a Not-A-Number
  $x = Math::BigFloat->bpi();                   # returns pi

  $y = $x->copy();        # make a copy (unlike $y = $x)
  $y = $x->as_int();      # return as BigInt
  $y = $x->as_float();    # return as a Math::BigFloat
  $y = $x->as_rat();      # return as a Math::BigRat

  # Boolean methods (these don't modify the invocand)

  $x->is_zero();          # true if $x is 0
  $x->is_one();           # true if $x is +1
  $x->is_one("+");        # true if $x is +1
  $x->is_one("-");        # true if $x is -1
  $x->is_inf();           # true if $x is +inf or -inf
  $x->is_inf("+");        # true if $x is +inf
  $x->is_inf("-");        # true if $x is -inf
  $x->is_nan();           # true if $x is NaN

  $x->is_finite();        # true if -inf < $x < inf
  $x->is_positive();      # true if $x > 0
  $x->is_pos();           # true if $x > 0
  $x->is_negative();      # true if $x < 0
  $x->is_neg();           # true if $x < 0
  $x->is_non_positive()   # true if $x <= 0
  $x->is_non_negative()   # true if $x >= 0

  $x->is_odd();           # true if $x is odd
  $x->is_even();          # true if $x is even
  $x->is_int();           # true if $x is an integer

  # Comparison methods (these don't modify the invocand)

  $x->bcmp($y);           # compare numbers (undef, < 0, == 0, > 0)
  $x->bacmp($y);          # compare abs values (undef, < 0, == 0, > 0)
  $x->beq($y);            # true if $x == $y
  $x->bne($y);            # true if $x != $y
  $x->blt($y);            # true if $x < $y
  $x->ble($y);            # true if $x <= $y
  $x->bgt($y);            # true if $x > $y
  $x->bge($y);            # true if $x >= $y

  # Arithmetic methods (these modify the invocand)

  $x->bneg();             # negation
  $x->babs();             # absolute value
  $x->bsgn();             # sign function (-1, 0, 1, or NaN)
  $x->binc();             # increment $x by 1
  $x->bdec();             # decrement $x by 1
  $x->badd($y);           # addition (add $y to $x)
  $x->bsub($y);           # subtraction (subtract $y from $x)
  $x->bmul($y);           # multiplication (multiply $x by $y)
  $x->bmuladd($y, $z);    # $x = $x * $y + $z
  $x->bdiv($y);           # division (floored), set $x to quotient
  $x->bmod($y);           # modulus (x % y)
  $x->bmodinv($mod);      # modular multiplicative inverse
  $x->bmodpow($y, $mod);  # modular exponentiation (($x ** $y) % $mod)
  $x->btdiv($y);          # division (truncated), set $x to quotient
  $x->btmod($y);          # modulus (truncated)
  $x->binv()              # inverse (1/$x)
  $x->bpow($y);           # power of arguments (x ** y)
  $x->blog();             # logarithm of $x to base e (Euler's number)
  $x->blog($base);        # logarithm of $x to base $base (e.g., base 2)
  $x->bexp();             # calculate e ** $x where e is Euler's number
  $x->bilog2();           # log2($x) rounded down to nearest int
  $x->bilog10();          # log10($x) rounded down to nearest int
  $x->bclog2();           # log2($x) rounded up to nearest int
  $x->bclog10();          # log10($x) rounded up to nearest int
  $x->bnok($y);           # combinations (binomial coefficient n over k)
  $x->bperm($y);          # permutations
  $x->bsin();             # sine
  $x->bcos();             # cosine
  $x->batan();            # inverse tangent
  $x->batan2($y);         # two-argument inverse tangent
  $x->bsqrt();            # calculate square root
  $x->broot($y);          # $y'th root of $x (e.g. $y == 3 => cubic root)
  $x->bfac();             # factorial of $x (1*2*3*4*..$x)
  $x->bdfac();            # double factorial of $x ($x*($x-2)*($x-4)*...)
  $x->btfac();            # triple factorial of $x ($x*($x-3)*($x-6)*...)
  $x->bmfac($k);          # $k'th multi-factorial of $x ($x*($x-$k)*...)
  $x->bfib($k);           # $k'th Fibonacci number
  $x->blucas($k);         # $k'th Lucas number

  $x->blsft($n);          # left shift $n places in base 2
  $x->blsft($n, $b);      # left shift $n places in base $b
  $x->brsft($n);          # right shift $n places in base 2
  $x->brsft($n, $b);      # right shift $n places in base $b

  # Bitwise methods (these modify the invocand)

  $x->bblsft($y);         # bitwise left shift
  $x->bbrsft($y);         # bitwise right shift
  $x->band($y);           # bitwise and
  $x->bior($y);           # bitwise inclusive or
  $x->bxor($y);           # bitwise exclusive or
  $x->bnot();             # bitwise not (two's complement)

  # Rounding methods (these modify the invocand)

  $x->round($A, $P, $R);  # round to accuracy or precision using
                          #   rounding mode $R
  $x->bround($n);         # accuracy: preserve $n digits
  $x->bfround($n);        # $n > 0: round to $nth digit left of dec. point
                          # $n < 0: round to $nth digit right of dec. point
  $x->bfloor();           # round towards minus infinity
  $x->bceil();            # round towards plus infinity
  $x->bint();             # round towards zero

  # Other mathematical methods (these don't modify the invocand)

  $x->bgcd($y);           # greatest common divisor
  $x->blcm($y);           # least common multiple

  # Object property methods (these don't modify the invocand)

  $x->sign();             # the sign, either +, - or NaN
  $x->digit($n);          # the nth digit, counting from the right
  $x->digit(-$n);         # the nth digit, counting from the left
  $x->length();           # return number of digits in number
  $x->mantissa();         # return (signed) mantissa as BigInt
  $x->exponent();         # return exponent as BigInt
  $x->parts();            # return (mantissa,exponent) as BigInt
  $x->sparts();           # mantissa and exponent (as integers)
  $x->nparts();           # mantissa and exponent (normalised)
  $x->eparts();           # mantissa and exponent (engineering notation)
  $x->dparts();           # integer and fraction part
  $x->fparts();           # numerator and denominator
  $x->numerator();        # numerator
  $x->denominator();      # denominator

  # Conversion methods (these don't modify the invocand)

  $x->bstr();             # decimal notation (possibly zero padded)
  $x->bsstr();            # string in scientific notation with integers
  $x->bnstr();            # string in normalized notation
  $x->bestr();            # string in engineering notation
  $x->bdstr();            # string in decimal notation (no padding)
  $x->bfstr();            # string in fractional notation

  $x->to_hex();           # as signed hexadecimal string
  $x->to_bin();           # as signed binary string
  $x->to_oct();           # as signed octal string
  $x->to_bytes();         # as byte string
  $x->to_ieee754($fmt);   # to bytes encoded according to IEEE 754-2008
  $x->to_fp80();          # encode value in x86 80-bit format

  $x->as_hex();           # as signed hexadecimal string with "0x" prefix
  $x->as_bin();           # as signed binary string with "0b" prefix
  $x->as_oct();           # as signed octal string with "0" prefix

  # Other conversion methods (these don't modify the invocand)

  $x->numify();           # return as scalar (might overflow or underflow)

=head1 DESCRIPTION

Math::BigFloat provides support for arbitrary precision floating point.
Overloading is also provided for Perl operators.

All operators (including basic math operations) are overloaded if you
declare your big floating point numbers as

  $x = Math::BigFloat -> new('12_3.456_789_123_456_789E-2');

Operations with overloaded operators preserve the arguments, which is
exactly what you expect.

=head2 Input

Input values to these routines may be any scalar number or string that looks
like a number. Anything that is accepted by Perl as a literal numeric constant
should be accepted by this module.

=over

=item *

Leading and trailing whitespace is ignored.

=item *

Leading zeros are ignored, except for floating point numbers with a binary
exponent, in which case the number is interpreted as an octal floating point
number. For example, "01.4p+0" gives 1.5, "00.4p+0" gives 0.5, but "0.4p+0"
gives a NaN. And while "0377" gives 255, "0377p0" gives 255.

=item *

If the string has a "0x" or "0X" prefix, it is interpreted as a hexadecimal
number.

=item *

If the string has a "0o" or "0O" prefix, it is interpreted as an octal number.
A floating point literal with a "0" prefix is also interpreted as an octal
number.

=item *

If the string has a "0b" or "0B" prefix, it is interpreted as a binary number.

=item *

Underline characters are allowed in the same way as they are allowed in literal
numerical constants.

=item *

If the string can not be interpreted, NaN is returned.

=item *

For hexadecimal, octal, and binary floating point numbers, the exponent must be
separated from the significand (mantissa) by the letter "p" or "P", not "e" or
"E" as with decimal numbers.

=back

Some examples of valid string input

    Input string                Resulting value

    123                         123
    1.23e2                      123
    12300e-2                    123

    67_538_754                  67538754
    -4_5_6.7_8_9e+0_1_0         -4567890000000

    0x13a                       314
    0x13ap0                     314
    0x1.3ap+8                   314
    0x0.00013ap+24              314
    0x13a000p-12                314

    0o472                       314
    0o1.164p+8                  314
    0o0.0001164p+20             314
    0o1164000p-10               314

    0472                        472     Note!
    01.164p+8                   314
    00.0001164p+20              314
    01164000p-10                314

    0b100111010                 314
    0b1.0011101p+8              314
    0b0.00010011101p+12         314
    0b100111010000p-3           314

    0x1.921fb5p+1               3.14159262180328369140625e+0
    0o1.2677025p1               2.71828174591064453125
    01.2677025p1                2.71828174591064453125
    0b1.1001p-4                 9.765625e-2

=head2 Output

Output values are usually Math::BigFloat objects.

Boolean operators L<is_zero()|Math::BigInt/is_zero()>,
L<is_one()|Math::BigInt/is_one()>, L<is_inf()|Math::BigInt/is_inf()>, etc.
return true or false.

Comparison operators L<bcmp()|Math::BigInt/bcmp()> and
L<bacmp()|Math::BigInt/bacmp()>) return -1, 0, 1, or undef.

=head1 METHODS

Math::BigFloat supports all methods that Math::BigInt supports, except it
calculates non-integer results when possible. Please see L<Math::BigInt> for a
full description of each method. Below are just the most important differences:

=head2 Configuration methods

=over

=item accuracy()

    $x->accuracy(5);           # local for $x
    CLASS->accuracy(5);        # global for all members of CLASS
                               # Note: This also applies to new()!

    $A = $x->accuracy();       # read out accuracy that affects $x
    $A = CLASS->accuracy();    # read out global accuracy

Set or get the global or local accuracy, aka how many significant digits the
results have. If you set a global accuracy, then this also applies to new()!

Warning! The accuracy I<sticks>, e.g. once you created a number under the
influence of C<< CLASS->accuracy($A) >>, all results from math operations with
that number will also be rounded.

In most cases, you should probably round the results explicitly using one of
L<Math::BigInt/round()>, L<Math::BigInt/bround()> or L<Math::BigInt/bfround()>
or by passing the desired accuracy to the math operation as additional
parameter:

    my $x = Math::BigInt->new(30000);
    my $y = Math::BigInt->new(7);
    print scalar $x->copy()->bdiv($y, 2);           # print 4300
    print scalar $x->copy()->bdiv($y)->bround(2);   # print 4300

=item precision()

    $x->precision(-2);        # local for $x, round at the second
                              # digit right of the dot
    $x->precision(2);         # ditto, round at the second digit
                              # left of the dot

    CLASS->precision(5);      # Global for all members of CLASS
                              # This also applies to new()!
    CLASS->precision(-5);     # ditto

    $P = CLASS->precision();  # read out global precision
    $P = $x->precision();     # read out precision that affects $x

Note: You probably want to use L</accuracy()> instead. With L</accuracy()> you
set the number of digits each result should have, with L</precision()> you
set the place where to round!

=back

=head2 Constructor methods

=over

=item from_dec()

    $x -> from_hex("314159");
    $x = Math::BigInt -> from_hex("314159");

Interpret input as a decimal. It is equivalent to new(), but does not accept
anything but strings representing finite, decimal numbers.

=item from_hex()

    $x -> from_hex("0x1.921fb54442d18p+1");
    $x = Math::BigFloat -> from_hex("0x1.921fb54442d18p+1");

Interpret input as a hexadecimal string.A prefix ("0x", "x", ignoring case) is
optional. A single underscore character ("_") may be placed between any two
digits. If the input is invalid, a NaN is returned. The exponent is in base 2
using decimal digits.

If called as an instance method, the value is assigned to the invocand.

=item from_oct()

    $x -> from_oct("1.3267p-4");
    $x = Math::BigFloat -> from_oct("1.3267p-4");

Interpret input as an octal string. A single underscore character ("_") may be
placed between any two digits. If the input is invalid, a NaN is returned. The
exponent is in base 2 using decimal digits.

If called as an instance method, the value is assigned to the invocand.

=item from_bin()

    $x -> from_bin("0b1.1001p-4");
    $x = Math::BigFloat -> from_bin("0b1.1001p-4");

Interpret input as a hexadecimal string. A prefix ("0b" or "b", ignoring case)
is optional. A single underscore character ("_") may be placed between any two
digits. If the input is invalid, a NaN is returned. The exponent is in base 2
using decimal digits.

If called as an instance method, the value is assigned to the invocand.

=item from_bytes()

    $x = Math::BigFloat->from_bytes("\xf3\x6b");  # $x = 62315

Interpret the input as a byte string, assuming big endian byte order. The
output is always a non-negative, finite integer.

See L<Math::BigInt/from_bytes()>.

=item from_ieee754()

Interpret the input as a value encoded as described in IEEE754-2008. The input
can be given as a byte string, hex string, or binary string. The input is
assumed to be in big-endian byte-order.

    # Both $dbl, $xr, $xh, and $xb below are 3.141592...

    $dbl = unpack "d>", "\x40\x09\x21\xfb\x54\x44\x2d\x18";

    $raw = "\x40\x09\x21\xfb\x54\x44\x2d\x18";          # raw bytes
    $xr  = Math::BigFloat -> from_ieee754($raw, "binary64");

    $hex = "400921fb54442d18";
    $xh  = Math::BigFloat -> from_ieee754($hex, "binary64");

    $bin = "0100000000001001001000011111101101010100010001000010110100011000";
    $xb  = Math::BigFloat -> from_ieee754($bin, "binary64");

Supported formats are all IEEE 754 binary formats: "binary16", "binary32",
"binary64", "binary128", "binary160", "binary192", "binary224", "binary256",
etc. where the number of bits is a multiple of 32 for all formats larger than
"binary128". Aliases are "half" ("binary16"), "single" ("binary32"), "double"
("binary64"), "quadruple" ("binary128"), "octuple" ("binary256"), and
"sexdecuple" ("binary512").

See also L</to_ieee754()>.

=item from_fp80()

Interpret the input as a value encoded as an x86 80-bit floating point number. The input
can be given as a 10 character byte string, 20 character hex string, or 80 character binary string. The input is
assumed to be in big-endian byte-order.

    # Both $xr, $xh, and $xb below are 3.141592...

    $dbl = unpack "d>", "\x40\x09\x21\xfb\x54\x44\x2d\x18";

    $raw = "\x40\x00\xc9\x0f\xda\xa2\x21\x68\xc2\x35";  # raw bytes
    $xr  = Math::BigFloat -> from_fp80($raw);

    $hex = "4000c90fdaa22168c235";
    $xh  = Math::BigFloat -> from_fp80($hex);

    $bin = "0100000000000000110010010000111111011010"
         . "1010001000100001011010001100001000110101";
    $xb  = Math::BigFloat -> from_fp80($bin);
See also L</to_ieee754()>.

=item from_base()

See L<Math::BigInt/from_base()>.

=item bpi()

    print Math::BigFloat->bpi(100), "\n";

Calculate PI to N digits (including the 3 before the dot). The result is
rounded according to the current rounding mode, which defaults to "even".

This method was added in v1.87 of Math::BigInt (June 2007).

=item as_int()

    $y = $x -> as_int();        # $y is a Math::BigInt

Returns $x as a Math::BigInt object regardless of upgrading and downgrading. If
$x is finite, but not an integer, $x is truncated.

=item as_rat()

    $y = $x -> as_rat();        # $y is a Math::BigRat

Returns $x a Math::BigRat object regardless of upgrading and downgrading. The
invocand is not modified.

=item as_float()

    $y = $x -> as_float();      # $y is a Math::BigFloat

Returns $x a Math::BigFloat object regardless of upgrading and downgrading. The
invocand is not modified.

=back

=head2 Arithmetic methods

=over

=item bdiv()

    $x->bdiv($y);               # set $x to quotient
    ($q, $r) = $x->bdiv($y);    # also remainder

This is an alias for L</bfdiv()>.

=item bmod()

    $x->bmod($y);

Returns $x modulo $y. When $x is finite, and $y is finite and non-zero, the
result is identical to the remainder after floored division (F-division). If,
in addition, both $x and $y are integers, the result is identical to the result
from Perl's % operator.

=item bfdiv()

    $q = $x->bfdiv($y);
    ($q, $r) = $x->bfdiv($y);

In scalar context, divides $x by $y and returns the result to the given
accuracy or precision or the default accuracy. In list context, does floored
division (F-division), returning an integer $q and a remainder $r

    $q = floor($x / $y)
    $r = $x - $q * $y

so that the following relationship always holds

    $x = $q * $y + $r

The remainer (modulo) is equal to what is returned by C<< $x->bmod($y) >>.

=item binv()

    $x->binv();

Invert the value of $x, i.e., compute 1/$x.

=item bmuladd()

    $x->bmuladd($y,$z);

Multiply $x by $y, and then add $z to the result.

This method was added in v1.87 of Math::BigInt (June 2007).

=item bexp()

    $x->bexp($accuracy);            # calculate e ** X

Calculates the expression C<e ** $x> where C<e> is Euler's number.

This method was added in v1.82 of Math::BigInt (April 2007).

=item bnok()

See L<Math::BigInt/bnok()>.

=item bperm()

See L<Math::BigInt/bperm()>.

=item bsin()

    my $x = Math::BigFloat->new(1);
    print $x->bsin(100), "\n";

Calculate the sinus of $x, modifying $x in place.

This method was added in v1.87 of Math::BigInt (June 2007).

=item bcos()

    my $x = Math::BigFloat->new(1);
    print $x->bcos(100), "\n";

Calculate the cosinus of $x, modifying $x in place.

This method was added in v1.87 of Math::BigInt (June 2007).

=item batan()

    my $x = Math::BigFloat->new(1);
    print $x->batan(100), "\n";

Calculate the arcus tanges of $x, modifying $x in place. See also L</batan2()>.

This method was added in v1.87 of Math::BigInt (June 2007).

=item batan2()

    my $y = Math::BigFloat->new(2);
    my $x = Math::BigFloat->new(3);
    print $y->batan2($x), "\n";

Calculate the arcus tanges of C<$y> divided by C<$x>, modifying $y in place.
See also L</batan()>.

This method was added in v1.87 of Math::BigInt (June 2007).

=item bgcd()

    $x -> bgcd($y);             # GCD of $x and $y
    $x -> bgcd($y, $z, ...);    # GCD of $x, $y, $z, ...

Returns the greatest common divisor (GCD), which is the number with the largest
absolute value such that $x/$gcd, $y/$gcd, ... is an integer. For example, when
the operands are 0.8 and 1.2, the GCD is 0.4. This is a generalisation of the
ordinary GCD for integers. See L<Math::BigInt/gcd()>.

=back

=head2 String conversion methods

=over

=item bstr()

    my $x = Math::BigRat->new('8/4');
    print $x->bstr(), "\n";             # prints 1/2

Returns a string representing the number.

=item bsstr()

See L<Math::BigInt/bsstr()>.

=item bnstr()

See L<Math::BigInt/bnstr()>.

=item bestr()

See L<Math::BigInt/bestr()>.

=item bdstr()

See L<Math::BigInt/bdstr()>.

=item to_bytes()

See L<Math::BigInt/to_bytes()>.

=item to_ieee754()

Encodes the invocand as a byte string in the given format as specified in IEEE
754-2008. Note that the encoded value is the nearest possible representation of
the value. This value might not be exactly the same as the value in the
invocand.

    # $x = 3.1415926535897932385
    $x = Math::BigFloat -> bpi(30);

    $b = $x -> to_ieee754("binary64");  # encode as 8 bytes
    $h = unpack "H*", $b;               # "400921fb54442d18"

    # 3.141592653589793115997963...
    $y = Math::BigFloat -> from_ieee754($h, "binary64");

All binary formats in IEEE 754-2008 are accepted. For convenience, som aliases
are recognized: "half" for "binary16", "single" for "binary32", "double" for
"binary64", "quadruple" for "binary128", "octuple" for "binary256", and
"sexdecuple" for "binary512".

See also L</from_ieee754()>, L<https://en.wikipedia.org/wiki/IEEE_754>.

=back

=head2 ACCURACY AND PRECISION

See also: L<Rounding|/Rounding>.

Math::BigFloat supports both precision (rounding to a certain place before or
after the dot) and accuracy (rounding to a certain number of digits). For a
full documentation, examples and tips on these topics please see the large
section about rounding in L<Math::BigInt>.

Since things like C<sqrt(2)> or C<1 / 3> must presented with a limited
accuracy lest a operation consumes all resources, each operation produces
no more than the requested number of digits.

If there is no global precision or accuracy set, B<and> the operation in
question was not called with a requested precision or accuracy, B<and> the
input $x has no accuracy or precision set, then a fallback parameter will
be used. For historical reasons, it is called C<div_scale> and can be accessed
via:

    $d = Math::BigFloat->div_scale();       # query
    Math::BigFloat->div_scale($n);          # set to $n digits

The default value for C<div_scale> is 40.

In case the result of one operation has more digits than specified,
it is rounded. The rounding mode taken is either the default mode, or the one
supplied to the operation after the I<scale>:

    $x = Math::BigFloat->new(2);
    Math::BigFloat->accuracy(5);              # 5 digits max
    $y = $x->copy()->bdiv(3);                 # gives 0.66667
    $y = $x->copy()->bdiv(3,6);               # gives 0.666667
    $y = $x->copy()->bdiv(3,6,undef,'odd');   # gives 0.666667
    Math::BigFloat->round_mode('zero');
    $y = $x->copy()->bdiv(3,6);               # will also give 0.666667

Note that C<< Math::BigFloat->accuracy() >> and
C<< Math::BigFloat->precision() >> set the global variables, and thus B<any>
newly created number will be subject to the global rounding B<immediately>.
This means that in the examples above, the C<3> as argument to L</bdiv()> will
also get an accuracy of B<5>.

It is less confusing to either calculate the result fully, and afterwards
round it explicitly, or use the additional parameters to the math
functions like so:

    use Math::BigFloat;
    $x = Math::BigFloat->new(2);
    $y = $x->copy()->bdiv(3);
    print $y->bround(5),"\n";               # gives 0.66667

    or

    use Math::BigFloat;
    $x = Math::BigFloat->new(2);
    $y = $x->copy()->bdiv(3,5);             # gives 0.66667
    print "$y\n";

=head2 Rounding

=over

=item bfround ( +$scale )

Rounds to the $scale'th place left from the '.', counting from the dot.
The first digit is numbered 1.

=item bfround ( -$scale )

Rounds to the $scale'th place right from the '.', counting from the dot.

=item bfround ( 0 )

Rounds to an integer.

=item bround  ( +$scale )

Preserves accuracy to $scale digits from the left (aka significant digits) and
pads the rest with zeros. If the number is between 1 and -1, the significant
digits count from the first non-zero after the '.'

=item bround  ( -$scale ) and bround ( 0 )

These are effectively no-ops.

=back

All rounding functions take as a second parameter a rounding mode from one of
the following: 'even', 'odd', '+inf', '-inf', 'zero', 'trunc' or 'common'.

The default rounding mode is 'even'. By using
C<< Math::BigFloat->round_mode($round_mode); >> you can get and set the default
mode for subsequent rounding. The usage of C<$Math::BigFloat::$round_mode> is
no longer supported.
The second parameter to the round functions then overrides the default
temporarily.

The L</as_int()> method returns a BigInt from a Math::BigFloat. It uses 'trunc'
as rounding mode to make it equivalent to:

    $x = 2.5;
    $y = int($x) + 2;

You can override this by passing the desired rounding mode as parameter to
L</as_int()>:

    $x = Math::BigFloat->new(2.5);
    $y = $x->as_number('odd');      # $y = 3

=head1 NUMERIC LITERALS

After C<use Math::BigFloat ':constant'> all numeric literals in the given scope
are converted to C<Math::BigFloat> objects. This conversion happens at compile
time.

For example,

    perl -MMath::BigFloat=:constant -le 'print 2e-150'

prints the exact value of C<2e-150>. Note that without conversion of constants
the expression C<2e-150> is calculated using Perl scalars, which leads to an
inaccuracte result.

Note that strings are not affected, so that

    use Math::BigFloat qw/:constant/;

    $y = "1234567890123456789012345678901234567890"
            + "123456789123456789";

does not give you what you expect. You need an explicit Math::BigFloat->new()
around at least one of the operands. You should also quote large constants to
prevent loss of precision:

    use Math::BigFloat;

    $x = Math::BigFloat->new("1234567889123456789123456789123456789");

Without the quotes Perl converts the large number to a floating point constant
at compile time, and then converts the result to a Math::BigFloat object at
runtime, which results in an inaccurate result.

=head2 Hexadecimal, octal, and binary floating point literals

Perl (and this module) accepts hexadecimal, octal, and binary floating point
literals, but use them with care with Perl versions before v5.32.0, because
some versions of Perl silently give the wrong result. Below are some examples
of different ways to write the number decimal 314.

Hexadecimal floating point literals:

    0x1.3ap+8         0X1.3AP+8
    0x1.3ap8          0X1.3AP8
    0x13a0p-4         0X13A0P-4

Octal floating point literals (with "0" prefix):

    01.164p+8         01.164P+8
    01.164p8          01.164P8
    011640p-4         011640P-4

Octal floating point literals (with "0o" prefix) (requires v5.34.0):

    0o1.164p+8        0O1.164P+8
    0o1.164p8         0O1.164P8
    0o11640p-4        0O11640P-4

Binary floating point literals:

    0b1.0011101p+8    0B1.0011101P+8
    0b1.0011101p8     0B1.0011101P8
    0b10011101000p-2  0B10011101000P-2

=head2 Math library

Math with the numbers is done (by default) by a module called
Math::BigInt::Calc. This is equivalent to saying:

    use Math::BigFloat lib => "Calc";

You can change this by using:

    use Math::BigFloat lib => "GMP";

B<Note>: General purpose packages should not be explicit about the library to
use; let the script author decide which is best.

Note: The keyword 'lib' will warn when the requested library could not be
loaded. To suppress the warning use 'try' instead:

    use Math::BigFloat try => "GMP";

If your script works with huge numbers and Calc is too slow for them, you can
also for the loading of one of these libraries and if none of them can be used,
the code will die:

    use Math::BigFloat only => "GMP,Pari";

The following would first try to find Math::BigInt::Foo, then
Math::BigInt::Bar, and when this also fails, revert to Math::BigInt::Calc:

    use Math::BigFloat lib => "Foo,Math::BigInt::Bar";

See the respective low-level library documentation for further details.

See L<Math::BigInt> for more details about using a different low-level library.

=head1 EXPORTS

C<Math::BigFloat> exports nothing by default, but can export the L</bpi()>
method:

    use Math::BigFloat qw/bpi/;

    print bpi(10), "\n";

=over

=item Modifying and =

Beware of:

    $x = Math::BigFloat->new(5);
    $y = $x;

It will not do what you think, e.g. making a copy of $x. Instead it just makes
a second reference to the B<same> object and stores it in $y. Thus anything
that modifies $x will modify $y (except overloaded math operators), and vice
versa. See L<Math::BigInt> for details and how to avoid that.

=item precision() vs. accuracy()

A common pitfall is to use L</precision()> when you want to round a result to
a certain number of digits:

    use Math::BigFloat;

    Math::BigFloat->precision(4);           # does not do what you
                                            # think it does
    my $x = Math::BigFloat->new(12345);     # rounds $x to "12000"!
    print "$x\n";                           # print "12000"
    my $y = Math::BigFloat->new(3);         # rounds $y to "0"!
    print "$y\n";                           # print "0"
    $z = $x / $y;                           # 12000 / 0 => NaN!
    print "$z\n";
    print $z->precision(),"\n";             # 4

Replacing L</precision()> with L</accuracy()> is probably not what you want,
either:

    use Math::BigFloat;

    Math::BigFloat->accuracy(4);          # enables global rounding:
    my $x = Math::BigFloat->new(123456);  # rounded immediately
                                          #   to "12350"
    print "$x\n";                         # print "123500"
    my $y = Math::BigFloat->new(3);       # rounded to "3
    print "$y\n";                         # print "3"
    print $z = $x->copy()->bdiv($y),"\n"; # 41170
    print $z->accuracy(),"\n";            # 4

What you want to use instead is:

    use Math::BigFloat;

    my $x = Math::BigFloat->new(123456);    # no rounding
    print "$x\n";                           # print "123456"
    my $y = Math::BigFloat->new(3);         # no rounding
    print "$y\n";                           # print "3"
    print $z = $x->copy()->bdiv($y,4),"\n"; # 41150
    print $z->accuracy(),"\n";              # undef

In addition to computing what you expected, the last example also does B<not>
"taint" the result with an accuracy or precision setting, which would
influence any further operation.

=back

=head1 BUGS

Please report any bugs or feature requests to
C<bug-math-bigint at rt.cpan.org>, or through the web interface at
L<https://rt.cpan.org/Ticket/Create.html?Queue=Math-BigInt> (requires login).
We will be notified, and then you'll automatically be notified of progress on
your bug as I make changes.

=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc Math::BigFloat

You can also look for information at:

=over 4

=item * GitHub

L<https://github.com/pjacklam/p5-Math-BigInt>

=item * RT: CPAN's request tracker

L<https://rt.cpan.org/Dist/Display.html?Name=Math-BigInt>

=item * MetaCPAN

L<https://metacpan.org/release/Math-BigInt>

=item * CPAN Testers Matrix

L<http://matrix.cpantesters.org/?dist=Math-BigInt>

=back

=head1 LICENSE

This program is free software; you may redistribute it and/or modify it under
the same terms as Perl itself.

=head1 SEE ALSO

L<Math::BigInt> and L<Math::BigRat> as well as the backend libraries
L<Math::BigInt::FastCalc>, L<Math::BigInt::GMP>, and L<Math::BigInt::Pari>,
L<Math::BigInt::GMPz>, and L<Math::BigInt::BitVect>.

The pragmas L<bigint>, L<bigfloat>, and L<bigrat> might also be of interest. In
addition there is the L<bignum> pragma which does upgrading and downgrading.

=head1 AUTHORS

=over 4

=item *

Mark Biggar, overloaded interface by Ilya Zakharevich, 1996-2001.

=item *

Completely rewritten by Tels L<http://bloodgate.com> in 2001-2008.

=item *

Florian Ragwitz E<lt>flora@cpan.orgE<gt>, 2010.

=item *

Peter John Acklam E<lt>pjacklam@gmail.comE<gt>, 2011-.

=back

=cut
