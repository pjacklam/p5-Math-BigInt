# -*- coding: utf-8-unix -*-

package Math::BigInt;

#
# "Mike had an infinite amount to do and a negative amount of time in which
# to do it." - Before and After
#

# The following hash values are used:
#
#          sign : "+", "-", "+inf", "-inf", or "NaN"
#         value : unsigned int with actual value ($LIB thingy)
#      accuracy : accuracy (scalar)
#     precision : precision (scalar)

# Remember not to take shortcuts ala $xs = $x->{value}; $LIB->foo($xs); since
# underlying lib might change the reference!

use 5.006001;
use strict;
use warnings;

use Carp          qw< carp croak >;
use Scalar::Util  qw< blessed refaddr >;

our $VERSION = '2.005003';
$VERSION =~ tr/_//d;

require Exporter;
our @ISA       = qw< Exporter >;
our @EXPORT_OK = qw< objectify bgcd blcm >;

# Inside overload, the first arg is always an object. If the original code had
# it reversed (like $x = 2 * $y), then the third parameter is true.
# In some cases (like add, $x = $x + 2 is the same as $x = 2 + $x) this makes
# no difference, but in some cases it does.

# For overloaded ops with only one argument we simple use $_[0]->copy() to
# preserve the argument.

# Thus inheritance of overload operators becomes possible and transparent for
# our subclasses without the need to repeat the entire overload section there.

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
# global constants, flags and accessory

# These vars are public, but their direct usage is not recommended, use the
# accessor methods instead

our $accuracy   = undef;
our $precision  = undef;

our $round_mode = 'even';       # even, odd, +inf, -inf, zero, trunc, common
our $div_scale  = 40;

our $upgrade    = undef;
our $downgrade  = undef;

our $_trap_nan  = 0;            # croak on NaNs?
our $_trap_inf  = 0;            # croak on Infs?

my $nan = 'NaN';                # constant for easier life

# Module to do the low level math.

my $DEFAULT_LIB = 'Math::BigInt::Calc';
my $LIB;

# Has import() been called yet? This variable is needed to make "require" work.

my $IMPORT = 0;

##############################################################################
# the old code had $rnd_mode, so we need to support it, too

our $rnd_mode   = 'even';

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
    # tie to enable $rnd_mode to work transparently
    tie $rnd_mode, 'Math::BigInt';

    # set up some handy alias names
    *is_pos = \&is_positive;
    *is_neg = \&is_negative;
    *as_number = \&as_int;
}

###############################################################################
# Configuration methods
###############################################################################

sub accuracy {
    my $x = shift;
    my $class = ref($x) || $x || __PACKAGE__;

    # setter/mutator

    if (@_) {
        my $a = shift;

        if (defined $a) {
            $a = $a -> can('numify') ? $a -> numify() : 0 + "$a" if ref($a);
            croak "accuracy must be a number, not '$a'"
              if $a !~ /^\s*[+-]?(?:\d+(?:\.\d*)?|\.\d+)(?:[Ee][+-]?\d+)?\s*\z/;
            croak "accuracy must be an integer, not '$a'"
              if $a != int $a;
        }

        if (ref($x)) {
            $x -> bround($a) if defined $a;
            $x -> {precision} = undef;          # clear instance P
            $x -> {accuracy}  = $a;             # set instance A
        } else {
            no strict 'refs';
            ${"${class}::precision"} = undef;   # clear class P
            ${"${class}::accuracy"}  = $a;      # set class A
        }
    }

    # getter/accessor

    else {
        if (ref($x)) {
            return $x -> {accuracy};
        } else {
            no strict 'refs';
            return ${"${class}::accuracy"};
        }
    }
}

sub precision {
    my $x = shift;
    my $class = ref($x) || $x || __PACKAGE__;

    # setter/mutator

    if (@_) {
        my $p = shift;

        if (defined $p) {
            $p = $p -> can('numify') ? $p -> numify() : 0 + "$p" if ref($p);
            croak "precision must be a number, not '$p'"
              if $p !~ /^\s*[+-]?(?:\d+(?:\.\d*)?|\.\d+)(?:[Ee][+-]?\d+)?\s*\z/;
            croak "precision must be an integer, not '$p'"
              if $p != int $p;
        }

        if (ref($x)) {
            $x -> bfround($p) if defined $p;
            $x -> {accuracy}  = undef;          # clear instance A
            $x -> {precision} = $p;             # set instance P
        } else {
            no strict 'refs';
            ${"${class}::accuracy"}  = undef;   # clear class A
            ${"${class}::precision"} = $p;      # set class P
        }
    }

    # getter/accessor

    else {
        if (ref($x)) {
            return $x -> {precision};
        } else {
            no strict 'refs';
            return ${"${class}::precision"};
        }
    }
}

sub round_mode {
    my $self = shift;
    my $class = ref($self) || $self || __PACKAGE__;

    # setter/mutator

    if (@_) {
        my $m = shift;
        croak("The value for 'round_mode' must be defined")
          unless defined $m;
        croak("Unknown round mode '$m'")
          unless $m =~ /^(even|odd|\+inf|\-inf|zero|trunc|common)$/;

        if (ref($self) && exists $self -> {round_mode}) {
            $self->{round_mode} = $m;
        } else {
            no strict 'refs';
            ${"${class}::round_mode"}  = $m;
        }
    }

    # getter/accessor

    else {
        if (ref($self) && exists $self -> {round_mode}) {
            return $self->{round_mode};
        } else {
            no strict 'refs';
            my $m = ${"${class}::round_mode"};
            return defined($m) ? $m : $round_mode;
        }
    }
}

sub div_scale {
    my $self = shift;
    my $class = ref($self) || $self || __PACKAGE__;

    # setter/mutator

    if (@_) {
        my $f = shift;
        croak("The value for 'div_scale' must be defined") unless defined $f;
        $f = $f -> can('numify') ? $f -> numify() : 0 + "$f" if ref($f);
        # also croak on non-numerical
        croak "div_scale must be a number, not '$f'"
          unless $f =~/^[+-]?(?:\d+(?:\.\d*)?|\.\d+)(?:[Ee][+-]?\d+)?\z/;
        croak "div_scale must be an integer, not '$f'"
          if $f != int $f;
        # It is not documented what div_scale <= 0 means, but Astro::Units sets
        # div_scale to 0 and fails its tests if this is not supported. So we
        # silently support div_scale = 0.
        croak "div_scale must be positive, not '$f'" if $f < 0;

        if (ref($self) && exists $self -> {div_scale}) {
            $self -> {div_scale} = $f;
        } else {
            no strict 'refs';
            ${"${class}::div_scale"} = $f;
        }
    }

    # getter/accessor

    else {
        if (ref($self) && exists $self -> {div_scale}) {
            return $self -> {div_scale};
        } else {
            no strict 'refs';
            my $f = ${"${class}::div_scale"};
            return defined($f) ? $f : $div_scale;
        }
    }
}

sub trap_inf {
    my $self = shift;
    my $class = ref($self) || $self || __PACKAGE__;

    # setter/mutator

    if (@_) {
        my $b = shift() ? 1 : 0;
        if (ref($self) && exists $self -> {trap_inf}) {
            $self -> {trap_inf} = $b;
        } else {
            no strict 'refs';
            ${"${class}::_trap_inf"} = $b;
        }
    }

    # getter/accessor

    else {
        if (ref($self) && exists $self -> {trap_inf}) {
            return $self -> {trap_inf};
        } else {
            no strict 'refs';
            return ${"${class}::_trap_inf"};
        }
    }
}

sub trap_nan {
    my $self = shift;
    my $class = ref($self) || $self || __PACKAGE__;

    # setter/mutator

    if (@_) {
        my $b = shift() ? 1 : 0;
        if (ref($self) && exists $self -> {trap_nan}) {
            $self -> {trap_nan} = $b;
        } else {
            no strict 'refs';
            ${"${class}::_trap_nan"} = $b;
        }
    }

    # getter/accessor

    else {
        if (ref($self) && exists $self -> {trap_nan}) {
            return $self -> {trap_nan};
        } else {
            no strict 'refs';
            return ${"${class}::_trap_nan"};
        }
    }
}

sub upgrade {
    my $self = shift;
    my $class = ref($self) || $self || __PACKAGE__;

    # setter/mutator

    if (@_) {
        my $u = shift;
        if (ref($self) && exists $self -> {upgrade}) {
            $self -> {upgrade} = $u;
        } else {
            no strict 'refs';
            ${"${class}::upgrade"} = $u;
        }
    }

    # getter/accessor

    else {
        if (ref($self) && exists $self -> {upgrade}) {
            return $self -> {upgrade};
        } else {
            no strict 'refs';
            return ${"${class}::upgrade"};
        }
    }
}

sub downgrade {
    my $self = shift;
    my $class = ref($self) || $self || __PACKAGE__;

    # setter/mutator

    if (@_) {
        my $d = shift;
        if (ref($self) && exists $self -> {downgrade}) {
            $self -> {downgrade} = $d;
        } else {
            no strict 'refs';
            ${"${class}::downgrade"} = $d;
        }
    }

    # getter/accessor

    else {
        if (ref($self) && exists $self -> {downgrade}) {
            return $self -> {downgrade};
        } else {
            no strict 'refs';
            return ${"${class}::downgrade"};
        }
    }
}

sub modify () {
    # This method returns 0 if the object can be modified, or 1 if not. We use
    # a fast constant sub() here, to avoid costly calls. Subclasses may
    # override it with special code (f.i. Math::BigInt::Constant does so).

    0;
}

sub config {
    # return (or set) configuration data.
    my $self  = shift;
    my $class = ref($self) || $self || __PACKAGE__;

    # setter/mutator
    #
    # $self -> config(param => value, ...)
    # $self -> config({ param => value, ... })

    if (@_ > 1 || (@_ == 1 && (ref($_[0]) eq 'HASH'))) {
        # try to set given options as arguments from hash

        # If the argument is a hash ref, make a copy of it, since hash keys
        # will be deleted below, and we don't want to modify the input hash.

        my $args = ref($_[0]) eq 'HASH' ? { %{ $_[0] } }: { @_ };

        # We use this special handling of accuracy and precision because
        # accuracy() always sets precision to undef and precision() always sets
        # accuracy to undef. With out this special treatment, the following
        # would result in both accuracy and precision being undef.
        #
        #   $x -> config(accuracy => 3, precision => undef)

        croak "config(): both accuracy and precision are defined"
          if defined($args -> {accuracy}) && defined ($args -> {precision});

        if (defined $args -> {accuracy}) {
            $self -> accuracy($args -> {accuracy});
        } elsif (defined $args -> {precision}) {
            $self -> precision($args -> {precision});
        } else {
            $self -> accuracy(undef);  # also sets precision to undef
        }

        delete $args->{accuracy};
        delete $args->{precision};

        # Set any remaining hash keys.

        foreach my $key (qw/
                               round_mode div_scale
                               upgrade downgrade
                               trap_inf trap_nan
                           /)
        {
            # use a method call to check argument
            $self -> $key($args->{$key}) if exists $args->{$key};
            delete $args->{$key};
        }

        # If there are any keys left, they are invalid.

        if (keys %$args) {
            croak("Illegal key(s) '", join("', '", keys %$args),
                        "' passed to ${class}->config()");
        }
    }

    # getter/accessor

    my $cfg = {};

    # When only a single parameter is wanted, it is not necessary to build the
    # whole configuration first.

    if (@_ == 1 && (ref($_[0]) ne 'HASH')) {
        my $param = shift;

        return $LIB                if $param eq 'lib';
        return $LIB -> VERSION()   if $param eq 'lib_version';
        return $class              if $param eq 'class';
        return $class -> VERSION() if $param eq 'version';

        # $x -> config("param") or $class -> config("param")

        return $self -> $param();
    }

    else {

        if (ref($self)) {               # $x -> config()

            # Currently, only 'accuracy' and 'precision' are supported, but
            # more parameters will be added as the global variables are moved
            # into the OO interface.

            my @param = ('accuracy', 'precision');

            for my $param (@param) {
                $cfg -> {$param} = $self -> {$param};
            }

        } else {                        # $class -> config()

            my @param = ('accuracy', 'precision', 'round_mode', 'div_scale',
                         'upgrade', 'downgrade', 'trap_inf', 'trap_nan');

            for my $param (@param) {
                $cfg -> {$param} = $self -> $param();
            }

            # Additional read-only parameters.

            $cfg -> {lib}         = $LIB;
            $cfg -> {lib_version} = $LIB -> VERSION();
            $cfg -> {class}       = $class;
            $cfg -> {version}     = $class -> VERSION();
        }

        return $cfg;
    }
}

sub _scale_a {
    # select accuracy parameter based on precedence,
    # used by bround() and bfround(), may return undef for scale (means no op)
    my ($x, $scale, $mode) = @_;

    $scale = $x->{accuracy} unless defined $scale;

    my $class = ref($x);

    $mode = $class -> round_mode() unless defined $mode;

    if (defined $scale) {
        $scale = $scale -> can('numify') ? $scale -> numify()
                                         : "$scale" if ref($scale);
        $scale = int($scale);
    }

    ($scale, $mode);
}

sub _scale_p {
    # select precision parameter based on precedence,
    # used by bround() and bfround(), may return undef for scale (means no op)
    my ($x, $scale, $mode) = @_;

    $scale = $x->{precision} unless defined $scale;

    my $class = ref($x);

    $scale = $class -> precision() unless defined $scale;
    $mode = $class -> round_mode() unless defined $mode;

    if (defined $scale) {
        $scale = $scale -> can('numify') ? $scale -> numify()
                                         : "$scale" if ref($scale);
        $scale = int($scale);
    }

    ($scale, $mode);
}

# An undocumented method which downgrades an instance to its downgrade class.

sub _dng {
    my $self  = shift;
    my $class = ref($self);

    my $downgrade = $class -> downgrade();
    return $self unless $downgrade;           # bail out if no downgrading
    return $self if ref($self) eq $downgrade; # bail out if already downgraded

    # new() might perform upgrading or downgrading, so temporarily disable
    # upgrading and downgrading in the downgrade class while calling new(). It
    # should be possible to give new() extra arguments that disable
    # downgrading. XXX

    my $upg = $downgrade -> upgrade();
    my $dng = $downgrade -> downgrade();

    $downgrade -> upgrade(undef);
    $downgrade -> downgrade(undef);

    my $tmp = $downgrade -> new($self);       # new instance

    $downgrade -> upgrade($upg);
    $downgrade -> downgrade($dng);

    for my $param ('accuracy', 'precision') { # copy instance variables
        $tmp -> {$param} = $self -> {$param} if exists $self -> {$param};
    }

    %$self = %$tmp;                           # replace
    bless $self, $downgrade;                  # bless into downgrade class

    return $self;
}

# An undocumented method which upgrades an instance to its upgrade class.

sub _upg {
    my $self = shift;
    my $class = ref($self);

    my $upgrade = $class -> upgrade();
    return $self unless $upgrade;             # bail out if no upgrading
    return $self if ref($self) eq $upgrade;   # bail out if already upgraded

    # new() might perform upgrading or downgrading, so temporarily disable
    # upgrading and downgrading in the upgrade class while calling new(). It
    # should be possible to give new() extra arguments that disable
    # upgrading. XXX

    my $upg = $upgrade -> upgrade();
    my $dng = $upgrade -> downgrade();

    $upgrade -> upgrade(undef);
    $upgrade -> downgrade(undef);

    my $tmp = $upgrade -> new($self);         # new instance

    $upgrade -> upgrade($upg);
    $upgrade -> downgrade($dng);

    for my $param ('accuracy', 'precision') { # copy instance variables
        $tmp -> {$param} = $self -> {$param} if exists $self -> {$param};
    }

    %$self = %$tmp;                           # replace
    bless $self, $upgrade;                    # bless into upgrade class

    return $self;
}

###############################################################################
# Constructor methods
###############################################################################

sub _init {
    my $self  = shift;
    my $class = ref($self);

    $self -> SUPER::_init() if SUPER -> can('_init');

    $self -> {accuracy}   = $class -> accuracy();
    $self -> {precision}  = $class -> precision();

    #$self -> {round_mode} = $round_mode;
    #$self -> {div_scale}  = $div_scale;

    #$self -> {trap_inf}   = $_trap_inf;
    #$self -> {trap_nan}   = $_trap_nan;

    #$self -> {upgrade}    = $upgrade;
    #$self -> {downgrade}  = $downgrade;

    return $self;
}

sub new {
    # Create a new Math::BigInt object from a string or another Math::BigInt,
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
        #carp("Use of uninitialized value in new()")
        #  if warnings::enabled("uninitialized");
        return $class -> bzero(@r);
    }

    if (!ref($wanted) && $wanted eq "") {
        #carp(q|Argument "" isn't numeric in new()|)
        #  if warnings::enabled("numeric");
        #return $class -> bzero(@r);
        return $class -> bnan(@r);
    }

    # Initialize a new object.

    $self = bless {}, $class;
    #$self -> _init();   # <-- this causes problems because if the global
                         # accuracy is 2, new(3, 5) will not set the accuracy
                         # to 5 because it is currently not possible to
                         # increase the accuracy. Ditto for precision. XXX

    # See if $wanted is an object that is a Math::BigInt. We could check if the
    # object supports the as_int() method. However, as_int() truncates a finite
    # non-integer whereas new() is supposed to return a NaN for finite
    # non-integers. This inconsistency should be sorted out. XXX

    if (defined(blessed($wanted)) && $wanted -> isa(__PACKAGE__)) {

        # Don't copy the accuracy and precision, because a new object should
        # get them from the global configuration.

        $self -> {sign}  = $wanted -> {sign};
        $self -> {value} = $LIB -> _copy($wanted -> {value});
        $self -> round(@r)
          unless @r >= 2 && !defined($r[0]) && !defined($r[1]);
        return $self;
    }

    # From now on we only work on the stringified version of $wanted, so
    # stringify it once and for all.

    $wanted = "$wanted";

    # Shortcut for non-zero scalar integers with no non-zero exponent.

    if ($wanted =~
        / ^

          # optional leading whitespace
          \s*

          # optional sign
          ( [+-]? )

          # integer mantissa with optional leading zeros
          0* ( [1-9] \d* (?: _ \d+ )* | 0 )

          # ... with optional zero fraction part
          (?: \.0* )?

          # optional non-negative exponent
          (?: [eE] \+? ( \d+ (?: _ \d+ )* ) )?

          # optional trailing whitespace
          \s*

          $
        /x)
    {
        my $sign = $1;
        (my $mant = $2) =~ tr/_//d;
        my $expo = $3;
        $mant .= "0" x $expo if defined($expo) && $mant ne "0";

        $self->{sign} = $sign eq "-" && $mant ne "0" ? "-" : "+";
        $self->{value} = $LIB->_new($mant);
        $self -> round(@r);
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
        # The value is an integer iff the exponent is non-negative.

        if ($parts[2] eq '+') {
            $self -> {sign}  = $parts[0];
            $self -> {value} = $LIB -> _lsft($parts[1], $parts[3], 10);
            $self -> round(@r)
              unless @r >= 2 && !defined($r[0]) && !defined($r[1]);
            return $self;
        }

        # The value is not an integer, so upgrade if upgrading is enabled.

        my $upg = $class -> upgrade();
        return $upg -> new($wanted, @r) if $upg;
    }

    # If we get here, the value is neither a valid decimal, binary, octal, or
    # hexadecimal number. It is not explicit an Inf or a NaN either.

    return $class -> bnan(@r);
}

# Create a Math::BigInt from a decimal string. This is an equivalent to
# from_hex(), from_oct(), and from_bin(). It is like new() except that it does
# not accept anything but a string representing a finite decimal number.

sub from_dec {
    my $self    = shift;
    my $selfref = ref $self;
    my $class   = $selfref || $self;

    # Make "require" work.

    $class -> import() if $IMPORT == 0;

    # Don't modify constant (read-only) objects.

    return $self if $selfref && $self -> modify('from_dec');

    my $str = shift;
    my @r   = @_;

    if (my @parts = $class -> _dec_str_to_flt_lib_parts($str)) {

        # If called as a class method, initialize a new object.

        unless ($selfref) {
            $self = bless {}, $class;
            #$self -> _init();           # see comment on _init() in new()
        }

        # The value is an integer iff the exponent is non-negative.

        if ($parts[2] eq '+') {
            $self -> {sign}  = $parts[0];
            $self -> {value} = $LIB -> _lsft($parts[1], $parts[3], 10);
            return $self -> round(@r);
        }

        # The value is not an integer, so upgrade if upgrading is enabled.

        my $upg = $class -> upgrade();
        if ($upg) {
            return $self -> _upg() -> from_dec($str, @r)    # instance method
              if $selfref && $selfref ne $upg;
            return $upg -> from_dec($str, @r);              # class method
        }
    }

    return $self -> bnan(@r);
}

# Create a Math::BigInt from a hexadecimal string.

sub from_hex {
    my $self    = shift;
    my $selfref = ref $self;
    my $class   = $selfref || $self;

    # Make "require" work.

    $class -> import() if $IMPORT == 0;

    # Don't modify constant (read-only) objects.

    return $self if $selfref && $self -> modify('from_hex');

    my $str = shift;
    my @r   = @_;

    if (my @parts = $class -> _hex_str_to_flt_lib_parts($str)) {

        # If called as a class method, initialize a new object.

        unless ($selfref) {
            $self = bless {}, $class;
            #$self -> _init();           # see comment on _init() in new()
        }

        # The value is an integer iff the exponent is non-negative.

        if ($parts[2] eq '+') {
            $self -> {sign}  = $parts[0];
            $self -> {value} = $LIB -> _lsft($parts[1], $parts[3], 10);
            return $self -> round(@r);
        }

        # The value is not an integer, so upgrade if upgrading is enabled.

        my $upg = $class -> upgrade();
        if ($upg) {
            return $self -> _upg() -> from_hex($str, @r)    # instance method
              if $selfref && $selfref ne $upg;
            return $upg -> from_hex($str, @r);              # class method
        }
    }

    return $self -> bnan(@r);
}

# Create a Math::BigInt from an octal string.

sub from_oct {
    my $self    = shift;
    my $selfref = ref $self;
    my $class   = $selfref || $self;

    # Make "require" work.

    $class -> import() if $IMPORT == 0;

    # Don't modify constant (read-only) objects.

    return $self if $selfref && $self -> modify('from_oct');

    my $str = shift;
    my @r   = @_;

    if (my @parts = $class -> _oct_str_to_flt_lib_parts($str)) {

        # If called as a class method, initialize a new object.

        unless ($selfref) {
            $self = bless {}, $class;
            #$self -> _init();           # see comment on _init() in new()
        }

        # The value is an integer iff the exponent is non-negative.

        if ($parts[2] eq '+') {
            $self -> {sign}  = $parts[0];
            $self -> {value} = $LIB -> _lsft($parts[1], $parts[3], 10);
            return $self -> round(@r);
        }

        # The value is not an integer, so upgrade if upgrading is enabled.

        my $upg = $class -> upgrade();
        if ($upg) {
            return $self -> _upg() -> from_oct($str, @r)    # instance method
              if $selfref && $selfref ne $upg;
            return $upg -> from_oct($str, @r);              # class method
        }
    }

    return $self -> bnan(@r);
}

# Create a Math::BigInt from a binary string.

sub from_bin {
    my $self    = shift;
    my $selfref = ref $self;
    my $class   = $selfref || $self;

    # Make "require" work.

    $class -> import() if $IMPORT == 0;

    # Don't modify constant (read-only) objects.

    return $self if $selfref && $self -> modify('from_bin');

    my $str = shift;
    my @r   = @_;

    if (my @parts = $class -> _bin_str_to_flt_lib_parts($str)) {

        # If called as a class method, initialize a new object.

        unless ($selfref) {
            $self = bless {}, $class;
            #$self -> _init();           # see comment on _init() in new()
        }

        # The value is an integer iff the exponent is non-negative.

        if ($parts[2] eq '+') {
            $self -> {sign}  = $parts[0];
            $self -> {value} = $LIB -> _lsft($parts[1], $parts[3], 10);
            return $self -> round(@r);
        }

        # The value is not an integer, so upgrade if upgrading is enabled.

        my $upg = $class -> upgrade();
        if ($upg) {
            return $self -> _upg() -> from_bin($str, @r)    # instance method
              if $selfref && $selfref ne $upg;
            return $upg -> from_bin($str, @r);              # class method
        }
    }

    return $self -> bnan(@r);
}

# Create a Math::BigInt from a byte string.

sub from_bytes {
    my $self    = shift;
    my $selfref = ref $self;
    my $class   = $selfref || $self;

    # Make "require" work.

    $class -> import() if $IMPORT == 0;

    # Don't modify constant (read-only) objects.

    return $self if $selfref && $self -> modify('from_bytes');

    croak("from_bytes() requires a newer version of the $LIB library.")
        unless $LIB -> can('_from_bytes');

    my $str = shift;
    my @r = @_;

    # If called as a class method, initialize a new object.

    $self = $class -> bzero(@r) unless $selfref;
    $self -> {sign}  = '+';
    $self -> {value} = $LIB -> _from_bytes($str);
    return $self -> round(@r);
}

sub from_ieee754 {
    my $self    = shift;
    my $selfref = ref $self;
    my $class   = $selfref || $self;

    # Make "require" work.

    $class -> import() if $IMPORT == 0;

    # Don't modify constant (read-only) objects.

    return $self if $selfref && $self -> modify('from_ieee754');

    my $in     = shift;
    my $format = shift;
    my @r      = @_;

    require Math::BigFloat;
    my $tmp = Math::BigFloat -> from_ieee754($in, $format, @r);
    return $self -> bnan(@r) unless $tmp -> is_inf() || $tmp -> is_int();
    $tmp = $tmp -> as_int();

    # If called as a class method, initialize a new object.

    $self = $class -> bzero(@r) unless $selfref;
    $self -> {sign}  = $tmp -> {sign};
    $self -> {value} = $tmp -> {value};

    return $self;
}

sub from_fp80 {
    my $self    = shift;
    my $selfref = ref $self;
    my $class   = $selfref || $self;

    # Make "require" work.

    $class -> import() if $IMPORT == 0;

    # Don't modify constant (read-only) objects.

    return $self if $selfref && $self -> modify('from_fp80');

    my $in = shift;
    my @r  = @_;

    require Math::BigFloat;
    my $tmp = Math::BigFloat -> from_fp80($in, @r);
    return $self -> bnan(@r) unless $tmp -> is_inf() || $tmp -> is_int();
    $tmp = $tmp -> as_int();

    # If called as a class method, initialize a new object.

    $self = $class -> bzero(@r) unless $selfref;
    $self -> {sign}  = $tmp -> {sign};
    $self -> {value} = $tmp -> {value};

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

    $self -> {sign}  = '+';
    $self -> {value} = $LIB->_from_base($str, $base -> {value},
                                        defined($cs) ? $cs : ());
    return $self -> bround(@r);
}

sub from_base_num {
    my $self    = shift;
    my $selfref = ref $self;
    my $class   = $selfref || $self;

    # Make "require" work.

    $class -> import() if $IMPORT == 0;

    # Don't modify constant (read-only) objects.

    return $self if $selfref && $self -> modify('from_base_num');

    # Make sure we have an array of non-negative, finite, numerical objects.

    my $nums = shift;
    $nums = [ @$nums ];         # create new reference

    for my $i (0 .. $#$nums) {
        # Make sure we have an object.
        $nums -> [$i] = $class -> new($nums -> [$i])
          unless defined(blessed($nums -> [$i]))
                   && $nums -> [$i] -> isa(__PACKAGE__);
        # Make sure we have a finite, non-negative integer.
        croak "the elements must be finite non-negative integers"
          if $nums -> [$i] -> is_neg() || ! $nums -> [$i] -> is_int();
    }

    my $base = shift;
    $base = $class -> new($base)
      unless defined(blessed($base)) && $base -> isa(__PACKAGE__);

    my @r = @_;

    # If called as a class method, initialize a new object.

    $self = $class -> bzero(@r) unless $selfref;

    croak("from_base_num() requires a newer version of the $LIB library.")
      unless $LIB -> can('_from_base_num');

    $self -> {sign}  = '+';
    $self -> {value} = $LIB -> _from_base_num([ map { $_ -> {value} } @$nums ],
                                           $base -> {value});

    return $self -> round(@r);
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

    # Get the rounding parameters, if any.

    my @r = @_;

    # If called as a class method, initialize a new object.

    unless ($selfref) {
        $self = bless {}, $class;
        #$self -> _init();              # see comment on _init() in new()
    }

    $self->{sign} = '+';
    $self->{value} = $LIB->_zero();

    # If rounding parameters are given as arguments, use them. If no rounding
    # parameters are given, and if called as a class method, initialize the new
    # instance with the class variables.

    if (@r) {
        if (@r >= 2 && defined($r[0]) && defined($r[1])) {
            carp "can't specify both accuracy and precision";
            return $self -> bnan();
        }
        $self->{accuracy} = $_[0];
        $self->{precision} = $_[1];
    } elsif (!$selfref) {
        $self->{accuracy} = $class -> accuracy();
        $self->{precision} = $class -> precision();
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

    my ($sign, @r) = @_;

    # Get the sign.

    if (defined($_[0]) && $_[0] =~ /^\s*([+-])\s*$/) {
        $sign = $1;
        shift;
    } else {
        $sign = '+';
    }

    # If called as a class method, initialize a new object.

    unless ($selfref) {
        $self = bless {}, $class;
        #$self -> _init();              # see comment on _init() in new()
    }

    $self->{sign}  = $sign;
    $self->{value} = $LIB->_one();

    # If rounding parameters are given as arguments, use them. If no rounding
    # parameters are given, and if called as a class method, initialize the new
    # instance with the class variables.

    if (@r) {
        if (@r >= 2 && defined($r[0]) && defined($r[1])) {
            carp "can't specify both accuracy and precision";
            return $self -> bnan();
        }
        $self->{accuracy} = $_[0];
        $self->{precision} = $_[1];
    } elsif (!$selfref) {
        $self->{accuracy} = $class -> accuracy();
        $self->{precision} = $class -> precision();
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

    # If called as a class method, initialize a new object.

    unless ($selfref) {
        $self = bless {}, $class;
        #$self -> _init();              # see comment on _init() in new()
    }

    $self -> {sign}  = $sign . 'inf';
    $self -> {value} = $LIB -> _zero();

    # If rounding parameters are given as arguments, use them. If no rounding
    # parameters are given, and if called as a class method, initialize the new
    # instance with the class variables.

    if (@r) {
        if (@r >= 2 && defined($r[0]) && defined($r[1])) {
            carp "can't specify both accuracy and precision";
            return $self -> bnan();
        }
        $self->{accuracy} = $_[0];
        $self->{precision} = $_[1];
    } elsif (!$selfref) {
        $self->{accuracy} = $class -> accuracy();
        $self->{precision} = $class -> precision();
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
    my $selfref = ref($self);
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

    # Get the rounding parameters, if any.

    my @r = @_;

    # If called as a class method, initialize a new object.

    unless ($selfref) {
        $self = bless {}, $class;
        #$self -> _init();              # see comment on _init() in new()
    }

    $self -> {sign}  = $nan;
    $self -> {value} = $LIB -> _zero();

    # If rounding parameters are given as arguments, use them. If no rounding
    # parameters are given, and if called as a class method, initialize the new
    # instance with the class variables.

    if (@r) {
        if (@r >= 2 && defined($r[0]) && defined($r[1])) {
            carp "can't specify both accuracy and precision";
            return $self -> bnan();
        }
        $self->{accuracy} = $_[0];
        $self->{precision} = $_[1];
    } elsif (!$selfref) {
        $self->{accuracy} = $class -> accuracy();
        $self->{precision} = $class -> precision();
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
        #$self -> _init();              # see comment on _init() in new()
    }

    my $upg = $class -> upgrade();
    if ($upg) {
        return $self -> _upg() -> bpi(@r)        # instance method
          if $selfref && $selfref ne $upg;
        return $upg -> bpi(@r);                  # class method
    }

    # hard-wired to "3"
    $self -> {sign}  = '+';
    $self -> {value} = $LIB -> _new("3");
    $self -> round(@r);
    return $self;
}

sub copy {
    my ($x, $class);
    if (ref($_[0])) {           # $y = $x -> copy()
        $x = shift;
        $class = ref($x);
    } else {                    # $y = Math::BigInt -> copy($x)
        $class = shift;
        $x = shift;
    }

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @_;

    my $copy = bless {}, $class;

    $copy->{sign}      = $x->{sign};
    $copy->{value}     = $LIB->_copy($x->{value});
    $copy->{accuracy}  = $x->{accuracy}  if exists $x->{accuracy};
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

    # Temporarily disable upgrading and downgrading.

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

    # Restore upgrading and downgrading..

    Math::BigFloat -> upgrade($upg);
    Math::BigFloat -> downgrade($dng);

    return $y;
}

###############################################################################
# Boolean methods
###############################################################################

sub is_zero {
    # return true if arg (BINT or num_str) is zero (array '+', '0')
    my (undef, $x) = ref($_[0]) ? (undef, @_) : objectify(1, @_);

    return 0 if $x->{sign} ne '+';
    return 1 if $LIB->_is_zero($x->{value});
    return 0;
}

sub is_one {
    # return true if arg (BINT or num_str) is +1, or -1 if sign is given
    my (undef, $x, $sign) = ref($_[0]) ? (undef, @_) : objectify(1, @_);

    if (defined($sign)) {
        croak 'is_one(): sign argument must be "+" or "-"'
          unless $sign eq '+' || $sign eq '-';
    } else {
        $sign = '+';
    }

    return 0 if $x->{sign} ne $sign;
    $LIB->_is_one($x->{value}) ? 1 : 0;
}

sub is_finite {
    my (undef, $x) = ref($_[0]) ? (undef, @_) : objectify(1, @_);
    $x->{sign} eq '+' || $x->{sign} eq '-' ? 1 : 0;
}

sub is_inf {
    # return true if arg (BINT or num_str) is +-inf
    my (undef, $x, $sign) = ref($_[0]) ? (undef, @_) : objectify(1, @_);

    if (defined $sign) {
        $sign = '[+-]inf' if $sign eq ''; # +- doesn't matter, only that's inf
        $sign = "[$1]inf" if $sign =~ /^([+-])(inf)?$/; # extract '+' or '-'
        return $x->{sign} =~ /^$sign$/ ? 1 : 0;
    }
    $x->{sign} =~ /^[+-]inf$/ ? 1 : 0; # only +-inf is infinity
}

sub is_nan {
    # return true if arg (BINT or num_str) is NaN
    my (undef, $x) = ref($_[0]) ? (undef, @_) : objectify(1, @_);

    $x->{sign} eq $nan ? 1 : 0;
}

sub is_positive {
    # return true when arg (BINT or num_str) is positive (> 0)
    my (undef, $x) = ref($_[0]) ? (undef, @_) : objectify(1, @_);

    return 1 if $x -> is_inf("+");

    # 0+ is neither positive nor negative
    ($x->{sign} eq '+' && !$x -> is_zero()) ? 1 : 0;
}

sub is_negative {
    # return true when arg (BINT or num_str) is negative (< 0)
    my (undef, $x) = ref($_[0]) ? (undef, @_) : objectify(1, @_);

    $x->{sign} =~ /^-/ ? 1 : 0; # -inf is negative, but NaN is not
}

sub is_non_positive {
    # Return true if argument is non-positive (<= 0).
    my (undef, $x) = ref($_[0]) ? (undef, @_) : objectify(1, @_);

    return 1 if $x->{sign} =~ /^\-/;
    return 1 if $x -> is_zero();
    return 0;
}

sub is_non_negative {
    # Return true if argument is non-negative (>= 0).
    my (undef, $x) = ref($_[0]) ? (undef, @_) : objectify(1, @_);

    return 1 if $x->{sign} =~ /^\+/;
    return 1 if $x -> is_zero();
    return 0;
}

sub is_odd {
    # return true when arg (BINT or num_str) is odd, false for even
    my (undef, $x) = ref($_[0]) ? (undef, @_) : objectify(1, @_);

    return 0 unless $x -> is_finite();
    $LIB->_is_odd($x->{value}) ? 1 : 0;
}

sub is_even {
    # return true when arg (BINT or num_str) is even, false for odd
    my (undef, $x) = ref($_[0]) ? (undef, @_) : objectify(1, @_);

    return 0 unless $x -> is_finite();
    $LIB->_is_even($x->{value}) ? 1 : 0;
}

sub is_int {
    # return true when arg (BINT or num_str) is an integer
    my (undef, $x) = ref($_[0]) ? (undef, @_) : objectify(1, @_);

    $x -> is_finite() ? 1 : 0;
}

###############################################################################
# Comparison methods
###############################################################################

sub bcmp {
    # Compares 2 values.  Returns one of undef, <0, =0, >0. (suitable for sort)
    # (BINT or num_str, BINT or num_str) return cond_code

    # set up parameters
    my ($class, $x, $y, @r) = ref($_[0]) && ref($_[0]) eq ref($_[1])
                            ? (ref($_[0]), @_)
                            : objectify(2, @_);

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    ###########################################################################
    # Code for all classes that share the common interface.
    ###########################################################################

    # Unless both $x and $y are finite ...

    unless ($x -> is_finite() && $y -> is_finite()) {
        # handle +-inf and NaN
        return    if $x -> is_nan() || $y -> is_nan();
        return  0 if $x->{sign} eq $y->{sign} && $x->{sign} =~ /^[+-]inf$/;
        return +1 if $x -> is_inf("+");
        return -1 if $x -> is_inf("-");
        return -1 if $y -> is_inf("+");
        return +1;
    }

    # check sign for speed first
    return  1 if $x->{sign} eq '+' && $y->{sign} eq '-'; # does also 0 <=> -y
    return -1 if $x->{sign} eq '-' && $y->{sign} eq '+'; # does also -x <=> 0

    ###########################################################################
    # Code for things that aren't Math::BigInt
    ###########################################################################

    # If called with "foreign" arguments.

    unless ($y -> isa(__PACKAGE__)) {
        if ($y -> is_int()) {
            $y = $y -> as_int();
        } else {
            return $x -> _upg() -> bcmp($y, @r) if $class -> upgrade();
            croak "Can't handle a ", ref($y), " in ", (caller(0))[3], "()";
        }
    }

    ###########################################################################
    # Code for Math::BigInt objects
    ###########################################################################

    # post-normalized compare for internal use (honors signs)
    if ($x->{sign} eq '+') {
        # $x and $y both > 0
        return $LIB->_acmp($x->{value}, $y->{value});
    }

    # $x && $y both < 0; use swapped acmp (lib returns 0, 1, -1)
    $LIB->_acmp($y->{value}, $x->{value});
}

sub bacmp {
    # Compares 2 values, ignoring their signs.
    # Returns one of undef, <0, =0, >0. (suitable for sort)
    # (BINT, BINT) return cond_code

    # set up parameters
    my ($class, $x, $y, @r) = ref($_[0]) && ref($_[0]) eq ref($_[1])
                            ? (ref($_[0]), @_)
                            : objectify(2, @_);

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    ###########################################################################
    # Code for all classes that share the common interface.
    ###########################################################################

    if ((!$x -> is_finite()) || (!$y -> is_finite())) {
        # handle +-inf and NaN
        return    if $x -> is_nan() || $y -> is_nan();
        return  0 if $x->{sign} =~ /^[+-]inf$/ && $y->{sign} =~ /^[+-]inf$/;
        return  1 if $x->{sign} =~ /^[+-]inf$/ && $y->{sign} !~ /^[+-]inf$/;
        return -1;
    }

    ###########################################################################
    # Code for things that aren't Math::BigInt
    ###########################################################################

    # If called with "foreign" arguments.

    unless ($y -> isa(__PACKAGE__)) {
        if ($y -> is_int()) {
            $y = $y -> as_int();
        } else {
            return $x -> _upg() -> bacmp($y, @r) if $class -> upgrade();
            croak "Can't handle a ", ref($y), " in ", (caller(0))[3], "()";
        }
    }

    ###########################################################################
    # Code for Math::BigInt objects
    ###########################################################################

    $LIB->_acmp($x->{value}, $y->{value}); # lib does only 0, 1, -1
}

sub beq {
    my (undef, $x, $y, @r) = ref($_[0]) && ref($_[0]) eq ref($_[1])
                           ? (undef, @_)
                           : objectify(2, @_);

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    my $cmp = $x -> bcmp($y);           # bcmp() upgrades if necessary
    return defined($cmp) && !$cmp;
}

sub bne {
    my (undef, $x, $y, @r) = ref($_[0]) && ref($_[0]) eq ref($_[1])
                           ? (undef, @_)
                           : objectify(2, @_);

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    my $cmp = $x -> bcmp($y);           # bcmp() upgrades if necessary
    return defined($cmp) && !$cmp ? '' : 1;
}

sub blt {
    my (undef, $x, $y, @r) = ref($_[0]) && ref($_[0]) eq ref($_[1])
                           ? (undef, @_)
                           : objectify(2, @_);

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    my $cmp = $x -> bcmp($y);           # bcmp() upgrades if necessary
    return defined($cmp) && $cmp < 0;
}

sub ble {
    my (undef, $x, $y, @r) = ref($_[0]) && ref($_[0]) eq ref($_[1])
                           ? (undef, @_)
                           : objectify(2, @_);

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    my $cmp = $x -> bcmp($y);           # bcmp() upgrades if necessary
    return defined($cmp) && $cmp <= 0;
}

sub bgt {
    my (undef, $x, $y, @r) = ref($_[0]) && ref($_[0]) eq ref($_[1])
                           ? (undef, @_)
                           : objectify(2, @_);

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    my $cmp = $x -> bcmp($y);           # bcmp() upgrades if necessary
    return defined($cmp) && $cmp > 0;
}

sub bge {
    my (undef, $x, $y, @r) = ref($_[0]) && ref($_[0]) eq ref($_[1])
                           ? (undef, @_)
                           : objectify(2, @_);

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    my $cmp = $x -> bcmp($y);           # bcmp() upgrades if necessary
    return defined($cmp) && $cmp >= 0;
}

###############################################################################
# Arithmetic methods
###############################################################################

sub bneg {
    # negate number or make a negated number from string
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    ###########################################################################
    # Code for all classes that share the common interface.
    ###########################################################################

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('bneg');

    $x->{sign} =~ tr/+-/-+/ unless $x -> is_zero();
    $x -> round(@r);
    $x -> _dng() if ($x -> is_int() || $x -> is_inf() || $x -> is_nan());
    return $x;
}

sub babs {
    # (BINT or num_str) return BINT
    # make number absolute, or return absolute BINT from string
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('babs');

    $x->{sign} =~ s/^-/+/;

    $x -> round(@r);
    $x -> _dng() if ($x -> is_int() || $x -> is_inf() || $x -> is_nan());
    return $x;
}

sub bsgn {
    # Signum function.
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('bsgn');

    # bone() downgrades, if necessary

    return $x -> bone("+", @r) if $x -> is_pos();
    return $x -> bone("-", @r) if $x -> is_neg();

    $x -> round(@r);
    $x -> _dng() if $x -> is_int();
    return $x;
}

sub bnorm {
    # (numstr or BINT) return BINT
    # Normalize number -- no-op here
    my ($class, $x, @r) = ref($_[0]) ? (undef, $_[0]) : objectify(1, @_);

    # This method is called from the rounding methods, so if this method
    # supports rounding by calling the rounding methods, we get an infinite
    # recursion.

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    $x;
}

sub binc {
    # increment arg by one
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    ###########################################################################
    # Code for all classes that share the common interface.
    ###########################################################################

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('binc');

    return $x -> round(@r) if $x -> is_inf() || $x -> is_nan();

    ###########################################################################
    # Code for Math::BigInt objects
    ###########################################################################

    if ($x->{sign} eq '+') {
        $x->{value} = $LIB->_inc($x->{value});
    } elsif ($x->{sign} eq '-') {
        $x->{value} = $LIB->_dec($x->{value});
        $x->{sign} = '+' if $LIB->_is_zero($x->{value}); # -1 +1 => -0 => +0
    }

    return $x -> round(@r);
}

sub bdec {
    # decrement arg by one
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    ###########################################################################
    # Code for all classes that share the common interface.
    ###########################################################################

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('bdec');

    return $x -> round(@r) if $x -> is_inf() || $x -> is_nan();

    ###########################################################################
    # Code for Math::BigInt objects
    ###########################################################################

    if ($x->{sign} eq '-') {
        $x->{value} = $LIB->_inc($x->{value});
    } elsif ($x->{sign} eq '+') {
        if ($LIB->_is_zero($x->{value})) {      # +1 - 1 => +0
            $x->{value} = $LIB->_one();
            $x->{sign} = '-';
        } else {
            $x->{value} = $LIB->_dec($x->{value});
        }
    }

    return $x -> round(@r);
}

#sub bstrcmp {
#    my $self    = shift;
#    my $selfref = ref $self;
#    my $class   = $selfref || $self;
#
#    croak 'bstrcmp() is an instance method, not a class method'
#        unless $selfref;
#    croak 'Wrong number of arguments for bstrcmp()' unless @_ == 1;
#
#    return $self -> bstr() CORE::cmp shift;
#}
#
#sub bstreq {
#    my $self    = shift;
#    my $selfref = ref $self;
#    my $class   = $selfref || $self;
#
#    croak 'bstreq() is an instance method, not a class method'
#        unless $selfref;
#    croak 'Wrong number of arguments for bstreq()' unless @_ == 1;
#
#    my $cmp = $self -> bstrcmp(shift);
#    return defined($cmp) && ! $cmp;
#}
#
#sub bstrne {
#    my $self    = shift;
#    my $selfref = ref $self;
#    my $class   = $selfref || $self;
#
#    croak 'bstrne() is an instance method, not a class method'
#        unless $selfref;
#    croak 'Wrong number of arguments for bstrne()' unless @_ == 1;
#
#    my $cmp = $self -> bstrcmp(shift);
#    return defined($cmp) && ! $cmp ? '' : 1;
#}
#
#sub bstrlt {
#    my $self    = shift;
#    my $selfref = ref $self;
#    my $class   = $selfref || $self;
#
#    croak 'bstrlt() is an instance method, not a class method'
#        unless $selfref;
#    croak 'Wrong number of arguments for bstrlt()' unless @_ == 1;
#
#    my $cmp = $self -> bstrcmp(shift);
#    return defined($cmp) && $cmp < 0;
#}
#
#sub bstrle {
#    my $self    = shift;
#    my $selfref = ref $self;
#    my $class   = $selfref || $self;
#
#    croak 'bstrle() is an instance method, not a class method'
#        unless $selfref;
#    croak 'Wrong number of arguments for bstrle()' unless @_ == 1;
#
#    my $cmp = $self -> bstrcmp(shift);
#    return defined($cmp) && $cmp <= 0;
#}
#
#sub bstrgt {
#    my $self    = shift;
#    my $selfref = ref $self;
#    my $class   = $selfref || $self;
#
#    croak 'bstrgt() is an instance method, not a class method'
#        unless $selfref;
#    croak 'Wrong number of arguments for bstrgt()' unless @_ == 1;
#
#    my $cmp = $self -> bstrcmp(shift);
#    return defined($cmp) && $cmp > 0;
#}
#
#sub bstrge {
#    my $self    = shift;
#    my $selfref = ref $self;
#    my $class   = $selfref || $self;
#
#    croak 'bstrge() is an instance method, not a class method'
#        unless $selfref;
#    croak 'Wrong number of arguments for bstrge()' unless @_ == 1;
#
#    my $cmp = $self -> bstrcmp(shift);
#    return defined($cmp) && $cmp >= 0;
#}

sub badd {
    # add second arg (BINT or string) to first (BINT) (modifies first)
    # return result as BINT

    # set up parameters
    my ($class, $x, $y, @r) = ref($_[0]) && ref($_[0]) eq ref($_[1])
                            ? (ref($_[0]), @_)
                            : objectify(2, @_);

    return $x -> badd($y, @r) unless $x -> isa(__PACKAGE__);

    ###########################################################################
    # Code for all classes that share the common interface.
    ###########################################################################

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('badd');

    $r[3] = $y;                 # no push!

    unless ($x -> is_finite() && $y -> is_finite()) {

        return $x -> bnan(@r) if $x -> is_nan() || $y -> is_nan();

        return $x -> is_inf("+") ? ($y -> is_inf("-") ? $x -> bnan(@r)
                                                      : $x -> binf("+", @r))
             : $x -> is_inf("-") ? ($y -> is_inf("+") ? $x -> bnan(@r)
                                                      : $x -> binf("-", @r))
             :                     ($y -> is_inf("+") ? $x -> binf("+", @r)
                                                      : $x -> binf("-", @r));
    }

    ###########################################################################
    # Code for things that aren't Math::BigInt
    ###########################################################################

    # If called with "foreign" arguments.

    unless ($y -> isa(__PACKAGE__)) {
        if ($y -> is_int()) {
            $y = $y -> as_int();
        } else {
            return $x -> _upg() -> badd($y, @r) if $class -> upgrade();
            croak "Can't handle a ", ref($y), " in ", (caller(0))[3], "()";
        }
    }

    ###########################################################################
    # Code for Math::BigInt objects
    ###########################################################################

    ($x->{value}, $x->{sign})
      = $LIB -> _sadd($x->{value}, $x->{sign}, $y->{value}, $y->{sign});

    $x -> round(@r);
}

sub bsub {
    # (BINT or num_str, BINT or num_str) return BINT
    # subtract second arg from first, modify first

    # set up parameters
    my ($class, $x, $y, @r) = ref($_[0]) && ref($_[0]) eq ref($_[1])
                            ? (ref($_[0]), @_)
                            : objectify(2, @_);

    return $x -> bsub($y, @r) unless $x -> isa(__PACKAGE__);

    ###########################################################################
    # Code for all classes that share the common interface.
    ###########################################################################

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

    return $x -> bzero(@r) if refaddr($x) eq refaddr($y);   # $x -> bsub($x)

    ###########################################################################
    # Code for things that aren't Math::BigInt
    ###########################################################################

    # If called with "foreign" arguments.

    unless ($y -> isa(__PACKAGE__)) {
        if ($y -> is_int()) {
            $y = $y -> as_int();
        } else {
            return $x -> _upg() -> bsub($y, @r) if $class -> upgrade();
            croak "Can't handle a ", ref($y), " in ", (caller(0))[3], "()";
        }
    }

    ###########################################################################
    # Code for Math::BigInt objects
    ###########################################################################

    ($x->{value}, $x->{sign})
      = $LIB -> _ssub($x->{value}, $x->{sign}, $y->{value}, $y->{sign});

    $x -> round(@r);
}

sub bmul {
    # multiply the first number by the second number
    # (BINT or num_str, BINT or num_str) return BINT

    # set up parameters
    my ($class, $x, $y, @r) = ref($_[0]) && ref($_[0]) eq ref($_[1])
                            ? (ref($_[0]), @_)
                            : objectify(2, @_);

    ###########################################################################
    # Code for all classes that share the common interface.
    ###########################################################################

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('bmul');

    return $x -> bmul($y, @r) unless $x -> isa(__PACKAGE__);

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

    ###########################################################################
    # Code for things that aren't Math::BigInt
    ###########################################################################

    # If called with "foreign" arguments.

    unless ($y -> isa(__PACKAGE__)) {
        if ($y -> is_int()) {
            $y = $y -> as_int();
        } else {
            return $x -> _upg() -> bmul($y, @r) if $class -> upgrade();
            croak "Can't handle a ", ref($y), " in ", (caller(0))[3], "()";
        }
    }

    ###########################################################################
    # Code for Math::BigInt objects
    ###########################################################################

    $r[3] = $y;                 # no push here

    $x->{sign} = $x->{sign} eq $y->{sign} ? '+' : '-';

    $x->{value} = $LIB->_mul($x->{value}, $y->{value}); # do actual math
    $x->{sign} = '+' if $LIB->_is_zero($x->{value});   # no -0

    $x -> round(@r);
}

*bdiv = \&bfdiv;
*bmod = \&bfmod;

sub bfdiv {
    # This does floored division, where the quotient is floored, i.e., rounded
    # towards negative infinity. As a consequence, the remainder has the same
    # sign as the divisor.
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
    #     inf /    0 =  inf                  inf %    0 =  inf
    #       5 /    0 =  inf                    5 %    0 =    5
    #       0 /    0 =  NaN                    0 %    0 =    0
    #      -5 /    0 = -inf                   -5 %    0 =   -5
    #    -inf /    0 = -inf                 -inf %    0 = -inf

    if ($y -> is_zero()) {
        my $rem;
        if ($wantarray) {
            $rem = $x -> copy() -> round(@r);
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
            $x -> binf($sign, @r);
        }
        return $wantarray ? ($x, $rem) : $x;
    }

    # Denominator (divisor) is +/-inf. The cases when the numerator is +/-inf
    # are covered above. In the modulo cases (in the right column) we return
    # the same as core Perl, which does floored division, so for consistency we
    # also do floored division in the division cases (in the left column).
    #
    #      -5 /  inf =   -1                   -5 %  inf =  inf
    #       0 /  inf =    0                    0 %  inf =    0
    #       5 /  inf =    0                    5 %  inf =    5
    #
    #      -5 / -inf =    0                   -5 % -inf =   -5
    #       0 / -inf =    0                    0 % -inf =    0
    #       5 / -inf =   -1                    5 % -inf = -inf

    if ($y -> is_inf()) {
        my $rem;
        if ($x -> is_zero() || $x -> bcmp(0) == $y -> bcmp(0)) {
            $rem = $x -> copy() -> round(@r) if $wantarray;
            $x -> bzero(@r);
        } else {
            $rem = $class -> binf($y -> {sign}, @r) if $wantarray;
            $x -> bone('-', @r);
        }
        return $wantarray ? ($x, $rem) : $x;
    }

    ###########################################################################
    # Code for things that aren't Math::BigInt
    ###########################################################################

    # At this point, both the numerator and denominator are finite, non-zero
    # numbers.

    unless ($wantarray) {
        my $upg = $class -> upgrade();
        if ($upg) {
            my $tmp = $upg -> bfdiv($x, $y, @r);
            if ($tmp -> is_int()) {
                $tmp = $tmp -> as_int();
                %$x = %$tmp;
            } else {
                %$x = %$tmp;
                bless $x, $upg;
            }
            return $x;
        }
    }

    # If called with "foreign" arguments.

    unless ($y -> isa(__PACKAGE__)) {
        if ($y -> is_int()) {
            $y = $y -> as_int();
        } else {
            return $x -> _upg() -> bfdiv($y, @r) if $class -> upgrade();
            croak "Can't handle a ", ref($y), " in ", (caller(0))[3], "()";
        }
    }

    ###########################################################################
    # Code for Math::BigInt objects
    ###########################################################################

    $r[3] = $y;                                   # no push!

    # Initialize remainder.

    my $rem = $class -> bzero();

    # Are both operands the same object, i.e., like $x -> bfdiv($x)? If so,
    # flipping the sign of $y also flips the sign of $x.

    my $xsign = $x -> {sign};
    my $ysign = $y -> {sign};

    $y -> {sign} =~ tr/+-/-+/;            # Flip the sign of $y, and see ...
    my $same = $xsign ne $x -> {sign};    # ... if that changed the sign of $x.
    $y -> {sign} = $ysign;                # Restore the original sign.

    if ($same) {                          # $x -> bfdiv($x)
        $x -> bone();
    } else {

        # Compute quotient and remainder, ignoring signs.

        ($x -> {value}, $rem -> {value}) =
          $LIB -> _div($x -> {value}, $y -> {value});

        #   x    y     q  r
        #  23 /  7 =>  3  2
        # -23 /  7 => -4  5
        #  23 / -7 => -4 -5
        # -23 / -7 =>  3 -2

        # We are doing floored division, so adjust quotient and remainder as
        # necessary.

        if ($xsign ne $ysign && !$LIB -> _is_zero($rem -> {value})) {
            $x -> {value}   = $LIB -> _inc($x -> {value});
            $rem -> {value} = $LIB -> _sub($LIB -> _copy($y -> {value}),
                                           $rem -> {value});
        }

        # Now do the signs.

        $x -> {sign} = $xsign eq $ysign || $LIB -> _is_zero($x -> {value})
                         ? '+' : '-';
        $rem -> {sign} = $ysign eq '+' || $LIB -> _is_zero($rem -> {value})
                         ? '+' : '-';
    }

    # List context.

    if ($wantarray) {
        $rem -> {accuracy} = $x -> {accuracy};
        $rem -> {precision} = $x -> {precision};
        $x -> round(@r);
        $rem -> round(@r);
        return $x, $rem;
    }

    # Scalar context.

    return $x -> round(@r) if $LIB -> _is_zero($rem -> {value});

    # We could use this instead of the upgrade code above, but this code gives
    # more decimals when the integer part is non-zero. This is because the
    # fraction part is divided separately and the rounding is done on that part
    # separeately before the integer part is added.
    #
    #if ($class -> upgrade()) {
    #    $rem -> _upg() -> bfdiv($y);
    #    $x -> _upg() -> badd($rem, @r);
    #    return $x;
    #}

    $x -> round(@r);
    return $x;
}

sub bfmod {
    # This is the remainder after floored division.

    # Set up parameters.
    my ($class, $x, $y, @r) = ref($_[0]) && ref($_[0]) eq ref($_[1])
                            ? (ref($_[0]), @_)
                            : objectify(2, @_);

    ###########################################################################
    # Code for all classes that share the common interface.
    ###########################################################################

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('bfmod');

    $r[3] = $y;                 # no push!

    # At least one argument is NaN.

    if ($x -> is_nan() || $y -> is_nan()) {
        return $x -> bnan(@r);
    }

    # Modulo zero. See documentation for bfdiv().

    if ($y -> is_zero()) {
        return $x -> round(@r);
    }

    # Numerator (dividend) is +/-inf.

    if ($x -> is_inf()) {
        return $x -> bnan(@r);
    }

    # Denominator (divisor) is +/-inf.

    if ($y -> is_inf()) {
        if ($x -> is_zero() || $x -> bcmp(0) == $y -> bcmp(0)) {
            return $x -> round(@r);
        } else {
            return $x -> binf($y -> sign(), @r);
        }
    }

    ###########################################################################
    # Code for things that aren't Math::BigInt
    ###########################################################################

    # If called with "foreign" arguments.

    unless ($y -> isa(__PACKAGE__)) {
        if ($y -> is_int()) {
            $y = $y -> as_int();
        } else {
            return $x -> _upg() -> bfmod($y, @r) if $class -> upgrade();
            croak "Can't handle a ", ref($y), " in ", (caller(0))[3], "()";
        }
    }

    ###########################################################################
    # Code for Math::BigInt objects
    ###########################################################################

    # Calc new sign and in case $y == +/- 1, return $x.

    $x -> {value} = $LIB -> _mod($x -> {value}, $y -> {value});
    if ($LIB -> _is_zero($x -> {value})) {
        $x -> {sign} = '+';     # do not leave -0
    } else {
        $x -> {value} = $LIB -> _sub($y -> {value}, $x -> {value}, 1) # $y-$x
          if ($x -> {sign} ne $y -> {sign});
        $x -> {sign} = $y -> {sign};
    }

    $x -> round(@r);
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
        $rem = $x -> copy() -> round(@r) if $wantarray;
        $x -> bzero(@r);
        return $wantarray ? ($x, $rem) : $x;
    }

    ###########################################################################
    # Code for things that aren't Math::BigInt
    ###########################################################################

    # Division might return a non-integer result, so upgrade, if upgrading is
    # enabled.

    unless ($wantarray) {
        my $upg = $class -> upgrade();
        if ($upg) {
            my $tmp = $upg -> btdiv($x, $y, @r);
            if ($tmp -> is_int()) {
                $tmp = $tmp -> as_int();
                %$x = %$tmp;
            } else {
                %$x = %$tmp;
                bless $x, $upg;
            }
            return $x;
        }
    }

    # If called with "foreign" arguments.

    unless ($y -> isa(__PACKAGE__)) {
        if ($y -> is_int()) {
            $y = $y -> as_int();
        } else {
            return $x -> _upg() -> btdiv($y, @r) if $class -> upgrade();
            croak "Can't handle a ", ref($y), " in ", (caller(0))[3], "()";
        }
    }

    ###########################################################################
    # Code for Math::BigInt objects objects
    ###########################################################################

    $r[3] = $y;                 # no push!

    # Initialize remainder.

    my $rem = $class -> bzero();

    # Are both operands the same object, i.e., like $x -> btdiv($x)? If so,
    # flipping the sign of $y also flips the sign of $x.

    my $xsign = $x -> {sign};
    my $ysign = $y -> {sign};

    $y -> {sign} =~ tr/+-/-+/;            # Flip the sign of $y, and see ...
    my $same = $xsign ne $x -> {sign};    # ... if that changed the sign of $x.
    $y -> {sign} = $ysign;                # Re-insert the original sign.

    if ($same) {
        $x -> bone(@r);
    } else {
        ($x -> {value}, $rem -> {value}) =
          $LIB -> _div($x -> {value}, $y -> {value});

        $x -> {sign} = $xsign eq $ysign ? '+' : '-';
        $x -> {sign} = '+' if $LIB -> _is_zero($x -> {value});
        $x -> round(@r);
    }

    if ($wantarray) {
        $rem -> {sign} = $xsign;
        $rem -> {sign} = '+' if $LIB -> _is_zero($rem -> {value});
        $rem -> {accuracy} = $x -> {accuracy};
        $rem -> {precision} = $x -> {precision};
        $rem -> round(@r);
        return $x, $rem;
    }

    return $x;
}

sub btmod {
    # Remainder after truncated division.

    # set up parameters
    my ($class, $x, $y, @r) = ref($_[0]) && ref($_[0]) eq ref($_[1])
                            ? (ref($_[0]), @_)
                            : objectify(2, @_);

    ###########################################################################
    # Code for all classes that share the common interface.
    ###########################################################################

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('btmod');

    $r[3] = $y;                 # no push!

    # At least one argument is NaN.

    if ($x -> is_nan() || $y -> is_nan()) {
        return $x -> bnan(@r);
    }

    # Modulo zero. See documentation for btdiv().

    if ($y -> is_zero()) {
        return $x -> round(@r);
    }

    # Numerator (dividend) is +/-inf.

    if ($x -> is_inf()) {
        return $x -> bnan(@r);
    }

    # Denominator (divisor) is +/-inf.

    if ($y -> is_inf()) {
        return $x -> round(@r);
    }

    ###########################################################################
    # Code for things that aren't Math::BigInt
    ###########################################################################

    # If called with "foreign" arguments.

    unless ($y -> isa(__PACKAGE__)) {
        if ($y -> is_int()) {
            $y = $y -> as_int();
        } else {
            return $x -> _upg() -> btmod($y, @r) if $class -> upgrade();
            croak "Can't handle a ", ref($y), " in ", (caller(0))[3], "()";
        }
    }

    ###########################################################################
    # Code for Math::BigInt objects
    ###########################################################################

    my $xsign = $x -> {sign};

    $x -> {value} = $LIB -> _mod($x -> {value}, $y -> {value});

    $x -> {sign} = $xsign;
    $x -> {sign} = '+' if $LIB -> _is_zero($x -> {value});
    $x -> round(@r);
}

sub binv {
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), $_[0]) : objectify(1, @_);

    ###########################################################################
    # Code for all classes that share the common interface.
    ###########################################################################

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('binv');

    return $x -> binf("+", @r)  if $x -> is_zero();
    return $x -> bzero(@r)      if $x -> is_inf();
    return $x -> bnan(@r)       if $x -> is_nan();
    return $x -> round(@r)      if $x -> is_one("+") || $x -> is_one("-");

    ###########################################################################
    # Output might be finite, non-integer, so upgrade.
    ###########################################################################

    return $x -> _upg() -> binv(@r) if $class -> upgrade();

    ###########################################################################
    # Code for things that aren't Math::BigInt
    ###########################################################################

    unless ($x -> isa(__PACKAGE__)) {
        croak "Can't handle a ", ref($x), " in ", (caller(0))[3], "()";
    }

    ###########################################################################
    # Code for Math::BigInt objects
    ###########################################################################

    $x -> bzero(@r);
}

sub bsqrt {
    # calculate square root of $x
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    ###########################################################################
    # Code for all classes that share the common interface.
    ###########################################################################

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('bsqrt');

    return $x -> round(@r) if ($x -> is_zero() || $x -> is_one("+") ||
                               $x -> is_nan() || $x -> is_inf("+"));
    return $x -> bnan(@r)  if $x -> is_negative();

    ###########################################################################
    # Output might be finite, non-integer, so upgrade.
    ###########################################################################

    return $x -> _upg() -> bsqrt(@r) if $class -> upgrade();

    ###########################################################################
    # Code for things that aren't Math::BigInt
    ###########################################################################

    unless ($x -> isa(__PACKAGE__)) {
        croak "Can't handle a ", ref($x), " in ", (caller(0))[3], "()";
    }

    ###########################################################################
    # Code for Math::BigInt objects
    ###########################################################################

    $x->{value} = $LIB -> _sqrt($x->{value});
    return $x -> round(@r);
}

sub bpow {
    # (BINT or num_str, BINT or num_str) return BINT
    # compute power of two numbers -- stolen from Knuth Vol 2 pg 233
    # modifies first argument

    # set up parameters
    my ($class, $x, $y, @r) = ref($_[0]) && ref($_[0]) eq ref($_[1])
                            ? (ref($_[0]), @_)
                            : objectify(2, @_);

    ###########################################################################
    # Code for all classes that share the common interface.
    ###########################################################################

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('bpow');

    # $x and/or $y is a NaN
    return $x -> bnan(@r) if $x -> is_nan() || $y -> is_nan();

    # $x and/or $y is a +/-Inf
    if ($x -> is_inf("-")) {
        return $x -> bzero(@r)      if $y -> is_negative();
        return $x -> bnan(@r)       if $y -> is_zero();
        return $x -> round(@r)      if $y -> is_odd();
        return $x -> bneg(@r);
    } elsif ($x -> is_inf("+")) {
        return $x -> bzero(@r)      if $y -> is_negative();
        return $x -> bnan(@r)       if $y -> is_zero();
        return $x -> round(@r);
    } elsif ($y -> is_inf("-")) {
        return $x -> bnan(@r)       if $x -> is_one("-");
        return $x -> binf("+", @r)  if $x -> is_zero();
        return $x -> bone(@r)       if $x -> is_one("+");
        return $x -> bzero(@r);
    } elsif ($y -> is_inf("+")) {
        return $x -> bnan(@r)       if $x -> is_one("-");
        return $x -> bzero(@r)      if $x -> is_zero();
        return $x -> bone(@r)       if $x -> is_one("+");
        return $x -> binf("+", @r);
    }

    if ($x -> is_zero()) {
        return $x -> bone(@r) if $y -> is_zero();
        return $x -> binf(@r) if $y -> is_negative();
        return $x -> round(@r);
    }

    if ($x -> is_one("+")) {
        return $x -> round(@r);
    }

    if ($x -> is_one("-")) {
        return $x -> round(@r) if $y -> is_odd();
        return $x -> bneg(@r);
    }

    ###########################################################################
    # Code for things that aren't Math::BigInt
    ###########################################################################

    return $x -> _upg() -> bpow($y, @r) if $class -> upgrade();

    # We don't support finite non-integers, so return zero. The reason for
    # returning zero, not NaN, is that all output is in the open interval
    # (0,1), and truncating that to integer gives zero.

    if ($y->{sign} eq '-' || !$y -> isa(__PACKAGE__)) {
        return $x -> bzero(@r);
    }

    $r[3] = $y;                 # no push!

    $x->{value} = $LIB -> _pow($x->{value}, $y->{value});
    $x->{sign}  = $x -> is_negative() && $y -> is_odd() ? '-' : '+';
    $x -> round(@r);
}

sub broot {
    # calculate $y'th root of $x

    # set up parameters

    my ($class, $x, $y, @r) = ref($_[0]) && ref($_[0]) eq ref($_[1])
                            ? (ref($_[0]), @_) : objectify(2, @_);

    $y = $class -> new("2") unless defined $y;  # default base

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('broot');

    # If called with "foreign" argument.

    unless ($y -> isa(__PACKAGE__)) {
        if ($y -> is_int()) {
            $y = $y -> as_int();
        } else {
            return $x -> _upg() -> broot($y, @r) if $class -> upgrade();
            croak "Can't handle a ", ref($y), " in ", (caller(0))[3], "()";
        }
    }

    # NaN handling: $x ** 1/0, x or y NaN, or y inf/-inf or y == 0
    return $x -> bnan(@r) if ($x->{sign} !~ /^\+/ || $y -> is_zero() ||
                              $y->{sign} !~ /^\+$/);

    # Quick exit for trivial cases.
    return $x -> round(@r)
      if $x -> is_zero() || $x -> is_one() || $x -> is_inf() || $y -> is_one();

    return $x -> _upg() -> broot($y, @r) if $class -> upgrade();

    $x->{value} = $LIB->_root($x->{value}, $y->{value});
    $x -> round(@r);
}

sub bmuladd {
    # multiply two numbers and then add the third to the result
    # (BINT or num_str, BINT or num_str, BINT or num_str) return BINT

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

    # If called with "foreign" arguments.

    unless ($y -> isa(__PACKAGE__) && $z -> isa(__PACKAGE__)) {
        if ($y -> is_int() && $z -> is_int()) {
            $y = $y -> as_int();
            $z = $z -> as_int();
        } else {
            return $x -> _upg() -> bmuladd($y, $z, @r) if $class -> upgrade();
            croak "Can't handle a ", ref($y), " in ", (caller(0))[3], "()"
              unless $y -> isa(__PACKAGE__);
            croak "Can't handle a ", ref($z), " in ", (caller(0))[3], "()"
              unless $z -> isa(__PACKAGE__);
        }
    }

    # At this point, we know that x, y, and z are finite numbers

    # TODO: what if $y and $z have A or P set?
    $r[3] = $z;             # no push here

    my $zs = $z->{sign};
    my $zv = $z->{value};
    $zv = $LIB -> _copy($zv) if refaddr($x) eq refaddr($z);

    $x->{sign} = $x->{sign} eq $y->{sign} ? '+' : '-';
    $x->{value} = $LIB->_mul($x->{value}, $y->{value}); # do actual math
    $x->{sign} = '+' if $LIB->_is_zero($x->{value});    # no -0

    ($x->{value}, $x->{sign})
      = $LIB -> _sadd($x->{value}, $x->{sign}, $zv, $zs);
    return $x -> round(@r);
}

sub bmodpow {
    # Modular exponentiation. Raises a very large number to a very large
    # exponent in a given very large modulus quickly, thanks to binary
    # exponentiation.  Supports negative exponents.
    my ($class, $num, $exp, $mod, @r)
      = ref($_[0]) && ref($_[0]) eq ref($_[1]) && ref($_[1]) eq ref($_[2])
      ? (ref($_[0]), @_)
      : objectify(3, @_);

    ###########################################################################
    # Code for all classes that share the common interface.
    ###########################################################################

    # Don't modify constant (read-only) objects.

    return $num if $num -> modify('bmodpow');

    # Check for valid input. All operands must be finite, and the modulus must
    # be non-zero.

    return $num -> bnan(@r) if (!$num -> is_finite() ||     # NaN, -inf, +inf
                                !$exp -> is_finite() ||     # NaN, -inf, +inf
                                !$mod -> is_finite());      # NaN, -inf, +inf

    ###########################################################################
    # Code for things that aren't Math::BigInt
    ###########################################################################

    # If called with "foreign" arguments.

    unless ($exp -> isa(__PACKAGE__) && $mod -> isa(__PACKAGE__)) {
        if ($exp -> is_int() && $mod -> is_int()) {
            $exp = $exp -> as_int();
            $mod = $mod -> as_int();
        } else {
            return $num -> _upg() -> bmodpow($exp, $mod, @r) if $class -> upgrade();
            croak "Can't handle a ", ref($exp), " in ", (caller(0))[3], "()"
              unless $exp -> isa(__PACKAGE__);
            croak "Can't handle a ", ref($mod), " in ", (caller(0))[3], "()"
              unless $mod -> isa(__PACKAGE__);
        }
    }

    # When the exponent 'e' is negative, use the following relation, which is
    # based on finding the multiplicative inverse 'd' of 'b' modulo 'm':
    #
    #    b^(-e) (mod m) = d^e (mod m) where b*d = 1 (mod m)
    #
    # Return NaN if no modular multiplicative inverse exists.

    if ($exp->{sign} eq '-') {
        $num -> bmodinv($mod);
        return $num -> bnan(@r) if $num -> is_nan();
    }

    # Modulo zero. See documentation for Math::BigInt's bmod() method.

    if ($mod -> is_zero()) {
        if ($num -> is_zero()) {
            return $num -> bnan(@r);
        } else {
            return $num -> round(@r);
        }
    }

    ###########################################################################
    # Code for Math::BigInt objects
    ###########################################################################

    # Compute 'a (mod m)', ignoring the signs on 'a' and 'm'. If the resulting
    # value is zero, the output is also zero, regardless of the signs on 'a'
    # and 'm'.

    my $value = $LIB->_modpow($num->{value}, $exp->{value}, $mod->{value});
    my $sign  = '+';

    # If the resulting value is non-zero, we have four special cases, depending
    # on the signs on 'a' and 'm'.

    unless ($LIB->_is_zero($value)) {

        # There is a negative sign on 'a' (= $num**$exp) only if the number we
        # are exponentiating ($num) is negative and the exponent ($exp) is odd.

        if ($num->{sign} eq '-' && $exp -> is_odd()) {

            # When both the number 'a' and the modulus 'm' have a negative
            # sign, use this relation:
            #
            #    -a (mod -m) = -(a (mod m))

            if ($mod->{sign} eq '-') {
                $sign = '-';
            }

            # When only the number 'a' has a negative sign, use this relation:
            #
            #    -a (mod m) = m - (a (mod m))

            else {
                # Use copy of $mod since _sub() modifies the first argument.
                my $mod = $LIB->_copy($mod->{value});
                $value = $LIB->_sub($mod, $value);
                $sign  = '+';
            }

        } else {

            # When only the modulus 'm' has a negative sign, use this relation:
            #
            #    a (mod -m) = (a (mod m)) - m
            #               = -(m - (a (mod m)))

            if ($mod->{sign} eq '-') {
                # Use copy of $mod since _sub() modifies the first argument.
                my $mod = $LIB->_copy($mod->{value});
                $value = $LIB->_sub($mod, $value);
                $sign  = '-';
            }

            # When neither the number 'a' nor the modulus 'm' have a negative
            # sign, directly return the already computed value.
            #
            #    (a (mod m))

        }

    }

    $num->{value} = $value;
    $num->{sign}  = $sign;

    return $num -> round(@r);
}

sub bmodinv {
    # Return modular multiplicative inverse:
    #
    #   z is the modular inverse of x (mod y) if and only if
    #
    #       x*z ≡ 1  (mod y)
    #
    # If the modulus y is larger than one, x and z are relative primes (i.e.,
    # their greatest common divisor is one).
    #
    # If no modular multiplicative inverse exists, NaN is returned.

    # set up parameters
    my ($class, $x, $y, @r) = ref($_[0]) && ref($_[0]) eq ref($_[1])
                            ? (ref($_[0]), @_)
                            : objectify(2, @_);

    ###########################################################################
    # Code for all classes that share the common interface.
    ###########################################################################

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('bmodinv');

    # Return NaN if one or both arguments is +inf, -inf, or nan.

    return $x -> bnan(@r) if !$y -> is_finite() || !$x -> is_finite();

    # Return NaN if $y is zero; 1 % 0 makes no sense.

    return $x -> bnan(@r) if $y -> is_zero();

    ###########################################################################
    # Code for things that aren't Math::BigInt
    ###########################################################################

    # If called with "foreign" arguments.

    unless ($y -> isa(__PACKAGE__)) {
        if ($y -> is_int()) {
            $y = $y -> as_int();
        } else {
            return $x -> _upg() -> bmodinv($y, @r) if $class -> upgrade();
            croak "Can't handle a ", ref($y), " in ", (caller(0))[3], "()";
        }
    }

    ###########################################################################
    # Code for Math::BigInt objects
    ###########################################################################

    # Return 0 in the trivial case. $x % 1 or $x % -1 is zero for all finite
    # integers $x.

    return $x -> bzero(@r) if $y -> is_one('+') || $y -> is_one('-');

    # Return NaN if $x = 0, or $x modulo $y is zero. The only valid case when
    # $x = 0 is when $y = 1 or $y = -1, but that was covered above.
    #
    # Note that computing $x modulo $y here affects the value we'll feed to
    # $LIB->_modinv() below when $x and $y have opposite signs. E.g., if $x =
    # 5 and $y = 7, those two values are fed to _modinv(), but if $x = -5 and
    # $y = 7, the values fed to _modinv() are $x = 2 (= -5 % 7) and $y = 7.
    # The value if $x is affected only when $x and $y have opposite signs.

    $x -> bfmod($y);
    return $x -> bnan(@r) if $x -> is_zero();

    # Compute the modular multiplicative inverse of the absolute values. We'll
    # correct for the signs of $x and $y later. Return NaN if no GCD is found.

    ($x->{value}, $x->{sign}) = $LIB->_modinv($x->{value}, $y->{value});
    return $x -> bnan(@r) if !defined($x->{value});

    # Library inconsistency workaround: _modinv() in Math::BigInt::GMP versions
    # <= 1.32 return undef rather than a "+" for the sign.

    $x->{sign} = '+' unless defined $x->{sign};

    # When one or both arguments are negative, we have the following
    # relations.  If x and y are positive:
    #
    #   modinv(-x, -y) = -modinv(x, y)
    #   modinv(-x,  y) = y - modinv(x, y) = -modinv(x, y) (mod y)
    #   modinv( x, -y) = modinv(x, y) - y =  modinv(x, y) (mod -y)

    # We must swap the sign of the result if the original $x is negative.
    # However, we must compensate for ignoring the signs when computing the
    # inverse modulo. The net effect is that we must swap the sign of the
    # result if $y is negative.

    $x -> bneg() if $y->{sign} eq '-';

    # Compute $x modulo $y again after correcting the sign.

    $x -> bmod($y) if $x->{sign} ne $y->{sign};

    $x -> round(@r);
}

sub blog {
    # Return the logarithm of the operand. If a second operand is defined, that
    # value is used as the base, otherwise the base is assumed to be Euler's
    # constant.

    my ($class, $x, $base, @r);

    # Only objectify the base if it is defined, since an undefined base, as in
    # $x->blog() or $x->blog(undef) signals that the base is Euler's number =
    # 2.718281828...

    if (!ref($_[0]) && $_[0] =~ /^[a-z]\w*(?:::\w+)*$/i) {
        # E.g., Math::BigInt->blog(256, 2)
        ($class, $x, $base, @r) =
          defined $_[2] ? objectify(2, @_) : objectify(1, @_);
    } else {
        # E.g., $x->blog(2) or the deprecated Math::BigInt::blog(256, 2)
        ($class, $x, $base, @r) =
          defined $_[1] ? objectify(2, @_) : objectify(1, @_);
    }

    # Don't modify constant (read-only) objects.

    return $x if $x->modify('blog');

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
        return $x -> bone(@r) if $x == $base;   # 0 < base && 0 < x < inf
    }

    # We now know that the base is either undefined or >= 2 and finite.

    if ($x -> is_inf()) {                       # x = +/-inf
        return $x -> binf('+', @r);
    } elsif ($x -> is_neg()) {                  # -inf < x < 0
        return $x -> _upg() -> blog($base, @r) if $class -> upgrade();
        return $x -> bnan(@r);
    } elsif ($x -> is_one()) {                  # x = 1
        return $x -> bzero(@r);
    } elsif ($x -> is_zero()) {                 # x = 0
        return $x -> binf('-', @r);
    }

    # At this point we are done handling all exception cases and trivial cases.

    return $x -> _upg() -> blog($base, @r) if $class -> upgrade();

    # fix for bug #24969:
    # the default base is e (Euler's number) which is not an integer
    if (!defined $base) {
        require Math::BigFloat;

        # disable upgrading and downgrading

        my $upg = Math::BigFloat -> upgrade();
        my $dng = Math::BigFloat -> downgrade();
        Math::BigFloat -> upgrade(undef);
        Math::BigFloat -> downgrade(undef);

        my $u = Math::BigFloat -> new($x) -> blog() -> as_int();

        # reset upgrading and downgrading

        Math::BigFloat -> upgrade($upg);
        Math::BigFloat -> downgrade($dng);

        # modify $x in place

        $x->{value} = $u->{value};
        $x->{sign}  = $u->{sign};

        return $x -> round(@r);
    }

    my ($rc) = $LIB -> _log_int($x->{value}, $base->{value});
    return $x -> bnan(@r) unless defined $rc;   # not possible to take log?
    $x->{value} = $rc;
    $x -> round(@r);
}

sub bexp {
    # Calculate e ** $x (Euler's number to the power of X), truncated to
    # an integer value.
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    ###########################################################################
    # Code for all classes that share the common interface.
    ###########################################################################

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('bexp');

    # inf, -inf, NaN, <0 => NaN
    return $x -> bnan(@r)  if $x -> is_nan();
    return $x -> bone(@r)  if $x -> is_zero();
    return $x -> round(@r) if $x -> is_inf("+");
    return $x -> bzero(@r) if $x -> is_inf("-");

    ###########################################################################
    # Output might be finite, non-integer, so upgrade.
    ###########################################################################

    return $x -> _upg() -> bexp(@r) if $class -> upgrade();

    ###########################################################################
    # Code for things that aren't Math::BigInt
    ###########################################################################

    unless ($x -> isa(__PACKAGE__)) {
        croak "Can't handle a ", ref($x), " in ", (caller(0))[3], "()";
    }

    ###########################################################################
    # Code for Math::BigInt objects
    ###########################################################################

    require Math::BigFloat;
    my $tmp = Math::BigFloat -> bexp($x) -> bint() -> round(@r) -> as_int();
    $x->{value} = $tmp->{value};
    return $x -> round(@r);
}

sub bilog2 {
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('bilog2');

    return $x -> bnan(@r)      if $x -> is_nan();
    return $x -> binf("+", @r) if $x -> is_inf("+");
    return $x -> binf("-", @r) if $x -> is_zero();

    if ($x -> is_neg()) {
        return $x -> _upg() -> bilog2(@r) if $class -> upgrade();
        return $x -> bnan(@r);
    }

    $x -> {value} = $LIB -> _ilog2($x -> {value});
    return $x -> round(@r);
}

sub bilog10 {
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('bilog10');

    return $x -> bnan(@r)      if $x -> is_nan();
    return $x -> binf("+", @r) if $x -> is_inf("+");
    return $x -> binf("-", @r) if $x -> is_zero();

    if ($x -> is_neg()) {
        return $x -> _upg() -> bilog10(@r) if $class -> upgrade();
        return $x -> bnan(@r);
    }

    $x -> {value} = $LIB -> _ilog10($x -> {value});
    return $x -> round(@r);
}

sub bclog2 {
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('bclog2');

    return $x -> bnan(@r)      if $x -> is_nan();
    return $x -> binf("+", @r) if $x -> is_inf("+");
    return $x -> binf("-", @r) if $x -> is_zero();

    if ($x -> is_neg()) {
        return $x -> _upg() -> bclog2(@r) if $class -> upgrade();
        return $x -> bnan(@r);
    }

    $x -> {value} = $LIB -> _clog2($x -> {value});
    return $x -> round(@r);
}

sub bclog10 {
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('bclog10');

    return $x -> bnan(@r)      if $x -> is_nan();
    return $x -> binf("+", @r) if $x -> is_inf("+");
    return $x -> binf("-", @r) if $x -> is_zero();

    if ($x -> is_neg()) {
        return $x -> _upg() -> bclog10(@r) if $class -> upgrade();
        return $x -> bnan(@r);
    }

    $x -> {value} = $LIB -> _clog10($x -> {value});
    return $x -> round(@r);
}

sub bnok {
    # Calculate n over k (binomial coefficient or "choose" function) as
    # integer.

    # Set up parameters.
    my ($class, $n, $k, @r) = ref($_[0]) && ref($_[0]) eq ref($_[1])
                            ? (ref($_[0]), @_)
                            : objectify(2, @_);

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    # Don't modify constant (read-only) objects.

    return $n if $n -> modify('bnok');

    # If called with "foreign" arguments.

    unless ($k -> isa(__PACKAGE__)) {
        if ($k -> is_int()) {
            $k = $k -> as_int();
        } else {
            return $n -> _upg() -> bnok($k, @r) if $class -> upgrade();
            croak "Can't handle a ", ref($k), " in ", (caller(0))[3], "()";
        }
    }

    # All cases where at least one argument is NaN.

    return $n -> bnan(@r) if $n -> is_nan() || $k -> is_nan();

    # All cases where at least one argument is +/-inf.

    if ($n -> is_inf()) {
        if ($k -> is_inf()) {                   # bnok(+/-inf,+/-inf)
            return $n -> bnan(@r);
        } elsif ($k -> is_neg()) {              # bnok(+/-inf,k), k < 0
            return $n -> bzero(@r);
        } elsif ($k -> is_zero()) {             # bnok(+/-inf,k), k = 0
            return $n -> bone(@r);
        } else {
            if ($n -> is_inf("+", @r)) {        # bnok(+inf,k), 0 < k < +inf
                return $n -> binf("+");
            } else {                            # bnok(-inf,k), k > 0
                my $sign = $k -> is_even() ? "+" : "-";
                return $n -> binf($sign, @r);
            }
        }
    }

    elsif ($k -> is_inf()) {            # bnok(n,+/-inf), -inf <= n <= inf
        return $n -> bnan(@r);
    }

    # At this point, both n and k are real numbers.

    my $sign = 1;

    if ($n >= 0) {
        if ($k < 0 || $k > $n) {
            return $n -> bzero(@r);
        }
    } else {

        if ($k >= 0) {

            # n < 0 and k >= 0: bnok(n,k) = (-1)^k * bnok(-n+k-1,k)

            $sign = (-1) ** $k;
            $n -> bneg() -> badd($k) -> bdec();

        } elsif ($k <= $n) {

            # n < 0 and k <= n: bnok(n,k) = (-1)^(n-k) * bnok(-k-1,n-k)

            $sign = (-1) ** ($n - $k);
            my $x0 = $n -> copy();
            $n -> bone() -> badd($k) -> bneg();
            $k = $k -> copy();
            $k -> bneg() -> badd($x0);

        } else {

            # n < 0 and n < k < 0:

            return $n -> bzero(@r);
        }
    }

    # Some backends, e.g., Math::BigInt::GMP, can't handle the case when k is
    # very large, so if k > n/2, or, equivalently, 2*k > n, perform range
    # reduction by computing nok(n, k) as nok(n, n-k).

    my $k_val = $k->{value};
    my $two_k = $LIB -> _mul($LIB -> _two(), $k_val);
    if ($LIB -> _acmp($two_k, $n->{value}) > 0) {
        $k_val = $LIB -> _sub($LIB -> _copy($n->{value}), $k_val);
    }

    $n->{value} = $LIB -> _nok($n->{value}, $k_val);
    $n -> bneg() if $sign == -1;
    $n -> round(@r);
}

sub bperm {
    # Calculate permutations: n! / (n - k)!

    # Set up parameters.
    my ($class, $n, $k, @r) = ref($_[0]) && ref($_[0]) eq ref($_[1])
                            ? (ref($_[0]), @_)
                            : objectify(2, @_);

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    # Don't modify constant (read-only) objects.

    return $n if $n -> modify('bnok');

    # If called with "foreign" arguments.

    unless ($k -> isa(__PACKAGE__)) {
        if ($k -> is_int()) {
            $k = $k -> as_int();
        } else {
            return $n -> _upg() -> bperm($k, @r) if $class -> upgrade();
            croak "Can't handle a ", ref($k), " in ", (caller(0))[3], "()";
        }
    }

    # Special cases.

    return $n -> bnan(@r) if $n -> is_nan() || $k -> is_nan();
    return $n -> bnan(@r) unless $n >= $k && $k >= 0;
    return $n -> bone("+", @r) if $k -> is_zero();

    if ($n -> is_inf()) {
        if ($k -> is_inf()) {
            return $n -> bnan(@r);
        } else {
            return $n -> binf("+", @r);
        }
    }

    # Should this code be moved into the backend library? XXX

    # $factor is $n
    my $factor = $LIB -> _copy($n->{value});

    # $limit is $n - $k + 1
    my $limit  = $LIB -> _copy($n->{value});
    $limit = $LIB -> _sub($limit, $k->{value});
    $limit = $LIB -> _inc($limit);

    while ($LIB -> _acmp($factor, $limit) > 0) {
        $LIB -> _dec($factor);
        $LIB -> _mul($n->{value}, $factor);
    }

    $n -> round(@r);
}

sub bhyperop {
    my ($class, $a, $n, $b, @r) = objectify(3, @_);

    # Don't modify constant (read-only) objects.

    return $a if $a -> modify('bhyperop');

    my $tmp = $a -> hyperop($n, $b);
    $a -> {value} = $tmp -> {value};
    return $a -> round(@r);
}

sub hyperop {
    my ($class, $a, $n, $b, @r) = objectify(3, @_);

    croak("a must be non-negative") if $a < 0;
    croak("n must be non-negative") if $n < 0;
    croak("b must be non-negative") if $b < 0;

    # The following is a non-recursive implementation of the hyperoperator,
    # with special cases handled for speed.

    my @stack = ($a, $n, $b);
    while (@stack > 1) {
        my ($a, $n, $b) = splice @stack, -3;

        # Special cases for $b

        if ($b == 2 && $a == 2) {
            push @stack, $n == 0 ? Math::BigInt -> new("3")
                                 : Math::BigInt -> new("4");
            next;
        }

        if ($b == 1) {
            if ($n == 0) {
                push @stack, Math::BigInt -> new("2");
                next;
            }
            if ($n == 1) {
                push @stack, $a + 1;
                next;
            }
            push @stack, $a;
            next;
        }

        if ($b == 0) {
            if ($n == 1) {
                push @stack, $a;
                next;
            }
            if ($n == 2) {
                push @stack, Math::BigInt -> bzero();
                next;
            }
            push @stack, Math::BigInt -> bone();
            next;
        }

        # Special cases for $a

        if ($a == 0) {
            if ($n == 0) {
                push @stack, $b + 1;
                next;
            }
            if ($n == 1) {
                push @stack, $b;
                next;
            }
            if ($n == 2) {
                push @stack, Math::BigInt -> bzero();
                next;
            }
            if ($n == 3) {
                push @stack, $b == 0 ? Math::BigInt -> bone()
                                     : Math::BigInt -> bzero();
                next;
            }
            push @stack, $b -> is_odd() ? Math::BigInt -> bzero()
                                        : Math::BigInt -> bone();
            next;
        }

        if ($a == 1) {
            if ($n == 0 || $n == 1) {
                push @stack, $b + 1;
                next;
            }
            if ($n == 2) {
                push @stack, $b;
                next;
            }
            push @stack, Math::BigInt -> bone();
            next;
        }

        # Special cases for $n

        if ($n == 4) {          # tetration
            if ($b == 0) {
                push @stack, Math::BigInt -> bone();
                next;
            }
            my $y = $a;
            $y = $a ** $y for 2 .. $b;
            push @stack, $y;
            next;
        }

        if ($n == 3) {          # exponentiation
            push @stack, $a ** $b;
            next;
        }

        if ($n == 2) {          # multiplication
            push @stack, $a * $b;
            next;
        }

        if ($n == 1) {          # addition
            push @stack, $a + $b;
            next;
        }

        if ($n == 0) {          # succession
            push @stack, $b + 1;
            next;
        }

        push @stack, $a, $n - 1, $a, $n, $b - 1;
    }

    $a = pop @stack;
    return $a -> round(@r);
}

sub buparrow {
    my ($class, $a, $n, $b, @r) = objectify(3, @_);

    # Don't modify constant (read-only) objects.

    return $a if $a -> modify('buparrow');

    $a -> bhyperop($n + 2, $b, @r);
}

sub uparrow {
    my ($class, $a, $n, $b, @r) = objectify(3, @_);
    $a -> hyperop($n + 2, $b, @r);
}

sub backermann {
    my $m = shift;

    # Don't modify constant (read-only) objects.

    return $m if $m -> modify('backermann');

    my $y = $m -> ackermann(@_);
    $m -> {value} = $y -> {value};
    return $m;
}

sub ackermann {
    # Ackermann's function ackermann(m, n)
    #
    # The following is a simple, recursive implementation of the ackermann
    # function, just to show the idea. Such implementations cause "Deep
    # recursion on subroutine ..." warnings, so we use a faster, non-recursive
    # algorithm below with @_ as a stack.
    #
    # sub ackermann {
    #     my ($m, $n) = @_;
    #     return $n + 1                                  if $m == 0;
    #     return ackermann($m - 1, 1)                    if $m > 0 && $n == 0;
    #     return ackermann($m - 1, ackermann($m, $n - 1) if $m > 0 && $n > 0;
    # }

    my ($m, $n) = @_;
    my $class = ref $m;
    croak("m must be non-negative") if $m < 0;
    croak("n must be non-negative") if $n < 0;

    my $two      = $class -> new("2");
    my $three    = $class -> new("3");
    my $thirteen = $class -> new("13");

    $n = pop;
    $n = $class -> new($n) unless ref($n);
    while (@_) {
        my $m = pop;
        if ($m > $three) {
            push @_, (--$m) x $n;
            while (--$m >= $three) {
                push @_, $m;
            }
            $n = $thirteen;
        } elsif ($m == $three) {
            $n = $class -> bone() -> blsft($n + $three) -> bsub($three);
        } elsif ($m == $two) {
            $n -> bmul($two) -> badd($three);
        } elsif ($m >= 0) {
            $n -> badd($m) -> binc();
        } else {
            die "negative m!";
        }
    }
    $n;
}

sub bsin {
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('bsin');

    # Trivial cases.

    return $x -> bzero(@r) if $x -> is_zero();
    return $x -> bnan(@r)  if $x -> is_inf() || $x -> is_nan();

    my $upg = $class -> upgrade();
    if ($upg) {
        my $xtmp = $upg -> bsin($x, @r);
        if ($xtmp -> is_int()) {
            $xtmp = $xtmp -> as_int();
            %$x = %$xtmp;
        } else {
            %$x = %$xtmp;
            bless $x, $upg;
        }
        return $x;
    }

    # When x is an integer ≠ 0, sin(x) truncated to an integer is always zero.

    $x -> bzero(@r);
}

sub bcos {
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('bcos');

    # Trivial cases.

    return $x -> bone(@r) if $x -> is_zero();
    return $x -> bnan(@r) if $x -> is_inf() || $x -> is_nan();

    my $upg = $class -> upgrade();
    if ($upg) {
        my $xtmp = $upg -> bcos($x, @r);
        if ($xtmp -> is_int()) {
            $xtmp = $xtmp -> as_int();
            %$x = %$xtmp;
        } else {
            %$x = %$xtmp;
            bless $x, $upg;
        }
        return $x;
    }

    # When x is a non-zero integer, cos(x) truncated to an integer is always
    # zero.

    $x -> bzero(@r);
}

sub batan {
    # Calculate arctan(x) to N digits. Unless upgrading is in effect, returns
    # the result truncated to an integer.
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('batan');

    return $x -> bnan(@r)  if $x -> is_nan();
    return $x -> bzero(@r) if $x -> is_zero();

    return $x -> _upg() -> batan(@r) if $class -> upgrade();

    return $x -> bone("+", @r) if $x -> bgt("1");
    return $x -> bone("-", @r) if $x -> blt("-1");

    $x -> bzero(@r);
}

sub batan2 {
    # calculate arcus tangens of ($y/$x)

    my ($class, $y, $x, @r) = ref($_[0]) && ref($_[0]) eq ref($_[1])
                            ? (ref($_[0]), @_) : objectify(2, @_);

    # Don't modify constant (read-only) objects.

    return $y if $y -> modify('batan2');

    # If called with "foreign" arguments.

    unless ($y -> isa(__PACKAGE__)) {
        if ($y -> is_int()) {
            $y = $y -> as_int();
        } else {
            return $y -> _upg() -> batan2($y, @r) if $class -> upgrade();
            croak "Can't handle a ", ref($y), " in ", (caller(0))[3], "()";
        }
    }

    return $y -> bnan() if $y -> is_nan() || $x -> is_nan();

    # Y    X
    # != 0 -inf result is +- pi
    if ($x -> is_inf() || $y -> is_inf()) {
        if ($y -> is_inf()) {
            if ($x -> is_inf("-")) {
                # calculate 3 pi/4 => 2.3.. => 2
                $y -> bone(substr($y->{sign}, 0, 1));
                $y -> bmul($class -> new(2));
            } elsif ($x -> is_inf("+")) {
                # calculate pi/4 => 0.7 => 0
                $y -> bzero();
            } else {
                # calculate pi/2 => 1.5 => 1
                $y -> bone(substr($y->{sign}, 0, 1));
            }
        } else {
            if ($x -> is_inf("+")) {
                # calculate pi/4 => 0.7 => 0
                $y -> bzero();
            } else {
                # PI => 3.1415.. => 3
                $y -> bone(substr($y->{sign}, 0, 1));
                $y -> bmul($class -> new(3));
            }
        }
        return $y;
    }

    # Temporarily disable downgrading in Math::BigFloat.

    my $dng = Math::BigFloat -> downgrade();
    Math::BigFloat -> downgrade(undef);

    my $yflt = $y -> as_float();
    my $xflt = $x -> as_float();
    my $yint = $yflt -> batan2($xflt, @r) -> as_int();

    $y->{value} = $yint->{value};
    $y->{sign}  = $yint->{sign};

    # Restore downgrading.

    Math::BigFloat -> downgrade($dng);
    $y -> round(@r);
}

sub bfac {
    # (BINT or num_str, BINT or num_str) return BINT
    # compute factorial number from $x, modify $x in place
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('bfac');

    return $x -> bnan(@r)      if $x -> is_nan() || $x -> is_inf("-");;
    return $x -> binf("+", @r) if $x -> is_inf("+");
    return $x -> bnan(@r)      if $x -> is_neg();
    return $x -> bone(@r)      if $x -> is_zero() || $x -> is_one();

    $x->{value} = $LIB->_fac($x->{value});
    $x -> round(@r);
}

sub bdfac {
    # compute double factorial, modify $x in place
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('bdfac');

    return $x -> bnan(@r)      if $x -> is_nan() || $x -> is_inf("-");
    return $x -> binf("+", @r) if $x -> is_inf("+");
    return $x -> bnan(@r)      if $x <= -2;
    return $x -> bone(@r)      if $x <= 1;

    croak("bdfac() requires a newer version of the $LIB library.")
        unless $LIB -> can('_dfac');

    $x->{value} = $LIB->_dfac($x->{value});
    $x -> round(@r);
}

sub btfac {
    # compute triple factorial, modify $x in place
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('btfac');

    return $x -> bnan(@r)      if $x -> is_nan();
    return $x -> binf("+", @r) if $x -> is_inf("+");

    my $k = $class -> new("3");
    return $x -> bnan(@r) if $x <= -$k;

    my $one = $class -> bone();
    return $x -> bone(@r) if $x <= $one;

    my $f = $x -> copy();
    while ($f -> bsub($k) > $one) {
        $x -> bmul($f);
    }
    $x -> round(@r);
}

sub bmfac {
    # compute multi-factorial

    my ($class, $x, $k, @r) = ref($_[0]) && ref($_[0]) eq ref($_[1])
                            ? (ref($_[0]), @_) : objectify(2, @_);

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('bmfac');

    return $x -> bnan(@r)      if $x -> is_nan();
    return $x -> bnan(@r)      if $k -> is_nan();
    return $x -> binf("+", @r) if $x -> is_inf("+");

    # If called with "foreign" arguments.

    unless ($k -> isa(__PACKAGE__)) {
        if ($k -> is_int()) {
            $k = $k -> as_int();
        } else {
            return $x -> _upg() -> bmfac($k, @r) if $class -> upgrade();
            croak "Can't handle a ", ref($k), " in ", (caller(0))[3], "()";
        }
    }

    return $x -> bnan(@r) if $k < 1 || $x <= -$k;

    my $one = $class -> bone();
    return $x -> bone(@r) if $x <= $one;

    my $f = $x -> copy();
    while ($f -> bsub($k) > $one) {
        $x -> bmul($f);
    }
    $x -> round(@r);
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
            $y[0]{value} = $LIB -> _zero();
            last if $n == 0;

            $y[1] = $y[0] -> copy();
            $y[1]{value} = $LIB -> _one();
            last if $n == 1;

            for (my $i = 2 ; $i <= abs($n) ; $i++) {
                $y[$i] = $y[$i - 1] -> copy();
                $y[$i]{value} = $LIB -> _add($LIB -> _copy($y[$i - 1]{value}),
                                                           $y[$i - 2]{value});
            }

            # If negative, insert sign as appropriate.

            if ($x -> is_neg()) {
                for (my $i = 2 ; $i <= $#y ; $i += 2) {
                    $y[$i]{sign} = '-';
                }
            }

            # The last element in the array is the invocand.

            $x->{value} = $y[-1]{value};
            $x->{sign}  = $y[-1]{sign};
            $y[-1] = $x;
        }

        @y = map { $_ -> round(@r) } @y;
        return @y;
    }

    # Scalar context.

    else {
        return $x if $x -> is_inf('+');
        return $x -> bnan() if $x -> is_nan() || $x -> is_inf('-');

        $x->{sign}  = $x -> is_neg() && $x -> is_even() ? '-' : '+';
        $x->{value} = $LIB -> _fib($x->{value});
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
        return if $x -> is_nan();
        croak("blucas() can't return an infinitely long list of numbers")
          if $x -> is_inf();

        # The following places a limit on how large $x can be. Should this
        # limit be removed? XXX

        my $n = $x -> numify();

        my @y;
        {
            $y[0] = $x -> copy() -> babs();
            $y[0]{value} = $LIB -> _two();
            last if $n == 0;

            $y[1] = $y[0] -> copy();
            $y[1]{value} = $LIB -> _one();
            last if $n == 1;

            for (my $i = 2 ; $i <= abs($n) ; $i++) {
                $y[$i] = $y[$i - 1] -> copy();
                $y[$i]{value} = $LIB -> _add($LIB -> _copy($y[$i - 1]{value}),
                                                           $y[$i - 2]{value});
            }

            # If negative, insert sign as appropriate.

            if ($x -> is_neg()) {
                for (my $i = 2 ; $i <= $#y ; $i += 2) {
                    $y[$i]{sign} = '-';
                }
            }

            # The last element in the array is the invocand.

            $x->{value} = $y[-1]{value};
            $x->{sign}  = $y[-1]{sign};
            $y[-1] = $x;
        }

        @y = map { $_ -> round(@r) } @y;
        return @y;
    }

    # Scalar context.

    else {
        return $x if $x -> is_inf('+');
        return $x -> bnan() if $x -> is_nan() || $x -> is_inf('-');

        $x->{sign}  = $x -> is_neg() && $x -> is_even() ? '-' : '+';
        $x->{value} = $LIB -> _lucas($x->{value});
        return $x -> round(@r);
    }
}

sub blsft {
    # (BINT or num_str, BINT or num_str) return BINT
    # compute $x << $y, base $n

    my ($class, $x, $y, $b, @r);

    # Objectify the base only when it is defined, since an undefined base, as
    # in $x->blsft(3) or $x->blog(3, undef) means use the default base 2.

    if (!ref($_[0]) && $_[0] =~ /^[A-Za-z]|::/) {
        # E.g., Math::BigInt->blog(256, 5, 2)
        ($class, $x, $y, $b, @r) =
          defined $_[3] ? objectify(3, @_) : objectify(2, @_);
    } else {
        # E.g., Math::BigInt::blog(256, 5, 2) or $x->blog(5, 2)
        ($class, $x, $y, $b, @r) =
          defined $_[2] ? objectify(3, @_) : objectify(2, @_);
    }

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('blsft');

    # The default base is 2.

    $b = 2 unless defined $b;
    $b = $class -> new($b) unless defined(blessed($b));

    # If called with "foreign" arguments.

    unless ($y -> isa(__PACKAGE__) && $b -> isa(__PACKAGE__)) {
        if ($y -> is_int() && $b -> is_int()) {
            $y = $y -> as_int();
            $b = $b -> as_int();
        } else {
            return $x -> _upg() -> blsft($y, $b, @r) if $class -> upgrade();
            croak "Can't handle a ", ref($x), " in ", (caller(0))[3], "()"
              unless $y -> isa(__PACKAGE__);
            croak "Can't handle a ", ref($b), " in ", (caller(0))[3], "()"
              unless $b -> isa(__PACKAGE__);
        }
    }

    # Handle NaN cases.

    return $x -> bnan(@r)
      if $x -> is_nan() || $y -> is_nan() || $b -> is_nan();

    # blsft($x, -$y, $b) = brsft($x, $y, $b)

    return $x -> brsft($y -> copy() -> bneg(), $b, @r) if $y -> is_neg();

    # Now handle all cases where at least one operand is ±Inf or the result
    # will be ±Inf or NaN.

    if ($y -> is_inf("+")) {
       if ($b -> is_one("-")) {
            return $x -> bnan(@r);
        } elsif ($b -> is_one("+")) {
            return $x -> round(@r);
        } elsif ($b -> is_zero()) {
            return $x -> bnan(@r) if $x -> is_inf();
            return $x -> bzero(@r);
        } else {
            return $x -> binf("-", @r) if $x -> is_negative();
            return $x -> binf("+", @r) if $x -> is_positive();
            return $x -> bnan(@r);
        }
    }

    if ($b -> is_inf()) {
        return $x -> bnan(@r) if $x -> is_zero() || $y -> is_zero();
        if ($b -> is_inf("-")) {
            return $x -> binf("+", @r)
              if ($x -> is_negative() && $y -> is_odd() ||
                  $x -> is_positive() && $y -> is_even());
            return $x -> binf("-", @r);
       } else {
           return $x -> binf("-", @r) if $x -> is_negative();
           return $x -> binf("+", @r);
        }
    }

    if ($b -> is_zero()) {
        return $x -> round(@r) if $y -> is_zero();
        return $x -> bnan(@r)  if $x -> is_inf();
        return $x -> bzero(@r);
    }

    if ($x -> is_inf()) {
        if ($b -> is_negative()) {
            if ($x -> is_inf("-")) {
                if ($y -> is_even()) {
                    return $x -> round(@r);
                } else {
                    return $x -> binf("+", @r);
                }
            } else {
                if ($y -> is_even()) {
                    return $x -> round(@r);
                } else {
                    return $x -> binf("-", @r);
                }
            }
        } else {
            return $x -> round(@r);
        }
    }

    # At this point, we know that both the input and the output is finite.
    # Handle some trivial cases.

    return $x -> round(@r) if $x -> is_zero() || $y -> is_zero()
                              || $b -> is_one("+")
                              || $b -> is_one("-") && $y -> is_even();

    return $x -> bneg(@r) if $b -> is_one("-") && $y -> is_odd();

    # While some of the libraries support an arbitrarily large base, not all of
    # them do, so rather than returning an incorrect result in those cases,
    # disallow bases that don't work with all libraries.

    my $uintmax = ~0;
    if ($x -> bcmp($uintmax) > 0) {
        $x -> bmul($b -> bpow($y));
    } else {
        my $neg = 0;
        if ($b -> is_negative()) {
            $neg = 1 if $y -> is_odd();
            $b -> babs();
        }
        $b = $b -> numify();
        $x -> {value} = $LIB -> _lsft($x -> {value}, $y -> {value}, $b);
        $x -> {sign} =~ tr/+-/-+/ if $neg;
    }
    $x -> round(@r);
}

sub brsft {
    # (BINT or num_str, BINT or num_str) return BINT
    # compute $x >> $y, base $n

    my ($class, $x, $y, $b, @r);

    # Objectify the base only when it is defined, since an undefined base, as
    # in $x->blsft(3) or $x->blog(3, undef) means use the default base 2.

    if (!ref($_[0]) && $_[0] =~ /^[A-Za-z]|::/) {
        # E.g., Math::BigInt->blog(256, 5, 2)
        ($class, $x, $y, $b, @r) =
          defined $_[3] ? objectify(3, @_) : objectify(2, @_);
    } else {
        # E.g., Math::BigInt::blog(256, 5, 2) or $x -> blog(5, 2)
        ($class, $x, $y, $b, @r) =
          defined $_[2] ? objectify(3, @_) : objectify(2, @_);
    }

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('brsft');

    # The default base is 2.

    $b = 2 unless defined $b;
    $b = $class -> new($b) unless defined(blessed($b));

    # If called with "foreign" arguments.

    unless ($y -> isa(__PACKAGE__) && $b -> isa(__PACKAGE__)) {
        if ($y -> is_int() && $b -> is_int()) {
            $y = $y -> as_int();
            $b = $b -> as_int();
        } else {
            return $x -> _upg() -> brsft($y, $b, @r) if $class -> upgrade();
            croak "Can't handle a ", ref($x), " in ", (caller(0))[3], "()"
              unless $y -> isa(__PACKAGE__);
            croak "Can't handle a ", ref($b), " in ", (caller(0))[3], "()"
              unless $b -> isa(__PACKAGE__);
        }
    }

    # Handle NaN cases.

    return $x -> bnan(@r)
      if $x -> is_nan() || $y -> is_nan() || $b -> is_nan();

    # brsft($x, -$y, $b) = blsft($x, $y, $b)

    return $x -> blsft($y -> copy() -> bneg(), $b, @r) if $y -> is_neg();

    # Now handle all cases where at least one operand is ±Inf or the result
    # will be ±Inf or NaN.

    if ($b -> is_inf()) {
        return $x -> bnan(@r) if $x -> is_inf() || $y -> is_zero();
        if ($b -> is_inf("+")) {
            if ($x -> is_negative()) {
                return $x -> bone("-", @r);
            } else {
                return $x -> bzero(@r);
            }
        } else {
            if ($x -> is_negative()) {
                return $y -> is_odd() ? $x -> bzero(@r)
                                      : $x -> bone("-", @r);
            } elsif ($x -> is_positive()) {
                return $y -> is_odd() ? $x -> bone("-", @r)
                                      : $x -> bzero(@r);
            } else {
                return $x -> bzero(@r);
            }
        }
    }

    if ($b -> is_zero()) {
        return $x -> round(@r) if $y -> is_zero();
        return $x -> bnan(@r)  if $x -> is_zero();
        return $x -> is_negative() ? $x -> binf("-", @r)
                                   : $x -> binf("+", @r);
    }

    if ($y -> is_inf("+")) {
        if ($b -> is_one("-")) {
            return $x -> bnan(@r);
        } elsif ($b -> is_one("+")) {
            return $x -> round(@r);
        } else {
            return $x -> bnan(@r) if $x -> is_inf();
            return $x -> is_negative() ? $x -> bone("-", @r)
                                       : $x -> bzero(@r);
        }
    }

    if ($x -> is_inf()) {
        if ($b -> is_negative()) {
            if ($x -> is_inf("-")) {
                if ($y -> is_even()) {
                    return $x -> round(@r);
                } else {
                    return $x -> binf("+", @r);
                }
            } else {
                if ($y -> is_even()) {
                    return $x -> round(@r);
                } else {
                    return $x -> binf("-", @r);
                }
            }
        } else {
            return $x -> round(@r);
        }
    }

    # At this point, we know that both the input and the output is finite.
    # Handle some trivial cases.

    return $x -> round(@r) if $x -> is_zero() || $y -> is_zero()
                              || $b -> is_one("+")
                              || $b -> is_one("-") && $y -> is_even();

    return $x -> bneg(@r) if $b -> is_one("-") && $y -> is_odd();

    # We know that $y is positive. Shifting right by a positive amount might
    # lead to a non-integer result.

    return $x -> _upg() -> brsft($y, $b, @r) if $class -> upgrade();

    # This only works for negative numbers when shifting in base 2.
    if ($x -> is_neg() && $b -> bcmp("2") == 0) {
        return $x -> round(@r) if $x -> is_one('-'); # -1 => -1
        # Although this is O(N*N) in Math::BigInt::Calc->_as_bin(), it is O(N)
        # in Pari et al., but perhaps there is a better emulation for two's
        # complement shift ... if $y != 1, we must simulate it by doing:
        # convert to bin, flip all bits, shift, and be done
        $x -> binc();                      # -3 => -2
        my $bin = $x -> to_bin();               # convert to string
        $bin =~ s/^-//;                         # strip leading minus
        $bin =~ tr/10/01/;                      # flip bits
        my $nbits = CORE::length($bin);
        return $x -> bone("-", @r) if $y >= $nbits;
        $bin = substr $bin, 0, $nbits - $y;     # keep most significant bits
        $bin = '1' . $bin;                      # prepend one dummy '1'
        $bin =~ tr/10/01/;                      # flip bits back
        my $res = $class -> from_bin($bin);     # convert back from string
        $res -> binc();                  # remember to increment
        $x -> {value} = $res -> {value};        # take over value
        return $x -> round(@r);
    }

    # While some of the libraries support an arbitrarily large base, not all of
    # them do, so rather than returning an incorrect result in those cases, use
    # division.

    my $uintmax = ~0;
    if ($x -> bcmp($uintmax) > 0 || $x -> is_neg() || $b -> is_negative()) {
        $x -> bdiv($b -> bpow($y));
    } else {
        $b = $b -> numify();
        $x -> {value} = $LIB -> _rsft($x -> {value}, $y -> {value}, $b);
    }

    return $x -> round(@r);
}

###############################################################################
# Bitwise methods
###############################################################################

# Bitwise left shift.

sub bblsft {
    # We don't call objectify(), because the bitwise methods should not
    # upgrade, even when upgrading is enabled.

    my ($class, $x, $y, @r);

    # $x -> bblsft($y)

    if (ref($_[0])) {
        ($class, $x, $y, @r) = (ref($_[0]), @_);
        $y = $y -> as_int()
          if ref($y) && !$y -> isa(__PACKAGE__) && $y -> can('as_int');
        $y = $class -> new(int($y)) unless ref($y);
    }

    # $class -> bblsft($x, $y)

    else {
        ($class, $x, $y, @r) = @_;
        for ($x, $y) {
            $_ = $_ -> as_int()
              if ref($_) && !$_ -> isa(__PACKAGE__) && $_ -> can('as_int');
            $_ = $class -> new(int($_)) unless ref($_);
        }
    }

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('bblsft');

    return $x -> bnan(@r) if $x -> is_nan() || $y -> is_nan();

    # bblsft($x, -$y) = bbrsft($x, $y)

    return $x -> bbrsft($y -> copy() -> bneg()) if $y -> is_neg();

    # Shifting infinitely far to the left.

    if ($y -> is_inf("+")) {
        return $x -> binf("+", @r) if $x -> is_pos();
        return $x -> binf("-", @r) if $x -> is_neg();
        return $x -> bnan(@r);
    }

    # These cases change nothing.

    return $x -> round(@r) if $x -> is_zero() || $x -> is_inf() ||
                              $y -> is_zero();

    $x -> {value} = $LIB -> _lsft($x -> {value}, $y -> {value}, 2);
    $x -> round(@r);
}

# Bitwise right shift.

sub bbrsft {
    # We don't call objectify(), because the bitwise methods should not
    # upgrade, even when upgrading is enabled.

    my ($class, $x, $y, @r);

    # $x -> bblsft($y)

    if (ref($_[0])) {
        ($class, $x, $y, @r) = (ref($_[0]), @_);
        $y = $y -> as_int()
          if ref($y) && !$y -> isa(__PACKAGE__) && $y -> can('as_int');
        $y = $class -> new(int($y)) unless ref($y);
    }

    # $class -> bblsft($x, $y)

    else {
        ($class, $x, $y, @r) = @_;
        for ($x, $y) {
            $_ = $_ -> as_int()
              if ref($_) && !$_ -> isa(__PACKAGE__) && $_ -> can('as_int');
            $_ = $class -> new(int($_)) unless ref($_);
        }
    }

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('bbrsft');

    return $x -> bnan(@r) if $x -> is_nan() || $y -> is_nan();

    # bbrsft($x, -$y) = bblsft($x, $y)

    return $x -> bblsft($y -> copy() -> bneg()) if $y -> is_neg();

    # Shifting infinitely far to the right.

    if ($y -> is_inf("+")) {
        return $x -> bnan(@r)      if $x -> is_inf();
        return $x -> bone("-", @r) if $x -> is_neg();
        return $x -> bzero(@r);
    }

    # These cases change nothing.

    return $x -> round(@r) if $x -> is_zero() || $x -> is_inf() ||
                              $y -> is_zero();

    # At this point, $x is either positive or negative, not zero.

    if ($x -> is_pos()) {
        $x -> {value} = $LIB -> _rsft($x -> {value}, $y -> {value}, 2);
    } else {
        my $n = $x -> {value};
        my $d = $LIB -> _pow($LIB -> _new("2"), $y -> {value});
        my ($p, $q) = $LIB -> _div($n, $d);
        $p = $LIB -> _inc($p) unless $LIB -> _is_zero($q);
        $x -> {value} = $p;
    }

    $x -> round(@r);
}

sub band {
    #(BINT or num_str, BINT or num_str) return BINT
    # compute x & y

    my ($class, $x, $y, @r) = ref($_[0]) && ref($_[0]) eq ref($_[1])
                            ? (ref($_[0]), @_) : objectify(2, @_);

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('band');

    # If called with "foreign" arguments.

    unless ($y -> isa(__PACKAGE__)) {
        if ($y -> is_int()) {
            $y = $y -> as_int();
        } else {
            return $x -> _upg() -> band($y, @r) if $class -> upgrade();
            croak "Can't handle a ", ref($y), " in ", (caller(0))[3], "()";
        }
    }

    $r[3] = $y;                 # no push!

    # If $x and/or $y is Inf or NaN, return NaN.

    return $x -> bnan(@r) if !$x -> is_finite() || !$y -> is_finite();

    if ($x->{sign} eq '+' && $y->{sign} eq '+') {
        $x->{value} = $LIB->_and($x->{value}, $y->{value});
    } else {
        ($x->{value}, $x->{sign}) = $LIB->_sand($x->{value}, $x->{sign},
                                                $y->{value}, $y->{sign});
    }

    return $x -> round(@r);
}

sub bior {
    #(BINT or num_str, BINT or num_str) return BINT
    # compute x | y

    my ($class, $x, $y, @r) = ref($_[0]) && ref($_[0]) eq ref($_[1])
                            ? (ref($_[0]), @_) : objectify(2, @_);

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('bior');

    # If called with "foreign" arguments.

    unless ($y -> isa(__PACKAGE__)) {
        if ($y -> is_int()) {
            $y = $y -> as_int();
        } else {
            return $x -> _upg() -> bior($y, @r) if $class -> upgrade();
            croak "Can't handle a ", ref($y), " in ", (caller(0))[3], "()";
        }
    }

    $r[3] = $y;                 # no push!

    # If $x and/or $y is Inf or NaN, return NaN.

    return $x -> bnan() if (!$x -> is_finite() || !$y -> is_finite());

    if ($x->{sign} eq '+' && $y->{sign} eq '+') {
        $x->{value} = $LIB->_or($x->{value}, $y->{value});
    } else {
        ($x->{value}, $x->{sign}) = $LIB->_sor($x->{value}, $x->{sign},
                                               $y->{value}, $y->{sign});
    }
    return $x -> round(@r);
}

sub bxor {
    #(BINT or num_str, BINT or num_str) return BINT
    # compute x ^ y

    my ($class, $x, $y, @r) = ref($_[0]) && ref($_[0]) eq ref($_[1])
                            ? (ref($_[0]), @_) : objectify(2, @_);

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('bxor');

    # If called with "foreign" arguments.

    unless ($y -> isa(__PACKAGE__)) {
        if ($y -> is_int()) {
            $y = $y -> as_int();
        } else {
            return $x -> _upg() -> bxor($y, @r) if $class -> upgrade();
            croak "Can't handle a ", ref($y), " in ", (caller(0))[3], "()";
        }
    }

    $r[3] = $y;                 # no push!

    # If $x and/or $y is Inf or NaN, return NaN.

    return $x -> bnan(@r) if !$x -> is_finite() || !$y -> is_finite();

    if ($x->{sign} eq '+' && $y->{sign} eq '+') {
        $x->{value} = $LIB->_xor($x->{value}, $y->{value});
    } else {
        ($x->{value}, $x->{sign}) = $LIB->_sxor($x->{value}, $x->{sign},
                                               $y->{value}, $y->{sign});
    }
    return $x -> round(@r);
}

sub bnot {
    # (num_str or BINT) return BINT
    # represent ~x as twos-complement number
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('bnot');

    $x -> binc() -> bneg(@r);
}

###############################################################################
# Rounding methods
###############################################################################

sub round {
    # Round $self according to given parameters, or given second argument's
    # parameters or global defaults
    my ($class, $self, @args) = ref($_[0]) ? (ref($_[0]), @_)
                                           : objectify(1, @_);

    # These signal no rounding:
    #
    #   $x->round(undef)
    #   $x->round(undef, undef, ...)
    #
    # The "@args <= 3" is necessary because the final set of parameters that
    # will be used for rounding depend on the "extra arguments", if any.

    if (@args == 1 && !defined($args[0]) ||
        @args >= 2 && @args <= 3 && !defined($args[0]) && !defined($args[1]))
    {
        $self->{accuracy} = undef;
        $self->{precision} = undef;
        return $self;
    }

    my ($a, $p, $r) = splice @args, 0, 3;

    # $a accuracy, if given by caller
    # $p precision, if given by caller
    # $r round_mode, if given by caller
    # @args all 'other' arguments (0 for unary, 1 for binary ops)

    if (defined $a) {
        croak "accuracy must be a number, not '$a'"
          unless $a =~/^[+-]?(?:\d+(?:\.\d*)?|\.\d+)(?:[Ee][+-]?\d+)?\z/;
    }

    if (defined $p) {
        croak "precision must be a number, not '$p'"
          unless $p =~/^[+-]?(?:\d+(?:\.\d*)?|\.\d+)(?:[Ee][+-]?\d+)?\z/;
    }

    # now pick $a or $p, but only if we have got "arguments"
    if (!defined $a) {
        foreach ($self, @args) {
            # take the defined one, or if both defined, the one that is smaller
            $a = $_->{accuracy}
              if (defined $_->{accuracy}) && (!defined $a || $_->{accuracy} < $a);
        }
    }
    if (!defined $p) {
        # even if $a is defined, take $p, to signal error for both defined
        foreach ($self, @args) {
            # take the defined one, or if both defined, the one that is bigger
            # -2 > -3, and 3 > 2
            $p = $_->{precision}
              if (defined $_->{precision}) && (!defined $p || $_->{precision} > $p);
        }
    }

    # if still none defined, use globals
    unless (defined $a || defined $p) {
        $a = $class -> accuracy();
        $p = $class -> precision();
    }

    # A == 0 is useless, so undef it to signal no rounding
    $a = undef if defined $a && $a == 0;

    # no rounding today?
    return $self unless defined $a || defined $p;

    # set A and set P is an fatal error
    if (defined $a && defined $p) {
        #carp "can't specify both accuracy and precision";
        return $self -> bnan();
    }

    # Infs and NaNs are not rounded, but assign rounding parameters to them.
    #
    #if ($self -> is_inf() || $self -> is_nan()) {
    #    $self->{accuracy}  = $a;
    #    $self->{precision} = $p;
    #    return $self;
    #}

    $r = $class -> round_mode() unless defined $r;
    if ($r !~ /^(even|odd|[+-]inf|zero|trunc|common)$/) {
        croak("Unknown round mode '$r'");
    }

    # now round, by calling either bround or bfround:
    if (defined $a) {
        $self -> bround(int($a), $r)
          if !defined $self->{accuracy} || $self->{accuracy} >= $a;
    } else {                  # both can't be undefined due to early out
        $self -> bfround(int($p), $r)
          if !defined $self->{precision} || $self->{precision} <= $p;
    }

    # bround() or bfround() already called bnorm() if nec.
    $self;
}

sub bround {
    # accuracy: +$n preserve $n digits from left,
    #           -$n preserve $n digits from right (f.i. for 0.1234 style in MBF)
    # no-op for $n == 0
    # and overwrite the rest with 0's, return normalized number
    # do not return $x->bnorm(), but $x

    my ($class, $x, @a) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('bround');

    my ($scale, $mode) = $x->_scale_a(@a);
    return $x if !defined $scale; # no-op

    if ($x -> is_zero() || $scale == 0) {
        $x->{accuracy} = $scale
          if !defined $x->{accuracy} || $x->{accuracy} > $scale; # 3 > 2
        return $x;
    }
    return $x if !$x -> is_finite(); # inf, NaN

    # we have fewer digits than we want to scale to
    my $len = $x -> length();
    # convert $scale to a scalar in case it is an object (put's a limit on the
    # number length, but this would already limited by memory constraints),
    # makes it faster
    $scale = $scale -> numify() if ref ($scale);

    # scale < 0, but > -len (not >=!)
    if (($scale < 0 && $scale < -$len-1) || ($scale >= $len)) {
        $x->{accuracy} = $scale
          if !defined $x->{accuracy} || $x->{accuracy} > $scale; # 3 > 2
        return $x;
    }

    # count of 0's to pad, from left (+) or right (-): 9 - +6 => 3, or |-6| => 6
    my ($pad, $digit_round, $digit_after);
    $pad = $len - $scale;
    $pad = abs($scale-1) if $scale < 0;

    # do not use digit(), it is very costly for binary => decimal
    # getting the entire string is also costly, but we need to do it only once
    my $xs = $LIB->_str($x->{value});
    my $pl = -$pad-1;

    # pad:   123: 0 => -1, at 1 => -2, at 2 => -3, at 3 => -4
    # pad+1: 123: 0 => 0, at 1 => -1, at 2 => -2, at 3 => -3
    $digit_round = '0';
    $digit_round = substr($xs, $pl, 1) if $pad <= $len;
    $pl++;
    $pl ++ if $pad >= $len;
    $digit_after = '0';
    $digit_after = substr($xs, $pl, 1) if $pad > 0;

    # in case of 01234 we round down, for 6789 up, and only in case 5 we look
    # closer at the remaining digits of the original $x, remember decision
    my $round_up = 1;           # default round up
    $round_up -- if
      ($mode eq 'trunc')                      ||   # trunc by round down
        ($digit_after =~ /[01234]/)           ||   # round down anyway,
          # 6789 => round up
          ($digit_after eq '5')               &&   # not 5000...0000
            ($x->_scan_for_nonzero($pad, $xs, $len) == 0)   &&
              (
               ($mode eq 'even') && ($digit_round =~ /[24680]/) ||
               ($mode eq 'odd')  && ($digit_round =~ /[13579]/) ||
               ($mode eq '+inf') && ($x->{sign} eq '-')         ||
               ($mode eq '-inf') && ($x->{sign} eq '+')         ||
               ($mode eq 'zero') # round down if zero, sign adjusted below
              );
    my $put_back = 0;           # not yet modified

    if (($pad > 0) && ($pad <= $len)) {
        substr($xs, -$pad, $pad) = '0' x $pad; # replace with '00...'
        $xs =~ s/^0+(\d)/$1/;                  # "00000" -> "0"
        $put_back = 1;                         # need to put back
    } elsif ($pad > $len) {
        $x -> bzero();          # round to '0'
    }

    if ($round_up) {            # what gave test above?
        $put_back = 1;                               # need to put back
        $pad = $len, $xs = '0' x $pad if $scale < 0; # tlr: whack 0.51=>1.0

        # we modify directly the string variant instead of creating a number
        # and adding it, since that is faster (we already have the string)
        my $c = 0;
        $pad ++;                # for $pad == $len case
        while ($pad <= $len) {
            $c = substr($xs, -$pad, 1) + 1;
            $c = '0' if $c eq '10';
            substr($xs, -$pad, 1) = $c;
            $pad++;
            last if $c != 0;    # no overflow => early out
        }
        $xs = '1'.$xs if $c == 0;
    }
    $x->{value} = $LIB->_new($xs) if $put_back == 1; # put back, if needed

    $x->{accuracy} = $scale if $scale >= 0;
    if ($scale < 0) {
        $x->{accuracy} = $len+$scale;
        $x->{accuracy} = 0 if $scale < -$len;
    }
    $x;
}

sub bfround {
    # precision: round to the $Nth digit left (+$n) or right (-$n) from the '.'
    # $n == 0 || $n == 1 => round to integer

    my ($class, $x, @p) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('bfround');

    my ($scale, $mode) = $x->_scale_p(@p);

    return $x if !defined $scale;

    # no-op for Math::BigInt objects if $n <= 0
    $x = $x -> bround($x -> length() - $scale, $mode) if $scale > 0;

    $x->{accuracy} = undef;
    $x->{precision} = $scale;          # store new precision
    $x;
}

sub fround {
    # Exists to make life easier for switch between MBF and MBI (should we
    # autoload fxxx() like MBF does for bxxx()?)
    my $x = shift;
    $x = __PACKAGE__ -> new($x) unless ref $x;
    $x -> bround(@_);
}

sub bfloor {
    # round towards minus infinity; no-op since it's already integer
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('bfloor');

    $x -> round(@r);
}

sub bceil {
    # round towards plus infinity; no-op since it's already int
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('bceil');

    $x -> round(@r);
}

sub bint {
    # round towards zero; no-op since it's already integer
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('bint');

    $x->round(@r);
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
        return $class -> bnan() unless $arg -> is_int();
    }

    # Upgrade?

    my $upg = $class -> upgrade();
    if ($upg) {
        my $do_upgrade = 0;
        for my $arg (@args) {
            unless ($arg -> isa(__PACKAGE__)) {
                $do_upgrade = 1;
                last;
            }
        }
        if ($do_upgrade) {
            my $x = shift @args;
            $x -> _upg();
            return $x -> bgcd(@args);
        }
    }

    my $x = shift @args;
    $x = $x -> copy();          # bgcd() and blcm() never modify any operands
    while (@args) {
        my $y = shift @args;
        $x->{value} = $LIB->_gcd($x->{value}, $y->{value});
        last if $LIB->_is_one($x->{value});
    }

    return $x -> babs();
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

    # Upgrade?

    my $upg = $class -> upgrade();
    if ($upg) {
        my $do_upgrade = 0;
        for my $arg (@args) {
            unless ($arg -> isa(__PACKAGE__)) {
                $do_upgrade = 1;
                last;
            }
        }
        if ($do_upgrade) {
            my $x = shift @args;
            $x -> _upg();
            return $x -> bgcd(@args);
        }
    }

    my $x = shift @args;
    $x = $x -> copy();          # bgcd() and blcm() never modify any operands

    while (@args) {
        my $y = shift @args;
        return $x -> bnan() if !$y -> is_int();         # is $y not integer?
        $x -> {value} = $LIB->_lcm($x -> {value}, $y -> {value});
    }

    return $x -> babs();
}

###############################################################################
# Object property methods
###############################################################################

sub sign {
    # return the sign of the number: +/-/-inf/+inf/NaN
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    $x->{sign};
}

sub digit {
    # return the nth decimal digit, negative values count backward, 0 is right
    my (undef, $x, $n, @r) = ref($_[0]) ? (undef, @_) : objectify(1, @_);

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    $n = $n -> numify() if ref($n);
    $LIB->_digit($x->{value}, $n || 0);
}

sub bdigitsum {
    # like digitsum(), but assigns the result to the invocand
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    # Don't modify constant (read-only) objects.

    return $x if $x -> modify('bdigitsum');

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    return $x           if $x -> is_nan();
    return $x -> bnan() if $x -> is_inf();

    $x -> {value} = $LIB -> _digitsum($x -> {value});
    $x -> {sign}  = '+';
    return $x;
}

sub digitsum {
    # compute sum of decimal digits and return it
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    return $class -> bnan() if $x -> is_nan();
    return $class -> bnan() if $x -> is_inf();

    my $y = $class -> bzero();
    $y -> {value} = $LIB -> _digitsum($x -> {value});
    $y -> round(@r);
}

sub length {
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    my $e = $LIB->_len($x->{value});
    wantarray ? ($e, 0) : $e;
}

sub mantissa {
    # return the mantissa (compatible to Math::BigFloat, e.g. reduced)
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    if (!$x -> is_finite()) {
        # for NaN, +inf, -inf: keep the sign
        return $class -> new($x->{sign}, @r);
    }
    my $m = $x -> copy();
    $m -> precision(undef);
    $m -> accuracy(undef);

    # that's a bit inefficient:
    my $zeros = $LIB->_zeros($m->{value});
    $m = $m -> brsft($zeros, 10) if $zeros != 0;
    $m -> round(@r);
}

sub exponent {
    # return a copy of the exponent (here always 0, NaN or 1 for $m == 0)
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    if (!$x -> is_finite()) {
        my $s = $x->{sign};
        $s =~ s/^[+-]//; # NaN, -inf, +inf => NaN or inf
        return $class -> new($s, @r);
    }
    return $class -> bzero(@r) if $x -> is_zero();

    # 12300 => 2 trailing zeros => exponent is 2
    $class -> new($LIB->_zeros($x->{value}), @r);
}

sub parts {
    # return a copy of both the exponent and the mantissa
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    ($x -> mantissa(@r), $x -> exponent(@r));
}

# Parts used for scientific notation with significand/mantissa and exponent as
# integers. E.g., "12345.6789" is returned as "123456789" (mantissa) and "-4"
# (exponent).

sub sparts {
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    # Not-a-number.

    if ($x -> is_nan()) {
        my $mant = $class -> bnan(@r);          # mantissa
        return $mant unless wantarray;          # scalar context
        my $expo = $class -> bnan(@r);          # exponent
        return $mant, $expo;                    # list context
    }

    # Infinity.

    if ($x -> is_inf()) {
        my $mant = $class -> binf($x->{sign}, @r);  # mantissa
        return $mant unless wantarray;              # scalar context
        my $expo = $class -> binf('+', @r);         # exponent
        return $mant, $expo;                        # list context
    }

    # Finite number.

    my $mant   = $x -> copy();
    my $nzeros = $LIB -> _zeros($mant -> {value});

    $mant -> {value}
      = $LIB -> _rsft($mant -> {value}, $LIB -> _new($nzeros), 10)
        if $nzeros != 0;
    return $mant unless wantarray;

    my $expo = $class -> new($nzeros, @r);
    return $mant, $expo;
}

# Parts used for normalized notation with significand/mantissa as either 0 or a
# number in the semi-open interval [1,10). E.g., "12345.6789" is returned as
# "1.23456789" and "4".

sub nparts {
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    # Not-a-Number and Infinity.

    return $x -> sparts(@r) if $x -> is_nan() || $x -> is_inf();

    # Finite number.

    my ($mant, $expo) = $x -> sparts(@r);
    if ($mant -> bcmp(0)) {
        my ($ndigtot, $ndigfrac) = $mant -> length();
        my $expo10adj = $ndigtot - $ndigfrac - 1;

        if ($expo10adj > 0) {          # if mantissa is not an integer
            return $x -> _upg() -> nparts(@r) if $class -> upgrade();
            $mant -> bnan(@r);
            return $mant unless wantarray;
            $expo -> badd($expo10adj, @r);
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

    return $x -> sparts(@r) if $x -> is_nan() || $x -> is_inf();

    # Finite number.

    my ($mant, $expo) = $x -> sparts(@r);

    if ($mant -> bcmp(0)) {
        my $ndigmant  = $mant -> length();
        $expo -> badd($ndigmant, @r);

        # $c is the number of digits that will be in the integer part of the
        # final mantissa.

        my $c = $expo -> copy() -> bdec() -> bmod(3) -> binc();
        $expo -> bsub($c);

        if ($ndigmant > $c) {
            return $x -> _upg() -> eparts(@r) if $class -> upgrade();
            $mant -> bnan(@r);
            return $mant unless wantarray;
            return $mant, $expo;
        }

        $mant -> blsft($c - $ndigmant, 10, @r);
    }

    return $mant unless wantarray;
    return $mant, $expo;
}

# Parts used for decimal notation, e.g., "12345.6789" is returned as "12345"
# (integer part) and "0.6789" (fraction part).

sub dparts {
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    # Not-a-number.

    if ($x -> is_nan()) {
        my $int = $class -> bnan(@r);
        return $int unless wantarray;
        my $frc = $class -> bzero(@r);    # or NaN?
        return $int, $frc;
    }

    # Infinity.

    if ($x -> is_inf()) {
        my $int = $class -> binf($x->{sign}, @r);
        return $int unless wantarray;
        my $frc = $class -> bzero(@r);
        return $int, $frc;
    }

    # Finite number.

    my $int = $x -> copy() -> round(@r);
    return $int unless wantarray;

    my $frc = $class -> bzero(@r);
    return $int, $frc;
}

# Fractional parts with the numerator and denominator as integers. E.g.,
# "123.4375" is returned as "1975" and "16".

sub fparts {
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    # NaN => NaN/NaN

    if ($x -> is_nan()) {
        return $class -> bnan(@r) unless wantarray;
        return $class -> bnan(@r), $class -> bnan(@r);
    }

    # ±Inf => ±Inf/1

    if ($x -> is_inf()) {
        my $numer = $class -> binf($x->{sign}, @r);
        return $numer unless wantarray;
        my $denom = $class -> bone(@r);
        return $numer, $denom;
    }

    # N => N/1

    my $numer = $x -> copy() -> round(@r);
    return $numer unless wantarray;

    my $denom = $x -> copy();
    $denom -> {sign}  = "+";
    $denom -> {value} = $LIB -> _one();
    $denom -> round(@r);
    return $numer, $denom;
}

sub numerator {
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    return $x -> copy() -> round(@r);
}

sub denominator {
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    return $x -> is_nan() ? $class -> bnan(@r) : $class -> bone(@r);
}

###############################################################################
# String conversion methods
###############################################################################

sub bstr {
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    # Inf and NaN

    if ($x->{sign} ne '+' && $x->{sign} ne '-') {
        return $x->{sign} unless $x -> is_inf("+");     # -inf, NaN
        return 'inf';                                   # +inf
    }

    # Upgrade?

    $x -> _upg() -> bstr(@r) if $class -> upgrade() && !$x -> isa(__PACKAGE__);

    # Finite number

    my $str = $LIB->_str($x->{value});
    return $x->{sign} eq '-' ? "-$str" : $str;
}

# Scientific notation with significand/mantissa as an integer, e.g., "12345" is
# written as "1.2345e+4".

sub bsstr {
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    # Inf and NaN

    if ($x->{sign} ne '+' && $x->{sign} ne '-') {
        return $x->{sign} unless $x -> is_inf("+");  # -inf, NaN
        return 'inf';                                   # +inf
    }

    # Upgrade?

    $x -> _upg() -> bsstr(@r) if $class -> upgrade() && !$x -> isa(__PACKAGE__);

    # Finite number

    my $expo = $LIB -> _zeros($x->{value});
    my $mant = $LIB -> _str($x->{value});
    $mant = substr($mant, 0, -$expo) if $expo;  # strip trailing zeros

    ($x->{sign} eq '-' ? '-' : '') . $mant . 'e+' . $expo;
}

# Normalized notation, e.g., "12345" is written as "1.2345e+4".

sub bnstr {
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    # Inf and NaN

    if ($x->{sign} ne '+' && $x->{sign} ne '-') {
        return $x->{sign} unless $x -> is_inf("+");  # -inf, NaN
        return 'inf';                                   # +inf
    }

    # Upgrade?

    $x -> _upg() -> bnstr(@r) if $class -> upgrade() && !$x -> isa(__PACKAGE__);

    # Finite number

    my $expo = $LIB -> _zeros($x->{value});
    my $mant = $LIB -> _str($x->{value});
    $mant = substr($mant, 0, -$expo) if $expo;  # strip trailing zeros

    my $mantlen = CORE::length($mant);
    if ($mantlen > 1) {
        $expo += $mantlen - 1;                  # adjust exponent
        substr $mant, 1, 0, ".";                # insert decimal point
    }

    ($x->{sign} eq '-' ? '-' : '') . $mant . 'e+' . $expo;
}

# Engineering notation, e.g., "12345" is written as "12.345e+3".

sub bestr {
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    # Inf and NaN

    if ($x->{sign} ne '+' && $x->{sign} ne '-') {
        return $x->{sign} unless $x -> is_inf("+");  # -inf, NaN
        return 'inf';                                   # +inf
    }

    # Upgrade?

    $x -> _upg() -> bestr(@r) if $class -> upgrade() && !$x -> isa(__PACKAGE__);

    # Finite number

    my $expo = $LIB -> _zeros($x->{value});     # number of trailing zeros
    my $mant = $LIB -> _str($x->{value});       # mantissa as a string
    $mant = substr($mant, 0, -$expo) if $expo;  # strip trailing zeros
    my $mantlen = CORE::length($mant);          # length of mantissa
    $expo += $mantlen;

    my $dotpos = ($expo - 1) % 3 + 1;           # offset of decimal point
    $expo -= $dotpos;

    if ($dotpos < $mantlen) {
        substr $mant, $dotpos, 0, ".";          # insert decimal point
    } elsif ($dotpos > $mantlen) {
        $mant .= "0" x ($dotpos - $mantlen);    # append zeros
    }

    ($x->{sign} eq '-' ? '-' : '') . $mant . 'e+' . $expo;
}

# Decimal notation, e.g., "12345" (no exponent).

sub bdstr {
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    # Inf and NaN

    if ($x->{sign} ne '+' && $x->{sign} ne '-') {
        return $x->{sign} unless $x -> is_inf("+");     # -inf, NaN
        return 'inf';                                   # +inf
    }

    # Upgrade?

    $x -> _upg() -> bdstr(@r) if $class -> upgrade() && !$x -> isa(__PACKAGE__);

    # Finite number

    ($x->{sign} eq '-' ? '-' : '') . $LIB->_str($x->{value});
}

# Fraction notation, e.g., "123.4375" is written as "1975/16", but "123" is
# written as "123", not "123/1".

sub bfstr {
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    # Inf and NaN

    if ($x->{sign} ne '+' && $x->{sign} ne '-') {
        return $x->{sign} unless $x -> is_inf("+");     # -inf, NaN
        return 'inf';                                   # +inf
    }

    # Upgrade?

    $x -> _upg() -> bfstr(@r) if $class -> upgrade() && !$x -> isa(__PACKAGE__);

    # Finite number

    ($x->{sign} eq '-' ? '-' : '') . $LIB->_str($x->{value});
}

sub to_hex {
    # return as hex string with no prefix

    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    # Inf and NaN

    if ($x->{sign} ne '+' && $x->{sign} ne '-') {
        return $x->{sign} unless $x -> is_inf("+");     # -inf, NaN
        return 'inf';                                   # +inf
    }

    # Upgrade?

    return $x -> _upg() -> to_hex(@r) if $class -> upgrade() && !$x -> isa(__PACKAGE__);

    # Finite number

    my $hex = $LIB->_to_hex($x->{value});
    return $x->{sign} eq '-' ? "-$hex" : $hex;
}

sub to_oct {
    # return as octal string with no prefix

    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    # Inf and NaN

    if ($x->{sign} ne '+' && $x->{sign} ne '-') {
        return $x->{sign} unless $x -> is_inf("+");     # -inf, NaN
        return 'inf';                                   # +inf
    }

    # Upgrade?

    return $x -> _upg() -> to_oct(@r) if $class -> upgrade() && !$x -> isa(__PACKAGE__);

    # Finite number

    my $oct = $LIB->_to_oct($x->{value});
    return $x->{sign} eq '-' ? "-$oct" : $oct;
}

sub to_bin {
    # return as binary string with no prefix

    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    # Inf and NaN

    if ($x->{sign} ne '+' && $x->{sign} ne '-') {
        return $x->{sign} unless $x -> is_inf("+");     # -inf, NaN
        return 'inf';                                   # +inf
    }

    # Upgrade?

    return $x -> _upg() -> to_bin(@r) if $class -> upgrade() && !$x -> isa(__PACKAGE__);

    # Finite number

    my $bin = $LIB->_to_bin($x->{value});
    return $x->{sign} eq '-' ? "-$bin" : $bin;
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

    return $LIB->_to_bytes($x->{value});
}

sub to_ieee754 {
    my ($class, $x, $format, @r) = ref($_[0]) ? (ref($_[0]), @_)
                                              : objectify(1, @_);

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    return $x -> _upg() -> to_ieee754($format, @r)
      if $class -> upgrade() && !$x -> isa(__PACKAGE__);

    croak("the value to convert must be an integer, +/-infinity, or NaN")
      unless $x -> is_int() || $x -> is_inf() || $x -> is_nan();

    return $x -> as_float() -> to_ieee754($format);
}

sub to_fp80 {
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_)
                                              : objectify(1, @_);

    return $x -> _upg() -> to_fp80(@r)
      if $class -> upgrade() && !$x -> isa(__PACKAGE__);

    croak("the value to convert must be an integer, +/-infinity, or NaN")
      unless $x -> is_int() || $x -> is_inf() || $x -> is_nan();

    return $x -> as_float(@r) -> to_fp80();
}

sub to_base {
    # return a base anything string

    # $cs is the collation sequence
    my ($class, $x, $base, $cs, @r) = ref($_[0]) && ref($_[0]) eq ref($_[1])
                                    ? (ref($_[0]), @_) : objectify(2, @_);

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    croak("the value to convert must be a finite, non-negative integer")
      if $x -> is_neg() || !$x -> is_int();

    croak("the base must be a finite integer >= 2")
      if $base < 2 || ! $base -> is_int();

    # If no collating sequence is given, pass some of the conversions to
    # methods optimized for those cases.

    unless (defined $cs) {
        return    $x -> to_bin() if $base == 2;
        return    $x -> to_oct() if $base == 8;
        return uc $x -> to_hex() if $base == 16;
        return    $x -> bstr()   if $base == 10;
    }

    croak("to_base() requires a newer version of the $LIB library.")
      unless $LIB -> can('_to_base');

    return $x -> _upg() -> to_basen($base, $cs, @r)
      if $class -> upgrade() && (!$x -> isa(__PACKAGE__) ||
                               !$base -> isa(__PACKAGE__));

    return $LIB->_to_base($x->{value}, $base -> {value},
                          defined($cs) ? $cs : ());
}

sub to_base_num {
    # return a base anything array ref, e.g.,
    # Math::BigInt -> new(255) -> to_base_num(10) returns [2, 5, 5];

    # $cs is the collation sequence
    my ($class, $x, $base, @r) = ref($_[0]) && ref($_[0]) eq ref($_[1])
                               ? (ref($_[0]), @_) : objectify(2, @_);

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    croak("the value to convert must be a finite non-negative integer")
      if $x -> is_neg() || !$x -> is_int();

    croak("the base must be a finite integer >= 2")
      if $base < 2 || ! $base -> is_int();

    croak("to_base() requires a newer version of the $LIB library.")
      unless $LIB -> can('_to_base');

    return $x -> _upg() -> to_base_num($base, @r)
      if $class -> upgrade() && (!$x -> isa(__PACKAGE__) ||
                      !$base -> isa(__PACKAGE__));

    # Get a reference to an array of library thingies, and replace each element
    # with a Math::BigInt object using that thingy.

    my $vals = $LIB -> _to_base_num($x->{value}, $base -> {value});

    for my $i (0 .. $#$vals) {
        my $x = $class -> bzero();
        $x -> {value} = $vals -> [$i];
        $vals -> [$i] = $x;
    }

    return $vals;
}

sub as_hex {
    # return as hex string, with prefixed 0x

    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    return $x -> bstr() if !$x -> is_finite(); # inf, nan etc

    return $x -> _upg() -> as_hex(@r)
      if $class -> upgrade() && !$x -> isa(__PACKAGE__);

    my $hex = $LIB->_as_hex($x->{value});
    return $x->{sign} eq '-' ? "-$hex" : $hex;
}

sub as_oct {
    # return as octal string, with prefixed 0

    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    return $x -> bstr() if !$x -> is_finite(); # inf, nan etc

    return $x -> _upg() -> as_oct(@r)
      if $class -> upgrade() && !$x -> isa(__PACKAGE__);

    my $oct = $LIB->_as_oct($x->{value});
    return $x->{sign} eq '-' ? "-$oct" : $oct;
}

sub as_bin {
    # return as binary string, with prefixed 0b

    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

    carp "Rounding is not supported for ", (caller(0))[3], "()" if @r;

    return $x -> bstr() if !$x -> is_finite(); # inf, nan etc

    return $x -> _upg() -> as_bin(@r)
      if $class -> upgrade() && !$x -> isa(__PACKAGE__);

    my $bin = $LIB->_as_bin($x->{value});
    return $x->{sign} eq '-' ? "-$bin" : $bin;
}

*as_bytes = \&to_bytes;

###############################################################################
# Other conversion methods
###############################################################################

sub numify {
    # Make a Perl scalar number from a Math::BigInt object.
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);

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

    return $x -> _upg() -> numify(@r)
      if $class -> upgrade() && !$x -> isa(__PACKAGE__);

    my $num = 0 + $LIB->_num($x->{value});
    return $x->{sign} eq '-' ? -$num : $num;
}

###############################################################################
# Private methods and functions.
###############################################################################

sub _trailing_zeros {
    # return the amount of trailing zeros in $x (as scalar)
    my $x = shift;
    $x = __PACKAGE__ -> new($x) unless ref $x;

    return 0 if !$x -> is_finite(); # NaN, inf, -inf etc

    $LIB->_zeros($x->{value}); # must handle odd values, 0 etc
}

sub _scan_for_nonzero {
    # internal, used by bround() to scan for non-zeros after a '5'
    my ($x, $pad, $xs, $len) = @_;

    return 0 if $len == 1;      # "5" is trailed by invisible zeros
    my $follow = $pad - 1;
    return 0 if $follow > $len || $follow < 1;

    # use the string form to check whether only '0's follow or not
    substr ($xs, -$follow) =~ /[^0]/ ? 1 : 0;
}

sub _find_round_parameters {
    # After any operation or when calling round(), the result is rounded by
    # regarding the A & P from arguments, local parameters, or globals.

    # !!!!!!! If you change this, remember to change round(), too! !!!!!!!!!!

    # This procedure finds the round parameters, but it is for speed reasons
    # duplicated in round. Otherwise, it is tested by the testsuite and used
    # by bdiv().

    # returns ($self) or ($self, $a, $p, $r) - sets $self to NaN of both A and
    # P were requested/defined (locally or globally or both)

    my ($self, $a, $p, $r, @args) = @_;
    # $a accuracy, if given by caller
    # $p precision, if given by caller
    # $r round_mode, if given by caller
    # @args all 'other' arguments (0 for unary, 1 for binary ops)

    my $class = ref($self);       # find out class of argument(s)

    # convert to normal scalar for speed and correctness in inner parts
    $a = $a -> can('numify') ? $a -> numify() : "$a" if defined $a && ref($a);
    $p = $p -> can('numify') ? $p -> numify() : "$p" if defined $p && ref($p);

    # now pick $a or $p, but only if we have got "arguments"
    if (!defined $a) {
        foreach ($self, @args) {
            # take the defined one, or if both defined, the one that is smaller
            $a = $_->{accuracy}
              if (defined $_->{accuracy}) && (!defined $a || $_->{accuracy} < $a);
        }
    }
    if (!defined $p) {
        # even if $a is defined, take $p, to signal error for both defined
        foreach ($self, @args) {
            # take the defined one, or if both defined, the one that is bigger
            # -2 > -3, and 3 > 2
            $p = $_->{precision}
              if (defined $_->{precision}) && (!defined $p || $_->{precision} > $p);
        }
    }

    # if still none defined, use globals (#2)
    $a = $class -> accuracy()  unless defined $a;
    $p = $class -> precision() unless defined $p;

    # A == 0 is useless, so undef it to signal no rounding
    $a = undef if defined $a && $a == 0;

    # no rounding today?
    return ($self) unless defined $a || defined $p; # early out

    # set A and set P is an fatal error
    return ($self -> bnan()) if defined $a && defined $p; # error

    $r = $class -> round_mode() unless defined $r;
    if ($r !~ /^(even|odd|[+-]inf|zero|trunc|common)$/) {
        croak("Unknown round mode '$r'");
    }

    $a = int($a) if defined $a;
    $p = int($p) if defined $p;

    ($self, $a, $p, $r);
}

# Return true if the input is numeric and false if it is a string.

sub _is_numeric {
    shift;              # class name
    my $value = shift;
    no warnings 'numeric';
    # detect numbers
    # string & "" -> ""
    # number & "" -> 0 (with warning)
    # nan and inf can detect as numbers, so check with * 0
    return unless CORE::length((my $dummy = "") & $value);
    return unless 0 + $value eq $value;
    return 1 if $value * 0 == 0;
    return -1;          # Inf/NaN
}

# Trims the sign of the significand, the (absolute value of the) significand,
# the sign of the exponent, and the (absolute value of the) exponent. The
# returned values have no underscores ("_") or unnecessary leading or trailing
# zeros.

sub _trim_split_parts {
    shift;               # class name

    my $sig_sgn = shift() || '+';
    my $sig_str = shift() || '0';
    my $exp_sgn = shift() || '+';
    my $exp_str = shift() || '0';

    $sig_str =~ tr/_//d;                        # "1.0_0_0" -> "1.000"
    $sig_str =~ s/^0+//;                        # "01.000" -> "1.000"
    $sig_str =~ s/\.0*$//                       # "1.000" -> "1"
      || $sig_str =~ s/(\..*[^0])0+$/$1/;       # "1.010" -> "1.01"
    $sig_str = '0' unless CORE::length($sig_str);

    return '+', '0', '+', '0' if $sig_str eq '0';

    $exp_str =~ tr/_//d;                        # "01_234" -> "01234"
    $exp_str =~ s/^0+//;                        # "01234" -> "1234"
    $exp_str = '0' unless CORE::length($exp_str);
    $exp_sgn = '+' if $exp_str eq '0';          # "+3e-0" -> "+3e+0"

    return $sig_sgn, $sig_str, $exp_sgn, $exp_str;
}

# Takes any string representing a valid decimal number and splits it into four
# strings: the sign of the significand, the absolute value of the significand,
# the sign of the exponent, and the absolute value of the exponent. Both the
# significand and the exponent are in base 10.
#
# Perl accepts literals like the following. The value is 100.1.
#
#   1__0__.__0__1__e+0__1__     (prints "Misplaced _ in number")
#   1_0.0_1e+0_1
#
# Strings representing decimal numbers do not allow underscores, so only the
# following is valid
#
#   "10.01e+01"

sub _dec_str_to_dec_str_parts {
    my $class = shift;
    my $str   = shift;

    if ($str =~ /
                    ^

                    # optional leading whitespace
                    \s*

                    # optional sign
                    ( [+-]? )

                    # significand
                    (
                        # integer part and optional fraction part ...
                        \d+ (?: _+ \d+ )* _*
                        (?:
                            \.
                            (?: _* \d+ (?: _+ \d+ )* _* )?
                        )?
                    |
                        # ... or mandatory fraction part
                        \.
                        \d+ (?: _+ \d+ )* _*
                    )

                    # optional exponent
                    (?:
                        [Ee]
                        ( [+-]? )
                        ( \d+ (?: _+ \d+ )* _* )
                    )?

                    # optional trailing whitespace
                    \s*

                    $
                /x)
    {
        return $class -> _trim_split_parts($1, $2, $3, $4);
    }

    return;
}

# Takes any string representing a valid hexadecimal number and splits it into
# four strings: the sign of the significand, the absolute value of the
# significand, the sign of the exponent, and the absolute value of the
# exponent. The significand is in base 16, and the exponent is in base 2.
#
# Perl accepts literals like the following. The "x" might be a capital "X". The
# value is 32.0078125.
#
#   0x__1__0__.0__1__p+0__1__   (prints "Misplaced _ in number")
#   0x1_0.0_1p+0_1
#
# The CORE::hex() function does not accept floating point accepts
#
#   "0x_1_0"
#   "x_1_0"
#   "_1_0"

sub _hex_str_to_hex_str_parts {
    my $class = shift;
    my $str   = shift;

    if ($str =~ /
                    ^

                    # optional leading whitespace
                    \s*

                    # optional sign
                    ( [+-]? )

                    # optional hex prefix
                    (?: 0? [Xx] _* )?

                    # significand using the hex digits 0..9 and a..f
                    (
                        # integer part and optional fraction part ...
                        [0-9a-fA-F]+ (?: _+ [0-9a-fA-F]+ )* _*
                        (?:
                            \.
                            (?: _* [0-9a-fA-F]+ (?: _+ [0-9a-fA-F]+ )* _* )?
                        )?
                    |
                        # ... or mandatory fraction part
                        \.
                        [0-9a-fA-F]+ (?: _+ [0-9a-fA-F]+ )* _*
                    )

                    # optional exponent (power of 2) using decimal digits
                    (?:
                        [Pp]
                        ( [+-]? )
                        ( \d+ (?: _+ \d+ )* _* )
                    )?

                    # optional trailing whitespace
                    \s*

                    $
                /x)
    {
        return $class -> _trim_split_parts($1, $2, $3, $4);
    }

    return;
}

# Takes any string representing a valid octal number and splits it into four
# strings: the sign of the significand, the absolute value of the significand,
# the sign of the exponent, and the absolute value of the exponent. The
# significand is in base 8, and the exponent is in base 2.

sub _oct_str_to_oct_str_parts {
    my $class = shift;
    my $str   = shift;

    if ($str =~ /
                    ^

                    # optional leading whitespace
                    \s*

                    # optional sign
                    ( [+-]? )

                    # optional octal prefix
                    (?: 0? [Oo] _* )?

                    # significand using the octal digits 0..7
                    (
                        # integer part and optional fraction part ...
                        [0-7]+ (?: _+ [0-7]+ )* _*
                        (?:
                            \.
                            (?: _* [0-7]+ (?: _+ [0-7]+ )* _* )?
                        )?
                    |
                        # ... or mandatory fraction part
                        \.
                        [0-7]+ (?: _+ [0-7]+ )* _*
                    )

                    # optional exponent (power of 2) using decimal digits
                    (?:
                        [Pp]
                        ( [+-]? )
                        ( \d+ (?: _+ \d+ )* _* )
                    )?

                    # optional trailing whitespace
                    \s*

                    $
                /x)
    {
        return $class -> _trim_split_parts($1, $2, $3, $4);
    }

    return;
}

# Takes any string representing a valid binary number and splits it into four
# strings: the sign of the significand, the absolute value of the significand,
# the sign of the exponent, and the absolute value of the exponent. The
# significand is in base 2, and the exponent is in base 2.

sub _bin_str_to_bin_str_parts {
    my $class = shift;
    my $str   = shift;

    if ($str =~ /
                    ^

                    # optional leading whitespace
                    \s*

                    # optional sign
                    ( [+-]? )

                    # optional binary prefix
                    (?: 0? [Bb] _* )?

                    # significand using the binary digits 0 and 1
                    (
                        # integer part and optional fraction part ...
                        [01]+ (?: _+ [01]+ )* _*
                        (?:
                            \.
                            (?: _* [01]+ (?: _+ [01]+ )* _* )?
                        )?
                    |
                        # ... or mandatory fraction part
                        \.
                        [01]+ (?: _+ [01]+ )* _*
                    )

                    # optional exponent (power of 2) using decimal digits
                    (?:
                        [Pp]
                        ( [+-]? )
                        ( \d+ (?: _+ \d+ )* _* )
                    )?

                    # optional trailing whitespace
                    \s*

                    $
                /x)
    {
        return $class -> _trim_split_parts($1, $2, $3, $4);
    }

    return;
}

# Takes any string representing a valid decimal number and splits it into four
# parts: the sign of the significand, the absolute value of the significand as
# a libray thingy, the sign of the exponent, and the absolute value of the
# exponent as a library thingy.

sub _dec_str_parts_to_flt_lib_parts {
    shift;               # class name

    my ($sig_sgn, $sig_str, $exp_sgn, $exp_str) = @_;

    # Handle zero.

    if ($sig_str eq '0') {
        return '+', $LIB -> _zero(), '+', $LIB -> _zero();
    }

    # Absolute value of exponent as library "object".

    my $exp_lib = $LIB -> _new($exp_str);

    # If there is a dot in the significand, remove it so the significand
    # becomes an integer and adjust the exponent accordingly. Also remove
    # leading zeros which might now appear in the significand. E.g.,
    #
    #     12.345e-2  ->  12345e-5
    #     12.345e+2  ->  12345e-1
    #     0.0123e+5  ->  00123e+1  ->  123e+1

    my $idx = index $sig_str, '.';
    if ($idx >= 0) {
        substr($sig_str, $idx, 1) = '';

        # delta = length - index
        my $delta = $LIB -> _new(CORE::length($sig_str));
        $delta = $LIB -> _sub($delta, $LIB -> _new($idx));

        # exponent - delta
        ($exp_lib, $exp_sgn) = $LIB -> _ssub($exp_lib, $exp_sgn, $delta, '+');

        $sig_str =~ s/^0+//;
    }

    # If there are trailing zeros in the significand, remove them and
    # adjust the exponent. E.g.,
    #
    #     12340e-5  ->  1234e-4
    #     12340e-1  ->  1234e0
    #     12340e+3  ->  1234e4

    if ($sig_str =~ s/(0+)\z//) {
        my $len = CORE::length($1);
        ($exp_lib, $exp_sgn) =
          $LIB -> _sadd($exp_lib, $exp_sgn, $LIB -> _new($len), '+');
    }

    # At this point, the significand is empty or an integer with no trailing
    # zeros. The exponent is in base 10.

    unless (CORE::length $sig_str) {
        return '+', $LIB -> _zero(), '+', $LIB -> _zero();
    }

    # Absolute value of significand as library "object".

    my $sig_lib = $LIB -> _new($sig_str);

    return $sig_sgn, $sig_lib, $exp_sgn, $exp_lib;
}

# Takes any string representing a valid binary number and splits it into four
# parts: the sign of the significand, the absolute value of the significand as
# a libray thingy, the sign of the exponent, and the absolute value of the
# exponent as a library thingy.

sub _bin_str_parts_to_flt_lib_parts {
    shift;               # class name

    my ($sig_sgn, $sig_str, $exp_sgn, $exp_str, $bpc) = @_;
    my $bpc_lib = $LIB -> _new($bpc);

    # Handle zero.

    if ($sig_str eq '0') {
        return '+', $LIB -> _zero(), '+', $LIB -> _zero();
    }

    # Absolute value of exponent as library "object".

    my $exp_lib = $LIB -> _new($exp_str);

    # If there is a dot in the significand, remove it so the significand
    # becomes an integer and adjust the exponent accordingly. Also remove
    # leading zeros which might now appear in the significand. E.g., with
    # hexadecimal numbers
    #
    #     12.345p-2  ->  12345p-14
    #     12.345p+2  ->  12345p-10
    #     0.0123p+5  ->  00123p-11  ->  123p-11

    my $idx = index $sig_str, '.';
    if ($idx >= 0) {
        substr($sig_str, $idx, 1) = '';

        # delta = (length - index) * bpc
        my $delta = $LIB -> _new(CORE::length($sig_str));
        $delta = $LIB -> _sub($delta, $LIB -> _new($idx));
        $delta = $LIB -> _mul($delta, $bpc_lib) if $bpc != 1;

        # exponent - delta
        ($exp_lib, $exp_sgn) = $LIB -> _ssub($exp_lib, $exp_sgn, $delta, '+');

        $sig_str =~ s/^0+//;
    }

    # If there are trailing zeros in the significand, remove them and
    # adjust the exponent accordingly. E.g., with hexadecimal numbers
    #
    #     12340p-5  ->  1234p-1
    #     12340p-1  ->  1234p+3
    #     12340p+3  ->  1234p+7

    if ($sig_str =~ s/(0+)\z//) {

        # delta = length * bpc
        my $delta = $LIB -> _new(CORE::length($1));
        $delta = $LIB -> _mul($delta, $bpc_lib) if $bpc != 1;

        # exponent + delta
        ($exp_lib, $exp_sgn) = $LIB -> _sadd($exp_lib, $exp_sgn, $delta, '+');
    }

    # At this point, the significand is empty or an integer with no leading
    # or trailing zeros. The exponent is in base 2.

    unless (CORE::length $sig_str) {
        return '+', $LIB -> _zero(), '+', $LIB -> _zero();
    }

    # Absolute value of significand as library "object".

    my $sig_lib = $bpc == 1 ? $LIB -> _from_bin('0b' . $sig_str)
                : $bpc == 3 ? $LIB -> _from_oct('0'  . $sig_str)
                : $bpc == 4 ? $LIB -> _from_hex('0x' . $sig_str)
                : die "internal error: invalid exponent multiplier";

    # If the exponent (in base 2) is positive or zero ...

    if ($exp_sgn eq '+') {

        if (!$LIB -> _is_zero($exp_lib)) {

            # Multiply significand by 2 raised to the exponent.

            my $p = $LIB -> _pow($LIB -> _two(), $exp_lib);
            $sig_lib = $LIB -> _mul($sig_lib, $p);
            $exp_lib = $LIB -> _zero();
        }
    }

    # ... else if the exponent is negative ...

    else {

        # Rather than dividing the significand by 2 raised to the absolute
        # value of the exponent, multiply the significand by 5 raised to the
        # absolute value of the exponent and let the exponent be in base 10:
        #
        # a * 2^(-b) = a * 5^b * 10^(-b) = c * 10^(-b), where c = a * 5^b

        my $p = $LIB -> _pow($LIB -> _new("5"), $exp_lib);
        $sig_lib = $LIB -> _mul($sig_lib, $p);
    }

    # Adjust for the case when the conversion to decimal introduced trailing
    # zeros in the significand.

    my $n = $LIB -> _zeros($sig_lib);
    if ($n) {
        $n = $LIB -> _new($n);
        $sig_lib = $LIB -> _rsft($sig_lib, $n, 10);
        ($exp_lib, $exp_sgn) = $LIB -> _sadd($exp_lib, $exp_sgn, $n, '+');
    }

    return $sig_sgn, $sig_lib, $exp_sgn, $exp_lib;
}

# Takes any string representing a valid hexadecimal number and splits it into
# four parts: the sign of the significand, the absolute value of the
# significand as a libray thingy, the sign of the exponent, and the absolute
# value of the exponent as a library thingy.

sub _hex_str_to_flt_lib_parts {
    my $class = shift;
    my $str   = shift;
    if (my @parts = $class -> _hex_str_to_hex_str_parts($str)) {
        # 4 bits pr. chr
        return $class -> _bin_str_parts_to_flt_lib_parts(@parts, 4);
    }
    return;
}

# Takes any string representing a valid octal number and splits it into four
# parts: the sign of the significand, the absolute value of the significand as
# a libray thingy, the sign of the exponent, and the absolute value of the
# exponent as a library thingy.

sub _oct_str_to_flt_lib_parts {
    my $class = shift;
    my $str   = shift;
    if (my @parts = $class -> _oct_str_to_oct_str_parts($str)) {
        # 3 bits pr. chr
        return $class -> _bin_str_parts_to_flt_lib_parts(@parts, 3);
    }
    return;
}

# Takes any string representing a valid binary number and splits it into four
# parts: the sign of the significand, the absolute value of the significand as
# a libray thingy, the sign of the exponent, and the absolute value of the
# exponent as a library thingy.

sub _bin_str_to_flt_lib_parts {
    my $class = shift;
    my $str   = shift;
    if (my @parts = $class -> _bin_str_to_bin_str_parts($str)) {
        # 1 bit pr. chr
        return $class -> _bin_str_parts_to_flt_lib_parts(@parts, 1);
    }
    return;
}

# Decimal string is split into the sign of the signficant, the absolute value
# of the significand as library thingy, the sign of the exponent, and the
# absolute value of the exponent as a a library thingy.

sub _dec_str_to_flt_lib_parts {
    my $class = shift;
    my $str   = shift;
    if (my @parts = $class -> _dec_str_to_dec_str_parts($str)) {
        return $class -> _dec_str_parts_to_flt_lib_parts(@parts);
    }
    return;
}

# Decimal string to a string using decimal floating point notation.

sub dec_str_to_dec_flt_str {
    my $class = shift;
    my $str   = shift;
    if (my @parts = $class -> _dec_str_to_flt_lib_parts($str)) {
        return $class -> _flt_lib_parts_to_flt_str(@parts);
    }
    return;
}

# Hexdecimal string to a string using decimal floating point notation.

sub hex_str_to_dec_flt_str {
    my $class = shift;
    my $str   = shift;
    if (my @parts = $class -> _hex_str_to_flt_lib_parts($str)) {
        return $class -> _flt_lib_parts_to_flt_str(@parts);
    }
    return;
}

# Octal string to a string using decimal floating point notation.

sub oct_str_to_dec_flt_str {
    my $class = shift;
    my $str   = shift;
    if (my @parts = $class -> _oct_str_to_flt_lib_parts($str)) {
        return $class -> _flt_lib_parts_to_flt_str(@parts);
    }
    return;
}

# Binary string to a string decimal floating point notation.

sub bin_str_to_dec_flt_str {
    my $class = shift;
    my $str   = shift;
    if (my @parts = $class -> _bin_str_to_flt_lib_parts($str)) {
        return $class -> _flt_lib_parts_to_flt_str(@parts);
    }
    return;
}

# Decimal string to decimal notation (no exponent).

sub dec_str_to_dec_str {
    my $class = shift;
    my $str   = shift;
    if (my @parts = $class -> _dec_str_to_flt_lib_parts($str)) {
        return $class -> _flt_lib_parts_to_dec_str(@parts);
    }
    return;
}

# Hexdecimal string to decimal notation (no exponent).

sub hex_str_to_dec_str {
    my $class = shift;
    my $str   = shift;
    if (my @parts = $class -> _dec_str_to_flt_lib_parts($str)) {
        return $class -> _flt_lib_parts_to_dec_str(@parts);
    }
    return;
}

# Octal string to decimal notation (no exponent).

sub oct_str_to_dec_str {
    my $class = shift;
    my $str   = shift;
    if (my @parts = $class -> _oct_str_to_flt_lib_parts($str)) {
        return $class -> _flt_lib_parts_to_dec_str(@parts);
    }
    return;
}

# Binary string to decimal notation (no exponent).

sub bin_str_to_dec_str {
    my $class = shift;
    my $str   = shift;
    if (my @parts = $class -> _bin_str_to_flt_lib_parts($str)) {
        return $class -> _flt_lib_parts_to_dec_str(@parts);
    }
    return;
}

sub _flt_lib_parts_to_flt_str {
    my $class = shift;
    my @parts = @_;
    return $parts[0] . $LIB -> _str($parts[1])
      . 'e' . $parts[2] . $LIB -> _str($parts[3]);
}

sub _flt_lib_parts_to_dec_str {
    my $class = shift;
    my @parts = @_;

    # The number is an integer iff the exponent is non-negative.

    if ($parts[2] eq '+') {
        my $str = $parts[0]
          . $LIB -> _str($LIB -> _lsft($parts[1], $parts[3], 10));
        return $str;
    }

    # If it is not an integer, add a decimal point.

    else {
        my $mant     = $LIB -> _str($parts[1]);
        my $mant_len = CORE::length($mant);
        my $expo     = $LIB -> _num($parts[3]);
        my $len_cmp = $mant_len <=> $expo;
        if ($len_cmp <= 0) {
            return $parts[0] . '0.' . '0' x ($expo - $mant_len) . $mant;
        } else {
            substr $mant, $mant_len - $expo, 0, '.';
            return $parts[0] . $mant;
        }
    }
}

# Takes four arguments, the sign of the significand, the absolute value of the
# significand as a libray thingy, the sign of the exponent, and the absolute
# value of the exponent as a library thingy, and returns three parts: the sign
# of the rational number, the absolute value of the numerator as a libray
# thingy, and the absolute value of the denominator as a library thingy.
#
# For example, to convert data representing the value "+12e-2", then
#
#   $sm = "+";
#   $m  = $LIB -> _new("12");
#   $se = "-";
#   $e  = $LIB -> _new("2");
#   ($sr, $n, $d) = $class -> _flt_lib_parts_to_rat_lib_parts($sm, $m, $se, $e);
#
# returns data representing the same value written as the fraction "+3/25"
#
#   $sr = "+"
#   $n  = $LIB -> _new("3");
#   $d  = $LIB -> _new("12");

sub _flt_lib_parts_to_rat_lib_parts {
    my $self = shift;
    my ($msgn, $mabs, $esgn, $eabs) = @_;

    if ($esgn eq '-') {                 # "12e-2" -> "12/100" -> "3/25"
        my $num_lib = $LIB -> _copy($mabs);
        my $den_lib = $LIB -> _1ex($LIB -> _num($eabs));
        my $gcd_lib = $LIB -> _gcd($LIB -> _copy($num_lib), $den_lib);
        $num_lib = $LIB -> _div($LIB -> _copy($num_lib), $gcd_lib);
        $den_lib = $LIB -> _div($den_lib, $gcd_lib);
        return $msgn, $num_lib, $den_lib;
    }

    elsif (!$LIB -> _is_zero($eabs)) {  # "12e+2" -> "1200" -> "1200/1"
        return $msgn, $LIB -> _lsft($LIB -> _copy($mabs), $eabs, 10),
                      $LIB -> _one();
    }

    else {                              # "12e+0" -> "12" -> "12/1"
        return $msgn, $mabs, $LIB -> _one();
    }
}

# Add the function _register_callback() to Math::BigInt. It is provided for
# backwards compabibility so that old version of Math::BigRat etc. don't
# complain about missing it.

sub _register_callback { }

###############################################################################
# Other methods.
###############################################################################

sub objectify {
    # Convert strings and "foreign objects" to the objects we want.

    # The first argument, $count, is the number of following arguments that
    # objectify() looks at and converts to objects. The first is a classname.
    # If the given count is 0, all arguments will be used.

    # After the count is read, objectify obtains the name of the class to which
    # the following arguments are converted. If the second argument is a
    # reference, use the reference type as the class name. Otherwise, if it is
    # a string that looks like a class name, use that. Otherwise, use $class.

    # Caller:                        Gives us:
    #
    # $x->badd(1);                => ref x, scalar y
    # Class->badd(1, 2);           => classname x (scalar), scalar x, scalar y
    # Class->badd(Class->(1), 2);  => classname x (scalar), ref x, scalar y
    # Math::BigInt::badd(1, 2);    => scalar x, scalar y

    # A shortcut for the common case $x->unary_op(), in which case the argument
    # list is (0, $x) or (1, $x).

    return (ref($_[1]), $_[1]) if @_ == 2 && ($_[0] || 0) == 1 && ref($_[1]);

    # Check the context.

    unless (wantarray) {
        croak(__PACKAGE__ . "::objectify() needs list context");
    }

    # Get the number of arguments to objectify.

    my $count = shift;

    # Initialize the output array.

    my @a = @_;

    # If the first argument is a reference, use that reference type as our
    # class name. Otherwise, if the first argument looks like a class name,
    # then use that as our class name. Otherwise, use the default class name.

    my $class;
    if (ref($a[0])) {                   # reference?
        $class = ref($a[0]);
    } elsif ($a[0] =~ /^[A-Z].*::/) {   # string with class name?
        $class = shift @a;
    } else {
        $class = __PACKAGE__;           # default class name
    }

    $count ||= @a;
    unshift @a, $class;

    # What we upgrade to, if anything. Note that we need the whole upgrade
    # chain, since there might be multiple levels of upgrading. E.g., class A
    # upgrades to class B, which upgrades to class C. Delay getting the chain
    # until we actually need it.

    my @upg = ();
    my $have_upgrade_chain = 0;

    # Disable downgrading, because Math::BigFloat -> foo('1.0', '2.0') needs
    # floats.

    my $dng = $class -> downgrade();
    $class -> downgrade(undef);

  ARG: for my $i (1 .. $count) {

        my $ref = ref $a[$i];

        # Perl scalars are fed to the appropriate constructor.

        unless ($ref) {
            $a[$i] = $class -> new($a[$i]);
            next;
        }

        # If it is an object of the right class, all is fine.

        next if $ref -> isa($class);

        # Upgrading is OK, so skip further tests if the argument is upgraded,
        # but first get the whole upgrade chain if we haven't got it yet.

        unless ($have_upgrade_chain) {
            my $cls = $class;
            my $upg = $cls -> upgrade();
            while (defined $upg) {
                last if $upg eq $cls;
                push @upg, $upg;
                $cls = $upg;
                $upg = $cls -> upgrade();
            }
            $have_upgrade_chain = 1;
        }

        for my $upg (@upg) {
            next ARG if $ref -> isa($upg);
        }

        # See if we can call one of the as_xxx() methods. We don't know whether
        # the as_xxx() method returns an object or a scalar, so re-check
        # afterwards.

        my $recheck = 0;

        if ($class -> isa('Math::BigInt')) {
            if ($a[$i] -> can('as_int')) {
                $a[$i] = $a[$i] -> as_int();
                $recheck = 1;
            } elsif ($a[$i] -> can('as_number')) {
                $a[$i] = $a[$i] -> as_number();
                $recheck = 1;
            }
        }

        elsif ($class -> isa('Math::BigRat')) {
            if ($a[$i] -> can('as_rat')) {
                $a[$i] = $a[$i] -> as_rat();
                $recheck = 1;
            }
        }

        elsif ($class -> isa('Math::BigFloat')) {
            if ($a[$i] -> can('as_float')) {
                $a[$i] = $a[$i] -> as_float();
                $recheck = 1;
            }
        }

        # If we called one of the as_xxx() methods, recheck.

        if ($recheck) {
            $ref = ref($a[$i]);

            # Perl scalars are fed to the appropriate constructor.

            unless ($ref) {
                $a[$i] = $class -> new($a[$i]);
                next;
            }

            # If it is an object of the right class, all is fine.

            next if $ref -> isa($class);
        }

        # Last resort.

        $a[$i] = $class -> new($a[$i]);
    }

    # Restore the downgrading.

    $class -> downgrade($dng);

    return @a;
}

sub import {
    my $class = shift;
    $IMPORT++;                  # remember we did import()
    my @a;                      # unrecognized arguments

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
            # try  => 0 (no warn if unavailable module)
            # lib  => 1 (warn on fallback)
            # only => 2 (die on fallback)

            # Get the list of user-specified libraries.

            croak "Library argument for import parameter '$param' is missing"
              unless @_;
            my $libs = shift;
            croak "Library argument for import parameter '$param' is undefined"
              unless defined($libs);

            # Check and clean up the list of user-specified libraries.

            my @libs;
            for my $lib (split /,/, $libs) {
                $lib =~ s/^\s+//;
                $lib =~ s/\s+$//;

                if ($lib =~ /[^a-zA-Z0-9_:]/) {
                    carp "Library name '$lib' contains invalid characters";
                    next;
                }

                if (! CORE::length $lib) {
                    carp "Library name is empty";
                    next;
                }

                $lib = "Math::BigInt::$lib" if $lib !~ /^Math::BigInt::/i;

                # If a library has already been loaded, that is OK only if the
                # requested library is identical to the loaded one.

                if (defined($LIB)) {
                    if ($lib ne $LIB) {
                        #carp "Library '$LIB' has already been loaded, so",
                        #  " ignoring requested library '$lib'";
                    }
                    next;
                }

                push @libs, $lib;
            }

            next if defined $LIB;

            croak "Library list contains no valid libraries" unless @libs;

            # Try to load the specified libraries, if any.

            for (my $i = 0 ; $i <= $#libs ; $i++) {
                my $lib = $libs[$i];
                eval "require $lib";
                unless ($@) {
                    $LIB = $lib;
                    last;
                }
            }

            next if defined $LIB;

            # No library has been loaded, and none of the requested libraries
            # could be loaded, and fallback and the user doesn't allow
            # fallback.

            if ($param eq 'only') {
                croak "Couldn't load the specified math lib(s) ",
                  join(", ", map "'$_'", @libs),
                  ", and fallback to '$DEFAULT_LIB' is not allowed";
            }

            # No library has been loaded, and none of the requested libraries
            # could be loaded, but the user accepts the use of a fallback
            # library, so try to load it.

            eval "require $DEFAULT_LIB";
            if ($@) {
                croak "Couldn't load the specified math lib(s) ",
                  join(", ", map "'$_'", @libs),
                  ", not even the fallback lib '$DEFAULT_LIB'";
            }

            # The fallback library was successfully loaded, but the user
            # might want to know that we are using the fallback.

            if ($param eq 'lib') {
                carp "Couldn't load the specified math lib(s) ",
                  join(", ", map "'$_'", @libs),
                  ", so using fallback lib '$DEFAULT_LIB'";
            }

            next;
        }

        # Unrecognized parameter.

        push @a, $param;
    }

    # Any non-':constant' stuff is handled by our parent, Exporter

    $class -> SUPER::import(@a);                        # for subclasses
    $class -> export_to_level(1, $class, @a) if @a;     # need this, too

    # We might not have loaded any backend library yet, either because the user
    # didn't specify any, or because the specified libraries failed to load and
    # the user allows the use of a fallback library.

    unless (defined $LIB) {
        eval "require $DEFAULT_LIB";
        if ($@) {
            croak "No lib specified, and couldn't load the default",
              " lib '$DEFAULT_LIB'";
        }
        $LIB = $DEFAULT_LIB;
    }

    # import done
}

1;

__END__

=pod

=head1 NAME

Math::BigInt - arbitrary size integer math package

=head1 SYNOPSIS

  use Math::BigInt;

  # or make it faster with huge numbers: install (optional)
  # Math::BigInt::GMP and always use (it falls back to
  # pure Perl if the GMP library is not installed):
  # (See also the L</Math Library> section!)

  # to warn if Math::BigInt::GMP cannot be found, use
  use Math::BigInt lib => 'GMP';

  # to suppress the warning if Math::BigInt::GMP cannot be found, use
  # use Math::BigInt try => 'GMP';

  # to die if Math::BigInt::GMP cannot be found, use
  # use Math::BigInt only => 'GMP';

  # Configuration methods (may be used as class methods and instance methods)

  Math::BigInt->accuracy($n);       # set accuracy
  Math::BigInt->accuracy();         # get accuracy
  Math::BigInt->precision($n);      # set precision
  Math::BigInt->precision();        # get precision
  Math::BigInt->round_mode($m);     # set rounding mode, must be
                                    # 'even', 'odd', '+inf', '-inf',
                                    # 'zero', 'trunc', or 'common'
  Math::BigInt->round_mode();       # get class rounding mode
  Math::BigInt->div_scale($n);      # set fallback accuracy
  Math::BigInt->div_scale();        # get fallback accuracy
  Math::BigInt->trap_inf($b);       # trap infinities or not
  Math::BigInt->trap_inf();         # get trap infinities status
  Math::BigInt->trap_nan($b);       # trap NaNs or not
  Math::BigInt->trap_nan();         # get trap NaNs status
  Math::BigInt->config($par, $val); # set configuration parameter
  Math::BigInt->config($par);       # get configuration parameter
  Math::BigInt->config();           # get hash with configuration
  Math::BigFloat->config("lib");    # get name of backend library

  # Generic constructor method (always returns a new object)

  $x = Math::BigInt->new($str);             # defaults to 0
  $x = Math::BigInt->new('256');            # from decimal
  $x = Math::BigInt->new('0256');           # from decimal
  $x = Math::BigInt->new('0xcafe');         # from hexadecimal
  $x = Math::BigInt->new('0x1.fap+7');      # from hexadecimal
  $x = Math::BigInt->new('0o377');          # from octal
  $x = Math::BigInt->new('0o1.35p+6');      # from octal
  $x = Math::BigInt->new('0b101');          # from binary
  $x = Math::BigInt->new('0b1.101p+3');     # from binary

  # Specific constructor methods (no prefix needed; when used as
  # instance method, the value is assigned to the invocand)

  $x = Math::BigInt->from_dec('234');       # from decimal
  $x = Math::BigInt->from_hex('cafe');      # from hexadecimal
  $x = Math::BigInt->from_hex('1.fap+7');   # from hexadecimal
  $x = Math::BigInt->from_oct('377');       # from octal
  $x = Math::BigInt->from_oct('1.35p+6');   # from octal
  $x = Math::BigInt->from_bin('1101');      # from binary
  $x = Math::BigInt->from_bin('1.101p+3');  # from binary
  $x = Math::BigInt->from_bytes($bytes);    # from byte string
  $x = Math::BigInt->from_base('why', 36);  # from any base
  $x = Math::BigInt->from_base_num([1, 0], 2);  # from any base
  $x = Math::BigInt->from_ieee754($b, $fmt);    # from IEEE-754 bytes
  $x = Math::BigInt->from_fp80($b);         # from x86 80-bit
  $x = Math::BigInt->bzero();               # create a +0
  $x = Math::BigInt->bone();                # create a +1
  $x = Math::BigInt->bone('-');             # create a -1
  $x = Math::BigInt->binf();                # create a +inf
  $x = Math::BigInt->binf('-');             # create a -inf
  $x = Math::BigInt->bnan();                # create a Not-A-Number
  $x = Math::BigInt->bpi();                 # returns pi

  $y = $x->copy();        # make a copy (unlike $y = $x)
  $y = $x->as_int();      # return as a Math::BigInt
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
  $x->bdigitsum();        # sum of decimal digits
  $x->binc();             # increment $x by 1
  $x->bdec();             # decrement $x by 1
  $x->badd($y);           # addition (add $y to $x)
  $x->bsub($y);           # subtraction (subtract $y from $x)
  $x->bmul($y);           # multiplication (multiply $x by $y)
  $x->bmuladd($y, $z);    # $x = $x * $y + $z
  $x->bdiv($y);           # division (floored)
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
  $x->buparrow($n, $y);   # Knuth's up-arrow notation
  $x->bhyperop($n, $y);   # n'th hyperoprator
  $x->backermann($y);     # the Ackermann function
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
  $x->digitsum();         # sum of decimal digits
  $x->length();           # return number of digits in number
  $x->mantissa();         # return (signed) mantissa as a Math::BigInt
  $x->exponent();         # return exponent as a Math::BigInt
  $x->parts();            # return (mantissa,exponent) as a Math::BigInt
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
  $x->to_base($b);        # as string in any base
  $x->to_base_num($b);    # as array of integers in any base
  $x->to_ieee754($fmt);   # to bytes encoded according to IEEE 754-2008
  $x->to_fp80();          # encode value in x86 80-bit format

  $x->as_hex();           # as signed hexadecimal string with "0x" prefix
  $x->as_bin();           # as signed binary string with "0b" prefix
  $x->as_oct();           # as signed octal string with "0" prefix

  # Other conversion methods (these don't modify the invocand)

  $x->numify();           # return as scalar (might overflow or underflow)

=head1 DESCRIPTION

Math::BigInt provides support for arbitrary precision integers. Overloading is
also provided for Perl operators.

=head2 Input

Input values to these routines may be any scalar number or string that looks
like a number and represents an integer. Anything that is accepted by Perl as a
literal numeric constant should be accepted by this module, except that finite
non-integers return NaN.

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

If the string can not be interpreted, or does not represent a finite integer,
NaN is returned.

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

Input given as scalar numbers might lose precision. Quote your input to ensure
that no digits are lost:

    $x = Math::BigInt->new( 56789012345678901234 );   # bad
    $x = Math::BigInt->new('56789012345678901234');   # good

Currently, C<Math::BigInt->new()> (no input argument) and
C<Math::BigInt->new("")> return 0. This might change in the future, so always
use the following explicit forms to get a zero:

    $zero = Math::BigInt->bzero();

=head2 Output

Output values are usually Math::BigInt objects.

Boolean operators L</is_zero()>, L</is_one()>, L</is_inf()>, etc. return true
or false.

Comparison operators L</bcmp()> and L</bacmp()>) return -1, 0, 1, or undef.

=head1 METHODS

=head2 Configuration methods

Each of the methods below (except L</config()>, L</accuracy()> and
L</precision()>) accepts three additional parameters. These arguments C<$A>,
C<$P> and C<$R> are C<accuracy>, C<precision> and C<round_mode>. Please see the
section about L</ACCURACY AND PRECISION> for more information.

Setting a class variable effects all object instance that are created
afterwards.

=over

=item accuracy()

    Math::BigInt->accuracy(5);      # set class accuracy
    $x->accuracy(5);                # set instance accuracy

    $A = Math::BigInt->accuracy();  # get class accuracy
    $A = $x->accuracy();            # get instance accuracy

Set or get the accuracy, i.e., the number of significant digits. The accuracy
must be an integer. If the accuracy is set to C<undef>, no rounding is done.

Alternatively, one can round the results explicitly using one of L</round()>,
L</bround()> or L</bfround()> or by passing the desired accuracy to the method
as an additional parameter:

    my $x = Math::BigInt->new(30000);
    my $y = Math::BigInt->new(7);
    print scalar $x->copy()->bdiv($y, 2);               # prints 4300
    print scalar $x->copy()->bdiv($y)->bround(2);       # prints 4300

Please see the section about L</ACCURACY AND PRECISION> for further details.

    $y = Math::BigInt->new(1234567);    # $y is not rounded
    Math::BigInt->accuracy(4);          # set class accuracy to 4
    $x = Math::BigInt->new(1234567);    # $x is rounded automatically
    print "$x $y";                      # prints "1235000 1234567"

    print $x->accuracy();       # prints "4"
    print $y->accuracy();       # also prints "4", since
                                #   class accuracy is 4

    Math::BigInt->accuracy(5);  # set class accuracy to 5
    print $x->accuracy();       # prints "4", since instance
                                #   accuracy is 4
    print $y->accuracy();       # prints "5", since no instance
                                #   accuracy, and class accuracy is 5

Note: Each class has it's own globals separated from Math::BigInt, but it is
possible to subclass Math::BigInt and make the globals of the subclass aliases
to the ones from Math::BigInt.

=item precision()

    Math::BigInt->precision(-2);     # set class precision
    $x->precision(-2);               # set instance precision

    $P = Math::BigInt->precision();  # get class precision
    $P = $x->precision();            # get instance precision

Set or get the precision, i.e., the place to round relative to the decimal
point. The precision must be a integer. Setting the precision to $P means that
each number is rounded up or down, depending on the rounding mode, to the
nearest multiple of 10**$P. If the precision is set to C<undef>, no rounding is
done.

You might want to use L</accuracy()> instead. With L</accuracy()> you set the
number of digits each result should have, with L</precision()> you set the
place where to round.

Please see the section about L</ACCURACY AND PRECISION> for further details.

    $y = Math::BigInt->new(1234567);    # $y is not rounded
    Math::BigInt->precision(4);         # set class precision to 4
    $x = Math::BigInt->new(1234567);    # $x is rounded automatically
    print $x;                           # prints "1230000"

Note: Each class has its own globals separated from Math::BigInt, but it is
possible to subclass Math::BigInt and make the globals of the subclass aliases
to the ones from Math::BigInt.

=item round_mode()

Set/get the rounding mode.

=item div_scale()

Set/get the fallback accuracy. This is the accuracy used when neither accuracy
nor precision is set explicitly. It is used when a computation might otherwise
attempt to return an infinite number of digits.

=item trap_inf()

Set/get the value determining whether infinities should cause a fatal error or
not.

=item trap_nan()

Set/get the value determining whether NaNs should cause a fatal error or not.

=item upgrade()

Set/get the class for upgrading. When a computation might result in a
non-integer, the operands are upgraded to this class. This is used for instance
by L<bignum>. The default is C<undef>, i.e., no upgrading.

    # with no upgrading
    $x = Math::BigInt->new(12);
    $y = Math::BigInt->new(5);
    print $x / $y, "\n";                # 2 as a Math::BigInt

    # with upgrading to Math::BigFloat
    Math::BigInt -> upgrade("Math::BigFloat");
    print $x / $y, "\n";                # 2.4 as a Math::BigFloat

    # with upgrading to Math::BigRat (after loading Math::BigRat)
    Math::BigInt -> upgrade("Math::BigRat");
    print $x / $y, "\n";                # 12/5 as a Math::BigRat

=item downgrade()

Set/get the class for downgrading. The default is C<undef>, i.e., no
downgrading. Downgrading is not done by Math::BigInt.

=item modify()

    $x->modify('bpowd');

This method returns 0 if the object can be modified with the given operation,
or 1 if not.

This is used for instance by L<Math::BigInt::Constant>.

=item config()

    Math::BigInt->config("trap_nan" => 1);      # set
    $accu = Math::BigInt->config("accuracy");   # get

Set or get class variables. Read-only parameters are marked as RO. Read-write
parameters are marked as RW. The following parameters are supported.

    Parameter       RO/RW   Description
                            Example
    ============================================================
    lib             RO      Name of the math backend library
                            Math::BigInt::Calc
    lib_version     RO      Version of the math backend library
                            0.30
    class           RO      The class of config you just called
                            Math::BigRat
    version         RO      version number of the class you used
                            0.10
    upgrade         RW      To which class numbers are upgraded
                            undef
    downgrade       RW      To which class numbers are downgraded
                            undef
    precision       RW      Global precision
                            undef
    accuracy        RW      Global accuracy
                            undef
    round_mode      RW      Global round mode
                            even
    div_scale       RW      Fallback accuracy for division etc.
                            40
    trap_nan        RW      Trap NaNs
                            undef
    trap_inf        RW      Trap +inf/-inf
                            undef

=back

=head2 Constructor methods

=over

=item new()

    $x = Math::BigInt->new($str,$A,$P,$R);

Creates a new Math::BigInt object from a scalar or another Math::BigInt object.
The input is accepted as decimal, hexadecimal (with leading '0x'), octal (with
leading ('0o') or binary (with leading '0b').

See L</Input> for more info on accepted input formats.

=item from_dec()

    $x = Math::BigInt->from_dec("314159");    # input is decimal

Interpret input as a decimal. It is equivalent to L</new()>, but does not
accept anything but strings representing finite, decimal numbers.

=item from_hex()

    $x = Math::BigInt->from_hex("0xcafe");    # input is hexadecimal

Interpret input as a hexadecimal string. A "0x" or "x" prefix is optional. A
single underscore character may be placed right after the prefix, if present,
or between any two digits. If the input is invalid, a NaN is returned.

=item from_oct()

    $x = Math::BigInt->from_oct("0775");      # input is octal

Interpret the input as an octal string and return the corresponding value. A
"0" (zero) prefix is optional. A single underscore character may be placed
right after the prefix, if present, or between any two digits. If the input is
invalid, a NaN is returned.

=item from_bin()

    $x = Math::BigInt->from_bin("0b10011");   # input is binary

Interpret the input as a binary string. A "0b" or "b" prefix is optional. A
single underscore character may be placed right after the prefix, if present,
or between any two digits. If the input is invalid, a NaN is returned.

=item from_bytes()

    $x = Math::BigInt->from_bytes("\xf3\x6b");  # $x = 62315

Interpret the input as a byte string, assuming big endian byte order. The
output is always a non-negative, finite integer.

In some special cases, L</from_bytes()> matches the conversion done by
unpack():

    $b = "\x4e";                             # one char byte string
    $x = Math::BigInt->from_bytes($b);       # = 78
    $y = unpack "C", $b;                     # ditto, but scalar

    $b = "\xf3\x6b";                         # two char byte string
    $x = Math::BigInt->from_bytes($b);       # = 62315
    $y = unpack "S>", $b;                    # ditto, but scalar

    $b = "\x2d\xe0\x49\xad";                 # four char byte string
    $x = Math::BigInt->from_bytes($b);       # = 769673645
    $y = unpack "L>", $b;                    # ditto, but scalar

    $b = "\x2d\xe0\x49\xad\x2d\xe0\x49\xad"; # eight char byte string
    $x = Math::BigInt->from_bytes($b);       # = 3305723134637787565
    $y = unpack "Q>", $b;                    # ditto, but scalar

=item from_ieee754()

    # set $x to 314159
    $x = Math::BigInt -> from_ieee754("40490fdb", "binary32");

Interpret the input as a value encoded as described in IEEE754-2008. NaN is
returned if the value is neither +/-infinity nor an integer.

See L<Math::BigFloat/from_ieee754()>.

=item from_fp80()

    # set $x to 314159
    $x = Math::BigInt -> from_fp80("40119965e00000000000");

Interpret the input as a value encoded in the x86 extended-precision 80-bit
format.

See L<Math::BigFloat/from_fp80()>.

=item from_base()

Given a string, a base, and an optional collation sequence, interpret the
string as a number in the given base. The collation sequence describes the
value of each character in the string.

If a collation sequence is not given, a default collation sequence is used. If
the base is less than or equal to 36, the collation sequence is the string
consisting of the 36 characters "0" to "9" and "A" to "Z". In this case, the
letter case in the input is ignored. If the base is greater than 36, and
smaller than or equal to 62, the collation sequence is the string consisting of
the 62 characters "0" to "9", "A" to "Z", and "a" to "z". A base larger than 62
requires the collation sequence to be specified explicitly.

These examples show standard binary, octal, and hexadecimal conversion. All
cases return 250.

    $x = Math::BigInt->from_base("11111010", 2);
    $x = Math::BigInt->from_base("372", 8);
    $x = Math::BigInt->from_base("fa", 16);

When the base is less than or equal to 36, and no collation sequence is given,
the letter case is ignored, so both of these also return 250:

    $x = Math::BigInt->from_base("6Y", 16);
    $x = Math::BigInt->from_base("6y", 16);

When the base greater than 36, and no collation sequence is given, the default
collation sequence contains both uppercase and lowercase letters, so
the letter case in the input is not ignored:

    $x = Math::BigInt->from_base("6S", 37);         # $x is 250
    $x = Math::BigInt->from_base("6s", 37);         # $x is 276
    $x = Math::BigInt->from_base("121", 3);         # $x is 16
    $x = Math::BigInt->from_base("XYZ", 36);        # $x is 44027
    $x = Math::BigInt->from_base("Why", 42);        # $x is 58314

The collation sequence can be any set of unique characters. These two cases
are equivalent

    $x = Math::BigInt->from_base("100", 2, "01");   # $x is 4
    $x = Math::BigInt->from_base("|--", 2, "-|");   # $x is 4

=item from_base_num()

Returns a new Math::BigInt object given an array of values and a base. This
method is equivalent to L</from_base()>, but works on numbers in an array
rather than characters in a string. Unlike L</from_base()>, all input values
may be arbitrarily large.

    $x = Math::BigInt->from_base_num([1, 1, 0, 1], 2)     # $x is 13
    $x = Math::BigInt->from_base_num([3, 125, 39], 128)   # $x is 65191

=item bzero()

    $x = Math::BigInt->bzero();
    $x->bzero();

Returns a new Math::BigInt object representing zero. If used as an instance
method, assigns the value to the invocand.

=item bone()

    $x = Math::BigInt->bone();          # +1
    $x = Math::BigInt->bone("+");       # +1
    $x = Math::BigInt->bone("-");       # -1
    $x->bone();                         # +1
    $x->bone("+");                      # +1
    $x->bone('-');                      # -1

Creates a new Math::BigInt object representing one. The optional argument is
either '-' or '+', indicating whether you want plus one or minus one. If used
as an instance method, assigns the value to the invocand.

=item binf()

    $x = Math::BigInt->binf($sign);

Creates a new Math::BigInt object representing infinity. The optional argument
is either '-' or '+', indicating whether you want infinity or minus infinity.
If used as an instance method, assigns the value to the invocand.

    $x->binf();
    $x->binf('-');

=item bnan()

    $x = Math::BigInt->bnan();

Creates a new Math::BigInt object representing NaN (Not A Number). If used as
an instance method, assigns the value to the invocand.

    $x->bnan();

=item bpi()

    $x = Math::BigInt->bpi(100);        # 3
    $x->bpi(100);                       # 3

Creates a new Math::BigInt object representing PI. If used as an instance
method, assigns the value to the invocand. With Math::BigInt this always
returns 3.

If upgrading is in effect, returns PI, rounded to N digits with the current
rounding mode:

    use Math::BigFloat;
    use Math::BigInt upgrade => "Math::BigFloat";
    print Math::BigInt->bpi(3), "\n";           # 3.14
    print Math::BigInt->bpi(100), "\n";         # 3.1415....

=item copy()

    $x->copy();         # make a true copy of $x (unlike $y = $x)

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

=head2 Boolean methods

None of these methods modify the invocand object.

=over

=item is_zero()

    $x->is_zero();              # true if $x is 0

Returns true if the invocand is zero and false otherwise.

=item is_one()

    $x->is_one();               # true if $x is +1
    $x->is_one("+");            # ditto
    $x->is_one("-");            # true if $x is -1

Returns true if the invocand is one and false otherwise.

=item is_finite()

    $x->is_finite();    # true if $x is not +inf, -inf or NaN

Returns true if the invocand is a finite number, i.e., it is neither +inf,
-inf, nor NaN.

=item is_inf()

    $x->is_inf();               # true if $x is +inf or -inf
    $x->is_inf("+");            # true if $x is +inf
    $x->is_inf("-");            # true if $x is -inf

Returns true if the invocand is infinite and false otherwise.

=item is_nan()

    $x->is_nan();               # true if $x is NaN

=item is_positive()

=item is_pos()

    $x->is_positive();          # true if > 0
    $x->is_pos();               # ditto

Returns true if the invocand is positive and false otherwise. A C<NaN> is
neither positive nor negative.

=item is_negative()

=item is_neg()

    $x->is_negative();          # true if < 0
    $x->is_neg();               # ditto

Returns true if the invocand is negative and false otherwise. A C<NaN> is
neither positive nor negative.

=item is_non_positive()

    $x->is_non_positive();      # true if <= 0

Returns true if the invocand is negative or zero.

=item is_non_negative()

    $x->is_non_negative();      # true if >= 0

Returns true if the invocand is positive or zero.

=item is_odd()

    $x->is_odd();               # true if odd, false for even

Returns true if the invocand is odd and false otherwise. C<NaN>, C<+inf>, and
C<-inf> are neither odd nor even.

=item is_even()

    $x->is_even();              # true if $x is even

Returns true if the invocand is even and false otherwise. C<NaN>, C<+inf>,
C<-inf> are not integers and are neither odd nor even.

=item is_int()

    $x->is_int();               # true if $x is an integer

Returns true if the invocand is an integer and false otherwise. C<NaN>,
C<+inf>, C<-inf> are not integers.

=back

=head2 Comparison methods

None of these methods modify the invocand object. Note that a C<NaN> is neither
less than, greater than, or equal to anything else, even a C<NaN>.

=over

=item bcmp()

    $x->bcmp($y);

Returns -1, 0, 1 depending on whether $x is less than, equal to, or grater than
$y. Returns undef if any operand is a NaN.

=item bacmp()

    $x->bacmp($y);

Returns -1, 0, 1 depending on whether the absolute value of $x is less than,
equal to, or grater than the absolute value of $y. Returns undef if any operand
is a NaN.

=item beq()

    $x -> beq($y);

Returns true if and only if $x is equal to $y, and false otherwise.

=item bne()

    $x -> bne($y);

Returns true if and only if $x is not equal to $y, and false otherwise.

=item blt()

    $x -> blt($y);

Returns true if and only if $x is equal to $y, and false otherwise.

=item ble()

    $x -> ble($y);

Returns true if and only if $x is less than or equal to $y, and false
otherwise.

=item bgt()

    $x -> bgt($y);

Returns true if and only if $x is greater than $y, and false otherwise.

=item bge()

    $x -> bge($y);

Returns true if and only if $x is greater than or equal to $y, and false
otherwise.

=back

=head2 Arithmetic methods

These methods modify the invocand object and returns it.

=over

=item bneg()

    $x->bneg();

Negate the number, e.g. change the sign between '+' and '-', or between '+inf'
and '-inf', respectively. Does nothing for NaN or zero.

=item babs()

    $x->babs();

Set the number to its absolute value, e.g. change the sign from '-' to '+'
and from '-inf' to '+inf', respectively. Does nothing for NaN or positive
numbers.

=item bsgn()

    $x->bsgn();

Signum function. Set the number to -1, 0, or 1, depending on whether the
number is negative, zero, or positive, respectively. Does not modify NaNs.

=item bnorm()

    $x->bnorm();                        # normalize (no-op)

Normalize the number. This is a no-op and is provided only for backwards
compatibility.

=item binc()

    $x->binc();                 # increment x by 1

=item bdec()

    $x->bdec();                 # decrement x by 1

=item badd()

    $x->badd($y);               # addition (add $y to $x)

=item bsub()

    $x->bsub($y);               # subtraction (subtract $y from $x)

=item bmul()

    $x->bmul($y);               # multiplication (multiply $x by $y)

=item bdiv()

    $x->bdiv($y);               # set $x to quotient
    ($q, $r) = $x->bdiv($y);    # also return remainder

The behaviour of L</bdiv()> and L</bmod()> is based on Perl's C<%> operator,
which is the remainder after performing floored division.

Because of this, L</bdiv()> and L</bmod()> are aliases for L</bfdiv()> and
L</bfmod()>, respectively.

=item bmod()

    $x->bmod($y);               # modulus (x % y)

This is an alias for L</bfmod()>.

=item bfdiv()

    $x->bfdiv($y);              # return quotient
    ($q, $r) = $x->bfdiv($y);   # return quotient and remainder

Divides $x by $y by doing floored division (F-division), where the quotient is
the floored (rounded towards negative infinity) quotient of the two operands.
In list context, returns the quotient and the remainder. In scalar context,
only the quotient is returned.

    $q = floor($x / $y)         # quotient
    $r = $x - $q * $y           # remainder

With F-division, the remainder is either zero or has the same sign as the
divisor.

     7 /  4 => ( 1,  3)
    -7 /  4 => (-2,  1)
    -7 / -4 => ( 1, -3)
     7 / -4 => (-2, -1)

The behavior of the overloaded operator % agrees with the behavior of Perl's
built-in % operator (as documented in the perlop manpage), and the equation

    $x == ($x / $y) * $y + ($x % $y)

holds true for any finite $x and finite, non-zero $y.

Perl's "use integer" might change the behaviour of % and / for scalars. This is
because under 'use integer' Perl does what the underlying C library thinks is
right, and this varies. However, "use integer" does not change the way things
are done with Math::BigInt objects.

=item bfmod()

    $x->bfmod($y);              # floored modulus (x % y)

Returns $x modulo $y, i.e., the remainder after floored division (F-division).
This method is like Perl's % operator. See L</bfdiv()>.

=item btdiv()

    $x->btdiv($y);              # divide, set $x to quotient

Divides $x by $y by doing truncated division (T-division), where quotient is
the truncated (rouneded towards zero) quotient of the two operands. In list
context, returns the quotient and the remainder. The remainder is either zero
or has the same sign as the first operand. In scalar context, only the quotient
is returned.

=item btmod()

    $x->btmod($y);              # modulus

Returns the remainer after truncated division (T-division). See L</btdiv()>.

=item binv()

    $x->binv();

Invert the value of $x, i.e., compute 1/$x.

=item bsqrt()

    $x->bsqrt();                # calculate square root

Returns the square root truncated to an integer.

If you want a better approximation of the square root, then use:

    $x = Math::BigFloat->new(12);
    Math::BigFloat->precision(0);
    Math::BigFloat->round_mode("even");
    print $x->copy->bsqrt(),"\n";           # 4

    Math::BigFloat->precision(2);
    print $x->bsqrt(),"\n";                 # 3.46
    print $x->bsqrt(3),"\n";                # 3.464

=item bpow()

    $x->bpow($y);               # power of arguments (x ** y)

Returns $x raised to the power of $y. The first two modifies $x, the last one
doesn't:

    print $x->bpow($i),"\n";        # modifies $x
    print $x **= $i,"\n";           # ditto
    print $x ** $i,"\n";            # leaves $x alone

The form C<$x **= $y> is faster than C<$x = $x ** $y;>, though.

=item broot()

    $x->broot($N);

Calculates the $N'th root of C<$x>.

=item bmuladd()

    $x->bmuladd($y,$z);

Multiply $x by $y, and then add $z to the result,

This method was added in v1.88 of Math::BigInt.

=item bmodpow()

    $num->bmodpow($exp,$mod);           # modular exponentiation
                                        # ($num**$exp % $mod)

Returns the value of C<$num> taken to the power C<$exp> in the modulus
C<$mod> using binary exponentiation.  C<bmodpow> is far superior to
writing

    $num ** $exp % $mod

because it is much faster - it reduces internal variables into
the modulus whenever possible, so it operates on smaller numbers.

C<bmodpow> also supports negative exponents.

    bmodpow($num, -1, $mod)

is exactly equivalent to

    bmodinv($num, $mod)

=item bmodinv()

    $x->bmodinv($mod);          # modular multiplicative inverse

Returns the multiplicative inverse of C<$x> modulo C<$mod>. If

    $y = $x -> copy() -> bmodinv($mod)

then C<$y> is the number closest to zero, and with the same sign as C<$mod>,
satisfying

    ($x * $y) % $mod = 1 % $mod

If C<$x> and C<$y> are non-zero, they must be relative primes, i.e.,
C<bgcd($y, $mod)==1>. 'C<NaN>' is returned when no modular multiplicative
inverse exists.

=item blog()

    $x->blog($base, $accuracy);         # logarithm of x to the base $base

If C<$base> is not defined, Euler's number (e) is used:

    print $x->blog(undef, 100);         # log(x) to 100 digits

=item bexp()

    $x->bexp($accuracy);                # calculate e ** X

Calculates the expression C<e ** $x> where C<e> is Euler's number.

This method was added in v1.82 of Math::BigInt (April 2007).

See also L</blog()>.

=item bilog2()

Base 2 logarithm rounded down towards the nearest integer.

    $x->bilog2();               # int(log2(x)) = int(log(x)/log(2))

In list context a second argument is returned. This is 1 if the result is
exact, i.e., the input is an exact power of 2, and 0 otherwise.

=item bilog10()

Base 10 logarithm rounded down towards the nearest integer.

    $x->bilog10();              # int(log10(x)) = int(log(x)/log(10))

In list context a second argument is returned. This is 1 if the result is
exact, i.e., the input is an exact power of 10, and 0 otherwise.

=item bclog2()

Base 2 logarithm rounded up towards the nearest integer.

    $x->bclog2();               # ceil(log2(x)) = ceil(log(x)/log(2))

In list context a second argument is returned. This is 1 if the result is
exact, i.e., the input is an exact power of 2, and 0 otherwise.

=item bclog10()

Base 10 logarithm rounded up towards the nearest integer.

    $x->bclog10();              # ceil(log10(x)) = ceil(log(x)/log(10))

In list context a second argument is returned. This is 1 if the result is
exact, i.e., the input is an exact power of 10, and 0 otherwise.

=item bnok()

Combinations.

    $n->bnok($k);               # binomial coefficient n over k

Calculates the binomial coefficient n over k, also called the "choose"
function, which is the number of ways to choose a sample of k elements from a
set of n distinct objects where order does not matter and replacements are not
allowed. The result is equivalent to

              / n \      n!
    C(n, k) = |   | = --------  where 0 <= k <= n
              \ k /   k!(n-k)!

when n and k are non-negative. This method implements the full Kronenburg
extension (Kronenburg, M.J. "The Binomial Coefficient for Negative Arguments."
18 May 2011. http://arxiv.org/abs/1105.3689/) illustrated by the following
pseudo-code:

    if n >= 0 and k >= 0:
        return binomial(n, k)
    if k >= 0:
        return (-1)^k*binomial(-n+k-1, k)
    if k <= n:
        return (-1)^(n-k)*binomial(-k-1, n-k)
    else
        return 0

The behaviour is identical to the behaviour of the Maple and Mathematica
function for negative integers n, k.

=item bperm()

Permutations

    $n->bperm($k);

Calculates the number of ways to choose a sample of k elements from a set of n
distinct objects where order does matter and replacements are not allowed.

                n!
    P(n, k) = ------  where 0 <= k <= n
              (n-k)!

=item bhyperop()

=item hyperop()

    $a -> bhyperop($n, $b);         # modifies $a
    $x = $a -> hyperop($n, $b);     # does not modify $a

H_n(a, b) = a[n]b is the I<n>th hyperoperator,

    n = 0 : succession (b + 1)
    n = 1 : addition (a + b)
    n = 2 : multiplication (a * b)
    n = 3 : exponentiation (a ** b)
    n = 4 : tetration (a ** a ** ... ** a) (b occurrences of a)
    ...

                        / b+1                     if n = 0
                        | a                       if n = 1 and b = 0
    H_n(a, b) = a[n]b = | 0                       if n = 2 and b = 0
                        | 1                       if n >= 3 and b = 0
                        \ H_(n-1)(a, H_n(a, b-1)) otherwise

Note that the result can be a very large number, even for small operands. Also
note that the backend library C<Math::BigInt::GMP> silently returns the
incorrect result when the numbers are larger than it can handle. It is better
to use C<Math::BigInt::GMPz> or C<Math::BigInt::Pari>; they throw an error if
they can't handle the number.

See also L</buparrow()>, L<https://en.wikipedia.org/wiki/Hyperoperation>.

=item buparrow()

=item uparrow()

    $a -> buparrow($n, $b);         # modifies $a
    $x = $a -> uparrow($n, $b);     # does not modify $a

This method implements Knuth's up-arrow notation, where $n is a non-negative
integer representing the number of up-arrows. $n = 0 gives multiplication, $n =
1 gives exponentiation, $n = 2 gives tetration, $n = 3 gives hexation etc. The
following illustrates the relation between the first values of $n.

The L</buparrow()> method is equivalent to the L</bhyperop()> method with an
offset of two. The following two give the same result:

    $x -> buparrow($n, $b);
    $x -> bhyperop($n + 2, $b);

See also L</bhyperop()>,
L<https://en.wikipedia.org/wiki/Knuth%27s_up-arrow_notation>.

=item backermann()

=item ackermann()

    $m -> backermann($n);           # modifies $a
    $x = $m -> ackermann($n);       # does not modify $a

This method implements the Ackermann function:

             / n + 1              if m = 0
   A(m, n) = | A(m-1, 1)          if m > 0 and n = 0
             \ A(m-1, A(m, n-1))  if m > 0 and n > 0

Its value grows rapidly, even for small inputs. For example, A(4, 2) is an
integer of 19729 decimal digits.

See https://en.wikipedia.org/wiki/Ackermann_function

=item bsin()

    my $x = Math::BigInt->new(1);
    print $x->bsin(100), "\n";

Calculate the sine of $x, modifying $x in place.

In Math::BigInt, unless upgrading is in effect, the result is truncated to an
integer.

This method was added in v1.87 of Math::BigInt (June 2007).

=item bcos()

    my $x = Math::BigInt->new(1);
    print $x->bcos(100), "\n";

Calculate the cosine of $x, modifying $x in place.

In Math::BigInt, unless upgrading is in effect, the result is truncated to an
integer.

This method was added in v1.87 of Math::BigInt (June 2007).

=item batan()

    my $x = Math::BigFloat->new(0.5);
    print $x->batan(100), "\n";

Calculate the arcus tangens of $x, modifying $x in place.

In Math::BigInt, unless upgrading is in effect, the result is truncated to an
integer.

This method was added in v1.87 of Math::BigInt (June 2007).

=item batan2()

    my $x = Math::BigInt->new(1);
    my $y = Math::BigInt->new(1);
    print $y->batan2($x), "\n";

Calculate the arcus tangens of C<$y> divided by C<$x>, modifying $y in place.

In Math::BigInt, unless upgrading is in effect, the result is truncated to an
integer.

This method was added in v1.87 of Math::BigInt (June 2007).

=item bfac()

    $x->bfac();             # factorial of $x

Returns the factorial of C<$x>, i.e., $x*($x-1)*($x-2)*...*2*1, the product of
all positive integers up to and including C<$x>. C<$x> must be > -1. The
factorial of N is commonly written as N!, or N!1, when using the multifactorial
notation.

=item bdfac()

    $x->bdfac();                # double factorial of $x

Returns the double factorial of C<$x>, i.e., $x*($x-2)*($x-4)*... C<$x> must be
> -2. The double factorial of N is commonly written as N!!, or N!2, when using
the multifactorial notation.

=item btfac()

    $x->btfac();            # triple factorial of $x

Returns the triple factorial of C<$x>, i.e., $x*($x-3)*($x-6)*... C<$x> must be
> -3. The triple factorial of N is commonly written as N!!!, or N!3, when using
the multifactorial notation.

=item bmfac()

    $x->bmfac($k);          # $k'th multifactorial of $x

Returns the multi-factorial of C<$x>, i.e., $x*($x-$k)*($x-2*$k)*... C<$x> must
be > -$k. The multi-factorial of N is commonly written as N!K.

=item bfib()

    $F = $n->bfib();            # a single Fibonacci number
    @F = $n->bfib();            # a list of Fibonacci numbers

In scalar context, returns a single Fibonacci number. In list context, returns
a list of Fibonacci numbers. The invocand is the last element in the output.

The Fibonacci sequence is defined by

    F(0) = 0
    F(1) = 1
    F(n) = F(n-1) + F(n-2)

In list context, F(0) and F(n) is the first and last number in the output,
respectively. For example, if $n is 12, then C<< @F = $n->bfib() >> returns the
following values, F(0) to F(12):

    0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144

The sequence can also be extended to negative index n using the re-arranged
recurrence relation

    F(n-2) = F(n) - F(n-1)

giving the bidirectional sequence

       n  -7  -6  -5  -4  -3  -2  -1   0   1   2   3   4   5   6   7
    F(n)  13  -8   5  -3   2  -1   1   0   1   1   2   3   5   8  13

If $n is -12, the following values, F(0) to F(12), are returned:

    0, 1, -1, 2, -3, 5, -8, 13, -21, 34, -55, 89, -144

=item blucas()

    $F = $n->blucas();          # a single Lucas number
    @F = $n->blucas();          # a list of Lucas numbers

In scalar context, returns a single Lucas number. In list context, returns a
list of Lucas numbers. The invocand is the last element in the output.

The Lucas sequence is defined by

    L(0) = 2
    L(1) = 1
    L(n) = L(n-1) + L(n-2)

In list context, L(0) and L(n) is the first and last number in the output,
respectively. For example, if $n is 12, then C<< @L = $n->blucas() >> returns
the following values, L(0) to L(12):

    2, 1, 3, 4, 7, 11, 18, 29, 47, 76, 123, 199, 322

The sequence can also be extended to negative index n using the re-arranged
recurrence relation

    L(n-2) = L(n) - L(n-1)

giving the bidirectional sequence

       n  -7  -6  -5  -4  -3  -2  -1   0   1   2   3   4   5   6   7
    L(n)  29 -18  11  -7   4  -3   1   2   1   3   4   7  11  18  29

If $n is -12, the following values, L(0) to L(-12), are returned:

    2, 1, -3, 4, -7, 11, -18, 29, -47, 76, -123, 199, -322

=item blsft()

Left shift.

    $x->blsft($n);              # left shift $n places in base 2
    $x->blsft($n, $b);          # left shift $n places in base $b

The latter is equivalent to

    $x -> bmul($b -> copy() -> bpow($n));

=item brsft()

Right shift.

    $x->brsft($n);              # right shift $n places in base 2
    $x->brsft($n, $b);          # right shift $n places in base $b

The latter is equivalent to

    $x -> bdiv($b -> copy() -> bpow($n));

=back

=head2 Bitwise methods

For all bitwise methods, the operands are truncated to integers, i.e., rounded
towards zero, if necessary, before the method is applied. The bitwise methods
never upgrade, and they always return an integer.

=over

=item bblsft()

Bitwise left shift. This is equivalent to Perl's C<E<lt>E<lt>> operator.

    $x -> bblsft($n);           # left shift $n places in base 2

If C<$n> is negative, the shifting is done in the opposite direction, so these
two are equivalent for all C<$x> and C<$n>

    $y = $x -> bblsft($n);
    $y = $x -> bbrsft(-$n);

and also equivalent to

    $y = $x -> bmul(ref($x) -> new(2) -> bpow($n));   # if $n > 0
    $y = $x -> bdiv(ref($x) -> new(2) -> bpow($n));   # if $n < 0

=item bbrsft()

Bitwise right shift. This is equivalent to Perl's C<E<gt>E<gt>> operator.

    $x -> bbrsft($n);           # right shift $n places in base 2

If C<$n> is negative, the shifting is done in the opposite direction, so these
two are equivalent for all C<$x> and C<$n>

    $y = $x -> bbrsft($n);
    $y = $x -> bblsft(-$n);

and also equivalent to

    $y = $x -> bdiv(ref($x) -> new(2) -> bpow($n));   # if $n > 0
    $y = $x -> bmul(ref($x) -> new(2) -> bpow(-$n));  # if $n < 0

=item band()

    $x->band($y);               # bitwise and

=item bior()

    $x->bior($y);               # bitwise inclusive or

=item bxor()

    $x->bxor($y);               # bitwise exclusive or

=item bnot()

    $x->bnot();                 # bitwise not (two's complement)

Two's complement (bitwise not). This is equivalent to, but faster than,

    $x->binc()->bneg();

=back

=head2 Rounding methods

=over

=item round()

    $x->round($A,$P,$round_mode);

Round $x to accuracy C<$A> or precision C<$P> using the round mode
C<$round_mode>.

=item bround()

    $x->bround($N);               # accuracy: preserve $N digits

Rounds $x to an accuracy of $N digits.

=item bfround()

    $x->bfround($N);

Rounds to a multiple of 10**$N. Examples:

    Input            N          Result

    123456.123456    3          123500
    123456.123456    2          123450
    123456.123456   -2          123456.12
    123456.123456   -3          123456.123

=item bfloor()

    $x->bfloor();

Round $x towards minus infinity, i.e., set $x to the largest integer less than
or equal to $x.

=item bceil()

    $x->bceil();

Round $x towards plus infinity, i.e., set $x to the smallest integer greater
than or equal to $x.

=item bint()

    $x->bint();

Round $x towards zero.

=back

=head2 Other mathematical methods

=over

=item bgcd()

    $x -> bgcd($y);             # GCD of $x and $y
    $x -> bgcd($y, $z, ...);    # GCD of $x, $y, $z, ...

Returns the greatest common divisor (GCD), which is the largest positive
integer that divides each of the operands.

=item blcm()

    $x -> blcm($y);             # LCM of $x and $y
    $x -> blcm($y, $z, ...);    # LCM of $x, $y, $z, ...

Returns the least common multiple (LCM).

=back

=head2 Object property methods

=over

=item sign()

    $x->sign();

Return the sign, of $x, meaning either C<+>, C<->, C<-inf>, C<+inf> or NaN.

If you want $x to have a certain sign, use one of the following methods:

    $x->babs();                 # '+'
    $x->babs()->bneg();         # '-'
    $x->bnan();                 # 'NaN'
    $x->binf();                 # '+inf'
    $x->binf('-');              # '-inf'

=item digit()

    $x->digit($n);       # return the nth digit, counting from right

If C<$n> is negative, returns the digit counting from left.

=item bdigitsum()

    $x->bdigitsum();

Computes the sum of the base 10 digits and assigns the result to the invocand.

=item digitsum()

    $x->digitsum();

Computes the sum of the base 10 digits and returns it.

=item length()

    $x->length();
    ($xl, $fl) = $x->length();

Returns the number of digits in the decimal representation of the number. In
list context, returns the length of the integer and fraction part. For
Math::BigInt objects, the length of the fraction part is always 0.

The following probably doesn't do what you expect:

    $c = Math::BigInt->new(123);
    print $c->length(),"\n";                # prints 30

It prints both the number of digits in the number and in the fraction part
since print calls L</length()> in list context. Use something like:

    print scalar $c->length(),"\n";         # prints 3

=item mantissa()

    $x->mantissa();

Return the signed mantissa of $x as a Math::BigInt.

=item exponent()

    $x->exponent();

Return the exponent of $x as a Math::BigInt.

=item parts()

    $x->parts();

Returns the significand (mantissa) and the exponent as integers. In
Math::BigFloat, both are returned as Math::BigInt objects.

=item sparts()

Returns the significand (mantissa) and the exponent as integers. In scalar
context, only the significand is returned. The significand is the integer with
the smallest absolute value. The output of L</sparts()> corresponds to the
output from L</bsstr()>.

In Math::BigInt, this method is identical to L</parts()>.

=item nparts()

Returns the significand (mantissa) and exponent corresponding to normalized
notation. In scalar context, only the significand is returned. For finite
non-zero numbers, the significand's absolute value is greater than or equal to
1 and less than 10. The output of L</nparts()> corresponds to the output from
L</bnstr()>. In Math::BigInt, if the significand can not be represented as an
integer, upgrading is performed or NaN is returned.

=item eparts()

Returns the significand (mantissa) and exponent corresponding to engineering
notation. In scalar context, only the significand is returned. For finite
non-zero numbers, the significand's absolute value is greater than or equal to
1 and less than 1000, and the exponent is a multiple of 3. The output of
L</eparts()> corresponds to the output from L</bestr()>. In Math::BigInt, if
the significand can not be represented as an integer, upgrading is performed or
NaN is returned.

=item dparts()

Returns the integer part and the fraction part. If the fraction part can not be
represented as an integer, upgrading is performed or NaN is returned. The
output of L</dparts()> corresponds to the output from L</bdstr()>.

=item fparts()

Returns the smallest possible numerator and denominator so that the numerator
divided by the denominator gives back the original value. For finite numbers,
both values are integers. Mnemonic: fraction.

=item numerator()

Together with L</denominator()>, returns the smallest integers so that the
numerator divided by the denominator reproduces the original value. With
Math::BigInt, L</numerator()> simply returns a copy of the invocand.

=item denominator()

Together with L</numerator()>, returns the smallest integers so that the
numerator divided by the denominator reproduces the original value. With
Math::BigInt, L</denominator()> always returns either a 1 or a NaN.

=back

=head2 String conversion methods

=over

=item bstr()

Returns a string representing the number using decimal notation. In
Math::BigFloat, the output is zero padded according to the current accuracy or
precision, if any of those are defined.

=item bsstr()

Returns a string representing the number using scientific notation where both
the significand (mantissa) and the exponent are integers. The output
corresponds to the output from L</sparts()>.

      123 is returned as "123e+0"
     1230 is returned as "123e+1"
    12300 is returned as "123e+2"
    12000 is returned as "12e+3"
    10000 is returned as "1e+4"

=item bnstr()

Returns a string representing the number using normalized notation, the most
common variant of scientific notation. For finite non-zero numbers, the
absolute value of the significand is greater than or equal to 1 and less than
10. The output corresponds to the output from L</nparts()>.

      123 is returned as "1.23e+2"
     1230 is returned as "1.23e+3"
    12300 is returned as "1.23e+4"
    12000 is returned as "1.2e+4"
    10000 is returned as "1e+4"

=item bestr()

Returns a string representing the number using engineering notation. For finite
non-zero numbers, the absolute value of the significand is greater than or
equal to 1 and less than 1000, and the exponent is a multiple of 3. The output
corresponds to the output from L</eparts()>.

      123 is returned as "123e+0"
     1230 is returned as "1.23e+3"
    12300 is returned as "12.3e+3"
    12000 is returned as "12e+3"
    10000 is returned as "10e+3"

=item bdstr()

Returns a string representing the number using decimal notation. The output
corresponds to the output from L</dparts()>.

      123 is returned as "123"
     1230 is returned as "1230"
    12300 is returned as "12300"
    12000 is returned as "12000"
    10000 is returned as "10000"

=item bfstr()

Returns a string representing the number using fractional notation. The output
corresponds to the output from L</fparts()>.

        12.345 is returned as "2469/200"
       123.45 is returned as "2469/20"
      1234.5 is returned as "2469/2"
     12345 is returned as "12345"
    123450 is returned as "123450"

=item to_hex()

    $x->to_hex();

Returns a hexadecimal string representation of the number. See also
L</from_hex()>.

=item to_oct()

    $x->to_oct();

Returns an octal string representation of the number. See also L</from_oct()>.

=item to_bin()

    $x->to_bin();

Returns a binary string representation of the number. See also L</from_bin()>.

=item to_bytes()

    $x = Math::BigInt->new("1667327589");
    $s = $x->to_bytes();                    # $s = "cafe"

Returns a byte string representation of the number using big endian byte order.
The invocand must be a non-negative, finite integer. See also L</from_bytes()>.

=item to_ieee754()

See L<Math::BigFloat/to_ieee754()>.

=item to_fp80()

See L<Math::BigFloat/to_fp80()>.

=item to_base()

    $x = Math::BigInt->new("250");
    $x->to_base(2);     # returns "11111010"
    $x->to_base(8);     # returns "372"
    $x->to_base(16);    # returns "fa"

Returns a string representation of the number in the given base. If a collation
sequence is given, the collation sequence determines which characters are used
in the output.

Here are some more examples

    $x = Math::BigInt->new("16")->to_base(3);       # returns "121"
    $x = Math::BigInt->new("44027")->to_base(36);   # returns "XYZ"
    $x = Math::BigInt->new("58314")->to_base(42);   # returns "Why"
    $x = Math::BigInt->new("4")->to_base(2, "-|");  # returns "|--"

If the collation sequence are the bytes from "\x00" to "\xff", and the base is
256, then L</to_base()> returns the same output as L</to_bytes()>. In the
following example, $x and $y are identical:

    $cs = join "", map chr, 0 .. 255;  # collation sequence
    $x = Math::BigInt -> to_base("1230129310", 256, $cs)
    $y = Math::BigInt -> to_bytes("1230129310");

See L</from_base()> for information and examples.

=item to_base_num()

Converts the given number to the given base. This method is equivalent to
L</to_base()>, but returns numbers in an array rather than characters in a
string. In the output, the first element is the most significant.

    $x = Math::BigInt->new(13);         # decimal 13 is binary 1101
    $x->to_base_num(2);                 # returns [1, 1, 0, 1]

    $x = Math::BigInt->new(65191);
    $x->to_base_num(128);               # returns [3, 125, 39]

=item as_hex()

    $x->as_hex();

As, L</to_hex()>, but with a "0x" prefix.

=item as_oct()

    $x->as_oct();

As, L</to_oct()>, but with a "0" prefix.

=item as_bin()

    $x->as_bin();

As, L</to_bin()>, but with a "0b" prefix.

=item as_bytes()

This is an alias for L</to_bytes()>.

=back

=head2 Other conversion methods

=over

=item numify()

    print $x->numify();

Returns a Perl scalar from $x. It is used automatically whenever a scalar is
needed, for instance in array index operations.

=back

=head2 Utility methods

These utility methods are made public

=over

=item dec_str_to_dec_flt_str()

Takes a string representing any valid number using decimal notation and
converts it to a string representing the same number using decimal floating
point notation. The output consists of five parts joined together: the sign of
the significand, the absolute value of the significand as the smallest possible
integer, the letter "e", the sign of the exponent, and the absolute value of
the exponent. If the input is invalid, nothing is returned.

    $str2 = $class -> dec_str_to_dec_flt_str($str1);

Some examples

    Input           Output
    31400.00e-4     +314e-2
    -0.00012300e8   -123e+2
    0               +0e+0

=item hex_str_to_dec_flt_str()

Takes a string representing any valid number using hexadecimal notation and
converts it to a string representing the same number using decimal floating
point notation. The output has the same format as that of
L</dec_str_to_dec_flt_str()>.

    $str2 = $class -> hex_str_to_dec_flt_str($str1);

Some examples

    Input           Output
    0xff            +255e+0

Some examples

=item oct_str_to_dec_flt_str()

Takes a string representing any valid number using octal notation and converts
it to a string representing the same number using decimal floating point
notation. The output has the same format as that of
L</dec_str_to_dec_flt_str()>.

    $str2 = $class -> oct_str_to_dec_flt_str($str1);

=item bin_str_to_dec_flt_str()

Takes a string representing any valid number using binary notation and converts
it to a string representing the same number using decimal floating point
notation. The output has the same format as that of
L</dec_str_to_dec_flt_str()>.

    $str2 = $class -> bin_str_to_dec_flt_str($str1);

=item dec_str_to_dec_str()

Takes a string representing any valid number using decimal notation and
converts it to a string representing the same number using decimal notation. If
the number represents an integer, the output consists of a sign and the
absolute value. If the number represents a non-integer, the output consists of
a sign, the integer part of the number, the decimal point ".", and the fraction
part of the number without any trailing zeros. If the input is invalid, nothing
is returned.

=item hex_str_to_dec_str()

Takes a string representing any valid number using hexadecimal notation and
converts it to a string representing the same number using decimal notation.
The output has the same format as that of L</dec_str_to_dec_str()>.

=item oct_str_to_dec_str()

Takes a string representing any valid number using octal notation and converts
it to a string representing the same number using decimal notation. The output
has the same format as that of L</dec_str_to_dec_str()>.

=item bin_str_to_dec_str()

Takes a string representing any valid number using binary notation and converts
it to a string representing the same number using decimal notation. The output
has the same format as that of L</dec_str_to_dec_str()>.

=back

=head1 ACCURACY AND PRECISION

Math::BigInt and Math::BigFloat have full support for accuracy and precision
based rounding, both automatically after every operation, as well as manually.

This section describes the accuracy/precision handling in Math::BigInt and
Math::BigFloat as it used to be and as it is now, complete with an explanation
of all terms and abbreviations.

Not yet implemented things (but with correct description) are marked with '!',
things that need to be answered are marked with '?'.

In the next paragraph follows a short description of terms used here (because
these may differ from terms used by others people or documentation).

During the rest of this document, the shortcuts A (for accuracy), P (for
precision), R (rounding mode), and F (fallback) are be used.

=head2 Accuracy A

Number of significant digits. Leading zeros are not counted. A number may have
an accuracy greater than the non-zero digits when there are zeros in it or
trailing zeros. For example, 123.456 has A of 6, 10203 has 5, 123.0506 has 7,
123.45000 has 8 and 0.000123 has 3.

The string output (of floating point numbers) is padded with zeros:

    Initial value    P      A       Result          String
    ------------------------------------------------------------
    1234.01                 3       1230            1230
    1234.01                 6       1234.01         1234.01
    1234.1                  8       1234.1          1234.1000

For Math::BigInt objects, no padding occurs.

=head2 Precision P

Precision is a fixed number of digits before (positive) or after (negative) the
decimal point. For example, 123.45 has a precision of -2. 0 means an integer
like 123 (or 120). A precision of 2 means at least two digits to the left of
the decimal point are zero, so 123 with P = 1 becomes 120. Note that numbers
with zeros before the decimal point may have different precisions, because 1200
can have P = 0, 1 or 2 (depending on what the initial value was). It could also
have p < 0, when the digits after the decimal point are zero.

The string output (of floating point numbers) is padded with zeros:

    Initial value    P      A       Result          String
    ------------------------------------------------------------
    1234.01         -3              1000            1000
    1234            -2              1200            1200
    1234.5          -1              1230            1230
    1234.001         1              1234            1234.0
    1234.01          0              1234            1234
    1234.01          2              1234.01         1234.01
    1234.01          5              1234.01         1234.01000

For Math::BigInt objects, no padding occurs.

=head2 Rounding mode R

When rounding a number, different 'styles' or 'kinds' of rounding are possible.
(Note that random rounding, as in Math::Round, is not implemented.)

=head3 Directed rounding

These round modes always round in the same direction.

=over

=item 'trunc'

Round towards zero. Remove all digits following the rounding place, i.e.,
replace them with zeros. Thus, 987.65 rounded to tens (P=1) becomes 980, and
rounded to the fourth significant digit becomes 987.6 (A=4). 123.456 rounded to
the second place after the decimal point (P=-2) becomes 123.46. This
corresponds to the IEEE 754 rounding mode 'roundTowardZero'.

=back

=head3 Rounding to nearest

These rounding modes round to the nearest digit. They differ in how they
determine which way to round in the ambiguous case when there is a tie.

=over

=item 'even'

Round towards the nearest even digit, e.g., when rounding to nearest integer,
-5.5 becomes -6, 4.5 becomes 4, but 4.501 becomes 5. This corresponds to the
IEEE 754 rounding mode 'roundTiesToEven'.

=item 'odd'

Round towards the nearest odd digit, e.g., when rounding to nearest integer,
4.5 becomes 5, -5.5 becomes -5, but 5.501 becomes 6. This corresponds to the
IEEE 754 rounding mode 'roundTiesToOdd'.

=item '+inf'

Round towards plus infinity, i.e., always round up. E.g., when rounding to the
nearest integer, 4.5 becomes 5, -5.5 becomes -5, and 4.501 also becomes 5. This
corresponds to the IEEE 754 rounding mode 'roundTiesToPositive'.

=item '-inf'

Round towards minus infinity, i.e., always round down. E.g., when rounding to
the nearest integer, 4.5 becomes 4, -5.5 becomes -6, but 4.501 becomes 5. This
corresponds to the IEEE 754 rounding mode 'roundTiesToNegative'.

=item 'zero'

Round towards zero, i.e., round positive numbers down and negative numbers up.
E.g., when rounding to the nearest integer, 4.5 becomes 4, -5.5 becomes -5, but
4.501 becomes 5. This corresponds to the IEEE 754 rounding mode
'roundTiesToZero'.

=item 'common'

Round away from zero, i.e., round to the number with the largest absolute
value. E.g., when rounding to the nearest integer, -1.5 becomes -2, 1.5 becomes
2 and 1.49 becomes 1. This corresponds to the IEEE 754 rounding mode
'roundTiesToAway'.

=back

=head2 Fallback F

When neither A nor P are defined, the fallback accuracy is used when computing
values that would potentially give an infinite number of digits, e.g.,
division, roots, logarithms, trigonometric functions etc.

=head2 More details on rounding

The handling of A & P in MBI/MBF (the old core code shipped with Perl versions
<= 5.7.2) is like this:

=over

=item Precision

  * bfround($p) is able to round to $p number of digits after the decimal
    point
  * otherwise P is unused

=item Accuracy (significant digits)

  * bround($a) rounds to $a significant digits
  * only bdiv() and bsqrt() take A as (optional) parameter
    + other operations simply create the same number (bneg etc), or
      more (bmul) of digits
    + rounding/truncating is only done when explicitly calling one
      of bround or bfround, and never for Math::BigInt (not implemented)
  * bsqrt() simply hands its accuracy argument over to bdiv.
  * the documentation and the comment in the code indicate two
    different ways on how bdiv() determines the maximum number
    of digits it should calculate, and the actual code does yet
    another thing
    POD:
      max($Math::BigFloat::div_scale,length(dividend)+length(divisor))
    Comment:
      result has at most max(scale, length(dividend), length(divisor)) digits
    Actual code:
      scale = max(scale, length(dividend)-1,length(divisor)-1);
      scale += length(divisor) - length(dividend);
    So for lx = 3, ly = 9, scale = 10, scale will actually be 16 (10
    So for lx = 3, ly = 9, scale = 10, scale will actually be 16
    (10+9-3). Actually, the 'difference' added to the scale is cal-
    culated from the number of "significant digits" in dividend and
    divisor, which is derived by looking at the length of the man-
    tissa. Which is wrong, since it includes the + sign (oops) and
    actually gets 2 for '+100' and 4 for '+101'. Oops again. Thus
    124/3 with div_scale=1 will get you '41.3' based on the strange
    assumption that 124 has 3 significant digits, while 120/7 will
    get you '17', not '17.1' since 120 is thought to have 2 signif-
    icant digits. The rounding after the division then uses the
    remainder and $y to determine whether it must round up or down.
 ?  I have no idea which is the right way. That's why I used a slightly more
 ?  simple scheme and tweaked the few failing testcases to match it.

=back

This is how it works now:

=over

=item Setting/Accessing

  * You can set the A global via Math::BigInt->accuracy() or
    Math::BigFloat->accuracy() or whatever class you are using.
  * You can also set P globally by using Math::SomeClass->precision()
    likewise.
  * Globals are classwide, and not inherited by subclasses.
  * to undefine A, use Math::SomeClass->accuracy(undef);
  * to undefine P, use Math::SomeClass->precision(undef);
  * Setting Math::SomeClass->accuracy() clears automatically
    Math::SomeClass->precision(), and vice versa.
  * To be valid, A must be > 0, P can have any value.
  * If P is negative, this means round to the P'th place to the right of the
    decimal point; positive values mean to the left of the decimal point.
    P of 0 means round to integer.
  * to find out the current global A, use Math::SomeClass->accuracy()
  * to find out the current global P, use Math::SomeClass->precision()
  * use $x->accuracy() respective $x->precision() for the local
    setting of $x.
  * Please note that $x->accuracy() respective $x->precision()
    return eventually defined global A or P, when $x's A or P is not
    set.

=item Creating numbers

  * When you create a number, you can give the desired A or P via:
    $x = Math::BigInt->new($number,$A,$P);
  * Only one of A or P can be defined, otherwise the result is NaN
  * If no A or P is give ($x = Math::BigInt->new($number) form), then the
    globals (if set) will be used. Thus changing the global defaults later on
    will not change the A or P of previously created numbers (i.e., A and P of
    $x will be what was in effect when $x was created)
  * If given undef for A and P, NO rounding will occur, and the globals will
    NOT be used. This is used by subclasses to create numbers without
    suffering rounding in the parent. Thus a subclass is able to have its own
    globals enforced upon creation of a number by using
    $x = Math::BigInt->new($number,undef,undef):

        use Math::BigInt::SomeSubclass;
        use Math::BigInt;

        Math::BigInt->accuracy(2);
        Math::BigInt::SomeSubclass->accuracy(3);
        $x = Math::BigInt::SomeSubclass->new(1234);

    $x is now 1230, and not 1200. A subclass might choose to implement
    this otherwise, e.g. falling back to the parent's A and P.

=item Usage

  * If A or P are enabled/defined, they are used to round the result of each
    operation according to the rules below
  * Negative P is ignored in Math::BigInt, since Math::BigInt objects never
    have digits after the decimal point
  * Math::BigFloat uses Math::BigInt internally, but setting A or P inside
    Math::BigInt as globals does not tamper with the parts of a Math::BigFloat.
    A flag is used to mark all Math::BigFloat numbers as 'never round'.

=item Precedence

  * It only makes sense that a number has only one of A or P at a time.
    If you set either A or P on one object, or globally, the other one will
    be automatically cleared.
  * If two objects are involved in an operation, and one of them has A in
    effect, and the other P, this results in an error (NaN).
  * A takes precedence over P (Hint: A comes before P).
    If neither of them is defined, nothing is used, i.e. the result will have
    as many digits as it can (with an exception for bdiv/bsqrt) and will not
    be rounded.
  * There is another setting for bdiv() (and thus for bsqrt()). If neither of
    A or P is defined, bdiv() will use a fallback (F) of $div_scale digits.
    If either the dividend's or the divisor's mantissa has more digits than
    the value of F, the higher value will be used instead of F.
    This is to limit the digits (A) of the result (just consider what would
    happen with unlimited A and P in the case of 1/3 :-)
  * bdiv will calculate (at least) 4 more digits than required (determined by
    A, P or F), and, if F is not used, round the result
    (this will still fail in the case of a result like 0.12345000000001 with A
    or P of 5, but this can not be helped - or can it?)
  * Thus you can have the math done by on Math::Big* class in two modi:
    + never round (this is the default):
      This is done by setting A and P to undef. No math operation
      will round the result, with bdiv() and bsqrt() as exceptions to guard
      against overflows. You must explicitly call bround(), bfround() or
      round() (the latter with parameters).
      Note: Once you have rounded a number, the settings will 'stick' on it
      and 'infect' all other numbers engaged in math operations with it, since
      local settings have the highest precedence. So, to get SaferRound[tm],
      use a copy() before rounding like this:

        $x = Math::BigFloat->new(12.34);
        $y = Math::BigFloat->new(98.76);
        $z = $x * $y;                           # 1218.6984
        print $x->copy()->bround(3);            # 12.3 (but A is now 3!)
        $z = $x * $y;                           # still 1218.6984, without
                                                # copy would have been 1210!

    + round after each op:
      After each single operation (except for testing like is_zero()), the
      method round() is called and the result is rounded appropriately. By
      setting proper values for A and P, you can have all-the-same-A or
      all-the-same-P modes. For example, Math::Currency might set A to undef,
      and P to -2, globally.

 ?Maybe an extra option that forbids local A & P settings would be in order,
 ?so that intermediate rounding does not 'poison' further math?

=item Overriding globals

  * you will be able to give A, P and R as an argument to all the calculation
    routines; the second parameter is A, the third one is P, and the fourth is
    R (shift right by one for binary operations like badd). P is used only if
    the first parameter (A) is undefined. These three parameters override the
    globals in the order detailed as follows, i.e. the first defined value
    wins:
    (local: per object, global: global default, parameter: argument to sub)
      + parameter A
      + parameter P
      + local A (if defined on both of the operands: smaller one is taken)
      + local P (if defined on both of the operands: bigger one is taken)
      + global A
      + global P
      + global F
  * bsqrt() will hand its arguments to bdiv(), as it used to, only now for two
    arguments (A and P) instead of one

=item Local settings

  * You can set A or P locally by using $x->accuracy() or
    $x->precision()
    and thus force different A and P for different objects/numbers.
  * Setting A or P this way immediately rounds $x to the new value.
  * $x->accuracy() clears $x->precision(), and vice versa.

=item Rounding

  * the rounding routines will use the respective global or local settings.
    bround() is for accuracy rounding, while bfround() is for precision
  * the two rounding functions take as the second parameter one of the
    following rounding modes (R):
    'even', 'odd', '+inf', '-inf', 'zero', 'trunc', 'common'
  * you can set/get the global R by using Math::SomeClass->round_mode()
    or by setting $Math::SomeClass::round_mode
  * after each operation, $result->round() is called, and the result may
    eventually be rounded (that is, if A or P were set either locally,
    globally or as parameter to the operation)
  * to manually round a number, call $x->round($A,$P,$round_mode);
    this will round the number by using the appropriate rounding function
    and then normalize it.
  * rounding modifies the local settings of the number:

        $x = Math::BigFloat->new(123.456);
        $x->accuracy(5);
        $x->bround(4);

    Here 4 takes precedence over 5, so 123.5 is the result and $x->accuracy()
    will be 4 from now on.

=item Default values

  * A: undef
  * P: undef
  * R: 'even'
  * F: 40

=item Remarks

  * The defaults are set up so that the new code gives the same results as
    the old code (except in a few cases on bdiv):
    + Both A and P are undefined and thus will not be used for rounding
      after each operation.
    + round() is thus a no-op, unless given extra parameters A and P

=back

=head1 INTERNALS

You should neither care about nor depend on the internal representation; it
might change without notice. Use B<ONLY> method calls like C<< $x->sign(); >>
instead relying on the internal representation.

=head2 Math Library

The mathematical computations are performed by a backend library. It is not
required to specify which backend library to use, but some backend libraries
are much faster than the default library.

=head3 The default library

The default library is L<Math::BigInt::Calc>, which is implemented in pure Perl
and hence does not require a compiler.

=head3 Specifying a library

The simple case

    use Math::BigInt;

is equivalent to saying

    use Math::BigInt try => 'Calc';

You can use a different backend library with, e.g.,

    use Math::BigInt try => 'GMP';

which attempts to load the L<Math::BigInt::GMP> library, and falls back to the
default library if the specified library can't be loaded.

Multiple libraries can be specified by separating them by a comma, e.g.,

    use Math::BigInt try => 'GMP,Pari';

If you request a specific set of libraries and do not allow fallback to the
default library, specify them using "only",

    use Math::BigInt only => 'GMP,Pari';

If you prefer a specific set of libraries, but want to see a warning if the
fallback library is used, specify them using "lib",

    use Math::BigInt lib => 'GMP,Pari';

The following first tries to find Math::BigInt::Foo, then Math::BigInt::Bar,
and if this also fails, reverts to Math::BigInt::Calc:

    use Math::BigInt try => 'Foo,Math::BigInt::Bar';

=head3 Which library to use?

B<Note>: General purpose packages should not be explicit about the library to
use; let the script author decide which is best.

L<Math::BigInt::GMP>, L<Math::BigInt::Pari>, and L<Math::BigInt::GMPz> are in
cases involving big numbers much faster than L<Math::BigInt::Calc>. However
these libraries are slower when dealing with very small numbers (less than
about 20 digits) and when converting very large numbers to decimal (for
instance for printing, rounding, calculating their length in decimal etc.).

So please select carefully what library you want to use.

Different low-level libraries use different formats to store the numbers, so
mixing them won't work. You should not depend on the number having a specific
internal format.

See the respective math library module documentation for further details.

=head3 Loading multiple libraries

The first library that is successfully loaded is the one that will be used. Any
further attempts at loading a different module will be ignored. This is to
avoid the situation where module A requires math library X, and module B
requires math library Y, causing modules A and B to be incompatible. For
example,

    use Math::BigInt;                   # loads default "Calc"
    use Math::BigFloat only => "GMP";   # ignores "GMP"

=head2 Sign

The sign is either '+', '-', 'NaN', '+inf' or '-inf'.

A sign of 'NaN' is used to represent values that are not numbers, e.g., the
result of 0/0. '+inf' and '-inf' represen positive and negative infinity,
respectively. For example you get '+inf' when dividing a positive number by 0,
and '-inf' when dividing any negative number by 0.

=head1 EXAMPLES

  use Math::BigInt;

  sub bigint { Math::BigInt->new(shift); }

  $x = Math::BigInt->bstr("1234")       # string "1234"
  $x = "$x";                            # same as bstr()
  $x = Math::BigInt->bneg("1234");      # Math::BigInt "-1234"
  $x = Math::BigInt->babs("-12345");    # Math::BigInt "12345"
  $x = Math::BigInt->bnorm("-0.00");    # Math::BigInt "0"
  $x = bigint(1) + bigint(2);           # Math::BigInt "3"
  $x = bigint(1) + "2";                 # ditto ("2" becomes a Math::BigInt)
  $x = bigint(1);                       # Math::BigInt "1"
  $x = $x + 5 / 2;                      # Math::BigInt "3"
  $x = $x ** 3;                         # Math::BigInt "27"
  $x *= 2;                              # Math::BigInt "54"
  $x = Math::BigInt->new(0);            # Math::BigInt "0"
  $x--;                                 # Math::BigInt "-1"
  $x = Math::BigInt->badd(4,5)          # Math::BigInt "9"
  print $x->bsstr();                    # 9e+0

Examples for rounding:

  use Math::BigFloat;
  use Test::More;

  $x = Math::BigFloat->new(123.4567);
  $y = Math::BigFloat->new(123.456789);
  Math::BigFloat->accuracy(4);          # no more A than 4

  is ($x->copy()->bround(),123.4);      # even rounding
  print $x->copy()->bround(),"\n";      # 123.4
  Math::BigFloat->round_mode('odd');    # round to odd
  print $x->copy()->bround(),"\n";      # 123.5
  Math::BigFloat->accuracy(5);          # no more A than 5
  Math::BigFloat->round_mode('odd');    # round to odd
  print $x->copy()->bround(),"\n";      # 123.46
  $y = $x->copy()->bround(4),"\n";      # A = 4: 123.4
  print "$y, ",$y->accuracy(),"\n";     # 123.4, 4

  Math::BigFloat->accuracy(undef);      # A not important now
  Math::BigFloat->precision(2);         # P important
  print $x->copy()->bnorm(),"\n";       # 123.46
  print $x->copy()->bround(),"\n";      # 123.46

Examples for converting:

  my $x = Math::BigInt->new('0b1'.'01' x 123);
  print "bin: ",$x->as_bin()," hex:",$x->as_hex()," dec: ",$x,"\n";

=head1 NUMERIC LITERALS

After C<use Math::BigInt ':constant'> all numeric literals in the given scope
are converted to C<Math::BigInt> objects. This conversion happens at compile
time. Every non-integer is convert to a NaN.

For example,

    perl -MMath::BigInt=:constant -le 'print 2**150'

prints the exact value of C<2**150>. Note that without conversion of constants
to objects the expression C<2**150> is calculated using Perl scalars, which
leads to an inaccurate result.

Please note that strings are not affected, so that

    use Math::BigInt qw/:constant/;

    $x = "1234567890123456789012345678901234567890"
            + "123456789123456789";

does give you what you expect. You need an explicit Math::BigInt->new() around
at least one of the operands. You should also quote large constants to prevent
loss of precision:

    use Math::BigInt;

    $x = Math::BigInt->new("1234567889123456789123456789123456789");

Without the quotes Perl first converts the large number to a floating point
constant at compile time, and then converts the result to a Math::BigInt object
at run time, which results in an inaccurate result.

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

=head1 PERFORMANCE

Using the form $x += $y; etc over $x = $x + $y is faster, since a copy of $x
must be made in the second case. For long numbers, the copy can eat up to 20%
of the work (in the case of addition/subtraction, less for
multiplication/division). If $y is very small compared to $x, the form $x += $y
is MUCH faster than $x = $x + $y since making the copy of $x takes more time
then the actual addition.

With a technique called copy-on-write, the cost of copying with overload could
be minimized or even completely avoided. A test implementation of COW did show
performance gains for overloaded math, but introduced a performance loss due to
a constant overhead for all other operations. So Math::BigInt does currently
not COW.

The rewritten version of this module (vs. v0.01) is slower on certain
operations, like L</new()>, L</bstr()> and L</numify()>. The reason are that it
does now more work and handles much more cases. The time spent in these
operations is usually gained in the other math operations so that code on the
average should get (much) faster. If they don't, please contact the author.

Some operations may be slower for small numbers, but are significantly faster
for big numbers. Other operations are now constant (O(1), like L</bneg()>,
L</babs()> etc), instead of O(N) and thus nearly always take much less time.
These optimizations were done on purpose.

If you find the Calc module to slow, try to install any of the replacement
modules and see if they help you.

=head2 Alternative math libraries

You can use an alternative library to drive Math::BigInt. See the section
L</Math Library> for more information.

For more benchmark results see L<http://bloodgate.com/perl/benchmarks.html>.

=head1 SUBCLASSING

=head2 Subclassing Math::BigInt

The basic design of Math::BigInt allows simple subclasses with very little
work, as long as a few simple rules are followed:

=over

=item *

The public API must remain consistent, i.e. if a sub-class is overloading
addition, the sub-class must use the same name, in this case badd(). The reason
for this is that Math::BigInt is optimized to call the object methods directly.

=item *

The private object hash keys like C<< $x->{sign} >> may not be changed, but
additional keys can be added, like C<< $x->{_custom} >>.

=item *

Accessor functions are available for all existing object hash keys and should
be used instead of directly accessing the internal hash keys. The reason for
this is that Math::BigInt itself has a pluggable interface which permits it to
support different storage methods.

=back

More complex sub-classes may have to replicate more of the logic internal of
Math::BigInt if they need to change more basic behaviors. A subclass that needs
to merely change the output only needs to overload L</bstr()>.

All other object methods and overloaded functions can be directly inherited
from the parent class.

At the very minimum, any subclass needs to provide its own L</new()> and can
store additional hash keys in the object. There are also some package globals
that must be defined, e.g.:

    # Globals
    our $accuracy = 2;          # round to 2 decimal places
    our $precision = undef;
    our $round_mode = 'even';
    our $div_scale = 40;

Additionally, you might want to provide the following two globals to allow
auto-upgrading and auto-downgrading:

    our $upgrade = undef;
    our $downgrade = undef;

This allows Math::BigInt to correctly retrieve package globals from the
subclass, like C<$SubClass::precision>. See C<t/Math/BigInt/Subclass.pm>,
C<t/Math/BigFloat/SubClass.pm>, or C<t/Math/BigRat/SubClass.pm> for subclass
examples.

Don't forget to

    use overload;

in your subclass to automatically inherit the overloading from the parent. If
you like, you can change part of the overloading, look at Math::String for an
example.

=head1 UPGRADING

When used like this:

    use Math::BigInt upgrade => 'Foo::Bar';

    use Math::BigInt;
    Math::BigInt -> upgrade('Foo::Bar');

any operation whose result cannot be represented as an Math::BigInt object is
upgraded to the class Foo::Bar. Usually this is used in conjunction with
Math::BigRat or Math::BigFloat:

    use Math::BigInt upgrade => 'Math::BigFloat';

For example, the following returns 3 as a Math::BigInt when no upgrading is
defined, and 3.125 as a Math::BigFloat if Math::BigInt is set to upgrade to
Math::BigFloat:

    $x = Math::BigInt -> new(25) -> bdiv(8);

As a shortcut, you can use the module L<bignum>:

    use bignum;

which is also good for one-liners:

    perl -Mbignum -le 'print 2 ** 255'

This makes it possible to mix arguments of different classes (as in 2.5 + 2) as
well as preserve accuracy (as in sqrt(3)).

=head2 Auto-upgrade

The following methods upgrade themselves unconditionally; that is if upgrade is
in effect, they always hands up their work:

    bdiv bfdiv btdiv bsqrt blog bexp bpi bsin bcos batan batan2

All other methods upgrade themselves only when one (or all) of their arguments
are of the class mentioned in $upgrade.

=head1 EXPORTS

C<Math::BigInt> exports nothing by default, but can export the following
methods:

    bgcd
    blcm

=head1 CAVEATS

Some things might not work as you expect them. Below is documented what is
known to be troublesome:

=over

=item Comparing numbers as strings

Both L</bstr()> and L</bsstr()> as well as stringify via overload drop the
leading '+'. This is to be consistent with Perl and to make C<cmp> (especially
with overloading) to work as you expect. It also solves problems with
C<Test.pm> and L<Test::More>, which stringify arguments before comparing them.

Mark Biggar said, when asked about to drop the '+' altogether, or make only
C<cmp> work:

    I agree (with the first alternative), don't add the '+' on positive
    numbers.  It's not as important anymore with the new internal form
    for numbers.  It made doing things like abs and neg easier, but
    those have to be done differently now anyway.

So, the following examples now works as expected:

    use Test::More tests => 1;
    use Math::BigInt;

    my $x = Math::BigInt -> new(3*3);
    my $y = Math::BigInt -> new(3*3);

    is($x,3*3, 'multiplication');
    print "$x eq 9" if $x eq $y;
    print "$x eq 9" if $x eq '9';
    print "$x eq 9" if $x eq 3*3;

Additionally, the following still works:

    print "$x == 9" if $x == $y;
    print "$x == 9" if $x == 9;
    print "$x == 9" if $x == 3*3;

There is now a L</bsstr()> method to get the string in scientific notation aka
C<1e+2> instead of C<100>. Be advised that overloaded 'eq' always uses bstr()
for comparison, but Perl represents some numbers as 100 and others as 1e+308.
If in doubt, convert both arguments to Math::BigInt before comparing them as
strings:

    use Test::More tests => 3;
    use Math::BigInt;

    $x = Math::BigInt->new('1e56');
    $y = 1e56;
    is($x,$y);                     # fails
    is($x->bsstr(), $y);           # okay
    $y = Math::BigInt->new($y);
    is($x, $y);                    # okay

Alternatively, simply use C<< <=> >> for comparisons, this always gets it
right. There is not yet a way to get a number automatically represented as a
string that matches exactly the way Perl represents it.

=item oct()/hex()

These perl routines currently (as of Perl v.5.8.6) cannot handle passed inf.

    te@linux:~> perl -wle 'print 2 ** 3333'
    Inf
    te@linux:~> perl -wle 'print 2 ** 3333 == 2 ** 3333'
    1
    te@linux:~> perl -wle 'print oct(2 ** 3333)'
    0
    te@linux:~> perl -wle 'print hex(2 ** 3333)'
    Illegal hexadecimal digit 'I' ignored at -e line 1.
    0

The same problems occur if you pass them Math::BigInt->binf() objects. Since
overloading these routines is not possible, this cannot be fixed from
Math::BigInt.

=item int()

L</int()> returns (at least for Perl v5.7.1 and up) another Math::BigInt, not a
Perl scalar:

    $x = Math::BigInt->new(123);
    $y = int($x);                           # 123 as a Math::BigInt
    $x = Math::BigFloat->new(123.45);
    $y = int($x);                           # 123 as a Math::BigFloat

If you want a real Perl scalar, use L</numify()>:

    $y = $x->numify();                      # 123 as a scalar

This is seldom necessary, though, because this is done automatically, like when
you access an array:

    $z = $array[$x];                        # does work automatically

=item Modifying and =

Beware of:

    $x = Math::BigFloat->new(5);
    $y = $x;

This makes a second reference to the B<same> object and stores it in $y. Thus
anything that modifies $x (except overloaded operators) also modifies $y, and
vice versa. Or in other words, C<=> is only safe if you modify your
Math::BigInt objects only via overloaded math. As soon as you use a method call
it breaks:

    $x->bmul(2);
    print "$x, $y\n";       # prints '10, 10'

If you want a true copy of $x, use:

    $y = $x->copy();

You can also chain the calls like this, this first makes a copy and then
multiply it by 2:

    $y = $x->copy()->bmul(2);

See also the documentation for overload.pm regarding C<=>.

=item Overloading -$x

The following:

    $x = -$x;

is slower than

    $x->bneg();

since overload calls C<sub($x,0,1);> instead of C<neg($x)>. The first variant
needs to preserve $x since it does not know that it later gets overwritten.
This makes a copy of $x and takes O(N), but $x->bneg() is O(1).

=item Mixing different object types

With overloaded operators, it is the first (dominating) operand that determines
which method is called. Here are some examples showing what actually gets
called in various cases.

    use Math::BigInt;
    use Math::BigFloat;

    $mbf  = Math::BigFloat->new(5);
    $mbi2 = Math::BigInt->new(5);
    $mbi  = Math::BigInt->new(2);
                                    # what actually gets called:
    $float = $mbf + $mbi;           # $mbf->badd($mbi)
    $float = $mbf / $mbi;           # $mbf->bdiv($mbi)
    $integer = $mbi + $mbf;         # $mbi->badd($mbf)
    $integer = $mbi2 / $mbi;        # $mbi2->bdiv($mbi)
    $integer = $mbi2 / $mbf;        # $mbi2->bdiv($mbf)

For instance, Math::BigInt->bdiv() always returns a Math::BigInt, regardless of
whether the second operant is a Math::BigFloat. To get a Math::BigFloat you
either need to call the operation manually, make sure each operand already is a
Math::BigFloat, or cast to that type via Math::BigFloat->new():

    $float = Math::BigFloat->new($mbi2) / $mbi;     # = 2.5

Beware of casting the entire expression, as this would cast the
result, at which point it is too late:

    $float = Math::BigFloat->new($mbi2 / $mbi);     # = 2

Beware also of the order of more complicated expressions like:

    $integer = ($mbi2 + $mbi) / $mbf;               # int / float => int
    $integer = $mbi2 / Math::BigFloat->new($mbi);   # ditto

If in doubt, break the expression into simpler terms, or cast all operands
to the desired resulting type.

Scalar values are a bit different, since:

    $float = 2 + $mbf;
    $float = $mbf + 2;

both result in the proper type due to the way the overloaded math works.

This section also applies to other overloaded math packages, like Math::String.

One solution to you problem might be autoupgrading|upgrading. See the
pragmas L<bignum>, L<bigint> and L<bigrat> for an easy way to do this.

=back

=head1 BUGS

Please report any bugs or feature requests to
C<bug-math-bigint at rt.cpan.org>, or through the web interface at
L<https://rt.cpan.org/Ticket/Create.html?Queue=Math-BigInt> (requires login).
We will be notified, and then you'll automatically be notified of progress on
your bug as I make changes.

=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc Math::BigInt

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

L<Math::BigFloat> and L<Math::BigRat> as well as the backend libraries
L<Math::BigInt::FastCalc>, L<Math::BigInt::GMP>, and L<Math::BigInt::Pari>,
L<Math::BigInt::GMPz>, and L<Math::BigInt::BitVect>.

The pragmas L<bigint>, L<bigfloat>, and L<bigrat> might also be of interest. In
addition there is the L<bignum> pragma which does upgrading and downgrading.

=head1 AUTHORS

=over 4

=item *

Mark Biggar, overloaded interface by Ilya Zakharevich, 1996-2001.

=item *

Completely rewritten by Tels L<http://bloodgate.com>, 2001-2008.

=item *

Florian Ragwitz E<lt>flora@cpan.orgE<gt>, 2010.

=item *

Peter John Acklam E<lt>pjacklam@gmail.comE<gt>, 2011-.

=back

Many people contributed in one or more ways to the final beast, see the file
CREDITS for an (incomplete) list. If you miss your name, please drop me a
mail. Thank you!

=cut
