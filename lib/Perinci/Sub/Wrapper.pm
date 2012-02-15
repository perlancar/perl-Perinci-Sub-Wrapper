package Perinci::Sub::Wrapper;

use 5.010;
use strict;
use warnings;
use Log::Any '$log';

require Exporter;
our @ISA = qw(Exporter);
our @EXPORT_OK = qw(wrap_sub);

# VERSION

our %SPEC;

sub new {
    my ($class, %args) = @_;
    $args{comppkg} //= "Perinci::Sub::Wrapped";
    $args{indent}  //= " " x 4;
    bless \%args, $class;
}

sub __squote {
    require Data::Dumper;
    my $res = Data::Dumper->new([shift])->
        Purity(1)->Terse(1)->Deepcopy(1)->Dump;
    chomp $res;
    $res;
}

sub _known_sections {
    state $v = {
        # reserved by wrapper for setting Perl package and declaring 'sub {'
        OPEN_SUB => {order=>0},

        # for handlers to put stuffs right before eval. for example, 'timeout'
        # uses this to set ALRM signal handler.
        before_eval => {order=>10},

        # reserved by wrapper for generating 'eval {'
        OPEN_EVAL => {order=>20},

        # for handlers to put various checks before calling the wrapped
        # function, from data validation, argument conversion, etc.
        before_call => {order=>30},

        # reserved by the wrapper for calling the function
        CALL => {order=>50},

        # for handlers to put various things after calling, from validating
        # result, enveloping result, etc.
        after_call => {order=>60},

        # reserved by wrapper to put eval end '}' and capturing $@ in $eval_err
        CLOSE_EVAL => {order=>70},

        # for handlers to put checks against $eval_err
        after_eval => {order=>80},

        # reserved for returning '$res' and the sub closing '}' line
        CLOSE_SUB => {order=>90},
    };
    $v;
}

sub section_empty {
    my ($self, $section) = @_;
    !$self->{_codes}{$section};
}

sub _needs_eval {
    my ($self) = @_;
    !($self->section_empty('before_eval') &&
          $self->section_empty('after_eval'));
}

sub _check_known_section {
    my ($self, $section) = @_;
    my $ks = $self->_known_sections;
    $ks->{$section} or die "BUG: Unknown code section '$section'";
}

sub _errif {
    my ($self, $c_status, $c_msg, $c_cond) = @_;
    $self->push_lines("if ($c_cond) {");
    $self->indent;
    $self->push_lines(
        # we set $res here when we return from inside the eval block
        '$res = ' . (
            $self->{_meta}{result_naked} ?
                'undef' : "[$c_status, $c_msg]") . ';',
        'return $res;');
    $self->unindent;
    $self->push_lines('}');
}

sub select_section {
    my ($self, $section) = @_;
    $self->_check_known_section($section);
    $self->{_cur_section} = $section;
    $self;
}

sub indent {
    my ($self) = @_;
    my $section = $self->{_cur_section};
    $self->{_codes}{$section} //= undef;
    $self->{_levels}{$section}++;
    $self;
}

sub unindent {
    my ($self) = @_;
    my $section = $self->{_cur_section};
    $self->{_codes}{$section} //= undef;
    $self->{_levels}{$section}--;
    $self;
}

sub push_lines {
    my ($self, @lines) = @_;
    my $section = $self->{_cur_section};

    unless (exists $self->{_codes}{$section}) {
        unshift @lines, "", "# * section: $section";
        $self->{_codes}{$section} = [];
        $self->{_levels}{$section} = 0;
    }

    @lines = map {[$self->{_levels}{$section}, $_]} @lines;
    push @{$self->{_codes}{$section}}, @lines;
    $self;
}

sub _code_as_str {
    my ($self) = @_;
    my @lines;
    my $ss = $self->_known_sections;
    my $prev_section_level = 0;
    my $i = 0;
    for my $s (sort {$ss->{$a}{order} <=> $ss->{$b}{order}}
                   keys %$ss) {
        next if $self->section_empty($s);
        $i++;
        for my $l (@{ $self->{_codes}{$s} }) {
            $l->[0] += $prev_section_level;
            die "BUG: Negative indent level in line $i: '$l'"
                if $l->[0] < 0;
            push @lines, ($self->{indent} x $l->[0]) . $l->[1];
        }
        $prev_section_level += $self->{_levels}{$s};
    }
    join "\n", @lines;
}

sub handlemeta_v { {} }
sub handlemeta_default_lang { {} }
sub handlemeta_name { {} }
sub handlemeta_summary { {} }
sub handlemeta_description { {} }
sub handlemeta_tags { {} }
sub handlemeta_links { {} }
sub handlemeta_text_markup { {} }
sub handlemeta_is_func { {} }
sub handlemeta_is_meth { {} }
sub handlemeta_is_class_meth { {} }
sub handlemeta_examples { {} }
sub handlemeta_features { {} }

# run before args
sub handlemeta_args_as { {prio=>1, convert=>1} }
sub handle_args_as {
    my ($self, %args) = @_;

    my $value  = $args{value};
    my $new    = $args{new};
    my $meta   = $self->{_meta};
    my $args_p = $meta->{args} // {};

    # We support conversion of arguments between hash/hashref/array/arrayref. To
    # make it simple, currently the algorithm is as follow: we first form the
    # %args hash. If args_as is already 'hash', we just do 'my %args = @_'.
    # Otherwise, we convert from the other forms.
    #
    # We then validate each argument in %args (code generated in 'args'
    # handler).
    #
    # Finally, unless original args_as is 'hash' we convert to the final form
    # that the wrapped function expects.
    #
    # This setup is optimal when both the function and generated wrapper accept
    # 'hash', but suboptimal for other cases (especially positional ones, as
    # they have to undergo a round-trip to hash even when both accept 'array').
    # This will be rectified in the future.

    $self->select_section('before_call');

    my $v = $new // $value;
    $self->push_lines('', "# accept arguments ($v)");
    if ($v eq 'hash') {
         $self->push_lines('my %args = @_;');
    } elsif ($v eq 'hashref') {
        $self->push_lines('my %args = %{$_[0]};');
    } elsif ($v =~ /\Aarray(ref)?\z/) {
        my $ref = $1 ? 1:0;
        $self->push_lines('my %args;');
        while (my ($a, $as) = each %$args_p) {
            my $line = '$args{'.__squote($a).'} = ';
            defined($as->{pos}) or die "Error in args property for arg '$a': ".
                "no pos defined";
            my $pos = int($as->{pos} + 0);
            $pos >= 0 or die "Error in args property for arg '$a': ".
                "negative value in pos";
            if ($as->{greedy}) {
                $line .= '[splice '.($ref ? '@{$_[0]}' : '@_').", $pos]";
            } else {
                $line .= $ref ? '$_[0]['.$pos.']' : '$_['.$pos.']';
            }
            $self->push_lines("$line;");
        }
    } else {
        die "Unknown args_as value '$v'";
    }

    my $tok;
    $self->push_lines('', "# convert arguments for wrapped function ($value)")
        unless $v eq $value;
    $v = $value;
    if ($v eq 'hash') {
        $tok = '%args';
    } elsif ($v eq 'hashref') {
        $tok = '\%args';
    } elsif ($v =~ /\Aarray(ref)?\z/) {
        my $ref = $1 ? 1:0;
        $self->push_lines('my @args;');
        for my $a (sort {$args_p->{$a}{pos} <=> $args_p->{$b}{pos}}
                       keys %$args_p) {
            my $as = $args_p->{$a};
            my $t = '$args{'.__squote($a).'}';
            my $line;
            defined($as->{pos}) or die "Error in args property for arg '$a': ".
                "no pos defined";
            my $pos = int($as->{pos} + 0);
            $pos >= 0 or die "Error in args property for arg '$a': ".
                "negative value in pos";
            if ($as->{greedy}) {
                $line = 'splice @args, '.$pos.', scalar(@args)-1, @{'.$t.'}';
            } else {
                $line = '$args['.$pos."] = $t";
            }
            $self->push_lines("$line;");
        }
        $tok = $ref ? '\@args' : '@args';
    } else {
        die "Unknown args_as value '$v'";
    }
    $self->{_args_token} = $tok;
}

# XXX not implemented yet
sub handlemeta_args { {} }
sub handle_args { {} }

# XXX not implemented yet
sub handlemeta_result { {} }
sub handle_result { {} }

sub handlemeta_result_naked { {prio=>90, convert=>1} }
sub handle_result_naked {
    my ($self, %args) = @_;

    # XXX option to check whether result is really naked

    if (defined($args{new}) && !!$args{value} ne !!$args{new}) {
        $self->select_section('after_call');
        if ($args{new}) {
            $self->push_lines(
                '', '# strip result envelope',
                '$res = $res->[2];',
            );
        } else {
            $self->push_lines(
                '', '# add result envelope',
                '$res = [200, "OK", $res];',
            );
        }
    }
}

sub handlemeta_deps { {prio=>0.5} }
sub handle_deps {
    my ($self, %args) = @_;
    my $value = $args{value};
    my $meta  = $self->{_meta};
    my $v     = $self->{_var_meta};
    $self->select_section('before_call');
    $self->push_lines('', '# check dependencies');
    $self->push_lines('require Perinci::Sub::DepChecker;');
    $self->push_lines('my $deps_res = Perinci::Sub::DepChecker::check_deps($'.
                          $v.'->{deps});');
    if ($self->{_args}{trap}) {
        $self->_errif(412, '"Deps failed: $deps_res"', '$deps_res');
    } else {
        $self->push_lines('die "Deps failed: $deps_res" if $deps_res;');
    }
}

sub wrap {
    require Scalar::Util;

    my ($self, %args) = @_;
    $log->tracef("-> wrap(%s)", \%args);

    my $sub      = $args{sub} or return [400, "Please specify sub"];
    $args{meta} or return [400, "Please specify meta"];
    my $meta     = { %{$args{meta}} };
    $args{convert} //= {};
    my $convert  = $args{convert};
    $args{trap} //= 1;
    my $trap     = $args{trap};
    $args{compile} //= 1;
    my $compile  = $args{compile};

    my $comppkg  = $self->{comppkg};

    # add properties from convert, if not yet mentioned in meta
    for (keys %$convert) {
        $meta->{$_} = undef unless exists $meta->{$_};
    }

    my $v = $meta->{v} // 1.0;
    return [412, "Unsupported metadata version ($v), only 1.1 supported"]
        unless $v == 1.1;

    # put the sub in a named variable, so it can be accessed by the wrapper
    # code.
    my $subname = $comppkg . "::sub".Scalar::Util::refaddr($sub);
    { no strict 'refs'; ${$subname} = $sub; }

    # also store the meta, it is needed by the wrapped sub. sometimes the meta
    # contains coderef and can't be dumped reliably, so we store it instead.
    my $metaname = $comppkg . "::meta".Scalar::Util::refaddr($meta);
    { no strict 'refs'; ${$metaname} = $meta; }
    $self->{_var_meta} = $metaname;

    # reset work variables. we haven't tested this yet because we expect the
    # object to be used only for one-off wrapping, via wrap_sub().
    $self->{_cur_section} = undef;
    $self->{_levels} = {};
    $self->{_codes} = {};

    $self->{_args} = \%args;
    $self->{_meta} = $meta;
    $self->select_section('OPEN_SUB');
    $self->push_lines(
        "package $comppkg;",
        'sub {');
    $self->indent;
    $self->push_lines(
        'my ($res, $eval_err);');

    # XXX validate metadata first to filter invalid properties, also to fill
    # default values. currently this is a quick/temp code.
    $meta->{args_as} //= "hash";
    $meta->{result_naked} //= 0;

    my %handler_args;
    for my $k0 (keys %$meta) {
        next if $k0 =~ /^_/;
        my $k = $k0;
        $k =~ s/\..+//;
        next if $handler_args{$k};
        my $meth = "handlemeta_$k";
        unless ($self->can($meth)) {
            return [500, "Can't handle wrapping property $k0 ($meth)"];
        }
        my $hm = $self->$meth;
        next unless defined $hm->{prio};
        my $ha = {
            prio=>$hm->{prio}, value=>$meta->{$k0}, property=>$k0,
            meth=>"handle_$k",
        };
        if (exists $convert->{$k0}) {
            return [502, "Property '$k0' does not support conversion"]
                unless $hm->{convert};
            $ha->{new}   = $convert->{$k0};
            $meta->{$k0} = $convert->{$k0};
        }
        $handler_args{$k} = $ha;
    }

    for my $k (sort {$handler_args{$a}{prio} <=> $handler_args{$b}{prio}}
                   keys %handler_args) {
        my $ha = $handler_args{$k};
        my $meth = $ha->{meth};
        $log->tracef("Calling %s(%s) ...", $meth, $ha);
        $self->$meth(args=>\%args, meta=>$meta, %$ha);
    }

    $self->select_section('CALL');
    $self->push_lines('$res = $'.$subname."->(".$self->{_args_token}.");");

    if ($trap || $self->_needs_eval) {
        $self->select_section('CLOSE_EVAL');
        $self->unindent;
        $self->push_lines(
            '',
            '};',
            '$eval_err = $@;');
        # _needs_eval will automatically be enabled here, due after_eval being
        # filled
        $self->select_section('after_eval');
        $self->_errif(500, '"Function died: $eval_err"', '$eval_err');
    }

    # return result
    $self->select_section('CLOSE_SUB');
    $self->push_lines('$res;');
    $self->unindent;
    $self->push_lines('}');

    # hm, indent + unindent doesn't work properly here?
    #$self->unindent;
    #$self->push_lines('}');

    if ($self->_needs_eval) {
        $self->select_section('OPEN_EVAL');
        $self->push_lines('eval {');
        $self->indent;
    }

    my $source = $self->_code_as_str;
    $log->tracef("wrapper source code: %s", $source);
    my $result = {source=>$source};
    if ($compile) {
        my $wrapped = eval $source;
        die "BUG: Wrapper code can't be compiled: $@" if $@;

        # mark the wrapper with bless, to detect double wrapping attempt
        bless $wrapped, $comppkg;

        $result->{sub}  = $wrapped;
        $result->{meta} = $meta;
    }
    $log->tracef("<- wrap()");
    [200, "OK", $result];
}

$SPEC{wrap_sub} = {
    v => 1.1,
    summary => 'Wrap subroutine to do various things, '.
        'like enforcing Rinci properties',
    description => <<'_',

Will wrap subroutine and bless the generated wrapped subroutine (by default into
'Perinci::Sub::Wrapped') as a way of marking that the subroutine is a wrapped
one.

_
    result => {
        summary => 'The wrapped subroutine along with its new metadata',
        description => <<'_',

Aside from wrapping the subroutine, will also create a new metadata for it. The
new metadata is a shallow copy of the original, with most properties usually
untouched. Only certain properties will be changed to match the new subroutine
behavior. For example, if you set a different 'args_as' or 'result_naked' in
'convert', then the new metadata will carry the new values.

_
        schema=>['hash*'=>[keys=>{
            sub=>'code*',
            source=>'str*',
            meta=>'hash*',
        }]],
    },
    args => {
        sub => {
            schema => 'code*',
            summary => 'The code to wrap',
            req => 1, pos => 0,
        },
        meta => {
            schema => 'hash*',
            summary => 'The function metadata',
            req =>1, pos => 1,
        },
        convert => {
            schema => 'hash*',
            summary => 'Properties to convert to new value',
            description => <<'_',

So far you can convert 'args_as' and 'result_naked'.

_
        },
        trap => {
            schema => 'bool',
            summary => 'Whether to trap exception using an eval block',
            description => <<'_',

If set to true, will wrap call using an eval {} block and return 500 /undef if
function dies. Note that if some other properties requires an eval block (like
'timeout') an eval block will be added regardless of this parameter.

_
            default => 1,
        },
        compile => {
            schema => 'bool',
            summary => 'Whether to compile the generated wrapper',
            description => <<'_',

Can be set to 0 to not actually wrap but just return the generated wrapper
source code.

_
            default => 1,
        },
    },
};
sub wrap_sub {
    __PACKAGE__->new->wrap(@_);
}

1;
# ABSTRACT: A multi-purpose subroutine wrapping framework

=for Pod::Coverage ^(new|handle(meta)?_.+|convert(meta)?_.+|wrap|add_.+|section_empty|indent|unindent|select_section|push_lines)$

=head1 SYNOPSIS

 use Perinci::Sub::Wrapper qw(wrap_sub);
 my $res = wrap_sub(sub => sub {die "test\n"}, meta=>{...});
 my ($wrapped, $meta) = ($res->[2]{sub}, $res->[2]{meta});
 $wrapped->(); # call the wrapped function


=head1 DESCRIPTION

Perinci::Sub::Wrapper is an extensible subroutine wrapping framework. It works
by creating a single "large" wrapper function from a composite bits of code,
instead of using multiple small wrappers (a la Python's decorator). The
single-wrapper approach has the benefit of smaller function call overhead. You
can still wrap multiple times if needed.

This module is used to enforce Rinci properties, e.g. C<args> (by performing
schema validation before calling the function), C<timeout> (by doing function
call inside an C<eval()> and using C<alarm()> to limit the execution), or
C<retry> (by wrapping function call inside a simple retry loop).

It can also be used to convert argument passing style, e.g. from C<args_as>
C<array> to C<args_as> C<hash>, so you can call function using named arguments
even though the function accepts positional arguments.

There are many other possible uses.

This module uses L<Log::Any> for logging.


=head1 EXTENDING

The framework is simple and extensible. Please delve directly into the source
code for now. Some notes:

The internal uses OO.

The main wrapper building mechanism is in the C<wrap()> method.

For each Rinci property, it will call C<handle_NAME()> wrapper handler method.
The C<handlemeta_NAME()> methods are called first, to determine order of
processing. You can supply these methods either by subclassing the class or,
more simply, monkeypatching the method in the C<Perinci::Sub::Wrapper> package.

The wrapper handler method will be called with a hash argument, containing these
keys: B<value> (property value), B<new> (this key will exist if C<convert>
argument of C<wrap()> exists, to convert a property to a new value).

For properties that have name in the form of C<NAME1.NAME2.NAME3> (i.e., dotted)
only the first part of the name will be used (i.e., C<handle_NAME1()>).


=head1 METHODS

The OO interface is only used internally or when you want to extend the wrapper.


=head1 FUNCTIONS

None are exported, but they are exportable.


=head1 SEE ALSO

L<Perinci>

=cut
