package Perinci::Sub::Wrapper;

use 5.010;
use strict;
use warnings;
use Log::Any '$log';

use Data::Dump::OneLine qw(dump1);
use Data::Dumper;
use Scalar::Util qw(blessed refaddr);

require Exporter;
our @ISA = qw(Exporter);
our @EXPORT_OK = qw(wrap_sub);

# VERSION

our %SPEC;
our $INDENT = " " x 4;

sub new {
    my ($class) = @_;
    bless {}, $class;
}

sub __squote {
    my $res = Data::Dumper->new([shift])->
        Purity(1)->Terse(1)->Deepcopy(1)->Dump;
    chomp $res;
    $res;
}

sub _known_sections {
    state $v = {
        before_sub   => {order=>0, indent=>0},
        sub_top      => {order=>1, indent=>1},
        before_eval  => {order=>2, indent=>1},
        before_call  => {order=>3, indent=>2},
        call         => {order=>4, indent=>2},
        after_call   => {order=>5, indent=>2},
        after_eval   => {order=>6, indent=>1},
        sub_bottom   => {order=>7, indent=>1},
        after_sub    => {order=>8, indent=>0},
    };
    $v;
}

sub section_empty {
    my ($self, $section) = @_;
    !$self->{code}{$section};
}

sub _has_inside_eval {
    my ($self) = @_;
    !($self->section_empty('before_eval') &&
          $self->section_empty('after_eval'));
}

sub _check_known_section {
    my ($self, $section) = @_;
    my $ks = $self->_known_sections;
    $ks->{$section} or die "BUG: Unknown code section '$section'";
}

sub select_section {
    my ($self, $section) = @_;
    $self->_check_known_section($section);
    $self->{cur_section} = $section;
    $self;
}

sub indent {
    my ($self) = @_;
    my $section = $self->{cur_section};
    $self->{indent}{$section}++;
    $self;
}

sub unindent {
    my ($self) = @_;
    my $section = $self->{cur_section};
    if (--$self->{indent}{$section} < 0) {
        die "BUG: over-unindent for section '$section'!";
    }
    $self;
}

sub _push_or_unshift_lines {
    my ($self, $which, @lines) = @_;
    my $section = $self->{cur_section};

    unless ($self->{code}{$section}) {
        unshift @lines, "", "# * section: $section";
        $self->{code}{$section} = [];
        $self->{indent}{$section} = $self->_known_sections->{$section}{indent};
    }

    my $indent = $self->{indent}{$section};
    @lines = map {($INDENT x $indent) . $_} @lines;
    if ($which eq 'push') {
        push @{$self->{code}{$section}}, @lines;
    } else {
        unshift @{$self->{code}{$section}}, @lines;
    }
    $self;
}

sub push_lines {
    my ($self, @lines) = @_;
    $self->_push_or_unshift_lines("push", @lines);
}

# so far never actually used
sub unshift_lines {
    my ($self, @lines) = @_;
    $self->_push_or_unshift_lines("unshift", @lines);
}

sub _code_as_str {
    my ($self) = @_;
    my @lines;
    my $ss = $self->_known_sections;
    for my $s (sort {$ss->{$a}{order} <=> $ss->{$b}{order}} keys %$ss) {
        next if $self->section_empty($s);
        push @lines, @{$self->{code}{$s}};
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

# XXX not implemented yet
sub handlemeta_args { {} }
sub handle_args { {} }

# XXX not implemented yet
sub handlemeta_result { {} }
sub handle_result { {} }

# run before args
sub handlemeta_args_as { {prio=>1} }
sub handle_args_as {
    my ($self, %args) = @_;

    my $meta   = $args{meta};
    my $args_p = $meta->{args} // {};
    my $value  = $args{value};
    my $new    = $args{new};

    # args_token and arg_tokens are for argument validation later

    $self->select_section('sub_top');

    my $v = $new // $value;
    $self->push_lines("# accept args as $v");
    if ($v =~ /\Ahash(ref)?\z/) {
        my $ref = $v =~ /ref/;
        my $tok = $ref ? '$args' : '%args';
        $self->push_lines("my $tok = " . ($ref ? '{@_}' : '@_') . ';');
        $self->{args_token} = $tok;
        while(my ($a, $as) = each %$args_p) {
            $self->{arg_tokens}{$a} = '$args' . ($ref ? '->' : '') .
                '{'.__squote($a).'}';
        }
    } elsif ($v =~ /^\Aarray(ref)?\z/) {
        my $ref = $v =~ /ref/;
        my $tok = $ref ? '$args' : '@args';
        $self->push_lines("my $tok = " . ($ref ? '[@_]' : '@_') . ';');
        $self->{args_token} = $tok;
        while(my ($a, $as) = each %$args_p) {
            defined($as->{pos}) or die "Error in args property for arg '$a': ".
                "no pos defined";
            if ($as->{greedy}) {
                # XXX currently not a real token, not lvalue
                $as->{pos} += 0;
                my $av = '$args'.$as->{pos};
                $self->push_lines(
                    "my $av = [splice ".($ref ? '@$':'@').'args, '.
                        $as->{pos}.'];');
                $self->{arg_tokens}{$a} = $av;
            } else {
                $self->{arg_tokens}{$a} = '$args' . ($ref ? '->' : '') .
                    '[' . $as->{pos} . ']';
            }
        }
    } else {
        die "Unsupported args_as '$v'";
    }
}

sub handlemeta_result_naked { {prio=>90} }
sub handle_result_naked {
    my ($self, %args) = @_;
    return unless defined($args{new}) && !!$args{value} ne !!$args{new};

    $self->select_section('sub_bottom');
    if ($args{new}) {
        $self->push_lines(
            '# strip envelope (convert result_naked 0->1)',
            '$res = $res->[2];',
        );
    } else {
        $self->push_lines(
            '# envelope result (convert result_naked 1->0)',
            '$res = [200, "OK", $res];',
        );
    }
}

sub handlemeta_deps { {prio=>2} }
sub handle_deps {
    # XXX require+call Perinci::Sub::DepChecker
}

sub handlemeta_examples { {} }
sub handlemeta_features { {} }

sub wrap {
    my ($self, %args) = @_;
    $log->tracef("-> wrap(%s)", \%args);

    my $sub      = $args{sub} or return [400, "Please specify sub"];
    $args{meta} or return [400, "Please specify meta"];
    my $meta     = { %{$args{meta}} };
    $args{convert} //= {};
    my $convert  = $args{convert};
    my $force    = $args{force};
    my $trap     = $args{trap} // 1;

    my $comppkg  = "Perinci::Sub::Wrapped";

    return [304, "Already wrapped"] if blessed($sub) && !$force;

    my $v = $meta->{v} // 1.0;
    return [412, "Unsupported metadata version ($v), only 1.1 supported"]
        unless $v == 1.1;

    # put the sub in a named variable, so it can be accessed by the wrapper
    # code.
    my $subname = $comppkg . "::sub".refaddr($sub);
    { no strict 'refs'; ${$subname} = $sub; }

    $self->{args} = \%args;
    $self->select_section('before_sub');
    $self->push_lines(
        "package $comppkg;",
        'sub {');
    $self->select_section('sub_top');
    $self->push_lines(
        'my $res;');

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

    $self->select_section('call');
    $self->push_lines('$res = $'.$subname."->(".$self->{args_token}.");");

    if ($trap || $self->_has_inside_eval) {
        $self->select_section('after_eval');
        $self->push_lines(
            '};',
            'my $eval_err = $@;',
            'if ($eval_err) {');
        $self->indent;
        $self->push_lines(
            '$res = '.(
                $meta->{result_naked} ?
                    'undef' :
                        '[500, "Function died: $eval_err"]').';');
        $self->unindent;
        $self->push_lines(
            '}');
    }

    # return result
    $self->select_section('sub_bottom');
    $self->push_lines(
        '$res;');
    $self->select_section('after_sub');
    $self->push_lines(
        '}');

    if ($self->_has_inside_eval) {
        $self->select_section('before_eval');
        $self->push_lines(
            'eval {');
    }

    my $source = $self->_code_as_str;
    $log->tracef("wrapper source code: %s", $source);
    my $wrapped = eval $source;
    die "BUG: Wrapper code can't be compiled: $@" if $@;


    $log->tracef("<- wrap()");
    [200, "OK", {sub=>$wrapped, source=>$source, meta=>$meta}];
}

$SPEC{wrap_sub} = {
    v => 1.1,
    summary => 'Wrap subroutine to do various things, '.
        'like enforcing Rinci properties',
    description => <<'_',

Will wrap subroutine and bless the wrapped subroutine (by default into
'Perinci::Sub::Wrapped') as a way of marking that the subroutine has been
wrapped.

Will not wrap again (status 304) if input subroutine has already been wrapped
(blessed the above way), unless 'force' argument is set to true.

_
    result => {
        summary => 'The wrapped subroutine along with its new metadata',
        description => <<'_',

Aside from wrapping the subroutine, will also create a new metadata for it. The
new metadata is a shallow copy of the original, with most properties usually
untouched. Only certain properties will be changed to match the new subroutine
behavior. For example, if you set a different 'args_as' or 'result_naked' in
'opts', then the new metadata will carry the new values.

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
        force => {
            schema => 'bool',
            summary => 'Whether to force wrap again even '.
                'when sub has been wrapped',
            default => 0,
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
    },
};
sub wrap_sub {
    __PACKAGE__->new->wrap(@_);
}

1;
# ABSTRACT: A multi-purpose subroutine wrapping framework

=for Pod::Coverage ^(new|handle(meta)?_.+|convert(meta)?_.+|wrap|add_.+|section_empty|indent|unindent|select_section|(push|unshift)_lines)$

=head1 SYNOPSIS

 use Perinci::Sub::Wrapper qw(wrap_sub);
 my $res = wrap_sub(sub => sub {die "test\n"}, ...);
 my $wrapped = $res->[2]{wrapped};
 $wrapped->(); # call the wrapped function


=head1 DESCRIPTION

Perinci::Sub::Wrapper is an extensible subroutine wrapping framework. It works
by creating a single "large" wrapper function from a composite bits of code,
instead of using multiple small wrappers (a la Python's decorator). The
single-wrapper approach has the benefit of smaller function call overhead.

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
