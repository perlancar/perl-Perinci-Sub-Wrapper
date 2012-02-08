package Perinci::Sub::Wrapper;

use 5.010;
use strict;
use warnings;
use Log::Any '$log';

use Data::Dump::OneLine qw(dump1);
use Scalar::Util qw(blessed refaddr);

require Exporter;
our @ISA = qw(Exporter);
our @EXPORT_OK = qw(wrap_sub);

# VERSION

our %SPEC;

sub new {
    my ($class) = @_;
    bless {}, $class;
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

sub handlemeta_args_as { {prio=>1} }
sub handle_args_as {
    my ($self, %args) = @_;

    my $meta  = $args{meta};
    my $value = $args{value};
    my $new   = $args{new};

    state $args_token_sub = sub {
        my ($args_as) = @_;
        if ($args_as eq 'hash') {
            return '%args';
        } elsif ($args_as eq 'hashref') {
            return '$args';
        } elsif ($args_as eq 'array') {
            return '@args';
        } elsif ($args_as eq 'arrayref') {
            return '$args';
        } else {
            die "BUG: Unknown value of args_as: $args_as";
        }
    };

    if ($new) { $self->add_line("# convert args from $value to $new") }
    my $args_line;
    my $p; # XXX
    if ($value eq 'hash') {
        $args_line = 'my %args = @_;';
    } elsif ($value eq 'hashref') {
        $args_line = 'my $args = {@_};';
    } elsif ($value =~ /\A(arrayref|array)\z/) {
        # temp, eventually will use codegen_convert_args_to_array()
        $self->add_line(
            '    require Perinci::Sub::ConvertArgs::Array;',
            '    my $ares = Perinci::Sub::ConvertArgs::Array::'.
                'convert_args_to_array(args=>{@_}, spec=>'.
                    dump1($meta->{args}).');',
            '    return $ares if $ares->[0] != 200;',
        );
        if ($value eq 'array') {
            $args_line = 'my @args = @{$ares->[2]};';
        } else {
            $args_line = 'my $args = $ares->[2];';
        }
    } else {
        die "Invalid args_as: $value, must be hash/hashref/".
            "array/arrayref";
    }
    $self->add_line("    $args_line");
}

sub handlemeta_result_naked { {prio=>90} }
sub handle_result_naked {
    my ($self, %args) = @_;
    #return if !!$args{old} eq !!$args{new};
}

sub add_line {
    my ($self, @lines) = @_;
    $self->{code} //= [];
    push @{$self->{code}}, @lines;
}

sub wrap {
    my ($self, %args) = @_;
    $log->tracef("-> wrap(%s)", \%args);

    my $sub      = $args{sub} or return [400, "Please specify sub"];
    $args{meta} or return [400, "Please specify meta"];
    my $meta     = { %{$args{meta}} };
    $args{convert} //= {};
    my $convert  = $args{convert};
    my $force    = $args{force};

    my $comppkg  = "Perinci::Sub::Wrapped";

    return [304, "Already wrapped"] if blessed($sub) && !$force;

    # put the sub in a named variable, so it can be accessed by the wrapper
    # code.
    my $subname = $comppkg . "::sub".refaddr($sub);
    { no strict 'refs'; ${$subname} = $sub; }

    $self->{args} = \%args;
    $self->add_line(
        "package $comppkg;",
        'sub {',
    );

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
        $ha->{new} = $convert->{$k0} if exists $convert->{$k0};
        $handler_args{$k} = $ha;
    }

    for my $k (sort {$handler_args{$a}{prio} <=> $handler_args{$b}{prio}}
                   keys %handler_args) {
        my $ha = $handler_args{$k};
        my $meth = $ha->{meth};
        $log->tracef("Calling %s(%s) ...", $meth, $ha);
        $self->$meth(args=>\%args, meta=>$meta, %$ha);
    }

=begin comment

    $self->add_line(
        '    my $res;',
    );

    $self->call_handlers("before_eval", $spec);
    $self->add_line('eval {');
    $self->call_handlers("before_call", $spec);
    $self->add_line('$res = $'.$subname."->($args_var);");
    $self->add_line('};');
    $self->add_line(
        '    my $eval_err = $@;',
        '    if ($eval_err) { return [500, "Sub died: $eval_err"] }',
    );
    $self->call_handlers("after_eval", $spec);
    $self->add_line(
        '    $res;',
        '}',
    );

    my $source = join "\n", @{$self->{code}};
    $log->tracef("wrapper source code: %s", $wrapped);
    my $wrapped = eval $source;
    die "BUG: Wrapper code can't be compiled: $@" if $@;


=end comment

=cut

    $log->tracef("<- wrap()");
    #[200, "OK", {sub=>$wrapped, source=>$code, meta=>$meta}];

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
    },
};
sub wrap_sub {
    __PACKAGE__->new->wrap(@_);
}

1;
# ABSTRACT: A multi-purpose subroutine wrapping framework

=for Pod::Coverage ^(new|handle(meta)?_.+|convert(meta)?_.+|wrap|add_.+)$

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
