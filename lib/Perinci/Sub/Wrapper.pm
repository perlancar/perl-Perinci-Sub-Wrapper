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

sub _args_token {
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
}

$SPEC{wrap_sub} = {
    v => 1.1,
    summary => 'Wrap subroutine to implement Rinci properties and features',
    description => <<'_',

Will wrap subroutine with codes that implement Rinci metadata properties, like
'timeout' (using Perl's eval block), 'args' (using Sah schema), etc. Will bless
subroutine (into 'Perinci::Sub::Wrapped') to mark that the subroutine has been
wrapped.

Will not wrap again if input subroutine has already been wrapped (blessed),
unless 'force' argument is set to true.

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
        schema=>['hash*'=>[keys=>{sub=>'code*', meta=>'hash*'}]],
    },
    result_naked => 1,
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
        target => {
            schema => 'hash*',
            summary => 'New properties to target',
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
    my %args   = @_;
    my $sub    = $args{sub}   or die "Please specify sub";
    my $meta   = $args{spec}  or die "Please specify meta";
    my $target = $args{target} // {};
    my $force  = $args{force};
    my $new    = { %$meta };

    my $comppkg = "Perinci::Sub::Wrapper";

    {
        last if blessed($sub) && !$force;

        # put the sub in a named variable, so it can be accessed by the wrapper
        # code.
        my $subname = $comppkg . "::sub".refaddr($sub);
        { no strict 'refs'; ${$subname} = $sub; }

        my $w = __PACKAGE__->new(args=>\%args, spec=>$spec);
        $w->add_line(
            "package $comppkg;",
            'sub {',
        );

        my $orig_args_as = $meta->{args_as} // "hash";
        my $args_as      = $target->{args_as} // "hash";
        if ($orig_args_as ne $args_as) {
            $w->add_line("# convert args from $args_as to $orig_args_as");
            my $p = "$args_as-$orig_args_as";
            my $args_line;
            if ($p eq 'hash') {
                $args_line = 'my %args = @_;';
            } elsif ($args_as eq 'hashref') {
                $args_line = 'my $args = {@_};';
            } elsif ($args_as =~ /\A(arrayref|array)\z/) {
                # temp, eventually will use codegen_convert_args_to_array()
                $w->add_line(
                    '    require Perinci::Sub::ConvertArgs::Array;',
                    '    my $ares = Perinci::Sub::ConvertArgs::Array::'.
                        'convert_args_to_array(args=>{@_}, spec=>'.
                            dump1($meta->{args}).');',
                    '    return $ares if $ares->[0] != 200;',
                );
                if ($args_as eq 'array') {
            $args_line = 'my @args = @{$ares->[2]};';
        } else {
            $args_line = 'my $args = $ares->[2];';
        }
    } elsif ($args_as eq 'object') {
        die "Sorry, args_as 'object' is not supported yet";
    } else {
        die "Invalid args_as: $args_as, must be hash/hashref/".
            "array/arrayref/object";
    }
    $wrapper->add_line("    $args_line");

    $wrapper->add_line(
        '    my $res;',
    );

    $wrapper->call_handlers("before_eval", $spec);
    $wrapper->add_line('eval {');
    $wrapper->call_handlers("before_call", $spec);
    $wrapper->add_line('$res = $'.$subname."->($args_var);");
    $wrapper->add_line('};');
    $wrapper->add_line(
        '    my $eval_err = $@;',
        '    if ($eval_err) { return [500, "Sub died: $eval_err"] }',
    );
    $wrapper->call_handlers("after_eval", $spec);
    $wrapper->add_line(
        '    $res;',
        '}',
    );
    if ($log->is_trace) {
        $log->trace("wrapper code: ".join("\n", @{$wrapper->{code}}));
    }
    $wrapper->compile;

    };

    {sub=>$compiled, meta=>$new};
}

# oo interface
sub new {
    my ($class, %args) = @_;
    bless \%args, $class;
}

sub meta { $_[0]->{meta} }

sub args { $_[0]->{meta}{args} }

sub call_handlers {
    my ($self, $phase_name, $meta) = @_;

    $log->tracef("call_handlers(phase=%s)", $phase_name);
    for my $prop (keys %$meta) {
        my $pn = "Perinci::Sub::Wrapper::property::$prop";
        my $pnp = $pn; $pnp =~ s!::!/!g; $pnp .= ".pm";
        eval { require $pnp };
        next if $@;
        #$log->trace("Package $pn exists");
        my $hn = "$pn\::$phase_name";
        next unless defined &{$hn};
        #$log->trace("Sub $hn exists");
        $log->tracef("Calling %s(%s)", $hn, $meta->{$prop});
        my $h = \&{$hn};
        $h->($self, $meta->{$prop});
    }
}

sub add_line {
    my ($self, @lines) = @_;
    $self->{code} //= [];
    push @{$self->{code}}, @lines;
}

sub compile {
    my ($self, @lines) = @_;
    my $res = eval join "\n", @{$self->{code}};
    die "BUG: Wrapper code can't be compiled: $@" if $@;
    $res;
}

1;
# ABSTRACT: Wrap subroutine to implement Rinci properties and add features

=head1 SYNOPSIS

 use Perinci::Sub::Wrapper qw(wrap_sub);
 my $sub = wrap_sub(sub => sub {die "test\n"}, spec=>{});
 my $res = $sub->(); # [500, "Function died: test"]


=head1 DESCRIPTION

This module provides wrap_sub().

This module uses L<Log::Any> for logging.


=head1 FUNCTIONS

None are exported, but they are exportable.


=head1 SEE ALSO

L<Perinci>

=cut
