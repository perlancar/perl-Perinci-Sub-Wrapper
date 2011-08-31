package Sub::Spec::Wrapper;

use 5.010;
use strict;
use warnings;
use Log::Any '$log';

use Scalar::Util qw(blessed refaddr);

require Exporter;
our @ISA = qw(Exporter);
our @EXPORT_OK = qw(wrap_sub);

# VERSION

our %SPEC;

$SPEC{wrap_sub} = {
    summary => 'Wrap subroutine to its implement Sub::Spec clauses',
    result => 'code',
    result_naked => 1,
    description => <<'_',

Will wrap subroutine with codes that implement Sub::Spec clauses, like ~timeout~
(using Perl's eval block), ~args~ (using Sah schema), etc. Will bless subroutine
(into ~Sub::Spec::Wrapped~) to mark that the subroutine has been wrapped.

Will not wrap again if input subroutine has already been wrapped (blessed),
unless ~force~ argument is set to true.

_
    required_args => [qw/sub spec/],
    args => {
        sub => ['code*' => {
            summary => 'The code to wrap',
        }],
        spec => ['hash*' => {
            summary => 'The sub spec',
        }],
        force => ['bool' => {
            summary => 'Whether to force wrap again even '.
                'when sub has been wrapped',
            default => 0,
        }],
    },
};
sub wrap_sub {
    my %args  = @_;
    my $sub   = $args{sub}   or die "Please specify sub";
    my $spec  = $args{spec}  or die "Please specify spec";
    my $force = $args{force};

    return $sub if blessed($sub) && !$force;

    # put the sub in a named variable, so it can be accessed by the wrapper
    # code.
    my $subname = "Sub::Spec::Wrapped::sub".refaddr($sub);
    {
        no strict 'refs';
        ${$subname} = $sub;
    }

    my $wrapper = __PACKAGE__->new(args=>\%args, spec=>$spec);
    $wrapper->add_line(
        'package Sub::Spec::Wrapped;',
        'sub {',
        '    my %args = @_;',
        '    my $res;',
    );

    $wrapper->call_handlers("before_eval", $spec);
    $wrapper->add_line('eval {');
    $wrapper->call_handlers("before_call", $spec);
    $wrapper->add_line('$res = $'.$subname.'->(%args);');
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
}

# oo interface
sub new {
    my ($class, %args) = @_;
    bless \%args, $class;
}

sub spec { $_[0]->{spec} }

sub args { $_[0]->{args} }

sub call_handlers {
    my ($self, $phase_name, $spec) = @_;

    $log->tracef("call_handlers(phase=%s)", $phase_name);
    for my $clause (keys %$spec) {
        my $pn = "Sub::Spec::Wrapper::Clause::$clause";
        my $pnp = $pn; $pnp =~ s!::!/!g; $pnp .= ".pm";
        eval { require $pnp };
        next if $@;
        #$log->trace("Package $pn exists");
        my $hn = "$pn\::$phase_name";
        next unless defined &{$hn};
        #$log->trace("Sub $hn exists");
        $log->tracef("Calling %s(%s)", $hn, $spec->{$clause});
        my $h = \&{$hn};
        $h->($self, $spec->{$clause});
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
# ABSTRACT: Wrap subroutine to its implement Sub::Spec clauses

=head1 SYNOPSIS

 use Sub::Spec::Wrapper qw(wrap_sub);
 my $sub = wrap_sub(sub => sub {die "test\n"}, spec=>{});
 my $res = $sub->(); # [500, "Sub died: test"]


=head1 DESCRIPTION

WARNING: PRELIMINARY VERSION, NOT EVERYTHING DESCRIBED IS IMPLEMENTED

This module provides wrap_sub() that implements/utilizes many spec clauses, like
C<args>, C<result>, C<timeout>, etc, via wrapping.

This module uses L<Log::Any> for logging.


=head1 FUNCTIONS

None are exported, but they are exportable.


=head1 SEE ALSO

L<Sub::Spec>

=cut
