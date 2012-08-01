package Test::Perinci::Sub::Wrapper;

use 5.010;
use strict;
use warnings;

use Perinci::Sub::Wrapper qw(wrap_sub);
use Test::More 0.96;

our @ISA = qw(Exporter);
our @EXPORT_OK = qw(test_wrap);

# VERSION

sub test_wrap {
    my %test_args = @_;
    my $wrap_args = $test_args{wrap_args} or die "BUG: wrap_args not defined";
    my $test_name = $test_args{name} or die "BUG: test name not defined";

    subtest $test_name => sub {

        my $wrap_res;
        eval { $wrap_res = wrap_sub(%$wrap_args) };
        my $wrap_eval_err = $@;
        if ($test_args{wrap_dies}) {
            ok($wrap_eval_err, "wrap dies");
            return;
        } else {
            ok(!$wrap_eval_err, "wrap doesn't die") or diag $wrap_eval_err;
        }

        if (defined $test_args{wrap_status}) {
            is(ref($wrap_res), 'ARRAY', 'wrap res is array');
            is($wrap_res->[0], $test_args{wrap_status},
               "wrap status is $test_args{wrap_status}")
                or diag "wrap res: ", explain($wrap_res);
        }

        return unless $wrap_res->[0] == 200;

        my $call_argsr = $test_args{call_argsr};
        my $call_res;
        if ($call_argsr) {
            my $sub = $wrap_res->[2]{sub};
            eval { $call_res = $sub->(@$call_argsr) };
            my $call_eval_err = $@;
            if ($test_args{call_dies}) {
                ok($call_eval_err, "call dies");
                if ($test_args{call_die_message}) {
                    like($call_eval_err, $test_args{call_die_message},
                         "call die message");
                }
                return;
            } else {
                ok(!$call_eval_err, "call doesn't die")
                    or diag $call_eval_err;
            }

            if (defined $test_args{call_status}) {
                is(ref($call_res), 'ARRAY', 'call res is array');
                is($call_res->[0], $test_args{call_status},
                   "call status is $test_args{call_status}");
            }

            if (exists $test_args{call_res}) {
                is_deeply($call_res, $test_args{call_res},
                          "call res")
                    or diag explain $call_res;
            }
        }

        if ($test_args{posttest}) {
            $test_args{posttest}->($wrap_res, $call_res);
        }

        done_testing();
    };
}

1;
# ABSTRACT: Provide test_wrap() to test wrapper
