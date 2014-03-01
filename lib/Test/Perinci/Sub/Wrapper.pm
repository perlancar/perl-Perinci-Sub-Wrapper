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
    $test_args{wrap_args} or die "BUG: wrap_args not defined";
    my $test_name = $test_args{name} or die "BUG: test_name not defined";

    for my $wrapper_type (qw/dynamic embed/) {
        subtest "$test_name ($wrapper_type)" => sub {

            my $wrap_args = { %{ $test_args{wrap_args} } };
            die "BUG: embed must not be specified in wrap_args, test_wrap() ".
                "will always test dynamic (embed=0) *and* embed mode"
                    if exists $wrap_args->{embed};
            if ($wrapper_type eq 'embed') {
                $wrap_args->{embed} = 1;
            } else {
                $wrap_args->{embed} = 0;
            }

            my $wrap_res;
            eval { $wrap_res = wrap_sub(%$wrap_args) };
            my $wrap_eval_err = $@;
            if ($test_args{wrap_dies}) {
                ok($wrap_eval_err, "wrap dies");
                return;
            } else {
                ok(!$wrap_eval_err, "wrap doesn't die") or do {
                    diag $wrap_eval_err;
                    return;
                };
            }

            if (defined $test_args{wrap_status}) {
                is(ref($wrap_res), 'ARRAY', 'wrap res is array');
                is($wrap_res->[0], $test_args{wrap_status},
                   "wrap status is $test_args{wrap_status}")
                    or diag "wrap res: ", explain($wrap_res);
            }

            return unless $wrap_res->[0] == 200;

            my $sub;
            if ($wrapper_type eq 'embed') {
                my $src = $wrap_res->[2]{source};
                my $sub_name = $wrap_res->[2]{sub_name};
                my $eval_src = join(
                    "\n",
                    $src->{part1},
                    $src->{part2},
                    'sub {',
                    '    my %args = @_;',
                    $src->{part3},
                    ($src->{part4} ? '    $_w_res = do {' : ''),
                    $sub_name. ($sub_name =~ /\A\$/ ? '->':''). '(%args);',
                    ($src->{part4} ? '}; # do' : ''),
                    $src->{part4},
                    '}; # sub',
                );
                $sub = eval $eval_src;
                my $eval_err = $@;
                ok(!$eval_err, "embed code compiles ok") or do {
                    diag "eval err: ", $eval_err;
                    diag "eval source: ", $eval_src;
                    return;
                };
            } else {
                $sub = $wrap_res->[2]{sub};
            }

            # testing a single sub call
            my $call_argsr = $test_args{call_argsr};
            my $call_res;
            if ($call_argsr) {
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
                    is(ref($call_res), 'ARRAY', 'call res is array')
                        or diag "call res = ", explain($call_res);
                    is($call_res->[0], $test_args{call_status},
                       "call status is $test_args{call_status}")
                        or diag "call res = ", explain($call_res);
                }

                if (exists $test_args{call_res}) {
                    is_deeply($call_res, $test_args{call_res},
                              "call res")
                        or diag explain $call_res;
                }

                if (exists $test_args{call_actual_res_re}) {
                    like($call_res->[2], $test_args{call_actual_res_re},
                         "call actual res");
                }
            }

            # testing multiple sub calls
            if ($test_args{calls}) {
                my $i = 0;
                for my $call (@{$test_args{calls}}) {
                    $i++;
                    subtest "call #$i: ".($call->{name} // "") => sub {
                        my $res;
                        eval { $res = $sub->(@{$call->{argsr}}) };
                        my $eval_err = $@;
                        if ($call->{dies}) {
                            ok($eval_err, "dies");
                            if ($call->{die_message}) {
                                like($eval_err, $call->{die_message},
                                     "die message");
                            }
                            return;
                        } else {
                            ok(!$eval_err, "doesn't die")
                                or diag $eval_err;
                        }

                        if (defined $call->{status}) {
                            is(ref($res), 'ARRAY', 'res is array')
                                or diag "res = ", explain($res);
                            is($res->[0], $call->{status},
                               "status is $call->{status}")
                                or diag "res = ", explain($res);
                        }

                        if (exists $call->{res}) {
                            is_deeply($res, $call->{res}, "res")
                                or diag explain $res;
                        }

                        if (exists $call->{actual_res_re}) {
                            like($res->[2], $call->{actual_res_re},
                                 "actual res");
                        }
                    }; # subtest call #$i
                }
            } # if calls

            if ($test_args{posttest}) {
                $test_args{posttest}->($wrap_res, $call_res);
            }

            done_testing();
        }; # subtest
    } # for $wrapper_type
}

1;
# ABSTRACT: Provide test_wrap() to test wrapper

=for Pod::Coverage test_wrap
