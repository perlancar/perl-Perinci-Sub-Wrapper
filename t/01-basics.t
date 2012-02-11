#!perl

use 5.010;
use strict;
use warnings;
use Test::More 0.96;

use Perinci::Sub::Wrapper qw(wrap_sub);

my ($sub, $meta);

$sub = sub {};
$meta = {};
test_wrap(
    name => 'meta version != 1.1 -> fail',
    wrap_args => {sub => $sub, meta => $meta},
    wrap_status => 412,
);

$sub = sub { [200, "OK", $_[0]/$_[1]] };
$meta = {v=>1.1, args_as=>"array", args=>{a=>{pos=>0}, b=>{pos=>1}}};
test_wrap(
    name => '(trap=1, default) call doesn\'t die',
    wrap_args => {sub => $sub, meta => $meta},
    wrap_status => 200,
    call_args => [12, 3],
    call_res => [200, "OK", 4],
);
test_wrap(
    name => '(trap=1, default) call dies -> 500',
    wrap_args => {sub => $sub, meta => $meta},
    wrap_status => 200,
    call_args => [12, 0],
    call_status => 500,
);
test_wrap(
    name => '(trap=0) call dies -> dies',
    wrap_args => {sub => $sub, meta => $meta, trap=>0},
    wrap_status => 200,
    call_args => [12, 0],
    call_dies => 1,
);
test_wrap(
    name => '(result_naked=0) convert result_naked to 1',
    wrap_args => {sub => $sub, meta => $meta, convert=>{result_naked=>1}},
    wrap_status => 200,
    call_args => [12, 3],
    call_res => 4,
);

$sub = sub { $_[0]/$_[1] };
$meta = {v=>1.1, args_as=>"array", args=>{a=>{pos=>0}, b=>{pos=>1}},
         result_naked => 1};
test_wrap(
    name => '(result_naked=1)',
    wrap_args => {sub => $sub, meta => $meta},
    wrap_status => 200,
    call_args => [12, 3],
    call_res => 4,
);
test_wrap(
    name => '(result_naked=1) (convert result_naked to 0',
    wrap_args => {sub => $sub, meta => $meta, convert=>{result_naked=>0}},
    wrap_status => 200,
    call_args => [12, 3],
    call_res => [200, "OK", 4],
);

DONE_TESTING:
done_testing();

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
        } else {
            ok(!$wrap_eval_err, "wrap doesn't die") or diag $wrap_eval_err;
        }

        if (defined $test_args{wrap_status}) {
            is(ref($wrap_res), 'ARRAY', 'wrap res is array');
            is($wrap_res->[0], $test_args{wrap_status},
               "wrap status is $test_args{wrap_status}");
        }

        my $call_args = $test_args{call_args};
        if ($call_args) {

            my $sub = $wrap_res->[2]{sub};
            my $call_res;
            eval { $call_res = $sub->(@$call_args) };
            my $call_eval_err = $@;
            if ($test_args{call_dies}) {
                ok($call_eval_err, "call dies");
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

        done_testing();
    };
}
