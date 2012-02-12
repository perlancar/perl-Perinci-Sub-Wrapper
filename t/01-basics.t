#!perl

use 5.010;
use strict;
use warnings;
use Test::More 0.96;

use List::Util qw(sum);
use Perinci::Sub::Wrapper qw(wrap_sub);
use Test::Perinci::Sub::Wrapper qw(test_wrap);

my ($sub, $meta);

$sub = sub {};
$meta = {};
test_wrap(
    name => 'meta version != 1.1 -> fail',
    wrap_args => {sub => $sub, meta => $meta},
    wrap_status => 412,
);

$meta = {v=>1.1, deps=>{}};
test_wrap(
    name => 'unsupported conversion -> fail',
    wrap_args => {sub => $sub, meta => $meta, convert=>{deps=>{}}},
    wrap_status => 502,
);

# test wrap arg 'trap' + wrapping 'args_as' property

$sub = sub { [200, "OK", $_[0]/$_[1]] };
$meta = {v=>1.1, args_as=>"array", args=>{a=>{pos=>0}, b=>{pos=>1}}};
test_wrap(
    name => '(trap=1, default) call doesn\'t die',
    wrap_args => {sub => $sub, meta => $meta},
    wrap_status => 200,
    call_argsr => [12, 3],
    call_res => [200, "OK", 4],
    posttest => sub {
        my ($wrap_res, $call_res) = @_;
        my $newmeta = $wrap_res->[2]{meta};
        # currently not the case, we add default args_as, result_naked, etc.
        #is("$newmeta", "$meta", "meta not copied when there's no conversion");
    },
);
test_wrap(
    name => '(trap=1, default) call dies -> 500',
    wrap_args => {sub => $sub, meta => $meta},
    wrap_status => 200,
    call_argsr => [12, 0],
    call_status => 500,
);
test_wrap(
    name => '(trap=0) call dies -> dies',
    wrap_args => {sub => $sub, meta => $meta, trap=>0},
    wrap_status => 200,
    call_argsr => [12, 0],
    call_dies => 1,
);
test_wrap(
    name => '(result_naked=0) convert result_naked to 1',
    wrap_args => {sub => $sub, meta => $meta, convert=>{result_naked=>1}},
    wrap_status => 200,
    call_argsr => [12, 3],
    call_res => 4,
    posttest => sub {
        my ($wrap_res, $call_res) = @_;
        my $meta = $wrap_res->[2]{meta};
        ok($meta->{result_naked}, "new meta result_naked=1");
    },
);

$sub = sub { $_[0]/$_[1] };
$meta = {v=>1.1, args_as=>"array", args=>{a=>{pos=>0}, b=>{pos=>1}},
         result_naked => 1};
test_wrap(
    name => '(result_naked=1)',
    wrap_args => {sub => $sub, meta => $meta},
    wrap_status => 200,
    call_argsr => [12, 3],
    call_res => 4,
);
test_wrap(
    name => '(result_naked=1) convert result_naked to 0',
    wrap_args => {sub => $sub, meta => $meta, convert=>{result_naked=>0}},
    wrap_status => 200,
    call_argsr => [12, 3],
    call_res => [200, "OK", 4],
    posttest => sub {
        my ($wrap_res, $call_res) = @_;
        my $meta = $wrap_res->[2]{meta};
        ok(!$meta->{result_naked}, "new meta result_naked=0");
    },
);

# test args_as conversion

test_wrap(
    name => '(args_as=array) convert args_as to arrayref',
    wrap_args => {sub => $sub, meta => $meta, convert=>{args_as=>'arrayref'}},
    wrap_status => 200,
    call_argsr => [[12, 3]],
    call_res => 4,
    posttest => sub {
        my ($wrap_res, $call_res) = @_;
        my $meta = $wrap_res->[2]{meta};
        is($meta->{args_as}, 'arrayref', "new meta args_as");
    },
);
test_wrap(
    name => '(args_as=array) convert args_as to hash',
    wrap_args => {sub => $sub, meta => $meta, convert=>{args_as=>'hash'}},
    wrap_status => 200,
    call_argsr => [a=>12, b=>3],
    call_res => 4,
    posttest => sub {
        my ($wrap_res, $call_res) = @_;
        my $meta = $wrap_res->[2]{meta};
        is($meta->{args_as}, 'hash', "new meta args_as");
    },
);
test_wrap(
    name => '(args_as=array) convert args_as to hashref',
    wrap_args => {sub => $sub, meta => $meta, convert=>{args_as=>'hashref'}},
    wrap_status => 200,
    call_argsr => [{a=>12, b=>3}],
    call_res => 4,
    posttest => sub {
        my ($wrap_res, $call_res) = @_;
        my $meta = $wrap_res->[2]{meta};
        is($meta->{args_as}, 'hashref', "new meta args_as");
    },
);

$sub = sub { $_[0][0]/$_[0][1] };
$meta = {v=>1.1, args_as=>"arrayref", args=>{a=>{pos=>0}, b=>{pos=>1}},
         result_naked => 1};
test_wrap(
    name => '(args_as=arrayref)',
    wrap_args => {sub => $sub, meta => $meta},
    wrap_status => 200,
    call_argsr => [[12, 3]],
    call_res => 4,
);
test_wrap(
    name => '(args_as=arrayref) convert args_as to array',
    wrap_args => {sub => $sub, meta => $meta, convert=>{args_as=>'array'}},
    wrap_status => 200,
    call_argsr => [12, 3],
    call_res => 4,
    posttest => sub {
        my ($wrap_res, $call_res) = @_;
        my $meta = $wrap_res->[2]{meta};
        is($meta->{args_as}, 'array', "new meta args_as");
    },
);
test_wrap(
    name => '(args_as=arrayref) convert args_as to hash',
    wrap_args => {sub => $sub, meta => $meta, convert=>{args_as=>'hash'}},
    wrap_status => 200,
    call_argsr => [a=>12, b=>3],
    call_res => 4,
    posttest => sub {
        my ($wrap_res, $call_res) = @_;
        my $meta = $wrap_res->[2]{meta};
        is($meta->{args_as}, 'hash', "new meta args_as");
    },
);
test_wrap(
    name => '(args_as=arrayref) convert args_as to hashref',
    wrap_args => {sub => $sub, meta => $meta, convert=>{args_as=>'hashref'}},
    wrap_status => 200,
    call_argsr => [{a=>12, b=>3}],
    call_res => 4,
    posttest => sub {
        my ($wrap_res, $call_res) = @_;
        my $meta = $wrap_res->[2]{meta};
        is($meta->{args_as}, 'hashref', "new meta args_as");
    },
);

$sub = sub { my %args = @_; $args{a}/$args{b} };
$meta = {v=>1.1, args_as=>"hash", args=>{a=>{pos=>0}, b=>{pos=>1}},
         result_naked => 1};
test_wrap(
    name => '(args_as=hash)',
    wrap_args => {sub => $sub, meta => $meta},
    wrap_status => 200,
    call_argsr => [a=>12, b=>3],
    call_res => 4,
);
test_wrap(
    name => '(args_as=hash) convert args_as to array',
    wrap_args => {sub => $sub, meta => $meta, convert=>{args_as=>'array'}},
    wrap_status => 200,
    call_argsr => [12, 3],
    call_res => 4,
    posttest => sub {
        my ($wrap_res, $call_res) = @_;
        my $meta = $wrap_res->[2]{meta};
        is($meta->{args_as}, 'array', "new meta args_as");
    },
);
test_wrap(
    name => '(args_as=hash) convert args_as to arrayref',
    wrap_args => {sub => $sub, meta => $meta, convert=>{args_as=>'arrayref'}},
    wrap_status => 200,
    call_argsr => [[12, 3]],
    call_res => 4,
    posttest => sub {
        my ($wrap_res, $call_res) = @_;
        my $meta = $wrap_res->[2]{meta};
        is($meta->{args_as}, 'arrayref', "new meta args_as");
    },
);
test_wrap(
    name => '(args_as=hash) convert args_as to hashref',
    wrap_args => {sub => $sub, meta => $meta, convert=>{args_as=>'hashref'}},
    wrap_status => 200,
    call_argsr => [{a=>12, b=>3}],
    call_res => 4,
    posttest => sub {
        my ($wrap_res, $call_res) = @_;
        my $meta = $wrap_res->[2]{meta};
        is($meta->{args_as}, 'hashref', "new meta args_as");
    },
);

$sub = sub { my $args = shift; $args->{a}/$args->{b} };
$meta = {v=>1.1, args_as=>"hashref", args=>{a=>{pos=>0}, b=>{pos=>1}},
         result_naked => 1};
test_wrap(
    name => '(args_as=hashref)',
    wrap_args => {sub => $sub, meta => $meta},
    wrap_status => 200,
    call_argsr => [{a=>12, b=>3}],
    call_res => 4,
);
test_wrap(
    name => '(args_as=hashref) convert args_as to array',
    wrap_args => {sub => $sub, meta => $meta, convert=>{args_as=>'array'}},
    wrap_status => 200,
    call_argsr => [12, 3],
    call_res => 4,
    posttest => sub {
        my ($wrap_res, $call_res) = @_;
        my $meta = $wrap_res->[2]{meta};
        is($meta->{args_as}, 'array', "new meta args_as");
    },
);
test_wrap(
    name => '(args_as=hashref) convert args_as to arrayref',
    wrap_args => {sub => $sub, meta => $meta, convert=>{args_as=>'arrayref'}},
    wrap_status => 200,
    call_argsr => [[12, 3]],
    call_res => 4,
    posttest => sub {
        my ($wrap_res, $call_res) = @_;
        my $meta = $wrap_res->[2]{meta};
        is($meta->{args_as}, 'arrayref', "new meta args_as");
    },
);
test_wrap(
    name => '(args_as=hashref) convert args_as to hash',
    wrap_args => {sub => $sub, meta => $meta, convert=>{args_as=>'hash'}},
    wrap_status => 200,
    call_argsr => [a=>12, b=>3],
    call_res => 4,
    posttest => sub {
        my ($wrap_res, $call_res) = @_;
        my $meta = $wrap_res->[2]{meta};
        is($meta->{args_as}, 'hash', "new meta args_as");
    },
);

$sub = sub { my %args = @_; $args{a}/sum(@{$args{b}}) };
$meta = {v=>1.1, args=>{a=>{pos=>0}, b=>{pos=>1, greedy=>1}},
         result_naked => 1};
my ($wrapped, $wrapped_meta);
test_wrap(
    name => '(args_as=hash, default) greedy, no conversion',
    wrap_args => {sub => $sub, meta => $meta},
    wrap_status => 200,
    call_argsr => [a=>12, b=>[1, 2]],
    call_res => 4,
);
test_wrap(
    name => '(args_as=hash) greedy, conversion to array',
    wrap_args => {sub => $sub, meta => $meta, convert=>{args_as=>'array'}},
    wrap_status => 200,
    call_argsr => [12, 1, 2],
    call_res => 4,
    posttest => sub {
        my ($wrap_res, $call_res) = @_;
        $wrapped = $wrap_res->[2]{sub};
        $wrapped_meta = $wrap_res->[2]{meta};
    },
);

# test wrapping 'deps' property

$sub = sub {[200,"OK"]};
$meta = {v=>1.1, args=>{}, deps=>{env=>"A"}};
{
    local $ENV{A};
    test_wrap(
        name => 'deps 1',
        wrap_args => {sub => $sub, meta => $meta},
        wrap_status => 200,
        call_argsr => [],
        call_status => 412,
    );
    $ENV{A} = 1;
    test_wrap(
        name => 'deps 2',
        wrap_args => {sub => $sub, meta => $meta},
        wrap_status => 200,
        call_argsr => [],
        call_status => 200,
    );
}

# test wrap arg 'force'

test_wrap(
    name => 'force=0, double wrapping -> fail',
    wrap_args => {sub => $wrapped, meta => $wrapped_meta},
    wrap_status => 304,
);
test_wrap(
    name => 'force=1, double wrapping, no conversion',
    wrap_args => {sub => $wrapped, meta => $wrapped_meta,
                  convert=>{}, force=>1},
    wrap_status => 200,
    call_argsr => [12, 1, 2],
    call_res => 4,
);

DONE_TESTING:
done_testing();

