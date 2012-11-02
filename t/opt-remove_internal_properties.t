#!perl

use 5.010;
use Test::More 0.98;
use Test::Perinci::Sub::Wrapper qw(test_wrap);

my $sub = sub { [200, "OK"] };
my $meta = {
    v=>1.1,
    args=>{a=>{pos=>0, schema=>"int", _argspec1=>"internal"}, b=>{pos=>1}},
    _prop1=>"internal",
    result=>{_res1=>"1"},
    examples=>[{_ex1=>"1"}],
    links=>[{_ln1=>"1"}],
};
test_wrap(
    name => '(remove_internal_properties=1, default)',
    wrap_args => {sub => $sub, meta => $meta},
    wrap_status => 200,
    posttest => sub {
        my ($wrap_res, $call_res) = @_;
        my $newmeta = $wrap_res->[2]{meta};
        ok(!exists($newmeta->{_prop1}), "_prop1 removed");
        ok(!exists($newmeta->{args}{a}{_argspec1}), "_argspec1 removed");
        ok(!exists($newmeta->{result}{_res1}), "_res1 removed");
        ok(!exists($newmeta->{examples}[0]{_ex1}), "_ex1 removed");
        ok(!exists($newmeta->{links}[0]{_ln1}), "_ln1 removed");
    },
);
test_wrap(
    name => '(remove_internal_properties=0)',
    wrap_args => {sub => $sub, meta => $meta, remove_internal_properties=>0},
    wrap_status => 200,
    posttest => sub {
        my ($wrap_res, $call_res) = @_;
        my $newmeta = $wrap_res->[2]{meta};
        ok($newmeta->{_prop1}, "_prop1 exists");
        ok($newmeta->{args}{a}{_argspec1}, "_argspec1 exists");
        ok($newmeta->{result}{_res1}, "_res1 exists");
        ok($newmeta->{examples}[0]{_ex1}, "_ex1 exists");
        ok($newmeta->{links}[0]{_ln1}, "_ln1 exists");
    },
);

done_testing;
