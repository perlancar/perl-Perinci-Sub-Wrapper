#!perl

use 5.010;
use strict;
use warnings;

use Test::More 0.98;
use Test::Perinci::Sub::Wrapper qw(test_wrap);

my $sub = sub {
    my %args = @_;
    [200, "OK", join(",", map{"$_=".($args{$_}//"")} "a".."e")];
};
my $meta;

$meta = {v=>1.1, args=>{a=>{foo=>1}}};
test_wrap(
    name      => 'unknown arg spec key -> dies',
    wrap_args => {sub => $sub, meta => $meta},
    wrap_dies => 1,
);

$meta = {v=>1.1, args=>{a=>{_foo=>1}}};
test_wrap(
    name        => 'arg spec key prefixed by _ is ignored',
    wrap_args   => {sub => $sub, meta => $meta},
    wrap_status => 200,
);

my @t;

$meta = {
    v => 1.1,
    args => {
        a => {
            summary => 'req arg, has default',
            req => 1,
            schema => [int => min=>1, max=>100, default=>10],
        },
        b => {
            summary => 'req arg, has no default',
            req => 1,
            schema => "int",
        },
        c => {
            summary => 'req arg, req schema',
            req => 1,
            schema => "int*",
        },
        d => {
            summary => 'has default',
            schema => [int => {min=>1, max=>100, default=>40}],
        },
        e => {
            summary => 'has no default',
            schema => [int => {min=>1, max=>100}],
        },
    },
};
test_wrap(
    name        => 'basics',
    wrap_args   => {sub => $sub, meta => $meta},
    wrap_status => 200,
    calls       => [
        {name => 'arg schema checked (b invalid)',
         argsr => [a=>101, b=>101, c=>1], status => 400},
        {name => 'arg schema checked (c req value)',
         argsr => [a=>101, b=>1, c=>undef], status => 400},

        @t = (
            {name => 'normal',
             argsr => [a=>1, b=>1, c=>1], status => 200},
            {name => 'req arg checked (b missing)',
             argsr => [a=>1, c=>1], status => 400},
            {name => 'req arg checked #2 (undef is ok for b)',
             argsr => [a=>1, c=>1, b=>undef], status => 200},

            {name => 'unknown arg rejected',
             argsr => [a=>1, b=>1, c=>1, f=>1], status => 400},
            {name => 'invalid arg checked',
             argsr => [a=>1, " "=>1], status => 400},
            {name => 'unknown special arg not checked',
             argsr => [a=>1, -b=>1], status => 200},
        ),
    ],
);

test_wrap(
    name        => 'opt: validate_args=0',
    wrap_args   => {sub => $sub, meta => $meta, validate_args => 0},
    wrap_status => 200,
    calls       => [
        # the only difference is here
        {name => 'arg schema not checked', argsr => [a=>11], status => 200},

        # the rest are the same
        @t,
    ],
);

done_testing;
