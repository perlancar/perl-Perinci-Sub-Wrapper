#!perl

use 5.010;
use strict;
use warnings;

use Test::More 0.98;
use Test::Perinci::Sub::Wrapper qw(test_wrap);

my $sub  = sub {[200]};
my $meta;

$meta = {
    v => 1.1,
    args => {
        a => {
            req    => 1,
            schema => [int => min => 1, max => 10],
        },
    },
};

test_wrap(
    name        => 'on (the default)',
    wrap_args   => {sub => $sub, meta => $meta},
    wrap_status => 200,
    calls       => [
        {name => 'normal', argsr => [a=>1], status => 200},
        {name => 'schema checked', argsr => [a=>11], status => 400},
        {name => 'req checked', argsr => [], status => 400},
        {name => 'unknown arg checked', argsr => [a=>1, b=>1], status => 400},
        {name => 'unknown special arg not checked',
         argsr => [a=>1, -b=>1], status => 200},
    ],
);

test_wrap(
    name        => 'off',
    wrap_args   => {sub => $sub, meta => $meta, validate_args => 0},
    wrap_status => 200,
    calls       => [
        {name => 'normal', argsr => [a=>1], status => 200},
        {name => 'schema not checked', argsr => [a=>11], status => 200},
        {name => 'req checked', argsr => [], status => 400},
        {name => 'unknown arg checked', argsr => [a=>1, b=>1], status => 400},
        {name => 'unknown special arg not checked',
         argsr => [a=>1, -b=>1], status => 200},
    ],
);

done_testing;
