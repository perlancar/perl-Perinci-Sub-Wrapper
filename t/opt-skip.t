#!perl

use 5.010;
use strict;
use warnings;

use Test::More 0.98;
use Test::Perinci::Sub::Wrapper qw(test_wrap);

my $sub = sub {[200, "OK"]};
my $meta = {v=>1.1, args=>{a=>{}}};

test_wrap(
    name      => 'none (the default)',
    wrap_args => {sub => $sub, meta => $meta},
    calls     => [
        {argsr=>[], status=>200},
        {argsr=>[x=>1], status=>400, name=>'unknown arg'},
    ],
);

test_wrap(
    name      => 'args',
    wrap_args => {sub => $sub, meta => $meta, skip=>[qw/args/]},
    calls     => [
        {argsr=>[], status=>200},
        {argsr=>[x=>1], status=>200, name=>'unknown arg not checked'},
    ],
);

done_testing;
