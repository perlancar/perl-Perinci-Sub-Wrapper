#!perl

use 5.010;
use strict;
use warnings;

package Foo;

use Perinci::Sub::Wrapper;

our %SPEC;
$SPEC{f1} = {v=>1.1, result_naked=>1};
sub f1 { "f1" }
$SPEC{f2} = {v=>1.1, result_naked=>1};
sub f2 { "f2" }

Perinci::Sub::Wrapper::wrap_all_subs(wrap_args=>{convert=>{result_naked=>0}});

package main;

use Test::More 0.96;

is_deeply(Foo::f1(), [200, "OK", "f1"], "f1 wrapped");
is_deeply(Foo::f2(), [200, "OK", "f2"], "f1 wrapped");

DONE_TESTING:
done_testing();

