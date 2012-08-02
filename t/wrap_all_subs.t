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
use Scalar::Util qw(blessed);

ok(blessed(\&Foo::f1), "f1 wrapped");
is_deeply(Foo::f1(), [200, "OK", "f1"], "f1 result");
is($Foo::SPEC{f1}{result_naked}, 0, "f1 meta replaced");

ok(blessed(\&Foo::f2), "f2 wrapped");
is_deeply(Foo::f2(), [200, "OK", "f2"], "f2 result");
is($Foo::SPEC{f2}{result_naked}, 0, "f2 meta replaced");

DONE_TESTING:
done_testing();

