#!perl

use 5.010;
use strict;
use warnings;

package Foo;
use Perinci::Sub::Wrapper qw(wrapped);

our %SPEC;
$SPEC{f1} = {v=>1.1, result_naked=>1};
sub f1 { wrapped() ? 1:0 }

package main;
use Test::More 0.96;

is(Foo::f1(), 0, "f1 not yet wrapped");
Perinci::Sub::Wrapper::wrap_all_subs(package=>"Foo");
is(Foo::f1(), 1, "f1 now wrapped");

DONE_TESTING:
done_testing();

