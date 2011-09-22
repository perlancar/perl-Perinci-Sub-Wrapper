#!perl

use 5.010;
use strict;
use warnings;
use Test::More 0.96;

use Sub::Spec::Wrapper qw(wrap_sub);

my $sub;

$sub = wrap_sub(sub => sub {[200, "OK", 42]}, spec=>{});
is_deeply($sub->(), [200, "OK", 42], "wrap 1");

$sub = wrap_sub(sub => sub {42}, spec=>{result_naked=>1});
is_deeply($sub->(), [200, "OK", 42], "result for result_naked sub is wrapped");

my $orig = sub {

};

$sub = wrap_sub(sub => sub {my $args=shift; $args->{a}**2},
                spec=>{args=>{a=>"float*"},result_naked=>1,args_as=>'hashref'});
is_deeply($sub->(a=>4)->[2], 16, "args_as=hashref");

$sub = wrap_sub(sub => sub {my $a=shift; my $b=shift; $a*$b},
                spec=>{args=>{a=>["float*"=>{arg_pos=>0}],
                              b=>["float*"=>{arg_pos=>1}],},
                       result_naked=>1,args_as=>'array'});
is_deeply($sub->(a=>3, b=>7)->[2], 21, "args_as=array");

$sub = wrap_sub(sub => sub {my $args=shift; $args->[0]*$args->[1]},
                spec=>{args=>{a=>["float*"=>{arg_pos=>0}],
                              b=>["float*"=>{arg_pos=>1}],},
                       result_naked=>1,args_as=>'arrayref'});
is_deeply($sub->(a=>3, b=>7)->[2], 21, "args_as=arrayref");

DONE_TESTING:
done_testing();
