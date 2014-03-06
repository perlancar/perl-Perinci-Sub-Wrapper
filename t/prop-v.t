#!perl

use 5.010;
use strict;
use warnings;

use Test::More 0.98;
use Test::Perinci::Sub::Wrapper qw(test_wrap);

subtest "prop: v" => sub {
    my $sub  = sub {};
    my $meta = {
        args=>{
            a=>[str => {default=>'x', arg_pos=>0, arg_greedy=>1,
                        arg_aliases=>{a1=>{}}}],
            b=>'int',
        },
        result=>'int',
    };
    test_wrap(
        name => 'convert v',
        wrap_args => {sub => $sub, meta => $meta},
        wrap_status => 200,
        posttest => sub {
            my ($wrap_res, $call_res) = @_;
            my $newmeta = $wrap_res->[2]{meta};
            is($newmeta->{v}, 1.1, 'version');
            is_deeply($newmeta->{args},
                      {a=>{schema=>['str'=>{default=>'x'}, {}],
                           pos=>0, greedy=>1, cmdline_aliases=>{a1=>{}}},
                       b=>{schema=>[int=>{}, {}]}}, 'args')
                or diag explain $newmeta->{args};
            # XXX test arg_completion conversion
            is_deeply($newmeta->{result},
                      {schema=>[int=>{}, {}]}, 'result')
                or diag explain $newmeta->{result};
        },
    );

    $meta = {v=>1.1};
    test_wrap(
        name => 'convert unsupported property -> fail',
        wrap_args => {sub => $sub, meta => $meta, convert=>{deps=>{}}},
        wrap_status => 502,
    );
};

DONE_TESTING:
done_testing;
