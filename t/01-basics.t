#!perl

use 5.010;
use strict;
use warnings;
use FindBin '$Bin';
use lib $Bin, "$Bin/t";

use Sub::Spec::URI;
use Test::More 0.96;

sub test_uri {
    my %args = @_;
    my $name = $args{name};
    my $res;

    subtest $args{name} => sub {
        eval { $res = Sub::Spec::URI->new(@{ $args{args} }); };
        my $eval_err = $@;
        if ($args{dies}) {
            ok($eval_err, "dies");
        } else {
            ok(!$eval_err, "doesnt die") or diag "died: $eval_err";
        }

        if ($args{post_test}) {
            $args{post_test}->($res);
        }
    };
}

test_uri name=>'schemeless not acceptable', args=>['foo'], dies=>1;
test_uri name=>'unknown scheme not acceptable', args=>['foo:bar'], dies=>1;
test_uri name=>'invalid scheme', args=>['http+x://foo'], dies=>1;

test_uri name=>'pm: invalid syntax 1', args=>['pm:a:b'], dies=>1;
test_uri name=>'pm: invalid syntax 3', args=>['pm:a-b'], dies=>1;
test_uri name=>'pm: invalid syntax 4', args=>['pm:Dies/a-b'], dies=>1;
test_uri
    name=>'pm: basic tests (module)',
    args=>['pm:Dies'],
    post_test=>sub {
        my ($uri) = @_;
        is($uri->module, "Dies", "module()");
        ok(!$uri->sub, "sub()");
        is_deeply($uri->args, {}, "args()");
    };
test_uri
    name=>'pm: basic tests (module) (2)',
    args=>['pm:/Dies::Foo/'],
    post_test=>sub {
        my ($uri) = @_;
        is($uri->module, "Dies::Foo", "module()");
        ok(!$uri->sub, "sub()");
        is_deeply($uri->args, {}, "args()");
    };
test_uri
    name=>'pm: basic tests (module+sub)',
    args=>['pm://Dies/baz'],
    post_test=>sub {
        my ($uri) = @_;
        is($uri->module, "Dies", "module()");
        is($uri->sub, "baz", "sub()");
        is_deeply($uri->args, {}, "args()");
    };
test_uri
    name=>'pm: basic tests (module+sub+args)',
    args=>['pm:Dies/baz?arg1=1&arg2:j=[2,3]&arg3:j=[4'],
    post_test=>sub {
        my ($uri) = @_;
        is($uri->module, "Dies");
        is($uri->sub, "baz");
        is_deeply($uri->args, {arg1=>1, arg2=>[2,3]}, "args()");
    };

test_uri
    name=>'pm: list_subs, spec, call',
    args=>['pm:Foo/f2?a1=1'],
    post_test=>sub {
        my ($uri) = @_;
        is_deeply($uri->list_subs, ["f1", "f2"], "list_subs()")
            or diag explain $uri->list_subs;
        is_deeply($uri->spec, {summary=>"f2", args=>{}}, "spec()")
            or diag explain $uri->spec;
        my $res = $uri->call(a2=>2);
        is_deeply($res, [200, "OK", {a1=>1, a2=>2}], "call()")
            or diag explain $res;
    };

test_uri
    name=>'custom scheme handler',
    args=>['bar:soap'],
    post_test=>sub {
        my ($uri) = @_;
        is($uri->module, "barmod");
        is($uri->sub, "barsub");
    };

{
    local $Sub::Spec::URI::handlers{http} = 'Sub::Spec::URI::bar';
    test_uri name=>'setting %handlers',
        args=>['http://foo'],
            post_test=>sub {
                my ($uri) = @_;
                is($uri->module, "barmod");
                is($uri->sub, "barsub");
            };
}

done_testing();
