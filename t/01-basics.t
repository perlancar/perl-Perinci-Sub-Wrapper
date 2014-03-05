#!perl

use 5.010;
use strict;
use warnings;

use List::Util qw(sum);
use Scalar::Util qw(blessed);
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

subtest 'arg: _schema_is_normalized' => sub {
    my $sub  = sub {};
    my $meta = {v=>1.1, args=>{a=>{schema=>"int"},
                               b=>{cmdline_aliases=>{B=>{schema=>"bool"}}}}};
    test_wrap(
        name => "normalized",
        wrap_args => {sub => $sub, meta => $meta},
        posttest => sub {
            my ($wrap_res, $call_res) = @_;
            my $newmeta = $wrap_res->[2]{meta};
            is_deeply($newmeta->{args}{a}{schema}, [int=>{}, {}],
                      "schemas by default are normalized (a)");
            is_deeply($newmeta->{args}{b}{cmdline_aliases}{B}{schema},[bool=>{},{}],
                      "schemas in cmdline_aliases by default are normalized (b)");
        },
    );
    test_wrap(
        name => 'not normalized',
        wrap_args => {sub => $sub, meta => $meta, _schema_is_normalized=>1},
        posttest => sub {
            my ($wrap_res, $call_res) = @_;
            my $newmeta = $wrap_res->[2]{meta};
            is_deeply($newmeta->{args}{a}{schema}, "int",
                      "schemas are not normalized (a)")
                or diag explain $newmeta;
            is_deeply($newmeta->{args}{b}{cmdline_aliases}{B}{schema}, "bool",
                      "schemas in cmdline_aliases are not normalized (b)")
                or diag explain $newmeta;
        },
    );
};

subtest 'prop: args_as' => sub {
    my $meta = {v=>1.1,
                args=>{a=>{pos=>0, schema=>"int"},
                       b=>{pos=>1, schema=>"int"}}};
    {
        test_wrap(
            name => 'args_as=hash (default)',
            wrap_args => {sub => sub {my %args=@_; [200,"OK",$args{a}/$args{b}]}, meta => $meta},
            wrap_status => 200,
            calls => [
                {argsr=>[a=>10, b=>5], status=>200, actual_res=>2},
                {argsr=>[a=>12, b=>3], status=>200, actual_res=>4},
            ],
        );
    }
    {
        local $meta->{args_as} = 'hashref';
        test_wrap(
            name => 'args_as=hashref',
            wrap_args => {sub => sub {[200,"OK",$_[0]{a}/$_[0]{b}]}, meta => $meta},
            wrap_status => 200,
            calls => [
                {argsr=>[{a=>10, b=>5}], status=>200, actual_res=>2},
                {argsr=>[{a=>12, b=>3}], status=>200, actual_res=>4},
            ],
        );
    }
    {
        local $meta->{args_as} = 'array';
        test_wrap(
            name => 'args_as=array',
            wrap_args => {sub => sub {[200,"OK",$_[0]/$_[1]]}, meta => $meta},
            wrap_status => 200,
            calls => [
                {argsr=>[10, 5], status=>200, actual_res=>2},
                {argsr=>[12, 3], status=>200, actual_res=>4},
            ],
        );
    }
    {
        local $meta->{args_as} = 'arrayref';
        test_wrap(
            name => 'args_as=arrayref',
            wrap_args => {sub => sub {[200,"OK",$_[0][0]/$_[0][1]]}, meta => $meta},
            wrap_status => 200,
            calls => [
                {argsr=>[[10, 5]], status=>200, actual_res=>2},
                {argsr=>[[12, 3]], status=>200, actual_res=>4},
            ],
        );
    }
    {
        local $meta->{args_as} = 'hash';
        test_wrap(
            name => 'convert args_as hash -> array',
            wrap_args => {sub => sub {my %args=@_; [200,"OK",$args{a}/$args{b}]}, meta => $meta, convert=>{args_as=>'array'}},
            wrap_status => 200,
            calls => [
                {argsr=>[10, 5], status=>200, actual_res=>2},
                {argsr=>[12, 3], status=>200, actual_res=>4},
            ],
        );
    }
    # XXX convert hash->hashref
    # XXX convert hash->arrayref

    # XXX convert hashref->hash
    # XXX convert hashref->array
    # XXX convert hashref->arrayref

    {
        local $meta->{args_as} = 'array';
        test_wrap(
            name => 'convert args_as array -> hash',
            wrap_args => {sub => sub {[200,"OK",$_[0]/$_[1]]}, meta => $meta, convert=>{args_as=>'hash'}},
            wrap_status => 200,
            calls => [
                {argsr=>[a=>10, b=>5], status=>200, actual_res=>2},
                {argsr=>[a=>12, b=>3], status=>200, actual_res=>4},
            ],
        );
    }
    # XXX convert array->hashref
    # XXX convert array->arrayref

    # XXX convert arrayref->hash
    # XXX convert arrayref->hashref
    # XXX convert arrayref->array

    # XXX convert hash->array + greedy
};

subtest 'prop: result_naked' => sub {
    my $meta = {v=>1.1,
                args=>{a=>{pos=>0, schema=>"int"},
                       b=>{pos=>1, schema=>"int"}}};
    test_wrap(
        name => 'convert result_naked 0->1',
        wrap_args => {sub => sub {my %args=@_;[200,"OK",$args{a}/$args{b}]}, meta => $meta, convert=>{result_naked=>1}},
        calls => [
            {argsr => [a=>12, b=>3], res => 4},
        ],
        posttest => sub {
            my ($wrap_res, $call_res) = @_;
            my $meta = $wrap_res->[2]{meta};
            ok($meta->{result_naked}, "new meta result_naked=1");
        },
    );
    $meta->{result_naked} = 1;
    test_wrap(
        name => 'convert result_naked 1->0',
        wrap_args => {sub => sub {my %args=@_;$args{a}/$args{b}}, meta => $meta, convert=>{result_naked=>0}},
        calls => [
            {argsr => [a=>12, b=>3], res => [200,"OK",4]},
        ],
        posttest => sub {
            my ($wrap_res, $call_res) = @_;
            my $meta = $wrap_res->[2]{meta};
            ok(!$meta->{result_naked}, "new meta result_naked=0");
        },
    );
};

subtest 'arg: log' => sub {
    test_wrap(
        name => 'log=1 (default)',
        wrap_args => {sub => sub {}, meta => {v=>1.1}},
        posttest => sub {
            my ($wrap_res, $call_res) = @_;
            my $meta = $wrap_res->[2]{meta};
            ok($meta->{"x.perinci.sub.wrapper.log"}, "wrap log produced");
        },
    );
    test_wrap(
        name => 'log=0',
        wrap_args => {sub => sub {}, meta => {v=>1.1}, log=>0},
        posttest => sub {
            my ($wrap_res, $call_res) = @_;
            my $meta = $wrap_res->[2]{meta};
            ok(!$meta->{"x.perinci.sub.wrapper.log"}, "wrap log not produced");
        },
    );
};

subtest 'prop: default_lang' => sub {
    my $meta = {
        v=>1.1,
        summary => "EN",
        "summary.alt.lang.id_ID" => "ID",
        "summary.alt.lang.fr_FR" => "FR",
        args=>{a=>{default_lang=>"id_ID", summary=>"ID arg.a"},
               b=>{summary=>"EN arg.b"}},
        result=>{"description.alt.lang.id_ID"=>"ID res"},
        examples=>[{"description.alt.lang.en_US"=>"EN ex1"}],
        links=>[{},
                {"description.alt.lang.fr_FR"=>"FR link1"}],
        tags => ['test', {name=>'category', summary=>'EN t1'}],
    };
    my $newmeta_expected = {
        v=>1.1,
        args_as=>"hash",
        default_lang=>"id_ID",
        summary => "ID",
        "summary.alt.lang.en_US" => "EN",
        "summary.alt.lang.fr_FR" => "FR",
        args=>{a=>{default_lang=>"id_ID", summary=>"ID arg.a"},
               b=>{default_lang=>"id_ID", "summary.alt.lang.en_US"=>"EN arg.b"}},
        result=>{default_lang=>"id_ID", description=>"ID res"},
        examples=>[{default_lang=>"id_ID", "description.alt.lang.en_US"=>"EN ex1"}],
        links=>[{default_lang=>"id_ID"},
                {default_lang=>"id_ID", "description.alt.lang.fr_FR"=>"FR link1"}],
        tags => ['test',
                 {default_lang=>'id_ID', name=>'category',
                  "summary.alt.lang.en_US"=>'EN t1'}],

        "x.perinci.sub.wrapper.log" => [
            {normalize_schema=>1, validate_args=>1, validate_result=>1},
        ],
    };
    test_wrap(
        name => 'convert default_lang',
        wrap_args => {sub => sub{}, meta => $meta, convert=>{default_lang=>"id_ID"}},
        wrap_status => 200,
        posttest => sub {
            my ($wrap_res, $call_res) = @_;
            my $newmeta = $wrap_res->[2]{meta};
            is_deeply($newmeta, $newmeta_expected, "newmeta")
                or diag explain $newmeta;
        },
    );
};

DONE_TESTING:
done_testing;
