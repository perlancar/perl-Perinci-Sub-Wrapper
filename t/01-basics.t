#!perl

use 5.010;
use strict;
use warnings;
use Test::More 0.96;

use List::Util qw(sum);
use Perinci::Sub::Wrapper qw(wrap_sub);
use Test::Perinci::Sub::Wrapper qw(test_wrap);
use Scalar::Util qw(blessed);

my ($sub, $meta);

$sub = sub {};
$meta = {
    args=>{
        a=>[str => {default=>'x', arg_pos=>0, arg_greedy=>1,
                    arg_aliases=>{a1=>{}}}],
        b=>'int',
    },
    result=>'int',
};
# XXX test arg_completion conversion
test_wrap(
    name => 'meta version == 1.0 -> converted to 1.1',
    wrap_args => {sub => $sub, meta => $meta},
    wrap_status => 200,
    posttest => sub {
        my ($wrap_res, $call_res) = @_;
        my $newmeta = $wrap_res->[2]{meta};
        is($newmeta->{v}, 1.1, 'version');
        is_deeply($newmeta->{args},
                  {a=>{schema=>['str'=>{default=>'x'}],
                       pos=>0, greedy=>1, cmdline_aliases=>{a1=>{}}},
                   b=>{schema=>[int=>{}]}}, 'args')
            or diag explain $newmeta->{args};
        is_deeply($newmeta->{result},
                  {schema=>[int=>{}]}, 'result')
            or diag explain $newmeta->{result};


    },
);

$meta = {v=>1.1};
test_wrap(
    name => 'unsupported conversion -> fail',
    wrap_args => {sub => $sub, meta => $meta, convert=>{deps=>{}}},
    wrap_status => 502,
);

# test wrap arg 'trap' + wrapping 'args_as' property, also test
# normalizing schemas

$sub = sub { [200, "OK", $_[0]/$_[1]] };
$meta = {v=>1.1, args_as=>"array",
         args=>{a=>{pos=>0, schema=>"int"},
                b=>{pos=>1, cmdline_aliases=>{B=>{schema=>'bool'}}}}};
test_wrap(
    name => '(trap=1, default) call doesn\'t die',
    wrap_args => {sub => $sub, meta => $meta},
    wrap_status => 200,
    call_argsr => [12, 3],
    call_res => [200, "OK", 4],
    posttest => sub {
        my ($wrap_res, $call_res) = @_;
        my $newmeta = $wrap_res->[2]{meta};
        is_deeply($newmeta->{args}{a}{schema}, [int=>{}],
                  "schemas by default are normalized (a)");
        is_deeply($newmeta->{args}{b}{cmdline_aliases}{B}{schema}, [bool=>{}],
                  "schemas in cmdline_aliases by default are normalized (b)");
    },
);
test_wrap(
    name => '(trap=1, default) call dies -> 500',
    wrap_args => {sub => $sub, meta => $meta, normalize_schemas=>0},
    wrap_status => 200,
    call_argsr => [12, 0],
    call_status => 500,
    posttest => sub {
        my ($wrap_res, $call_res) = @_;
        my $newmeta = $wrap_res->[2]{meta};
        is_deeply($newmeta->{args}{a}{schema}, "int",
                  "schemas are not normalize when normalized_schemas=0 (a)")
            or diag explain $newmeta;
    },
);
test_wrap(
    name => '(trap=0) call dies -> dies',
    wrap_args => {sub => $sub, meta => $meta, trap=>0},
    wrap_status => 200,
    call_argsr => [12, 0],
    call_dies => 1,
);
test_wrap(
    name => '(result_naked=0) convert result_naked to 1',
    wrap_args => {sub => $sub, meta => $meta, convert=>{result_naked=>1}},
    wrap_status => 200,
    call_argsr => [12, 3],
    call_res => 4,
    posttest => sub {
        my ($wrap_res, $call_res) = @_;
        my $meta = $wrap_res->[2]{meta};
        ok($meta->{result_naked}, "new meta result_naked=1");
    },
);

$sub = sub { $_[0]/$_[1] };
$meta = {v=>1.1, args_as=>"array", args=>{a=>{pos=>0}, b=>{pos=>1}},
         result_naked => 1};
test_wrap(
    name => '(result_naked=1)',
    wrap_args => {sub => $sub, meta => $meta},
    wrap_status => 200,
    call_argsr => [12, 3],
    call_res => 4,
);
test_wrap(
    name => '(result_naked=1) convert result_naked to 0',
    wrap_args => {sub => $sub, meta => $meta, convert=>{result_naked=>0}},
    wrap_status => 200,
    call_argsr => [12, 3],
    call_res => [200, "OK", 4],
    posttest => sub {
        my ($wrap_res, $call_res) = @_;
        my $meta = $wrap_res->[2]{meta};
        ok(!$meta->{result_naked}, "new meta result_naked=0");
    },
);

# test args_as conversion

test_wrap(
    name => '(args_as=array) convert args_as to arrayref',
    wrap_args => {sub => $sub, meta => $meta, convert=>{args_as=>'arrayref'}},
    wrap_status => 200,
    call_argsr => [[12, 3]],
    call_res => 4,
    posttest => sub {
        my ($wrap_res, $call_res) = @_;
        my $meta = $wrap_res->[2]{meta};
        is($meta->{args_as}, 'arrayref', "new meta args_as");
    },
);
test_wrap(
    name => '(args_as=array) convert args_as to hash',
    wrap_args => {sub => $sub, meta => $meta, convert=>{args_as=>'hash'}},
    wrap_status => 200,
    call_argsr => [a=>12, b=>3],
    call_res => 4,
    posttest => sub {
        my ($wrap_res, $call_res) = @_;
        my $meta = $wrap_res->[2]{meta};
        is($meta->{args_as}, 'hash', "new meta args_as");
    },
);
test_wrap(
    name => '(args_as=array) convert args_as to hashref',
    wrap_args => {sub => $sub, meta => $meta, convert=>{args_as=>'hashref'}},
    wrap_status => 200,
    call_argsr => [{a=>12, b=>3}],
    call_res => 4,
    posttest => sub {
        my ($wrap_res, $call_res) = @_;
        my $meta = $wrap_res->[2]{meta};
        is($meta->{args_as}, 'hashref', "new meta args_as");
    },
);

$sub = sub { $_[0][0]/$_[0][1] };
$meta = {v=>1.1, args_as=>"arrayref", args=>{a=>{pos=>0}, b=>{pos=>1}},
         result_naked => 1};
test_wrap(
    name => '(args_as=arrayref)',
    wrap_args => {sub => $sub, meta => $meta},
    wrap_status => 200,
    call_argsr => [[12, 3]],
    call_res => 4,
);
test_wrap(
    name => '(args_as=arrayref) convert args_as to array',
    wrap_args => {sub => $sub, meta => $meta, convert=>{args_as=>'array'}},
    wrap_status => 200,
    call_argsr => [12, 3],
    call_res => 4,
    posttest => sub {
        my ($wrap_res, $call_res) = @_;
        my $meta = $wrap_res->[2]{meta};
        is($meta->{args_as}, 'array', "new meta args_as");
    },
);
test_wrap(
    name => '(args_as=arrayref) convert args_as to hash',
    wrap_args => {sub => $sub, meta => $meta, convert=>{args_as=>'hash'}},
    wrap_status => 200,
    call_argsr => [a=>12, b=>3],
    call_res => 4,
    posttest => sub {
        my ($wrap_res, $call_res) = @_;
        my $meta = $wrap_res->[2]{meta};
        is($meta->{args_as}, 'hash', "new meta args_as");
    },
);
test_wrap(
    name => '(args_as=arrayref) convert args_as to hashref',
    wrap_args => {sub => $sub, meta => $meta, convert=>{args_as=>'hashref'}},
    wrap_status => 200,
    call_argsr => [{a=>12, b=>3}],
    call_res => 4,
    posttest => sub {
        my ($wrap_res, $call_res) = @_;
        my $meta = $wrap_res->[2]{meta};
        is($meta->{args_as}, 'hashref', "new meta args_as");
    },
);

$sub = sub { my %args = @_; $args{a}/$args{b} };
$meta = {v=>1.1, args_as=>"hash", args=>{a=>{pos=>0}, b=>{pos=>1}},
         result_naked => 1};
test_wrap(
    name => '(args_as=hash)',
    wrap_args => {sub => $sub, meta => $meta},
    wrap_status => 200,
    call_argsr => [a=>12, b=>3],
    call_res => 4,
);
test_wrap(
    name => '(args_as=hash) convert args_as to array',
    wrap_args => {sub => $sub, meta => $meta, convert=>{args_as=>'array'}},
    wrap_status => 200,
    call_argsr => [12, 3],
    call_res => 4,
    posttest => sub {
        my ($wrap_res, $call_res) = @_;
        my $meta = $wrap_res->[2]{meta};
        is($meta->{args_as}, 'array', "new meta args_as");
    },
);
test_wrap(
    name => '(args_as=hash) convert args_as to arrayref',
    wrap_args => {sub => $sub, meta => $meta, convert=>{args_as=>'arrayref'}},
    wrap_status => 200,
    call_argsr => [[12, 3]],
    call_res => 4,
    posttest => sub {
        my ($wrap_res, $call_res) = @_;
        my $meta = $wrap_res->[2]{meta};
        is($meta->{args_as}, 'arrayref', "new meta args_as");
    },
);
test_wrap(
    name => '(args_as=hash) convert args_as to hashref',
    wrap_args => {sub => $sub, meta => $meta, convert=>{args_as=>'hashref'}},
    wrap_status => 200,
    call_argsr => [{a=>12, b=>3}],
    call_res => 4,
    posttest => sub {
        my ($wrap_res, $call_res) = @_;
        my $meta = $wrap_res->[2]{meta};
        is($meta->{args_as}, 'hashref', "new meta args_as");
    },
);

$sub = sub { my $args = shift; $args->{a}/$args->{b} };
$meta = {v=>1.1, args_as=>"hashref", args=>{a=>{pos=>0}, b=>{pos=>1}},
         result_naked => 1};
test_wrap(
    name => '(args_as=hashref)',
    wrap_args => {sub => $sub, meta => $meta},
    wrap_status => 200,
    call_argsr => [{a=>12, b=>3}],
    call_res => 4,
);
test_wrap(
    name => '(args_as=hashref) convert args_as to array',
    wrap_args => {sub => $sub, meta => $meta, convert=>{args_as=>'array'}},
    wrap_status => 200,
    call_argsr => [12, 3],
    call_res => 4,
    posttest => sub {
        my ($wrap_res, $call_res) = @_;
        my $meta = $wrap_res->[2]{meta};
        is($meta->{args_as}, 'array', "new meta args_as");
    },
);
test_wrap(
    name => '(args_as=hashref) convert args_as to arrayref',
    wrap_args => {sub => $sub, meta => $meta, convert=>{args_as=>'arrayref'}},
    wrap_status => 200,
    call_argsr => [[12, 3]],
    call_res => 4,
    posttest => sub {
        my ($wrap_res, $call_res) = @_;
        my $meta = $wrap_res->[2]{meta};
        is($meta->{args_as}, 'arrayref', "new meta args_as");
    },
);
test_wrap(
    name => '(args_as=hashref) convert args_as to hash',
    wrap_args => {sub => $sub, meta => $meta, convert=>{args_as=>'hash'}},
    wrap_status => 200,
    call_argsr => [a=>12, b=>3],
    call_res => 4,
    posttest => sub {
        my ($wrap_res, $call_res) = @_;
        my $meta = $wrap_res->[2]{meta};
        is($meta->{args_as}, 'hash', "new meta args_as");
    },
);

$sub = sub { my %args = @_; $args{a}/sum(@{$args{b}}) };
$meta = {v=>1.1, args=>{a=>{pos=>0}, b=>{pos=>1, greedy=>1}},
         result_naked => 1};
my ($wrapped, $wrapped_meta);
test_wrap(
    name => '(args_as=hash, default) greedy, no conversion',
    wrap_args => {sub => $sub, meta => $meta},
    wrap_status => 200,
    call_argsr => [a=>12, b=>[1, 2]],
    call_res => 4,
);
test_wrap(
    name => '(args_as=hash) greedy, conversion to array',
    wrap_args => {sub => $sub, meta => $meta, convert=>{args_as=>'array'}},
    wrap_status => 200,
    call_argsr => [12, 1, 2],
    call_res => 4,
    posttest => sub {
        my ($wrap_res, $call_res) = @_;
        $wrapped = $wrap_res->[2]{sub};
        $wrapped_meta = $wrap_res->[2]{meta};
    },
);

# test blessed and double wrapping

ok(blessed($wrapped), 'generated wrapper is blessed');
ok(!blessed($sub), 'original input subroutine not blessed');

test_wrap(
    name => 'double wrapping, no conversion',
    wrap_args => {sub => $wrapped, meta => $wrapped_meta,
                  convert=>{}},
    wrap_status => 200,
    call_argsr => [12, 1, 2],
    call_res => 4,
);

# test wrapping 'args' property
$meta = {v=>1.1, args=>{a=>{foo=>1}}};
test_wrap(
    name => 'args: unknown arg spec key -> dies',
    wrap_args => {sub => $sub, meta => $meta},
    wrap_dies => 1,
);
$meta = {v=>1.1, args=>{a=>{_foo=>1}}};
test_wrap(
    name => 'args: arg spec key prefixed by _ is ignored',
    wrap_args => {sub => $sub, meta => $meta},
    wrap_status => 200,
);

# test wrapping 'deps' property

$sub = sub {[200,"OK"]};
$meta = {v=>1.1, args=>{}, deps=>{env=>"A"}};
{
    local $ENV{A};
    test_wrap(
        name => 'deps 1',
        wrap_args => {sub => $sub, meta => $meta},
        wrap_status => 200,
        call_argsr => [],
        call_status => 412,
    );
    $ENV{A} = 1;
    test_wrap(
        name => 'deps 2',
        wrap_args => {sub => $sub, meta => $meta},
        wrap_status => 200,
        call_argsr => [],
        call_status => 200,
    );

    # XXX test under trap=0
}

# test wrapping 'features' property

$sub = sub {[200,"OK"]};
$meta = {v=>1.1, features=>{tx=>{req=>1}}};
{
    test_wrap(
        name => 'deps 1',
        wrap_args => {sub => $sub, meta => $meta},
        wrap_status => 200,
        call_argsr => [],
        call_status => 412,
    );
    test_wrap(
        name => 'deps 1',
        wrap_args => {sub => $sub, meta => $meta},
        wrap_status => 200,
        call_argsr => [-tx_manager=>"dummy"],
        call_status => 200,
    );

    # XXX test under trap=0
}

$sub = sub { [200, "OK"] };
$meta = {
    v=>1.1,
    args=>{a=>{pos=>0, schema=>"int", _argspec1=>"internal"}, b=>{pos=>1}},
    _prop1=>"internal",
    result=>{_res1=>"1"},
    examples=>[{_ex1=>"1"}],
    links=>[{_ln1=>"1"}],
};
test_wrap(
    name => '(remove_internal_properties=1, default)',
    wrap_args => {sub => $sub, meta => $meta},
    wrap_status => 200,
    posttest => sub {
        my ($wrap_res, $call_res) = @_;
        my $newmeta = $wrap_res->[2]{meta};
        ok(!exists($newmeta->{_prop1}), "_prop1 removed");
        ok(!exists($newmeta->{args}{a}{_argspec1}), "_argspec1 removed");
        ok(!exists($newmeta->{result}{_res1}), "_res1 removed");
        ok(!exists($newmeta->{examples}[0]{_ex1}), "_ex1 removed");
        ok(!exists($newmeta->{links}[0]{_ln1}), "_ln1 removed");
    },
);
test_wrap(
    name => '(remove_internal_properties=0)',
    wrap_args => {sub => $sub, meta => $meta, remove_internal_properties=>0},
    wrap_status => 200,
    posttest => sub {
        my ($wrap_res, $call_res) = @_;
        my $newmeta = $wrap_res->[2]{meta};
        ok($newmeta->{_prop1}, "_prop1 exists");
        ok($newmeta->{args}{a}{_argspec1}, "_argspec1 exists");
        ok($newmeta->{result}{_res1}, "_res1 exists");
        ok($newmeta->{examples}[0]{_ex1}, "_ex1 exists");
        ok($newmeta->{links}[0]{_ln1}, "_ln1 exists");
    },
);

$meta = {
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
};
test_wrap(
    name => 'convert default_lang',
    wrap_args => {sub => $sub, meta => $meta, convert=>{default_lang=>"id_ID"}},
    wrap_status => 200,
    posttest => sub {
        my ($wrap_res, $call_res) = @_;
        my $newmeta = $wrap_res->[2]{meta};
        is_deeply($newmeta, $newmeta_expected, "newmeta")
            or diag explain $newmeta;
    },
);

DONE_TESTING:
done_testing();

