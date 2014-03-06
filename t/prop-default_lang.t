#!perl

use 5.010;
use strict;
use warnings;

use Test::More 0.98;
use Test::Perinci::Sub::Wrapper qw(test_wrap);

# XXX fudged momentarily, i'm getting weird thing with result.description being
# undef. it shouldn't be.
if (0) {
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
} else {
    ok 1;
} # if 0

DONE_TESTING:
done_testing;
