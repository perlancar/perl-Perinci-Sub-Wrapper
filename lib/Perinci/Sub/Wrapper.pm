package Perinci::Sub::Wrapper;

use 5.010001;
use strict;
use warnings;
use experimental 'smartmatch';
use Log::Any '$log';

use Function::Fallback::CoreOrPP qw(clone);
use Perinci::Sub::Normalize qw(normalize_function_metadata);
use Perinci::Sub::Util qw(err);

use Exporter qw(import);
our @EXPORT_OK = qw(wrap_sub);

our $Log_Wrapper_Code = $ENV{LOG_PERINCI_WRAPPER_CODE} // 0;

# VERSION
# DATE

our %SPEC;

# "protocol version" (v). whenever there's a significant change in the basic
# structure of the wrapper, which potentially cause some/a lot of property
# handlers to stop working, we increase this. property handler must always state
# which version it follows in its meta. if unspecified, it's assumed to be 1.
our $protocol_version = 2;

sub new {
    my ($class) = @_;
    bless {}, $class;
}

sub __squote {
    require Data::Dumper;
    my $res = Data::Dumper->new([shift])->
        Purity(1)->Terse(1)->Deepcopy(1)->Indent(0)->Dump;
    chomp $res;
    $res;
}

sub _add_module {
    my ($self, $mod) = @_;
    unless ($mod ~~ $self->{_modules}) {
        local $self->{_cur_section};
        $self->select_section('before_sub_require_modules');
        if ($mod =~ /(.+?)\s+(.+)/) {
            $self->push_lines("use $1 $2;");
        } else {
            $self->push_lines("require $mod;");
        }
        push @{ $self->{_modules} }, $mod;
    }
}

sub _add_var {
    my ($self, $var, $value) = @_;
    unless (exists $self->{_vars}{$var}) {
        local $self->{_cur_section};
        $self->select_section('declare_vars');
        $self->push_lines("my \$$var = ".__squote($value).";");
        $self->{_vars}{$var} = $value;
    }
}

sub _known_sections {

    # order=>N regulates the order of code. embed=>1 means the code is for embed
    # mode only and should not be included in dynamic wrapper code.

    state $val = {
        before_sub_require_modules => {order=>1},

        # reserved by wrapper for setting Perl package and declaring 'sub {'
        OPEN_SUB => {order=>4},

        # reserved to say 'my %args = @_;' or 'my @args = @_;' etc
        ACCEPT_ARGS => {order=>5},

        # reserved to get args values if converted from array/arrayref
        ACCEPT_ARGS2 => {order=>6},

        declare_vars => {order=>7},

        # for handlers to put stuffs right before eval. for example, 'timeout'
        # uses this to set ALRM signal handler.
        before_eval => {order=>10},

        # reserved by wrapper for generating 'eval {'
        OPEN_EVAL => {order=>20},

        # used e.g. to load modules used by validation
        before_call_before_arg_validation => {order=>31},

        before_call_arg_validation => {order=>32},

        # used e.g. by dependency checking
        before_call_after_arg_validation => {order=>33},

        # feed arguments to sub
        before_call_feed_args => {order=>48},

        # for handlers that *must* do stuffs right before call
        before_call_right_before_call => {order=>49},

        # reserved by the wrapper for calling the sub
        CALL => {order=>50},

        # for handlers that *must* do stuffs right after call
        after_call_right_after_call => {order=>51},

        # reserved by the wrapper for adding/stripping result envelope, this
        # happens before result validation
        AFTER_CALL_ADD_OR_STRIP_RESULT_ENVELOPE => {order=>52},

        # used e.g. to load modules used by validation
        after_call_before_res_validation => {order=>61},

        after_call_res_validation => {order=>62},

        after_call_after_res_validation => {order=>63},

        # reserved by wrapper to put eval end '}' and capturing result in
        # $_w_res and $@ in $eval_err
        CLOSE_EVAL => {order=>70},

        # for handlers to put checks against $eval_err
        after_eval => {order=>80},

        # reserved for returning final result '$_w_res'
        BEFORE_CLOSE_SUB => {order=>99},

        # reserved for sub closing '}' line
        CLOSE_SUB => {order=>100},
    };
    $val;
}

sub section_empty {
    my ($self, $section) = @_;
    !$self->{_codes}{$section};
}

sub _needs_eval {
    my ($self) = @_;
    !($self->section_empty('before_eval') &&
          $self->section_empty('after_eval'));
}

# whether we need to store call result to a variable ($_w_res)
sub _needs_store_res {
    my ($self) = @_;
    return 1 if $self->{_args}{validate_result};
    return 1 if $self->_needs_eval;
    my $ks = $self->_known_sections;
    for (grep {/^after_call/} keys %$ks) {
        return 1 unless $self->section_empty($_);
    }
    0;
}

sub _check_known_section {
    my ($self, $section) = @_;
    my $ks = $self->_known_sections;
    $ks->{$section} or die "BUG: Unknown code section '$section'";
}

sub _err {
    my ($self, $c_status, $c_msg) = @_;
    if ($self->{_meta}{result_naked}) {
        $self->push_lines(
            "warn 'ERROR ' . ($c_status) . ': '. ($c_msg);",
            'return undef;',
        );
    } else {
        $self->push_lines("return [$c_status, $c_msg];");
    }
}

sub _errif {
    my ($self, $c_status, $c_msg, $c_cond) = @_;
    $self->push_lines("if ($c_cond) {");
    $self->indent;
    $self->_err($c_status, $c_msg);
    $self->unindent;
    $self->push_lines('}');
}

sub select_section {
    my ($self, $section) = @_;
    $self->_check_known_section($section);
    $self->{_cur_section} = $section;
    $self;
}

sub indent {
    my ($self) = @_;
    my $section = $self->{_cur_section};
    $self->{_codes}{$section} //= undef;
    $self->{_levels}{$section}++;
    $self;
}

sub unindent {
    my ($self) = @_;
    my $section = $self->{_cur_section};
    $self->{_codes}{$section} //= undef;
    $self->{_levels}{$section}--;
    $self;
}

sub get_indent_level {
    my ($self) = @_;
    my $section = $self->{_cur_section};
    $self->{_levels}{$section} // 0;
}

# line can be code or comment. code should not contain string literals that
# cross lines (i.e. contain literal newlines) because push_lines() might add
# comment at the end of each line.

sub push_lines {
    my ($self, @lines) = @_;
    my $section = $self->{_cur_section};

    unless (exists $self->{_codes}{$section}) {
        unshift @lines, "# * section: $section";
        # don't give blank line for the top-most section (order=>0)
        unshift @lines, "" if $self->_known_sections->{$section}{order};
        $self->{_codes}{$section} = [];
        $self->{_levels}{$section} = 0;
    }

    @lines = map {[$self->{_levels}{$section}, $_]} @lines;
    if ($self->{_args}{debug}) {
        for my $l (@lines) {
            $l->[2] =
                $self->{_cur_handler} ?
                    "$self->{_cur_handler} p=".$self->{_cur_handler_meta}{prio}
                        : "";
        }
    }
    push @{$self->{_codes}{$section}}, @lines;
    $self;
}

sub _join_codes {
    my ($self, $crit, $prev_section_level) = @_;
    my @lines;
    my $ks = $self->_known_sections;
    $prev_section_level //= 0;
    my $i = 0;
    for my $s (sort {$ks->{$a}{order} <=> $ks->{$b}{order}}
                   keys %$ks) {
        next if $self->section_empty($s);
        next unless $crit->(section => $s);
        $i++;
        for my $l (@{ $self->{_codes}{$s} }) {
            $l->[0] += $prev_section_level;
            die "BUG: Negative indent level in line $i (section $s): '$l->[1]'"
                if $l->[0] < 0;
            my $s = ($self->{_args}{indent} x $l->[0]) . $l->[1];
            if (defined $l->[2]) {
                my $num_ws = 80 - length($s);
                $num_ws = 1 if $num_ws < 1;
                $s .= (" " x $num_ws) . "# $l->[2]";
            }
            push @lines, $s;
        }
        $prev_section_level += $self->{_levels}{$s};
    }
    [join("\n", @lines), $prev_section_level];
}

sub _format_dyn_wrapper_code {
    my ($self) = @_;
    my $ks = $self->_known_sections;
    $self->_join_codes(
        sub {
            my %args = @_;
            my $section = $args{section};
            !$ks->{$section}{embed};
        })->[0];
}

# for embedded, we need to produce three sections which will be inserted in
# different places, demonstrated below:
#
#   $SPEC{foo} = {
#       ...
#   };
#   sub foo {
#       my %args = @_;
#       # do stuffs
#   }
#
# becomes:
#
#   #PRESUB1: require modules (inserted before sub declaration)
#   require Data::Dumper;
#   require Scalar::Util;
#
#   $SPEC{foo} = {
#       ...
#   };
#   #PRESUB2: modify metadata piece-by-piece (inserted before sub declaration &
#   #after $SPEC{foo}). we're avoiding dumping the new modified metadata because
#   #metadata might contain coderefs which is sometimes problematic when dumping
#   {
#       my $meta = $SPEC{foo};
#       $meta->{v} = 1.1;
#       $meta->{result_naked} = 0;
#   }
#
#   sub foo {
#       my %args = @_;
#       #PREAMBLE: before call sections (inserted after accept args), e.g.
#       #validate arguments, convert argument type, setup eval block
#       #...
#
#       # do stuffs
#
#       #POSTAMBLE: after call sections (inserted before sub end), e.g.
#       #validate result, close eval block and do retry/etc.
#       #...
#   }
sub _format_embed_wrapper_code {
    my ($self) = @_;

    my $res = {};
    my $ks = $self->_known_sections;
    my $j;

    $j = $self->_join_codes(
        sub {
            my %args = @_;
            my $section = $args{section};
            $section =~ /\A(before_sub_require_modules)\z/;
        });
    $res->{presub1} = $j->[0];

    # no longer needed/produce, code to modify metadata
    $res->{presub2} = '';

    $j = $self->_join_codes(
        sub {
            my %args = @_;
            my $section = $args{section};
            my $order = $ks->{$section}{order};
            return 1 if $order > $ks->{ACCEPT_ARGS}{order} &&
                $order < $ks->{CALL}{order};
            0;
        }, 1);
    $res->{preamble} = $j->[0];

    $j = $self->_join_codes(
        sub {
            my %args = @_;
            my $section = $args{section};
            my $order = $ks->{$section}{order};
            return 1 if $order > $ks->{CALL}{order} &&
                $order < $ks->{CLOSE_SUB}{order};
            0;
        }, $j->[1]);
    $res->{postamble} = $j->[0];

    $res;
}

sub handlemeta_v { {} }
sub handlemeta_name { {} }
sub handlemeta_summary { {} }
sub handlemeta_description { {} }
sub handlemeta_tags { {} }
sub handlemeta_default_lang { {} }
sub handlemeta_links { {} }
sub handlemeta_text_markup { {} }
sub handlemeta_is_func { {} }
sub handlemeta_is_meth { {} }
sub handlemeta_is_class_meth { {} }
sub handlemeta_examples { {} }

# after args
sub handlemeta_features { {v=>2, prio=>15} }
sub handle_features {
    my ($self, %args) = @_;

    my $meta = $self->{_meta};
    my $v = $meta->{features} // {};

    $self->select_section('before_call_before_arg_validation');

    if ($v->{tx} && $v->{tx}{req}) {
        $self->push_lines('', '# check required transaction');
        $self->_errif(412, '"Must run with transaction (pass -tx_manager)"',
                      '!$args{-tx_manager}');
    }
}

# run before args
sub handlemeta_args_as { {v=>2, prio=>1, convert=>1} }
sub handle_args_as {
    my ($self, %args) = @_;

    my $value  = $args{value};
    my $new    = $args{new};
    my $meta   = $self->{_meta};
    my $args_p = $meta->{args} // {};
    my $opt_va = $self->{_args}{validate_args};

    # We support conversion of arguments between hash/hashref/array/arrayref. To
    # make it simple, currently the algorithm is as follow: we first form the
    # %args hash. If args_as is already 'hash', we just do 'my %args = @_'.
    # Otherwise, we convert from the other forms.
    #
    # We then validate each argument in %args (code generated in 'args'
    # handler).
    #
    # Finally, unless original args_as is 'hash' we convert to the final form
    # that the wrapped sub expects.
    #
    # This setup is optimal when both the sub and generated wrapper accept
    # 'hash', but suboptimal for other cases (especially positional ones, as
    # they have to undergo a round-trip to hash even when both accept 'array').
    # This will be rectified in the future.

    my $v = $new // $value;

    $self->select_section('ACCEPT_ARGS');
    if ($v eq 'hash') {
         $self->push_lines(q{die 'BUG: Odd number of hash elements supplied' if @_ % 2;})
             if $opt_va;
         $self->push_lines('my %args = @_;');
    } elsif ($v eq 'hashref') {
        $self->push_lines(q{die 'BUG: $_[0] needs to be hashref' if @_ && ref($_[0]) ne "HASH";})
            if $opt_va;
        $self->push_lines('my %args = %{$_[0] // {}};');
    } elsif ($v =~ /\Aarray(ref)?\z/) {
        my $ref = $1 ? 1:0;
        if ($ref) {
            $self->push_lines(q{die 'BUG: $_[0] needs to be arrayref' if @_ && ref($_[0]) ne "ARRAY";})
                if $opt_va;
        }
        $self->push_lines('my %args;');
        $self->select_section('ACCEPT_ARGS2');
        for my $a (sort keys %$args_p) {
            my $as = $args_p->{$a};
            my $line = '$args{'.__squote($a).'} = ';
            defined($as->{pos}) or die "Error in args property for arg '$a': ".
                "no pos defined";
            my $pos = int($as->{pos} + 0);
            $pos >= 0 or die "Error in args property for arg '$a': ".
                "negative value in pos";
            if ($as->{greedy}) {
                if ($ref) {
                    $line .= '[splice @{$_[0]}, '.$pos.'] if @{$_[0]} > '.$pos;
                } else {
                    $line .= '[splice @_, '.$pos.'] if @_ > '.$pos;
                }
            } else {
                if ($ref) {
                    $line .= '$_[0]['.$pos.'] if @{$_[0]} > '.$pos;
                } else {
                    $line .= '$_['.$pos.'] if @_ > '.$pos;
                }
            }
            $self->push_lines("$line;");
        }
    } else {
        die "Unknown args_as value '$v'";
    }

    $self->select_section('ACCEPT_ARGS');
    if ($value eq 'hashref') {
        $self->push_lines('my $args;');
    } elsif ($value eq 'array') {
        $self->push_lines('my @args;');
    } elsif ($value eq 'arrayref') {
        $self->push_lines('my $args;');
    }

    my $tok;
    $self->select_section('before_call_feed_args');
    $v = $value;
    if ($v eq 'hash') {
        $tok = '%args';
    } elsif ($v eq 'hashref') {
        $tok = '$args';
        $self->push_lines($tok.' = \%args;'); # XXX should we set each arg instead?
    } elsif ($v =~ /\Aarray(ref)?\z/) {
        my $ref = $1 ? 1:0;
        $tok = ($ref ? '$':'@') . 'args';
        for my $a (sort {$args_p->{$a}{pos} <=> $args_p->{$b}{pos}}
                       keys %$args_p) {
            my $as = $args_p->{$a};
            my $t = '$args{'.__squote($a).'}';
            my $line;
            defined($as->{pos}) or die "Error in args property for arg '$a': ".
                "no pos defined";
            my $pos = int($as->{pos} + 0);
            $pos >= 0 or die "Error in args property for arg '$a': ".
                "negative value in pos";
            if ($as->{greedy}) {
                $line = 'splice @args, '.$pos.', scalar(@args)-1, @{'.$t.'}';
            } else {
                $line = '$args'.($ref ? '->':'').'['.$pos."] = $t if exists $t";
            }
            $self->push_lines("$line;");
        }
    } else {
        die "Unknown args_as value '$v'";
    }
    $self->{_args_token} = $tok;
}

sub _sah {
    require Data::Sah;

    my $self = shift;
    state $sah = Data::Sah->new;
    $sah;
}

sub _plc {
    my $self = shift;
    state $plc = $self->_sah->get_compiler("perl");
}

sub handlemeta_args { {v=>2, prio=>10} }
sub handle_args {
    my ($self, %args) = @_;

    my $v = $self->{_meta}{args};
    return unless $v;

    my $opt_sin = $self->{_args}{_schema_is_normalized};
    my $opt_va  = $self->{_args}{validate_args};

    if ($opt_va) {
        $self->_add_module("experimental 'smartmatch'");
        $self->select_section('before_call_arg_validation');
        $self->push_lines('', '# check args');
        $self->push_lines('for (sort keys %args) {');
        $self->indent;
        $self->_errif(400, q["Invalid argument name '$_'"],
                      '!/\A(-?)\w+(\.\w+)*\z/o');
        $self->_errif(400, q["Unknown argument '$_'"],
                      '!($1 || $_ ~~ '.__squote([sort keys %$v]).')');
        $self->unindent;
        $self->push_lines('}');
    }

    for my $argname (sort keys %$v) {
        my $argspec = $v->{$argname};

        my $argterm = "\$args{'$argname'}";

        my $sch = $argspec->{schema};
        if ($sch) {
            my $has_default_prop = exists($argspec->{default});
            my $has_sch_default  = ref($sch) eq 'ARRAY' &&
                exists($sch->[1]{default}) ? 1:0;
            if ($opt_va) {
                my $dn = $argname; $dn =~ s/\W+/_/g;
                my $cd = $self->_plc->compile(
                    data_name            => $dn,
                    data_term            => $argterm,
                    schema               => $sch,
                    schema_is_normalized => $opt_sin,
                    return_type          => 'str',
                    indent_level         => $self->get_indent_level + 4,
                    %{ $self->{_args}{_extra_sah_compiler_args} // {}},
                );
                $self->_add_module($_) for @{ $cd->{modules} };
                $self->_add_var($_, $cd->{vars}{$_})
                    for sort keys %{ $cd->{vars} };
                $self->push_lines("if (exists($argterm)) {");
                $self->indent;
                $self->push_lines("my \$err_$dn;\n$cd->{result};");
                $self->_errif(
                    400, qq["Invalid value for argument '$argname': \$err_$dn"],
                    "\$err_$dn");
                $self->unindent;
                if ($has_default_prop) {
                    $self->push_lines(
                        '} else {',
                        "    $argterm //= ".__squote($argspec->{default}).";");
                } elsif ($has_sch_default) {
                    $self->push_lines(
                        '} else {',
                        "    $argterm //= ".__squote($sch->[1]{default}).";");
                }
                $self->push_lines('}');
            }
        }
        if ($argspec->{req} && $opt_va) {
            $self->_errif(
                400, qq["Missing required argument: $argname"],
                "!exists($argterm)");
        }
    }
}

sub handlemeta_result { {v=>2, prio=>50} }
sub handle_result {
    require Data::Sah;

    my ($self, %args) = @_;

    my $meta = $self->{_meta};
    my $v = $meta->{result};
    return unless $v;

    my $opt_sin = $self->{_args}{_schema_is_normalized};
    my $opt_vr  = $self->{_args}{validate_result};

    my %ss; # key = status, value = schema

    # collect and check handlers
    my %handler_args;
    my %handler_metas;
    for my $k0 (keys %$v) {
        my $k = $k0;
        $k =~ s/\..+//;
        next if $k =~ /\A_/;

        # check builtin result spec key
        next if $k =~ /\A(
                           summary|description|tags|default_lang|
                           schema|
                           x
                       )\z/x;
        # try a property module first
        require "Perinci/Sub/Property/result/$k.pm";
        my $meth = "handlemeta_result__$k";
        unless ($self->can($meth)) {
            die "No handler for property result/$k0 ($meth)";
        }
        my $hm = $self->$meth;
        $hm->{v} //= 1;
        next unless defined $hm->{prio};
        die "Please update property handler result/$k which is still at v=$hm->{v} ".
            "(needs v=$protocol_version)"
                unless $hm->{v} == $protocol_version;
        my $ha = {
            prio=>$hm->{prio}, value=>$v->{$k0}, property=>$k0,
            meth=>"handle_result__$k",
        };
        $handler_args{$k} = $ha;
        $handler_metas{$k} = $hm;
    }

    # call all the handlers in order
    for my $k (sort {$handler_args{$a}{prio} <=> $handler_args{$b}{prio}}
                   keys %handler_args) {
        my $ha = $handler_args{$k};
        my $meth = $ha->{meth};
        local $self->{_cur_handler}      = $meth;
        local $self->{_cur_handler_meta} = $handler_metas{$k};
        local $self->{_cur_handler_args} = $ha;
        $self->$meth(args=>\%args, meta=>$meta, %$ha);
    }

    # validate result
    my @modules;
    if ($v->{schema} && $opt_vr) {
        $ss{200} = $v->{schema};
    }
    if ($v->{statuses} && $opt_vr) {
        for my $s (keys %{$v->{statuses}}) {
            my $sv = $v->{statuses}{$s};
            if ($sv->{schema}) {
                $ss{$s} = $sv->{schema};
            }
        }
    }

    my $sub_name = $self->{_args}{sub_name};
    if (keys %ss) {
        $self->select_section('after_call_res_validation');
        $self->push_lines("my \$_w_res2 = \$_w_res->[2];");
        $self->push_lines("my \$_w_err2_res;");

        for my $s (sort keys %ss) {
            my $sch = $ss{$s};
            my $cd = $self->_plc->compile(
                data_name            => '_w_res2',
                # err_res can clash on arg named 'res'
                err_term             => '$_w_err2_res',
                schema               => $sch,
                schema_is_normalized => $opt_sin,
                return_type          => 'str',
                indent_level         => $self->get_indent_level + 4,
                %{ $self->{_args}{_extra_sah_compiler_args} // {}},
            );
            $self->_add_module($_) for @{ $cd->{modules} };
            $self->_add_var($_, $cd->{vars}{$_})
                for sort keys %{ $cd->{vars} };
            $self->push_lines("if (\$_w_res->[0] == $s) {");
            $self->indent;
            $self->push_lines("$cd->{result};");
            $self->_errif(
                500,
                qq["BUG: Sub $sub_name produces invalid result (status=$s): ].
                    qq[\$_w_err2_res"],
                "\$_w_err2_res");
            $self->unindent;
            $self->push_lines("}");
        }
    }
}

sub handlemeta_result_naked { {v=>2, prio=>99, convert=>1} }
sub handle_result_naked {
    my ($self, %args) = @_;

    my $old = $args{value};
    my $v   = $args{new} // $old;

    return if !!$v == !!$old;

    $self->select_section('AFTER_CALL_ADD_OR_STRIP_RESULT_ENVELOPE');
    if ($v) {
        $self->push_lines(
            '', '# strip result envelope',
            '$_w_res = $_w_res->[2];',
        );
    } else {
        $self->push_lines(
            '', '# add result envelope',
            '$_w_res = [200, "OK", $_w_res];',
        );
    }
}

sub handlemeta_deps { {v=>2, prio=>0.5} }
sub handle_deps {
    my ($self, %args) = @_;
    my $value = $args{value};
    my $meta  = $self->{_meta};
    my $v     = $self->{_args}{meta_name};
    $self->select_section('before_call_after_arg_validation');
    $self->push_lines('', '# check dependencies');
    $self->_add_module("Perinci::Sub::DepChecker");
    #$self->push_lines('use Data::Dump; dd '.$v.';');
    $self->push_lines('my $_w_deps_res = Perinci::Sub::DepChecker::check_deps('.
                          $v.'->{deps});');
    $self->_errif(412, '"Deps failed: $_w_deps_res"', '$_w_deps_res');

    # we handle some deps our own
    if ($value->{tmp_dir}) {
        $self->_errif(412, '"Dep failed: please specify -tmp_dir"',
                      '!$args{-tmp_dir}');
    }
    if ($value->{trash_dir}) {
        $self->_errif(412, '"Dep failed: please specify -trash_dir"',
                      '!$args{-trash_dir}');
    }
    if ($value->{undo_trash_dir}) {
        $self->_errif(412, '"Dep failed: please specify -undo_trash_dir"',
                      '!($args{-undo_trash_dir} || $args{-tx_manager} || '.
                          '$args{-undo_action} && $args{-undo_action}=~/\A(?:undo|redo)\z/)');
    }
}

sub handlemeta_x { {} }
sub handlemeta_entity_v { {} }

sub _reset_work_data {
    my ($self, %args) = @_;

    # to make it stand out more, all work/state data is prefixed with
    # underscore.

    $self->{_cur_section}      = undef;
    $self->{_cur_handler}      = undef;
    $self->{_cur_handler_args} = undef;
    $self->{_cur_handler_meta} = undef;
    $self->{_levels}           = {};
    $self->{_codes}            = {};
    $self->{_modules}          = []; # modules loaded by wrapper sub
    $self->{$_} = $args{$_} for keys %args;
}

sub wrap {
    require Scalar::Util;

    my ($self, %args) = @_;

    my $wrap_log_prop = "x.perinci.sub.wrapper.logs";

    # required arguments
    my $sub      = $args{sub};
    my $sub_name = $args{sub_name};
    $sub || $sub_name or return [400, "Please specify sub or sub_name"];
    $args{meta} or return [400, "Please specify meta"];
    my $meta_name = $args{meta_name};
    # we clone the meta because we'll replace stuffs
    my $meta     = clone($args{meta});
    my $wrap_logs = $meta->{$wrap_log_prop} // [];

    # currently internal args, not exposed/documented
    $args{_compiled_package}           //= 'Perinci::Sub::Wrapped';
    my $comppkg  = $args{_compiled_package};
    $args{_schema_is_normalized}       //=
        $wrap_logs->[-1] && $wrap_logs->[-1]{normalize_schema} ? 1 : 0;
    $args{_embed}                      //= 0;
    $args{_extra_sah_compiler_args}    //= undef;

    # defaults for arguments
    $args{indent}                      //= " " x 4;
    $args{convert}                     //= {};
    $args{compile}                     //= 1;
    $args{log}                         //= 1;
    $args{validate_args}               //= 0
        # function might want to disable validate_args by default, e.g. if
        # source code has been processed with
        # Dist::Zilla::Plugin::Rinci::Validate
        if $meta->{'x.perinci.sub.wrapper.disable_validate_args'};
    $args{validate_args}               //= 0
        # by default do not validate args again if previous wrapper(s) have
        # already done it
        if (grep {$_->{validate_args}} @$wrap_logs);
    $args{validate_args}               //= 1;
    $args{validate_result}             //= 0
        # function might want to disable validate_result by default, e.g. if
        # source code has been processed with
        # Dist::Zilla::Plugin::Rinci::Validate
        if $meta->{'x.perinci.sub.wrapper.disable_validate_result'};
    $args{validate_result}             //= 0
        # by default do not validate result again if previous wrapper(s) have
        # already done it
        if (grep {$_->{validate_result}} @$wrap_logs);
    $args{validate_result}             //= 1;

    my $sub_ref_name;
    # if sub_name is not provided, create a unique name for it. it is needed by
    # the wrapper-generated code (e.g. printing error messages)
    if (!$sub_name || $sub) {
        my $n = $comppkg . "::sub".Scalar::Util::refaddr($sub);
        no strict 'refs'; no warnings; ${$n} = $sub;
        use experimental 'smartmatch';
        if (!$sub_name) {
            $args{sub_name} = $sub_name = '$' . $n;
        }
        $sub_ref_name = '$' . $n;
    }
    # if meta name is not provided, we store the meta somewhere, it is needed by
    # the wrapper-generated code (e.g. deps clause).
    if (!$meta_name) {
        my $n = $comppkg . "::meta".Scalar::Util::refaddr($meta);
        no strict 'refs'; no warnings; ${$n} = $meta;
        use experimental 'smartmatch';
        $args{meta_name} = $meta_name = '$' . $n;
    }

    # shallow copy
    my $opt_cvt = { %{ $args{convert} } };
    my $opt_sin = $args{_schema_is_normalized};

    $meta = normalize_function_metadata($meta)
        unless $opt_sin;

    $self->_reset_work_data(_args=>\%args, _meta=>$meta);

    # add properties from convert, if not yet mentioned in meta
    for (keys %$opt_cvt) {
        $meta->{$_} = undef unless exists $meta->{$_};
    }

    # mark in the metadata that we have done the wrapping, so future wrapping
    # can avoid needless duplicated functionality (like validating args twice).
    # note that handler can log their mark too.
    {
        my @wrap_log = @{ $meta->{$wrap_log_prop} // [] };
        push @wrap_log, {
            validate_args     => $args{validate_args},
            validate_result   => $args{validate_result},
            normalize_schema  => !$opt_sin,
        };
        if ($args{log}) {
            $meta->{$wrap_log_prop} = \@wrap_log;
        }
    }

    # start iterating over properties

    $self->select_section('OPEN_SUB');
    $self->push_lines(
        "package $comppkg;", 'sub {');
    $self->indent;

    $meta->{args_as} //= "hash";

    if ($meta->{args_as} =~ /hash/) {
        $self->select_section('before_call_after_arg_validation');
        # tell function it's being wrapped, currently disabled
        #$self->push_lines('$args{-wrapped} = 1;');
    }

    my %props = map {$_=>1} keys %$meta;
    $props{$_} = 1 for keys %$opt_cvt;

    # collect and check handlers
    my %handler_args;
    my %handler_metas;
    for my $k0 (keys %props) {
        my $k = $k0;
        $k =~ s/\..+//;
        next if $k =~ /\A_/;
        next if $handler_args{$k};
        #if ($k ~~ $self->{_args}{skip}) {
        #    $log->tracef("Skipped property %s (mentioned in skip)", $k);
        #    next;
        #}
        return [500, "Invalid property name $k"] unless $k =~ /\A\w+\z/;
        my $meth = "handlemeta_$k";
        unless ($self->can($meth)) {
            # try a property module first
            require "Perinci/Sub/Property/$k.pm";
            unless ($self->can($meth)) {
                return [500, "No handler for property $k0 ($meth)"];
            }
        }
        my $hm = $self->$meth;
        $hm->{v} //= 1;
        next unless defined $hm->{prio};
        die "Please update property handler $k which is still at v=$hm->{v} ".
            "(needs v=$protocol_version)"
                unless $hm->{v} == $protocol_version;
        my $ha = {
            prio=>$hm->{prio}, value=>$meta->{$k0}, property=>$k0,
            meth=>"handle_$k",
        };
        if (exists $opt_cvt->{$k0}) {
            return [502, "Property '$k0' does not support conversion"]
                unless $hm->{convert};
            $ha->{new}   = $opt_cvt->{$k0};
            $meta->{$k0} = $opt_cvt->{$k0};
        }
        $handler_args{$k}  = $ha;
        $handler_metas{$k} = $hm;
    }

    # call all the handlers in order
    for my $k (sort {$handler_args{$a}{prio} <=> $handler_args{$b}{prio}}
                   keys %handler_args) {
        my $ha = $handler_args{$k};
        my $meth = $ha->{meth};
        local $self->{_cur_handler}      = $meth;
        local $self->{_cur_handler_meta} = $handler_metas{$k};
        local $self->{_cur_handler_args} = $ha;
        $self->$meth(args=>\%args, meta=>$meta, %$ha);
    }

    my $needs_store_res = $self->_needs_store_res;
    if ($needs_store_res) {
        $self->_add_var('_w_res');
    }

    $self->select_section('CALL');
    my $sn = $sub_ref_name // $sub_name;
    $self->push_lines(
        ($needs_store_res ? '$_w_res = ' : "") .
        $sn. ($sn =~ /^\$/ ? "->" : "").
            "(".$self->{_args_token}.");");
    if ($args{validate_result}) {
        $self->select_section('after_call_before_res_validation');
        unless ($meta->{result_naked}) {
            $self->push_lines(
                '',
                '# check that sub produces enveloped result',
                'unless (ref($_w_res) eq "ARRAY" && $_w_res->[0]) {',
            );
            $self->indent;
            if ($log->is_trace) {
                $self->_add_module('Data::Dumper');
                $self->push_lines(
                    'local $Data::Dumper::Purity   = 1;',
                    'local $Data::Dumper::Terse    = 1;',
                    'local $Data::Dumper::Indent   = 0;',
                );
                $self->_err(500,
                            qq['BUG: Sub $sub_name does not produce envelope: '.].
                                qq[Data::Dumper::Dumper(\$_w_res)]);
            } else {
                $self->_err(500,
                            qq['BUG: Sub $sub_name does not produce envelope']);
            }
            $self->unindent;
            $self->push_lines('}');
        }
    }

    my $use_eval = $self->_needs_eval;
    if ($use_eval) {
        $self->select_section('CLOSE_EVAL');
        $self->push_lines('return $_w_res;');
        $self->unindent;
        $self->_add_var('_w_eval_err');
        $self->push_lines(
            '};',
            '$_w_eval_err = $@;');

        # _needs_eval will automatically be enabled here, due after_eval being
        # filled
        $self->select_section('after_eval');
        $self->push_lines('warn $_w_eval_err if $_w_eval_err;');
        $self->_errif(500, '"Function died: $_w_eval_err"', '$_w_eval_err');

        $self->select_section('OPEN_EVAL');
        $self->push_lines('eval {');
        $self->indent;
    }

    # return sub result
    $self->select_section('BEFORE_CLOSE_SUB');
    $self->push_lines('return $_w_res;') if $needs_store_res;
    $self->select_section('CLOSE_SUB');
    $self->unindent;
    $self->push_lines('}'); # wrapper sub

    # return wrap result
    my $result = {
        sub_name     => $sub_name,
        sub_ref_name => $sub_ref_name,
        meta         => $meta,
        meta_name    => $meta_name,
        use_eval     => $use_eval,
    };
    if ($args{embed}) {
        $result->{source} = $self->_format_embed_wrapper_code;
    } else {
        my $source = $self->_format_dyn_wrapper_code;
        if ($Log_Wrapper_Code && $log->is_trace) {
            require SHARYANTO::String::Util;
            $log->tracef("wrapper code:\n%s",
                         $ENV{LINENUM} // 1 ?
                             SHARYANTO::String::Util::linenum($source) :
                                   $source);
        }
        $result->{source} = $source;
        if ($args{compile}) {
            my $wrapped = eval $source;
            die "BUG: Wrapper code can't be compiled: $@" if $@ || !$wrapped;
            $result->{sub}  = $wrapped;
        }
    }

    [200, "OK", $result];
}

$SPEC{wrap_sub} = {
    v => 1.1,
    summary => 'Wrap subroutine to do various things, '.
        'like enforcing Rinci properties',
    result => {
        summary => 'The wrapped subroutine along with its new metadata',
        description => <<'_',

Aside from wrapping the subroutine, the wrapper will also create a new metadata
for the subroutine. The new metadata is a clone of the original, with some
properties changed, e.g. schema in `args` and `result` normalized, some values
changed according to the `convert` argument, some defaults set, etc.

The new metadata will also contain (or append) the wrapping log located in the
`x.perinci.sub.wrapper.logs` attribute. The wrapping log marks that the wrapper
has added some functionality (like validating arguments or result) so that
future nested wrapper can choose to avoid duplicating the same functionality.

_
        schema=>['hash*'=>{keys=>{
            sub=>'code*',
            source=>['any*' => of => ['str*', ['hash*' => each_value=>'str*']]],
            meta=>'hash*',
        }}],
    },
    args => {
        sub => {
            schema => 'str*',
            summary => 'The code to be wrapped',
            description => <<'_',

At least one of `sub` or `sub_name` must be specified.

_
        },
        sub_name => {
            schema => 'str*',
            summary => 'The name of the subroutine, '.
                'e.g. func or Foo::func (qualified)',
            description => <<'_',

At least one of `sub` or `sub_name` must be specified.

_
        },
        meta => {
            schema => 'hash*',
            summary => 'The function metadata',
            req => 1,
        },
        meta_name => {
            schema => 'str*',
            summary => 'Where to find the metadata, e.g. "$SPEC{foo}"',
            description => <<'_',

Some wrapper code (e.g. handler for `dep` property) needs to refer to the
function metadata. If not provided, the wrapper will store the function metadata
in a unique variable (e.g. `$Perinci::Sub::Wrapped::meta34127816`).

_
        },
        convert => {
            schema => 'hash*',
            summary => 'Properties to convert to new value',
            description => <<'_',

Not all properties can be converted, but these are a partial list of those that
can: v (usually do not need to be specified when converting from 1.0 to 1.1,
will be done automatically), args_as, result_naked, default_lang.

_
        },
        compile => {
            schema => ['bool' => {default=>1}],
            summary => 'Whether to compile the generated wrapper',
            description => <<'_',

Can be set to 0 to not actually wrap but just return the generated wrapper
source code.

_
        },
        compile => {
            schema => ['bool' => {default=>1}],
            summary => 'Whether to compile the generated wrapper',
            description => <<'_',

Can be set to 0 to not actually wrap but just return the generated wrapper
source code.

_
        },
        debug => {
            schema => [bool => {default=>0}],
            summary => 'Generate code with debugging',
            description => <<'_',

If turned on, will produce various debugging in the generated code. Currently
what this does:

* add more comments (e.g. for each property handler)

_
        },
        validate_args => {
            schema => ['bool'],
            summary => 'Whether wrapper should validate arguments',
            description => <<'_',

If set to true, will validate arguments. Validation error will cause status 400
to be returned. The default is to enable this unless previous wrapper(s) have
already done this.

_
        },
        validate_result => {
            schema => ['bool'],
            summary => 'Whether wrapper should validate arguments',
            description => <<'_',

If set to true, will validate sub's result. Validation error will cause wrapper
to return status 500 instead of sub's result. The default is to enable this
unless previous wrapper(s) have already done this.

_
        },
    },
};
sub wrap_sub {
    __PACKAGE__->new->wrap(@_);
}

1;
# ABSTRACT: A multi-purpose subroutine wrapping framework

=for Pod::Coverage ^(new|handle(meta)?_.+|wrap|add_.+|section_empty|indent|unindent|get_indent_level|select_section|push_lines)$

=head1 SYNOPSIS

For dynamic usage:

 use Perinci::Sub::Wrapper qw(wrap_sub);
 my $res = wrap_sub(sub_name => "mysub", meta=>{...});
 my ($wrapped_sub, $meta) = ($res->[2]{sub}, $res->[2]{meta});
 $wrapped_sub->(); # call the wrapped function


=head1 DESCRIPTION

Perinci::Sub::Wrapper (PSW for short) is an extensible subroutine wrapping
framework. It generates code to do stuffs before calling your subroutine, like
validate arguments, convert arguments from positional/array to named/hash or
vice versa, etc; as well as generate code to do stuffs after calling your
subroutine, like retry calling for a number of times if subroutine returns a
non-success status, check subroutine result against a schema, etc). Some other
things it can do: apply a timeout, currying, and so on.

PSW differs from other function composition or decoration system like Python
decorators (or its Perl equivalent L<Python::Decorator>) in a couple of ways:

=over

=item * Single wrapper

Instead of multiple/nested wrapping for implementing different features, PSW
is designed to generate a single large wrapper around your code, i.e.:

 sub _wrapper_for_your_sub {
     ...
     # do various stuffs before calling:

     # e.g. start timer
     # e.g. convert, prefill, validate arguments
     my @args = ...;
     ...
     your_sub(@args);
     ...
     # do various stuffs after calling
     ...
     # e.g. report times
     # e.g. perform retry
     # e.g. convert or envelope results

     # return result
 }

Multiple functionalities will be added and combined in this single wrapper
subroutine in the appropriate location. This is done to reduce function call
overhead or depth of nested call levels. And also to make it easier to embed the
wrapping code to your source code (see L<Dist::Zilla::Plugin::Rinci::Wrap>).

Of course, you can still wrap multiple times if wanted.

=item * Rinci

The wrapper code is built according to the L<Rinci> metadata you provide. Rinci
allows you to specify various things for your function, e.g. list of arguments
including the expected data type of each argument and whether an argument is
required or optional. PSW can then be used to generate the necessary code to
enforce this specification, e.g. generate validator for the function arguments.

Since Rinci specification is extensible, you can describe additional stuffs for
your function and write a PSW plugin to generate the necessary code to implement
your specification. An example is C<timeout> to specify execution time limit,
implemented by L<Perinci::Sub::Property::timeout> which generates code to call
function inside an C<eval()> block and use C<alarm()> to limit the execution.
Another example is C<retry> property, implemented by
L<Perinci::Sub::Property::retry> which generates code to call function inside a
simple retry loop.

=back

Normally you do not use PSW directly in your applications. You might want to
check out L<Perinci::Access::Perl> and L<Perinci::Exporter> on examples of
wrapping function dynamically (during runtime), or
L<Dist::Zilla::Plugin::Rinci::Wrap> on an example of embedding the generated
wrapping code to source code during build.


=head1 EXTENDING

The framework is simple and extensible. Please delve directly into the source
code for now. Some notes:

The internal uses OO.

The main wrapper building mechanism is in the C<wrap()> method.

For each Rinci property, it will call C<handle_NAME()> wrapper handler method.
The C<handlemeta_NAME()> methods are called first, to determine order of
processing. You can supply these methods either by subclassing the class or,
more simply, monkeypatching the method in the C<Perinci::Sub::Wrapper> package.

The wrapper handler method will be called with a hash argument, containing these
keys: B<value> (property value), B<new> (this key will exist if C<convert>
argument of C<wrap()> exists, to convert a property to a new value).

For properties that have name in the form of C<NAME1.NAME2.NAME3> (i.e., dotted)
only the first part of the name will be used (i.e., C<handle_NAME1()>).


=head1 VARIABLES

=head2 $Log_Wrapper_Code (BOOL)

Whether to log wrapper result. Default is from environment variable
LOG_PERINCI_WRAPPER_CODE, or false. Logging is done with L<Log::Any> at trace
level.


=head1 METHODS

The OO interface is only used internally or when you want to extend the wrapper.


=head1 ENVIRONMENT

=head2 LOG_PERINCI_WRAPPER_CODE (bool)

If set to 1, will log the generated wrapper code. This value is used to set
C<$Log_Wrapper_Code> if it is not already set.


=head1 RINCI FUNCTION METADATA

=head2 x.perinci.sub.wrapper.disable_validate_args => bool

Can be set to 1 to set C<validate_args> to 0 by default. This is used e.g. if
you already embed/insert code to validate arguments by other means and do not
want to repeat validating arguments. E.g. used if you use
L<Dist::Zilla::Plugin::Rinci::Validate>.

=head2 x.perinci.sub.wrapper.disable_validate_result => bool

Can be set to 1 to set C<validate_result> to 0 by default. This is used e.g. if
you already embed/insert code to validate result by other means and do not want
to repeat validating result. E.g. used if you use
L<Dist::Zilla::Plugin::Rinci::Validate>.

=head2 x.perinci.sub.wrapper.logs => array

Generated/added by this module to the function metadata for every wrapping done.
Used to avoid adding repeated code, e.g. to validate result or arguments.


=head1 PERFORMANCE NOTES

The following numbers are produced on an Intel Core i5-2400 3.1GHz desktop using
PSW v0.51 and Perl v5.18.2. Operating system is Debian sid (64bit).

For perspective, empty subroutine (C<< sub {} >>) as well as C<< sub { [200,
"OK"] } >> can be called around 5.3 mil/sec.

Wrapping this subroutine C<< sub { [200, "OK"] } >> and this simple metadata C<<
{v=>1.1} >> using default options yields call performance for C<< $sub->() >> of
about 0.9 mil/sec. With C<< validate_args=>0 >> and C<< validate_result=>0 >>,
it's 1.5 mil/sec.

As more (and more complex) arguments are introduced and validated, overhead will
increase. The significant portion of the overhead is in argument validation. For
example, this metadata C<< {v=>1.1, args=>{a=>{schema=>"int"}}} >> yields 0.5
mil/sec.

=head1 FAQ

=head2 General

=over

=item * What is a function wrapper?

A wrapper function calls the target function but with additional behaviors. The
goal is similar to function composition or decorator system like in Python (or
its Perl equivalent L<Python::Decorator>) where you use a higher-order function
which accepts another function and modifies it.

It is used to add various functionalities, e.g.: cache/memoization, singleton,
adding benchmarking/timing around function call, logging, argument validation
(parameter checking), checking pre/post-condition, authentication/authorization
checking, etc. The Python folks use decorators quite a bit; see discussions on
the Internet on those.

=item * How is PSW different from Python::Decorator?

PSW uses dynamic code generation (it generates Perl code on the fly). It also
creates a single large wrapper instead of nested wrappers. It builds wrapper
code according to L<Rinci> specification.

=item * Why use code generation?

Mainly because L<Data::Sah>, which is the module used to do argument validation,
also uses code generation. Data::Sah allows us to do data validation at full
Perl speed, which can be one or two orders of magnitude faster than
"interpreter" modules like L<Data::FormValidator>.

=item * Why use a single large wrapper?

This is just a design approach. It can impose some restriction for wrapper code
authors, since everything needs to be put in a single subroutine, but has nice
properties like less stack trace depth and less function call overhead.

=back

=head2 Debugging

=over

=item * How to display the wrapper code being generated?

If environment variable L<LOG_PERINCI_WRAPPER_CODE> or package variable
$Log_Perinci_Wrapper_Code is set to true, generated wrapper source code is
logged at trace level using L<Log::Any>. It can be displayed, for example, using
L<Log::Any::App>:

 % LOG_PERINCI_WRAPPER_CODE=1 TRACE=1 \
   perl -MLog::Any::App -MPerinci::Sub::Wrapper=wrap_sub \
   -e 'wrap_sub(sub=>sub{}, meta=>{v=>1.1, args=>{a=>{schema=>"int"}}});'

Note that L<Data::Sah> (the module used to generate validator code) observes
C<LOG_SAH_VALIDATOR_CODE>, but during wrapping this environment flag is
currently disabled by this module, so you need to set
L<LOG_PERINCI_WRAPPER_CODE> instead.

=back

=head2 caller() doesn't work from inside my wrapped code!

Wrapping adds at least one or two levels of calls: one for the wrapper
subroutine itself, the other is for the eval trap when necessary.

This poses a problem if you need to call caller() from within your wrapped code;
it will also be off by at least one or two.

The solution is for your function to use the caller() replacement, provided by
L<Perinci::Sub::Util>. Or use embedded mode, where the wrapper code won't add
extra subroutine calls.


=head1 SEE ALSO

L<Perinci>, L<Rinci>

L<Python::Decorator>

L<Dist::Zilla::Plugin::Rinci::Wrap>

L<Dist::Zilla::Plugin::Rinci::Validate>

=cut
