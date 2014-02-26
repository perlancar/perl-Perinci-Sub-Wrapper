package Perinci::Sub::Wrapper;

use 5.010001;
use strict;
use warnings;
use experimental 'smartmatch';
use Log::Any '$log';

use Perinci::Sub::Util qw(err);

use Exporter qw(import);
our @EXPORT_OK = qw(wrap_sub);

our $Log_Wrapper_Code = $ENV{LOG_PERINCI_WRAPPER_CODE} // 0;

# VERSION

our %SPEC;

# "protocol version" (v). whenever there's a significant change in the basic
# structure of the wrapper, which potentially cause some/a lot of property
# handlers to stop working, we increase this. property handler must always state
# which version it follows in its meta. if unspecified, it's assumed to be 1.
our $protocol_version = 2;

sub new {
    my ($class, %args) = @_;
    $args{_compiled_package}           //= 'Perinci::Sub::Wrapped';
    $args{indent}                      //= " " x 4;
    $args{convert}                     //= {};
    $args{compile}                     //= 1;
    $args{validate_args}               //= 1;
    $args{validate_result}             //= 1;

    # internal attributes
    $args{_schema_is_normalized}       //= 0;
    $args{_remove_internal_properties} //= 1;

    bless \%args, $class;
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
        $self->push_lines("require $mod;");
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
    state $val = {
        before_sub_require_modules => {order=>0},

        # reserved by wrapper for setting Perl package and declaring 'sub {'
        OPEN_SUB => {order=>1},

        declare_vars => {order=>2},

        # for handlers to put stuffs right before eval. for example, 'timeout'
        # uses this to set ALRM signal handler.
        before_eval => {order=>10},

        # reserved by wrapper for generating 'eval {'
        OPEN_EVAL => {order=>20},

        # for handlers to put various checks before calling the wrapped sub,
        # from data validation, argument conversion, etc. this is now
        # deprecated. see various before_call_* instead.
        before_call => {order=>30},

        # used by args_as handler to initialize %args from input data (@_)
        before_call_accept_args => {order=>31},

        # used e.g. to load modules used by validation
        before_call_before_arg_validation => {order=>32},

        before_call_arg_validation => {order=>32},

        # used e.g. by dependency checking
        before_call_after_arg_validation => {order=>33},

        # feed arguments to sub
        before_call_feed_args => {order=>49},

        # reserved by the wrapper for calling the sub
        CALL => {order=>50},

        # for handlers to put various things after calling, from validating
        # result, enveloping result, etc. this is now deprecated. see various
        # after_call_* instead.
        after_call => {order=>60},

        # used e.g. to load modules used by validation
        after_call_before_res_validation => {order=>61},

        after_call_res_validation => {order=>62},

        # reserved by wrapper to put eval end '}' and capturing $@ in $eval_err
        CLOSE_EVAL => {order=>70},

        # for handlers to put checks against $eval_err
        after_eval => {order=>80},

        # for handlers that want to do something with $res for the last time
        before_return_res => {order=>85},

        # reserved for returning '$_w_res' and the sub closing '}' line
        CLOSE_SUB => {order=>90},
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

sub _check_known_section {
    my ($self, $section) = @_;
    my $ks = $self->_known_sections;
    $ks->{$section} or die "BUG: Unknown code section '$section'";
}

sub _return_res {
    my ($self) = @_;
    if (!$self->{_avoid_postamble}) {
        # normally we do this
         'goto RETURN_RES;';
    } else {
        if ($self->{_meta}{result_naked}) {
            'return $_wres->[2];';
        } else {
            'return $_wres;';
        }
    }
}

sub _err {
    my ($self, $c_status, $c_msg) = @_;
    $self->push_lines(
        '$_w_res = ' . "[$c_status, $c_msg]" . ';',
        $self->_return_res);
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
    if ($self->{_debug}) {
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

sub _code_as_str {
    my ($self) = @_;
    my @lines;
    my $ss = $self->_known_sections;
    my $prev_section_level = 0;
    my $i = 0;
    for my $s (sort {$ss->{$a}{order} <=> $ss->{$b}{order}}
                   keys %$ss) {
        next if $self->section_empty($s);
        $i++;
        for my $l (@{ $self->{_codes}{$s} }) {
            $l->[0] += $prev_section_level;
            die "BUG: Negative indent level in line $i: '$l'"
                if $l->[0] < 0;
            my $s = ($self->{indent} x $l->[0]) . $l->[1];
            if (defined $l->[2]) {
                my $num_ws = 80 - length($s);
                $num_ws = 1 if $num_ws < 1;
                $s .= (" " x $num_ws) . "# $l->[2]";
            }
            push @lines, $s;
        }
        $prev_section_level += $self->{_levels}{$s};
    }
    join "\n", @lines;
}

sub handlemeta_v { {v=>2, prio=>0.1, convert=>1} }
sub handle_v {
    my ($self, %args) = @_;

    my $v      = $args{new} // $args{value};
    die "Cannot produce metadata other than v1.1 ($v)" unless $v == 1.1;

    return if $v == $args{value};
    die "Cannot convert metadata other than from v1.0"
        unless $args{value} == 1.0;

    my $meta = $self->{_meta};

    # converting metadata from v1.0 to v1.1
    if ($meta->{args}) {
        for my $a (keys %{$meta->{args}}) {
            my $old = $meta->{args}{$a};
            my $new = {};
            if (ref($old) eq 'ARRAY') {
                for (qw/summary description/) {
                    $new->{$_} = $old->[1]{$_} if defined $old->[1]{$_};
                    delete $old->[1]{$_};
                }
                if (defined $old->[1]{arg_pos}) {
                    $new->{pos} = $old->[1]{arg_pos};
                    delete $old->[1]{arg_pos};
                }
                if (defined $old->[1]{arg_greedy}) {
                    $new->{greedy} = $old->[1]{arg_greedy};
                    delete $old->[1]{arg_greedy};
                }
                if (defined $old->[1]{arg_complete}) {
                    $new->{completion} = $old->[1]{arg_complete};
                    delete $old->[1]{arg_complete};
                }
                if (defined $old->[1]{arg_aliases}) {
                    $new->{cmdline_aliases} = $old->[1]{arg_aliases};
                    while (my ($al, $als) = each %{ $new->{cmdline_aliases} }) {
                        if ($als->{code}) {
                            warn join(
                                "",
                                "Converting arg_aliases -> cmdline_aliases: ",
                                "alias '$al' has 'code', ",
                                "this must be converted manually due to change",
                                "of arguments (now only receives \\\%args)"
                            );
                        }
                    }
                    delete $old->[1]{arg_aliases};
                }
            } elsif (!ref($old)) {
                # do nothing
            } else {
                die "Can't handle v1.0 args property ".
                    "(arg '$a' not array/scalar)";
            }
            $new->{schema} = $old;
            $meta->{args}{$a} = $new;
        }
    }

    if ($meta->{result}) {
        $meta->{result} = {schema=>$meta->{result}};
    }

    $meta->{_note} = "Converted from v1.0 by ".__PACKAGE__.
        " on ".scalar(localtime);
}

# before all the other language properties (summary, description, ...)
sub handlemeta_default_lang { {v=>2, prio=>0.9, convert=>1} }
sub handle_default_lang {
    my ($self, %args) = @_;

    my $meta = $self->{_meta};
    my @m = ($meta);
    push @m, @{$meta->{links}} if $meta->{links};
    push @m, @{$meta->{examples}} if $meta->{examples};
    push @m, $meta->{result} if $meta->{result};
    push @m, values %{$meta->{args}} if $meta->{args};
    push @m, (grep {ref($_) eq 'HASH'} @{$meta->{tags}}) if $meta->{tags};

    my $i = 0;
    my ($value, $new);
    for my $m (@m) {
        $i++;
        if ($i == 1) {
            $value = $args{value} // "en_US";
            $new   = $args{new}   // $value;
        } else {
            $value = $m->{default_lang} // "en_US";
        }
        return if $value eq $new && $i == 1;
        $m->{default_lang} = $new;
        for my $prop (qw/summary description/) {
            $m->{"$prop.alt.lang.$value"} //= $m->{$prop}
                if defined $m->{$prop};
            $m->{$prop} = $m->{"$prop.alt.lang.$new"};
            delete $m->{$prop} unless defined $m->{$prop};
            delete $m->{"$prop.alt.lang.$new"};
        }
    }
}

sub handlemeta_name { {} }
sub handlemeta_summary { {} }
sub handlemeta_description { {} }
sub handlemeta_tags { {} }

sub handlemeta_links { {v=>2, prio=>5} }
sub handle_links {
    my ($self, %args) = @_;

    my $v = $self->{_meta}{links};
    return unless $v;

    my $opt_rip = $self->{_args}{_remove_internal_properties};
    for my $ln (@$v) {
        for my $k (keys %$ln) {
            if ($k =~ /^_/) {
                delete $ln->{$k} if $opt_rip;
            }
        }
    }
}

sub handlemeta_text_markup { {} }
sub handlemeta_is_func { {} }
sub handlemeta_is_meth { {} }
sub handlemeta_is_class_meth { {} }

sub handlemeta_examples { {v=>2, prio=>5} }
sub handle_examples {
    my ($self, %args) = @_;

    my $v = $self->{_meta}{examples};
    return unless $v;

    my $opt_rip = $self->{_args}{_remove_internal_properties};
    for my $ex (@$v) {
        for my $k (keys %$ex) {
            if ($k =~ /^_/) {
                delete $ex->{$k} if $opt_rip;
            }
        }
    }
}

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

    $self->select_section('before_call_accept_args');

    my $v = $new // $value;
    $self->push_lines('', "# accept arguments ($v)");
    if ($v eq 'hash') {
         $self->push_lines('my %args = @_;');
    } elsif ($v eq 'hashref') {
        $self->push_lines('my %args = %{$_[0]};');
    } elsif ($v =~ /\Aarray(ref)?\z/) {
        my $ref = $1 ? 1:0;
        $self->push_lines('my %args;');
        while (my ($a, $as) = each %$args_p) {
            my $line = '$args{'.__squote($a).'} = ';
            defined($as->{pos}) or die "Error in args property for arg '$a': ".
                "no pos defined";
            my $pos = int($as->{pos} + 0);
            $pos >= 0 or die "Error in args property for arg '$a': ".
                "negative value in pos";
            if ($as->{greedy}) {
                $line .= '[splice '.($ref ? '@{$_[0]}' : '@_').", $pos]";
            } else {
                $line .= $ref ? '$_[0]['.$pos.']' : '$_['.$pos.']';
            }
            $self->push_lines("$line;");
        }
    } else {
        die "Unknown args_as value '$v'";
    }

    my $tok;
    $self->select_section('before_call_feed_args');
    $v = $value;
    if ($v eq 'hash') {
        $tok = '%args';
    } elsif ($v eq 'hashref') {
        $tok = '\%args';
    } elsif ($v =~ /\Aarray(ref)?\z/) {
        my $ref = $1 ? 1:0;
        $self->push_lines('my @args;');
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
                $line = '$args['.$pos."] = $t";
            }
            $self->push_lines("$line;");
        }
        $tok = $ref ? '\@args' : '@args';
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

sub handlemeta_args { {v=>2, prio=>10, convert=>0} }
sub handle_args {

    my ($self, %args) = @_;

    my $v = $self->{_meta}{args};
    return unless $v;

    my $opt_rip = $self->{_args}{_remove_internal_properties};
    my $opt_sin = $self->{_args}{_schema_is_normalized};
    my $opt_va  = $self->{_args}{validate_args};

    # normalize schema
    unless ($opt_sin) {
        for my $an (keys %$v) {
            my $as = $v->{$an};
            if ($as->{schema}) {
                $as->{schema} =
                    $self->_sah->normalize_schema($as->{schema});
            }
            my $als = $as->{cmdline_aliases};
            if ($als) {
                for my $al (sort keys %$als) {
                    if ($als->{$al}{schema}) {
                        $als->{$al}{schema} =
                            $self->_sah->normalize_schema($als->{$al}{schema});
                    }
                }
            }
        }
    }

    if ($opt_va) {
        $self->select_section('before_call_arg_validation');
        $self->push_lines('', '# check args');
        $self->push_lines('for (keys %args) {');
        $self->indent;
        $self->_errif(400, q["Invalid argument name '$_'"],
                      '!/\A(-?)\w+(\.\w+)*\z/o');
        $self->_errif(
            400, q["Unknown argument '$_'"],
            '!($1 || $_ ~~ '.__squote([keys %$v]).')');

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
        for (sort keys %$argspec) {
            if (/\A_/) {
                delete $argspec->{$_} if $opt_rip;
                next;
            }
            # XXX schema's schema
            # check known arg key
            die "Unknown arg spec key '$_' for arg '$argname'" unless
                /\A(
                     summary|description|tags|default_lang|
                     schema|req|pos|greedy|
                     default|
                     completion|element_completion|
                     cmdline_aliases|
                     cmdline_src|
                     cmdline_on_getopt|
                     x
                 )(\..+)?\z/x;
        }

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
                    schema_is_normalized => !$opt_sin,
                    return_type          => 'str',
                    indent_level         => $self->get_indent_level + 4,
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
                $self->push_lines(
                    '} else {',
                    "    $argterm //= ".__squote($argspec->{default}).";")
                    if $has_default_prop;
                $self->push_lines(
                    '} else {',
                    "    $argterm //= ".__squote($sch->[1]{default}).";")
                    if $has_sch_default;
                $self->push_lines('}');
            }
        }
        if ($argspec->{req}) {
            $self->_errif(
                400, qq["Missing required argument: $argname"],
                "!exists($argterm)");
        }
    }
}

sub handlemeta_result { {v=>2, prio=>50, convert=>0} }
sub handle_result {
    require Data::Sah;

    my ($self, %args) = @_;

    my $v = $self->{_meta}{result};
    return unless $v;

    my $opt_sin = $self->{_args}{_schema_is_normalized};
    my $opt_vr  = $self->{_args}{validate_result};
    my $opt_rip = $self->{_args}{_remove_internal_properties};

    my %ss; # key = status, value = schema

    # normalize schemas
    unless ($opt_sin) {
        if ($v->{schema}) {
            $v->{schema} = $self->_sah->normalize_schema($v->{schema});
        }
        if ($v->{statuses}) {
            for my $s (keys %{$v->{statuses}}) {
                my $sv = $v->{statuses}{$s};
                if ($sv->{schema}) {
                    $sv->{schema} =
                        $self->_sah->normalize_schema($sv->{schema});
                }
            }
        }
    }

    for my $k (keys %$v) {
        if ($k =~ /^_/) {
            delete $v->{$k} if $opt_rip;
        }
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
            );
            $self->_add_module($_) for @{ $cd->{modules} };
            $self->_add_var($_, $cd->{vars}{$_})
                for sort keys %{ $cd->{vars} };
            $self->push_lines("if (\$_w_res->[0] == $s) {");
            $self->indent;
            $self->push_lines("$cd->{result};");
            $self->_errif(
                500, qq["BUG: Sub produces invalid result (status=$s): ].
                    qq[\$_w_err2_res"],
                "\$_w_err2_res");
            $self->unindent;
            $self->push_lines("}");
        }
    }
}

sub handlemeta_result_naked { {v=>2, prio=>100, convert=>1} }
sub handle_result_naked {
    my ($self, %args) = @_;

    my $old = $args{value};
    my $v   = $args{new} // $old;

    $self->select_section('before_return_res');
    if ($v) {
        $self->push_lines(
            '', '# strip result envelope',
            '$_w_res = $_w_res->[2];',
        );
    } elsif ($old && !$v) {
        $self->push_lines(
            '', '# add result envelope',
            '$_w_res = [200, "OK", $_w_res->[2]];',
        );
    }
}

sub handlemeta_deps { {v=>2, prio=>0.5} }
sub handle_deps {
    my ($self, %args) = @_;
    my $value = $args{value};
    my $meta  = $self->{_meta};
    my $v     = $self->{_var_meta};
    $self->select_section('before_call_after_arg_validation');
    $self->push_lines('', '# check dependencies');
    $self->push_lines('require Perinci::Sub::DepChecker;');
    $self->push_lines('my $_w_deps_res = Perinci::Sub::DepChecker::check_deps($'.
                          $v.'->{deps});');
    $self->_errif(412, '"Deps failed: $_w_deps_res"', '$_w_deps_res');

    # we handle some deps our own
    if ($value->{tmp_dir}) {
        $self->push_lines(
            'unless ($args{-tmp_dir}) { $_w_res = [412, "Dep failed: '.
                'please specify -tmp_dir"]; '. $self->_return_res . ' }');
    }
    if ($value->{trash_dir}) {
        $self->push_lines(
            'unless ($args{-trash_dir}) { $_w_res = [412, "Dep failed: '.
                'please specify -trash_dir"]; '. $self->_return_res .' }');
    }
    if ($value->{undo_trash_dir}) {
        $self->push_lines(join(
            '',
            'unless ($args{-undo_trash_dir} || $args{-tx_manager} || ',
            '$args{-undo_action} && $args{-undo_action}=~/\A(?:undo|redo)\z/) ',
            '{ $_w_res = [412, "Dep failed: ',
            'please specify -undo_trash_dir"]; '. $self->_return_res .' }'
        ));
    }
}

sub handlemeta_x { {v=>2, prio=>99} }
sub handle_x {}

sub handlemeta_entity_v { {v=>2, prio=>99} }
sub handle_entity_v {}

sub wrap {
    require Data::Clone;
    require Scalar::Util;

    my ($self, %args) = @_;

    my $sub      = $args{sub};
    my $sub_name = $args{sub_name};
    $sub || $sub_name or return [400, "Please specify sub or sub_name"];
    $args{meta} or return [400, "Please specify meta"];
    # we clone the meta because we'll replace stuffs
    my $meta     = Data::Clone::clone($args{meta});

    # reset state/work data
    $self->{_cur_section}      = undef;
    $self->{_cur_handler}      = undef;
    $self->{_cur_handler_args} = undef;
    $self->{_cur_handler_meta} = undef;
    $self->{_levels}           = {};
    $self->{_codes}            = {};
    $self->{_args}             = \%args;
    $self->{_meta}             = $meta; # the new metadata
    $self->{_modules} = []; # modules loaded by wrapper sub

    # temp vars
    my $opt_cvt = $args{replace};
    my $opt_rip = $args{_remove_internal_properties};

    my $comppkg  = $self->{_compiled_package};

    local $self->{_debug} = $args{debug} // 0;

    # add properties from convert, if not yet mentioned in meta
    for (keys %$opt_cvt) {
        $meta->{$_} = undef unless exists $meta->{$_};
    }

    $convert->{v} //= 1.1;

    # clone some properties

    $meta->{v} //= 1.0;
    return [412, "Unsupported metadata version ($meta->{v}), only 1.0 & 1.1 ".
                "supported"]
        unless $meta->{v} == 1.1 || $meta->{v} == 1.0;

    # if a coderef is supplied ($sub), put it in a named variable in $comppkg
    # package, so it can be accessed by the wrapper code.
    if (!$sub_name) {
        $sub_name = $comppkg . "::sub".Scalar::Util::refaddr($sub);
        no strict 'refs';
        no warnings;
        ${$sub_name} = $sub;
        $sub_name = "\$$sub_name"; # make it a scalar
    }
    use experimental 'smartmatch';

    # also store the meta, it is needed by the wrapped sub. sometimes the meta
    # contains coderef and can't be dumped reliably, so we store it instead.
    my $metaname = $comppkg . "::meta".Scalar::Util::refaddr($meta);
    { no strict 'refs'; no warnings; ${$metaname} = $meta; }
    $self->{_var_meta} = $metaname;

    $self->select_section('OPEN_SUB');
    $self->push_lines(
        "package $comppkg;",
        'sub {');
    $self->indent;
    $self->push_lines(
        'my ($_w_res, $_w_eval_err);');

    $meta->{args_as} //= "hash";

    if ($meta->{args_as} =~ /hash/) {
        $self->select_section('before_call_after_arg_validation');
        # tell function it's being wrapped, currently disabled
        #$self->push_lines('$args{-wrapped} = 1;');
    }

    my %props = map {$_=>1} keys %$meta;
    $props{$_} = 1 for keys %$convert;

    my %handler_args;
    my %handler_metas;
    for my $k0 (keys %props) {
        if ($k0 =~ /^_/) {
            delete $meta->{$k0} if $rip;
            next;
        }
        my $k = $k0;
        $k =~ s/\..+//;
        next if $handler_args{$k};
        if ($k ~~ $self->{_args}{skip}) {
            $log->tracef("Skipped property %s (mentioned in skip)", $k);
            next;
        }
        return [500, "Invalid property name $k"] unless $k =~ /\A\w+\z/;
        my $meth = "handlemeta_$k";
        unless ($self->can($meth)) {
            # try a property module first
            eval { require "Perinci/Sub/Property/$k.pm" };
            unless ($self->can($meth)) {
                return [500, "Can't handle wrapping property $k0 ($meth)"];
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
        if (exists $convert->{$k0}) {
            return [502, "Property '$k0' does not support conversion"]
                unless $hm->{convert};
            $ha->{new}   = $convert->{$k0};
            $meta->{$k0} = $convert->{$k0};
        }
        $handler_args{$k}  = $ha;
        $handler_metas{$k} = $hm;
    }

    $self->select_section('before_return_res');
    $self->push_lines('RETURN_RES:');

    for my $k (sort {$handler_args{$a}{prio} <=> $handler_args{$b}{prio}}
                   keys %handler_args) {
        my $ha = $handler_args{$k};
        my $meth = $ha->{meth};
        local $self->{_cur_handler}      = $meth;
        local $self->{_cur_handler_meta} = $handler_metas{$k};
        local $self->{_cur_handler_args} = $ha;
        $self->$meth(args=>\%args, meta=>$meta, %$ha);
    }

    $self->select_section('CALL');
    $self->push_lines('$_w_res = ' . $sub_name . ($sub_name =~ /^\$/ ? "->" : "").
                          "(".$self->{_args_token}.");");
    if ($self->{_args}{meta}{result_naked}) {
        # internally we always use result envelope, so let's envelope this
        # temporarily.
        $self->push_lines('# add temporary envelope',
                          '$_w_res = [200, "OK", $_w_res];');
    } elsif ($args{validate_result}) {
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
                '$_w_res = [500, "BUG: Sub does not produce envelope: ".'.
                    'Data::Dumper::Dumper($_w_res)];');
        } else {
            $self->push_lines(
                '$_w_res = [500, "BUG: Sub does not produce envelope"];');
        }
        $self->push_lines($self->_return_res);
        $self->unindent;
        $self->push_lines('}');
    }

    if ($self->_needs_eval) {
        $self->select_section('CLOSE_EVAL');
        $self->unindent;
        $self->push_lines(
            '',
            '};',
            '$_w_eval_err = $@;');
        # _needs_eval will automatically be enabled here, due after_eval being
        # filled
        $self->select_section('after_eval');
        $self->push_lines('warn $_w_eval_err if $_w_eval_err;');
        $self->_errif(500, '"Function died: $_w_eval_err"', '$_w_eval_err');
    }

    # return result
    $self->select_section('CLOSE_SUB');
    $self->push_lines('return $_w_res;');
    $self->unindent;
    $self->push_lines('}');

    # hm, indent + unindent doesn't work properly here?
    #$self->unindent;
    #$self->push_lines('}');

    if ($self->_needs_eval) {
        $self->select_section('OPEN_EVAL');
        $self->push_lines('eval {');
        $self->indent;
    }

    my $source = $self->_code_as_str;
    if ($Log_Wrapper_Code && $log->is_trace) {
        require SHARYANTO::String::Util;
        $log->tracef("wrapper code:\n%s",
                     $ENV{LINENUM} // 1 ?
                         SHARYANTO::String::Util::linenum($source) :
                               $source);
    }
    my $result = {source=>$source, meta=>$meta};
    if ($args{compile}) {
        my $wrapped = eval $source;
        die "BUG: Wrapper code can't be compiled: $@" if $@ || !$wrapped;

        $result->{sub}  = $wrapped;
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

Aside from wrapping the subroutine, will also create a new metadata for it. The
new metadata is a shallow copy of the original, with most properties usually
untouched. Only certain properties will be changed to match the new subroutine
behavior. For example, if you set a different 'args_as' or 'result_naked' in
'convert', then the new metadata will carry the new values.

_
        schema=>['hash*'=>{keys=>{
            sub=>'code*',
            source=>'str*',
            meta=>'hash*',
        }}],
    },
    args => {
        sub => {
            schema => 'code*',
            summary => 'The code to wrap',
            description => <<'_',

Either `sub` or `sub_name` must be supplied.

If generated wrapper code is to be saved to disk or used by another process,
then `sub_name` is required.

_
        },
        sub_name => {
            schema => 'str*',
            summary => 'The fully qualified name of the subroutine, '.
                'e.g. Foo::func',
            description => <<'_',

Either `sub` or `sub_name` must be supplied.

If generated wrapper code is to be saved to disk or used by another process,
then `sub_name` is required.

_
        },
        meta => {
            schema => 'hash*',
            summary => 'The function metadata',
            req => 1,
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
            schema => [bool => default=>1],
            summary => 'Whether wrapper should validate arguments',
            description => <<'_',

If set to true, will validate arguments. Validation error will cause status 400
to be returned. This will only be done for arguments which has `schema` arg spec
key. Will not be done if `args` property is skipped.

_
        },
        validate_result => {
            schema => [bool => default=>1],
            summary => 'Whether wrapper should validate arguments',
            description => <<'_',

If set to true, will validate sub's result. Validation error will cause wrapper
to return status 500 instead of sub's result. This will only be done if `schema`
or `statuses` keys are set in the `result` property. Will not be done if
`result` property is skipped.

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
 my $res = wrap_sub(sub => sub {die "test\n"}, meta=>{...});
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


=head1 PERFORMANCE NOTES

The following numbers are produced on an Asus Zenbook UX31 laptop (Intel Core i5
1.7GHz) using Perinci::Sub::Wrapper v0.33 and Perl v5.14.2. Operating system is
Ubuntu 11.10 (64bit).

For perspective, empty subroutine (C<< sub {} >>) as well as C<< sub { [200,
"OK"] } >> can be called around 4.3 mil/sec.

Wrapping this subroutine C<< sub { [200, "OK"] } >> and this simple metadata C<<
{v=>1.1, args=>{a=>{schema=>"int"}}} >> using default options yields call
performance for C<< $sub->() >> of about 0.28 mil/sec. For C<< $sub->(a=>1) >>
it is about 0.12 mil/sec. So if your sub needs to be called a million times a
second, the wrapping adds too big of an overhead.

As more arguments are introduced in the schema and passed, and as argument
schemas become more complex, overhead will increase. The significant portion of
the overhead is in argument validation.


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
