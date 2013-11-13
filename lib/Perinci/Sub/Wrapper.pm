package Perinci::Sub::Wrapper;

use 5.010001;
use strict;
use warnings;
use experimental 'smartmatch';
use Log::Any '$log';

use Perinci::Sub::Util qw(err);
use Scalar::Util       qw(blessed);

use Exporter qw(import);
our @EXPORT_OK = qw(wrap_sub wrap_all_subs wrapped);

our $Log_Wrapper_Code = $ENV{LOG_PERINCI_WRAPPER_CODE} // 0;

# VERSION

our %SPEC;

# "protocol version" (v). whenever there's a significant change in the basic
# structure of the wrapper, which potentially cause some/a lot of property
# handlers to stop working, we increase this. property handler must always state
# which version it follows in its meta. if unspecified, it's assumed to be 1.
our $protocol_version = 2;

my $default_wrapped_package = 'Perinci::Sub::Wrapped';

sub new {
    my ($class, %args) = @_;
    $args{comppkg} //= $default_wrapped_package;
    $args{indent}  //= " " x 4;
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

        # reserved for returning '$res' and the sub closing '}' line
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

sub _err {
    my ($self, $c_status, $c_msg) = @_;
    $self->push_lines(
        # we set $res here when we return from inside the eval block
        '$res = ' . "[$c_status, $c_msg]" . ';',
        'goto RETURN_RES;');
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

    my $rm = $self->{_args}{remove_internal_properties};
    for my $ln (@$v) {
        for my $k (keys %$ln) {
            if ($k =~ /^_/) {
                delete $ln->{$k} if $rm;
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

    my $rm = $self->{_args}{remove_internal_properties};
    for my $ex (@$v) {
        for my $k (keys %$ex) {
            if ($k =~ /^_/) {
                delete $ex->{$k} if $rm;
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
        if ($self->{_args}{trap}) {
            $self->_errif(412, '"Must run with transaction (pass -tx_manager)"',
                          '!$args{-tx_manager}');
        } else {
            $self->push_lines(
                'die "Must run with transaction (pass -tx_manager)" '.
                    'unless $args{-tx_manager};'),
        }
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

    my $rm = $self->{_args}{remove_internal_properties};
    my $ns = $self->{_args}{normalize_schemas};
    my $va = $self->{_args}{validate_args};

    # normalize schema
    if ($ns) {
        for my $an (keys %$v) {
            my $as = $v->{$an};
            if ($as->{schema}) {
                $as->{schema} =
                    $self->_sah->normalize_schema($as->{schema});
            }
            my $als = $as->{cmdline_aliases};
            if ($als) {
                for my $al (keys %$als) {
                    if ($als->{$al}{schema}) {
                        $als->{$al}{schema} =
                            $self->_sah->normalize_schema($als->{$al}{schema});
                    }
                }
            }
        }
    }

    $self->select_section('before_call_arg_validation');
    $self->push_lines('', '# check arguments');

    unless ($self->{_args}{allow_invalid_args}) {
        $self->push_lines('for (keys %args) {');
        $self->indent;
        $self->_errif(400, q["Invalid argument name '$_'"],
                      '!/\A(-?)\w+(\.\w+)*\z/o');
        unless ($self->{_args}{allow_unknown_args}) {
            $self->_errif(
                400, q["Unknown argument '$_'"],
                '!($1 || $_ ~~ '.__squote([keys %$v]).')');
        }
        $self->unindent;
        $self->push_lines('}');
    }

    for my $an (sort keys %$v) {
        my $as = $v->{$an};
        for (sort keys %$as) {
            if (/\A_/) {
                delete $as->{$_} if $rm;
                next;
            }
            # XXX schema's schema
            # check known arg key
            die "Unknown arg spec key '$_' for arg '$an'" unless
                /\A(
                     summary|description|tags|default_lang|
                     schema|req|pos|greedy|
                     default|
                     completion|element_completion|
                     cmdline_aliases|
                     cmdline_src|
                     x
                 )(\..+)?\z/x;
        }

        my $at = "\$args{'$an'}";

        my $sch = $as->{schema};
        if ($sch) {
            my $has_default_prop = exists($as->{default});
            my $has_sch_default  = ref($sch) eq 'ARRAY' &&
                exists($sch->[1]{default}) ? 1:0;
            if ($va) {
                my $dn = $an; $dn =~ s/\W+/_/g;
                my $cd = $self->_plc->compile(
                    data_name            => $dn,
                    data_term            => $at,
                    schema               => $sch,
                    schema_is_normalized => $ns,
                    return_type          => 'str',
                    indent_level         => $self->get_indent_level + 4,
                );
                $self->_add_module($_) for @{ $cd->{modules} };
                $self->_add_var($_, $cd->{vars}{$_})
                    for sort keys %{ $cd->{vars} };
                $self->push_lines("if (exists($at)) {");
                $self->indent;
                $self->push_lines("my \$err_$dn;\n$cd->{result};");
                $self->_errif(
                    400, qq["Invalid value for argument '$an': \$err_$dn"],
                    "\$err_$dn");
                $self->unindent;
                if ($has_default_prop) {
                    $self->push_lines(
                        '} else {',
                        "    $at //= ".__squote($as->{default}).";",
                        '}');
                } elsif ($has_sch_default) {
                    $self->push_lines(
                        '} else {',
                        "    $at //= ".__squote($sch->[1]{default}).";",
                        '}');
                } else {
                    $self->push_lines('}');
                }
            } else {
                $self->push_lines(
                    "$at //= ".__squote($sch->[1]{default}).';')
                    if $has_default;
            }
        }
        if ($as->{req}) {
            $self->_errif(
                400, qq["Missing required argument: $an"], "!exists($at)");
        }
    }
}

sub handlemeta_result { {v=>2, prio=>50, convert=>0} }
sub handle_result {
    require Data::Sah;

    my ($self, %args) = @_;

    my $v = $self->{_meta}{result};
    return unless $v;

    my $ns = $self->{_args}{normalize_schemas};
    my $vr = $self->{_args}{validate_result};

    my %ss; # key = status, value = schema

    # normalize schemas
    if ($ns) {
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

    my $rm = $self->{_args}{remove_internal_properties};
    for my $k (keys %$v) {
        if ($k =~ /^_/) {
            delete $v->{$k} if $rm;
        }
    }

    # validate result
    my @modules;
    if ($v->{schema} && $vr) {
        $ss{200} = $v->{schema};
    }
    if ($v->{statuses} && $vr) {
        for my $s (keys %{$v->{statuses}}) {
            my $sv = $v->{statuses}{$s};
            if ($sv->{schema}) {
                $ss{$s} = $sv->{schema};
            }
        }
    }

    if (keys %ss) {
        $self->select_section('after_call_res_validation');
        $self->push_lines("my \$res2 = \$res->[2];");
        $self->push_lines("my \$err2_res;");

        for my $s (keys %ss) {
            my $sch = $ss{$s};
            my $cd = $self->_plc->compile(
                data_name            => 'res2',
                # err_res can clash on arg named 'res'
                err_term             => '$err2_res',
                schema               => $sch,
                schema_is_normalized => $ns,
                return_type          => 'str',
                indent_level         => $self->get_indent_level + 4,
            );
            $self->_add_module($_) for @{ $cd->{modules} };
            $self->_add_var($_, $cd->{vars}{$_})
                for sort keys %{ $cd->{vars} };
            $self->push_lines("if (\$res->[0] == $s) {");
            $self->indent;
            $self->push_lines("$cd->{result};");
            $self->_errif(
                500, qq["BUG: Sub produces invalid result (status=$s): ].
                    qq[\$err2_res"],
                "\$err2_res");
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
            '$res = $res->[2];',
        );
    } elsif ($old && !$v) {
        $self->push_lines(
            '', '# add result envelope',
            '$res = [200, "OK", $res->[2]];',
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
    $self->push_lines('my $deps_res = Perinci::Sub::DepChecker::check_deps($'.
                          $v.'->{deps});');
    if ($self->{_args}{trap}) {
        $self->_errif(412, '"Deps failed: $deps_res"', '$deps_res');
    } else {
        $self->push_lines('die "Deps failed: $deps_res" if $deps_res;');
    }

    # we handle some deps our own
    if ($value->{tmp_dir}) {
        $self->push_lines(
            'unless ($args{-tmp_dir}) { $res = [412, "Dep failed: '.
                'please specify -tmp_dir"]; goto RETURN_RES }');
    }
    if ($value->{trash_dir}) {
        $self->push_lines(
            'unless ($args{-trash_dir}) { $res = [412, "Dep failed: '.
                'please specify -trash_dir"]; goto RETURN_RES }');
    }
    if ($value->{undo_trash_dir}) {
        $self->push_lines(join(
            '',
            'unless ($args{-undo_trash_dir} || $args{-tx_manager} || ',
            '$args{-undo_action} && $args{-undo_action}=~/\A(?:undo|redo)\z/) ',
            '{ $res = [412, "Dep failed: ',
            'please specify -undo_trash_dir"]; goto RETURN_RES }'
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

    warn "wrap() is currently designed to be only called once, ".
        "to wrap again, please create a new ".__PACKAGE__." object"
            if $self->{_done}++;

    my $sub      = $args{sub};
    my $sub_name = $args{sub_name};
    $sub || $sub_name or return [400, "Please specify sub or sub_name"];
    $args{meta} or return [400, "Please specify meta"];
    my $meta     = Data::Clone::clone($args{meta});

    my $mp = "_perinci.sub.wrapper";
    $args{convert}             //= $meta->{"$mp.convert"} // {};
    $args{trap}                //= $meta->{"$mp.trap"} // 1;
    $args{compile}             //= 1;
    $args{normalize_schemas}   //= $meta->{"$mp.normalize_schemas"} // 1;
    $args{remove_internal_properties} //=
        $meta->{"$mp.remove_internal_properties"} // 1;
    $args{validate_args}       //= $meta->{"$mp.validate_args"} //
        $ENV{PERINCI_WRAPPER_VALIDATE_ARGS} // 1;
    $args{validate_result}     //= $meta->{"$mp.validate_result"} // 1;
    $args{allow_invalid_args}  //= $meta->{"$mp.allow_invalid_args"} // 0;
    $args{allow_unknown_args}  //= $meta->{"$mp.allow_unknown_args"} // 0;
    $args{skip}                //= $meta->{"$mp.skip"} // [];

    # temp vars
    my $convert  = $args{convert};
    my $rip      = $args{remove_internal_properties};

    my $comppkg  = $self->{comppkg};

    local $self->{_debug} = $args{debug} // 0;

    # add properties from convert, if not yet mentioned in meta
    for (keys %$convert) {
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

    # reset work variables. we haven't tested this yet because we expect the
    # object to be used only for one-off wrapping, via wrap_sub().
    $self->{_cur_section} = undef;
    $self->{_cur_handler} = undef;
    $self->{_cur_handler_args} = undef;
    $self->{_cur_handler_meta} = undef;
    $self->{_levels} = {};
    $self->{_codes} = {};
    $self->{_args} = \%args;
    $self->{_meta} = $meta; # the new metadata
    $self->{_modules} = []; # modules loaded by wrapper sub

    $self->select_section('OPEN_SUB');
    $self->push_lines(
        "package $comppkg;",
        'sub {');
    $self->indent;
    $self->push_lines(
        'my ($res, $eval_err);');

    $meta->{args_as} //= "hash";

    if ($meta->{args_as} =~ /hash/) {
        $self->select_section('before_call_after_arg_validation');
        $self->push_lines('$args{-wrapped} = 1;');
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
        if ($args{forbid_tags} && $hm->{tags}) {
            for my $t (@{$hm->{tags}}) {
                if ($t ~~ $args{forbid_tags}) {
                    return [412, "Can't wrap property $k0 (forbidden by ".
                                "forbid_tags, tag=$t)"];
                }
            }
        }
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
    $self->push_lines('$res = ' . $sub_name . ($sub_name =~ /^\$/ ? "->" : "").
                          "(".$self->{_args_token}.");");
    if ($self->{_args}{meta}{result_naked}) {
        # internally we always use result envelope, so let's envelope this
        # temporarily.
        $self->push_lines('# add temporary envelope',
                          '$res = [200, "OK", $res];');
    } elsif ($args{validate_result}) {
        $self->push_lines(
            '',
            '# check that sub produces enveloped result',
            'unless (ref($res) eq "ARRAY" && $res->[0]) {',
        );
        $self->indent;
        if ($log->is_trace) {
            $self->_add_module('Data::Dumper');
            $self->push_lines(
                'local $Data::Dumper::Purity   = 1;',
                'local $Data::Dumper::Terse    = 1;',
                'local $Data::Dumper::Indent   = 0;',
                '$res = [500, "BUG: Sub does not produce envelope: ".'.
                    'Data::Dumper::Dumper($res)];');
        } else {
            $self->push_lines(
                '$res = [500, "BUG: Sub does not produce envelope"];');
        }
        $self->push_lines('goto RETURN_RES;');
        $self->unindent;
        $self->push_lines('}');
    }

    if ($args{trap} || $self->_needs_eval) {
        $self->select_section('CLOSE_EVAL');
        $self->unindent;
        $self->push_lines(
            '',
            '};',
            '$eval_err = $@;');
        # _needs_eval will automatically be enabled here, due after_eval being
        # filled
        $self->select_section('after_eval');
        $self->_errif(500, '"Function died: $eval_err"', '$eval_err');
    }

    # return result
    $self->select_section('CLOSE_SUB');
    $self->push_lines('return $res;');
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

        # mark the wrapper with bless, to detect double wrapping attempt
        bless $wrapped, $comppkg;

        $result->{sub}  = $wrapped;
    }
    [200, "OK", $result];
}

$SPEC{wrap_sub} = {
    v => 1.1,
    summary => 'Wrap subroutine to do various things, '.
        'like enforcing Rinci properties',
    description => <<'_',

Will wrap subroutine and bless the generated wrapped subroutine (by default into
`Perinci::Sub::Wrapped`) as a way of marking that the subroutine is a wrapped
one.

_
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
        skip => {
            schema => 'array*',
            summary => 'Properties to skip '.
                '(treat as if they do not exist in metadata)',
        },
        trap => {
            schema => ['bool' => {default=>1}],
            summary => 'Whether to trap exception using an eval block',
            description => <<'_',

If set to true, will wrap call using an eval {} block and return 500 /undef if
function dies. Note that if some other properties requires an eval block (like
'timeout') an eval block will be added regardless of this parameter.

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
        normalize_schemas => {
            schema => ['bool' => {default=>1}],
            summary => 'Whether to normalize schemas in metadata',
            description => <<'_',

By default, wrapper normalize Sah schemas in metadata, like in 'args' or
'result' property, for convenience so that it does not need to be normalized
again prior to use. If you want to turn off this behaviour, set to false.

_
        },
        remove_internal_properties => {
            schema => ['bool' => {default=>1}],
            summary => 'Whether to remove properties prefixed with _',
            description => <<'_',

By default, wrapper removes internal properties (properties which start with
underscore) in the new metadata. Set this to false to keep them.

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
        forbid_tags => {
            schema => 'array',
            summary => 'Forbid properties which have certain wrapping tags',
            description => <<'_',

Some property wrapper, like dies_on_error (see
Perinci::Sub::Property::dies_on_error) has tags 'die', to signify that it can
cause wrapping code to die.

Sometimes such properties are not desirable, e.g. in daemon environment. The use
of such properties can be forbidden using this setting.

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
        allow_invalid_args => {
            schema => [bool => default=>0],
            summary => 'Whether to allow invalid arguments',
            description => <<'_',

By default, wrapper will require that all argument names are valid
(`/\A-?\w+\z/`), except when this option is turned on.

_
        },
        allow_unknown_args => {
            schema => [bool => default=>0],
            summary => 'Whether to allow unknown arguments',
            description => <<'_',

By default, this setting is set to false, which means that wrapper will require
that all arguments are specified in `args` property, except for special
arguments (those started with underscore), which will be allowed nevertheless.
Will only be done if `allow_invalid_args` is set to false.

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

$SPEC{wrap_all_subs} = {
    v => 1.1,
    summary => 'Wrap all subroutines in a package '.
        'and replace them with the wrapped version',
    description => <<'_',

This function will search all subroutines in a package which have metadata, wrap
them, then replace the original subroutines and metadata with the wrapped
version.

One common use case is to put something like this at the bottom of your module:

    Perinci::Sub::Wrapper::wrap_all_subs();

to wrap ("protect") all your module's subroutines and discard the original
unwrapped version.

_
    args => {
        package => {
            schema => 'str*',
            summary => 'Package to search subroutines in',
            description => <<'_',

Default is caller package.

_
        },
        wrap_args => {
            schema => 'hash*',
            summary => 'Arguments to pass to wrap_sub()',
            description => <<'_',

Each subroutine will be wrapped by wrap_sub(). This argument specifies what
arguments to pass to wrap_sub().

Note: If you need different arguments for different subroutine, perhaps this
function is not for you. You can perform your own loop and wrap_sub().

_
        },
    },
    result => {
        summary => 'The original unwrapped subroutines and their metadata',
        description => <<'_',

Example:

    {
        func1 => {orig_sub=>..., orig_meta=>{...}},
        func2 => {orig_sub=>..., orig_meta=>{...}},
        ...
    }

If you need to restore the original subroutines (unwrap), save this somewhere.
Otherwise, you can discard this.

_
        schema=>'hash*',
    },
};
sub wrap_all_subs {
    my %args      = @_;
    my @caller    = CORE::caller(0);
    my $package   = $args{package}   // $caller[0];
    my $wrap_args = $args{wrap_args} // {};

    my $recap = {};

    no strict 'refs';

    my $metas = \%{"$package\::SPEC"} // {};
    for my $f (keys %$metas) {
        next unless $f =~ /\A\w+\z/;
        my $osub  = \&{"$package\::$f"};
        my $ometa = $metas->{$f};
        $recap->{$f} = {orig_sub => $osub, orig_meta => $ometa};
        my $res = wrap_sub(%$wrap_args, sub => $osub, meta => $ometa);
        return err(500, "Can't wrap $package\::$f", $res)
            unless $res->[0] == 200;
        $recap->{$f}{new_sub}  = $res->[2]{sub};
        $recap->{$f}{new_meta} = $res->[2]{meta};
    }

    no warnings 'redefine';

    # replace the originals
    for my $f (keys %$recap) {
        *{"$package\::$f"} = $recap->{$f}{new_sub};
        ${"$package\::SPEC"}{$f} = $recap->{$f}{new_meta};
    }

    [200, "OK", $recap];
}

1;
# ABSTRACT: A multi-purpose subroutine wrapping framework

=for Pod::Coverage ^(new|handle(meta)?_.+|wrap|add_.+|section_empty|indent|unindent|get_indent_level|select_section|push_lines)$

=head1 SYNOPSIS

 use Perinci::Sub::Wrapper qw(wrap_sub);
 my $res = wrap_sub(sub => sub {die "test\n"}, meta=>{...});
 my ($wrapped_sub, $meta) = ($res->[2]{sub}, $res->[2]{meta});
 $wrapped_sub->(); # call the wrapped function


=head1 DESCRIPTION

Perinci::Sub::Wrapper is an extensible subroutine wrapping framework. It works
by creating a single "large" wrapper function from a composite bits of code,
instead of using multiple small wrappers (a la Python's decorator). The
single-wrapper approach has the benefit of smaller function call overhead. You
can still wrap multiple times if needed.

This module is used to enforce Rinci properties, e.g. C<args> (by performing
schema validation before calling the function), C<timeout> (by doing function
call inside an C<eval()> and using C<alarm()> to limit the execution), or
C<retry> (by wrapping function call inside a simple retry loop).

It can also be used to convert argument passing style, e.g. from C<args_as>
C<array> to C<args_as> C<hash>, so you can call function using named arguments
even though the function accepts positional arguments, or vice versa.

There are many other possible uses.

This module uses L<Log::Any> for logging.


=head1 USAGE

Suppose you have a subroutine like this:

 sub gen_random_array {
     my %args = @_;
     my $len = $args{len} // 10;
     die "Length too big" if $len > 1000;
     die "Please specify at least length=1" if $len < 1;
     [map {rand} 1..$len];
 }

Wrapping can, among others, validate arguments and give default values. First
you add a L<Rinci> metadata to your subroutine:

 our %SPEC;
 $SPEC{gen_random_array} = {
     v => 1.1,
     summary=> 'Generate an array of specified length containing random values',
     args => {
         len => {req=>1, schema => ["int*" => between => [1, 1000]]},
     },
     result_naked=>1,
 };

You can then remove code that validates arguments and gives default values. You
might also want to make sure that your subroutine is run wrapped.

 sub gen_random_array {
     my %args = @_;
     die "This subroutine needs wrapping" unless $args{-wrapped}; # optional
     [map {rand} 1..$args{len}];
 }

Most wrapping options can also be put in C<_perinci.sub.wrapper.*>
attributes. For example:

 $SPEC{gen_random_array} = {
     v => 1.1,
     args => {
         len => {req=>1, schema => ["int*" => between => [1, 1000]]},
     },
     result_naked=>1,
     # skip validating arguments because sub already implements it
     "_perinci.sub.wrapper.validate_args" => 0,
 };
 sub gen_random_array {
     my %args = @_;
     my $len = $args{len} // 10;
     die "Length too big" if $len > 1000;
     die "Please specify at least length=1" if $len < 1;
     [map {rand} 1..$len];
 }

See also L<Dist::Zilla::Plugin::Rinci::Validate> which can insert validation
code into your Perl source code files so you can skip doing it again in
validation.


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

=head2 PERINCI_WRAPPER_VALIDATE_ARGS (bool, default 1)

Can be set to 0 to skip adding validation code. This provides a default for
C<validate_args> wrap_sub() argument.


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

By default, wrapper provides these functionality: checking invalid and unknown
arguments, argument value validation, exception trapping (C<eval {}>), and
result checking. If we turn off all these features except argument validation
(by adding options C<< allow_invalid_args=>1, trap=>0, validate_result=>0 >>)
call performance increases to around 0.47 mil/sec (for C<< $sub->() >> and 0.24
mil/sec (for C<< $sub->(a=>1) >>).

As more arguments are introduced in the schema and passed, and as argument
schemas become more complex, overhead will increase. For example, for 5 int
arguments being declared and passed, call performance is around 0.11 mil/sec.
Without passing any argument when calling, call performance is still around 0.43
mil/sec, indicating that the significant portion of the overhead is in argument
validation.


=head1 FAQ

=head2 How to display the wrapper code being generated?

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

=head2 How do I tell if I am being wrapped?

Wrapper code passes C<-wrapped> special argument with a true value. So you can
do something like this:

 sub my_sub {
     my %args = @_;
     return [412, "I need to be wrapped"] unless $args{-wrapped};
     ...
 }

Your subroutine needs accept arguments as hash/hashref.

=head2 caller() doesn't work from inside my wrapped code!

Wrapping adds at least one or two levels of calls: one for the wrapper
subroutine itself, the other is for the eval trap loop which can be disabled but
is enabled by default. The 'goto &NAME' special form, which can replace
subroutine and avoid adding another call level, cannot be used because wrapping
also needs to postprocess function result.

This poses a problem if you need to call caller() from within your wrapped code;
it will also be off by at least one or two.

The solution is for your function to use the caller() replacement, provided by
L<Perinci::Sub::Util>.

=head2 But that is not transparent!

True. The wrapped module needs to load and use that utility module explicitly.

An alternative is for Perinci::Sub::Wrapper to use L<Sub::Uplevel>. Currently
though, this module does not use Sub::Uplevel because, as explained in its
manpage, it is rather slow. If you don't use caller(), your subroutine actually
doesn't need to care if it is wrapped nor it needs "uplevel-ing".

=head2 How to ensure that users use the wrapped functions?

Sometimes you do rely on the functionalities provided by wrapping, most notably
argument validation, and you want to make sure that arguments are always
validated when users execute your function.

If your module use L<Perinci::Exporter>, users use()-ing your module will by
default import the wrapped version of your functions. But they can turn this off
via passing C<< wrap => 0 >>.

Another alternative is to embed the generated argument validation code directly
into your built source code. If you use L<Dist::Zilla>, take a look
L<Dist::Zilla::Plugin::Rinci::Validate>. This only covers the argument
validation functionality and not others, but this does not add levels of calls
or modifies the line numbers of your source code, so this solution is very
transparent.

I might write another dzil plugin which embeds the whole wrapper code into the
source code, should there be such a demand.


=head1 SEE ALSO

L<Perinci>

=cut
