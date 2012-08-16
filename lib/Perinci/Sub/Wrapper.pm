package Perinci::Sub::Wrapper;

use 5.010;
use strict;
use warnings;
use Log::Any '$log';

use Perinci::Util qw(get_package_meta_accessor);
use Scalar::Util qw(blessed);

use Exporter qw(import);
our @EXPORT_OK = qw(wrap_sub wrap_all_subs wrapped caller);

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
        Purity(1)->Terse(1)->Deepcopy(1)->Dump;
    chomp $res;
    $res;
}

sub _known_sections {
    state $val = {
        # reserved by wrapper for setting Perl package and declaring 'sub {'
        OPEN_SUB => {order=>0},

        # for handlers to put stuffs right before eval. for example, 'timeout'
        # uses this to set ALRM signal handler.
        before_eval => {order=>10},

        # reserved by wrapper for generating 'eval {'
        OPEN_EVAL => {order=>20},

        # for handlers to put various checks before calling the wrapped
        # function, from data validation, argument conversion, etc. this is now
        # deprecated.
        before_call => {order=>30},

        # argument validation now uses this, to get more specific ordering
        before_call_arg_validation => {order=>30},

        before_call_after_arg_validation => {order=>35},

        # reserved by the wrapper for calling the function
        CALL => {order=>50},

        # for handlers to put various things after calling, from validating
        # result, enveloping result, etc.
        after_call => {order=>60},

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

sub _errif {
    my ($self, $c_status, $c_msg, $c_cond) = @_;
    $self->push_lines("if ($c_cond) {");
    $self->indent;
    $self->push_lines(
        # we set $res here when we return from inside the eval block
        '$res = ' . "[$c_status, $c_msg]" . ';',
        'goto RETURN_RES;');
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

    my $v = $self->{_meta}{features} // {};

    $self->select_section('before_call_after_arg_validation');

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
    # that the wrapped function expects.
    #
    # This setup is optimal when both the function and generated wrapper accept
    # 'hash', but suboptimal for other cases (especially positional ones, as
    # they have to undergo a round-trip to hash even when both accept 'array').
    # This will be rectified in the future.

    $self->select_section('before_call_arg_validation');

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
    $self->push_lines('', "# convert arguments for wrapped function ($value)")
        unless $v eq $value;
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

sub handlemeta_args { {v=>2, prio=>10, convert=>0} }
sub handle_args {
    require Data::Sah;

    my ($self, %args) = @_;

    my $v = $self->{_meta}{args};
    return unless $v;

    my $rm = $self->{_args}{remove_internal_properties};

    # validation
    for my $an (keys %$v) {
        my $as = $v->{$an};
        for (keys %$as) {
            if (/\A_/) {
                delete $as->{$_} if $rm;
                next;
            }
            # check known arg key
            die "Unknown arg spec key '$_' for arg '$an'" unless
                /\A(
                     summary|description|tags|default_lang|
                     schema|req|pos|greedy|
                     completion|
                     cmdline_aliases|
                     cmdline_src
                 )(\..+)?\z/x;
            # XXX actually only summary/description can have .alt.lang.XXX

            # XXX there should only one argument with src=stdin/stdin_or_files.

            # XXX there should not be another argument with req=>1 + pos=>0,
            # there must not be one if there is argument with src.
        }
    }

    # normalize schema
    if ($self->{_args}{normalize_schemas}) {
        for my $an (keys %$v) {
            my $as = $v->{$an};
            if ($as->{schema}) {
                $as->{schema} =
                    Data::Sah::normalize_schema($as->{schema});
            }
            my $als = $as->{cmdline_aliases};
            if ($als) {
                for my $al (keys %$als) {
                    if ($als->{$al}{schema}) {
                        $als->{$al}{schema} =
                            Data::Sah::normalize_schema($als->{$al}{schema});
                    }
                }
            }
        }
    }
}

# XXX not implemented yet
sub handlemeta_result { {v=>2, prio=>50, convert=>0} }
sub handle_result {
    require Data::Sah;

    my ($self, %args) = @_;

    my $v = $self->{_meta}{result};
    return unless $v;

    # normalize schema
    if ($self->{_args}{normalize_schemas}) {
        if ($v->{schema}) {
            $v->{schema} = Data::Sah::normalize_schema($v->{schema});
        }
    }

    my $rm = $self->{_args}{remove_internal_properties};
    for my $k (keys %$v) {
        if ($k =~ /^_/) {
            delete $v->{$k} if $rm;
        }
    }

    # XXX validation not implemented yet
}

sub handlemeta_result_naked { {v=>2, prio=>100, convert=>1} }
sub handle_result_naked {
    my ($self, %args) = @_;

    # XXX option to check whether result is really naked

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

sub wrap {
    require Data::Clone;
    require Scalar::Util;

    my ($self, %args) = @_;
    $log->tracef("-> wrap(%s)", \%args);

    my $sub      = $args{sub} or return [400, "Please specify sub"];
    $args{meta} or return [400, "Please specify meta"];
    my $meta     = Data::Clone::clone($args{meta});
    $args{convert} //= {};
    my $convert  = $args{convert};
    $args{trap} //= 1;
    my $trap     = $args{trap};
    $args{compile} //= 1;
    my $compile  = $args{compile};
    $args{normalize_schemas} //= 1;
    my $normalize_schemas = $args{normalize_schemas};
    $args{remove_internal_properties} //= 1;
    my $remove_internal_properties = $args{remove_internal_properties};

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

    # put the sub in a named variable, so it can be accessed by the wrapper
    # code.
    my $subname = $comppkg . "::sub".Scalar::Util::refaddr($sub);
    { no strict 'refs'; ${$subname} = $sub; }

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
    $self->select_section('OPEN_SUB');
    $self->push_lines(
        "package $comppkg;",
        'sub {');
    $self->indent;
    $self->push_lines(
        'my ($res, $eval_err);');

    $meta->{args_as} //= "hash";

    # XXX validate metadata first to filter invalid properties

    my %props = map {$_=>1} keys %$meta;
    $props{$_} = 1 for keys %$convert;

    my %handler_args;
    my %handler_metas;
    for my $k0 (keys %props) {
        if ($k0 =~ /^_/) {
            delete $meta->{$k0} if $remove_internal_properties;
            next;
        }
        my $k = $k0;
        $k =~ s/\..+//;
        next if $handler_args{$k};
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
        $log->tracef("Calling %s(%s) ...", $meth, $ha);
        $self->$meth(args=>\%args, meta=>$meta, %$ha);
    }

    $self->select_section('CALL');
    $self->push_lines('$res = $'.$subname."->(".$self->{_args_token}.");");
    if ($self->{_args}{meta}{result_naked}) {
        # internally we always use result envelope, so let's envelope this
        # temporarily.
        $self->push_lines('# add temporary envelope',
                          '$res = [200, "OK", $res];');
    }

    if ($trap || $self->_needs_eval) {
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
    if ($log->is_trace) {
        require SHARYANTO::String::Util;
        $log->tracef("wrapper source code:\n%s",
                     SHARYANTO::String::Util::linenum($source));
    }
    my $result = {source=>$source};
    if ($compile) {
        my $wrapped = eval $source;
        die "BUG: Wrapper code can't be compiled: $@" if $@ || !$wrapped;

        # mark the wrapper with bless, to detect double wrapping attempt
        bless $wrapped, $comppkg;

        $result->{sub}  = $wrapped;
        $result->{meta} = $meta;
    }
    $log->tracef("<- wrap()");
    [200, "OK", $result];
}

$SPEC{wrap_sub} = {
    v => 1.1,
    summary => 'Wrap subroutine to do various things, '.
        'like enforcing Rinci properties',
    description => <<'_',

Will wrap subroutine and bless the generated wrapped subroutine (by default into
'Perinci::Sub::Wrapped') as a way of marking that the subroutine is a wrapped
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
            req => 1, pos => 0,
        },
        sub_name => {
            schema => 'str*',
            summary => 'The name of the code, e.g. Foo::func',
            description => <<'_',

It is a good idea to supply this so that wrapper code can display this
information when they need to (e.g. see Perinci::Sub::Property::dies_on_error).

_
        },
        meta => {
            schema => 'hash*',
            summary => 'The function metadata',
            req =>1, pos => 1,
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
        normalize_schema => {
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

    my $res = get_package_meta_accessor(package=>$package);
    return [500, "Can't get meta accessor: $res->[0] - $res->[1]"]
        unless $res->[0] == 200;
    my $ma = $res->[2];

    my $recap = {};

    no strict 'refs';

    my $metas = $ma->get_all_metas($package);
    for my $f (keys %$metas) {
        next unless $f =~ /\A\w+\z/;
        my $osub  = \&{"$package\::$f"};
        my $ometa = $metas->{$f};
        $recap->{$f} = {orig_sub => $osub, orig_meta => $ometa};
        $res = wrap_sub(%$wrap_args, sub => $osub, meta => $ometa);
        return [500, "Can't wrap $package\::$f: $res->[0] - $res->[1]"]
            unless $res->[0] == 200;
        $recap->{$f}{new_sub}  = $res->[2]{sub};
        $recap->{$f}{new_meta} = $res->[2]{meta};
    }

    no warnings 'redefine';

    # replace the originals
    for my $f (keys %$recap) {
        *{"$package\::$f"} = $recap->{$f}{new_sub};
        $ma->set_meta($package, $f, $recap->{$f}{new_meta});
    }

    [200, "OK", $recap];
}

$SPEC{wrapped} = {
    v => 1.1,
    summary => 'Check whether we are wrapped',
    description => <<'_',

This function is to be run inside a subroutine to check if *that* subroutine is
wrapped by Perinci::Sub::Wrapper. For example:

    sub some_sub {
        print "I'm wrapped" if wrapped();
    }

See also this package's caller(), a wrapper-aware replacement for Perl's builtin
caller().

_
    args => {
    },
    args_as => 'array',
    result => {
        schema=>'bool*',
    },
    result_naked => 1,
};
sub wrapped {
    no strict 'refs';

    # should i check whether *i* am wrapped first? because that would throw off
    # the stack counting.

    my @c1 = CORE::caller(1); # we want to check our *caller's* caller
    my @c2 = CORE::caller(2); # and its caller

    #use Data::Dump; dd \@c1; dd \@c2;

    my $p = $default_wrapped_package;

    $c1[0] eq $p &&
    $c1[1] =~ /^\(eval/ &&
    $c1[4] &&
    $c2[0] eq $p &&
    $c2[1] =~ /^\(eval/ &&
    $c2[3] eq '(eval)' &&
    !$c2[4] &&
    1;
}

$SPEC{caller} = {
    v => 1.1,
    summary => 'Wrapper-aware caller()',
    description => <<'_',

Just like Perl's builtin caller(), except that this one will ignore wrapper code
in the call stack. You should use this if your code is potentially wrapped.

_
    args => {
        n => {
            pos => 0,
        },
    },
    args_as => 'array',
    result => {
        schema=>'bool*',
    },
    result_naked => 1,
};
sub caller {
    my $n0 = shift;
    my $n  = $n0 // 0;

    my @r;
    my $i =  0;
    my $j = -1;
    while ($i <= $n+1) { # +1 for this sub itself
        $j++;
        @r = CORE::caller($j);
        last unless @r;
        if ($r[0] eq $default_wrapped_package && $r[1] =~ /^\(eval /) {
            next;
        }
        $i++;
    }

    return unless @r;
    return defined($n0) ? @r : $r[0];
}

1;
# ABSTRACT: A multi-purpose subroutine wrapping framework

=for Pod::Coverage ^(new|handle(meta)?_.+|wrap|add_.+|section_empty|indent|unindent|select_section|push_lines)$

=head1 SYNOPSIS

 use Perinci::Sub::Wrapper qw(wrap_sub);
 my $res = wrap_sub(sub => sub {die "test\n"}, meta=>{...});
 my ($wrapped, $meta) = ($res->[2]{sub}, $res->[2]{meta});
 $wrapped->(); # call the wrapped function


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
even though the function accepts positional arguments.

There are many other possible uses.

This module uses L<Log::Any> for logging.


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


=head1 METHODS

The OO interface is only used internally or when you want to extend the wrapper.


=head1 FAQ

=head2 caller() doesn't work from inside my wrapped code!

Wrapping adds at least one or two levels of calls: one for the wrapper
subroutine itself, the other is for the eval trap loop which can be disabled but
is enabled by default. The 'goto &NAME' special form, which can replace
subroutine and avoid adding another call level, cannot be used because wrapping
also needs to postprocess function result.

This poses a problem if you need to call caller() from within your wrapped code;
it will be off by at least one or two also.

The solution is for your function to use the caller() replacement, provided by
this module.

=head2 But that is not transparent!

True. The wrapped function needs to load and use this module's wrapper
deliberately.

An alternative is for Perinci::Sub::Wrapper to use L<Sub::Uplevel>. This module
does not use it because, as explained in its manpage, Sub::Uplevel is rather
slow. If you don't use caller(), your subroutine actually doesn't need to care
if it is wrapped nor it needs "uplevel-ing".


=head1 SEE ALSO

L<Perinci>

=cut
