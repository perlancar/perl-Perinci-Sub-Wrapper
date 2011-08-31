package Sub::Spec::Wrapper::Clause::result_naked;

sub after_eval {
    my ($wrapper, $val) = @_;
    if ($val) {
        $wrapper->add_line('$res = [200, "OK", $res];');
    }
}

1;

