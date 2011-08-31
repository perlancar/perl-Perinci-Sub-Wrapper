package Sub::Spec::Wrapper::Clause::timeout;

sub before_call {
    my ($wrapper, $val) = @_;
    if ($val > 0) {
        $wrapper->add_line(
            'local $SIG{ALRM} = sub { die "Timed out\n" };',
            'alarm('.($val+0).');'
        );
    }
}

sub after_call {
    my ($wrapper, $val) = @_;
    if ($val > 0) {
        $wrapper->add_line('alarm(0);');
    }
}

1;
