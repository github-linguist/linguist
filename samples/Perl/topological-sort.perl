sub print_topo_sort {
    my %deps = @_;

    my %ba;
    while ( my ( $before, $afters_aref ) = each %deps ) {
        for my $after ( @{ $afters_aref } ) {
            $ba{$before}{$after} = 1 if $before ne $after;
            $ba{$after} ||= {};
        }
    }

    while ( my @afters = sort grep { ! %{ $ba{$_} } } keys %ba ) {
        print "@afters\n";
        delete @ba{@afters};
        delete @{$_}{@afters} for values %ba;
    }

    print !!%ba ? "Cycle found! ". join( ' ', sort keys %ba ). "\n" : "---\n";
}

my %deps = (
    des_system_lib => [qw( std synopsys std_cell_lib des_system_lib dw02
                                                        dw01 ramlib ieee )],
    dw01           => [qw( ieee dw01 dware gtech                         )],
    dw02           => [qw( ieee dw02 dware                               )],
    dw03           => [qw( std synopsys dware dw03 dw02 dw01 ieee gtech  )],
    dw04           => [qw( dw04 ieee dw01 dware gtech                    )],
    dw05           => [qw( dw05 ieee dware                               )],
    dw06           => [qw( dw06 ieee dware                               )],
    dw07           => [qw( ieee dware                                    )],
    dware          => [qw( ieee dware                                    )],
    gtech          => [qw( ieee gtech                                    )],
    ramlib         => [qw( std ieee                                      )],
    std_cell_lib   => [qw( ieee std_cell_lib                             )],
    synopsys       => [qw(                                               )],
);
print_topo_sort(%deps);
push @{ $deps{'dw01'} }, 'dw04'; # Add unresolvable dependency
print_topo_sort(%deps);
