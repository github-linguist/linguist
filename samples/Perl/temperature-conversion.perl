my %scale = (
    Celcius    => { factor => 1  , offset => -273.15 },
    Rankine    => { factor => 1.8, offset =>    0    },
    Fahrenheit => { factor => 1.8, offset => -459.67 },
);

print "Enter a temperature in Kelvin: ";
chomp(my $kelvin = <STDIN>);
die "No such temperature!\n" unless $kelvin > 0;

foreach (sort keys %scale) {
    printf "%12s:%8.2f\n", $_, $kelvin * $scale{$_}{factor} + $scale{$_}{offset};
}
