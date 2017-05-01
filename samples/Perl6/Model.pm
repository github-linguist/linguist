use v6;

class Math::Model;

use Math::RungeKutta;
# TODO: only load when needed
use SVG;
use SVG::Plot;

has %.derivatives;
has %.variables;
has %.initials;
has @.captures is rw;

has %!inv = %!derivatives.invert;
# in Math::Model all variables are accessible by name
# in contrast Math::RungeKutta uses vectors, so we need
# to define an (arbitrary) ordering
# @!deriv-names holds the names of the derivatives in a fixed
# order, sod @!deriv-names[$number] turns the number into a name
# %!deriv-keying{$name} translates a name into the corresponding index
has @!deriv-names  =  %!inv.keys;
has %!deriv-keying =  @!deriv-names Z=> 0..Inf;

# snapshot of all variables in the current model
has %!current-values;

has %.results;
has @.time;

has $.numeric-error is rw = 0.0001;

my sub param-names(&c) {
    &c.signature.params».name».substr(1).grep({ $_ ne '_'});
}

method !params-for(&c) {
    param-names(&c).map( {; $_ => %!current-values{$_} } ).hash;
}

method topo-sort(*@vars) {
    my %seen;
    my @order;
    sub topo(*@a) {
        for @a {
            next if %!inv.exists($_) || %seen{$_} || $_ eq 'time';
            die "Undeclared variable '$_' used in model"
                unless %.variables.exists($_);
            topo(param-names(%.variables{$_}));
            @order.push: $_;
            %seen{$_}++;
        }
    }
    topo(@vars);
#    say @order.perl;
    @order;
}


method integrate(:$from = 0, :$to = 10, :$min-resolution = ($to - $from) / 20, :$verbose) {
    for %.derivatives -> $d {
        die "There must be a variable defined for each derivative, missing for '$d.key()'"
            unless %.variables.exists($d.key) || %!inv.exists($d.key);
        die "There must be an initial value defined for each derivative target, missing for '$d.value()'"
            unless %.initials.exists($d.value);
    }

    %!current-values       = %.initials;
    %!current-values<time> = $from;

    my @vars-topo          = self.topo-sort(%.variables.keys);
    sub update-current-values($time, @values) {
        %!current-values<time>          = $time;
        %!current-values{@!deriv-names} = @values;
        for @vars-topo {
            my $c = %.variables{$_};
            %!current-values{$_} = $c.(|self!params-for($c));
        }
    }

    my @initial = %.initials{@!deriv-names};

    sub derivatives($time, @values) {
        update-current-values($time, @values);
        my @r;
        for %!inv{@!deriv-names} {
            my $v = %.variables{$_};
            @r.push: $v.defined
                ?? $v(|self!params-for($v))
                !! %!current-values{$_};
        }
        @r;
    }

    @!time = ();
    for @.captures {
        %!results{$_} = [];
    }

    sub record($time, @values) {
        update-current-values($time, @values);
        @!time.push: $time;
        say $time if $verbose;

        for @.captures {
            %!results{$_}.push: %!current-values{$_};;
        }
    }

    record($from, %.initials{@!deriv-names});

    adaptive-rk-integrate(
        :$from,
        :$to,
        :@initial,
        :derivative(&derivatives),
        :max-stepsize($min-resolution),
        :do(&record),
        :epsilon($.numeric-error),
    );
    %!results;
}

method render-svg(
            $filename,
            :$x-axis = 'time',
            :$width = 800,
            :$height = 600,
            :$title = 'Model output') {
    my $f = open $filename, :w
            or die "Can't open file '$filename' for writing: $!";
    my @values = map { %!results{$_} }, @.captures.grep({ $_ ne $x-axis});
    my @x = $x-axis eq 'time' ?? @!time !! %!results{$x-axis}.flat;
    my $svg = SVG::Plot.new(
        :$width,
        :$height,
        :@x,
        :@values,
        :$title,
    ).plot(:xy-lines);
    $f.say(SVG.serialize($svg));
    $f.close;
    say "Wrote ouput to '$filename'";
}

# vim: ft=perl6
