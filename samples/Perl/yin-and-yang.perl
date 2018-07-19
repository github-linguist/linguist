sub circle {
        my ($radius, $cx, $cy, $fill, $stroke) = @_;
        print   "<circle cx='$cx' cy='$cy' r='$radius' ",
                "fill='$fill' stroke='$stroke' stroke-width='1'/>\n";
}

sub yin_yang {
        my ($rad, $cx, $cy, %opt) = @_;
        my ($c, $w) = (1, 0);
        $opt{fill}   //= 'white';
        $opt{stroke} //= 'black';
        $opt{recurangle} //= 0;

        print "<g transform='rotate($opt{angle}, $cx, $cy)'>"
                if $opt{angle};

        if ($opt{flip}) { ($c, $w) = ($w, $c) };

        circle($rad, $cx, $cy, $opt{fill}, $opt{stroke});

        print "<path d='M $cx ", $cy + $rad, "A ",
                $rad/2, " ", $rad/2, " 0 0 $c $cx $cy ",
                $rad/2, " ", $rad/2, " 0 0 $w $cx ", $cy - $rad, " ",
                $rad,   " ", $rad,   " 0 0 $c $cx ", $cy + $rad, " ",
                "z' fill='$opt{stroke}' stroke='none' />";

        if ($opt{recur} and $rad > 1) {
                # recursive "eyes" are slightly larger
                yin_yang($rad/4, $cx, $cy + $rad/2, %opt,
                                angle   => $opt{recurangle},
                                fill    => $opt{stroke},
                                stroke  => $opt{fill}   );
                yin_yang($rad/4, $cx, $cy - $rad/2, %opt,
                                angle   => 180 + $opt{recurangle});
        } else {
                circle($rad/5, $cx, $cy + $rad/2, $opt{fill}, $opt{stroke});
                circle($rad/5, $cx, $cy - $rad/2, $opt{stroke}, $opt{fill});
        }
        print "</g>" if $opt{angle};
}

print <<'HEAD';
<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN"
        "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">
<svg xmlns="http://www.w3.org/2000/svg" version="1.1"
        xmlns:xlink="http://www.w3.org/1999/xlink">
HEAD

yin_yang(200, 250, 250, recur=>1,
         angle=>0, recurangle=>90, fill=>'white', stroke=>'black');
yin_yang(100, 500, 500);

print "</svg>"
