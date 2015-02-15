my @heading = qw(X Y Z);
my $rows = 5;
print   '<table><thead><td>',
        (map { "<th>$_</th>" } @heading),
        "</thead><tbody>";

for (1 .. $rows) {
        print   "<tr><th>$_</th>",
                (map { "<td>".int(rand(10000))."</td>" } @heading),
                "</tr>";
}

print   "</tbody></table>";
