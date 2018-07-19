my ($board_size, @occupied, @past, @solutions);

sub try_column {
        my ($depth, @diag) = shift;
        if ($depth == $board_size) {
                push @solutions, "@past\n";
                return;
        }

        # @diag: marks cells diagonally attackable by any previous queens.
        #        Here it's pre-allocated to double size just so we don't need
        #        to worry about negative indices.
        $#diag = 2 * $board_size;
        for (0 .. $#past) {
                $diag[ $past[$_] + $depth - $_ ] = 1;
                $diag[ $past[$_] - $depth + $_ ] = 1;
        }

        for my $row (0 .. $board_size - 1) {
                next if $occupied[$row] || $diag[$row];

                # @past:     row numbers of previous queens
                # @occupied: rows already used. This gets inherited by each
                #            recursion so we don't need to repeatedly look them up
                push @past, $row;
                $occupied[$row] = 1;

                try_column($depth + 1);

                # clean up, for next recursion
                $occupied[$row] = 0;
                pop @past;
        }
}

$board_size = 12;  # takes a minute or so, 14,200 solutions
try_column(0);

local $" = "\n";
print @solutions;
print "total ", scalar(@solutions), " solutions\n";
