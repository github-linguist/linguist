// The `program` scope defines the data types, functions, and state associated with the `tictactoe` program.
program tictactoe.aleo {
    // A row in a tic tac toe board.
    // - `c1` : The first entry in the row.
    // - `c2` : The second entry in the row.
    // - `c3` : The third entry in the row.
    // A valid entry is either 0, 1, or 2, where 0 is empty, 1 corresponds to player 1, and 2 corresponds to player 2.
    // Any other values are invalid.
    struct Row {
        c1: u8,
        c2: u8,
        c3: u8
    }

    // A tic tac toe board.
    // - `r1` : The first row in the board.
    // - `r2` : The second row in the board.
    // - `r3` : The third row in the board.
    struct Board {
        r1: Row,
        r2: Row,
        r3: Row,
    }

    // Returns an empty board.
    transition new() -> Board {
        return Board {
            r1: Row { c1: 0u8, c2: 0u8, c3: 0u8 },
            r2: Row { c1: 0u8, c2: 0u8, c3: 0u8 },
            r3: Row { c1: 0u8, c2: 0u8, c3: 0u8 },
        };
    }

    // Returns `true` if there exists a row, column, or diagonal with all entries occupied by the same player.
    // - `b` : A tic tac toe board.
    // - `p` : A number corresponding to a player.
    function check_for_win(b: Board, p: u8) -> bool {
        return
            (b.r1.c1 == p && b.r1.c2 == p && b.r1.c3 == p) || // row 1
            (b.r2.c1 == p && b.r2.c2 == p && b.r2.c3 == p) || // row 2
            (b.r3.c1 == p && b.r3.c3 == p && b.r3.c3 == p) || // row 3
            (b.r1.c1 == p && b.r2.c1 == p && b.r3.c1 == p) || // column 1
            (b.r1.c2 == p && b.r2.c3 == p && b.r3.c2 == p) || // column 2
            (b.r1.c3 == p && b.r2.c3 == p && b.r3.c3 == p) || // column 3
            (b.r1.c1 == p && b.r2.c2 == p && b.r3.c3 == p) || // diagonal
            (b.r1.c3 == p && b.r2.c2 == p && b.r3.c1 == p); // other diagonal
    }

    // Returns an updated tic tac toe board with a move made by a player.
    // Returns a `u8` corresponding to the player who won the game, or 0 if no one has won yet.
    // - `player` : A number corresponding to a player.
    // - `row`    : The row of the move.
    // - `col`    : The column of the move.
    // - `board`  : A tic tac toe board.
    // Assumes that `player` is either 1 or 2.
    // Assumes that `row` and `col` are valid indices into the board.
    // If an entry is already occupied, the move is invalid and the board is returned unchanged.
    transition make_move(player: u8, row: u8, col: u8, board: Board) -> (Board, u8) {
        // Check that inputs are valid.
        assert(player == 1u8 || player == 2u8);
        assert(1u8 <= row && row <= 3u8);
        assert(1u8 <= col && col <= 3u8);

        // Unpack the entries in the board into variables.
        let r1c1: u8 = board.r1.c1;
        let r1c2: u8 = board.r1.c2;
        let r1c3: u8 = board.r1.c3;
        let r2c1: u8 = board.r2.c1;
        let r2c2: u8 = board.r2.c2;
        let r2c3: u8 = board.r2.c3;
        let r3c1: u8 = board.r3.c1;
        let r3c2: u8 = board.r3.c2;
        let r3c3: u8 = board.r3.c3;

        // Update the appropriate entry with the given move.
        if row == 1u8 && col == 1u8 && r1c1 == 0u8 {
            r1c1 = player;
        } else if row == 1u8 && col == 2u8 && r1c2 == 0u8 {
            r1c2 = player;
        } else if row == 1u8 && col == 3u8 && r1c3 == 0u8 {
            r1c3 = player;
        } else if row == 2u8 && col == 1u8 && r2c1 == 0u8 {
            r2c1 = player;
        } else if row == 2u8 && col == 2u8 && r2c2 == 0u8 {
            r2c2 = player;
        } else if row == 2u8 && col == 3u8 && r2c3 == 0u8 {
            r2c3 = player;
        } else if row == 3u8 && col == 1u8 && r3c1 == 0u8 {
            r3c1 = player;
        } else if row == 3u8 && col == 2u8 && r3c2 == 0u8 {
            r3c2 = player;
        } else if row == 3u8 && col == 3u8 && r3c3 == 0u8 {
            r3c3 = player;
        }

        // Construct the updated game board.
        let updated: Board = Board {
            r1: Row { c1: r1c1, c2: r1c2, c3: r1c3 },
            r2: Row { c1: r2c1, c2: r2c2, c3: r2c3 },
            r3: Row { c1: r3c1, c2: r3c2, c3: r3c3 },
        };

        // Check if the game is over.
        if check_for_win(updated, 1u8) {
            return (updated, 1u8);
        } else if check_for_win(updated, 2u8) {
            return (updated, 2u8);
        } else {
            return (updated, 0u8);
        }
    }

}
