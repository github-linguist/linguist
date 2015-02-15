// rustc 0.10-pre, 24th Feb
static side: i8 = 8;
static queens: i8 = 8; //change side and queens to modify parameters

fn place(mut board: [i8,..side*side], ix:i8) -> Option<[i8,..side*side]> {
    if board[ix] == 0 {
        return None
    };
    board[ix] = -1;
    let i1 = ix/side;
    let j1 = ix % side;
    for k in range(1,side) {
        let mut loc :i8 = i1 * side + k;
        board[loc] = 0;
        loc = k * side + j1;
        board[loc] = 0;
        loc = (i1-k) * side + (j1-k);
        if loc / side == i1 -k && loc % side == j1 - k && loc != ix && loc >= 0 { board[loc] = 0 };
        loc = loc + 2 * k;
        if loc / side == i1 - k && loc % side == j1 + k && loc != ix && loc >= 0 { board[loc] = 0};
        loc = loc + 2 * side * k;
        if loc / side == i1 + k && loc % side == j1 + k && loc != ix && loc < side*side { board[loc] = 0};
        loc = loc - 2 * k;
        if loc / side == i1 + k && loc % side == j1 - k && loc != ix && loc < side*side { board[loc] = 0};
    }
    Some(board)
}

fn tryplace(b : [i8,..side*side],ix:i8, nq: i8, mut score: u32) -> u32 {
    if nq == queens { return score + 1 }
    for ind in range(ix, side*side) {
        score = match place(b, ind) {
            Some(b2) => tryplace(b2, ind+1, nq+1, score),
            None() => score
        };
    }
    return score
}

fn main() {
    let b : [i8, ..side*side] = [1,..side*side];
    let score = tryplace(b, 0, 0, 0);
    println!("{}", score)
}
