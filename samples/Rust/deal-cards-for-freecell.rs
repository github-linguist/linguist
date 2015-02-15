/*
 * Microsoft C Run-time-Library-compatible Random Number Generator
 * Copyright by Shlomi Fish, 2011.
 * Released under the MIT/X11 License
 * ( http://en.wikipedia.org/wiki/MIT_License ).
 * */

struct MSVC_Rand_Gen {
    seed: i32
}

impl MSVC_Rand_Gen {
    fn rand(&mut self) -> i32 {
        self.seed = ((self.seed * 214013 + 2531011) & 0x7FFFFFFF);
        return ((self.seed >> 16) & 0x7FFF);
    }
    fn max_rand(&mut self, mymax: i32) -> i32 {
        return self.rand() % mymax;
    }
    fn shuffle<T>(&mut self, deck: &mut [T]) {
        if deck.len() > 0 {
            let mut i = (deck.len() as i32) - 1;
            while i > 0 {
                let j = self.max_rand(i+1);
                vec::swap(deck, i as uint, j as uint);
                i = i-1;
            }
        }
    }
}

/*
 * Microsoft Windows Freecell / Freecell Pro boards generation.
 *
 * See:
 *
 * - http://rosettacode.org/wiki/Deal_cards_for_FreeCell
 *
 * - http://www.solitairelaboratory.com/mshuffle.txt
 *
 * Under MIT/X11 Licence.
 *
 * */


fn deal_ms_fc_board(seed: i32) -> ~str {
    let mut randomizer = MSVC_Rand_Gen { seed: seed, };
    let num_cols = 8;

    let mut columns = vec::from_elem(num_cols, ~[]);
    let mut deck = vec::from_fn(4*13, |i| i);

    let rank_strings = str::to_chars("A23456789TJQK");
    let suit_strings = str::to_chars("CDHS");

    randomizer.shuffle(deck);

    vec::reverse(deck);

    for uint::range(0, 52) |i| {
        columns[i % num_cols].push(deck[i]);
    };

    let render_card = |card: &uint| {
        let suit = card % 4;
        let rank = card / 4;

        fmt!("%c%c",rank_strings[rank], suit_strings[suit])
    };

    let render_column = |col: &~[uint]| {
        fmt!(": %s\n", str::connect((col.map(render_card)), " "))
    };

    return str::concat(columns.map(render_column));
}

fn main() {
    let args: ~[~str] = os::args();

    match uint::from_str(args[1]) {
        Some(x) => print(deal_ms_fc_board(x as i32)),
        None => println("I need a real number"),
    }
}
