struct RomanNumeral {
  symbol: &'static str,
  value: uint
}

static NUMERALS: [RomanNumeral, ..13] = [
  RomanNumeral {symbol: "M",  value: 1000},
  RomanNumeral {symbol: "CM", value: 900},
  RomanNumeral {symbol: "D",  value: 500},
  RomanNumeral {symbol: "CD", value: 400},
  RomanNumeral {symbol: "C",  value: 100},
  RomanNumeral {symbol: "XC", value: 90},
  RomanNumeral {symbol: "L",  value: 50},
  RomanNumeral {symbol: "XL", value: 40},
  RomanNumeral {symbol: "X",  value: 10},
  RomanNumeral {symbol: "IX", value: 9},
  RomanNumeral {symbol: "V",  value: 5},
  RomanNumeral {symbol: "IV", value: 4},
  RomanNumeral {symbol: "I",  value: 1}
];

fn to_hindu(roman: &str) -> uint {
  for numeral in NUMERALS.iter() {
    if roman.starts_with(numeral.symbol) {
      return numeral.value + to_hindu(roman.slice_from(numeral.symbol.len()));
    }
  }

  return 0;
}

fn main() {
  let roms = ["MMXIV", "MCMXCIX", "XXV", "MDCLXVI", "MMMDCCCLXXXVIII"];
  for r in roms.iter() {
    println!("{:s} = {:u}", *r, to_hindu(*r));
  }
}
