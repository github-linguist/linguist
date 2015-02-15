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

fn to_roman(num: uint) -> ~str {
  for numeral in NUMERALS.iter() {
    if num >= numeral.value {
      return numeral.symbol + to_roman(num - numeral.value);
    }
  }

  return ~"";
}

fn main() {
  let nums = [2014, 1999, 25, 1666, 3888];
  for n in nums.iter() {
    println!("{:u} = {:s}", *n, to_roman(*n));
  }
}
