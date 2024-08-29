{{
import * as ast from "./ast.js";
}}

{
const rules = new ast.Rules();
}

rulelist = (rule / empty)+ {
  rules.loc = location();
  return rules;
}

empty
  = WSP* comment
  / WSP* CRLF

// Allow starting whitespace before a rule
rule = WSP* name:rulename operator:defined_as children:elements c_nl {
  if (operator === "=/") {
    rules.addAlternate(name, children, location())
  } else {
    rules.addRule(name, children, location())
  }
}

rulename = $(ALPHA (ALPHA / DIGIT / "-")*)

// RFC 2045 uses :=
// RFC 1730 uses ::=
// `::=` is from the original BNF.
// I don't know where `:=` came from.
defined_as = c_wsp* @("=/" / ":"|0..2| "=") c_wsp*

elements = @alternation c_wsp*

comment = ";" $(WSP / VCHAR)* EOL

alternation = head:concatenation tail:(c_wsp* "/" c_wsp* @concatenation)* {
  if (tail.length > 0) {
    return new ast.Alternation([head, ...tail], location());
  }
  return head;
}

concatenation = head:repetition tail:(c_wsp+ @repetition)* {
  if (tail.length > 0) {
    return new ast.Concatenation([head, ...tail], location());
  }
  return head;
}

repetition = repeat:repeat? e:element {
  if (repeat) {
    return new ast.Repetition(repeat, e, location());
  }
  return e;
}

repeat
  = min:decnum? "*" max:decnum? {
    min |= 0;
    if (max === 0) {
      error("max must not be zero");
      return;
    }
    if ((typeof max === "number") && (max < min)) {
      error(`max must be greater than or equal to min, but ${max} < ${min}`)
      return;
    }
    return new ast.Repeat(min, max, location());
  }
  / n:decnum { return new ast.Repeat(n, n, location()); }

element
  = name:rulename { return rules.addRef(name, location()) }
  / group
  / option
  / char_val
  / num_val
  / prose_val

group = "(" c_wsp* alt:alternation c_wsp* ")" {
  return new ast.Group(alt, location());
}

option = "[" c_wsp* alt:alternation c_wsp* "]" {
  const loc = location();
  return new ast.Repetition(new ast.Repeat(0, 1, loc), alt, loc);
}

char_val
  = "%i"i? DQUOTE txt:$([\x20-\x21] / [\x23-\x7e])* DQUOTE {
    return new ast.CaseInsensitiveString(txt, location());
  }
  / "%s"i DQUOTE txt:$([\x20-\x21] / [\x23-\x7e])* DQUOTE {
    return new ast.CaseSensitiveString(txt, 0, location());
  }

num_val = "%" @(bin_val / dec_val / hex_val)

prose_val = "<" prose:$([\x20-\x3d] / [\x3f-\x7e] / CRLF)* ">" {
  return new ast.Prose(prose, location());
}

DQUOTE = "\""

bin_val
  = "b"i nums:binnum|2..,"."| {
    return new ast.CaseSensitiveString(
      nums.map(b => String.fromCodePoint(b)).join(""),
      2,
      location()
    );
  }
  / "b"i min:binnum "-" max:binnum {
    return ast.Range.create(2, min, max, location());
  }
  / "b"i n:binnum {
    return new ast.CaseSensitiveString(String.fromCodePoint(n), 2, location())
  }

dec_val
  = "d"i nums:decnum|2..,"."| {
    return new ast.CaseSensitiveString(
      nums.map(d => String.fromCodePoint(d)).join(""),
      10,
      location()
    );
  }
  / "d"i min:decnum "-" max:decnum {
    return ast.Range.create(10, min, max, location());
  }
  / "d"i n:decnum {
    return new ast.CaseSensitiveString(String.fromCodePoint(n), 10, location())
  }

hex_val
  = "x"i nums:hexnum|2..,"."| {
    return new ast.CaseSensitiveString(
      nums.map(x => String.fromCodePoint(x)).join(""),
      16,
      location()
    );
  }
  / "x"i min:hexnum "-" max:hexnum {
    return ast.Range.create(16, min, max, location());
  }
  / "x"i n:hexnum {
    return new ast.CaseSensitiveString(String.fromCodePoint(n), 10, location())
  }

binnum = b:$BIT+ { return parseInt(b, 2) }
decnum = d:$DIGIT+ { return parseInt(d, 10) }
hexnum = h:$HEXDIG+ { return parseInt(h, 16) }

c_wsp
  = c_nl WSP
  / WSP

c_nl
  = EOL
  / comment

EOL
  = CRLF
  / EOF

EOF
  = !.

BIT
  = "0"
  / "1"

HEXDIG
  = DIGIT
  / [A-F]i

ALPHA = [\x41-\x5a] / [\x61-\x7a]

DIGIT = [\x30-\x39]

WSP = SP / HTAB

SP = " "

HTAB = "\t"

VCHAR = [\x21-\x7e]

// Take any CR/LF sequence
CRLF
  = CR LF // Windows
  / LF    // Unix
  / CR    // Mac

CR = "\r"

LF = "\n"
