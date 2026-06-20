```
/*
 * Copyright (c) 2026 Jonathan Brachthäuser and contributors
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
```

# Pull-based Lexing
In this case study, we show how to implement a _pull-based_ lexer
in terms of effect handlers.

```effekt:prelude
import regex
```

## Tokens and Positions
First we define the datatypes to represent lexemes (tokens) and positions in the input stream:
```
record Position(line: Int, col: Int, index: Int)

type TokenKind { Number(); Ident(); Punct(); Space() }

def infixEq(t1: TokenKind, t2: TokenKind): Bool =
  (t1, t2) match {
    case (Number(), Number()) => true
    case (Ident(), Ident()) => true
    case (Punct(), Punct()) => true
    case (Space(), Space()) => true
    case _ => false
  }

record Token(kind: TokenKind, text: String, position: Position)
```
Tokens simply are tagged with a token type (distinguishing numbers, identifiers, and punctuation),
the original text of the token and its position.

## The Lexer Effect
Next, we define the interface to the lexer as an effect signature.
```
interface Lexer {
  def peek(): Option[Token]
  def next(): Token
}
```
it consists of two effect operations, one to inspect the next token without consuming it (`peek`)
and one operation to advance in the stream of tokens. This describes the interface of a _pull-based_ lexer as a stream of tokens. Lexemes are only processed on demand.
An example program using the lexer effect is:

```
def example1() = {
  val t1 = do next();
  val t2 = do next();
  val t3 = do next();
  (t1, t2, t3)
}
```

## Handling the Lexer Effect with a given List
A dummy lexer reading lexemes from a given list can be implemented as a handler for the `Lexer` effect. The definition uses the effect `LexerError` to signal the end of the input stream:
```
effect LexerError(msg: String, pos: Position): Nothing
val dummyPosition = Position(0, 0, 0)

def lexerFromList[R](l: List[Token]) { program: => R / Lexer }: R / LexerError = {
  var in = l;
  try { program() } with Lexer {
    def peek() = in match {
      case Nil() => resume(None())
      case Cons(tok, _) => resume(Some(tok))
    }
    def next() = in match {
      case Nil() => do LexerError("Unexpected end of input", dummyPosition)
      case Cons(tok, _) => resume(tok)
    }
  }
}
```
We define a separate handler to report lexer errors to the console:
```
def report { prog: => Unit / LexerError }: Unit =
  try { prog() } with LexerError { (msg, pos) =>
    println(pos.line.show ++ ":" ++ pos.col.show ++ " " ++ msg)
  }
```
Given a list of example tokens
```
val exampleTokens = [
  Token(Ident(), "foo", dummyPosition),
  Token(Punct(), "(", dummyPosition),
  Token(Punct(), ")", dummyPosition)
]
```
we can compose the two handlers to run our example consumer:
```effekt:repl
report {
  exampleTokens.lexerFromList {
    println(example1().show)
  }
}
```

## Handling the Lexer Effect by Processing a String
Of course, we can also implement a handler for our `Lexer` effect that _actually_
processes an input and computes the tokens contained therein.

This time, we use a number of different regular expressions to recognize lexemes.
First, we define the different token types as a list of pairs of regular expressions and token types.

```
record TokenRx(kind: TokenKind, rx: Regex)

val tokenDesriptors = [
  TokenRx(Number(), "^[0-9]+".regex),
  TokenRx(Ident(),  "^[a-zA-Z]+".regex),
  TokenRx(Punct(),  "^[=,.()\\[\\]{}:]".regex),
  TokenRx(Space(),  "^[ \t\n]+".regex)
]
```

Now, we can define the function `lexer` which receives a string as input as well as a computation `prog` for which it handles the `Lexer` effect:

```
def lexer[R](in: String) { prog: => R / Lexer } : R / LexerError = {
  // Additionally, we keep track of the current position in the input stream, by maintaining
  // three mutable variables for the zero based index, and one-based column and line position.
  var index = 0
  var col = 1
  var line = 1
  // A few local helper functions ease the handling of the input stream.
  // At the same time, we need to keep track of the line information.
  def position() = Position(line, col, index)
  def input() = in.substring(index)
  def consume(text: String): Unit = {
    with ignore[MissingValue]
    val lines = text.split("\n")
    val offset = lines.last.length
    // compute new positions
    index = index + text.length
    line = line + lines.size - 1
    if (lines.size == 1) { col = col + text.length } else { col = offset }
  }
  def eos(): Bool = index >= in.length
  // The function `tryMatch` applies a given token description to the current position of
  // the input stream, without advancing it. Its companion `tryMatchAll` returns the first token
  // matched by any of the matches in the given description list.
  def tryMatch(desc: TokenRx): Option[Token] =
      desc.rx.exec(input()).map { m => Token(desc.kind, m.matched, position()) }

  def tryMatchAll(descs: List[TokenRx]): Option[Token] = descs match {
    case Nil() => None()
    case Cons(desc, descs) => tryMatch(desc).orElse { tryMatchAll(descs) }
  }
  // Now defining the lexer is trivial. We just need to use `tryMatchAll` and either consume
  // the input, or not.
  try { prog() } with Lexer {
    def peek() = resume(tryMatchAll(tokenDesriptors))
    def next() =
      if (eos())
        do LexerError("Unexpected EOS", position())
      else {
        val tok = tryMatchAll(tokenDesriptors).getOrElse {
          do LexerError("Cannot tokenize input", position())
        }
        consume(tok.text)
        resume(tok)
      }
  }
}
```
Running our above consumer with the string `"foo()"`:
```effekt:repl
  report {
    lexer("foo()") {
      println(example1().show)
    }
  }
```

## Whitespace Skipping
Interestingly, a whitespace skipping lexer can be implemented as a _effect transformer_. That is, a handler that (partially) re-raises effect operations.

```
def skipSpaces(): Unit / Lexer = do peek() match {
  case None() => ()
  case Some(Token(Space(), _, _)) => do next(); skipSpaces()
  case _ => ()
}

def skipWhitespace[R] { prog: => R / Lexer }: R / Lexer =
  try { prog() } with Lexer {
    def peek() = { skipSpaces(); resume(do peek()) }
    def next() = { skipSpaces(); resume(do next()) }
  }
```
The handler `skipWhitespace` simply skips all spaces by using the `Lexer` effect itself.

```effekt:repl
  report {
    lexer("foo (   \n  )") {
      skipWhitespace {
        println(example1().show)
      }
    }
  }
```

---
layout: docs
title: Parser
permalink: docs/casestudies/parser
redirect_to: docs/casestudies/frontend#parsing
---

# Parsing
In this case study, we show how to implement a parser, using the lexer from the
[Lexer case study](lexer).

---

Parsers can be expressed by using the lexer effect and process the token stream. To model different alternatives in the grammar, we use the following effect for non-determinism:

```
interface Nondet {
  def alt(): Bool
  def fail(msg: String): Nothing
}

effect Parser = { Nondet, Lexer }
```

## Parser Combinators
Given these two effects, we can readily define a host of (imperative) parser combinators.
We start by the simplest one, which applies a predicate to the next element in the
input stream and fails, if it does not match.

```
def accept { p: Token => Bool } : Token / Parser = {
  val got = do next();
  if (p(got)) got
  else do fail("Unexpected token " ++ show(got))
}
```

Using `accept`, we can define parsers for the different token types.
```
def any() = accept { t => true }
def accept(exp: TokenKind) = accept { t => t.kind == exp }
def ident() = accept(Ident()).text
def number() = accept(Number()).text
def punct(p: String) = {
  val tok = accept(Punct())
  if (tok.text == p) ()
  else do fail("Expected " ++ p ++ " but got " ++ tok.text)
}
def kw(exp: String): Unit / Parser = {
  val got = ident();
  if (got == exp) ()
  else do fail("Expected keyword " ++ exp ++ " but got " ++ got)
}
```
Using the effect for non-deterministic choice `alt`, we can model alternatives, optional matches and various repetitions:
```
def or[R] { p: => R } { q: => R } =
  if (do alt()) { p() } else { q() }

def opt[R] { p: => R }: Option[R] / Parser =
  or { Some(p()) } { None() }

def many { p: => Unit }: Unit / Parser =
  or { some { p() } } { () }

def some { p: => Unit }: Unit / Parser =
  { p(); many { p() } }
```

## Example: A Simple Expression Language
We illustrate the usage of the above parser combinators by parsing a simple
expressions language.

```
type Tree {
  Lit(value: Int)
  Var(name: String)
  Let(name: String, binding: Tree, body: Tree)
  App(name: String, arg: Tree)
}
```

Let us start by defining the parser for numeric literals.
```
def parseNum(): Tree / Parser = {
  val numText = number();
  attempt { Lit(numText.toInt) } { do fail("Expected number, but cannot convert input to integer: " ++ numText) }
}
```
We simply call the parser for `number()` and try to convert the
resulting string to an intenger.

```
def parseVar(): Tree / Parser =
  Var(ident())

def parseAtom() = or { parseVar() } { parseNum() }
```
Parsing variables is simply a matter of reusing the `ident` parser and changing the
result. Users of monadic parser combinator libraries might be delighted to see, that we
do not need to use `map` or something similar to transform the result. The parser is
simply written in direct style, still offering great flexibility in modifying the
semantics of effects.

Similarly, we can write parsers for let bindings, by sequentially composing
our existing parsers:

```
def parseLet(): Tree / Parser = {
  kw("let");
  val name = ident();
  punct("=");
  val binding = parseExpr();
  kw("in");
  val body = parseExpr();
  Let(name, binding, body)
}

def parseGroup() = or { parseAtom() } {
  punct("(");
  val res = parseExpr();
  punct(")");
  res
}

def parseApp(): Tree / Parser = {
  val funName = ident();
  punct("(");
  val arg = parseExpr();
  punct(")");
  App(funName, arg)
}

def parseExpr(): Tree / Parser =
  or { parseLet() } { or { parseApp() } { parseGroup() } }

```

Again, note how naturally the result can be composed from the individual results, much like
manually writing a recursive descent parser. Compared to handcrafted parsers, the imperative
parser combinators presented here offer a similar flexibility. At the same time, the semantics
of `alt` and `fail` is still left open, offering flexibility in the implementation of the actual underlying parsing algorithm.

## Example: Combining Parsers and Local Mutable State
It is possible to combine the imperative parser combinators with
local mutable state. The implementation of local variables in Effekt is
designed to interact well with continuation capture and multiple resumptions.
This is important for use cases like the parser example where the continuation is
potentially called multiple times.

The following example implements an example
```raw
<EXPR> ::= <NUMBER> | <IDENT> `(` <EXPR> (`,` <EXPR>)*  `)`
```
It uses local (mutable) variables to count the number of leafs as semantic action.
```
def parseCalls(): Int / Parser =
  or { number(); 1 } {
    var count = 1;
    ident();
    punct("(");
    count = count + parseCalls();
    many {
        punct(",");
        count = count + parseCalls()
    };
    punct(")");
    count
  }
```
Notice how the user defined combinator `many` feels like a built-in control operator
`while`.

## Backtracking Parsers
As mentioned above, so far we used the non-determinism effects, but did not specify
what they mean. We could implement depth-first parsing corresponding to recursive descent.
Alternatively, we also could implement breadth-first parsing. Further, in case of
ambiguities, we have the choice whether we want to recognize only the first result
or compute all possible alternative ways to recognize the input.

For this case study, we implement a depth-first parser, computing the first
successful result. Results are represented by the `ParseResult` datatype.
The parsing algorithm is simply implemented as a handler for `Parser`.

```
type ParseResult[R] {
  ParseSuccess(t: R);
  ParseFailure(msg: String)
}

def parse[R](input: String) { p: => R / Parser }: ParseResult[R] = try {
  lexer(input) { skipWhitespace { ParseSuccess(p()) } }
} with Nondet {
  def alt() = resume(true) match {
    case ParseFailure(msg) => resume(false)
    case ParseSuccess(res) => ParseSuccess(res)
  }
  def fail(msg) = ParseFailure(msg)
} with LexerError { (msg, pos) =>
  ParseFailure(msg)
}
```
The handler reuses the lexer implementation of the Lexer case study. The lexer
raises a `LexerError` in case of an unexpected enf of the input stream or if it cannot
recognize a token. Those lexer errors are simply converted into failures, which the
parser can backtrack. To establish the backtracking behavior, it is important that the
lexer is executed _under_ the parser handler and not the other way around. Only this way
the lexer positions will be restored when calling the continuation a second time with `resume(false)`.


## Running the Examples
Having implemented a handler for the `Parser` effect, we can run our example "grammars" on some inputs.

```effekt:repl
println(parse("42") { parseCalls() }.show)
println(parse("foo(1)") { parseCalls() }.show)
println(parse("foo(1, 2)") { parseCalls() }.show)
println(parse("foo(1, 2, 3, 4)") { parseCalls() }.show)
println(parse("foo(1, 2, bar(4, 5))") { parseCalls() }.show)
println(parse("foo(1, 2,\nbar(4, 5))") { parseCalls() }.show)

println(parse("}42") { parseExpr() }.show)
println(parse("42") { parseExpr() }.show)
println(parse("let x = 4 in 42") { parseExpr() }.show)
println(parse("let x = let y = 2 in 1 in 42") { parseExpr() }.show)
println(parse("let x = (let y = 2 in 1) in 42") { parseExpr() }.show)
println(parse("let x = (let y = f(42) in 1) in 42") { parseExpr() }.show)
println(parse("let x = (let y = f(let z = 1 in z) in 1) in 42") { parseExpr() }.show)
```
