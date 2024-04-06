# From https://github.com/lukewilliamboswell/roc-parser/blob/main/package/String.roc
# License is UPL
interface String
    exposes [
        Utf8,
        parseStr,
        parseStrPartial,
        parseUtf8,
        parseUtf8Partial,
        string,
        utf8,
        codeunit,
        codeunitSatisfies,
        anyString,
        anyThing,
        anyCodeunit,
        scalar,
        oneOf,
        digit,
        digits,
        strFromUtf8,
    ]
    imports [
        Core.{ Parser, ParseResult, const, map, skip, sepBy, keep, oneOrMore, parse, parsePartial, buildPrimitiveParser, chompUntil, chompWhile },
    ]

## ```
## Utf8 : List U8
## ```
Utf8 : List U8

## Parse a [Str] using a [Parser]
## ```
## color : Parser Utf8 [Red, Green, Blue]
## color =
##     oneOf [
##         const Red |> skip (string "red"),
##         const Green |> skip (string "green"),
##         const Blue |> skip (string "blue"),
##     ]
##
## expect parseStr color "green" == Ok Green
## ```
parseStr : Parser Utf8 a, Str -> Result a [ParsingFailure Str, ParsingIncomplete Str]
parseStr = \parser, input ->
    parser
    |> parseUtf8 (strToRaw input)
    |> Result.mapErr \problem ->
        when problem is
            ParsingFailure msg -> ParsingFailure msg
            ParsingIncomplete leftoverRaw -> ParsingIncomplete (strFromUtf8 leftoverRaw)

## Runs a parser against the start of a string, allowing the parser to consume it only partially.
##
## - If the parser succeeds, returns the resulting value as well as the leftover input.
## - If the parser fails, returns `Err (ParsingFailure msg)`
##
##
## ```
## atSign : Parser Utf8 [AtSign]
## atSign = const AtSign |> skip (codeunit '@')
##
## expect parseStr atSign "@" == Ok AtSign
## expect parseStrPartial atSign "@" |> Result.map .val == Ok AtSign
## expect parseStrPartial atSign "$" |> Result.isErr
## ```
parseStrPartial : Parser Utf8 a, Str -> ParseResult Str a
parseStrPartial = \parser, input ->
    parser
    |> parseUtf8Partial (strToRaw input)
    |> Result.map \{ val: val, input: restRaw } ->
        { val: val, input: strFromUtf8 restRaw }

## Runs a parser against a string, requiring the parser to consume it fully.
##
## - If the parser succeeds, returns `Ok a`
## - If the parser fails, returns `Err (ParsingFailure Str)`
## - If the parser succeeds but does not consume the full string, returns `Err (ParsingIncomplete (List U8))`
parseUtf8 : Parser Utf8 a, Utf8 -> Result a [ParsingFailure Str, ParsingIncomplete Utf8]
parseUtf8 = \parser, input ->
    parse parser input (\leftover -> List.len leftover == 0)

## Runs a parser against the start of a list of scalars, allowing the parser to consume it only partially.
parseUtf8Partial : Parser Utf8 a, Utf8 -> ParseResult Utf8 a
parseUtf8Partial = \parser, input ->
    parsePartial parser input

## ```
## isDigit : U8 -> Bool
## isDigit = \b -> b >= '0' && b <= '9'
##
## expect parseStr (codeunitSatisfies isDigit) "0" == Ok '0'
## expect parseStr (codeunitSatisfies isDigit) "*" |> Result.isErr
## ```
codeunitSatisfies : (U8 -> Bool) -> Parser Utf8 U8
codeunitSatisfies = \check ->
    buildPrimitiveParser \input ->
        { before: start, others: inputRest } = List.split input 1

        when List.get start 0 is
            Err OutOfBounds ->
                Err (ParsingFailure "expected a codeunit satisfying a condition, but input was empty.")

            Ok startCodeunit ->
                if check startCodeunit then
                    Ok { val: startCodeunit, input: inputRest }
                else
                    otherChar = strFromCodeunit startCodeunit
                    inputStr = strFromUtf8 input

                    Err (ParsingFailure "expected a codeunit satisfying a condition but found `\(otherChar)`.\n While reading: `\(inputStr)`")

## ```
## atSign : Parser Utf8 [AtSign]
## atSign = const AtSign |> skip (codeunit '@')
##
## expect parseStr atSign "@" == Ok AtSign
## expect parseStrPartial atSign "$" |> Result.isErr
## ```
codeunit : U8 -> Parser Utf8 U8
codeunit = \expectedCodeUnit ->
    buildPrimitiveParser \input ->
        when input is
            [] ->
                Err (ParsingFailure "expected char `\(strFromCodeunit expectedCodeUnit)` but input was empty.")
            [first, .. as rest] if first == expectedCodeUnit ->
                Ok { val: expectedCodeUnit, input: rest }
            [first, .. as rest] ->
                Err (ParsingFailure "expected char `\(strFromCodeunit expectedCodeUnit)` but found `\(strFromCodeunit first)`.\n While reading: `\(strFromUtf8 input)`")

## Parse an extact sequence of utf8
utf8 : List U8 -> Parser Utf8 (List U8)
utf8 = \expectedString ->
    # Implemented manually instead of a sequence of codeunits
    # because of efficiency and better error messages
    buildPrimitiveParser \input ->
        { before: start, others: inputRest } = List.split input (List.len expectedString)

        if start == expectedString then
            Ok { val: expectedString, input: inputRest }
        else
            errorString = strFromUtf8 expectedString
            otherString = strFromUtf8 start
            inputString = strFromUtf8 input

            Err (ParsingFailure "expected string `\(errorString)` but found `\(otherString)`.\nWhile reading: \(inputString)")

## Parse the given [Str]
## ```
## expect parseStr (string "Foo") "Foo" == Ok "Foo"
## expect parseStr (string "Foo") "Bar" |> Result.isErr
## ```
string : Str -> Parser Utf8 Str
string = \expectedString ->
    strToRaw expectedString
    |> utf8
    |> map \_val -> expectedString

## Matches any [U8] codeunit
## ```
## expect parseStr anyCodeunit "a" == Ok 'a'
## expect parseStr anyCodeunit "$" == Ok '$'
## ```
anyCodeunit : Parser Utf8 U8
anyCodeunit = codeunitSatisfies (\_ -> Bool.true)

expect parseStr anyCodeunit "a" == Ok 'a'
expect parseStr anyCodeunit "$" == Ok '$'

## Matches any [Utf8] and consumes all the input without fail.
## ```
## expect
##     bytes = Str.toUtf8 "consumes all the input"
##     parse anyThing bytes List.isEmpty == Ok bytes
## ```
anyThing : Parser Utf8 Utf8
anyThing = buildPrimitiveParser \input -> Ok { val: input, input: [] }

expect
    bytes = Str.toUtf8 "consumes all the input"
    parse anyThing bytes List.isEmpty == Ok bytes

# Matches any string
# as long as it is valid UTF8.
anyString : Parser Utf8 Str
anyString = buildPrimitiveParser \fieldUtf8ing ->
    when Str.fromUtf8 fieldUtf8ing is
        Ok stringVal ->
            Ok { val: stringVal, input: [] }

        Err (BadUtf8 _ _) ->
            Err (ParsingFailure "Expected a string field, but its contents cannot be parsed as UTF8.")

## ```
## expect parseStr digit "0" == Ok 0
## expect parseStr digit "not a digit" |> Result.isErr
## ```
digit : Parser Utf8 Nat
digit =
    buildPrimitiveParser \input ->
        when input is
            [] ->
                Err (ParsingFailure "Expected a digit from 0-9 but input was empty.")
            [first, .. as rest] if first >= '0' && first <= '9' ->
                Ok { val: Num.toNat (first - '0'), input: rest }
            _ ->
                Err (ParsingFailure "Not a digit")

## Parse a sequence of digits into a [Nat] accepting leading zeroes
## ```
## expect parseStr digits "0123" == Ok 123
## expect parseStr digits "not a digit" |> Result.isErr
## ```
digits : Parser Utf8 Nat
digits =
    oneOrMore digit
    |> map \ds -> List.walk ds 0 (\sum, d -> sum * 10 + d)

## Try a bunch of different parsers.
##
## The first parser which is tried is the one at the front of the list,
## and the next one is tried until one succeeds or the end of the list was reached.
## ```
## boolParser : Parser Utf8 Bool
## boolParser =
##     oneOf [string "true", string "false"]
##     |> map (\x -> if x == "true" then Bool.true else Bool.false)
##
## expect parseStr boolParser "true" == Ok Bool.true
## expect parseStr boolParser "false" == Ok Bool.false
## expect parseStr boolParser "not a bool" |> Result.isErr
## ```
oneOf : List (Parser Utf8 a) -> Parser Utf8 a
oneOf = \parsers ->
    buildPrimitiveParser \input ->
        List.walkUntil parsers (Err (ParsingFailure "(no possibilities)")) \_, parser ->
            when parseUtf8Partial parser input is
                Ok val ->
                    Break (Ok val)

                Err problem ->
                    Continue (Err problem)

scalar : U32 -> Parser Utf8 U32
scalar = \expectedScalar ->
    expectedScalar
    |> strFromScalar
    |> string
    |> map \_ -> expectedScalar

strFromUtf8 : Utf8 -> Str
strFromUtf8 = \rawStr ->
    rawStr
    |> Str.fromUtf8
    |> Result.withDefault "Unexpected problem while turning a List U8 (that was originally a Str) back into a Str. This should never happen!"

strToRaw : Str -> Utf8
strToRaw = \str ->
    str |> Str.toUtf8

strFromScalar : U32 -> Str
strFromScalar = \scalarVal ->
    Str.appendScalar "" (Num.intCast scalarVal)
    |> Result.withDefault "Unexpected problem while turning a U32 (that was probably originally a scalar constant) into a Str. This should never happen!"

strFromCodeunit : U8 -> Str
strFromCodeunit = \cu ->
    strFromUtf8 [cu]

# -------------------- example snippets used in docs --------------------

parseU32 : Parser Utf8 U32
parseU32 =
    const Num.toU32
    |> keep digits

expect parseStr parseU32 "123" == Ok 123u32

color : Parser Utf8 [Red, Green, Blue]
color =
    oneOf [
        const Red |> skip (string "red"),
        const Green |> skip (string "green"),
        const Blue |> skip (string "blue"),
    ]

expect parseStr color "green" == Ok Green

parseNumbers : Parser Utf8 (List Nat)
parseNumbers =
    digits |> sepBy (codeunit ',')

expect parseStr parseNumbers "1,2,3" == Ok [1,2,3]

expect parseStr (string "Foo") "Foo" == Ok "Foo"
expect parseStr (string "Foo") "Bar" |> Result.isErr

ignoreText : Parser Utf8 Nat
ignoreText =
    const (\d -> d)
    |> skip (chompUntil ':')
    |> skip (codeunit ':')
    |> keep digits

expect parseStr ignoreText "ignore preceding text:123" == Ok 123

ignoreNumbers : Parser Utf8 Str
ignoreNumbers =
    const (\str -> str)
    |> skip (chompWhile \b -> b >= '0' && b <= '9')
    |> keep (string "TEXT")

expect parseStr ignoreNumbers "0123456789876543210TEXT" == Ok "TEXT"

isDigit : U8 -> Bool
isDigit = \b -> b >= '0' && b <= '9'

expect parseStr (codeunitSatisfies isDigit) "0" == Ok '0'
expect parseStr (codeunitSatisfies isDigit) "*" |> Result.isErr

atSign : Parser Utf8 [AtSign]
atSign = const AtSign |> skip (codeunit '@')

expect parseStr atSign "@" == Ok AtSign
expect parseStrPartial atSign "@" |> Result.map .val == Ok AtSign
expect parseStrPartial atSign "$" |> Result.isErr

Requirement : [ Green Nat, Red Nat, Blue Nat ]
RequirementSet : List Requirement
Game : { id: Nat, requirements: List RequirementSet }

parseGame : Str -> Result Game [ParsingError]
parseGame = \s ->
    green = const Green |> keep digits |> skip (string " green")
    red = const Red |> keep digits |> skip (string " red")
    blue = const Blue |> keep digits |> skip (string " blue")

    requirementSet : Parser _ RequirementSet
    requirementSet = (oneOf [green, red, blue]) |> sepBy (string ", ")

    requirements : Parser _ (List RequirementSet)
    requirements = requirementSet |> sepBy (string "; ")

    game : Parser _ Game
    game =
        const (\id -> \r -> { id, requirements: r })
        |> skip (string "Game ")
        |> keep digits
        |> skip (string ": ")
        |> keep requirements

    when parseStr game s is
        Ok g -> Ok g
        Err (ParsingFailure _) | Err (ParsingIncomplete _) -> Err ParsingError

expect parseGame "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green" == Ok {
        id: 1,
        requirements: [
            [Blue 3, Red 4],
            [Red 1, Green 2, Blue 6],
            [Green 2],
        ]
    }

expect parseStr digit "0" == Ok 0
expect parseStr digit "not a digit" |> Result.isErr

expect parseStr digits "0123" == Ok 123
expect parseStr digits "not a digit" |> Result.isErr

boolParser : Parser Utf8 Bool
boolParser =
    oneOf [string "true", string "false"]
    |> map (\x -> if x == "true" then Bool.true else Bool.false)

expect parseStr boolParser "true" == Ok Bool.true
expect parseStr boolParser "false" == Ok Bool.false
expect parseStr boolParser "not a bool" |> Result.isErr
