## EDL tokens.
##
## A `Token` is a flat object (no variant) so the bootstrap-to-EDL migrator
## never has to translate bootstrap case-object semantics. Unused value fields
## are simply
## zero-initialised.
##
## Reserved-for-later tokens (`?`, `->`) are lexed but not yet parsed; the
## parser reports them through `edlParseNotImplemented` rather than emitting a
## confusing syntax error.

import ./source

type
  TokenKind* = enum
    tkEof
    tkBadToken

    # literals
    tkIntLit
    tkFloatLit
    tkStringLit
    tkCharLit

    # identifiers and keywords
    tkIdent
    tkKwFn
    tkKwLet
    tkKwVar
    tkKwConst
    tkKwStruct
    tkKwEnum
    tkKwImport
    tkKwFrom
    tkKwIf
    tkKwElse
    tkKwWhile
    tkKwFor
    tkKwIn
    tkKwReturn
    tkKwTrue
    tkKwFalse
    tkKwNil
    tkKwBreak
    tkKwContinue
    tkKwAnd
    tkKwOr
    tkKwNot
    tkKwUnsafe

    # punctuation
    tkLParen
    tkRParen
    tkLBrace
    tkRBrace
    tkLBracket
    tkRBracket
    tkComma
    tkColon
    tkDot
    tkSemicolon
    tkArrow        # ->
    tkQuestion      # ?  reserved: error propagation (specs/errors.md)

    # operators
    tkAssign       # =
    tkEq           # ==
    tkNe           # !=
    tkLt           # <
    tkLe           # <=
    tkGt           # >
    tkGe           # >=
    tkPlus
    tkMinus
    tkStar
    tkSlash
    tkPercent

  Token* = object
    kind*: TokenKind
    text*: string        ## raw lexeme as written in the source
    intVal*: int64
    floatVal*: float64
    strVal*: string      ## decoded value for string/char literals
    span*: SourceSpan

proc tokenKindName*(k: TokenKind): string =
  ## Human-readable name used in parser messages.
  case k
  of tkEof: "end of file"
  of tkBadToken: "invalid token"
  of tkIntLit: "integer literal"
  of tkFloatLit: "float literal"
  of tkStringLit: "string literal"
  of tkCharLit: "char literal"
  of tkIdent: "identifier"
  of tkKwFn: "'fn'"
  of tkKwLet: "'let'"
  of tkKwVar: "'var'"
  of tkKwConst: "'const'"
  of tkKwStruct: "'struct'"
  of tkKwEnum: "'enum'"
  of tkKwImport: "'import'"
  of tkKwFrom: "'from'"
  of tkKwIf: "'if'"
  of tkKwElse: "'else'"
  of tkKwWhile: "'while'"
  of tkKwFor: "'for'"
  of tkKwIn: "'in'"
  of tkKwReturn: "'return'"
  of tkKwTrue: "'true'"
  of tkKwFalse: "'false'"
  of tkKwNil: "'nil'"
  of tkKwBreak: "'break'"
  of tkKwContinue: "'continue'"
  of tkKwAnd: "'and'"
  of tkKwOr: "'or'"
  of tkKwNot: "'not'"
  of tkKwUnsafe: "'unsafe'"
  of tkLParen: "'('"
  of tkRParen: "')'"
  of tkLBrace: "'{'"
  of tkRBrace: "'}'"
  of tkLBracket: "'['"
  of tkRBracket: "']'"
  of tkComma: "','"
  of tkColon: "':'"
  of tkDot: "'.'"
  of tkSemicolon: "';'"
  of tkArrow: "'->'"
  of tkQuestion: "'?'"
  of tkAssign: "'='"
  of tkEq: "'=='"
  of tkNe: "'!='"
  of tkLt: "'<'"
  of tkLe: "'<='"
  of tkGt: "'>'"
  of tkGe: "'>='"
  of tkPlus: "'+'"
  of tkMinus: "'-'"
  of tkStar: "'*'"
  of tkSlash: "'/'"
  of tkPercent: "'%'"

proc describe*(t: Token): string =
  ## Renders a token for messages: shows the lexeme when it has one.
  case t.kind
  of tkIdent, tkIntLit, tkFloatLit, tkStringLit, tkCharLit, tkBadToken:
    result = tokenKindName(t.kind) & " '" & t.text & "'"
  else:
    result = tokenKindName(t.kind)

proc keywordKind*(text: string): TokenKind =
  ## Maps an identifier lexeme to its keyword token, or `tkIdent`.
  case text
  of "fn": tkKwFn
  of "let": tkKwLet
  of "var": tkKwVar
  of "const": tkKwConst
  of "struct": tkKwStruct
  of "enum": tkKwEnum
  of "import": tkKwImport
  of "from": tkKwFrom
  of "if": tkKwIf
  of "else": tkKwElse
  of "while": tkKwWhile
  of "for": tkKwFor
  of "in": tkKwIn
  of "return": tkKwReturn
  of "true": tkKwTrue
  of "false": tkKwFalse
  of "nil": tkKwNil
  of "break": tkKwBreak
  of "continue": tkKwContinue
  of "and": tkKwAnd
  of "or": tkKwOr
  of "not": tkKwNot
  of "unsafe": tkKwUnsafe
  else: tkIdent

proc isKeyword*(k: TokenKind): bool =
  result = k.ord >= tkKwFn.ord and k.ord <= tkKwUnsafe.ord

proc isLiteral*(k: TokenKind): bool =
  result = k.ord >= tkIntLit.ord and k.ord <= tkCharLit.ord
