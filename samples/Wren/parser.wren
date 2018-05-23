// https://github.com/munificent/wrenalyzer/blob/master/parser.wren
// MIT License

import "ast" for
    AssignmentExpr,
    BlockStmt,
    Body,
    BoolExpr,
    BreakStmt,
    CallExpr,
    ClassStmt,
    ConditionalExpr,
    FieldExpr,
    ForStmt,
    GroupingExpr,
    IfStmt,
    ImportStmt,
    InfixExpr,
    InterpolationExpr,
    ListExpr,
    MapEntry,
    MapExpr,
    Method,
    Module,
    NullExpr,
    NumExpr,
    PrefixExpr,
    ReturnStmt,
    StaticFieldExpr,
    StringExpr,
    SubscriptExpr,
    SuperExpr,
    ThisExpr,
    VarStmt,
    WhileStmt
import "lexer" for Lexer
import "token" for Token

var EQUALITY_OPERATORS = [
  Token.equalEqual,
  Token.bangEqual
]

var COMPARISON_OPERATORS = [
  Token.less,
  Token.lessEqual,
  Token.greater,
  Token.greaterEqual
]

var BITWISE_SHIFT_OPERATORS = [
  Token.lessLess,
  Token.greaterGreater
]

var RANGE_OPERATORS = [
  Token.dotDot,
  Token.dotDotDot
]

var TERM_OPERATORS = [
  Token.plus,
  Token.minus
]

var FACTOR_OPERATORS = [
  Token.star,
  Token.slash,
  Token.percent
]

var PREFIX_OPERATORS = [
  Token.minus,
  Token.bang,
  Token.tilde
]

var INFIX_OPERATORS = [
  Token.pipePipe,
  Token.ampAmp,
  Token.equalEqual,
  Token.bangEqual,
  Token.isKeyword,
  Token.less,
  Token.lessEqual,
  Token.greater,
  Token.greaterEqual,
  Token.pipe,
  Token.caret,
  Token.amp,
  Token.lessLess,
  Token.greaterGreater,
  Token.dotDot,
  Token.dotDotDot,
  Token.plus,
  Token.minus,
  Token.star,
  Token.slash,
  Token.percent
]

class Parser {
  construct new(lexer, reporter) {
    _lexer = lexer
    _reporter = reporter
    _current = _lexer.readToken()
  }

  parseModule() {
    ignoreLine()

    var statements = []
    while (peek() != Token.eof) {
      statements.add(definition())
      if (!matchLine()) break
    }

    consume(Token.eof, "Expect end of input.")
    return Module.new(statements)
  }

  definition() {
    if (match(Token.classKeyword)) {
      return finishClass(null)
    }

    if (match(Token.foreignKeyword)) {
      var foreignKeyword = _previous
      consume(Token.classKeyword, "Expect 'class' after 'foreign'.")
      return finishClass(foreignKeyword)
    }

    if (match(Token.importKeyword)) {
      var path = consume(Token.string, "Expect import path.")
      var variables

      // Parse the variable list, if there is one.
      if (match(Token.forKeyword)) {
        ignoreLine()

        variables = []
        while (true) {
          variables.add(consume(Token.name, "Expect imported variable name."))
          if (!match(Token.comma)) break
          ignoreLine()
        }
      }

      return ImportStmt.new(path, variables)
    }

    if (match(Token.varKeyword)) {
      var name = consume(Token.name, "Expect variable name.")
      var initializer
      if (match(Token.equal)) {
        initializer = expression()
      }

      return VarStmt.new(name, initializer)
    }

    return statement()
  }

  // Parses the rest of a class definition after the "class" token.
  finishClass(foreignKeyword) {
    var name = consume(Token.name, "Expect class name.")

    var superclass
    if (match(Token.isKeyword)) {
      // TODO: This is different from the VM (which is wrong). Need to make
      // sure we don't parse the class body as a block argument.
      superclass = consume(Token.name, "Expect name of superclass.")
    }

    var methods = []
    consume(Token.leftBrace, "Expect '{' after class name.")
    ignoreLine()

    while (!match(Token.rightBrace) && peek() != Token.eof) {
      methods.add(method())

      // Don't require a newline after the last definition.
      if (match(Token.rightBrace)) break

      consumeLine("Expect newline after definition in class.")
    }

    return ClassStmt.new(foreignKeyword, name, superclass, methods)
  }

  method() {
    // Note: This parses more permissively than the grammar actually is. For
    // example, it will allow "static construct *()". We'll report errors on
    // invalid forms later.
    var foreignKeyword
    if (match(Token.foreignKeyword)) {
      foreignKeyword = _previous
    }

    var staticKeyword
    if (match(Token.staticKeyword)) {
      staticKeyword = _previous
    }

    var constructKeyword
    if (match(Token.constructKeyword)) {
      constructKeyword = _previous
    }

    // TODO: Error on invalid combinations of above keywords.

    var name
    var parameters

    var allowParameters = false

    if (match(Token.leftBracket)) {
      // Subscript operator.
      parameters = parameterList()
      consume(Token.rightBracket, "Expect ']' after parameters.")
      allowParameters = false
    } else if (matchAny(INFIX_OPERATORS)) {
      _previous
      allowParameters = true
    } else if (matchAny([Token.bang, Token.tilde])) {
      allowParameters = false
    } else {
      consume(Token.name, "Expect method name.")
      allowParameters = true
    }
    name = _previous

    if (match(Token.leftParen)) {
      // Parse the parameter list even if not allowed to give better errors
      // and have fewer cascaded errors.
      if (!allowParameters) {
        error("A parameter list is not allowed for this method.")
      }

      ignoreLine()
      if (!match(Token.rightParen)) {
        parameters = parameterList()
        ignoreLine()
        consume(Token.rightParen, "Expect ')' after parameters.")
      }
    }
    // TODO: Setters.

    var body
    if (foreignKeyword == null) {
      consume(Token.leftBrace, "Expect '{' before method body.")
      body = finishBody(parameters)
    }

    return Method.new(foreignKeyword, staticKeyword, constructKeyword, name, body)
  }

  statement() {
    // Break statement.
    if (match(Token.breakKeyword)) {
      return BreakStmt.new(_previous)
    }

    // If statement.
    if (match(Token.ifKeyword)) {
      consume(Token.leftParen, "Expect '(' after 'if'.")
      ignoreLine()
      var condition = expression()
      consume(Token.rightParen, "Expect ')' after if condition.")
      var thenBranch = statement()
      var elseBranch
      if (match(Token.elseKeyword)) {
        elseBranch = statement()
      }
      return IfStmt.new(condition, thenBranch, elseBranch)
    }

    // For statement.
    if (match(Token.forKeyword)) {
      consume(Token.leftParen, "Expect '(' after 'for'.")
      var variable = consume(Token.name, "Expect for loop variable name.")
      consume(Token.inKeyword, "Expect 'in' after loop variable.")
      ignoreLine()
      var iterator = expression()
      consume(Token.rightParen, "Expect ')' after loop expression.")
      var body = statement()
      return ForStmt.new(variable, iterator, body)
    }

    // While statement.
    if (match(Token.whileKeyword)) {
      consume(Token.leftParen, "Expect '(' after 'while'.")
      ignoreLine()
      var condition = expression()
      consume(Token.rightParen, "Expect ')' after while condition.")
      var body = statement()
      return WhileStmt.new(condition, body)
    }

    // Return statement.
    if (match(Token.returnKeyword)) {
      var keyword = _previous
      var value
      if (peek() != Token.line) {
        value = expression()
      }

      return ReturnStmt.new(keyword, value)
    }

    // Block statement.
    if (match(Token.leftBrace)) {
      var statements = []
      ignoreLine()

      if (!match(Token.rightBrace)) {
        while (peek() != Token.eof) {
          statements.add(definition())
          consumeLine("Expect newline after statement.")

          if (match(Token.rightBrace)) break
        }
      }

      return BlockStmt.new(statements)
    }

    // Expression statement.
    return expression()
  }

  // Parses the rest of a method or block argument body.
  finishBody(parameters) {
    // An empty block.
    if (match(Token.rightBrace)) return Body.new(parameters, null, [])

    // If there's no line after the "{", it's a single-expression body.
    if (!matchLine()) {
      var expr = expression()
      ignoreLine()
      consume(Token.rightBrace, "Expect '}' at end of block.")
      return Body.new(parameters, expr, null)
    }

    // Empty blocks (with just a newline inside) do nothing.
    if (match(Token.rightBrace)) return Body.new(parameters, null, [])

    var statements = []
    while (peek() != Token.eof) {
      statements.add(definition())
      consumeLine("Expect newline after statement.")

      if (match(Token.rightBrace)) break
    }

    return Body.new(parameters, null, statements)
  }

  expression() { assignment() }

  // assignment: conditional ( "=" assignment )?
  assignment() {
    // TODO: This allows invalid LHS like "1 + 2 = 3". Decide if we want to
    // handle that here or later in the pipeline.
    var expr = conditional()
    if (!match(Token.equal)) return expr

    var equal = _previous
    var value = assignment()
    return AssignmentExpr.new(expr, equal, value)
  }

  // conditional: logicalOr ( "?" conditional ":" assignment )?
  conditional() {
    var expr = logicalOr()
    if (!match(Token.question)) return expr

    var question = _previous
    var thenBranch = conditional()
    var colon = consume(Token.colon,
        "Expect ':' after then branch of conditional operator.")
    var elseBranch = assignment()
    return ConditionalExpr.new(expr, question, thenBranch, colon, elseBranch)
  }

  // logicalOr: logicalAnd ( "||" logicalAnd )*
  logicalOr() { parseInfix([Token.pipePipe]) { logicalAnd() } }

  // logicalAnd: equality ( "&&" equality )*
  logicalAnd() { parseInfix([Token.ampAmp]) { equality() } }

  // equality: typeTest ( equalityOperator typeTest )*
  // equalityOperator: "==" | "!="
  equality() { parseInfix(EQUALITY_OPERATORS) { typeTest() } }

  // typeTest: comparison ( "is" comparison )*
  typeTest() { parseInfix([Token.isKeyword]) { comparison() } }

  // comparison: bitwiseOr ( comparisonOperator bitwiseOr )*
  // comparisonOperator: "<" | ">" | "<=" | ">="
  comparison() { parseInfix(COMPARISON_OPERATORS) { bitwiseOr() } }

  // bitwiseOr: bitwiseXor ( "|" bitwiseXor )*
  bitwiseOr() { parseInfix([Token.pipe]) { bitwiseXor() } }

  // bitwiseXor: bitwiseAnd ( "^" bitwiseAnd )*
  bitwiseXor() { parseInfix([Token.caret]) { bitwiseAnd() } }

  // bitwiseAnd: bitwiseShift ( "&" bitwiseShift )*
  bitwiseAnd() { parseInfix([Token.amp]) { bitwiseShift() } }

  // bitwiseShift: range ( bitwiseShiftOperator range )*
  // bitwiseShiftOperator: "<<" | ">>"
  bitwiseShift() { parseInfix(BITWISE_SHIFT_OPERATORS) { range() } }

  // range: term ( rangeOperator term )*
  // rangeOperator: ".." | ".."
  range() { parseInfix(RANGE_OPERATORS) { term() } }

  // term: factor ( termOperator factor )*
  // termOperator: "+" | "-"
  term() { parseInfix(TERM_OPERATORS) { factor() } }

  // factor: prefix ( factorOperator prefix )*
  // factorOperator: "*" | "/" | "%"
  factor() { parseInfix(FACTOR_OPERATORS) { prefix() } }

  // prefix: ("-" | "!" | "~")* call
  prefix() {
    if (matchAny(PREFIX_OPERATORS)) {
      return PrefixExpr.new(_previous, prefix())
    }

    return call()
  }

  // call: primary ( subscript | "." methodCall )*
  // subscript: "[" argumentList "]"
  call() {
    var expr = primary()

    while (true) {
      if (match(Token.leftBracket)) {
        var leftBracket = _previous
        var arguments = argumentList()
        var rightBracket = consume(Token.rightBracket,
            "Expect ']' after subscript arguments.")
        expr = SubscriptExpr.new(expr, leftBracket, arguments, rightBracket)
      } else if (match(Token.dot)) {
        var name = consume(Token.name, "Expect method name after '.'.")
        expr = methodCall(expr, name)
      } else {
        break
      }
    }

    return expr
  }

  // Parses the argument list for a method call and creates a call expression
  // for it.
  //
  // methodCall: ( "(" argumentList? ")" )? blockArgument?
  // blockArgument: "{" ( "|" parameterList "|" )? body "}"
  // parameterList: Name ( "," Name )*
  // body:
  //   | "\n" ( definition "\n" )*
  //   | expression
  methodCall(receiver, name) {
    var arguments = finishCall()
    return CallExpr.new(receiver, name, arguments[0], arguments[1])
  }

  // Parses the argument list for a method call. Returns a list containing the
  // argument list (if any) and block argument (if any). If either is missing,
  // the list element at that position is `null`.
  finishCall() {
    var arguments
    if (match(Token.leftParen)) {
      // Allow an empty argument list. Note that we treat this differently than
      // a getter (no argument list). The former will have a `null` argument
      // list and the latter will have an empty one.
      if (match(Token.rightParen)) {
        arguments = []
      } else {
        arguments = argumentList()
        consume(Token.rightParen, "Expect ')' after arguments.")
      }
    }

    var blockArgument
    if (match(Token.leftBrace)) {
      var parameters
      if (match(Token.pipe)) {
        parameters = parameterList()
        consume(Token.pipe, "Expect '|' after block parameters.")
      }

      blockArgument = finishBody(parameters)
    }

    return [arguments, blockArgument]
  }

  // argumentList: expression ( "," expression )*
  argumentList() {
    var arguments = []

    ignoreLine()
    while (true) {
      arguments.add(expression())
      if (!match(Token.comma)) break
      ignoreLine()
    }

    return arguments
  }

  // parameterList: name ( "," name )*
  parameterList() {
    var parameters = []

    while (true) {
      parameters.add(consume(Token.name, "Expect parameter name."))
      if (!match(Token.comma)) break
      ignoreLine()
    }

    return parameters
  }

  // primary:
  //   | grouping
  //   | listLiteral
  //   | mapLiteral
  //   | "true" | "false" | "null" | "this"
  //   | Field | StaticField | Number
  primary() {
    if (match(Token.leftParen))         return grouping()
    if (match(Token.leftBracket))       return listLiteral()
    if (match(Token.leftBrace))         return mapLiteral()
    if (match(Token.name))              return methodCall(null, _previous)
    if (match(Token.superKeyword))      return superCall()

    if (match(Token.falseKeyword))      return BoolExpr.new(_previous)
    if (match(Token.trueKeyword))       return BoolExpr.new(_previous)
    if (match(Token.nullKeyword))       return NullExpr.new(_previous)
    if (match(Token.thisKeyword))       return ThisExpr.new(_previous)

    // TODO: Error if not inside class.
    if (match(Token.field))             return FieldExpr.new(_previous)
    if (match(Token.staticField))       return StaticFieldExpr.new(_previous)

    if (match(Token.number))            return NumExpr.new(_previous)
    if (match(Token.string))            return StringExpr.new(_previous)

    if (peek() == Token.interpolation)  return stringInterpolation()
    // TODO: Token.super.

    error("Expected expression.")
    // Make a fake node so that we don't have to worry about null later.
    // TODO: Should this be an error node?
    return NullExpr.new(_previous)
  }

  // Finishes parsing a parenthesized expression.
  //
  // grouping: "(" expressions ")"
  grouping() {
    var leftParen = _previous
    var expr = expression()
    var rightParen = consume(Token.rightParen, "Expect ')' after expression.")
    return GroupingExpr.new(leftParen, expr, rightParen)
  }

  // Finishes parsing a list literal.
  //
  // listLiteral: "[" ( expression ("," expression)* ","? )? "]"
  listLiteral() {
    var leftBracket = _previous
    var elements = []

    ignoreLine()

    while (peek() != Token.rightBracket) {
      elements.add(expression())

      ignoreLine()
      if (!match(Token.comma)) break
      ignoreLine()
    }

    var rightBracket = consume(Token.rightBracket,
        "Expect ']' after list elements.")
    return ListExpr.new(leftBracket, elements, rightBracket)
  }

  // Finishes parsing a map literal.
  //
  // mapLiteral: "[" ( mapEntry ("," mapEntry)* ","? )? "}"
  // mapEntry:   expression ":" expression
  mapLiteral() {
    var leftBrace = _previous
    var entries = []

    ignoreLine()

    while (peek() != Token.rightBrace) {
      var key = expression()
      consume(Token.colon, "Expect ':' after map key.")

      var value = expression()
      entries.add(MapEntry.new(key, value))

      ignoreLine()
      if (!match(Token.comma)) break
      ignoreLine()
    }

    var rightBrace = consume(Token.rightBrace, "Expect '}' after map entries.")
    return MapExpr.new(leftBrace, entries, rightBrace)
  }

  superCall() {
    var name
    if (match(Token.dot)) {
      // It's a named super call.
      name = consume(Token.name, "Expect method name after 'super.'.")
    }

    var arguments = finishCall()
    return SuperExpr.new(name, arguments[0], arguments[1])
  }

  // stringInterpolation: (interpolation expression )? string
  stringInterpolation() {
    var strings = []
    var expressions = []

    while (match(Token.interpolation)) {
      strings.add(_previous)
      expressions.add(expression())
    }

    // This error should never be reported. It's the lexer's job to ensure we
    // generate the right token sequence.
    strings.add(consume(Token.string, "Expect end of string interpolation."))

    return InterpolationExpr.new(strings, expressions)
  }

  // Utility methods.

  // Parses a left-associative series of infix operator expressions using any
  // of [tokenTypes] as operators and calling [parseOperand] to parse the left
  // and right operands.
  parseInfix(tokenTypes, parseOperand) {
    var expr = parseOperand.call()
    while (matchAny(tokenTypes)) {
      var operator = _previous
      ignoreLine()
      var right = parseOperand.call()
      expr = InfixExpr.new(expr, operator, right)
    }

    return expr
  }

  // If the next token has [type], consumes and returns it. Otherwise, returns
  // `null`.
  match(type) {
    if (peek() != type) return null
    return consume()
  }

  // Consumes and returns the next token if its type is contained in the list
  // [types].
  matchAny(types) {
    for (type in types) {
      var result = match(type)
      if (result) return result
    }

    return null
  }

  // Consumes zero or more newlines. Returns `true` if at least one was matched.
  matchLine() {
    if (!match(Token.line)) return false
    while (match(Token.line)) {
      // Do nothing.
    }

    return true
  }

  // Same as [matchLine()], but makes it clear that the intent is to discard
  // newlines appearing where this is called.
  ignoreLine() { matchLine() }

  // Consumes one or more newlines.
  consumeLine(error) {
    consume(Token.line, error)
    ignoreLine()
  }

  // Reads and consumes the next token.
  consume() {
    peek()
    _previous = _current
    _current = null
    return _previous
  }

  // Reads the next token if it is of [type]. Otherwise, discards it and
  // reports an error with [message]
  consume(type, message) {
    var token = consume()
    if (token.type != type) error(message)

    return token
  }

  // Returns the type of the next token.
  peek() {
    if (_current == null) _current = _lexer.readToken()
    return _current.type
  }

  /// Reports an error on the most recent token.
  error(message) {
    _reporter.error(message, [_current != null ? _current : _previous])
  }
}
