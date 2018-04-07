// https://github.com/munificent/wrenalyzer/blob/master/ast.wren
// MIT License

class Node {}

class Expr is Node {}

class Stmt is Node {}

class Module is Node {
  construct new(statements) {
    _statements = statements
  }

  statements { _statements }

  accept(visitor) { visitor.visitModule(this) }

  toString { "Module(%(_statements))" }
}

class MapEntry {
  construct new(key, value) {
    _key = key
    _value = value
  }

  key { _key }
  value { _value }

  toString { "%(_key): %(_value)" }
}

class Method {
  construct new(foreignKeyword, staticKeyword, constructKeyword, name, body) {
    _foreignKeyword = foreignKeyword
    _staticKeyword = staticKeyword
    _constructKeyword = constructKeyword
    _name = name
    _body = body
  }

  foreignKeyword { _foreignKeyword }
  staticKeyword { _staticKeyword }
  constructKeyword { _constructKeyword }
  name { _name }
  body { _body }

  accept(visitor) { visitor.visitMethod(this) }

  toString {
    return "Method(%(_staticKeyword) %(_constructKeyword) %(_name) %(_body))"
  }
}

/// A block argument or method body.
class Body {
  construct new(parameters, expression, statements) {
    _parameters = parameters
    _expression = expression
    _statements = statements
  }

  parameters { _parameters }
  expression { _expression }
  statements { _statements }

  accept(visitor) { visitor.visitBody(this) }

  toString {
    return "Body(%(_parameters) %(_expression) %(_statements))"
  }
}

class ListExpr is Expr {
  construct new(leftBracket, elements, rightBracket) {
    _leftBracket = leftBracket
    _elements = elements
    _rightBracket = rightBracket
  }

  leftBracket { _leftBracket }
  elements { _elements }
  rightBracket { _rightBracket }

  accept(visitor) { visitor.visitListExpr(this) }

  toString {
    return "List(%(_leftBracket) %(_elements) %(_rightBracket))"
  }
}

class ThisExpr is Expr {
  construct new(keyword) {
    _keyword = keyword
  }

  keyword { _keyword }

  accept(visitor) { visitor.visitThisExpr(this) }

  toString {
    return "This(%(_keyword))"
  }
}

class NullExpr is Expr {
  construct new(value) {
    _value = value
  }

  value { _value }

  accept(visitor) { visitor.visitNullExpr(this) }

  toString {
    return "Null(%(_value))"
  }
}

class StaticFieldExpr is Expr {
  construct new(name) {
    _name = name
  }

  name { _name }

  accept(visitor) { visitor.visitStaticFieldExpr(this) }

  toString {
    return "StaticField(%(_name))"
  }
}

class FieldExpr is Expr {
  construct new(name) {
    _name = name
  }

  name { _name }

  accept(visitor) { visitor.visitFieldExpr(this) }

  toString {
    return "Field(%(_name))"
  }
}

class CallExpr is Expr {
  construct new(receiver, name, arguments, blockArgument) {
    _receiver = receiver
    _name = name
    _arguments = arguments
    _blockArgument = blockArgument
  }

  receiver { _receiver }
  name { _name }
  arguments { _arguments }
  blockArgument { _blockArgument }

  accept(visitor) { visitor.visitCallExpr(this) }

  toString {
    return "Call(%(_receiver) %(_name) %(_arguments) %(_blockArgument))"
  }
}

class PrefixExpr is Expr {
  construct new(operator, right) {
    _operator = operator
    _right = right
  }

  operator { _operator }
  right { _right }

  accept(visitor) { visitor.visitPrefixExpr(this) }

  toString {
    return "Prefix(%(_operator) %(_right))"
  }
}

class GroupingExpr is Expr {
  construct new(leftParen, expression, rightParen) {
    _leftParen = leftParen
    _expression = expression
    _rightParen = rightParen
  }

  leftParen { _leftParen }
  expression { _expression }
  rightParen { _rightParen }

  accept(visitor) { visitor.visitGroupingExpr(this) }

  toString {
    return "Grouping(%(_leftParen) %(_expression) %(_rightParen))"
  }
}

class AssignmentExpr is Expr {
  construct new(target, equal, value) {
    _target = target
    _equal = equal
    _value = value
  }

  target { _target }
  equal { _equal }
  value { _value }

  accept(visitor) { visitor.visitAssignmentExpr(this) }

  toString {
    return "Assignment(%(_target) %(_equal) %(_value))"
  }
}

class InfixExpr is Expr {
  construct new(left, operator, right) {
    _left = left
    _operator = operator
    _right = right
  }

  left { _left }
  operator { _operator }
  right { _right }

  accept(visitor) { visitor.visitInfixExpr(this) }

  toString {
    return "Infix(%(_left) %(_operator) %(_right))"
  }
}

class MapExpr is Expr {
  construct new(leftBrace, entries, rightBrace) {
    _leftBrace = leftBrace
    _entries = entries
    _rightBrace = rightBrace
  }

  leftBrace { _leftBrace }
  entries { _entries }
  rightBrace { _rightBrace }

  accept(visitor) { visitor.visitMapExpr(this) }

  toString {
    return "Map(%(_leftBrace) %(_entries) %(_rightBrace))"
  }
}

class ConditionalExpr is Expr {
  construct new(condition, question, thenBranch, colon, elseBranch) {
    _condition = condition
    _question = question
    _thenBranch = thenBranch
    _colon = colon
    _elseBranch = elseBranch
  }

  condition { _condition }
  question { _question }
  thenBranch { _thenBranch }
  colon { _colon }
  elseBranch { _elseBranch }

  accept(visitor) { visitor.visitConditionalExpr(this) }

  toString {
    return "Conditional(%(_condition) %(_question) %(_thenBranch) %(_colon) %(_elseBranch))"
  }
}

class NumExpr is Expr {
  construct new(value) {
    _value = value
  }

  value { _value }

  accept(visitor) { visitor.visitNumExpr(this) }

  toString {
    return "Num(%(_value))"
  }
}

class SuperExpr is Expr {
  construct new(name, arguments, blockArgument) {
    _name = name
    _arguments = arguments
    _blockArgument = blockArgument
  }

  name { _name }
  arguments { _arguments }
  blockArgument { _blockArgument }

  accept(visitor) { visitor.visitSuperExpr(this) }

  toString {
    return "Super(%(_name) %(_arguments) %(_blockArgument))"
  }
}

class StringExpr is Expr {
  construct new(value) {
    _value = value
  }

  value { _value }

  accept(visitor) { visitor.visitStringExpr(this) }

  toString {
    return "String(%(_value))"
  }
}

class SubscriptExpr is Expr {
  construct new(receiver, leftBracket, arguments, rightBracket) {
    _receiver = receiver
    _leftBracket = leftBracket
    _arguments = arguments
    _rightBracket = rightBracket
  }

  receiver { _receiver }
  leftBracket { _leftBracket }
  arguments { _arguments }
  rightBracket { _rightBracket }

  accept(visitor) { visitor.visitSubscriptExpr(this) }

  toString {
    return "Subscript(%(_receiver) %(_leftBracket) %(_arguments) %(_rightBracket))"
  }
}

class BoolExpr is Expr {
  construct new(value) {
    _value = value
  }

  value { _value }

  accept(visitor) { visitor.visitBoolExpr(this) }

  toString {
    return "Bool(%(_value))"
  }
}

class InterpolationExpr is Expr {
  construct new(strings, expressions) {
    _strings = strings
    _expressions = expressions
  }

  strings { _strings }
  expressions { _expressions }

  accept(visitor) { visitor.visitInterpolationExpr(this) }

  toString {
    return "Interpolation(%(_strings) %(_expressions))"
  }
}

class ForStmt is Stmt {
  construct new(variable, iterator, body) {
    _variable = variable
    _iterator = iterator
    _body = body
  }

  variable { _variable }
  iterator { _iterator }
  body { _body }

  accept(visitor) { visitor.visitForStmt(this) }

  toString {
    return "For(%(_variable) %(_iterator) %(_body))"
  }
}

class ReturnStmt is Stmt {
  construct new(keyword, value) {
    _keyword = keyword
    _value = value
  }

  keyword { _keyword }
  value { _value }

  accept(visitor) { visitor.visitReturnStmt(this) }

  toString {
    return "Return(%(_keyword) %(_value))"
  }
}

class BlockStmt is Stmt {
  construct new(statements) {
    _statements = statements
  }

  statements { _statements }

  accept(visitor) { visitor.visitBlockStmt(this) }

  toString {
    return "Block(%(_statements))"
  }
}

class VarStmt is Stmt {
  construct new(name, initializer) {
    _name = name
    _initializer = initializer
  }

  name { _name }
  initializer { _initializer }

  accept(visitor) { visitor.visitVarStmt(this) }

  toString {
    return "Var(%(_name) %(_initializer))"
  }
}

class ImportStmt is Stmt {
  construct new(path, variables) {
    _path = path
    _variables = variables
  }

  path { _path }
  variables { _variables }

  accept(visitor) { visitor.visitImportStmt(this) }

  toString {
    return "Import(%(_path) %(_variables))"
  }
}

class IfStmt is Stmt {
  construct new(condition, thenBranch, elseBranch) {
    _condition = condition
    _thenBranch = thenBranch
    _elseBranch = elseBranch
  }

  condition { _condition }
  thenBranch { _thenBranch }
  elseBranch { _elseBranch }

  accept(visitor) { visitor.visitIfStmt(this) }

  toString {
    return "If(%(_condition) %(_thenBranch) %(_elseBranch))"
  }
}

class BreakStmt is Stmt {
  construct new(keyword) {
    _keyword = keyword
  }

  keyword { _keyword }

  accept(visitor) { visitor.visitBreakStmt(this) }

  toString {
    return "Break(%(_keyword))"
  }
}

class WhileStmt is Stmt {
  construct new(condition, body) {
    _condition = condition
    _body = body
  }

  condition { _condition }
  body { _body }

  accept(visitor) { visitor.visitWhileStmt(this) }

  toString {
    return "While(%(_condition) %(_body))"
  }
}

class ClassStmt is Stmt {
  construct new(foreignKeyword, name, superclass, methods) {
    _foreignKeyword = foreignKeyword
    _name = name
    _superclass = superclass
    _methods = methods
  }

  foreignKeyword { _foreignKeyword }
  name { _name }
  superclass { _superclass }
  methods { _methods }

  accept(visitor) { visitor.visitClassStmt(this) }

  toString {
    return "Class(%(_foreignKeyword) %(_name) %(_superclass) %(_methods))"
  }
}
