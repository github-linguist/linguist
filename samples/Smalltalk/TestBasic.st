Koan subclass: TestBasic [
  <comment: 'A collection of introductory tests.'>

  testDeclarationAndAssignment [
    | declaration anotherDeclaration |
    "You must declare variables before using them."
    "Variables are separated by a single space."

    declaration _ 1.  "Squeak Smalltalk way to assign value"
    anotherDeclaration := 'string'.  "typical way to assign value
      (this will be used throughout the koans)"

    self expect: fillMeIn toEqual: declaration.
    self expect: fillMeIn toEqual: anotherDeclaration.
  ]

  testEqualSignIsNotAnAssignmentOperator [
    | variableA variableB value |

    variableA := variableB := 1234.  "multiple assignments work"
    value := variableA = variableB.  "equal is not used for assignment"

    self expect: fillMeIn toEqual: (variableA = variableB).

    "#== is a message that checks if identity is equal.  More about messages in the TestMessage koan."
  ]

  testMultipleStatementsInASingleLine [
    | variableA variableB variableC |

    "Multiple statements are separated by periods."
    variableA := 1. variableB := 2. variableC := 3.

    self expect: fillMeIn toEqual: variableA.
    self expect: fillMeIn toEqual: variableB.
    self expect: fillMeIn toEqual: variableC.
  ]

  testInequality [
    self expect: fillMeIn toEqual: ('hello' ~= 'world').

    "#~~ is a message that checks if identity is not equal.  More about messages in the TestMessage koan."
  ]

  testLogicalOr [
    | expression |

    expression := (3 > 4) | (5 < 6).

    self expect: fillMeIn toEqual: expression.
  ]

  testLogicalAnd [
    | expression |

    expression := (2 > 1) & ('a' < 'b').

    self expect: fillMeIn toEqual: expression.
  ]

  testNot [
    self expect: fillMeIn toEqual: true not.
  ]
]
