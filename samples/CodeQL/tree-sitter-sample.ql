/**
 * Sample taken from a tree-sitter-ql example.
 * Source: https://github.com/tree-sitter/tree-sitter-ql/blob/updated-ql-grammar/examples/002.ql
 * License: MIT
 */

private predicate foo(F f){
  f = f
}

int predicateWithResult(){
  result = 43
}

class A extends int {
  A() {
    this = -1
  }
}

select (A)-1
