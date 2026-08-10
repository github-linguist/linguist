// Copyright 2024 International Digital Economy Academy
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

// Origin: https://github.com/moonbit-community/moonbit-native-benchmark2

typealias List[X] = @immut/list.T[X]

fn safe(queen : Int, diag : Int, xs : List[Int]) -> Bool {
  match xs {
    Nil => true
    Cons(q, qs) => queen != q && queen != q + diag && queen != q - diag && safe(queen, diag + 1, qs)
  }
}

fn append_safe(queen : Int, xs : List[Int], xss : List[List[Int]]) -> List[List[Int]] {
  if queen <= 0 {
    xss
  } else if safe(queen, 1, xs) {
    append_safe(queen - 1, xs, Cons(Cons(queen, xs), xss))
  } else {
    append_safe(queen - 1, xs, xss)
  }
}

fn extend(queen : Int, acc : List[List[Int]], xss : List[List[Int]]) -> List[List[Int]] {
  match xss {
    Nil => acc
    Cons(xs, rest) => extend(queen, append_safe(queen, xs, acc), rest)
  }
}

fn find_solutions(n : Int, queen : Int) -> List[List[Int]] {
  if queen == 0 {
    Cons(Nil, Nil)
  } else {
    extend(n, Nil, find_solutions(n, queen - 1))
  }
}

fn queens(n : Int) -> Int {
  find_solutions(n, n).length()
}

fn main {
  println(queens(13))
}
