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

enum Expr {
  Val(Int)
  Var(String)
  Add(Expr, Expr)
  Mul(Expr, Expr)
  Pow(Expr, Expr)
  Ln(Expr)
}

fn pown(a : Int, n : Int) -> Int {
  match n {
    0 => 1
    1 => a
    n => {
      let b = pown(a, n >> 2)
      b * b * (if (n & 1) == 0 { 1 } else { a })
    }
  }
}

fn Expr::op_add(e1 : Expr, e2 : Expr) -> Expr {
  match (e1, e2) {
    (Val(n), Val(m)) => Val(n + m)
    (Val(0), f) | (f, Val(0)) => f
    (f, Val(n)) => Val(n) + f
    (Val(n), Add(Val(m), f)) => Val(n + m) + f
    (f, Add(Val(n), g)) => Val(n) + (f + g)
    (Add(f, g), h) => f + (g + h)
    (f, g) => Add(f, g)
  }
}

fn Expr::op_mul(e1 : Expr, e2 : Expr) -> Expr {
  match (e1, e2) {
    (Val(n), Val(m)) => Val(n * m)
    (Val(0), _) | (_, Val(0)) => Val(0)
    (Val(1), f) | (f, Val(1)) => f
    (f, Val(n)) => Val(n) * f
    (Val(n), Mul(Val(m), f)) => Val(n * m) * f
    (f, Mul(Val(n), g)) => Val(n) * (f * g)
    (Mul(f, g), h) => f * (g * h)
    (f, g) => Mul(f, g)
  }
}

fn pow(self : Expr, e2 : Expr) -> Expr {
  match (self, e2) {
    (Val(m), Val(n)) => Val(pown(m, n))
    (_, Val(0)) => Val(1)
    (f, Val(1)) => f
    (Val(0), _) => Val(0)
    (f, g) => Pow(f, g)
  }
}

fn ln(e : Expr) -> Expr {
  match e {
    Val(1) => Val(0)
    f => Ln(f)
  }
}

fn Expr::d(f : Expr, x : String) -> Expr {
  match f {
    Val(_) => Val(0)
    Var(y) => if x == y { Val(1) } else { Val(0) }
    Add(f, g) => f.d(x) + g.d(x)
    Mul(f, g) => f * g.d(x) + g * f.d(x)
    Pow(f, g) => f.pow(g) * (g * f.d(x) * f.pow(Val(-1)) + ln(f) * g.d(x))
    Ln(f) => f.d(x) * f.pow(Val(-1))
  }
}

fn count(self : Expr) -> Int {
  match self {
    Val(_) | Var(_) => 1
    Add(f, g) | Mul(f, g) | Pow(f, g) => f.count() + g.count()
    Ln(f) => f.count()
  }
}

fn nest(f : (Int, Expr) -> Expr, n : Int, x : Expr) -> Expr {
  loop n, x {
    0, x => x
    i, x => continue i - 1, f(n - i, x)
  }
}

fn deriv(i : Int, f : Expr) -> Expr {
  let d = f.d("x")
  println("\{i + 1} count: \{d.count()}")
  d
}

fn main {
  let x = Var("x")
  let f = pow(x, x)
  let _ = nest(deriv, 10, f)
}
