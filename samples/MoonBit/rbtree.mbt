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

enum Color {
  Red
  Black
}

enum Node {
  Leaf
  Node(Color, Node, Int, Bool, Node)
}

fn balance1(kv : Int, vv : Bool, t : Node, n : Node) -> Node {
  match n {
    Node(_, Node(Red, l, kx, vx, r1), ky, vy, r2) => Node(Red, Node(Black, l, kx, vx, r1), ky, vy, Node(Black, r2, kv, vv, t))
    Node(_, l1, ky, vy, Node(Red, l2, kx, vx, r)) => Node(Red, Node(Black, l1, ky, vy, l2), kx, vx, Node(Black, r, kv, vv, t))
    Node(_, l,  ky, vy, r)                        => Node(Black, Node(Red, l, ky, vy, r), kv, vv, t)
    _ => Leaf
  }
}

fn balance2(t : Node, kv : Int, vv : Bool, n : Node) -> Node {
  match n {
    Node(_, Node(Red, l, kx1, vx1, r1), ky, vy, r2)  => Node(Red, Node(Black, t, kv, vv, l), kx1, vx1, Node(Black, r1, ky, vy, r2))
    Node(_, l1, ky, vy, Node(Red, l2, kx2, vx2, r2)) => Node(Red, Node(Black, t, kv, vv, l1), ky, vy, Node(Black, l2, kx2, vx2, r2))
    Node(_, l, ky, vy, r)                            => Node(Black, t, kv, vv, Node(Red, l, ky, vy, r))
    _ => Leaf
  }
}

fn is_red(self : Node) -> Bool {
  match self {
    Node(Red, _, _, _, _) => true
    _ => false
  }
}

fn ins(self : Node, kx : Int, vx : Bool) -> Node {
  match self {
    Leaf => Node(Red, Leaf, kx, vx, Leaf)
    Node(Red, a, ky, vy, b) =>
      if kx < ky {
        Node(Red, a.ins(kx, vx), ky, vy, b)
      } else if ky == kx {
        Node(Red, a, kx, vx, b)
      } else {
        Node(Red, a, ky, vy, b.ins(kx, vx))
      }
    Node(Black, a, ky, vy, b) =>
      if kx < ky {
        if a.is_red() {
          balance1(ky, vy, b, a.ins(kx, vx))
        } else {
          Node(Black, a.ins(kx, vx), ky, vy, b)
        }
      } else if kx == ky {
        Node(Black, a, kx, vx, b)
      } else {
        if b.is_red() {
          balance2(a, ky, vy, b.ins(kx, vx))
        } else {
          Node(Black, a, ky, vy, b.ins(kx, vx))
        }
      }
  }
}

fn set_black(self : Node) -> Node {
  match self {
    Node(_, l, k, v, r) => Node(Black, l, k, v, r)
    e => e
  }
}

fn insert(self : Node, k : Int, v : Bool) -> Node {
  if self.is_red() {
    self.ins(k, v).set_black()
  } else {
    self.ins(k, v)
  }
}

fn fold[X](self : Node, f : (Int, Bool, X) -> X, d : X) -> X {
  match self {
    Leaf => d
    Node(_, l, k, v, r) => r.fold(f, f(k, v, l.fold(f, d)))
  }
}

fn mk_map(n : Int) -> Node {
  loop n, Leaf {
    0, m => m
    n, m => {
      let n1 = n - 1
      continue n1, m.insert(n1, n1 % 10 == 0)
    }
  }
}

fn main {
  let m = mk_map(4200000)
  let v = m.fold(fn { _, v, r => if v { r + 1 } else { r } }, 0)
  println(v)
}
