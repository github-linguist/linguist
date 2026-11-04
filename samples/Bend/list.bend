# bend-lemmas: List. Base's List.append recurses on its first list and
# List.reverse is List.reverse.go with an accumulator, so a lemma is
# an induction on the first list, and a lemma about reverse is stated
# over reverse.go with the accumulator generalized until the induction
# hypothesis is the goal. Lists are as reusable as their elements
# (Kind(a)), so a generic lemma may hand a list to one lemma only;
# Data lists (&2) get +xs and may compose freely (see the last one).
#
#   bend list.bend

import Base
import ./nat.bend as N

# Append
# ------

def append_nil(a, -A: Kind(a), xs: List<a, A>) -> {List.append(a, A, xs, Nil{}) == xs : List<a, A>}:
  match xs:
    case Nil{}:
      {==}
    case h <> t:
      %append_nil(a, A, t) : {h <> List.append(a, A, t, Nil{}) == h <> _ : List<a, A>}
      {==}

def append_assoc(a, -A: Kind(a), xs: List<a, A>, -ys: List<a, A>, -zs: List<a, A>)
  -> {List.append(a, A, xs, List.append(a, A, ys, zs)) == List.append(a, A, List.append(a, A, xs, ys), zs) : List<a, A>}:
  match xs:
    case Nil{}:
      {==}
    case h <> t:
      %append_assoc(a, A, t, ys, zs) : {h <> List.append(a, A, t, List.append(a, A, ys, zs)) == h <> _ : List<a, A>}
      {==}

def length_append(a, -A: Kind(a), xs: List<a, A>, -ys: List<a, A>)
  -> {List.length(a, A, List.append(a, A, xs, ys)) == Nat.add(List.length(a, A, xs), List.length(a, A, ys)) : Nat}:
  match xs:
    case Nil{}:
      {==}
    case h <> t:
      %length_append(a, A, t, ys) : {1n+List.length(a, A, List.append(a, A, t, ys)) == 1n+_ : Nat}
      {==}

# Reverse
# -------

# reverse.go onto acc1, then append acc2, is reverse.go onto their append
def rev_go_app(a, -A: Kind(a), xs: List<a, A>, -acc1: List<a, A>, -acc2: List<a, A>)
  -> {List.append(a, A, List.reverse.go(a, A, xs, acc1), acc2) == List.reverse.go(a, A, xs, List.append(a, A, acc1, acc2)) : List<a, A>}:
  match xs:
    case Nil{}:
      {==}
    case h <> t:
      rev_go_app(a, A, t, h <> acc1, acc2)

# reverse.go onto acc is the plain reverse, appended to acc
def rev_acc(a, -A: Kind(a), xs: List<a, A>, -acc: List<a, A>)
  -> {List.append(a, A, List.reverse(a, A, xs), acc) == List.reverse.go(a, A, xs, acc) : List<a, A>}:
  rev_go_app(a, A, xs, Nil{}, acc)

# reversing twice, generalized: the acc side reverses back onto xs
def rev_rev_go(a, -A: Kind(a), xs: List<a, A>, -acc: List<a, A>)
  -> {List.reverse(a, A, List.reverse.go(a, A, xs, acc)) == List.reverse.go(a, A, acc, xs) : List<a, A>}:
  match xs:
    case Nil{}:
      {==}
    case h <> t:
      rev_rev_go(a, A, t, h <> acc)

def rev_rev(a, -A: Kind(a), xs: List<a, A>) -> {List.reverse(a, A, List.reverse(a, A, xs)) == xs : List<a, A>}:
  rev_rev_go(a, A, xs, Nil{})

# Length of a reverse: this one needs a second lemma on the same list,
# so it is stated for Data lists (&2), whose +xs may be used twice.
# reverse.go's length is both lengths added, and add_succ moves the
# accumulator's cons out to the front
def length_rev_go(-A: Data, +xs: List<&2, A>, -acc: List<&2, A>)
  -> {List.length(&2, A, List.reverse.go(&2, A, xs, acc)) == Nat.add(List.length(&2, A, xs), List.length(&2, A, acc)) : Nat}:
  match xs:
    case Nil{}:
      {==}
    case h <> t:
      %N.add_succ(List.length(&2, A, t), List.length(&2, A, acc)) : {List.length(&2, A, List.reverse.go(&2, A, t, h <> acc)) == _ : Nat}
      length_rev_go(A, t, h <> acc)

def length_reverse(-A: Data, +xs: List<&2, A>) -> {List.length(&2, A, List.reverse(&2, A, xs)) == List.length(&2, A, xs) : Nat}:
  %N.add_zero(List.length(&2, A, xs)) : {List.length(&2, A, List.reverse(&2, A, xs)) == _ : Nat}
  length_rev_go(A, xs, Nil{})

# Take, drop, replicate, range, zip
# ---------------------------------

# take and drop split a list; both consume the list, so Data only
def take_drop(-A: Data, +xs: List<&2, A>, +n: Nat) -> {List.append(&2, A, List.take(&2, A, xs, n), List.drop(&2, A, xs, n)) == xs : List<&2, A>}:
  match xs n:
    case Nil{} m:
      {==}
    case h <> t 0n:
      {==}
    case h <> t 1n+p:
      %take_drop(A, t, p) : {h <> List.append(&2, A, List.take(&2, A, t, p), List.drop(&2, A, t, p)) == h <> _ : List<&2, A>}
      {==}

def length_take_le(a, -A: Kind(a), xs: List<a, A>, n: Nat) -> N.LE(List.length(a, A, List.take(a, A, xs, n)), n):
  match xs n:
    case Nil{} m:
      Unit{}
    case h <> t 0n:
      Unit{}
    case h <> t 1n+p:
      length_take_le(a, A, t, p)

def length_replicate(-A: Data, +n: Nat, +x: A) -> {List.length(&2, A, List.replicate(A, n, x)) == n : Nat}:
  match n:
    case 0n:
      {==}
    case 1n+p:
      %length_replicate(A, p, x) : {1n+List.length(&2, A, List.replicate(A, p, x)) == 1n+_ : Nat}
      {==}

# range.go counts down n onto acc: add_succ moves each cons out front
def length_range_go(+n: Nat, -acc: List<&2, Nat>) -> {List.length(&2, Nat, List.range.go(n, acc)) == Nat.add(n, List.length(&2, Nat, acc)) : Nat}:
  match n:
    case 0n:
      {==}
    case 1n+p:
      %N.add_succ(p, List.length(&2, Nat, acc)) : {List.length(&2, Nat, List.range.go(p, p <> acc)) == _ : Nat}
      length_range_go(p, p <> acc)

def length_range(+n: Nat) -> {List.length(&2, Nat, List.range(n)) == n : Nat}:
  %N.add_zero(n) : {List.length(&2, Nat, List.range(n)) == _ : Nat}
  length_range_go(n, Nil{})

def length_zip_le(a, -A: Kind(a), b, -B: Kind(b), xs: List<a, A>, ys: List<b, B>) -> N.LE(List.length(&1, A & B, List.zip(a, A, b, B, xs, ys)), List.length(a, A, xs)):
  match xs ys:
    case Nil{} w:
      Unit{}
    case h <> t Nil{}:
      Unit{}
    case x <> xt y <> yt:
      length_zip_le(a, A, b, B, xt, yt)

# Membership and permutation, on Nat lists
# ----------------------------------------
# In(x, xs) is the type of proofs that x is in xs: a clash at the head
# or a proof for the tail. A generic version would need an equality
# decider on A; Nat's is structural.

def In(x: Nat, xs: List<&2, Nat>) -> Type:
  match xs:
    case Nil{}:
      Empty
    case h <> t:
      Or({x == h : Nat}, In(x, t))

def in_append_l(-x: Nat, xs: List<&2, Nat>, -ys: List<&2, Nat>, e: In(x, xs)) -> In(x, List.append(&2, Nat, xs, ys)):
  match xs:
    case Nil{}:
      match e:
    case h <> t:
      match e:
        case Inl{q}:
          Inl{q}
        case Inr{r}:
          Inr{in_append_l(x, t, ys, r)}

def in_append_r(-x: Nat, xs: List<&2, Nat>, -ys: List<&2, Nat>, e: In(x, ys)) -> In(x, List.append(&2, Nat, xs, ys)):
  match xs:
    case Nil{}:
      e
    case h <> t:
      Inr{in_append_r(x, t, ys, e)}

# how many times x occurs in xs, as in demos/proof_insertion_sort
def bump(b: Bool, n: Nat) -> Nat:
  match b:
    case True{}:
      1n+n
    case False{}:
      n

def count(+x: Nat, xs: List<&2, Nat>) -> Nat:
  match xs:
    case Nil{}:
      0n
    case h <> t:
      bump(Cmp.is_eq(Nat.cmp(x, h)), count(x, t))

def bump_add(b: Bool, +n: Nat, +m: Nat) -> {bump(b, Nat.add(n, m)) == Nat.add(bump(b, n), m) : Nat}:
  match b:
    case True{}:
      {==}
    case False{}:
      {==}

def bump_swap(a: Bool, b: Bool, +n: Nat) -> {bump(a, bump(b, n)) == bump(b, bump(a, n)) : Nat}:
  match a b:
    case True{} True{}:
      {==}
    case True{} False{}:
      {==}
    case False{} True{}:
      {==}
    case False{} False{}:
      {==}

def count_append(+x: Nat, +xs: List<&2, Nat>, +ys: List<&2, Nat>) -> {count(x, List.append(&2, Nat, xs, ys)) == Nat.add(count(x, xs), count(x, ys)) : Nat}:
  match xs:
    case Nil{}:
      {==}
    case h <> t:
      %Equal.sym(Nat, count(x, List.append(&2, Nat, t, ys)), Nat.add(count(x, t), count(x, ys)), count_append(x, t, ys)) : {bump(Cmp.is_eq(Nat.cmp(x, h)), _) == Nat.add(bump(Cmp.is_eq(Nat.cmp(x, h)), count(x, t)), count(x, ys)) : Nat}
      bump_add(Cmp.is_eq(Nat.cmp(x, h)), count(x, t), count(x, ys))

# Perm(xs, ys): every value occurs as often in both. A def returning a
# function type; a proof is a lambda over the value
def Perm(xs: List<&2, Nat>, ys: List<&2, Nat>) -> Type:
  @x: Nat -> {count(x, xs) == count(x, ys) : Nat}

def perm_refl(-xs: List<&2, Nat>) -> Perm(xs, xs):
  x => {==}

def perm_sym(-xs: List<&2, Nat>, -ys: List<&2, Nat>, p: Perm(xs, ys)) -> Perm(ys, xs):
  +x => Equal.sym(Nat, count(x, xs), count(x, ys), p(x))

def perm_trans(-xs: List<&2, Nat>, -ys: List<&2, Nat>, -zs: List<&2, Nat>, p: Perm(xs, ys), q: Perm(ys, zs)) -> Perm(xs, zs):
  +x => Equal.trans(Nat, count(x, xs), count(x, ys), count(x, zs), p(x), q(x))

def perm_swap(+a: Nat, +b: Nat, +t: List<&2, Nat>) -> Perm(a <> b <> t, b <> a <> t):
  +x => bump_swap(Cmp.is_eq(Nat.cmp(x, a)), Cmp.is_eq(Nat.cmp(x, b)), count(x, t))

def perm_cons(-h: Nat, -xs: List<&2, Nat>, -ys: List<&2, Nat>, p: Perm(xs, ys)) -> Perm(h <> xs, h <> ys):
  +x =>
    %p(x) : {bump(Cmp.is_eq(Nat.cmp(x, h)), count(x, xs)) == bump(Cmp.is_eq(Nat.cmp(x, h)), _) : Nat}
    {==}
