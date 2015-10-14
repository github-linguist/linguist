(*--build-config
    options:--admit_fsi Set --z3timeout 20;
    other-files:set.fsi heap.fst st.fst all.fst list.fst
  --*)
module QuickSort
open List
#set-options "--initial_ifuel 2 --initial_fuel 1 --max_ifuel 2 --max_fuel 1"

(* Specification of sortedness according to some comparison function f *)
val sorted: ('a -> 'a -> Tot bool) -> list 'a -> Tot bool
let rec sorted f l = match l with
    | [] -> true
    | [x] -> true
    | x::y::xs -> f x y && sorted f (y::xs)


type total_order (a:Type) (f: (a -> a -> Tot bool)) =
    (forall a. f a a)                                           (* reflexivity   *)
    /\ (forall a1 a2. (f a1 a2 /\ f a2 a1)  ==> a1 == a2)       (* anti-symmetry *)
    /\ (forall a1 a2 a3. f a1 a2 /\ f a2 a3 ==> f a1 a3)        (* transitivity  *)
    /\ (forall a1 a2. f a1 a2 \/ f a2 a1)                       (* totality      *)


val count : 'a -> list 'a -> Tot nat
let rec count (x:'a) (l:list 'a) = match l with
  | hd::tl -> (if hd=x then 1 else 0) + count x tl
  | [] -> 0


val mem_count : x:'a -> l:list 'a ->
      Lemma (requires True) (ensures (mem x l == (count x l > 0)))
      [SMTPat (mem x l)] (decreases l)
let rec mem_count x l = match l with
  | [] -> ()
  | _::tl -> mem_count x tl


val append_count: l:list 'a -> m:list 'a -> x:'a
               -> Lemma (requires True)
                        (ensures (count x (append l m) = (count x l + count x m)))
                        [SMTPat (count x (append l m))]
let rec append_count l m x = match l with
  | [] -> ()
  | hd::tl -> append_count tl m x


val partition: ('a -> Tot bool) -> list 'a -> Tot (list 'a * list 'a)
let rec partition p = function
  | [] -> [], []
  | hd::tl ->
     let l1, l2 = partition p tl in
     if p hd
     then hd::l1, l2
     else l1, hd::l2

opaque logic type trigger (#a:Type) (x:a) = True
val partition_lemma: f:('a -> Tot bool) -> l:list 'a -> Lemma (requires True)
      (ensures (forall hi lo. (hi, lo) = partition f l
                ==>  (length l = length hi + length lo
                 /\ (forall x.{:pattern (trigger x)}
                              trigger x ==>
                               (mem x hi ==> f x)
                           /\  (mem x lo ==> not (f x))
                           /\  (count x l = count x hi + count x lo)))))
      [SMTPat (partition f l)]
let rec partition_lemma f l = match l with
    | [] -> ()
    | hd::tl -> cut (trigger hd); partition_lemma f tl


val sorted_app_lemma: #a:Type
                      -> f:(a -> a -> Tot bool){total_order a f}
                      -> l1:list a{sorted f l1}
                      -> l2:list a{sorted f l2}
                      -> pivot:a
                      -> Lemma (requires ((forall y.{:pattern (trigger y)} trigger y ==>
                                                        ((mem y l1 ==> not (f pivot y))
                                                      /\ (mem y l2 ==> f pivot y)))))
                               (ensures (sorted f (append l1 (pivot::l2))))
                               [SMTPat (sorted f (append l1 (pivot::l2)))]
let rec sorted_app_lemma f l1 l2 pivot = match l1 with
    | [] -> if is_Cons l2 then cut (trigger (Cons.hd l2)) else ()
    | hd::tl -> cut (trigger hd); sorted_app_lemma f tl l2 pivot

val sort: f:('a -> 'a -> Tot bool){total_order 'a f}
       -> l:list 'a
       -> Tot (m:list 'a{sorted f m /\ (forall i.{:pattern trigger i} trigger i ==> (count i l = count i m))})
              (decreases (length l))
let rec sort f = function
  | [] -> []
  | pivot::tl ->
    let hi, lo = partition (f pivot) tl in
    append (sort f lo) (pivot::(sort f hi))
