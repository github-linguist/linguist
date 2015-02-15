staload "prelude/DATS/list.dats"

datatype Tree (a:t@ype) =
  | Empty  (a)
  | Node (a) of (a, Tree a, Tree a)


fun print_list
(list: List int): void =
  case list of
  | nil ()  => ()
  | x :: xs => (print! (x, " "); print_list xs)

overload print with print_list

#define ++ list_append
infixr 40 ++


fun {a:t@ype} preorder
(tree: Tree a): List a =
  case tree of
  | Empty ()       => nil ()
  | Node (x, l, r) => '[x] ++ preorder l ++ preorder r

fun {a:t@ype} inorder
(tree: Tree a): List a =
  case tree of
  | Empty ()       => nil ()
  | Node (x, l, r) => inorder l ++ '[x] ++ inorder r

fun {a:t@ype} postorder
(tree: Tree a): List a =
  case tree of
  | Empty ()       => nil ()
  | Node (x, l, r) => postorder l ++ postorder r ++ '[x]

fun {a:t@ype} levelorder
(tree: Tree a): List a = loop '[tree]
where {
  fun loop (list: List (Tree a)): List a =
    case list of
    | nil ()               => nil ()
    | Empty () :: xs       => loop xs
    | Node (x, l, r) :: xs => x :: loop (xs ++ '[l, r])
}


implement main () = let
  val tree =
    Node (1, Node (2, Node (4, Node (7, Empty (), Empty ()),
                               Empty ()),
                      Node (5, Empty (), Empty ())),
             Node (3, Node (6, Node (8, Empty (), Empty ()),
                               Node (9, Empty (), Empty ())),
                      Empty ()))
in
  print! ("preorder:\t", preorder tree, "\n");
  print! ("inorder:\t", inorder tree, "\n");
  print! ("postorder:\t", postorder tree, "\n");
  print! ("level-order:\t", levelorder tree, "\n");
end
