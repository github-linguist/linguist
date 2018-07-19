data Color = R | B
data Tree a = E | T Color (Tree a) a (Tree a)

balance :: Color -> Tree a -> a -> Tree a -> Tree a
balance B (T R (T R a x b) y c          ) z d                               = T R (T B a x b) y (T B c z d)
balance B (T R a           x (T R b y c)) z d                               = T R (T B a x b) y (T B c z d)
balance B a                               x (T R (T R b y c) z d          ) = T R (T B a x b) y (T B c z d)
balance B a                               x (T R b           y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance col a x b = T col a x b

insert :: Ord a => a -> Tree a -> Tree a
insert x s = T B a y b where
  ins E          =  T R E x E
  ins s@(T col a y b)
    | x < y      =  balance col (ins a) y b
    | x > y      =  balance col a y (ins b)
    | otherwise  =  s
  T _ a y b = ins s
