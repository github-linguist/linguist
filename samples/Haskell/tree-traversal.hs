data Tree a = Empty
            | Node { value :: a,
                     left  :: Tree a,
                     right :: Tree a }

preorder, inorder, postorder, levelorder :: Tree a -> [a]

preorder Empty        = []
preorder (Node v l r) =    [v]
                        ++ preorder l
                        ++ preorder r

inorder Empty        = []
inorder (Node v l r) =    inorder l
                       ++ [v]
                       ++ inorder r

postorder Empty        = []
postorder (Node v l r) =    postorder l
                         ++ postorder r
                         ++ [v]

levelorder x = loop [x]
    where loop []                = []
          loop (Empty      : xs) = loop xs
          loop (Node v l r : xs) = v : loop (xs ++ [l,r])

tree :: Tree Int
tree = Node 1
            (Node 2
                  (Node 4
                        (Node 7 Empty Empty)
                        Empty)
                  (Node 5 Empty Empty))
            (Node 3
                  (Node 6
                        (Node 8 Empty Empty)
                        (Node 9 Empty Empty))
                  Empty)

main :: IO ()
main = do print $ preorder tree
          print $ inorder tree
          print $ postorder tree
          print $ levelorder tree
