definition module stack

:: Stack a

newStack :: (Stack a)
push :: a (Stack a) -> Stack a
pushes :: [a] (Stack a) -> Stack a
pop :: (Stack a) -> Stack a
popn :: Int (Stack a) -> Stack a
top :: (Stack a) -> a
topn :: Int (Stack a) -> [a]
elements :: (Stack a) -> [a]
count :: (Stack a) -> Int

