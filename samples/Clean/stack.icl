implementation module stack
import StdEnv

:: Stack a :== [a]

newStack :: (Stack a)
newStack = []

push :: a (Stack a) -> Stack a
push x s = [x:s]

pushes :: [a] (Stack a) -> Stack a
pushes x s = x ++ s

pop :: (Stack a) -> Stack a
pop [] = abort "Cannot use pop on an empty stack"
pop [e:s] = s

popn :: Int (Stack a) -> Stack a
popn n s  = drop n s

top :: (Stack a) -> a
top [] = abort "Cannot use top on an empty stack"
top [e:s] = e

topn :: Int (Stack a) -> [a]
topn n s = take n s
elements :: (Stack a) -> [a]
elements s = s

count :: (Stack a) -> Int
count s = length s

