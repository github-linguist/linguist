import qualified Data.Vector.Unboxed.Mutable as V
import Data.STRef
import Control.Monad (forM_, when)
import Control.Monad.ST

dir :: [(Int, Int)]
dir = [(1, 0), (-1, 0), (0, -1), (0, 1)]

data Env = Env { w, h, len, count, ret :: !Int, next :: ![Int] }

cutIt :: STRef s Env -> ST s ()
cutIt env = do
    e <- readSTRef env
    when (odd $ h e) $ modifySTRef env $ \en -> en { h = w e,
        w = h e }
    e <- readSTRef env
    if odd (h e)
        then modifySTRef env $ \en -> en { ret = 0 }
        else
            if w e == 1
            then modifySTRef env $ \en -> en { ret = 1 }
            else do
                let blen = (h e + 1) * (w e + 1) - 1
                    t = (h e `div` 2) * (w e + 1) + (w e `div` 2)
                modifySTRef env $ \en -> en { len = blen,
                    count = 0,
                    next = [ w e + 1, (negate $ w e) - 1, -1, 1] }
                grid <- V.replicate (blen + 1) False
                case odd (w e) of
                    True -> do
                        V.write grid t True
                        V.write grid (t + 1) True
                        walk grid (h e `div` 2) (w e `div` 2 - 1)
                        e1 <- readSTRef env
                        let res1 = count e1
                        modifySTRef env $ \en -> en { count = 0 }
                        walk grid (h e `div` 2 - 1) (w e `div` 2)
                        modifySTRef env $ \en -> en { ret = res1 +
                            (count en * 2) }
                    False -> do
                        V.write grid t True
                        walk grid (h e `div` 2) (w e `div` 2 - 1)
                        e2 <- readSTRef env
                        let count2 = count e2
                        if h e == w e
                            then modifySTRef env $ \en -> en { ret =
                                count2 * 2 }
                            else do
                                walk grid (h e `div` 2 - 1)
                                    (w e `div` 2)
                                modifySTRef env $ \en -> en { ret =
                                    count en }
    where
        walk grid y x = do
            e <- readSTRef env
            if y <= 0 || y >= h e || x <= 0 || x >= w e
                then modifySTRef env $ \en -> en { count = count en + 1 }
                else do
                    let t = y * (w e + 1) + x
                    V.write grid t True
                    V.write grid (len e - t) True
                    forM_ (zip (next e) [0..3]) $ \(n, d) -> do
                        g <- V.read grid (t + n)
                        when (not g) $
                            walk grid (y + fst (dir !! d)) (x + snd (dir !! d))
                    V.write grid t False
                    V.write grid (len e - t) False

cut :: (Int, Int) -> Int
cut (x, y) = runST $ do
    env <- newSTRef $ Env { w = y, h = x, len = 0, count = 0, ret = 0, next = [] }
    cutIt env
    result <- readSTRef env
    return $ ret result

main :: IO ()
main = do
    mapM_ (\(x, y) -> when (even (x * y)) (putStrLn $
        show x ++ " x " ++ show y ++ ": " ++ show (cut (x, y))))
        [ (x, y) | x <- [1..10], y <- [1..x] ]
