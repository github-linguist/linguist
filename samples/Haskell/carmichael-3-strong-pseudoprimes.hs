#!/usr/bin/runhaskell

import Data.Numbers.Primes
import Control.Monad (guard)

carmichaels = do
  p <- takeWhile (<= 61) primes
  h3 <- [2..(p-1)]
  let g = h3 + p
  d <- [1..(g-1)]
  guard $ (g * (p - 1)) `mod` d == 0 && (-1 * p * p) `mod` h3 == d `mod` h3
  let q = 1 + (((p - 1) * g) `div` d)
  guard $ isPrime q
  let r = 1 + ((p * q) `div` h3)
  guard $ isPrime r && (q * r) `mod` (p - 1) == 1
  return (p, q, r)

main = putStr $ unlines $ map show carmichaels
