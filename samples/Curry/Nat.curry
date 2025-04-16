------------------------------------------------------------------------------
--- Library defining natural numbers in Peano representation and
--- some operations on this representation.
---
--- @author Michael Hanus
--- @version January 2019
------------------------------------------------------------------------------

module Data.Nat
  ( Nat(..), fromNat, toNat, add, sub, mul, leq
  ) where

import Test.Prop

--- Natural numbers defined in Peano representation.
--- Thus, each natural number is constructor by a `Z` (zero)
--- or `S` (successor) constructor.
data Nat = Z | S Nat
 deriving (Eq,Show)

--- Transforms a natural number into a standard integer.
fromNat :: Nat -> Int
fromNat Z     = 0
fromNat (S n) = 1 + fromNat n

-- Postcondition: the result of `fromNat` is not negative
fromNat'post :: Nat -> Int -> Bool
fromNat'post _ n = n >= 0

--- Transforms a standard integer into a natural number.
toNat :: Int -> Nat
toNat n | n == 0 = Z
        | n > 0  = S (toNat (n-1))

-- Precondition: `toNat` must be called with non-negative numbers
toNat'pre :: Int -> Bool
toNat'pre n = n >= 0

-- Property: transforming natural numbers into integers and back is the identity
fromToNat :: Nat -> Prop
fromToNat n = toNat (fromNat n) -=- n

toFromNat :: Int -> Prop
toFromNat n = n>=0 ==> fromNat (toNat n) -=- n

--- Addition on natural numbers.
add :: Nat -> Nat -> Nat
add Z     n = n
add (S m) n = S(add m n)

-- Property: addition is commutative
addIsCommutative :: Nat -> Nat -> Prop
addIsCommutative x y = add x y -=- add y x

-- Property: addition is associative
addIsAssociative :: Nat -> Nat -> Nat -> Prop
addIsAssociative x y z = add (add x y) z -=- add x (add y z)

--- Subtraction defined by reversing addition.
sub :: Nat -> Nat -> Nat
sub x y | add y z == x  = z where z free

-- Properties: subtracting a value which was added yields the same value
subAddL :: Nat -> Nat -> Prop
subAddL x y = sub (add x y) x -=- y

subAddR :: Nat -> Nat -> Prop
subAddR x y = sub (add x y) y -=- x

--- Multiplication on natural numbers.
mul :: Nat -> Nat -> Nat
mul Z     _ = Z
mul (S m) n = add n (mul m n)

-- Property: multiplication is commutative
mulIsCommutative :: Nat -> Nat -> Prop
mulIsCommutative x y = mul x y -=- mul y x

-- Property: multiplication is associative
mulIsAssociative :: Nat -> Nat -> Nat -> Prop
mulIsAssociative x y z = mul (mul x y) z -=- mul x (mul y z)

-- Properties: multiplication is distributive over addition
distMulAddL :: Nat -> Nat -> Nat -> Prop
distMulAddL x y z = mul x (add y z) -=- add (mul x y) (mul x z)

distMulAddR :: Nat -> Nat -> Nat -> Prop
distMulAddR x y z = mul (add y z) x -=- add (mul y x) (mul z x)

-- less-or-equal predicated on natural numbers:
leq :: Nat -> Nat -> Bool
leq Z     _     = True
leq (S _) Z     = False
leq (S x) (S y) = leq x y

-- Property: adding a number yields always a greater-or-equal number
leqAdd :: Nat -> Nat -> Prop
leqAdd x y = always $ leq x (add x y)

------------------------------------------------------------------------------
