module AndOrNot where

open import Data.Bool
open import Data.Product

test : Bool → Bool → Bool × Bool × Bool
test x y = x ∧ y , x ∨ y , not x
