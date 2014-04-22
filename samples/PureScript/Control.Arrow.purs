module Control.Arrow where

import Data.Tuple

class Arrow a where
  arr :: forall b c. (b -> c) -> a b c
  first :: forall b c d. a b c -> a (Tuple b d) (Tuple c d)

instance arrowFunction :: Arrow (->) where
  arr f = f
  first f (Tuple b d) = Tuple (f b) d

second :: forall a b c d. (Category a, Arrow a) => a b c -> a (Tuple d b) (Tuple d c)
second f = arr swap >>> first f >>> arr swap

swap :: forall a b. Tuple a b -> Tuple b a
swap (Tuple x y) = Tuple y x

infixr 3 ***
infixr 3 &&&

(***) :: forall a b b' c c'. (Category a, Arrow a) => a b c -> a b' c' -> a (Tuple b b') (Tuple c c')
(***) f g = first f >>> second g

(&&&) :: forall a b b' c c'. (Category a, Arrow a) => a b c -> a b c' -> a b (Tuple c c')
(&&&) f g = arr (\b -> Tuple b b) >>> (f *** g)

class ArrowZero a where
  zeroArrow :: forall b c. a b c

infixr 5 <+>

class ArrowPlus a where
  (<+>) :: forall b c. a b c -> a b c -> a b c
