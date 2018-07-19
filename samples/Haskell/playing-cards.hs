import System.Random

data Pip = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten |
           Jack | Queen | King | Ace
  deriving (Ord, Enum, Bounded, Eq, Show)

data Suit = Diamonds | Spades | Hearts | Clubs
  deriving (Ord, Enum, Bounded, Eq, Show)

type Card = (Pip, Suit)

fullRange :: (Bounded a, Enum a) => [a]
fullRange = [minBound..maxBound]

fullDeck :: [Card]
fullDeck = [(pip, suit) | pip <- fullRange, suit <- fullRange]

insertAt :: Int -> a -> [a] -> [a]
insertAt 0 x ys     = x:ys
insertAt n _ []     = error "insertAt: list too short"
insertAt n x (y:ys) = y : insertAt (n-1) x ys

shuffle :: RandomGen g => g -> [a] -> [a]
shuffle g xs = shuffle' g xs 0 [] where
  shuffle' g []     _ ys = ys
  shuffle' g (x:xs) n ys = shuffle' g' xs (n+1) (insertAt k x ys) where
    (k,g') = randomR (0,n) g
