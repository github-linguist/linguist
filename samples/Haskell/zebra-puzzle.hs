import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List ((\\), isInfixOf)

-- types
data House = House
    { color :: Color
    , man   :: Man
    , pet   :: Pet
    , drink :: Drink
    , smoke :: Smoke
    }
    deriving (Eq, Show)

data Color = Red | Green | Blue | Yellow | White
    deriving (Eq, Show, Enum, Bounded)

data Man = Eng | Swe | Dan | Nor | Ger
    deriving (Eq, Show, Enum, Bounded)

data Pet = Dog | Birds | Cats | Horse | Zebra
    deriving (Eq, Show, Enum, Bounded)

data Drink = Coffee | Tea | Milk | Beer | Water
    deriving (Eq, Show, Enum, Bounded)

data Smoke = PallMall | Dunhill | Blend | BlueMaster | Prince
    deriving (Eq, Show, Enum, Bounded)


main :: IO ()
main = do
  mapM_ (\x-> mapM_ print (reverse x) >> putStrLn "----") solutions
  putStrLn "No More Solutions"


solutions :: [[House]]
solutions = filter (and . postChecks) $ foldM next [] [1..5]
    where
      next xs pos = [x:xs | x <- iterHouse xs, and $ checks pos x]


iterHouse :: [House] -> [House]
iterHouse xs =
    House <$> new color <*> new man <*> new pet <*> new drink <*> new smoke
    where
      new getter = [minBound ..] \\ map getter xs


-- immediate checks
checks :: Int -> House -> [Bool]
checks pos house =
    [ man   `is` Eng    <=> color `is` Red              --  2
    , man   `is` Swe    <=> pet   `is` Dog              --  3
    , man   `is` Dan    <=> drink `is` Tea              --  4
    , color `is` Green  <=> drink `is` Coffee           --  6
    , pet   `is` Birds  <=> smoke `is` PallMall         --  7
    , color `is` Yellow <=> smoke `is` Dunhill          --  8
    , const (pos == 3)  <=> drink `is` Milk             --  9
    , const (pos == 1)  <=> man   `is` Nor              -- 10
    , drink `is` Beer   <=> smoke `is` BlueMaster       -- 13
    , man   `is` Ger    <=> smoke `is` Prince           -- 14
    ]
    where
      infix 4 <=>
      p <=> q = p house == q house  -- both True or both False


-- final checks
postChecks :: [House] -> [Bool]
postChecks houses =
    -- NOTE: list of houses is generated in reversed order
    [ [White, Green] `isInfixOf` map color houses       --  5
    , (smoke `is` Blend  ) `nextTo` (pet   `is` Cats )  -- 11
    , (smoke `is` Dunhill) `nextTo` (pet   `is` Horse)  -- 12
    , (color `is` Blue   ) `nextTo` (man   `is` Nor  )  -- 15
    , (smoke `is` Blend  ) `nextTo` (drink `is` Water)  -- 16
    ]
    where
      nextTo :: (House -> Bool) -> (House -> Bool) -> Bool
      nextTo p q
          | (_:x:_) <- dropWhile (not . match) houses = match x
          | otherwise                                 = False
          where
            match x = p x || q x


is :: Eq a => (House -> a) -> a -> House -> Bool
getter `is` value = (== value) . getter
