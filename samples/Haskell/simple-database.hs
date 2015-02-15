import Control.Monad.State
import Data.List (sortBy, nub)
import System.Environment (getArgs, getProgName)
import System.Directory (doesFileExist)
import System.IO (openFile, hGetContents, hClose, IOMode(..),
    Handle, hPutStrLn)

-- for storing dates
data Date = Date Integer Int Int deriving (Show, Read, Eq, Ord)

-- for storing database items
data Item = Item    {description :: String
                    ,category    :: [String]
                    ,date        :: Date
                    ,optional    :: [String]}
                    deriving (Show, Read)

-- a state monad transformer which wraps IO actions.
-- the database (state) is passed implicitly between functions.
type ItemList a = StateT [Item] IO a

-- add an item to the database
addItem :: Item -> ItemList ()
addItem i = modify (++ [i])

-- get the newest of a list of items
latest :: [Item] -> [Item]
latest [] = []
latest [x]= [x]
latest xs = take 1 $ sortBy newer xs

-- compare two items to see which one is newer
newer :: Item -> Item -> Ordering
newer a b = compare (date b) (date a)

-- list all different categories (no duplicates)
categories :: ItemList [String]
categories = liftM (nub . concatMap category) get

-- list only the items with the given category tag
filterByCategory :: String -> ItemList [Item]
filterByCategory c = liftM (filter (\i -> c `elem` category i)) get

-- get the newest of all items
lastOfAll :: ItemList [Item]
lastOfAll = liftM latest get

-- get the newest item in each category
latestByCategory :: ItemList [Item]
latestByCategory = do
    cats <- categories
    filt <- mapM filterByCategory cats
    return $ concatMap latest filt

-- sort all items chronologically, newest first
sortByDate :: ItemList [Item]
sortByDate = liftM (sortBy newer) get

toScreen :: Item -> IO ()
toScreen (Item desc cats (Date y m d) opt) = putStrLn $
    "Description:\t" ++ desc ++ "\nCategories:\t" ++ show cats ++
    "\nDate:\t\t" ++ show y ++ "-" ++ show m ++ "-" ++ show d ++
    "\nOther info:\t" ++ show opt

-- command line argument handling
-- if the user called the program with the option "add", the
-- new item is returned to main so that it can be saved to disk.
-- the argument "opt" is a list.
arguments :: ItemList [Item]
arguments = do
    args <- liftIO getArgs
    case args of
        ("add":desc:cat:year:month:day:opt) -> do
            let newItem = parseItem args
            addItem newItem
            return [newItem]
        ("latest":[]) -> do
            item <- lastOfAll
            lift $ mapM_ toScreen item
            return []
        ("category":[]) -> do
            items <- latestByCategory
            lift $ mapM_ toScreen items
            return []
        ("all":[]) -> do
            sorted <- sortByDate
            lift $ mapM_ toScreen sorted
            return []
        _ -> do
            lift usage
            return []

parseItem :: [String] -> Item
parseItem (_:desc:cat:year:month:day:opt) =
    Item {description = desc, category = words cat,
        date = Date (read year) (read month) (read day),
        optional = opt}

usage :: IO ()
usage = do
    progName <- getProgName
    putStrLn $ "Usage: " ++ progName ++ " add|all|category|latest \
        \OPTIONS\n\nadd \"description\" \"category1 category2\"... \
        \year month day [\"note1\" \"note2\"...]\n\tAdds a new record \
        \to the database.\n\nall\n\tPrints all items in chronological \
        \order.\n\ncategory\n\tPrints the latest item for each category.\
        \\n\nlatest\n\tPrints the latest item."

-- the program creates, reads and writes to a file in the current directory
main :: IO ()
main = do
    progName <- getProgName
    let fileName = progName ++ ".db"
    e <- doesFileExist fileName
    if e
        then do
            hr <- openFile fileName ReadMode
            f <- hGetContents hr
            v <- evalStateT arguments (map read $ lines f)
            hClose hr -- must be called after working with contents!
            hw <- openFile fileName AppendMode
            mapM_ (hPutStrLn hw . show) v
            hClose hw
        else do
            v <- evalStateT arguments []
            hw <- openFile fileName WriteMode
            mapM_ (hPutStrLn hw . show) v
            hClose hw
