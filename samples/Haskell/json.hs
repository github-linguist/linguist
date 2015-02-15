{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Data.Attoparsec (parseOnly)
import Data.Text
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Char8 as S

testdoc = object [
    "foo"   .= (1 :: Int),
    "bar"   .= ([1.3, 1.6, 1.9] :: [Double]),
    "baz"   .= ("some string" :: Text),
    "other" .= object [
        "yes" .= ("sir" :: Text)
        ]
    ]

main = do
    let out = encode testdoc
    B.putStrLn out
    case parseOnly json (S.concat $ B.toChunks out) of
        Left e -> error $ "strange error re-parsing json: " ++ (show e)
        Right v | v /= testdoc -> error "documents not equal!"
        Right v | otherwise    -> print v
