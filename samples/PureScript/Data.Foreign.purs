module Data.Foreign
  ( Foreign(..)
  , ForeignParser(ForeignParser)
  , parseForeign
  , parseJSON
  , ReadForeign
  , read
  , prop
  ) where

import Prelude
import Data.Array
import Data.Either
import Data.Maybe
import Data.Tuple
import Data.Traversable

foreign import data Foreign :: *

foreign import fromString
  "function fromString (str) { \
  \  try { \
  \    return _ps.Data_Either.Right(JSON.parse(str)); \
  \  } catch (e) { \
  \    return _ps.Data_Either.Left(e.toString()); \
  \  } \
  \}" :: String -> Either String Foreign

foreign import readPrimType
  "function readPrimType (typeName) { \
  \  return function (value) { \
  \    if (toString.call(value) == '[object ' + typeName + ']') { \
  \      return _ps.Data_Either.Right(value);\
  \    } \
  \    return _ps.Data_Either.Left('Value is not a ' + typeName + ''); \
  \  }; \
  \}" :: forall a. String -> Foreign -> Either String a

foreign import readMaybeImpl
  "function readMaybeImpl (value) { \
  \  return value === undefined || value === null ? _ps.Data_Maybe.Nothing : _ps.Data_Maybe.Just(value); \
  \}" :: forall a. Foreign -> Maybe Foreign

foreign import readPropImpl
  "function readPropImpl (k) { \
  \  return function (obj) { \
  \    return _ps.Data_Either.Right(obj[k]);\
  \  }; \
  \}" :: forall a. String -> Foreign -> Either String Foreign

foreign import showForeignImpl
  "var showForeignImpl = JSON.stringify;" :: Foreign -> String

instance showForeign :: Prelude.Show Foreign where
  show = showForeignImpl

data ForeignParser a = ForeignParser (Foreign -> Either String a)

parseForeign :: forall a. ForeignParser a -> Foreign -> Either String a
parseForeign (ForeignParser p) x = p x

parseJSON :: forall a. (ReadForeign a) => String -> Either String a
parseJSON json = fromString json >>= parseForeign read

instance monadForeignParser :: Prelude.Monad ForeignParser where
  return x = ForeignParser \_ -> Right x
  (>>=) (ForeignParser p) f = ForeignParser \x -> case p x of
      Left err -> Left err
      Right x' -> parseForeign (f x') x

instance applicativeForeignParser :: Prelude.Applicative ForeignParser where
  pure x = ForeignParser \_ -> Right x
  (<*>) (ForeignParser f) (ForeignParser p) = ForeignParser \x -> case f x of
      Left err -> Left err
      Right f' -> f' <$> p x

instance functorForeignParser :: Prelude.Functor ForeignParser where
  (<$>) f (ForeignParser p) = ForeignParser \x -> f <$> p x

class ReadForeign a where
  read :: ForeignParser a

instance readString :: ReadForeign String where
  read = ForeignParser $ readPrimType "String"

instance readNumber :: ReadForeign Number where
  read = ForeignParser $ readPrimType "Number"

instance readBoolean :: ReadForeign Boolean where
  read = ForeignParser $ readPrimType "Boolean"

instance readArray :: (ReadForeign a) => ReadForeign [a] where
  read =
    let arrayItem (Tuple i x) = case parseForeign read x of
      Right result -> Right result
      Left err -> Left $ "Error reading item at index " ++ (show i) ++ ":\n" ++ err
    in
    (ForeignParser $ readPrimType "Array") >>= \xs -> 
      ForeignParser \_ -> arrayItem `traverse` (zip (range 0 (length xs)) xs)

instance readMaybe :: (ReadForeign a) => ReadForeign (Maybe a) where
  read = (ForeignParser $ Right <<< readMaybeImpl) >>= \x -> 
    ForeignParser \_ -> case x of
      Just x' -> parseForeign read x' >>= return <<< Just
      Nothing -> return Nothing

prop :: forall a. (ReadForeign a) => String -> ForeignParser a
prop p = (ForeignParser \x -> readPropImpl p x) >>= \x -> 
  ForeignParser \_ -> case parseForeign read x of
    Right result -> Right result
    Left err -> Left $ "Error reading property '" ++ p ++ "':\n" ++ err
