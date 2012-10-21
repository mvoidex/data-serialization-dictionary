{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}

-- | Module implements serialization for key-value (with custom key and value) dictionary
--
-- Usage:
--
-- @
-- data Some = Some Int Double [String]
--
-- $(makeIso \"some\" ''Some)
--
-- someDict :: Dictionarable String String Some
-- someDict =
--   entry "int" showable .**.
--   entry "double" showable .**.
--   entry "strings" showable
--   .:.
--   some
--
-- test :: encode someDict (Some 1 1.2 \"Hello\")
-- @
--
module Data.Serialization.Dictionary (
    FromDictionary, ToDictionary,
    DictionaryValue(..),
    Dictionarable,
    fromEntry, fromEntry_,
    toEntry, toEntry_,
    entry, entry_
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map as M
import qualified Data.Text as T

import Data.Serialization.Serialize
import Data.Serialization.Deserialize
import Data.Serialization.Serializable

-- | Deserialize from dictionary
newtype FromDictionary k v a = FromDictionary {
    runFromDictionary :: StateT (M.Map k v) (Either String) a }
        deriving (Functor, Applicative, Alternative, Monad)

instance Deserialization (FromDictionary k v) (M.Map k v) where
    runDeserialization (FromDictionary f) x = evalStateT f x
    deserializationEof _ = FromDictionary $ do
        obj <- get
        when (not $ M.null obj) $ lift $ Left "Not EOF"
    deserializeTail = FromDictionary $ do
        obj <- get
        put M.empty
        return obj

-- | Serialize to dictionary
newtype ToDictionary k v a = ToDictionary {
    runToDictionary :: WriterT [(k, v)] (Either String) a }
        deriving (Functor, Applicative, Alternative, Monad)

instance Ord k => Serialization (ToDictionary k v) (M.Map k v) where
    runSerialization (ToDictionary t) = M.fromList <$> execWriterT t
    serializeTail v = ToDictionary $ tell $ M.toList v

-- | Serializable to value
class DictionaryValue v a where
    dictionaryValue :: Convertible a v

-- | Type of dictionarable
type Dictionarable k v a = Serializable (M.Map k v) (ToDictionary k v) (FromDictionary k v) a

-- | Deserialize entry
fromEntry :: Ord k => k -> (v -> Either String a) -> Deserialize (FromDictionary k v) a
fromEntry name d = Deserialize $ FromDictionary $ do
    obj <- get
    put (M.delete name obj)
    case M.lookup name obj of
        Nothing -> lift $ Left "Key not found"
        Just x -> lift $ d x

-- | Deserialize entry as is
fromEntry_ :: (Ord k, DictionaryValue v a) => k -> Deserialize (FromDictionary k v) a
fromEntry_ name = Deserialize $ FromDictionary $ do
    obj <- get
    put (M.delete name obj)
    case M.lookup name obj of
        Nothing -> lift $ Left "Key not found"
        Just x -> lift $ convertFrom dictionaryValue x

-- | Serialize entry
toEntry :: k -> (a -> Either String v) -> Serialize (ToDictionary k v) a
toEntry name s = Serialize $ \v -> ToDictionary $ do
    x <- lift $ s v
    tell [(name, x)]

-- | Serialize entry as is
toEntry_ :: (DictionaryValue v a) => k -> Serialize (ToDictionary k v) a
toEntry_ name = Serialize $ \v -> ToDictionary $ do
    x <- lift $ convertTo dictionaryValue v
    tell [(name, x)]

-- | Entry serializer
entry :: Ord k => k -> Convertible a v -> Dictionarable k v a
entry name s = serializable (toEntry name $ convertTo s) (fromEntry name $ convertFrom s)

-- | Entry serializer by convertible
entry_ :: (Ord k, DictionaryValue v a) => k -> Dictionarable k v a
entry_ name = entry name dictionaryValue
