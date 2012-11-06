{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, DeriveGeneric, FlexibleInstances, OverlappingInstances #-}

-- | Module implements serialization for key-value (with custom key and value) dictionary
--
-- Declare some data and derive from Generic:
--
-- >data Some = Some {
-- >    someInt :: Int,
-- >    someString :: String }
-- >        deriving (Generic, Show)
-- 
-- Then create instances to automatically convert fields to dictionary value type (String in this example)
--
-- >instance (Read a, Show a) => DictionaryValue String a where
-- >    dictionaryValue = Convertible (Right . show) (Right . read)
-- 
-- Derive from Serializable
--
-- -- There are synonim Dictionarable k v for this type, but we can't use it here
-- >instance Serializable (Codec (M.Map String String) (ToDictionary String String) (FromDictionary String String)) Some
-- 
-- And use generated serializer with field names as in record
--
-- >some1 :: Dictionarable String String Some
-- >some1 = ser
--
-- Or declare your own serializer, changing any part of generated serializer
--
-- >some2 :: Dictionarable String String Some
-- >some2 =
-- >    dat_ (
-- >        ctor_ (
-- >            stor "Renamed first field" ser -- Rename field
-- >            .*.
-- >            gser)) -- Use generic serializer for this field
-- >    .:.
-- >    giso
--
-- Using:
--
-- >encode some1 (Some 123 "hello") == Right (fromList [("someInt","123"),("someString","\"hello\"")])
-- >encode some2 (Some 321 "world") == Right (fromList [("Renamed first field","321"),("someString","\"world\"")])
--
module Data.Serialization.Dictionary (
    FromDictionary, ToDictionary,
    DictionaryValue(..),
    Dictionarable,
    fromEntry, fromEntry_,
    toEntry, toEntry_,
    entry, entry_
    ) where

import Control.Arrow
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Error
import qualified Data.Map as M
import qualified Data.Text as T
import Data.String (IsString, fromString)
import GHC.Generics

import Data.Serialization.Combine
import Data.Serialization.Wrap
import Data.Serialization.Generic
import Data.Serialization.Codec

-- | Deserialize from dictionary
newtype FromDictionary k v a = FromDictionary { fromDict :: DecodeFrom (M.Map k v) a }
    deriving (Functor, Applicative, Alternative, Monad, MonadFail, MonadState (M.Map k v), MonadError String, Generic)

instance (Monoid k, IsString k, Ord k, Eq v) => GenericDecode (FromDictionary k v) where
    decodeStor name m = do
        v <- state (M.lookup name' &&& M.delete name')
        maybe (throwError "Key not found") (either throwError return . deserialize m . M.singleton (nullKey m)) v
        where
            name' = fromString name
            nullKey :: (Monoid k) => FromDictionary k v a -> k
            nullKey _ = mempty
instance (Monoid k, IsString k, Ord k, Eq v) => Deserializer (FromDictionary k v) (M.Map k v)

-- | Serialize to dictionary
newtype ToDictionary k v a = ToDictionary { toDict :: EncodeTo [(k, v)] a }
    deriving (Functor, Applicative, Alternative, Monad, MonadFail, MonadWriter [(k, v)], MonadError String, Generic)

instance (Monoid k, IsString k, Ord k, Eq v) => GenericEncode (ToDictionary k v) where
    encodeStor name m x = do
        v <- either throwError return $ serialize (m x)
        tell $ map (first (\k -> if k == mempty then name' else k)) $ M.toList v
        where
            name' = fromString name
instance (Monoid k, IsString k, Ord k, Eq v) => Serializer (ToDictionary k v) (M.Map k v) where
    serialize (ToDictionary v) = fmap M.fromList $ execWriterT v
    serializeTail v = ToDictionary $ tell $ M.toList v

-- | Serializable to value
class DictionaryValue v a where
    dictionaryValue :: Convertible a v

-- | Type of dictionarable
type Dictionarable k v a = Codec (M.Map k v) (ToDictionary k v) (FromDictionary k v) a

instance (Monoid k, Ord k, Eq v, DictionaryValue v a) => Serializable (Codec (M.Map k v) (ToDictionary k v) (FromDictionary k v)) a where
    ser = entry_ mempty

-- | Deserialize entry
fromEntry :: (Ord k, Eq v) => k -> (v -> Either String a) -> Decoding (FromDictionary k v) a
fromEntry name d = decodePart f where
    f m = do
        v <- maybe (Left "Key not found") Right $ M.lookup name m
        x <- d v
        return (x, M.delete name m)

-- | Deserialize entry as is
fromEntry_ :: (Ord k, Eq v, DictionaryValue v a) => k -> Decoding (FromDictionary k v) a
fromEntry_ name = decodePart f where
    f m = do
        v <- maybe (Left "Key not found") Right $ M.lookup name m
        x <- convertFrom dictionaryValue v
        return (x, M.delete name m)

-- | Serialize entry
toEntry :: k -> (a -> Either String v) -> Encoding (ToDictionary k v) a
toEntry name s = encodePart $ \v -> do
    x <- s v
    return [(name, x)]

-- | Serialize entry as is
toEntry_ :: (DictionaryValue v a) => k -> Encoding (ToDictionary k v) a
toEntry_ name = encodePart $ \v -> do
    x <- convertTo dictionaryValue v
    return [(name, x)]

-- | Entry serializer
entry :: (Ord k, Eq v) => k -> Convertible a v -> Dictionarable k v a
entry name s = codec (toEntry name $ convertTo s) (fromEntry name $ convertFrom s)

-- | Entry serializer by convertible
entry_ :: (Ord k, Eq v, DictionaryValue v a) => k -> Dictionarable k v a
entry_ name = entry name dictionaryValue
