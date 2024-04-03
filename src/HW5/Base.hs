{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}


module HW5.Base
  ( HiError (..)
  , HiExpr (..)
  , HiFun (..)
  , HiAction (..)
  , HiValue (..)
  , HiMonad (..)
  , Extract (..)
  , Extractm (..)
  , Result
  , HiNull
  , hiInt, hiList
  ) where

import           Codec.Serialise      (Serialise)
import           Control.Arrow        ((>>>))
import           Control.Monad.Except (ExceptT, MonadTrans (lift))
import           Data.ByteString      (ByteString)
import           Data.Foldable        (toList)
import           Data.Function        ((&))
import           Data.Kind            (Type)
import           Data.Map             (Map)
import           Data.Scientific      (floatingOrInteger)
import           Data.Sequence        (Seq, fromList)
import           Data.Text            (Text)
import           Data.Time            (UTCTime)
import           Data.Word            (Word8)
import           GHC.Generics         (Generic)
import           HW5.Utils            (HList (..), okOr, rightToMaybe)

data HiFun
  = HiFunDiv
  | HiFunMul
  | HiFunAdd
  | HiFunSub
  | HiFunNot
  | HiFunAnd
  | HiFunOr
  | HiFunLessThan
  | HiFunGreaterThan
  | HiFunEquals
  | HiFunNotLessThan
  | HiFunNotGreaterThan
  | HiFunNotEquals
  | HiFunIf
  | HiFunLength
  | HiFunToUpper
  | HiFunToLower
  | HiFunReverse
  | HiFunTrim
  | HiFunList
  | HiFunRange
  | HiFunFold
  | HiFunPackBytes
  | HiFunUnpackBytes
  | HiFunEncodeUtf8
  | HiFunDecodeUtf8
  | HiFunZip
  | HiFunUnzip
  | HiFunSerialise
  | HiFunDeserialise
  | HiFunRead
  | HiFunWrite
  | HiFunMkDir
  | HiFunChDir
  | HiFunParseTime
  | HiFunRand
  | HiFunEcho
  | HiFunCount
  | HiFunKeys
  | HiFunValues
  | HiFunInvert
  deriving (Eq, Ord, Enum, Bounded, Generic)

instance Serialise HiFun

instance Show HiFun where
  show HiFunDiv            = "div"
  show HiFunMul            = "mul"
  show HiFunAdd            = "add"
  show HiFunSub            = "sub"
  show HiFunNot            = "not"
  show HiFunAnd            = "and"
  show HiFunOr             = "or"
  show HiFunLessThan       = "less-than"
  show HiFunGreaterThan    = "greater-than"
  show HiFunEquals         = "equals"
  show HiFunNotLessThan    = "not-less-than"
  show HiFunNotGreaterThan = "not-greater-than"
  show HiFunNotEquals      = "not-equals"
  show HiFunIf             = "if"
  show HiFunLength         = "length"
  show HiFunToUpper        = "to-upper"
  show HiFunToLower        = "to-lower"
  show HiFunReverse        = "reverse"
  show HiFunTrim           = "trim"
  show HiFunList           = "list"
  show HiFunRange          = "range"
  show HiFunFold           = "fold"
  show HiFunPackBytes      = "pack-bytes"
  show HiFunUnpackBytes    = "unpack-bytes"
  show HiFunEncodeUtf8     = "encode-utf8"
  show HiFunDecodeUtf8     = "decode-utf8"
  show HiFunZip            = "zip"
  show HiFunUnzip          = "unzip"
  show HiFunSerialise      = "serialise"
  show HiFunDeserialise    = "deserialise"
  show HiFunRead           = "read"
  show HiFunWrite          = "write"
  show HiFunMkDir          = "mkdir"
  show HiFunChDir          = "cd"
  show HiFunParseTime      = "parse-time"
  show HiFunRand           = "rand"
  show HiFunEcho           = "echo"
  show HiFunCount          = "count"
  show HiFunKeys           = "keys"
  show HiFunValues         = "values"
  show HiFunInvert         = "invert"

data HiAction
  = HiActionRead  FilePath
  | HiActionWrite FilePath ByteString
  | HiActionMkDir FilePath
  | HiActionChDir FilePath
  | HiActionCwd
  | HiActionNow
  | HiActionRand Int Int
  | HiActionEcho Text
  deriving (Eq, Ord, Show, Generic)

instance Serialise HiAction

data HiValue
  = HiValueBool Bool
  | HiValueNumber Rational
  | HiValueFunction HiFun
  | HiValueNull
  | HiValueString Text
  | HiValueList (Seq HiValue)
  | HiValueBytes ByteString
  | HiValueAction HiAction
  | HiValueTime UTCTime
  | HiValueDict (Map HiValue HiValue)
  deriving (Eq, Ord, Show, Generic)

instance Serialise HiValue

data HiNull = HiNull

hiList :: Foldable t => t HiValue -> HiValue
hiList = toList >>> fromList >>> HiValueList

hiInt :: Real a => a -> HiValue
hiInt = toRational >>> HiValueNumber

data HiExpr
  = HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr]
  | HiExprDict [(HiExpr, HiExpr)]
  | HiExprRun HiExpr
  deriving Show

type Result a = Either HiError a

data HiError
  = HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero
  deriving Show

class Extract a where
  extract :: HiValue -> Maybe a

instance Extract Bool where
  extract (HiValueBool x) = Just x
  extract _               = Nothing

instance Extract Rational where
  extract (HiValueNumber x) = Just x
  extract _                 = Nothing

instance Extract Int where
  extract (HiValueNumber x) =
    fromRational x &
    floatingOrInteger @Double &
    rightToMaybe
  extract _                 = Nothing

instance Extract Text where
  extract (HiValueString x) = Just x
  extract _                 = Nothing

instance Extract HiFun where
  extract (HiValueFunction x) = Just x
  extract _                   = Nothing

instance Extract HiNull where
  extract HiValueNull = Just HiNull
  extract _           = Nothing

instance Extract (Seq HiValue) where
  extract (HiValueList x) = Just x
  extract _               = Nothing

instance Extract ByteString where
  extract (HiValueBytes x) = Just x
  extract _                = Nothing

instance Extract [Word8] where
  extract values = do
    sq <- extract @(Seq HiValue) values
    ints <- mapM (extract @Int) sq
    bytes <- mapM (\x -> if 0 <= x && x < 256 then Just $ fromIntegral x else Nothing) ints
    return $ toList bytes

instance Extract HiAction where
  extract (HiValueAction action) = Just action
  extract _                      = Nothing

instance Extract UTCTime where
  extract (HiValueTime time) = Just time
  extract _                  = Nothing

instance Extract (Map HiValue HiValue) where
  extract (HiValueDict dict) = Just dict
  extract _                  = Nothing

instance Extract HiValue where
  extract = Just

class Extractm (ts :: [Type]) where
  extractm :: [HiValue] -> Result (HList ts)

instance Extractm '[] where
  extractm [] = Right Nil
  extractm _  = Left HiErrorArityMismatch

instance (Extract a, Extractm ts) => Extractm (a ': ts) where
  extractm [] = Left HiErrorArityMismatch
  extractm (v:vs) = do
      xs <- extractm vs
      x <- extract v `okOr` HiErrorInvalidArgument
      return $ x :> xs

class Monad m => HiMonad m where
  runAction :: HiAction -> m HiValue

instance HiMonad m => HiMonad (ExceptT e m) where
  runAction action = lift $ runAction action
