{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module HW5.Utils
  ( okOr
  , rightToMaybe
  , HList (..)
  , Apply (..)
  , Fun
  ) where

import           Data.Kind (Type)

okOr :: Maybe a -> e -> Either e a
okOr (Just x) _ = Right x
okOr Nothing e  = Left e

rightToMaybe :: Either e a -> Maybe a
rightToMaybe (Right x) = Just x
rightToMaybe (Left _)  = Nothing

infixr :>

data HList (ts :: [Type]) where
    Nil :: HList '[]
    (:>) :: a -> HList ts -> HList (a ': ts)

type family Fun (ts :: [Type]) (r :: Type) where
  Fun '[] r       = r
  Fun (t ': ts) r = t -> Fun ts r

class Apply (ts :: [Type]) where
  apply :: Fun ts r -> HList ts -> r

instance Apply '[] where
  apply r Nil = r

instance Apply ts => Apply (t ': ts) where
  apply f (x :> xs) = apply (f x) xs
