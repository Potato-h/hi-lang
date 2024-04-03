{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -Wno-deprecations #-} -- GZ.bestCompression is deprecated

module HW5.Evaluator
  ( eval
  ) where

import qualified Codec.Compression.Zlib as GZ (CompressParams (compressLevel),
                                               CompressionLevel (BestCompression),
                                               compressWith, decompress,
                                               defaultCompressParams)
import           Codec.Serialise        (deserialiseOrFail, serialise)
import           Control.Arrow          ((>>>))
import           Control.Monad.Except
import qualified Data.ByteString        as BS (ByteString, drop, index, length,
                                               pack, take, unpack)
import           Data.ByteString.Lazy   (fromStrict, toStrict)
import           Data.Foldable          (Foldable (foldl'), toList)
import           Data.Function          ((&))
import           Data.Functor           ((<&>))
import qualified Data.Map               as M (Map, empty, fromList, insertWith,
                                              lookup, map, toAscList, toList)
import           Data.Maybe             (fromMaybe)
import           Data.Semigroup         (stimes)
import           Data.Sequence          (Seq (Empty, (:<|)))
import qualified Data.Sequence          as S (Seq, drop, length, lookup,
                                              reverse, singleton, take)
import qualified Data.Text              as T (Text, append, chunksOf, drop,
                                              length, reverse, strip, take,
                                              toLower, toUpper, unpack)
import qualified Data.Text.Encoding     as T
import qualified Data.Time              as TM
import           Data.Word              (Word8)
import           HW5.Base               (Extract, Extractm, HiAction (..),
                                         HiError (..), HiExpr (..), HiFun (..),
                                         HiMonad (runAction), HiNull,
                                         HiValue (..), Result, extract,
                                         extractm, hiInt, hiList)
import           HW5.Utils              (Apply (..), HList (..), okOr,
                                         rightToMaybe)
import           Text.Read              (readMaybe)

type MonadHiError = MonadError HiError

eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval expr = let ExceptT res = evalExpr expr in res

-- explicit forall to able to name a
evalTo1 :: forall a m. (MonadHiError m, HiMonad m, Extract a) => HiExpr -> m a
evalTo1 expr = do
  value <- evalExpr expr
  liftEither $ extract value `okOr` HiErrorInvalidArgument

-- explicit forall to able to name a
evalTo :: forall ts m. (MonadHiError m, HiMonad m, Extractm ts) => [HiExpr] -> m (HList ts)
evalTo args = do
  values <- forM args evalExpr
  liftEither $ extractm @ts values

evalComp :: (MonadHiError m, HiMonad m) => [HiExpr] -> (HiValue -> HiValue -> Bool) -> m HiValue
evalComp args op = evalTo @'[HiValue, HiValue] args <&> apply \a b -> HiValueBool $ a `op` b

evalText :: (MonadHiError m, HiMonad m) => [HiExpr] -> (T.Text -> HiValue) -> m HiValue
evalText args op = evalTo @'[T.Text] args <&> (\(a :> Nil) -> op a)

evalBytes :: (MonadHiError m, HiMonad m) => [HiExpr] -> (BS.ByteString -> HiValue) -> m HiValue
evalBytes args op = evalTo @'[BS.ByteString] args <&> (\(a :> Nil) -> op a)

idxStr :: T.Text -> Int -> HiValue
idxStr str i
  | 0 <= i && i < T.length str = subStr str (Just i) (Just $ i + 1)
  | otherwise = HiValueNull

subStr :: T.Text -> Maybe Int -> Maybe Int -> HiValue
subStr str (Just from) (Just to) = str
    & T.take (idx to)
    & T.drop (idx from)
    & HiValueString
  where
    idx i = if i < 0 then i + T.length str else i
subStr str Nothing Nothing = HiValueString str
subStr str Nothing to = subStr str (Just 0) to
subStr str from Nothing = subStr str from (Just $ T.length str)

idxSeq :: Seq HiValue -> Int -> HiValue
idxSeq xs i = Data.Maybe.fromMaybe HiValueNull (S.lookup i xs)

subSeq :: Seq HiValue -> Maybe Int -> Maybe Int -> HiValue
subSeq sq (Just from) (Just to) = sq
    & S.take (idx to)
    & S.drop (idx from)
    & HiValueList
  where
    idx i = if i < 0 then i + S.length sq else i
subSeq sq Nothing Nothing = HiValueList sq
subSeq sq Nothing to = subSeq sq (Just 0) to
subSeq sq from Nothing = subSeq sq from (Just $ S.length sq)

idxBytes :: BS.ByteString -> Int -> HiValue
idxBytes bytes i
  | 0 <= i && i < BS.length bytes = hiInt $ bytes `BS.index` i
  | otherwise = HiValueNull

subBytes :: BS.ByteString -> Maybe Int -> Maybe Int -> HiValue
subBytes bytes (Just from) (Just to) = bytes
    & BS.take (idx to)
    & BS.drop (idx from)
    & HiValueBytes
  where
    idx i = if i < 0 then i + BS.length bytes else i
subBytes bytes Nothing Nothing = HiValueBytes bytes
subBytes bytes Nothing to   = subBytes bytes (Just 0) to
subBytes bytes from Nothing = subBytes bytes from (Just $ BS.length bytes)

expectLen :: MonadHiError m => [a] -> [Int] -> m ()
expectLen args lens = when (length args `notElem` lens) $ throwError HiErrorArityMismatch

evalFun :: (MonadHiError m, HiMonad m) => HiFun -> [HiExpr] -> m HiValue
evalFun HiFunDiv args            = do
  args `expectLen` [2]
  values <- forM args evalExpr
  handlers values
    [ handler @'[Rational, Rational] $ apply \a b -> do
        when (b == 0) $ throwError HiErrorDivideByZero
        return $ HiValueNumber $ a / b
    , handler @'[T.Text, T.Text] $ apply \a b -> return $ HiValueString $ mconcat [a, "/", b]
    ]
evalFun HiFunMul args            = do
  args `expectLen` [2]
  values <- forM args evalExpr
  handlers values
    [ handler @'[Rational, Rational] $ apply \a b -> return $ HiValueNumber $ a * b
    , handler @'[T.Text, Int] $ apply \a b -> return $ HiValueString $ stimes b a
    , handler @'[S.Seq HiValue, Int] $ apply \a b -> return $ HiValueList $ stimes b a
    , handler @'[BS.ByteString, Int] $ apply \a b -> return $ HiValueBytes $ stimes b a
    ]
evalFun HiFunAdd args            = do
  args `expectLen` [2]
  values <- forM args evalExpr
  handlers values
    [ handler @'[Rational, Rational] $ apply \a b -> return $ HiValueNumber $ a + b
    , handler @'[T.Text, T.Text] $ apply \a b -> return $ HiValueString $ T.append a b
    , handler @'[S.Seq HiValue, S.Seq HiValue] $ apply \a b -> return $ HiValueList $ a <> b
    , handler @'[BS.ByteString, BS.ByteString] $ apply \a b -> return $ HiValueBytes $ a <> b
    , handler @'[TM.UTCTime, Rational] $ apply \a b -> return $ HiValueTime $ TM.addUTCTime (fromRational b) a
    ]
evalFun HiFunSub args            = do
  args `expectLen` [2]
  values <- forM args evalExpr
  handlers values
    [ handler @'[Rational, Rational] $ apply \a b -> return $ HiValueNumber $ a - b
    , handler @'[TM.UTCTime, TM.UTCTime] $ apply \a b -> return $ HiValueNumber . toRational $ TM.diffUTCTime a b
    ]
evalFun HiFunNot args            = evalTo @'[Bool] args <&> apply (not >>> HiValueBool)
evalFun HiFunAnd args            = case args of
  [lhs, rhs] -> do
    lhs' <- evalExpr lhs
    case lhs' of
      HiValueBool False -> return lhs'
      HiValueNull       -> return lhs'
      _                 -> evalExpr rhs
  _          -> throwError HiErrorArityMismatch
evalFun HiFunOr args             = case args of
  [lhs, rhs] -> do
    lhs' <- evalExpr lhs
    case lhs' of
      HiValueBool False -> evalExpr rhs
      HiValueNull       -> evalExpr rhs
      _                 -> return lhs'
  _ -> throwError HiErrorArityMismatch
evalFun HiFunLessThan args       = evalComp args (<)
evalFun HiFunGreaterThan args    = evalComp args (>)
evalFun HiFunEquals args         = evalComp args (==)
evalFun HiFunNotLessThan args    = evalComp args (>=)
evalFun HiFunNotGreaterThan args = evalComp args (<=)
evalFun HiFunNotEquals args      = evalComp args (/=)
evalFun HiFunIf args             = case args of
  [condition, lhs, rhs] -> do
    cond <- evalTo1 @Bool condition
    if cond
      then evalExpr lhs
      else evalExpr rhs
  _                     -> throwError HiErrorArityMismatch
evalFun HiFunLength args         = do
  args `expectLen` [1]
  values <- forM args evalExpr
  handlers values
    [ handler @'[T.Text] $ apply (T.length >>> hiInt >>> return)
    , handler @'[S.Seq HiValue] $ apply (S.length >>> hiInt >>> return)
    ]
evalFun HiFunToUpper args        = evalText args (T.toUpper >>> HiValueString)
evalFun HiFunToLower args        = evalText args (T.toLower >>> HiValueString)
evalFun HiFunReverse args        = do
  args `expectLen` [1]
  values <- forM args evalExpr
  handlers values
    [ handler @'[T.Text] $ apply (T.reverse >>> HiValueString >>> return)
    , handler @'[S.Seq HiValue] $ apply (S.reverse >>> HiValueList >>> return)
    ]
evalFun HiFunTrim args           = evalText args (T.strip >>> HiValueString)
evalFun HiFunList args           = forM args evalExpr <&> hiList
evalFun HiFunRange args          = evalTo @'[Rational, Rational] args <&> apply \from to ->
  iterate (+ 1) from
    & takeWhile (<= to)
    & map HiValueNumber
    & hiList
evalFun HiFunFold args           = do
  args `expectLen` [2]
  evalTo @'[HiFun, S.Seq HiValue] args >>= apply \f xs ->
    case xs of
      Empty    -> return HiValueNull
      x :<| xs' -> foldM (\acc y -> evalFun f [HiExprValue acc, HiExprValue y]) x xs'
evalFun HiFunPackBytes args      = evalTo @'[[Word8]] args <&> apply (BS.pack >>> HiValueBytes)
evalFun HiFunUnpackBytes args    = evalBytes args (BS.unpack >>> map hiInt >>> hiList)
evalFun HiFunEncodeUtf8 args     = evalText args (T.encodeUtf8 >>> HiValueBytes)
evalFun HiFunDecodeUtf8 args     = evalTo @'[BS.ByteString] args <&> apply \bytes ->
  case T.decodeUtf8' bytes of
    Left _    -> HiValueNull
    Right txt -> HiValueString txt
evalFun HiFunZip args            = evalBytes args $
  fromStrict
  >>> GZ.compressWith GZ.defaultCompressParams { GZ.compressLevel = GZ.BestCompression }
  >>> toStrict
  >>> HiValueBytes
evalFun HiFunUnzip args          = evalBytes args $ fromStrict >>> GZ.decompress >>> toStrict >>> HiValueBytes
evalFun HiFunSerialise args      = evalTo @'[HiValue] args <&> apply (serialise >>> toStrict >>> HiValueBytes)
evalFun HiFunDeserialise args    = evalTo @'[BS.ByteString] args <&> apply \bytes ->
  case deserialiseOrFail $ fromStrict bytes of
    Left _      -> HiValueNull
    Right value -> value
evalFun HiFunRead args = evalText args (T.unpack >>> HiActionRead >>> HiValueAction)
evalFun HiFunWrite args = evalTo @'[T.Text, T.Text] args <&> apply \path txt ->
  HiActionWrite (T.unpack path) (T.encodeUtf8 txt)
  & HiValueAction
evalFun HiFunMkDir args = evalText args (T.unpack >>> HiActionMkDir >>> HiValueAction)
evalFun HiFunChDir args = evalText args (T.unpack >>> HiActionChDir >>> HiValueAction)
evalFun HiFunParseTime args = evalText args (T.unpack >>> readMaybe @TM.UTCTime >>> maybe HiValueNull HiValueTime)
evalFun HiFunEcho args    = evalText args (HiActionEcho >>> HiValueAction)
evalFun HiFunRand args    = evalTo @'[Int, Int] args <&> apply \i j -> HiValueAction $ HiActionRand i j
evalFun HiFunCount args   = do
  args `expectLen` [1]
  values <- forM args evalExpr
  handlers values
    [ handler @'[S.Seq HiValue] $ apply (toList >>> count >>> return)
    , handler @'[T.Text] $ apply (T.chunksOf 1 >>> map HiValueString >>> count >>> return)
    , handler @'[BS.ByteString] $ apply (BS.unpack >>> map hiInt >>> count >>> return)
    ]
evalFun HiFunKeys args    = evalTo @'[M.Map HiValue HiValue] args <&> apply (M.toAscList >>> map fst >>> hiList)
evalFun HiFunValues args  = evalTo @'[M.Map HiValue HiValue] args <&> apply (M.toAscList >>> map snd >>> hiList)
evalFun HiFunInvert args  = evalTo @'[M.Map HiValue HiValue] args <&> apply \dict ->
    foldl' (\acc (k, v) -> M.insertWith (<>) v (S.singleton k) acc) M.empty (M.toList dict)
    & M.map HiValueList
    & HiValueDict

count :: [HiValue] -> HiValue
count xs = foldl' (\mp a -> M.insertWith (\_ (y :: Int) -> y + 1) a 1 mp) M.empty xs
  & M.map hiInt
  & HiValueDict

rangeLookup
  :: (MonadHiError m, HiMonad m)
  => [HiExpr]
  -> s
  -> (s -> Int -> HiValue)
  -> (s -> Maybe Int -> Maybe Int -> HiValue)
  -> m HiValue
rangeLookup args xs idx sub = do
  args `expectLen` [1, 2]
  values <- forM args evalExpr
  handlers values
    [ handler @'[Int] $ apply (idx xs >>> return)
    , handler @'[Int, Int] $ apply \from to -> return $ sub xs (Just from) (Just to)
    , handler @'[Int, HiNull] $ apply \from _ -> return $ sub xs (Just from) Nothing
    , handler @'[HiNull, Int] $ apply \_ to -> return $ sub xs Nothing (Just to)
    , handler @'[HiNull, HiNull] \_ -> return $ sub xs Nothing Nothing
    ]

evalExpr :: (MonadHiError m, HiMonad m) => HiExpr -> m HiValue
evalExpr (HiExprValue val)       = return val
evalExpr (HiExprApply call args) = do
  call' <- evalExpr call
  case call' of
    HiValueFunction f   -> evalFun f args
    HiValueString str   -> rangeLookup args str idxStr subStr
    HiValueList xs      -> rangeLookup args xs idxSeq subSeq
    HiValueBytes bytes  -> rangeLookup args bytes idxBytes subBytes
    HiValueDict dict    -> evalTo @'[HiValue] args <&> apply \k -> fromMaybe HiValueNull (M.lookup k dict)
    _                   -> throwError HiErrorInvalidFunction
evalExpr (HiExprRun expr) = do
  action <- evalTo1 @HiAction expr
  runAction action
evalExpr (HiExprDict entries) = do
  entries' <- forM entries $ \(k, v) -> do
    k' <- evalExpr k
    v' <- evalExpr v
    return (k', v')
  return $ HiValueDict $ M.fromList entries'

handlers :: MonadHiError m => [HiValue] -> [[HiValue] -> Maybe (Result HiValue)] -> m HiValue
handlers values hs = liftEither $ case msum (map (\h -> h values) hs) of
   Just v  -> v
   Nothing -> Left HiErrorInvalidArgument

handler :: forall ts. Extractm ts => (HList ts -> Result HiValue) -> [HiValue] -> Maybe (Result HiValue)
handler op values = rightToMaybe (extractm @ts values) <&> op
