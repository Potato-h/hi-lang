{-# LANGUAGE TypeApplications #-}

module HW5.Pretty
  ( prettyValue
  ) where

import           Prettyprinter                 (Doc, cat, comma, lbracket,
                                                parens, pretty, punctuate,
                                                rbracket, space, (<+>))
import           Prettyprinter.Render.Terminal (AnsiStyle)

import qualified Data.ByteString               as BS
import           Data.Foldable                 (toList)
import qualified Data.Map                      as M (toList)
import           Data.Ratio                    (denominator, numerator)
import           Data.Scientific               (fromRationalRepetendUnlimited,
                                                isFloating)
import qualified Data.Text                     as T
import           Data.Word                     (Word8)
import           HW5.Base                      (HiAction (..), HiValue (..))
import           Text.Printf                   (printf)

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue HiValueNull             = pretty "null"
prettyValue (HiValueBool x)         = pretty $ if x then "true" else "false"
prettyValue (HiValueNumber x)       = case fromRationalRepetendUnlimited x of
  (sci, Nothing) | isFloating sci -> pretty $ show sci
  _  -> let
    (num, denum) = (numerator x, denominator x)
    (i, r) = quotRem (abs num) denum
    in prettyFractional (num < 0) i r denum
prettyValue (HiValueString x)       = pretty '\"' <> pretty x <> pretty '\"'
prettyValue (HiValueFunction f)     = pretty $ show f
prettyValue (HiValueList xs)        = lbracket <+> punctuateByComma (prettyValue <$> xs) <+> rbracket
prettyValue (HiValueBytes bytes)    = pretty "[#" <+> punctuateBy space (prettyByte <$> BS.unpack bytes) <+> pretty "#]"
prettyValue (HiValueAction action)  = prettyAction action
prettyValue (HiValueTime time)      = prettyFun "parse-time" [prettyString (show time)]
prettyValue (HiValueDict dict)      = pretty "{" <+> punctuateByComma (prettyEntry <$> M.toList dict) <+> pretty "}"

prettyEntry :: (HiValue, HiValue) -> Doc AnsiStyle
prettyEntry (a, b) = prettyValue a <> pretty ": " <> prettyValue b

prettyByte :: Word8 -> Doc AnsiStyle
prettyByte w = pretty @String $ printf "%02x" w

prettyFractional :: Bool -> Integer -> Integer -> Integer -> Doc AnsiStyle
prettyFractional minus i r denum
  | r == 0    = pretty $ toNeg i
  | i == 0    = pretty (printf "%d/%d" (toNeg r) denum :: String)
  | minus     = pretty (printf "%d - %d/%d" (toNeg i) r denum :: String)
  | otherwise = pretty (printf "%d + %d/%d" i r denum :: String)
  where
    toNeg x = if minus then -x else x

prettyString :: String -> Doc AnsiStyle
prettyString str = prettyValue (HiValueString $ T.pack str)

prettyAction :: HiAction -> Doc AnsiStyle
prettyAction (HiActionRead s)     = prettyFun "read" [prettyString s]
prettyAction (HiActionWrite s bs) = prettyFun "write" [prettyString s, prettyValue (HiValueBytes bs)]
prettyAction (HiActionMkDir s)    = prettyFun "mkdir" [prettyString s]
prettyAction (HiActionChDir s)    = prettyFun "cd" [prettyString s]
prettyAction HiActionCwd          = pretty "cwd"
prettyAction HiActionNow          = pretty "now"
prettyAction (HiActionRand i j)   = prettyFun "rand" [pretty i, pretty j]
prettyAction (HiActionEcho txt)   = prettyFun "echo" [prettyString (T.unpack txt)]

punctuateBy :: Foldable t => Doc a -> t (Doc a) -> Doc a
punctuateBy sep = cat . punctuate sep . toList

punctuateByComma :: Foldable t => t (Doc a) -> Doc a
punctuateByComma = punctuateBy (comma <> space)

prettyFun :: String -> [Doc AnsiStyle] -> Doc AnsiStyle
prettyFun fn args = pretty fn <> parens (punctuateByComma args)
