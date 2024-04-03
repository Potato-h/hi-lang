{-# LANGUAGE OverloadedStrings #-}

module HW5.Parser
  ( parse
  , errorBundlePretty
  ) where

import           Data.Void                      (Void)
import           Text.Megaparsec                (MonadParsec (eof, try), Parsec,
                                                 between, choice, empty,
                                                 errorBundlePretty, many,
                                                 manyTill, optional, runParser,
                                                 satisfy, sepBy, sepBy1, some,
                                                 (<|>))
import           Text.Megaparsec.Error          (ParseErrorBundle)

import           Control.Arrow                  ((>>>))
import           Control.Monad                  (join, mfilter, void)
import           Control.Monad.Combinators.Expr (Operator (InfixL, InfixN, InfixR, Postfix),
                                                 makeExprParser)
import qualified Data.ByteString                as BS (ByteString, pack)
import           Data.Char                      (isAlpha, isAlphaNum)
import           Data.Foldable                  (foldl')
import           Data.Functor                   (($>), (<&>))
import           Data.Maybe                     (isJust)
import           Data.Text                      (Text, pack)
import           Data.Word                      (Word8)
import           HW5.Base                       (HiAction (HiActionCwd, HiActionNow),
                                                 HiExpr (HiExprApply, HiExprDict, HiExprRun, HiExprValue),
                                                 HiFun (..), HiValue (..))
import           Text.Megaparsec.Char           (alphaNumChar, char, letterChar,
                                                 space, space1)
import qualified Text.Megaparsec.Char.Lexer     as L

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser ()
symbol s = L.symbol sc s $> ()

pIdent :: String -> Parser ()
pIdent s = lexeme $ do
  space
  try $ do
    x <- letterChar
    void $ mfilter (\xs -> (x:xs) == s) $ some (alphaNumChar <|> char '-')

pOperator :: String -> Parser ()
pOperator s = lexeme $ do
  space
  try $ void $ mfilter (== s) $ choice $ (\op -> op <$ try (symbol op)) <$>
      [ "/=", "==", "<=", ">=", "<", ">"
      , "&&", "||", "*", "/", "+", "-"
      ]

pPars :: String -> Parser ()
pPars s = lexeme $ do
  space
  try $ void $ mfilter (== s) $ choice $ (\op -> op <$ try (symbol op)) <$>
      ["[#", "#]", "[", "]", "{", "}"]

pBool :: Parser Bool
pBool = choice
  [ symbol "true" $> True
  , symbol "false" $> False
  ]

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pHexDigit :: Parser Word8
pHexDigit = choice $ zipWith (\c i -> char c $> i) (['0'..'9'] ++ ['a'..'f']) [0..15]

pByte :: Parser Word8
pByte = do
  a <- pHexDigit
  b <- pHexDigit
  return $ a * 16 + b

pNumber :: Parser Rational
pNumber = do
  minus <- optional $ symbol "-"
  pos <- lexeme L.scientific
  let val = if isJust minus then -pos else pos
  return $ toRational val

pLiteral :: Parser Text
pLiteral = lexeme $ do
  void $ char '\"'
  chars <- manyTill L.charLiteral (char '\"')
  return $ pack chars

pFun :: Parser HiFun
pFun = choice $ [pIdent (show val) $> val | val <- [minBound..]]

pList :: Parser [HiExpr]
pList = lexeme $ between (pPars "[") (pPars "]") (pExpr `sepBy` symbol ",")

pDict :: Parser [(HiExpr, HiExpr)]
pDict = lexeme $ do
  pPars "{"
  entries <- do {
    a <- pExpr;
    symbol ":";
    b <- pExpr;
    return (a, b)
  } `sepBy` symbol ","
  pPars "}"
  return entries

pBytes :: Parser BS.ByteString
pBytes = lexeme $ do
  pPars "[#"
  bytes <- many $ try $ pByte <* space1
  pPars "#]"
  return $ BS.pack bytes

pAction :: Parser HiAction
pAction = lexeme $ choice
  [ symbol "cwd" $> HiActionCwd
  , symbol "now" $> HiActionNow
  ]

pValue :: Parser HiValue
pValue = choice
  [ pAction <&> HiValueAction
  , symbol "null" $> HiValueNull
  , pBool <&> HiValueBool
  , pNumber <&> HiValueNumber
  , pFun <&> HiValueFunction
  , pLiteral <&> HiValueString
  ]

pAtom :: Parser HiExpr
pAtom = choice
    [ pValue <&> HiExprValue
    , pBytes <&> HiExprValue . HiValueBytes
    , pList <&> HiExprApply (HiExprValue (HiValueFunction HiFunList))
    , pDict <&> HiExprDict
    , parens pExpr
    ]

pExpr :: Parser HiExpr
pExpr = space *> makeExprParser pAtom operators

pAll :: Parser HiExpr
pAll = pExpr <* eof

pApply :: Parser (HiExpr -> HiExpr)
pApply = flip HiExprApply <$> parens (pExpr `sepBy` symbol ",")

pDotAccess :: Parser (HiExpr -> HiExpr)
pDotAccess = do
  void $ char '.'
  name <- join <$> ((:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)) `sepBy1` char '-'
  return $ \expr -> HiExprApply expr [HiExprValue $ HiValueString $ pack name]

pPostfix :: Operator Parser HiExpr
pPostfix = Postfix $ foldl' (>>>) id <$> some (choice [pApply, symbol "!" $> HiExprRun, pDotAccess])

pBinary :: String -> HiFun -> Parser (HiExpr -> HiExpr -> HiExpr)
pBinary name f = (\lhs rhs -> HiExprApply (HiExprValue $ HiValueFunction f) [lhs, rhs]) <$ pOperator name

binaryL :: String -> HiFun -> Operator Parser HiExpr
binaryL name f = InfixL (pBinary name f)

binaryR :: String -> HiFun -> Operator Parser HiExpr
binaryR name f = InfixR (pBinary name f)

binaryN :: String -> HiFun -> Operator Parser HiExpr
binaryN name f = InfixN (pBinary name f)

operators :: [[Operator Parser HiExpr]]
operators =
  [ [pPostfix]
  , [ binaryL "*" HiFunMul
    , binaryL "/" HiFunDiv
    ]
  , [ binaryL "+" HiFunAdd
    , binaryL "-" HiFunSub
    ]
  , [ binaryN ">=" HiFunNotLessThan
    , binaryN "<=" HiFunNotGreaterThan
    , binaryN "<" HiFunLessThan
    , binaryN ">" HiFunGreaterThan
    , binaryN "==" HiFunEquals
    , binaryN "/=" HiFunNotEquals
    ]
  , [ binaryR "&&" HiFunAnd ]
  , [ binaryR "||" HiFunOr ]
  ]

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser pAll "Parser.hs"
