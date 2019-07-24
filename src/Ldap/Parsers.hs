{-# language OverloadedStrings #-}
{-# language LambdaCase #-}
{-# language StandaloneDeriving #-}

module Ldap.Parsers where

import Control.Applicative
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.Char (ord)
import Data.Word
import Data.Foldable (fold)
import qualified Data.Text.Encoding as T
import qualified Ldap.Client as L
import qualified Data.List.NonEmpty as NE

c2w :: Char -> Word8
c2w = fromIntegral . ord
{-# INLINE c2w #-}

parens :: Parser a -> Parser a
parens p = string "(" *> p <* string ")"

filterNot :: Parser L.Filter -> Parser L.Filter
filterNot p = string "!" *> parens (L.Not <$> p)

filterAnd :: Parser L.Filter -> Parser L.Filter
filterAnd p = string "&" *> (L.And <$> (NE.fromList <$> (p `sepBy1` (optional $ string " "))))

filterOr :: Parser L.Filter -> Parser L.Filter
filterOr p = string "|" *> (L.And <$> (NE.fromList <$> (p `sepBy1` (optional $ string " "))))

filterPresent :: Parser L.Filter
filterPresent = L.Present <$> (attr <* string "=*")

attr :: Parser L.Attr
attr = L.Attr <$> (T.decodeUtf8 <$> takeTill (\x -> x `elem` resChars))

resChars :: [Word8]
resChars = fmap c2w ['=', '~', '<', '>', ':', '(', ')']

attrValue :: Parser L.AttrValue
attrValue = takeTill (\x -> x `elem` resChars)

filterEQ :: Parser L.Filter
filterEQ = (L.:=) <$> attr <*> (string "=" *> attrValue)

filterGE :: Parser L.Filter
filterGE = (L.:>=) <$> attr <*> (string ">=" *> attrValue)

filterLE :: Parser L.Filter
filterLE = (L.:<=) <$> attr <*> (string "<=" *> attrValue)

filterApproxEQ :: Parser L.Filter
filterApproxEQ = (L.:~=) <$> attr <*> (string "~=" *> attrValue)

filterGlob :: Parser L.Filter
filterGlob = do
  attr' <- attr
  g1 <- (string "*" *> pure Nothing) <|> Just <$> attrValue
  v <- many attrValue
  g2 <- (string "*" *> pure Nothing) <|> Just <$> attrValue
  pure (attr' L.:=* (g1,v,g2))

filterParser :: Parser L.Filter
filterParser = do optional (string "(")
  *>  ( filterNot filterParser
  <|> filterOr filterParser
  <|> filterAnd filterParser
  <|> filterPresent
  <|> filterEQ
  <|> filterGE
  <|> filterLE
  <|> filterApproxEQ
  )
  -- <|> filterGlob
  <* optional (string ")")

decodeFilter :: ByteString -> Either String L.Filter
decodeFilter = parseOnly filterParser

encodeFilter :: L.Filter -> ByteString
encodeFilter = \case
  L.Not x -> "!(" <> encodeFilter x <> ")"
  L.And x -> "&(" <> parens' (encodeFilter <$> x) <> ")"
  L.Or x -> "|(" <> parens' (encodeFilter <$> x) <> ")"
  L.Present (L.Attr x) -> T.encodeUtf8 x <> "=*"
  (L.Attr a) L.:= b -> T.encodeUtf8 a <> "=" <> b
  (L.Attr a) L.:>= b -> T.encodeUtf8 a <> ">=" <> b
  (L.Attr a) L.:<= b -> T.encodeUtf8 a <> "<=" <> b
  (L.Attr a) L.:~= b -> T.encodeUtf8 a <> "~=" <> b
  (L.Attr a) L.:=* (p1, v, p2) -> T.encodeUtf8 a <> "=" <> encGlob p1 <> mconcat v <> encGlob p2
  _ L.::= _ -> "???"
  where
  paren' x = "(" <> x <> ")"
  parens' x = fold (fmap paren' x)
  encGlob Nothing = "*"
  encGlob (Just x) = x

