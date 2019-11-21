{-# language OverloadedStrings #-}
{-# language LambdaCase #-}
{-# language StandaloneDeriving #-}

-- {-# options_ghc -fno-warn-orphans #-}

module Ldap.Parsers 
  ( decodeFilter
  , encodeFilter
  , filterParser
  )
  where

import Control.Applicative
import Control.Monad (void)
import Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Encoding as T
import qualified Ldap.Client as L

parens :: Parser a -> Parser a
parens p = string "(" *> p <* string ")"

optionalParens :: Parser a -> Parser a
optionalParens p = (optional $ string "(") *> p <* (optional $ string ")")

filterNot :: Parser L.Filter -> Parser L.Filter
filterNot p = string "!" *> parens (L.Not <$> p)

filterAnd :: Parser L.Filter -> Parser L.Filter
filterAnd p = string "&" *> (L.And <$> (NE.fromList <$> (p `sepBy1` optional space)))

filterOr :: Parser L.Filter -> Parser L.Filter
filterOr p = string "|" *> (L.Or <$> (NE.fromList <$> (p `sepBy1` optional space)))

filterPresent :: Parser L.Filter
filterPresent = L.Present <$> (attr <* string "=*" <* (endOfInput <|> void (string ")")))

attr :: Parser L.Attr
attr = L.Attr <$> (T.decodeUtf8 <$> takeTill (inClass "=~<>:()"))

attrValue :: Parser L.AttrValue
attrValue = optional (string "(") *> takeTill (inClass ")")

filterEQ :: Parser L.Filter
filterEQ = (L.:=) <$> attr <*> (string "=" *> noGlobAttrValue)
  where
  noGlobAttrValue = do
    a <- attrValue
    if '*' `BC.elem` a 
    then fail "glob in EQ"
    else pure a

filterGE :: Parser L.Filter
filterGE = (L.:>=) <$> attr <*> (string ">=" *> attrValue)

filterLE :: Parser L.Filter
filterLE = (L.:<=) <$> attr <*> (string "<=" *> attrValue)

filterApproxEQ :: Parser L.Filter
filterApproxEQ = (L.:~=) <$> attr <*> (string "~=" *> attrValue)

filterGlob :: Parser L.Filter
filterGlob = do
  attr' <- attr <* string "="
  g1 <- glob <|> (optional globAttrValue)
  v <- globAttrValue `sepBy1'` string "*"
  g2 <- glob <|> (optional globAttrValue)
  let (v',g2') = case v of -- FIXME stupid hack
        ["", ""] -> ([], Just "*")
        [""] -> ([], g2)
        x -> (x, g2)
  pure (attr' L.:=* (g1,v',g2'))
  where
  glob = string "*" *> pure Nothing
  globAttrValue = takeTill $ inClass ")*"

filterExtensible :: Parser L.Filter
filterExtensible = do
  mattrType <- optional attr
  mdnFlag <- optional $ string ":dn"
  mruleId <- optional $ string ":" *> attr
  assertionVal <- string ":=" *> attrValue
  let dnFlag = case mdnFlag of
        Nothing -> False
        Just _ -> True
  pure $ (mattrType, mruleId, dnFlag) L.::= assertionVal

filterParser :: Parser L.Filter
filterParser = optionalParens 
    $ (filterPresent          <?> "filterPresent")
  <|> (filterEQ               <?> "filterEQ")
  <|> (filterNot filterParser <?> "filterNot")
  <|> (filterOr filterParser  <?> "filterOr")
  <|> (filterAnd filterParser <?> "filterAnd")
  <|> (filterGE               <?> "filterGE")
  <|> (filterLE               <?> "filterLE")
  <|> (filterApproxEQ         <?> "filterApproxEQ")
  <|> (filterExtensible       <?> "filterExtensible")
  <|> (filterGlob             <?> "filterGlob")

decodeFilter :: ByteString -> Either String L.Filter
decodeFilter = parseOnly filterParser

encodeFilter :: L.Filter -> ByteString
encodeFilter fi' = paren' (go fi')
  where
  paren' x = "(" <> x <> ")"
  parens' x = foldMap paren' x
  encGlob Nothing = "*"
  encGlob (Just x) = x
  unAttr (L.Attr x) = T.encodeUtf8 x
  go fi = case fi of
    L.Not x -> "!(" <> go x <> ")"
    L.And x -> "&(" <> parens' (go <$> x) <> ")"
    L.Or x -> "|(" <> parens' (go <$> x) <> ")"
    L.Present x -> unAttr x <> "=*"
    a L.:= b ->  unAttr a <> "=" <> b
    a L.:>= b -> unAttr a <> ">=" <> b
    a L.:<= b -> unAttr a <> "<=" <> b
    a L.:~= b -> unAttr a <> "~=" <> b
    a L.:=* (p1, v, p2) -> unAttr a <> "=" <> encGlob p1 <> B.intercalate "*" v <> encGlob p2
    (a,b,dnFlag) L.::= assertionVal -> 
         maybe "" unAttr a 
      <> if dnFlag then ":dn" else ""
      <> maybe "" ((<>) ":" . unAttr) b 
      <> ":=(" <> assertionVal <> ")"
