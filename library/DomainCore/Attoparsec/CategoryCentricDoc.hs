module DomainCore.Attoparsec.CategoryCentricDoc
where

import DomainCore.Prelude hiding (takeWhile)
import DomainCore.Models.CategoryCentricDoc
import Data.Attoparsec.Text
import DomainCore.Attoparsec.General


typeRefOnly =
  only typeRef

typeRef =
  fmap TypeRef $ sepBy1 ucName (char '.')

typeOnly =
  only type_ <|> only typeListType

type_ =
  do
    a <- nonAppType
    cont a
  where
    cont a =
      asum [
        do
          skipSpace1
          b <- nonAppType
          cont (AppType a b)
        ,
        pure a
        ]

nonAppType =
  inSquareBracketsType <|> inParensType <|> refType

refType =
  RefType <$> typeRef

inParensType =
  inParens typeListType

inSquareBracketsType =
  InSquareBracketsType <$> inSquareBrackets type_

typeListType =
  InParensType <$> commaSeparated type_
