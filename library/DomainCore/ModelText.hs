module DomainCore.ModelText
where

import DomainCore.Prelude
import DomainCore.Model
import qualified Data.Text as Text
import qualified Data.Char as Char


recordField (underscore, prefixWithTypeName) a b =
  bool mempty "_" underscore <>
  bool b (lcFirst a <> ucFirst b) prefixWithTypeName

sumConstructor a b =
  ucFirst b <> a

mapFirstChar fn =
  foldMap (\ (a, b) -> Text.cons (fn a) b) .
  Text.uncons

ucFirst =
  mapFirstChar Char.toUpper

lcFirst =
  mapFirstChar Char.toLower
