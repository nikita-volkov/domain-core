module DomainCore.Text
where

import DomainCore.Prelude
import Data.Text
import qualified Data.Char as Char


recordField (underscore, prefixWithTypeName) a b =
  bool mempty "_" underscore <>
  bool b (lcFirst a <> ucFirst b) prefixWithTypeName

sumConstructor a b =
  ucFirst b <> a

mapFirstChar fn =
  foldMap (\ (a, b) -> cons (fn a) b) .
  uncons

ucFirst =
  mapFirstChar Char.toUpper

lcFirst =
  mapFirstChar Char.toLower
