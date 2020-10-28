module DomainCore.Attoparsec.TypeString
where

import DomainCore.Prelude hiding (takeWhile)
import DomainCore.Models.TypeString
import Data.Attoparsec.Text hiding (sepBy1)
import DomainCore.Attoparsec.General
import Control.Applicative.Combinators.NonEmpty


commaSeq =
  commaSeparated appSeq

appSeq =
  sepBy1 unit skipSpace1

unit =
  asum [
    InSquareBracketsUnit <$> inSquareBrackets appSeq
    ,
    InParensUnit <$> inParens commaSeq
    ,
    RefUnit <$> typeRef
    ]

typeRef =
  sepBy1 ucName (char '.')
