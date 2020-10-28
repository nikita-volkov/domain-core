module DomainCore.Models.TypeCentricDoc
where

import DomainCore.Prelude hiding (Product, Sum, Enum)
import qualified DomainCore.Models.TypeString as TypeString


type Doc =
  [(Text, Structure)]

data Structure =
  ProductStructure [(Text, TypeString.AppSeq)] |
  SumStructure [(Text, SumTypeExpression)] |
  EnumStructure [Text] |
  WrapperStructure TypeString.AppSeq |
  AliasStructure TypeString.AppSeq
  deriving (Show)

data SumTypeExpression =
  SequenceSumTypeExpression [TypeString.AppSeq] |
  StringSumTypeExpression TypeString.CommaSeq
  deriving (Show)
