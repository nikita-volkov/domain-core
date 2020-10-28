module DomainCore.Resolvers.TypeCentricDoc
where

import DomainCore.Prelude hiding (lookup)
import DomainCore.Model
import qualified DomainCore.Models.TypeCentricDoc as Doc
import qualified DomainCore.Models.TypeString as TypeString
import qualified DomainCore.Util.List as List
import qualified Data.Text as Text


eliminateDoc =
  traverse eliminateNameAndStructure

eliminateNameAndStructure (name, structure) =
  TypeDec name <$> eliminateStructure structure

eliminateStructure =
  \ case
    Doc.ProductStructure structure ->
      ProductTypeDef <$>
      traverse eliminateProductStructureUnit structure
    Doc.SumStructure structure ->
      SumTypeDef <$>
      traverse eliminateSumStructureUnit structure
    Doc.EnumStructure variants ->
      pure (EnumTypeDef variants)
    Doc.WrapperStructure appSeq ->
      WrapperTypeDef . AppType <$> eliminateTypeStringAppSeq appSeq
    Doc.AliasStructure appSeq ->
      AliasTypeDef . AppType <$> eliminateTypeStringAppSeq appSeq

eliminateProductStructureUnit (name, appSeq) =
  (,) name . AppType <$> eliminateTypeStringAppSeq appSeq

eliminateSumStructureUnit (name, sumTypeExpression) =
  (,) name <$> eliminateSumTypeExpression sumTypeExpression

eliminateSumTypeExpression =
  \ case
    Doc.SequenceSumTypeExpression a ->
      traverse (fmap AppType . eliminateTypeStringAppSeq) a
    Doc.StringSumTypeExpression a ->
      traverse (fmap AppType . eliminateTypeStringAppSeq) a

eliminateTypeStringCommaSeq =
  traverse eliminateTypeStringAppSeq

eliminateTypeStringAppSeq =
  traverse eliminateTypeStringUnit

eliminateTypeStringUnit =
  \ case
    TypeString.InSquareBracketsUnit appSeq ->
      eliminateTypeStringAppSeq appSeq &
        fmap (ListType . AppType)
    TypeString.InParensUnit commaSeq ->
      eliminateTypeStringCommaSeq commaSeq &
        fmap (TupleType . fmap AppType)
    TypeString.RefUnit typeRef ->
      eliminateTypeRef typeRef &
        fmap RefType

eliminateTypeRef =
  pure . Text.intercalate "." . toList
