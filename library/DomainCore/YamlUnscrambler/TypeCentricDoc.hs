module DomainCore.YamlUnscrambler.TypeCentricDoc
where

import DomainCore.Prelude
import DomainCore.Models.TypeCentricDoc
import YamlUnscrambler
import qualified DomainCore.Attoparsec.TypeString as TypeStringAttoparsec
import qualified DomainCore.Attoparsec.General as GeneralAttoparsec
import qualified Control.Foldl as Fold
import qualified Data.Text as Text


doc =
  value onScalar (Just onMapping) Nothing
  where
    onScalar =
      [nullScalar []]
    onMapping =
      foldMapping (,) Fold.list typeNameString structure
      where
        typeNameString =
          formattedString "type name" $ \ input ->
            case Text.uncons input of
              Just (h, t) ->
                if isUpper h
                  then
                    if Text.all (\ a -> isAlphaNum a || a == '\'' || a == '_') t
                      then
                        Right input
                      else
                        Left "Contains invalid chars"
                  else
                    Left "First char is not upper-case"
              Nothing ->
                Left "Empty string"

structure =
  value [] (Just onMapping) Nothing
  where
    onMapping =
      byKeyMapping (CaseSensitive True) $
        atByKey "product" (ProductStructure <$> byFieldName appTypeString) <|>
        atByKey "sum" (SumStructure <$> byFieldName sumTypeExpression) <|>
        atByKey "enum" (EnumStructure <$> enumVariants) <|>
        atByKey "wrapper" (WrapperStructure <$> appTypeString) <|>
        atByKey "alias" (AliasStructure <$> appTypeString)

byFieldName onElement =
  value onScalar (Just onMapping) Nothing
  where
    onScalar =
      [nullScalar []]
    onMapping =
      foldMapping (,) Fold.list textString onElement

appTypeString =
  value [
    stringScalar $ attoparsedString "Type signature" $
    GeneralAttoparsec.only TypeStringAttoparsec.appSeq
    ] Nothing Nothing

sumTypeExpression =
  value onScalar Nothing (Just onSequence)
  where
    onScalar =
      [
        nullScalar (SequenceSumTypeExpression [])
        ,
        fmap StringSumTypeExpression $
        stringScalar $ attoparsedString "Type signature" $
        GeneralAttoparsec.only TypeStringAttoparsec.commaSeq
        ]
    onSequence =
      SequenceSumTypeExpression <$> foldSequence Fold.list appTypeString

enumVariants =
  sequenceValue (foldSequence Fold.list variant)
  where
    variant =
      scalarsValue [stringScalar textString]
