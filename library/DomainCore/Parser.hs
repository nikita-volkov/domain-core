module DomainCore.Parser
where

import DomainCore.Prelude
import DomainCore.Model
import qualified DomainCore.YamlUnscrambler.TypeCentricDoc as TypeCentricYaml
import qualified DomainCore.Resolvers.TypeCentricDoc as TypeCentricResolver
import qualified YamlUnscrambler
import qualified Data.Text.Encoding as Text


{-|
Parse bytestring into a list of type declarations.
-}
byteString =
  YamlUnscrambler.parseByteString TypeCentricYaml.doc
    >=> TypeCentricResolver.eliminateDoc

{-|
Parse text into a list of type declarations.
-}
text =
  byteString . Text.encodeUtf8
