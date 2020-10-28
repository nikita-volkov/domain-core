module DomainCore.Parser
where

import DomainCore.Prelude
import DomainCore.Model
import qualified DomainCore.YamlUnscrambler.TypeCentricDoc as TypeCentricYaml
import qualified DomainCore.Resolvers.TypeCentricDoc as TypeCentricResolver
import qualified YamlUnscrambler
import qualified Data.Text.Encoding as Text


byteString =
  YamlUnscrambler.parseByteString TypeCentricYaml.doc
    >=> TypeCentricResolver.eliminateDoc

text =
  byteString . Text.encodeUtf8
