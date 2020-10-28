module DomainCore.Util.List
where

import DomainCore.Prelude
import Data.List


unsnoc :: [a] -> Maybe ([a], a)
unsnoc =
  fmap (swap . fmap reverse) . uncons . reverse
