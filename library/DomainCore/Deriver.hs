{-|
Toolkit for definition of instance derivers for Domain specs.
-}
module DomainCore.Deriver
(
  -- * Deriver definitions
  Deriver(..),
  effectless,
)
where

import DomainCore.Prelude
import DomainCore.Model
import qualified Language.Haskell.TH as TH (Q, Dec)


{-|
Specification of which instances to automatically derive and how.

You can combine derivers using Monoid and Semigroup.
-}
newtype Deriver =
  {-|
  Function from the type declaration in this package\'s own AST
  to a list of Template Haskell declarations in its quotation monad.
  -}
  Deriver (TypeDec -> TH.Q [TH.Dec])
  deriving (Semigroup, Monoid)
    via ((->) TypeDec (Ap TH.Q [TH.Dec]))

{-|
Lift a pure function, which doesn't require the context of 'TH.Q'.
-}
effectless f =
  Deriver (pure . f)
