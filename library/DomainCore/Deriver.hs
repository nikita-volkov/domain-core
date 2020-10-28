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

import DomainCore.Prelude hiding (show, ord, all, lift)
import DomainCore.Model
import qualified Language.Haskell.TH as TH (Q, Dec)
import qualified DomainCore.InstanceDecs as InstanceDecs


{-|
Abstraction which allows to define automatic derivation of any class.

It is implemented as a function from the type declaration in this package\'s own AST
to a list of Template Haskell declarations in its quotation monad.

Its Monoid instance allows you to combine derivers.
-}
newtype Deriver =
  Deriver (TypeDec -> TH.Q [TH.Dec])
  deriving (Semigroup, Monoid)
    via ((->) TypeDec (Ap TH.Q [TH.Dec]))

{-|
Lift a pure function, which doesn't require the context of 'TH.Q'.
-}
effectless f =
  Deriver (pure . f)
