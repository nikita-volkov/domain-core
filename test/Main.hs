module Main where

import Prelude hiding (assert)
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Test.QuickCheck as QuickCheck
import qualified NeatInterpolation as NeatInterpolation
import qualified DomainCore.Model as Model
import qualified DomainCore.Parser as Parser
import qualified Data.Text.Encoding as Text


main =
  defaultMain $ 
  testGroup "All tests" [
    testCase "Should fail when wrong member of sum-type is supplied" $ let
      res =
        Parser.text [NeatInterpolation.text|
          A:
            sum:
              a:
                c: Int
              b: Char, Double
          |]
      in case res of
        Right res ->
          assertFailure (show res)
        Left err ->
          return ()
    ]
