{-# LANGUAGE CPP, DataKinds, FlexibleContexts, QuasiQuotes,
             ScopedTypeVariables, TemplateHaskell, TypeOperators #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}
module CoRecSpec (spec) where
import Control.Monad ((>=>))
import Data.Proxy
import Data.Vinyl
import Data.Vinyl.CoRec
import Data.Vinyl.Functor (Identity(..))

import Data.Vinyl.TH
import Test.Hspec
import Test.ShouldNotTypecheck

-- Custom error types
data TooBig = TooBig
data Even = Even
data Not7 = Not7

-- Functions that might return an error value
fun1 :: (TooBig ∈ rs) => Int -> Either (CoRec Identity rs) ()
fun1 x = if x < 10 then Right () else Left (Col (pure TooBig))

fun2 :: (Even ∈ rs) => Int -> Either (CoRec Identity rs) ()
fun2 x = if odd x then Right () else Left (Col (pure Even))

fun3 :: (Not7 ∈ rs) => Int -> Either (CoRec Identity rs) ()
fun3 x = if x == 7 then Right () else Left (Col (pure Not7))

-- Helper for sequencing values that can error
test1and2 :: (TooBig ∈ rs, Even ∈ rs) => Int -> Either (CoRec Identity rs) ()
test1and2 x = fun1 x >> fun2 x

-- Giving a name to a sequence. Crucially, these types include a
-- polymorphic row type. To eliminate the entire row, it must be made
-- concrete at some point. Hence the use of the QuasiQuoter below.
test1 :: (TooBig ∈ rs, Even ∈ rs) => Either (CoRec Identity rs) ()
test1 = test1and2 12

test2 :: (TooBig ∈ rs, Even ∈ rs) => Either (CoRec Identity rs) ()
test2 = test1and2 7

-- This empty declaration splice finishes the previous "declaration
-- group" in GHC parlance. This lets us reify the Names 'test1' and
-- 'test2' in TH below.
return []

spec :: SpecWith ()
spec = do
  describe "CoRecs" $ do
    let x = Col (pure True) :: Field '[Int,Bool,()]
    it "Can be cast successfully" $
      asA (Proxy :: Proxy Bool) x `shouldBe` Just True
    it "Can fail to cast" $
      asA (Proxy :: Proxy Int) x `shouldBe` Nothing
    it "Can be handled all at once" $
      match x (H (\y -> "Int")
               :& H (\y -> "Bool")
               :& H (\y -> "Unit")
               :& RNil) `shouldBe` "Bool"
    it "Can be handled piece by piece, out of order" $
      let handlers = match1 (H (\(u :: ()) -> "unit"))
                     >=> match1 (H (\(b :: Bool) -> "bool "++show b))
                     >=> match1 (H (\(i :: Int) ->  "int "++show i))
      in either id matchNil (handlers x) `shouldBe` "bool True"
    it "Can detect partial pattern matches" $
      let handlers = match1 (H (\(u :: ()) -> "unit"))
                     >=> match1 (H (\(b :: Bool) -> "bool "++show b))
      in shouldNotTypecheck (either id matchNil (handlers x))
    describe "Can have ambiguous row types fixed with TH" $ do
      it "Can still pattern match" $
        let handlers = match1 (H (\TooBig -> "too big"))
                       >=> match1 (H (\Even -> "too even"))
        in either (either id matchNil . handlers)
                  (const "cool")
                  [deal|test1|]  `shouldBe` "too big"
      it "Supports total pattern matching" $
        let handlers = match1 (H (\TooBig -> "too big"))
                       >=> match1 (H (\Even -> "too even"))
        in either (either id matchNil . handlers)
                  (const "cool")
                  [deal|test2|]  `shouldBe` "cool"
#if __GLASGOW_HASKELL__ >= 800
      -- This test fails on GHC-7.10.3 despite the fact that compiler
      -- complains. Perhaps a problem with the test-running apparatus?
      it "Fixes types to be as small as possible" $
        let handlers = match1 (H (\TooBig -> "too big"))
                       >=> match1 (H (\Even -> "too even"))
                       >=> match1 (H (\Not7 -> "pshah"))
        in shouldNotTypecheck (either (either id matchNil . handlers)
                                      (const "cool")
                                      [deal|test2|])
#endif
