{-# LANGUAGE DataKinds, FlexibleContexts, QuasiQuotes,
             ScopedTypeVariables, TemplateHaskell, TypeOperators #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}
import Control.Monad ((>=>))
import Data.Proxy
import Data.Vinyl
import Data.Vinyl.CoRec
import Data.Vinyl.Functor (Identity(..))

import Data.Vinyl.TH
import Test.Hspec
import Test.ShouldNotTypecheck

data TooBig = TooBig
data Even = Even
data Not7 = Not7

fun1 :: TooBig ∈ rs => Int -> Either (CoRec Identity rs) ()
fun1 x = if x < 10 then Right () else Left (Col (pure TooBig))

fun2 :: Even ∈ rs => Int -> Either (CoRec Identity rs) ()
fun2 x = if odd x then Right () else Left (Col (pure Even))

fun3 :: Not7 ∈ rs => Int -> Either (CoRec Identity rs) ()
fun3 x = if x == 7 then Right () else Left (Col (pure Not7))

test1and2 :: (TooBig ∈ rs, Even ∈ rs) => Int -> Either (CoRec Identity rs) ()
test1and2 x = fun1 x >> fun2 x

test1 :: (TooBig ∈ rs, Even ∈ rs) => Either (CoRec Identity rs) ()
test1 = test1and2 12

test2 :: (TooBig ∈ rs, Even ∈ rs) => Either (CoRec Identity rs) ()
test2 = test1and2 7

return []

main :: IO ()
main = hspec $ do
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
      it "Fixes types as small as possible" $
        let handlers = match1 (H (\TooBig -> "too big"))
                       >=> match1 (H (\Even -> "too even"))
                       >=> match1 (H (\Not7 -> "pshah"))
        in shouldNotTypecheck (either (either id matchNil . handlers)
                                      (const "cool")
                                      [deal|test2|])
