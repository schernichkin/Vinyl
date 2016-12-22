{-# LANGUAGE DataKinds, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}
import Control.Monad ((>=>))
import Data.Proxy
import Data.Vinyl
import Data.Vinyl.CoRec
import Test.Hspec
import Test.ShouldNotTypecheck

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
