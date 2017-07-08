module Derive.HetSpec (main, spec) where

import Test.Hspec
import Simple

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "heq" $ do
    it "works for SimpleGadt2" $ do
      heqSimpleGadt2 SimpleGadt2T1 SimpleGadt2T1 `shouldBe` True
      heqSimpleGadt2 SimpleGadt2T2 SimpleGadt2T2 `shouldBe` True
      heqSimpleGadt2 SimpleGadt2T1 SimpleGadt2T2 `shouldBe` False
      heqSimpleGadt2 SimpleGadt2T2 SimpleGadt2T1 `shouldBe` False
  describe "hlte" $ do
    it "works for SimpleGadt2" $ do
      hlteSimpleGadt2 SimpleGadt2T1 SimpleGadt2T1 `shouldBe` True
      hlteSimpleGadt2 SimpleGadt2T2 SimpleGadt2T2 `shouldBe` True
      hlteSimpleGadt2 SimpleGadt2T1 SimpleGadt2T2 `shouldBe` True
      hlteSimpleGadt2 SimpleGadt2T2 SimpleGadt2T1 `shouldBe` False
  describe "hcmp" $ do
    it "works for SimpleGadt2" $ do
      hcmpSimpleGadt2 SimpleGadt2T1 SimpleGadt2T1 `shouldBe` EQ
      hcmpSimpleGadt2 SimpleGadt2T2 SimpleGadt2T2 `shouldBe` EQ
      hcmpSimpleGadt2 SimpleGadt2T1 SimpleGadt2T2 `shouldBe` LT
      hcmpSimpleGadt2 SimpleGadt2T2 SimpleGadt2T1 `shouldBe` GT


