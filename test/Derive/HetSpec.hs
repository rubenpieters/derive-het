module Derive.HetSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

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
  describe "hcmp" $ do
    it "works for some examples with ParamGadt2" $ do
      hcmpParamGadt2 (ParamGadt2T1 2 2) (ParamGadt2T1 2 2) `shouldBe` EQ
      hcmpParamGadt2 (ParamGadt2T1 1 2) (ParamGadt2T1 2 1) `shouldBe` LT
      hcmpParamGadt2 (ParamGadt2T1 2 1) (ParamGadt2T1 1 2) `shouldBe` GT
      hcmpParamGadt2 (ParamGadt2T1 2 1) (ParamGadt2T1 2 2) `shouldBe` LT
      hcmpParamGadt2 (ParamGadt2T1 2 2) (ParamGadt2T1 2 1) `shouldBe` GT
    it "gives EQ when given equal parameters" $ property $ do
      \a b -> hcmpParamGadt2 (ParamGadt2T1 a b) (ParamGadt2T1 a b) `shouldBe` EQ
    it "gives LT when first parameter is higher" $ property $ do
      \a b -> hcmpParamGadt2 (ParamGadt2T1 1 a) (ParamGadt2T1 2 b) `shouldBe` LT
    it "gives GT when second parameter is higher" $ property $ do
      \a b -> hcmpParamGadt2 (ParamGadt2T1 2 a) (ParamGadt2T1 1 b) `shouldBe` GT
    it "returns result of last parameter comparsion when other parameters are equal" $ property $ do
      \a b -> hcmpParamGadt2 (ParamGadt2T1 2 a) (ParamGadt2T1 2 b) `shouldBe` compare a b
    it "gives GT when comparing second to first constructor" $ property $ do
      \b1 b2 i1 i2 -> hcmpParamGadt2 (ParamGadt2T2 b1 b2) (ParamGadt2T1 i1 i2) `shouldBe` GT
    it "gives LT when comparing first to second constructor" $ property $ do
      \b1 b2 i1 i2 -> hcmpParamGadt2 (ParamGadt2T1 i1 i2) (ParamGadt2T2 b1 b2) `shouldBe` LT

