{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module Simple where

import Derive.Het

-- creating data type
data SimpleGadt2 a where
  SimpleGadt2T1 :: SimpleGadt2 Int
  SimpleGadt2T2 :: SimpleGadt2 Bool

-- derive functions
genHetCmp ''SimpleGadt2
genHetEq ''SimpleGadt2
genHetLte ''SimpleGadt2

-- sample usage
sample1_1 :: Bool
sample1_1 = heqSimpleGadt2 SimpleGadt2T1 SimpleGadt2T1
sample1_2 :: Bool
sample1_2 = hlteSimpleGadt2 SimpleGadt2T1 SimpleGadt2T1
sample1_3 :: Ordering
sample1_3 = hcmpSimpleGadt2 SimpleGadt2T1 SimpleGadt2T1

-- creating data type
data ParamGadt2 a where
  ParamGadt2T1 :: Int -> Int -> ParamGadt2 Int
  ParamGadt2T2 :: Bool -> Bool-> ParamGadt2 Bool

-- derive functions
genHetCmp ''ParamGadt2

-- sample usage
sample2_4 :: Ordering
sample2_4 = hcmpParamGadt2 (ParamGadt2T1 1 2) (ParamGadt2T1 2 2)
sample2_5 :: Ordering
sample2_5 = hcmpParamGadt2 (ParamGadt2T1 2 2) (ParamGadt2T1 2 3)
sample2_6 :: Ordering
sample2_6 = hcmpParamGadt2 (ParamGadt2T1 2 3) (ParamGadt2T1 2 2)
sample2_7 :: Ordering
sample2_7 = hcmpParamGadt2 (ParamGadt2T1 2 2) (ParamGadt2T1 2 2)
