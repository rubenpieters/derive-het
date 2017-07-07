{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module Simple where

import Derive.Het

-- creating data type
data SimpleGadt2 a where
  SimpleGadt2T1 :: SimpleGadt2 Int
  SimpleGadt2T2 :: SimpleGadt2 Bool

-- derive functions
genHetEq ''SimpleGadt2
genHetLte ''SimpleGadt2

-- sample usage
sample1 = heqSimpleGadt2 SimpleGadt2T1 SimpleGadt2T1
sample2 = hlteSimpleGadt2 SimpleGadt2T1 SimpleGadt2T1
