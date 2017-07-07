{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Derive.Het where

import Language.Haskell.TH

{-
motivating example:

data TestType a where
  TestTerm1 :: TestType Int
  TestTerm2 :: TestType String

heq :: TestType a -> TestType b -> Bool
heq TestTerm1 TestTerm1 = True
heq TestTerm2 TestTerm2 = True
heq _ _ = False

hlte :: TestType a -> TestType b -> Bool
hlte a b | heq a b = True
hlte TestTerm1 TestTerm2 = True
hlte _ _ = False
-}

gadtFields :: Name -> Q [Name]
gadtFields gadt = do
  TyConI (DataD _ _ _ _ fields _) <- reify gadt
  -- TODO: why exactly does GadtC have multiple names?, not sure if it should return all those
  let names = concatMap (\(GadtC ns _ _) -> ns) fields
  return names

genHetEq :: Name -> DecsQ
genHetEq gadt = genHetEq' funcName gadt
  where funcName = mkName $ "heq" ++ nameBase gadt

genHetEq' :: Name -> Name -> DecsQ
genHetEq' funcName gadt = do
  signature <- [t| forall a b. $(return $ ConT gadt) a -> $(return $ ConT gadt) b -> Bool |]
  names <- gadtFields gadt
  let equalityClauses = heqCl <$> names
  return [ SigD funcName signature
         , FunD funcName $
                equalityClauses ++
                [Clause [WildP, WildP] (NormalB (ConE 'False)) []]
         ]

heqCl :: Name -> Clause
heqCl term = hlteCl term term

genHetLte :: Name -> DecsQ
genHetLte gadt = genHetLte' funcName gadt
  where funcName = mkName $ "hlte" ++ nameBase gadt

genHetLte' :: Name -> Name -> DecsQ
genHetLte' funcName gadt = do
  let heqName = mkName $ "heq" ++ nameBase gadt
  signature <- [t| forall a b. $(return $ ConT gadt) a -> $(return $ ConT gadt) b -> Bool |]
  equalAB <- [| $(return $ VarE heqName) a b |]
  names <- gadtFields gadt
  let nameOrderings = orderings names
  return [ SigD funcName signature
         , FunD funcName $
                Clause [VarP (mkName "a"), VarP (mkName "b")] (GuardedB [(NormalG equalAB, ConE 'True)]) [] :
                (uncurry hlteCl <$> nameOrderings) ++
                [Clause [WildP, WildP] (NormalB (ConE 'False)) []]
         ]

hlteCl :: Name -> Name -> Clause
hlteCl term1 term2 =
       -- term1 term2 = True
       Clause [ConP term1 [], ConP term2 []] (NormalB (ConE 'True)) []

orderings :: [a] -> [(a,a)]
orderings [] = []
orderings (x:xs) = ((x,) <$> xs) ++ orderings xs

-- print reification
-- $(stringE . show =<< reify ''TestType)

main :: IO ()
main = do
  putStrLn "hello world"
