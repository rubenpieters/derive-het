{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Derive.Het
  ( genHetEq
  , genHetLte
  , genHetCmp
  )
where

import Language.Haskell.TH

gadtFieldsAndSize :: Name -> Q [([Name],Int)]
gadtFieldsAndSize gadt = do
  TyConI (DataD _ _ _ _ fields _) <- reify gadt
  -- TODO: why exactly does GadtC have multiple names?, not sure if it should return all those
  let namesAndSize = map (\(GadtC ns paramList _) -> (ns, length paramList)) fields
  return namesAndSize

genHetEq :: Name -> DecsQ
genHetEq gadt = do
  signature <- [t| forall a b. $(conT gadt) a -> $(conT gadt) b -> Bool |]
  body <- [| $(varE hcmpFuncName) x y == EQ |]
  return [ SigD funcName signature
         , FunD funcName [Clause [VarP $ mkName "x", VarP $ mkName "y"] (NormalB body) []]
         ]
    where
      funcName = mkName $ "heq" ++ nameBase gadt
      hcmpFuncName = mkName $ "hcmp" ++ nameBase gadt

genHetLte :: Name -> DecsQ
genHetLte gadt = do
  signature <- [t| forall a b. $(conT gadt) a -> $(conT gadt) b -> Bool |]
  body <- [| $(varE hcmpFuncName) x y <= EQ |]
  return [ SigD funcName signature
         , FunD funcName [Clause [VarP $ mkName "x", VarP $ mkName "y"] (NormalB body) []]
         ]
    where
      funcName = mkName $ "hlte" ++ nameBase gadt
      hcmpFuncName = mkName $ "hcmp" ++ nameBase gadt

genHetCmp :: Name -> DecsQ
genHetCmp gadt = genHetCmp' funcName gadt
  where funcName = mkName $ "hcmp" ++ nameBase gadt

genHetCmp' :: Name -> Name -> DecsQ
genHetCmp' funcName gadt = do
  signature <- [t| forall a b. $(conT gadt) a -> $(conT gadt) b -> Ordering |]
  namesAndSize <- gadtFieldsAndSize gadt
  let namesAndSizeFlattened = flatten namesAndSize
      names = fst <$> namesAndSizeFlattened
      orderingsEq = (\x -> (x,x)) <$> names
      orderingsLt = orderings names
      orderingsGt = orderings (reverse names) in
      return [ SigD funcName signature
             , FunD funcName $
                    concatMap (uncurry genClauseParams) namesAndSizeFlattened
                    ++ (uncurry (genClause EQ) <$> orderingsEq)
                    ++ (uncurry (genClause LT) <$> orderingsLt)
                    ++ (uncurry (genClause GT) <$> orderingsGt)
             ]

genClause :: Ordering -> Name -> Name -> Clause
genClause ord term1 term2 =
          -- term1{} term2{} = ord
          Clause [RecP term1 [], RecP term2 []] (NormalB (ConE $ ordName ord)) []

ordName :: Ordering -> Name
ordName EQ = 'EQ
ordName GT = 'GT
ordName LT = 'LT

genClauseParams :: Name -> Int -> [Clause]
genClauseParams term params =
  (uncurry (genClauseParams' term params) <$> ((,) <$> [0 .. params - 1] <*> [LT, GT]))

genClauseParams' :: Name -> Int -> Int -> Ordering -> Clause
genClauseParams' term paramSize i ord =
  -- (term _ a _) (term _ b _) | compare a b == ord = ord
  --         ^ i-th position
  Clause [ConP term (oneHotVar "a"), ConP term (oneHotVar "b")] (GuardedB [(NormalG (guard "a" "b" ord), ConE $ ordName ord)]) []
  where
    -- compare a b == ord
    guard :: String -> String -> Ordering -> Exp
    guard var1 var2 result = InfixE (Just (AppE (AppE (VarE 'compare) (VarE $ mkName var1)) (VarE $ mkName var2)))
                                    (VarE '(==)) (Just (ConE $ ordName result))
    -- _ a _
    oneHotVar :: String -> [Pat]
    oneHotVar v = (\x -> if x then VarP (mkName v) else WildP) <$> oneHot paramSize i

oneHot :: Int -> Int -> [Bool]
oneHot 0 0 = []
oneHot max i =
  if i >= max
  then error "i must be smaller than max"
  else replicate i False ++ [True] ++ replicate (max - i - 1) False

flatten :: [([a], b)] -> [(a, b)]
flatten l = do
  (la, b) <- l
  a <- la
  return (a, b)

orderings :: [a] -> [(a,a)]
orderings [] = []
orderings (x:xs) = ((x,) <$> xs) ++ orderings xs

-- print reification
-- $(stringE . show =<< reify ''TestType)
