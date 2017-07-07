# derive-het

Automatic derivation of heterogeneous functions (such as equality and ordering) for simple GADTs.

Currently supported is the following GADT structure:

```haskell
data SimpleGadt a where
  SimpleGadtT1 :: SimpleGadt Int
  SimpleGadtT2 :: SimpleGadt Bool
```

We can derive the implementation for the following functions automatically:

```haskell
heqSimpleGadt :: SimpleGadt a -> SimpleGadt b -> Bool
heqSimpleGadt SimpleGadtT1 SimpleGadtT1 = True
heqSimpleGadt SimpleGadtT2 SimpleGadtT2 = True
...
heqSimpleGadt _ _ = False

hlteSimpleGadt :: SimpleGadt a -> SimpleGadt b -> Bool
hlteSimpleGadt a b | heqSimpleGadt2 a b = True
hlteSimpleGadt SimpleGadtT1 SimpleGadtT2 = True
...
hlteSimpleGadt _ _ = False
```

by using `genHetEq` and `genHetLte`.

Examples can be found in the examples folder.
