[![Build Status](https://travis-ci.org/rubenpieters/derive-het.svg?branch=master)](https://travis-ci.org/rubenpieters/derive-het)

# derive-het

Automatic derivation of heterogeneous functions (such as equality and ordering) for simple GADTs.

Currently supported is the following GADT structure:

```haskell
data SimpleGadt a where
  SimpleGadtT1 :: SimpleGadt Int
  SimpleGadtT2 :: SimpleGadt Bool
  -- ... more declarations of the form `X :: SimpleGadt x`
```

We can derive the implementation for the following functions automatically:

Heterogeneous equality by using `genHetEq ''SimpleGadt`, which implemented manually would be:
```haskell
heqSimpleGadt :: SimpleGadt a -> SimpleGadt b -> Bool
heqSimpleGadt SimpleGadtT1 SimpleGadtT1 = True
heqSimpleGadt SimpleGadtT2 SimpleGadtT2 = True
...
heqSimpleGadt _ _ = False
```

Heterogeneous less or equal by using `genHetLte ''SimpleGadt`, which implemented manually would be:
```haskell
hlteSimpleGadt :: SimpleGadt a -> SimpleGadt b -> Bool
hlteSimpleGadt a b | heqSimpleGadt2 a b = True
hlteSimpleGadt SimpleGadtT1 SimpleGadtT2 = True
...
hlteSimpleGadt _ _ = False
```

Examples can be found in the examples folder.
