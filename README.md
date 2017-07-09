[![Build Status](https://travis-ci.org/rubenpieters/derive-het.svg?branch=master)](https://travis-ci.org/rubenpieters/derive-het)

# derive-het

Automatic derivation of heterogeneous compare for simple GADTs.

Currently supported is the following GADT structure:

```haskell
data SimpleGadt a where
  SimpleGadtT1 :: Int ->  ... -> SimpleGadt Int
  SimpleGadtT2 :: Bool ->  ... -> SimpleGadt Bool
  -- ... more declarations of the form `X :: p1 -> (... ->) SimpleGadt x`
  -- the parameters of the gadt need to have an `Ord` instance
```

Then the following comparison function is derived (in this case we have 2 input parameters for `SimpleGadtT1`):

```haskell
hcmpSimpleGadt :: SimpleGadt a -> SimpleGadt b -> Ordering
hcmpSimpleGadt (SimpleGadtT1 a _) (SimpleGadtT1 b _) | compare a b == LT = LT
hcmpSimpleGadt (SimpleGadtT1 a _) (SimpleGadtT1 b _) | compare a b == GT = GT
hcmpSimpleGadt (SimpleGadtT1 _ a) (SimpleGadtT1 _ b) | compare a b == LT = LT
hcmpSimpleGadt (SimpleGadtT1 _ a) (SimpleGadtT1 _ b) | compare a b == GT = GT
-- these clauses are generated for the other constructors as well (SimpleGadtT2 in this case)
hcmpSimpleGadt (SimpleGadtT1{}) (SimpleGadtT1{}) == EQ
hcmpSimpleGadt (SimpleGadtT2{}) (SimpleGadtT2{}) == EQ
hcmpSimpleGadt (SimpleGadtT1{}) (SimpleGadtT2{}) == LT
hcmpSimpleGadt (SimpleGadtT2{}) (SimpleGadtT1{}) == GT
```

We can derive the implementation for the following functions automatically:

We can also use `genHetEq` or `genHetLte` to derive an equality or less-than-or-equal test, based on the above comparison function.

Examples can be found in the examples folder.
