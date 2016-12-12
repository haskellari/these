# 0.7.3

- Add `salign :: (Align f, Semigroup a) => f a -> f a -> f a`

# 0.7.2

- Support `aeson-1`: add `FromJSON1`, `FromJSON2` `ToJSON1`, and `ToJSON2` `These` instances.

# 0.7.1

- Add `AlignWithKey` in `Data.Align.Key` (added dependency `keys`)
- Add `These` instances for
    - `binary`: `Binary`
    - `aeson`: `FromJSON`, `ToJSON`
    - `QuickCheck`: `Arbitrary`, `CoArbitrary`, `Function`
    - `deepseq`: `NFData`

# 0.7

- Breaking change: Generalized `Monad`, `Applicative` instances of `These` and `Chronicle` to require only a `Semigroup` constraint
- More efficient `Align Seq` implementation
- Add `Crosswalk Seq` and `Vector` instances

# 0.6.2.1

- Support quickcheck-instances-0.3.12 (tests)

# 0.6.2.0

- Add support to bifunctors-5.1
