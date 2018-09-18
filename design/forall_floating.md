# Forall Floating

## Rank N functions

Starting with Haskell's rank n types:

```haskell
example1 :: forall a . (forall b c . b -> c) -> a
example2 :: (forall b c . b -> c) -> forall a . a
example3 :: forall a b . (forall c . b -> c) -> a
```

In these examples: `example1` and `example2` are the same type, but example3 is
a different type. `example1` and `example2` require their first arguments to be
polymorphic functions, but `example3` is polymorphic on the type of its first
argument.

## Impredicative Types

Although a GHC extension exists for impredicative types: it's almost completely
incompatible with the type inference engine. Since Carry will have
impredicative types on by default: it's necessary to handle them more
gracefully.

### Types which have no constructors with functions as arguments

Our simplest relevant example is the Maybe type: which will have the same
definition as in Haskell.

```carry
data Maybe a = Nothing | Just a
```

Which gives us the following example types:

```carry
example4 :: Maybe (\a | Monoid a . a)
example5 :: \a | Monoid a . Maybe a
```

In `example5`: the type instantiation of `a` can affect whether a value's
constructor will be `Just` or `Nothing`, so there are values of this type which
cannot be represented as the type of `example4`. However; all possible values
with the type of example4 can be represented in the type of `example5`, so we
can conclude that the type of `example4` is a subtype of the type of
`example5`. At present: this is the only form of subtype polymorphism planned
for Carry, but it will likely require the type inference engine and type
checker to be significantly different in implementation from their counterparts
in GHC.

### Function types

Rank N types behave as in Haskell. But since arguments and results may have
subtyping: function types can also have subtyping.

```carry
example6 :: \a . Int -> [a]
example7 :: Int -> [\a . a]
example8 :: (\a . [a]) -> Int
example9 :: [\a . a] -> Int
```

In the above examples: the type of `example7` is a subtype of the type of
`example6` and the type of `example8` is a subtype of the type of `example9`
(because `example8` has a more general argument type)

### Implementation

Each type constructor will be associated with subtyping rules. Since some
polymorphic functions require their parameterised types to follow certain
subtyping rules: the `<:` type operator will be available to expresss
subtype constraints. For example:

```carry
example10 :: \m | m (\b . b) <: (\b . m b), Monad m . Bool -> m Int
```
