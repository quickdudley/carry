# The Carry Type System

## Syntax

Case is not used to distinguish type variables from concrete types: type
variables must be explicitly introduced using `\`. For example:

```carry
Maybe Int -- Type names will be capitalized by convention, but not relying
          -- on case will allow types to be named in alphabets which don't
          -- have cases.

Int -> Int -> Int -- The type of arithmetic operators specialized to "Int"

\a | Ord a . [a] -> [a] -- The type of "sort"

\a . [a] -> [a] -- The type of "take 1"

[\a . [a] -> [a]] -- The type of a list of fully polymorphic functions which
                  -- transform lists without inspecting their contents
```

The `\` symbol was chosen because of its semantic similarity to lambda, and
because among the symbols immediately available on a standard keyboard it
looks the most like `∀`. The use of `|` was also chosen because of the parallel
between type constraints and pattern guards.

## Type Inference

Inferred types will introduce each type variable as deeply as possible. For
example:

```carry
ex1 = Just sort -- inferred type will be "Maybe (\a | Ord a . [a] -> [a])"
```

In most other respects the type inference algorithm will give similar results to
Hindley & Milner, but whether or not its internal operation will be similar is
still undecided.

## Type Variable Substitution (pending either proof of safety or counterexample)

A value with type variables introduced deeply in the type structure can be used
as if the type variables had been introduced less deeply, but not vice versa.
For example:

```carry
ex2 :: \a | Bound a . [a]
ex3 :: [\a | Bound a . a]
```

All possible values of the same type as `ex3` can also be interpreted as being
the same type as `ex2`, but there are values of type `ex2` which cannot be
interpreted as being the same type as `ex3`.

The function type `->` will be a special case as it's safe to shift type
variable introduction to the right provided the variable isn't used on the left.
Any means of detecting this kind of special case will be investigated.

## Typeclasses

Similar to Haskell's typeclasses, multi parameter type classes and type families
enabled by default. A typeclass can provide default implementations for its
superclasses' members, and an instance declaration can define implementations
for both the typeclass and its superclasses. For example

```carry
class \f . Functor f where
  map :: \a b . (a -> b) -> f a -> f b


class \f | Functor f . Applicative f where
  pure :: \a . a -> f a
  (<*>) :: \a b . f (a -> b) -> f a -> f b
  default{0} fmap f x = pure f <*> x
```

With `Applicative` defined as above: an instance for `Functor` will be generated
automatically if none is provided. This could admittedly make the situation with
orphan instances worse than in Haskell, but will allow typeclasses to be
refactored to a greater degree without breaking code which already defines
instances.

## Other Possible Features

Existential quantification will not be supported in the initial version, but
this is not a significant limitation because it can be emulated with
impredicative types and continuation passing style.
