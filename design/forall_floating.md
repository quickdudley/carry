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

### The left hand side of function arrows

```carry
example6 :: Maybe (\a | Monoid a . a) -> Integer
example7 :: (\a | Monoid a . Maybe a) -> Integer
```

A function with the type of `example7` can take an argument of either type
`(\a | Monoid a. Maybe a)` or `Maybe (\a | Monoid a . a)`. But a function with
the type of `example6` can only take arguments of type `Maybe (\a . a)`. That's
to say a value with the type of `example7` can be used as a value with the type
of `example6` but not vice-vesa.

### Generalizing to all types

```carry
data Example8 a b = Example6 (a -> b)

example9 :: Example6 (forall a . a -> Int) Int
example10 :: forall a . Example6 (a -> Int) Int
```

The observations regarding examples 1 to 3 also tell us that `example9` and
`example10` are distinct types. Examples 1 to 10 are sufficient to deduce a type
checking algorithm which safely implements forall floating.

### Rules

 * If a type parameter is applied on the left hand side of a function arrow,
  its introduction cannot be moved inwards or outwards

 * If a type parameter is applied to a constructor which is not a newtype
  constructor, its introduction cannot be moved inwards

 * If a type parameter is used in multiple ways, the restrictions for each use
  must all apply.

 * A type variable introduction may never be moved in such a way that it is
  referred to while out of scope.

 * If a complex type appears on the left hand of a function arrow: the
  directions in which type parameter introductions can be moved are reversed.

### Implementation

Carry will implement an extension to the kind system. As per Haskell: the kind
of a type with no type arguments will be `*`. Items on the left of kind
application arrows will be annotated to indicate how type variable
quantification can be safely moved. Programmers creating type constructors
will be able to directly annotate the kind, provided the manual annotation does
not allow any operation which would otherwise be forbidden.

The annotations will be single characters before the item they are annotating.

 * `|`: Type variable introductions may move in any direction

 * `-`: Type variable introductions may not move in any direction

 * `^`: Type variable introductions may only move outwards

 * `!`: Type variable introductions may only move inwards

 * `&`: The permitted movements of type variable introductions within this
  type argument are reversed.

```carry
-- Example annotated kinds
(->) :: &-* -> |* -> *
Example7 :: -* -> ^* -> *
Maybe :: ^* -> *
```
