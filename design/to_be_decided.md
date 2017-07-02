# Pending Decisions and/or Research

## Language

 * Decide whether or not to support any dependent typing

 * Design features for interoperability with object oriented systems. The
  following characteristics are desirable:

   1. Being able to use object oriented APIs, including functions which take
     things like Java's `Runnable` as parameters

   2. Being able to export object oriented APIs for use in other languages

   3. Achieving #1 and #2 without adopting an object oriented model in Carry
     itself.

 * Design a mechanism for typeclass authors to suggest monomorphization
  strategies to the compiler.

 * Design a feature for marking sections of code as being dependent on optional
  packages. An example use case is when a library author defines instances
  of a typeclass defined in another library, but doesn't use that library in
  any other way.

 * Possibly the same feature as above; design a feature for providing multiple
  platform-specific implementations of the same function.

 * Decide whether or not to implement the ideas introduced in Conal Elliott's
  paper [Compiling to Categories](http://conal.net/papers/compiling-to-categories)

   * If so: design a more explicit notation for instructing the compiler how
    to use typeclasses such as `BoolCat` and `NumCat` suggested in the paper

   * Also: I believe these ideas will make the bootstrap interpreter
    harder to implement, but once implemented will make the compiler easier.

 * How exactly rewrite rules will work; given the following:

   1. Users and library authors will be able to define rewrite phases

   2. There will be some kind of notation to signify that a rewrite rule may
     trigger when a given section of the left hand side is nop (that's to say:
     a function which can be implemented as a specialization of unsafeCoerce)

## Base Libraries

 * Exact structure for the numeric typeclasses. The following have been decided:

   1. `abs` and `signum` will be in a separate typeclass from everything else.

   2. `+` will be moved to `Semigroup`.
   
   3. `fromInteger` will remain in `Num`

 * Where exactly to define `pure`/`return` (we only need one). 2 options:
 
    1. Define `pure` in its own class, then `Applicative` is empty but is a
      subclass of both the class containing `pure` and `SemiApplicative`

    2. Define `pure` in `Applicative`, still move the other operators to
      `SemiApplicative`
