# Carry

A functional programming language with lazy evaluation and a Haskell like
syntax.

## Project Structure

 * /interpreter will contain a carry interpreter written in Haskell

 * /compiler will contain a carry compiler written in carry

 * /runtime will contain runtime libraries for various targets. Most will be
written in C, but some backends may require other languages.

 * /libs will contain the base library (including the Prelude module), libraries
closely related to the base library, and any library used by the comiler.

 * /snippets will contain short carry programs for testing the interpreter and
compiler.

 * /snippets/fail will contain short carry programs containing deliberate errors
which the interpreter and compiler should both reject.

 * /test will contain unit tests and quickcheck tests for all components.

## Notable Differences from Haskell (assuming GHC)

 1. The Num typeclass will be separated into classes of more closely related
functions.

 2. Type variables will be identified via explicit forall (with shorter syntax),
and although carry will be case sensitive it will allow any identifier to begin
with any case.

 3. Carry will support full integration with APIs written in supported object
oriented languages (feature yet to be designed)

 4. Rank n types and impredicative types will be enabled by default. The current
design does not include support for existential types

 5. Cyclic dependencies between modules will be handled without using bootfiles
or similar.

 6. The template metaprogramming system will not require all functions used in
splices to be defined in separate modules, but will require that no cycle in
the definition graph includes any splices.

 7. As well as an IO monad, there will be an INIT monad and an InitMonad
typeclass implemented by IO and INIT. INIT will get special treatment by the
compiler: when replacing the '=' in a top level definition with '<-': if the
right hand side has type 'INIT a', it will be run once when the left hand side
is first evaluated. Only functions which are relatively safe to use in this
manner will have the INIT type.