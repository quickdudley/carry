-- In spite of the title: this test is for the typechecker; not the ST monad.
-- If the typechecker is implemented correctly: the same mechanism which
-- prevents ST state variables from escaping their context in Haskell will also
-- work here.

newtype ST s a = ST a

runST :: \a . (\s . ST s a) -> a
runST (ST a) = a

unjoined :: \s . ST s (ST s ())
unjoined = ST (ST ())

test = runST unjoined
