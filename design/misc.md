# Miscellaneous Decisions

## fixity declarations

The syntax will be the same as in Haskell. Fixity declarations will be allowed
anywhere that declarations are allowed, and will not be applied anywhere that
they are out of scope. Scoping will mostly follow the same rules as normal
declarations, with one additional rule: fixity declarations in module scope will
not be exported from the module unless the operator is defined in that module.