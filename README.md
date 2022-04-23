# ice1000
> A Elbereth Gilthoniel
> 
> silivren penna míriel!

A small programming language based on polarization.

![Simple type theory is so great](img/Suppression1.png)

# Techniques

![I wonder how, I wonder why](img/Suppression2.png)

Since we need to process many kinds of syntax trees with binding, `ice1000`
uses a free-monad style syntax specification as described in the haskell
[`unification-fd`](https://hackage.haskell.org/package/unification-fd)
package and a binding aware version
[here](http://arxiv.org/abs/2204.05653v1). For example, if we would like to
implement the syntax tree of the simply typed lambda calculus:

```hs
data TermF scope term
  = AppF term term
  | LamF scope
  deriving (Eq, Show)
$(deriveBifunctor ''TermF)
$(deriveBifoldable ''TermF)
$(deriveBitraversable ''TermF)
type Type = FS TermF

instance Unifiable TermF where
  zipMatch (AppF s t) (AppF s' t') = Just (AppF (s, s') (t, t'))
  zipMatch (LamF c) (LamF c') = Just (LamF (c, c'))
  zipMatch _ _ = Nothing
```

And the `zipMatch` can easily be derived with template haskell. Now we can
write unification out of the box:

```hs
test :: Either UnifyError (Map.Map Int (FS TypeF Int))
test = runMapUnifyEnv $ unifyEqs [(lhs, rhs)]
```

Note that this performs unification with binding, but does not allow equations.
So it is not higher order. For example, this is used to unify isorecursive types
in type inference, where typical equations are like
`µx. a -> Bool -> x  ~  µy. Int -> b -> y`, which should return the solution
`a := Int` and `b := Bool`.

# Type theory

# Core Language

# Compiling
