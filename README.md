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
`(µx. a -> Bool -> x)  ~  (µy. Int -> b -> y)`, which should return the solution
`a := Int` and `b := Bool`.

# Type theory

![Sensored](img/Suppression4.jpg)

# Compiling
There are four stages:

- `ice1000` is the full language. After parsing, we translate away some small
  syntactic sugars; do scope checking and remove all the module stuff.
- `ice100` is what's left. We then do type checking, and translate pattern matching
  into case trees. <!-- Lambda floating? Stuff like that --->
- `ice10` is what's left. This does not have type information now. And we proceed
  to do more optimizations, and compile to bytecode or something
- `ice1` is the bytecode. Now we may run it in a VM or compile to binary.
