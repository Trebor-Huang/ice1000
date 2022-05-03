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

# Sample Program

![Perfection](img/Suppression5.jpg)

Pseudocode on what programs should look like.

```
-- Ordinary routines, Builtin functions
radius( a : Float , b : Float) : Float
  = sqrt( mulf(a,a) , mulf(b,b) )  -- No fancy infix but it's easy to add

-- Case
andb( a : Bool , b : Bool ) : Bool
  = \case Pair(a, b) {  -- keywords are prefixed with a backslash
    Pair(True() , True()) => True()
      -- Nullary constructors, to keep the parser simple
    _ => False()
  }

-- Hindley Milnor Polymorphism
k( x : +a , y : +b ) : +a = x

-- Case distinction with zero cases
falso( x : Void ) : +a
  = \case x {}
```

## Values and continuations

```
add_with_cont( x : Int , y : Int , c : ~ Int) : #  -- Note the type!
  = c # add(x , y)  -- Combining a value with a continuation creates a #.

-- To create a continuation, do a pattern matching
example() : ~ Int
  = \continue {
    1 => blabla  -- Return a # here
    2 => blabla
    _ => blabla
  }
```

## Effects

```
-- I/O
hw(cont : ~ String) : #
  = [] <- print("Your name, please: ")
  ; [name] <- input()
  ; [] <- print(concat("Hello, ", name))
  ; cont # name

-- So the syntax
--   [t1, t2, ...] <- effect(args) ; prog
-- carries out the effect, puts the result in the t's,
-- and then carries out prog of type #, where the variables t1, t2, ...
-- can be used. The whole thing again has type #.
-- This operator is right associative, so you can chain.

-- Exiting is an effect.
exit() : #
  = [v] <- abort() ; \case v {}
-- abort() returns a Void.
```

## Data structures

Todo

# Type theory

![Censored](img/Suppression4.jpg)

The type theory consists of judgements of the form
```
T1 , T2 , ... ; S1 , S2 , ... |- t : J
```
where `J` is either `Program` or `By constructor T` or `By pattern S`.

Todo

# Compiling
There are four stages:

- `ice1000` is the full language. After parsing, we translate away some small
  syntactic sugars; do scope checking and remove all the module stuff.
- `ice100` is what's left. We then do type checking, and translate pattern matching
  into case trees. <!-- Lambda floating? Stuff like that --->
- `ice10` is what's left. This does not have type information now. And we proceed
  to do more optimizations, and compile to bytecode or something
- `ice1` is the bytecode. Now we may run it in a VM or compile to binary.
