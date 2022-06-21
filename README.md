# ice1000
> A Elbereth Gilthoniel  
> silivren penna míriel!

A small programming language based on polarization.

![Simple type theory is so great](img/Suppression1.png)

# Techniques

![I wonder how, I wonder why](img/Suppression2.png)

(...)

# Sample Program

![Perfection](img/Suppression5.jpg)

Pseudocode on what programs should look like.

```
-- Ordinary routines, Builtin functions
radius( a : Float , b : Float ) : Float
  = sqrt( mulf(a,a) , mulf(b,b) )
  -- No fancy infix but it's easy to add

-- Case
andb( a : Bool , b : Bool ) : Bool
  = \case Pair(a, b) {  -- keywords are prefixed with a backslash
    Pair(True() , True()) => True()
      -- Nullary constructors, to keep the parser simple
    _ => False()
  }

-- Hindley Milnor Polymorphism
k( x : a , y : b ) : a = x

-- Case distinction with zero cases
falso( x : Void ) : a
  = \case x {}
```

## Values and continuations

```
add_with_cont( x : Int , y : Int , c :~ Int ) : #  -- Note the type!
  = c # add(x , y)  -- Combining a value with a continuation creates a #.

-- To create a continuation, do a pattern matching
example() :~ Int
  = \continue {
    1 => blabla  -- Return a # here
    2 => blabla
    _ => blabla
  }
```

## Effects

```
-- I/O
hw(cont :~ String) : #
  = [] <- print("Your name, please: ")
  ; [name] <- input()
  ; [] <- print(concat("Hello, ", name))
  ; cont # name

-- So the syntax
--   [t1, t2, ...] <- effect(args) ; prog
-- carries out the effect, puts the result in the t's, and then carries out
-- prog of type #, where the variables t1, t2, ... can be used. The whole thing
-- again has type #. This operator is right associative, so you can chain.

-- Exiting is an effect.
exit() : #
  = [v] <- abort() ; \case v {}
-- abort() returns a Void. During execution it directly exits the program.
-- This may seem slightly weird but it keeps the semantics uniform. Anyway you
-- can now use exit() directly to exit without doing this twist.
```

## Data structures

There is a slight subtlety in that we deliberately blur the difference of two
opposite things. So the explanation might get a little bit confusing.

Ordinary datatypes are straightforward:

```
-- A list of numbers (for simplicity, no polymorphism for now)
\data List { Nil() | Cons(head : Float, tail : List) }
-- You can still give names to these fields (Maybe I can automatically
-- generate projections? But I'm lazy), you can also choose to use
-- the _ wildcard.

append(a : List, b : List) : List
  = \case a {
    Nil() => b
    Cons(x, xs) => Cons(x, append(xs, b))
  }
```

The twist comes where you can also use the `:~` mechanism in datatypes:

```
\data Hole { Hole(_ :~ Float) }
```

(...)

We can have the eager/lazy distinction on data structures, but I'm too lazy.
Perhaps we have two keywords like `\eagerdata` and `\lazydata`, for which the
evaluation strategy will change accordingly. Currently I'm just going for all
call-by-need.

# Type theory

![Censored](img/Suppression4.jpg)

The type theory consists of judgements of the form

$$T_1, \dots, T_n; S_1, \dots, S_n \vdash J$$

where `J` is either `Program` or `By constructor T` or `By pattern S`.

A program (`#` in concrete syntax) is the result of combining a by-pattern term
with a by-constructor term. It can also be created by primitive effects. (...)

# Compiling
There are four stages:

- `ice1000` is the full language. After parsing, we translate away some small
  syntactic sugars; do scope checking and remove all the module stuff.
- `ice100` is what's left. We then do type checking, and translate pattern matching
  into case trees.
- `ice10` is what's left. This does not have type information now. And we proceed
  to do more optimizations, and compile to bytecode or something
- `ice1` is the bytecode. Now we may run it in a VM or compile to binary.
