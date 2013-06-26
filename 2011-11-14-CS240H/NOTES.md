# GHC Lecture Notes

Collection of notes for preparing and improving my lecture on how GHC
works. Some of the notes are general Haskell comments, i.e., good way
examples to teach students to help understand the nuances of the
language.

## Observations

return, evaluate and bottom:

    return :: a -> IO a
    evaluate :: a -> IO a

    return $bot$ >>= \_ -> putStrLn "Hi" == "Hi"
    evaluate $bot$ >>= \_ -> putStrLn "Hi" == "Hi"


fmap, applicative and monad:

    -- f is just a type constructor over a polymorphic type. But best
    -- to think of it as a 'computational context'.
    fmap :: (a -> b) -> f a -> f b

    -- applicative is the same but with the function itself lifted
    -- into the computational context.
    <*> :: f (a -> b) -> f a -> f b
    -- fmap f a = (<*>) (pure f) a

    -- monad bind can inspect intermediate result and make a decision
    -- *outside* the type, allowing changing of shape / structure of
    -- the type / computational.
    >>= :: (a -> m b) -> m a -> m b

    -- e.g., monads can collapse layers / structure.
    join :: m (m b) -> m b


Isomorphism of newtypes and data:

    -- how isomorphic is data? (i.e., Any to Bool?)
    data Any = Any { getAny :: Bool }

    -- isomorphic cases
    Any . getAny $ Any True  == Any True
    Any . getAny $ Any False == Any False
    Any . getAny $ Any $bot$ == Any $bot$

    -- non-isomorphic cases (want $bot$)
    Any . getAny $ $bot$     == Any $bot$

    -- how isomorphic is a strict data type?
    data Any' = Any' { getAny' :: Bool }

    -- isomorphic cases
    Any' . getAny' $ Any' True  == Any' True
    Any' . getAny' $ Any' False == Any' False
    Any' . getAny' $ $bot$      == $bot$

    -- non-isomorphic cases (want Any' $bot$)
    Any' . getAny' $ Any' $bot$ == $bot$


fix and fixIO:

    -- fixpoint, recursion operator
    fix :: (a -> a) -> a

    -- using fix we can define recursion without explicitly refuring
    -- to it. Here `f` represents `fib`.
    fib f 0 = 1
    fib f 1 = 1
    fib f n = f(n - 1) + f(n -2)

    -- type application
    fib :: (a -> a1) -> a -> a1
    fib :: a'@(a -> a1) -> b'(a -> a1)
    fix :: (a -> a) -> a
    fix fib :: a -> a1

    -- fixpoint in IO monad. The recursion is of a pure value `a`, not
    -- of the IO action. `fixIO` is strict in IO actions but not
    -- values.
    fixIO :: (a -> IO a) -> IO a
    
    cs r = getChar >>= \c -> return (c : r)
    -- let cs = 'a' : cs in cs

    cs :: [Char] -> IO [Char]
    fixIO cs :: IO [Char]
    -- produces an infinite list of characters consisting of just the
    -- first Char entered by the user (i.e., the IO action is only
    -- executed once).


## Questions

* Graph reduction: What kind of graph? DAG?

* Are let bindings computed once and shared? e.g
  ~~~~
  f x = let g = expensiveComputation 5 in x + 5
  ~~~~

* Is lazy evaluation explicit in Core / STG? Or is Core still lazy?
* How is seq implemented?

* how is lazy eval done and graph reduction? Is there some
  intelligence to the eval order (e.g rts basically has a graph
  evaluator) or is it simply that each function is compiled locally to
  be lazy and when they combine the various amounts of strictness
  evaluate the program? What about EZY claim that IO is the evaluator?

* Any difference between let and where?

* What does `'case xs of _' _` mean in core?

* In STG we have these nice rules:
  * case = evaluation and only place evaluation occurs
  * let  = allocation

* Two questions though about lazy evaluation and allocation in STG:
  * Is let the only place allocation occurs? What abbout partial
    application?
  * Are function application suspended in general? So function
    application is an allocation of a thunk?

