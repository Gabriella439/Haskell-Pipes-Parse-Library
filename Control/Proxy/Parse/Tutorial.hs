{-| This module provides the tutorial for the @pipes-parse@ library

    This tutorial assumes that you have read the @pipes@ tutorial in
    @Control.Proxy.Tutorial@.
-}

module Control.Proxy.Parse.Tutorial (
    -- * Introduction
    -- $introduction

    -- * End of input
    -- $eof

    -- * Compatibility
    -- $compatibility

    -- * Pushback and leftovers
    -- $leftovers

    -- * Diverse leftovers
    -- $diverse

    -- * Isolating leftovers
    -- $mix

    -- * Lenses
    -- $lenses

    -- * Return value
    -- $return

    -- * Resumable Parsing
    -- $resume

    -- * Error handling
    -- $errors

    -- * Nesting
    -- $nesting

    -- * Conclusion
    -- $conclusion
    ) where

import Control.Proxy
import Control.Proxy.Parse

{- $introduction
    @pipes-parse@ provides utilities commonly required for parsing streams:

    * End of input utilities and conventions for the @pipes-ecosystem@

    * Pushback and leftovers support for saving unused input

    * Tools to combine parsing stages with diverse or isolated leftover buffers

    * Ways to delimit parsers to subsets of streams

    Use these utilities to parse and validate streaming input in constant
    memory.
-}

{- $eof
    To guard an input stream against termination, protect it with the 'wrap'
    function:

> wrap :: (Monad m, Proxy p) => p a' a b' b m r -> p a' a b' (Maybe b) m s

    This wraps all output values in a 'Just' and then protects against
    termination by producing a never-ending stream of 'Nothing' values:

>>> -- Before
>>> runProxy $ enumFromToS 1 3 >-> printD
1
2
3
>>> -- After
>>> runProxy $ wrap . enumFromToS 1 3 >-> printD
Just 1
Just 2
Just 3
Nothing
Nothing
Nothing
Nothing
...

    You can also 'unwrap' streams:

> unwrap :: (Monad m, Proxy p) => x -> p x (Maybe a) x a m ()

    'unwrap' behaves like the inverse of 'wrap'.  Compose 'unwrap' downstream of
    a pipe to unwrap every 'Just' and terminate on the first 'Nothing':

> wrap . p >-> unwrap = p

    You will commonly use 'unwrap' to terminate an infinite stream:

>>> runProxy $ wrap . enumFromToS 1 3 >-> printD >-> unwrap
Just 1
Just 2
Just 3
Nothing

-}

{- $compatibility
    What if we want to ignore the 'Maybe' machinery entirely and interact with
    the original unwrapped stream?  We can use 'fmapPull' to lift existing
    proxies to ignore all 'Nothing's and only operate on the 'Just's:

> fmapPull
>     :: (Monad m, Proxy p)
>     => (x -> p x        a  x        b  m r)
>     -> (x -> p x (Maybe a) x (Maybe b) m r)

    We can use this to lift 'printD' to operate on the original stream:

>>> runProxy $ wrap . enumFromToS 1 3 >-> fmapPull printD >-> unwrap
1
2
3

    This lifting cleanly distributes over composition and obeys the following
    laws:

> fmapPull (f >-> g) = fmapPull f >-> fmapPull g
>
> fmapPull pull = pull

    You can navigate even more complicated mixtures of 'Maybe'-aware and
    'Maybe'-oblivious code using 'bindPull' and 'returnPull'.

    @pipes-parse@ requires no buy-in from the rest of the @pipes@ ecosystem
    thanks to these adapter routines that automatically lift existing pipes to
    interoperate with end-of-input protocols.
-}

{- $leftovers
    To take advantage of leftovers support, just replace your 'request's with
    'draw':

> draw :: (Monad m, Proxy p) => StateP [a] p () (Maybe a) y' y m (Maybe a)

    ... and use 'unDraw' to push back leftovers:

> unDraw :: (Monad m, Proxy p) => a -> StateP [a] p x' x y' y m ()

    These both use a last-in-first-out (LIFO) leftovers buffer of type @[a]@
    stored in a 'StateP' layer.  'unDraw' prepends elements to this list of
    leftovers and 'draw' will consume elements from the head of the leftovers
    list until it is empty before requesting new input from upstream:

> consumer :: (Proxy p) => () -> Consumer (StateP [a] p) (Maybe Int) IO ()
> consumer () = do
>     ma <- draw
>     lift $ print ma
>     -- You can push back values you never drew
>     unDraw 99
>     -- You can push back more than one value at a time
>     case ma of
>         Nothing -> return ()
>         -- The leftovers buffer only stores unwrapped values
>         Just a  -> unDraw a
>     -- Values come out of the buffer in last-in-first-out (LIFO) order
>     replicateM_ 2 $ do
>         ma <- draw
>         lift $ print ma

    To run the 'StateP' layer, just provide an empty initial state using
    'mempty':

>>> runProxy $ evalStateK mempty $ wrap . enumFromS 1 >-> consumer
Just 1
Just 1
Just 99

-}

{- $diverse
    Why use 'mempty' instead of @[]@?  @pipes-parse@ lets you easily mix
    distinct leftovers buffers into the same 'StateP' layer and 'mempty' will
    still do the correct thing when you use multiple buffers.

    For example, suppose that we need to compose parsing pipes that have
    different input types and therefore different types of leftovers buffers,
    such as the following two parsers:

> tallyLength
>     :: (Monad m, Proxy p)
>     => () -> Pipe (StateP [String] p) (Maybe String) (Maybe Int) m r
> tallyLength () = loop 0
>   where
>     loop tally = do
>         respond (Just tally)
>         mstr <- draw
>         case mstr of
>             Nothing  -> forever $ respond Nothing
>             Just str -> loop (tally + length str)
>
> adder
>     :: (Monad m, Proxy p)
>     => () -> Consumer (StateP [Int] p) (Maybe Int) m Int
> adder () = fmap sum $ drawAll ()

    We can use 'zoom' to unify these two parsers to share the same 'StateP'
    layer:

> combined
>     :: (Monad m, Proxy p)
>     => () -> Consumer (StateP ([String], [Int]) p) (Maybe String) m Int
> --                                 ^       ^
> --                                 |       |
> --        Two leftovers buffers ---+-------+
> combined = zoom _fst . tallyLength >-> zoom _snd . adder
>
> source :: (Monad m, Proxy p) => () -> Producer p String m ()
> source = fromListS ["One", "Two", "Three"]

    'zoom' takes a @Lens'@ as an argument which specifies which subset of the
    state that each parser will use.  '_fst' directs the @tallyLength@ parser to
    use the @[String]@ leftovers buffer and '_snd' directs the @adder@ parser to
    use the @[Int]@ leftovers buffer.

    Notice that we can still run the mixture of buffers by supplying 'mempty':

>>> runProxy $ evalStateK mempty $ wrap . source >-> combined
20

    This works because:

> (mempty :: ([String], [Int])) = ([], [])
-}

{- $lenses
    Let's study the type of 'zoom' to understand how it works:

> -- zoom's true type is slightly different to avoid a dependency on `lens`
> zoom :: Lens' s1 s2 -> StateP s2 p a' a b' b m r -> StateP s1 p a' a b' b m r

    'zoom' behaves like the function of the same name from the @lens@ package
    and zooms in on a sub-state using the provided lens.  When we give it the
    '_fst' lens we zoom in on the first element of a tuple:

> _fst :: Lens' (a, b) a
>
> zoom _fst :: StateP s1 p a' a b' b m r -> StateP (s1, s2) p a' a b' b m r

    ... and when we give it the '_snd' lens we zoom in on the second element of
    a tuple:

> _snd :: Lens' (a, b) b
>
> zoom _snd :: StateP s2 p a' a b' b m r -> StateP (s1, s2) p a' a b' b m r

    '_fst' and '_snd' are like '_1' and '_2' from the @lens@ package, except
    with a more monomorphic type.  This ensures that type inference works
    correctly when supplying 'mempty' as the initial state.

    If you want to merge more than one leftovers buffer, you can either nest
    pairs of tuples:

> p = zoom _fst . p1 >-> zoom (_snd . _fst) . p2 >-> zoom (_snd . _snd) . p3

    ... or you can create a data type that holds all your leftovers and generate
    lenses to its fields:

> import Control.Lens hiding (zoom)
>
> data Leftovers = Leftovers
>     { _buf1 :: [String]
>     , _buf2 :: [Int]
>     , _buf3 :: [Double]
>     }
> makeLenses ''Leftovers
> {- Generates:
>    buf1 :: Lens' Leftovers [String]
>    buf2 :: Lens' Leftovers [Int]
>    buf3 :: Lens' Leftovers [Double]
> -}
>
> instance Monoid Leftovers where
>     mempty = Leftovers [] [] []
>     mappend (Leftovers as bs cs) (Leftovers as' bs' cs')
>         = Leftovers (as ++ as') (bs ++ bs') (cs ++ cs')
>
> p = zoom buf1 . p1 >-> zoom buf2 . p2 >-> zoom buf3 . p3

    'zoom' works seamlessly with all lenses from the @lens@ package, but you
    don't need a @lens@ dependency to use @pipes-parse@.
-}

{- $mix
    'zoom' isn't the only way to isolate buffers.  Let's say that you want to
    mix the following three @pipes-parse@ utilities:

> -- Transmit up to the specified number of elements
> passUpTo
>     :: (Monad m, Proxy p)
>     => Int -> () -> Pipe (StateP [a] p) (Maybe a) (Maybe a) m r
>
> -- Fold all input into a list
> drawAll :: (Monad m, Proxy p) => () -> StateP [a] p () (Maybe a) y' y m [a]
>
> -- Check if at end of input stream
> isEndOfInput :: (Monad m, Proxy p) => StateP [a] p () (Maybe a) y' y m Bool

    We might expect the following code to yield chunks of three elements at a
    time:

> chunks :: (Monad m, Proxy p) => () -> Pipe (StateP [a] p) (Maybe a) [a] m ()
> chunks () = loop
>   where
>     loop = do
>         as <- (passUpTo 3 >-> drawAll) ()
>         respond as
>         eof <- isEndOfInput
>         unless eof loop

    ... but it doesn't:

>>> runProxy $ evalStateK mempty $ wrap . enumFromToS 1 15 >-> chunks >-> printD
[1,2,3]
[4,5,6,7]
[8,9,10,11]
[12,13,14,15]

    @chunks@ behaves strangely because 'drawAll' shares the same leftovers
    buffer as 'passUpTo' and 'isEndOfInput'.  After the first chunk completes,
    'isEndOfInput' peeks at the next value, @4@, and immediately 'unDraw's the
    value.  'drawAll' retrieves this undrawn value from the leftovers before
    consulting 'passUpTo' which is why every subsequent list contains an extra
    element.

    We often don't want composed parsing stages like 'drawAll' to share the same
    leftovers buffer as upstream stages, but we also don't want to use 'zoom' to
    add yet another permanent buffer to our global state.  To solve this, we
    embed 'drawAll' within a transient 'StateP' layer using 'evalStateK':

> chunks () = loop
>   where
>     loop = do
>         as  <- (passUpTo 3 >-> evalStateK mempty drawAll) ()
>         respond as
>         eof <- isEndOfInput
>         unless eof loop

    This runs 'drawAll' within a fresh temporary buffer so that it does not
    reuse the same buffer as the surrounding pipe:

>>> runProxy $ evalStateK mempty $ wrap . enumFromToS 1 15 >-> chunks >-> printD
[1,2,3]
[4,5,6]
[7,8,9]
[10,11,12]
[13,14,15]

    Conversely, remove the 'evalStateK' if you deliberately want downstream
    parsers to share the same leftovers buffers.
-}

{- $return
    'wrap' allows you to return values directly from parsers because it produces
    a polymorphic return value, @s@:

> wrap :: (Monad m, Proxy p) => p a' a b' b m r -> p a' a b' (Maybe b) m s

    This means that if you compose a parser downstream the parser can return the
    result directly:

> parser
>     :: (Monad m, Proxy p)
>     => () -> Consumer (StateP [a] p) (Maybe a) m (Maybe a, Maybe a)
> parser () = do
>     mx <- draw
>     my <- draw
>     return (mx, my)  -- Return the result

    The polymorphic return value of 'wrap' will type-check as anything,
    including our parser's result:

> session
>     :: (Monad m, Proxy p)
>     => () -> Session (StateP [Int] p) m (Maybe Int, Maybe Int)
> session = wrap . enumFromToS 0 9 >-> parser

    So we can run this 'Session' and retrieve the result directly from the
    return value:

>>> runProxy $ evalStateK session
(Just 0, Just 1)

-}

{- $resume
    You can save leftovers buffers if you need to interrupt parsing for any
    reason.  Just replace 'evalStateK' with 'runStateK':

>>> let session = wrap . enumFromS 0 >-> passWhile (< 3) >-> printD >-> unwrap
>>> runProxy $ runStateK mempty session
Just 0
Just 1
Just 2
Nothing
((), [3])

    'passWhile' pushed back the @3@ input onto the leftovers buffer and
    'runStateK' returns the leftovers in case we want to reuse them later on.
-}

{- $errors
    'draw' returns a 'Nothing' if it reaches the end of input, but this makes
    drawing multiple values tedious.  For example, if I want to draw three
    values, I'd have to hand-write the following error-checking loop:

> import Control.Proxy
> import Control.Proxy.Parse
>
> drawN
>     :: (Monad m, Proxy p)
>     => Int -> () -> Consumer (StateP [a] p) (Maybe a) m (Maybe [a])
> drawN n0 () = loop n0 []
>   where
>     loop 0 as = return $ Just (reverse as)
>     loop n as = do
>         ma <- draw
>         case ma of
>             Nothing -> return Nothing
>             Just a  -> loop (n - 1) (a:as)

    Or do I?  Why not use 'MaybeP' (from @Control.Proxy.Trans.Maybe@) to
    automate the 'Nothing' checks for me:

> draw
>     :: (Monad m, Proxy p) => Consumer (StateP [a] p) (Maybe a) m (Maybe a)
>
> MaybeP draw
>     :: (Monad m, Proxy p) => Consumer (MaybeP (StateP [a] p) (Maybe a) m a
>
> replicateM n $ MaybeP draw
>     :: (Monad m, Proxy p) => Consumer (MaybeP (StateP [a] p) (Maybe a) m [a]
>
> runMaybeP $ replicateM n $ MaybeP draw
>     :: (Monad m, Proxy p) => Consumer (StateP [a] p) (Maybe a) m (Maybe [a])

    That was easy!  The solution also more closely matches our intention: \"Get
    @n@ values, using 'MaybeP' to gloss over the 'Nothing' details\".

    Let's implement it:

> import Control.Monad
> import Control.Proxy.Trans.Maybe
>
> drawN
>     :: (Monad m, Proxy p)
>     => Int -> () -> StateP [a] p () (Maybe a) y' y m (Maybe [a])
> drawN n () = runMaybeP $ replicateM n $ MaybeP draw

    ... and see if it works as advertised:

>>> runProxy $ evalStateK mempty $ wrap . enumFromS   0   >-> drawN 4
Just [0,1,2,3]
>>> runProxy $ evalStateK mempty $ wrap . enumFromToS 0 2 >-> drawN 4
Nothing

    Note that this does not permit backtracking to fully recover from the failed
    parse.  If @drawN@ fails, the input it consumed is lost.  Derived libraries
    will build upon @pipes-parse@ to allow you to temporarily embed perfect
    backtracking parsers within the default non-backtracking background.
-}

{- $nesting
    @pipes-parse@ allows you to cleanly delimit the scope of sub-parsers by
    restricting them to a subset of the stream, as the following example
    illustrates:

> import Control.Proxy
> import Control.Proxy.Parse
>
> parser
>     :: (Proxy p)
>     => () -> Consumer (StateP [Int] p) (Maybe Int) IO ([Int], [Int])
> parser () = do
>     lift $ putStrLn "Skip the first three elements"
>     (passUpTo 3 >-> evalStateK mempty skipAll) ()
>     lift $ putStrLn "Restrict subParser to consecutive elements less than 10"
>     (passWhile (< 10) >-> evalStateK mempty subParser) ()
>
> subParser
>     :: (Proxy p)
>     => () -> Consumer (StateP [Int] p) (Maybe Int) IO ([Int], [Int])
> subParser () = do
>     lift $ putStrLn "- Get the next four elements"
>     xs <- (passUpTo 4 >-> evalStateK mempty drawAll) ()
>     lift $ putStrLn "- Get the rest of the input"
>     ys <- drawAll ()
>     return (xs, ys)

    The outer @parser@ correctly delimits the scope of the @subParser@ so that
    the final 'drawAll' only consumes elements less than @10@:

>>> runProxy $ evalStateK mempty $ wrap . enumFromS 0 >-> parser
Skip the first three elements
Restrict subParser to consecutive elements less than 10
- Get the next four elements
- Get the rest of the input
([3,4,5,6],[7,8,9])

-}

{- $conclusion
    @pipes-parse@ provides standardized end-of-input and leftovers utilities for
    you to use in your @pipes@-based libraries.  Unlike other streaming
    libraries, you can:

    * mix or isolate leftovers buffers in a precise and type-safe way,

    * easily delimit parsers to subsets of the input,

    * use convenient @pipes@ error-handling idioms like 'MaybeP', and

    * ignore standardization, thanks to compatibility functions like 'fmapPull'.

    This library is intentionally minimal and datatype-specific parsers belong
    in derived libraries.  This makes @pipes-parse@ a very light-weight and
    stable dependency that you can use in your own projects.
-}
