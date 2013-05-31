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

    -- * Isolating leftovers
    -- $mix

    -- * Diverse leftovers
    -- $diverse

    -- * Lenses
    -- $lenses

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

{- $mix
    Why use 'mempty' instead of @[]@?  @pipes-parse@ lets you easily mix
    distinct leftovers buffers into the same 'StateP' layer and 'mempty' will
    still do the correct thing when you use multiple buffers.

    For example, let's say that we want to mix three of the @pipes-parse@
    utilities:

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

    We often don't want composed parsing stages to share the same leftovers
    buffer, and @pipes-parse@ provides a way to reflect that in the types.  We
    can change the type of @chunks@ to:

> chunks
>     :: (Monad m, Proxy p)
>     => () -> Pipe (StateP ([a], [a]) p) (Maybe a) [a] m ()
> --                          ^    ^
> --                          |    |
> -- Two leftovers buffers ---+----+

    Now our 'StateP' layer holds two separate leftovers buffers, and we can
    specify which buffer each parsing primitive should interact with using
    lenses:

> chunks () = loop
>   where
>     loop = do
>         as  <- (zoom _fst . passUpTo 3 >-> zoom _snd . drawAll) ()
>         respond as
>         eof <- zoom _fst isEndOfInput
>         unless eof loop

    We use @zoom _fst@ to target both `passUpTo` and `isEndOfInput` to the first
    leftovers buffer and @zoom _snd@ to target `drawAll` to the second leftovers
    buffer.  This isolates the two leftovers buffers from each other, giving the
    desired behavior:

>>> runProxy $ evalStateK mempty $ wrap . enumFromToS 1 15 >-> chunks >-> printD
[1,2,3]
[4,5,6]
[7,8,9]
[10,11,12]
[13,14,15]

    Notice that 'mempty' still initializes both leftovers buffers correctly
    because:

> (mempty :: ([a], [a])) = ([], [])
-}

{- $diverse
    This trick is also useful for mixing stages with different leftover types,
    such as the following two pipes:

> adder
>     :: (Monad m, Proxy p) => () -> Consumer (StateP [Int] p) (Maybe Int) m Int
> adder () = fmap sum $ drawAll ()
> 
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

    We can use the same 'zoom' trick to unify them into a single 'StateP' layer:

> combined
>     :: (Monad m, Proxy p)
>     => () -> Consumer (StateP ([String], [Int]) p) (Maybe String) m Int
> combined = zoom _fst . tallyLength >-> zoom _snd . adder
>
> source :: (Monad m, Proxy p) => () -> Producer p (Maybe String) m ()
> source = fromListS ["One", "Two", "Three"]

    ... which gives the correct behavior:

>>> runProxy $ evalStateK mempty $ source >-> combined
20

-}

{- $lenses
    Let's study the type of 'zoom' to understand how it works:

> -- zoom's true type is slightly different to avoid a dependency on `lens`
> zoom :: Lens' s1 s2 -> StateP s2 p a' a b' b m r -> StateP s1 p a' a b' b m r

    'zoom' behaves like the function of the same name from the @lens@ package,
    zooming in on a sub-state using the provided lens.  When we give it the
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
>
> p = zoom _buf1 . p1 >-> zoom _buf2 . p2 >-> zoom _buf3 . p3

    'zoom' also works seamlessly with all lenses from the @lens@ package, but
    you don't need a @lens@ dependency to use @pipes-parse@.
-}

{- $conclusion
    @pipes-parse@ provides standardized end-of-input and leftovers utilities for
    you to use in your @pipes@-based libraries.  Unlike other streaming
    libraries, you wield more precise control over sharing and isolation of
    leftovers buffers.  Also, @pipes-parse@ requires no buy-in from the rest of
    the @pipes@ ecosystem thanks to compatibility routines like 'fmapPull' that
    automatically lift existing pipes to interoperate with end-of-input
    protocols.

    This library is intentionally minimal and datatype-specific parsers belong
    in derived libraries.  This makes @pipes-parse@ a very light-weight and
    stable dependency that you can use in your own projects.
-}
