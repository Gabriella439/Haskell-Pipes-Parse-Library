module Control.Proxy.Parse.Tutorial (
    -- * Introduction
    -- $introduction

    -- * End of input
    -- $eof

    -- * Compatibility
    -- $compatibility

    -- * Pushback and leftovers
    -- $leftovers

    -- * Mixing leftovers
    -- $lenses
    ) where

{- $introduction
    @pipes-parse@ provides utilities commonly required for parsing streams:

    * End of input utilities and conventions

    * Pushback and leftovers support for saving unused input

    * Tools to combine parsing stages that share leftover buffers

    * Tools to combine parsing stages with diverse or isolated leftover buffers
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

    Compose 'unwrap' downstream to unwrap every 'Just' and terminate on the
    first 'Nothing':

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

>>> runProxy $ wrap . enumFromToS 1 >-> fmapPull printD >-> unwrap
1
2
3

    This lifting cleanly distributes over composition and obeys the following
    laws:

> fmapPull (f >-> g) = fmapPull f >-> fmapPull g
>
> fmapPull pull = pull
-}

{- $leftovers
    To take advantage of leftovers support, just replace your 'request's with
    'draw':

> draw :: (Monad m, Proxy p) => StateP [a] p () (Maybe a) y' y m (Maybe a)

    ... and use 'unDraw' to push back leftovers:

> unDraw :: (Monad m, Proxy p) => a -> StateP [a] p x' x y' y m ()

    These both use a leftovers buffer of type @[a]@ stored in a 'StateP' layer.
    'unDraw' prepends elements to this list of leftovers and 'draw' will consume
    elements from this leftovers list until it is empty before requesting new
    input from upstream:

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

{- $lenses
    Why use 'mempty' instead of '[]'?  @pipes-parse@ lets you easily mix
    multiple distinct leftovers buffer into the same 'StateP' layer and 'mempty'
    will still do the correct thing when you start combining buffers.

    For example, imagine that you want to compose two separate parsers, one of
    
-}
