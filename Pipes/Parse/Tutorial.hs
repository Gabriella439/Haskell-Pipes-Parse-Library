{-| This module provides the tutorial for the @pipes-parse@ library

    This tutorial assumes that you have read the @pipes@ tutorial in
    @Control.Proxy.Tutorial@.
-}

module Pipes.Parse.Tutorial (
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

    -- * Return value
    -- $return

    -- * Resumable Parsing
    -- $resume

    -- * Nesting
    -- $nesting

    -- * Conclusion
    -- $conclusion
    ) where

import Pipes
import Pipes.Parse

{- $introduction
    @pipes-parse@ provides utilities commonly required for parsing streams using
    @pipes@:

    * End of input utilities and conventions for the @pipes@ ecosystem

    * Pushback and leftovers support for saving unused input

    * Tools to combine parsing stages with diverse or isolated leftover buffers

    * Ways to delimit parsers to subsets of streams

    Use these utilities to parse and validate streaming input in constant
    memory.
-}

{- $eof
    To guard an input stream against termination, protect it with the 'wrap'
    function:

> wrap :: (Monad m) => Proxy a' a b' b m r -> Proxy a' a b' (Maybe b) m s

    This wraps all output values in a 'Just' and then protects against
    termination by producing a never-ending stream of 'Nothing' values:

>>> import Pipes
>>> import Pipes.Parse
>>> import qualified Pipes.Prelude as P
>>>
>>> -- Before
>>> runEffect $ (P.fromList [1..3] >-> P.print) ()
1
2
3
>>> -- After
>>> runEffect $ (wrap . P.fromList [1..3] >-> P.print) ()
Just 1
Just 2
Just 3
Nothing
Nothing
Nothing
Nothing
...

    You can also 'unwrap' streams:

> unwrap :: (Monad m) => () -> Pipe (Maybe a) a m ()

    'unwrap' behaves like the inverse of 'wrap'.  Compose 'unwrap' downstream of
    a pipe to unwrap every 'Just' and terminate on the first 'Nothing':

> wrap . p >-> unwrap = p

    You will commonly use 'unwrap' to cancel out with 'wrap' and terminate an
    infinite stream:

>>> runEffect $ (wrap . P.fromList [1..3] >-> P.tee P.print >-> unwrap >-> P.discard) ()
Just 1
Just 2
Just 3
Nothing

-}

{- $compatibility
    What if we want to ignore the 'Maybe' machinery entirely and interact with
    the original unwrapped stream?  We can use 'fmapPull' to lift existing
    proxies to ignore all 'Nothing's and only operate on the 'Just's:

> -- The actual type of 'fmapPull' is more general
> fmapPull
>     :: (Monad m)
>     => (() -> Pipe        a         b  m r)
>     -> (() -> Pipe (Maybe a) (Maybe b) m r)

    We can use this to lift the printing stage to operate only on the 'Just'
    values and auto-forward any 'Nothing's:

>>> runEffect $ (wrap . P.fromList [1..3] >-> fmapPull (P.tee P.print) >-> unwrap >-> P.discard) ()
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
    interoperate with the 'Maybe'-based end-of-input protocol.
-}

{- $leftovers
    To take advantage of leftovers support, just replace your 'request's with
    'draw':

> draw :: (Monad m) => Consumer (Maybe a) (StateT [a] m) (Maybe a)

    ... and use 'unDraw' to push back leftovers:

> unDraw :: (Monad m) => a -> Effect (StateT [a] m) ()

    These both use a last-in-first-out (LIFO) leftovers buffer of type @[a]@
    stored in a 'StateT' layer.  'unDraw' prepends elements to this list of
    leftovers and 'draw' will consume elements from the head of the leftovers
    list until it is empty before 'request'ing new input from upstream:

> import Control.Monad
> import Control.Monad.IO.Class
> import Pipes
> import Pipes.Lift
> import Pipes.Parse
> import qualified Pipes.Prelude as P
> 
> consumer :: () -> Consumer (Maybe Int) (StateT [Int] IO) ()
> consumer () = do
>     ma <- draw
>     liftIO $ print ma
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
>         liftIO $ print ma

    To run the 'StateT' layer, just provide an empty initial list:

>>> runEffect $ evalStateP [] $ (wrap . P.fromList [1..] >-> consumer) ()
Just 1
Just 1
Just 99

-}

{- $diverse
    @pipes-parse@ lets you easily mix distinct leftovers buffers into the same
    'StateT' layer using lenses.  For example, suppose that we need to compose
    parsing pipes that have different input types and therefore different types
    of leftovers buffers, such as the following two parsers:

> import Control.Monad.Trans.State.Strict
>
> tallyLength
>     :: (Monad m) => () -> Pipe (Maybe String) (Maybe Int) (StateT [String] m) r
> tallyLength () = loop 0
>   where
>     loop tally = do
>         respond (Just tally)
>         mstr <- draw
>         case mstr of
>             Nothing  -> forever $ respond Nothing
>             Just str -> loop (tally + length str)
>
> adder :: (Monad m) => () -> Consumer (Maybe Int) (StateT [Int] m) Int
> adder () = fmap sum $ drawAll ()

    We can use 'zoom' to unify these two parsers to share the same 'StateP'
    layer:

> import Control.Lens
> 
> combined
>     :: (Monad m)
>     => () -> Consumer (Maybe String) (StateT ([String], [Int]) m) Int
> --                                                ^       ^
> --                                                |       |
> --                       Two leftovers buffers ---+-------+
> combined = hoist (zoom _1) . tallyLength >-> hoist (zoom _2) . adder
> 
> source :: (Monad m) => () -> Producer String m ()
> source = P.fromList ["One", "Two", "Three"]

    'zoom' takes a @Lens'@ as an argument which specifies which subset of the
    state that each parser will use.  '_1' directs the @tallyLength@ parser to
    use the first leftovers buffer (i.e. @[String]@) and '_2' directs the
    @adder@ parser to use the second leftovers buffer (i.e. @[Int]@).

    To run this mixed buffers parser, provide both buffers initially empty:

>>> runEffect $ evalStateP ([], []) $ (wrap . source >-> combined) ()
20

    Let's study the type of 'zoom' to understand how it works:

> -- zoom's true type is more general
> zoom :: (Monad m) => Lens' s1 s2 -> StateT s2 m r -> StateT s1 m r

    'zoom' focuses in on a sub-state using the provided lens.  When we give it
    the '_1' lens we zoom in on the first element of a tuple:

> -- _1's true type is more general
> _1 :: Lens' (s1, s2) s1
>
> zoom _1 :: StateT s1 m r -> StateT (s1, s2) m r

    ... and when we give it the '_snd' lens we zoom in on the second element of
    a tuple:

> _2 :: Lens' (s1, s2) s2
>
> zoom _2 :: StateT s2 m r -> StateT (s1, s2) m r

    We 'hoist' both of these 'zoom's to modify the base monads of both parsers
    to agree on a common global state:

> tallyLength
>     :: (Monad m)
>     => () -> Pipe (Maybe String) (Maybe Int) (StateT [String] m) r
>
> hoist (zoom _1) . tallyLength 
>     :: (Monad m)
>     => () -> Pipe (Maybe String) (Maybe Int) (StateT ([String], Int) m) r
>
> adder
>     :: (Monad m)
>     => () -> Consumer (Maybe Int) (StateT [Int] m) Int
>
> hoist (zoom _2) . adder
>     :: (Monad m)
>     => () -> Consumer (Maybe Int) (StateT ([String], [Int]) m) Int

    Once they agree on the same state we can compose them directly.

    If you want to merge more than one leftovers buffer, you can either nest
    pairs of tuples:

> p =     hoist (zoom  _1      ) . p1
>     >-> hoist (zoom (_2 . _1)) . p2
>     >-> hoist (zoom (_2 . _2)) . p3

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
> -- Generates:
> -- buf1 :: Lens' Leftovers [String]
> -- buf2 :: Lens' Leftovers [Int]
> -- buf3 :: Lens' Leftovers [Double]
>
> p =     hoist (zoom buf1) . p1
>     >-> hoist (zoom buf2) . p2
>     >-> hoist (zoom buf3) . p3
-}

{- $mix
    'zoom' isn't the only way to isolate buffers.  Let's say that you want to
    mix the following three @pipes-parse@ utilities:

> -- Transmit up to the specified number of elements
> passUpTo :: (Monad m) => Int -> () -> Pipe (Maybe a) (Maybe a) (StateT [a] m) r
>
> -- Fold all input into a list
> drawAll :: (Monad m) => () -> Consumer (Maybe a) (StateT [a] m) [a]
>
> -- Check if at end of input stream
> isEndOfInput :: (Monad m) => Consumer (Maybe a) (StateT [a] m) Bool

    We might expect the following code to yield chunks of three elements at a
    time:

> chunks :: (Monad m) => () -> Pipe (Maybe a) [a] (StateT [a] m) ()
> chunks () = loop
>   where
>     loop = do
>         as <- (passUpTo 3 >-> drawAll) ()
>         respond as
>         eof <- isEndOfInput
>         unless eof loop

    ... but it doesn't:

>>> runEffect $ evalStateP [] $ (wrap . P.fromList [1..15] >-> chunks >-> hoist lift . P.print) ()
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
    add yet another permanent buffer to our global leftovers state.  To solve
    this, we embed 'drawAll' within a transient 'StateT' layer with the 'using'
    command:

> using
>     :: (Monad m)
>     => s -> Proxy a' a b' b (StateT s m) r -> Proxy a' a b' b (StateT s m) r
> using s = hoist lift . evalStateP s

    This runs the sub-parser within a temporary 'StateT' layer initialized with
    the given leftovers buffer.  The leftovers buffer vanishes without a trace
    when the sub-parser completes:

> chunks () = loop
>   where
>     loop = do
>         as  <- (passUpTo 3 >-> using [] . drawAll) ()
>         respond as
>         eof <- isEndOfInput
>         unless eof loop

    This runs 'drawAll' within a fresh temporary buffer so that it does not
    reuse the same buffer as the surrounding pipe:

>>> runEffect $ evalStateP [] $ (wrap . P.fromList [1..15] >-> chunks >-> hoist lift . P.print) ()
[1,2,3]
[4,5,6]
[7,8,9]
[10,11,12]
[13,14,15]

    Conversely, remove the 'using' if you deliberately want downstream parsers
    to share the same leftovers buffers as upstream parsers.
-}

{- $return
    'wrap' allows you to return values directly from parsers because it produces
    a polymorphic return value:

> -- The 's' is polymorphic and will type-check as anything
> wrap :: (Monad m) => Proxy a' a b' b m r -> Proxy a' a b' (Maybe b) m s

    This means that if you compose a parser downstream the parser can return the
    result directly:

> parser
>     :: (Monad m) => () -> Consumer (Maybe a) (StateT [a] m) (Maybe a, Maybe a)
> parser () = do
>     mx <- draw
>     my <- draw
>     return (mx, my)  -- Return the result

    The polymorphic return value of 'wrap' will type-check as anything,
    including our parser's result:

> effect :: (Monad m) => () -> Effect (StateT [Int] m) (Maybe Int, Maybe Int)
> effect = wrap . P.fromList [0..9] >-> parser

    So we can run this 'Effect' and retrieve the result directly from the
    return value:

>>> runEffect $ evalStateP [] $ effect ()
(Just 0, Just 1)

-}

{- $resume
    You can save leftovers buffers if you need to interrupt parsing for any
    reason.  Just replace 'evalStateP' with 'runStateP':

>>> runEffect $ runStateP [] $ (wrap . P.fromList [(0::Int)..] >-> passWhile (< 3) >-> unwrap >-> P.discard) ()
((), [3])

    This returns the leftovers buffers in the result so that you can reuse them
    later on.  In the above example, 'passWhile' pushed back the @3@ input onto
    the leftovers buffer, so the result includes the unused @3@.
-}

{- $nesting
    @pipes-parse@ allows you to cleanly delimit the scope of sub-parsers by
    restricting them to a subset of the stream, as the following example
    illustrates:

> import Control.Monad.IO.Class
>
> outerParser :: () -> Consumer (Maybe Int) (StateT [Int] IO) ([Int], [Int])
> outerParser () = do
>     liftIO $ putStrLn "Skip the first three elements"
>     (passUpTo 3 >-> using [] . skipAll) ()
>     liftIO $ putStrLn "Restrict innerParser to consecutive elements less than 10"
>     (passWhile (< 10) >-> using [] . innerParser) ()
>
> innerParser :: () -> Consumer (Maybe Int) (StateT [Int] IO) ([Int], [Int])
> innerParser () = do
>     liftIO $ putStrLn "- Get the next four elements"
>     xs <- (passUpTo 4 >-> using [] . drawAll) ()
>     liftIO $ putStrLn "- Get the rest of the input"
>     ys <- drawAll ()
>     return (xs, ys)

    Notice how we use 'evalStateK' each time we subset a parser so that the
    sub-parser uses a fresh and transient leftovers buffer.

>>> runEffect $ evalStateP [] $ (wrap . P.fromList [0..] >-> parser) ()
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

    * easily delimit parsers to subsets of the input, and

    * ignore standardization, thanks to compatibility functions like 'fmapPull'.

    This library is intentionally minimal and datatype-specific parsers belong
    in derived libraries.  This makes @pipes-parse@ a very light-weight and
    stable dependency that you can use in your own projects.

    You can ask any questions about @pipes-parse@ and other @pipes@ libraries on
    the official @pipes@ mailing list at
    <mailto:haskell-pipes@googlegroups.com>.
-}
