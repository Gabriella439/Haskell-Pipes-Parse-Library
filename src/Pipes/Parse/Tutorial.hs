{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-| @pipes-parse@ builds upon @pipes@ to add several missing features necessary
    to implement 'Parser's:

    * End-of-input detection, so that 'Parser's can react to an exhausted input
      stream

    * Leftovers support, which simplifies several parsing problems

    * Connect-and-resume, to connect a 'Producer' to a 'Parser' and retrieve
      unused input
-}

module Pipes.Parse.Tutorial (
    -- * Overview
    -- $overview

    -- * Parsers
    -- $parsers

    -- * Lenses
    -- $lenses

    -- * Getters
    -- $getters

    -- * Building Lenses
    -- $buildlenses

    -- * Conclusion
    -- $conclusion
    ) where

import Pipes
import Pipes.Parse

{- $overview
    @pipes-parse@ centers on three abstractions:

    * 'Producer's, unchanged from @pipes@

    * 'Parser's, which play a role analogous to 'Consumer's

    * 'Lens.Family2.Lens''es between 'Producer's, which play a role analogous to
      'Pipe's

    There are four ways to connect these three abstractions:

    * Connect 'Parser's to 'Producer's using 'runStateT' \/ 'evalStateT' \/
      'execStateT':

> runStateT  :: Parser a m r -> Producer a m x -> m (r, Producer a m x)
> evalStateT :: Parser a m r -> Producer a m x -> m  r
> execStateT :: Parser a m r -> Producer a m x -> m (   Producer a m x)


    * Connect 'Lens.Family2.Lens''es to 'Parser's using
      'Lens.Family.State.Strict.zoom'

> zoom :: Lens' (Producer a m x) (Producer b m y)
>      -> Parser b m r
>      -> Parser a m r

    * Connect 'Producer's to 'Lens.Family2.Lens''es using ('Lens.Family.^.') or
      'Lens.Family.view':

> (^.) :: Producer a m x
>      -> Lens' (Producer a m x) (Producer b m y)
>      -> Producer b m y

    * Connect 'Lens.Family2.Lens''es to 'Lens.Family2.Lens''es using ('.') (i.e.
      function composition):

> (.) :: Lens' (Producer a m x) (Producer b m y)
>     -> Lens' (Producer b m y) (Producer c m z)
>     -> Lens' (Producer a m x) (Producer c m z)

    You can obtain the necessary lens utilities from either:
    
    * The @lens-family-core@ library, importing @Lens.Family@ (for
      ('Lens.Family.^.') \/ 'Lens.Family.view' and 'Lens.Family.over') and
      @Lens.Family.State.Strict@ (for 'Lens.Family.State.Strict.zoom'), or:

    * The @lens@ library, importing @Control.Lens@ (for ('Control.Lens.^.') \/
      'Control.Lens.view', 'Control.Lens.over' and 'Control.Lens.zoom')

    This tutorial uses @Lens.Family@ since it has fewer dependencies and simpler
    types.
-}

{- $parsers
    'Parser's handle end-of-input and pushback by storing a 'Producer' in a
    'StateT' layer:

> type Parser a m r = forall x . StateT (Producer a m x) m r

    To draw a single element from the underlying 'Producer', use the 'draw'
    command:

> draw :: Monad m => Parser a m (Maybe a)

    'draw' returns the next element from the 'Producer' wrapped in 'Just' or
    returns 'Nothing' if the underlying 'Producer' is empty.  Here's an example
    'Parser' written using 'draw' that retrieves the first two elements from a
    stream:

> import Pipes.Parse
>
> drawTwo :: Monad m => Parser a m (Maybe a, Maybe a)
> drawTwo = do
>     mx <- draw
>     my <- draw
>     return (mx, my)
>
> -- or: drawTwo = liftM2 (,) draw draw

    Since a 'Parser' is just a 'StateT' action, you run a 'Parser' using the
    same run functions as 'StateT':

> -- Feed a 'Producer' to a 'Parser', returning the result and leftovers
> runStateT  :: Parser a m r -> Producer a m x -> m (r, Producer a m x)
>
> -- Feed a 'Producer' to a 'Parser', returning only the result
> evalStateT :: Parser a m r -> Producer a m x -> m  r
>
> -- Feed a 'Producer' to a 'Parser', returning only the leftovers
> execStateT :: Parser a m r -> Producer a m x -> m (   Producer a m x)

    All three of these functions require a 'Producer' which we feed to the
    'Parser'.  For example, we can feed standard input:

>>> evalStateT drawTwo Pipes.Prelude.stdinLn
Pink<Enter>
Elephants<Enter>
(Just "Pink",Just "Elephants")

    The result is wrapped in a 'Maybe' because 'draw' can fail if the 'Producer'
    is empty:

>>> evalStateT drawTwo (yield 0)
(Just 0,Nothing)

    Parsing might not necessarily consume the entire stream.  We can use
    'runStateT' or 'execStateT' to retrieve unused elements that our parser does
    not consume:

>>> import Pipes
>>> (result, unused) <- runStateT drawTwo (each [1..4])
>>> -- View the parsed result
>>> result
(Just 1,Just 2)
>>> -- Now print the leftovers
>>> runEffect $ for unused (lift . print)
3
4

-}

{- $lenses
    @pipes-parse@ also provides a convenience function for testing purposes that
    draws all remaining elements and returns them as a list:

> drawAll :: Monad m => Parser a m [a]

    For example:

>>> import Pipes
>>> import Pipes.Parse
>>> evalStateT drawAll (each [1..10])
[1,2,3,4,5,6,7,8,9,10]

    However, this function is not recommended in general because it loads the
    entire input into memory, which defeats the purpose of streaming parsing.

    You can instead use 'foldAll' if you wish to fold all input elements into a
    single result:

>>> evalStateT (foldAll (+) 0 id) (each [1..10])
55

    You can also use the @foldl@ package to simplify writing more complex folds:

>>> import Control.Applicative
>>> import Control.Foldl as L
>>> evalStateT (purely foldAll (liftA2 (,) L.sum L.maximum)) (each [1..10])
(55,Just 10)

    But what if you wanted to draw or fold just the first three elements from
    an infinite stream instead of the entire input?  This is what lenses are
    for:

> import Lens.Family
> import Lens.Family.State.Strict
> import Pipes
> import Pipes.Parse
>
> import Prelude hiding (splitAt, span)
>
> drawThree :: Monad m => Parser a m [a]
> drawThree = zoom (splitAt 3) drawAll

    'Lens.Family.State.Strict.zoom' lets you delimit a 'Parser' using a
    'Lens.Family2.Lens''.  The above code says to limit 'drawAll' to a subset of
    the input, in this case the first three elements:

>>> evalStateT drawThree (each [1..])
[1,2,3]

    'splitAt' is a 'Lens.Family2.Lens'' with the following type:

> splitAt
>     :: Monad m
>     => Int -> Lens' (Producer a m x) (Producer a m (Producer a m x))

    The easiest way to understand 'splitAt' is to study what happens when you
    use it as a getter:

> view (splitAt 3) :: Producer a m x -> Producer a m (Producer a m x) 

    In this context, @(splitAt 3)@ behaves like 'splitAt' from the Prelude,
    except instead of splitting a list it splits a 'Producer'.  Here's an
    example of how you can use 'splitAt':

> outer :: Monad m => Producer Int m (Producer Int m ())
> outer = each [1..6] ^. splitAt 3

    The above definition of @outer@ is exactly equivalent to:

> outer = do
>     each [1..3]
>     return (each [4..6])

    We can prove this by successively running the outer and inner 'Producer'
    layers:

>>> -- Print all the elements in the outer layer and return the inner layer
>>> inner <- runEffect $ for outer (lift . print)
1
2
3
>>> -- Now print the elements in the inner layer
>>> runEffect $ for inner (lift . print)
4
5
6

    We can also use lenses to modify 'Parser's, using
    'Lens.Family.State.Strict.zoom'.  When we combine
    'Lens.Family.State.Strict.zoom' with @(splitAt 3)@ we limit a parser to the
    the first three elements of the stream.  When the parser is done
    'Lens.Family.State.Strict.zoom' also returns unused elements back to the
    original stream.  We can demonstrate this using the following example
    parser:

> splitExample :: Monad m => Parser a m ([a], Maybe a, [a])
> splitExample = do
>     x <- zoom (splitAt 3) drawAll
>     y <- zoom (splitAt 3) draw
>     z <- zoom (splitAt 3) drawAll
>     return (x, y, z)

    The second parser begins where the first parser left off:

>>> evalStateT splitExample (each [1..])
([1,2,3],Just 4,[5,6,7])

    'span' behaves the same way, except that it uses a predicate and takes as
    many consecutive elements as possible that satisfy the predicate:

> spanExample :: Monad m => Parser Int m (Maybe Int, [Int], Maybe Int)
> spanExample = do
>     x <- zoom (span (>= 4)) draw
>     y <- zoom (span (<  4)) drawAll
>     z <- zoom (span (>= 4)) draw
>     return (x, y, z)

    Note that even if the first parser fails, subsequent parsers can still
    succeed because they operate under a different lens:

>>> evalStateT spanExample (each [1..])
(Nothing,[1,2,3],Just 4)

    You can even nest 'Lens.Family.State.Strict.zoom's, too:

> nestExample :: Monad m => Parser Int m (Maybe Int, [Int], Maybe Int)
> nestExample = zoom (splitAt 2) spanExample

    All the parsers from @spanExample@ now only see a subset of the input,
    namely the first two elements:

>>> evalStateT nestExample (each [1..])
(Nothing,[1,2],Nothing)

-}

{- $getters
    Not all transformations are reversible.  For example, consider the following
    contrived function:

> import Pipes
> import qualified Pipes.Prelude as P
>
> map' :: Monad m => (a -> b) -> Producer a m r -> Producer b m r
> map' f p = p >-> P.map f

    Given a function of type @(a -> b)@, we can transform a stream of @a@'s into
    a stream of @b@'s, but not the other way around.  Transformations which are
    not reversible and cannot be modeled as 'Pipe's can only be modeled as
    functions between 'Producer's.  However, 'Pipe's are preferable to functions
    between 'Producer's when possible because 'Pipe's can transform both
    'Producer's and 'Consumer's.

    If you prefer, you can use lens-like syntax for functions between
    'Producer's by promoting them to @Getter@s using 'Lens.Family.to':

> import Lens.Family
>
> example :: Monad m => Producer Int m ()
> example = each [1..3] ^. to (map' (*2))

    However, a function of 'Producer's (or the equivalent @Getter@) cannot be
    used transform 'Parser's (using 'Lens.Family.State.Strict.zoom' or
    otherwise) .  This reflects the fact that such a transformation cannot be
    applied in reversed.
-}

{- $buildlenses
    Lenses are very easy to write if you are willing to depend on either the
    @lens-family@ or @lens@ library.  Both of these libraries provide an
    'Lens.Family2.Unchecked.iso' function that you can use to assemble your own
    lenses.  You only need two functions which reversibly transform back and
    forth between a stream of @a@s and a stream of @b@s:

> -- "Forward"
> fw :: Producer a m x -> Producer b m y
>
> -- "Backward"
> bw :: Producer b m y -> Producer a m x

    ... such that:

> fw . bw = id
>
> bw . fw = id

    You can then convert them to a 'Lens.Family2.Lens'' using
    'Lens.Family2.Unchecked.iso':

> import Lens.Family2 (Lens')
> import Lens.Family2.Unchecked (iso)
>
> lens :: Lens' (Producer a m x) (Producer b m y)
> lens = iso fw bw

    You can even do this without incurring any dependencies if you rewrite the
    above code like this:

> -- This type synonym requires the 'RankNTypes' extension
> type Lens' a b = forall f . Functor f => (b -> f b) -> (a -> f a)
>
> lens :: Lens' (Producer a m x) (Producer b m y)
> lens k p = fmap bw (k (fw p))

    This is what @pipes-parse@ does internally, and you will find several
    examples of this pattern in the source code of the "Pipes.Parse" module.

    Lenses defined using either approach will work with both the @lens@ and
    @lens-family@ libraries.

    You can even use `Parser`s to build a function between `Producer`s.  For
    example, a very common idiom is to define a function of type:

> example :: Monad m => Producer a m r -> Producer b m (Producer a m r)

    ... which parses as many @\'b\'@s as possible from the input stream of
    @\'a\'@s, returning the remainder of the stream if parsing fails.

    You can define that in terms of a parser of type:

> parser :: Monad m => StateT (Producer a m x) (Producer b m) r
>
> example = execStateT parser

    However, writing a parser of that type requires a few changes for
    everything to type-check.  For example, the `draw` function does not have
    the correct type for the above @parser@:

> draw :: StateT (Producer a m x) m (Maybe a)

    ... but @(hoist lift draw)@ does have the correct type, where `Pipes.hoist`
    comes from the @mmorph@ library and is re-exported by @pipes@:

> hoist lift draw :: StateT (Producer a m x) (Producer b m) (Maybe a)

    Similarly, `yield` does not have the right type when you want to emit an
    element of type @\'b\'@:

> yield :: Monad m => b -> Producer b m ()

    ... but @(lift . yield)@ does have the right type:

> lift . yield :: Monad m => b -> StateT (Producer a m x) (Producer b m) ()

-}

{- $conclusion
    @pipes-parse@ introduces core idioms for @pipes@-based parsing.  These
    idioms reuse 'Producer's, but introduce two new abstractions:
    'Lens.Family2.Lens''es and 'Parser's.

    This library is very minimal and only contains datatype-agnostic parsing
    utilities, so this tutorial does not explore the full range of parsing
    tricks using lenses.  For example, you can also use lenses to change the
    element type.

    Several downstream libraries provide more specific functionality, including:

    * @pipes-binary@: Lenses and parsers for @binary@ values

    * @pipes-attoparsec@: Converts @attoparsec@ parsers to @pipes@ parsers

    * @pipes-aeson@: Lenses and parsers for JSON values

    * @pipes-bytestring@: Lenses and parsers for byte streams

    * @pipes-text@: Lenses and parsers for text encodings

    To learn more about @pipes-parse@, ask questions, or follow development, you
    can subscribe to the @haskell-pipes@ mailing list at:

    <https://groups.google.com/forum/#!forum/haskell-pipes>

    ... or you can mail the list directly at:

    <mailto:haskell-pipes@googlegroups.com>
-}
