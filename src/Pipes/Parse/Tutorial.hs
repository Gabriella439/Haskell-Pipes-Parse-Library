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

> import Control.Applicative (liftA2)
> import Pipes.Parse
>
> drawTwo :: Monad m => Parser a m (Maybe (a, a))
> drawTwo = do
>     mx <- draw
>     my <- draw
>     return (liftA2 (,) mx my)

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
Just ("Pink","Elephants")

    The result is wrapped in a 'Maybe' because our 'Producer' might have less
    than two elements:

>>> evalStateT drawTwo (yield 0)
Nothing

    If either of our two 'draw's fails and returns a 'Nothing', the combined
    result will be a 'Nothing'.

    We can use 'runStateT' or 'execStateT' to retrieve unused elements after
    parsing:

>>> import Pipes
>>> (result, unused) <- runStateT drawTwo (each [1..4])
>>> print result
Just (1, 2)
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
    entire input into memory, which defeats the purpose of streaming parsing:

>>> evalStateT drawAll (each [1..])
<Does not terminate>

    But what if you wanted to draw just the first three elements from an
    infinite stream?  This is what lenses are for:

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
    example of how this works:

> outer :: Monad m => Producer Int m (Producer Int m ())
> outer = each [1..6] ^. splitAt 3

    In the above example the outer 'Producer' layer will contain the first three
    elements and the inner 'Producer' will contain the remaining elements:

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

    The above definition of @outer@ is exactly equivalent to:

> outer = do
>     each [1..3]
>     return (each [4..6])

    'Lens.Family.State.Strict.zoom' takes our lens a step further and uses it to
    limit our parser to the outer 'Producer' (the first three elements).  When
    the parser is done 'Lens.Family.State.Strict.zoom' also returns unused
    elements back to the original stream.  We can demonstrate this using the
    following example parser:

> splitExample :: Monad m => Parser a m (Maybe a, [a])
> splitExample = do
>     x <- zoom (splitAt 3) draw
>     y <- zoom (splitAt 3) drawAll
>     return (x, y)

    The second parser begins where the first parser left off:

>>> evalStateT splitExample (each [1..])
(Just 1,[2,3,4])

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
(Nothign,[1,2],Nothing)

-}

{- $conclusion
    @pipes-parse@ introduces core idioms for @pipes@-based parsing.  These
    idioms reuse 'Producer's, but introduce two new abstractions:
    'Lens.Family2.Lens''es and 'Parser's.

    This library is very minimal and only contains datatype-agnostic parsing
    utilities, so this tutorial does not explore the full range of parsing
    tricks using lenses.  See @pipes-bytestring@ and @pipes-text@ for more
    powerful examples of lens-based parsing.

    'Parser's are very straightforward to write, but lenses are more
    sophisticated.  If you are interested in writing your own custom lenses,
    study the implementation of 'splitAt'.

    'FreeT' requires even greater sophistication.  Study how 'groupsBy' works to
    learn how to use 'FreeT' to introduce boundaries in a stream of 'Producer's.
    You can then use 'FreeT' to create your own custom splitters.

    To learn more about @pipes-parse@, ask questions, or follow development, you
    can subscribe to the @haskell-pipes@ mailing list at:

    <https://groups.google.com/forum/#!forum/haskell-pipes>

    ... or you can mail the list directly at:

    <mailto:haskell-pipes@googlegroups.com>
-}
