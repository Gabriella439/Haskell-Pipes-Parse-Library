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

    -- * FreeT
    -- $freeT

    -- * Conclusion
    -- $conclusion
    ) where

import Lens.Family2
import Lens.Family2.State.Strict
import Pipes
import Pipes.Parse

{- $overview
    @pipes-parse@ centers on three abstractions:

    * 'Producer's, unchanged from @pipes@

    * 'Parser's, which play a role analogous to 'Consumer's

    * 'Lens''es between 'Producer's, which play a role analogous to 'Pipe's

    There are four ways to connect these three abstractions:

    * Connect 'Parser's to 'Producer's using 'runStateT' \/ 'evalStateT' \/
      'execStateT':

> runStateT  :: Parser a m r -> Producer a m x -> m (r, Producer a m x)
> evalStateT :: Parser a m r -> Producer a m x -> m  r
> execStateT :: Parser a m r -> Producer a m x -> m (   Producer a m x)


    * Connect 'Lens''s to 'Parser's using 'zoom'

> zoom :: Lens' (Producer a m x) (Producer b m y)
>      -> Parser b m r
>      -> Parser a m r

    * Connect 'Producer's to 'Lens''es using 'view' or ('^.'):

> view :: Lens' (Producer a m x) (Producer b m y)
>      -> Producer a m x
>      -> Producer b m y

    * Connect 'Lens''es to 'Lens''es using ('.') (i.e.  function composition):

> (.) :: Lens' (Producer a m x) (Producer b m y)
>     -> Lens' (Producer b m y) (Producer c m z)
>     -> Lens' (Producer a m x) (Producer c m z)

    You can obtain the necessary lens utilities from either:
    
    * The @lens-family-core@ library, importing @Lens.Family@ (for
      'view' \/ ('^.') and 'over') and @Lens.Family.State.Strict@ (for 'zoom'),
      or:

    * The @lens@ library, importing @Control.Lens@ (for 'Control.Lens.view' \/
      ('Control.Lens.^.'), 'Control.Lens.over' and 'Control.Lens.zoom')

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

    'zoom' lets you delimit a 'Parser' using a 'Lens''.  The above code says to
    limit 'drawAll' to a subset of the input, in this case the first three
    elements:

>>> evalStateT drawThree (each [1..])
[1,2,3]

    'splitAt' is a 'Lens'' with the following type:

> splitAt
>     :: Monad m
>     => Int -> Lens' (Producer a m x) (Producer a m (Producer a m x))

    The easiest way to understand 'splitAt' is to study what happens when you
    use it as a getter:

> view (splitAt 3) :: Producer a m x -> Producer a m (Producer a m x) 

    In this context, @(splitAt 3)@ behaves like 'splitAt' from the Prelude,
    except instead of splitting a list it splits a 'Producer'.  The outer
    'Producer' contains up to 3 elements and the inner 'Producer' contains the
    remainder of the elements.

> outer :: Monad m => Producer Int m (Producer Int m ())
> outer = view (splitAt 3) (each [1..6])

>>> inner <- runEffect $ for outer (lift . print)
1
2
3
>>> runEffect $ for inner (lift . print)
4
5
6

    The above definition of @outer@ is equivalent to:

> outer = do
>     each [1..3]
>     return (each [4..6])

    'zoom' takes our lens a step further and uses it to limit our parser to the
    outer 'Producer' (the first three elements).  When the parser is done 'zoom'
    also returns unused elements back to the original stream.  We can
    demonstrate this using the following example parser:

> splitExample :: Monad m => Parser a m (Maybe a, [a])
> splitExample = do
>     x <- zoom (splitAt 3) draw
>     y <- zoom (splitAt 3) drawAll
>     return (x, y)

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

>>> evalStateT spanExample (each [1..])
(Nothing,[1,2,3],Just 4)

    You can even nest 'zoom's, too:

> nestExample :: Monad m => Parser Int m (Maybe Int, [Int], Maybe Int)
> nestExample = zoom (splitAt 2) spanExample

>>> evalStateT nestExample (each [1..])
(Nothign,[1,2],Nothing)

    Note that 'zoom' nesting obeys the following two laws:

> zoom lens1 . zoom lens2 = zoom (lens1 . lens2)
>
> zoom id = id

    Also note that 'view' nesting obeys the following two laws:

> view lens2 . view lens1 = view (lens1 . lens2)
>
> view id = id

    Both the 'zoom' and 'view' laws are examples of functor laws, and they
    ensure that it does not matter whether you prefer to connect lenses to each
    other or directly to 'Producer's and 'Parser's.

    However, the lenses in this library are improper, meaning that they violate
    certain lens laws.  The first consequence of this is that 'zoom' does not
    obey the monad morphism laws for these lenses.  For example:

> do x <- zoom (splitAt 3) m  /=  zoom (splitAt 3) $ do x <- m
>    zoom (splitAt 3) (f x)                             f x

    The second consequence is that these lenses cannot always be used as
    setters.  For example:

> p = do
>     each [1, 2]
>     return $ each [3, 4]
>
> p2 = view (splitAt 1) (set (splitAt 1) p (return ()))
>
> -- p2 = do
> --     yield 1
> --     return $ each [2, 3, 4]
> --
> -- p1 /= p2, which violates the first lens law

-}

{- $freeT
    @pipes-parse@ also provides convenient utilities for working with grouped
    streams in a list-like manner.  These utilities are analogous to list-like
    functions if you make the following translations:

> -- '~' means "is analogous to"
> Producer a m ()            ~   [a]
>
> FreeT (Producer a m) m ()  ~  [[a]]

    Think of @FreeT (Producer a m) m ()@ as a \"list of 'Producer's\".  'FreeT'
    nests each subsequent 'Producer' within the return value of the previous
    'Producer' so that you cannot access the next 'Producer' until you
    completely drain the current 'Producer'.  However, you rarely need to work
    with 'FreeT' directly.  Instead, you can structure most things using
    \"splitters\", \"transformations\" and \"joiners\":

> -- A "splitter"
> Producer a m ()           -> FreeT (Producer a m) m ()  ~   [a]  -> [[a]]
>
> -- A "transformation"
> FreeT (Producer a m) m () -> FreeT (Producer a m) m ()  ~  [[a]] -> [[a]]
>
> -- A "joiner"
> FreeT (Producer a m) m () -> Producer a m ()            ~  [[a]] ->  [a]

    An example splitter is @(view groups)@, which splits a 'Producer' into
    'FreeT'-delimited 'Producer's, one for each group of consecutive equal
    elements:

> view groups :: (Eq a, Monad m) => Producer a m x -> FreeT (Producer a m) m x

    An example transformation is @(takes 3)@, which takes the first three
    'Producer's from a 'FreeT' and drops the rest:

> takes 3 :: Monad m => FreeT (Producer a m) m () -> FreeT (Producer a m) m ()

    An example joiner is 'concats', which collapses a 'FreeT' of 'Producer's
    back down into a single 'Producer':

> concats :: Monad m => FreeT (Producer a m) m x -> Producer a m x

    If you compose these three functions together, you will create a function
    that transforms a 'Producer' to keep only the first three groups of
    consecutive equal elements:

> import Lens.Family
> import Pipes.Parse
>
> threeGroups :: (Monad m, Eq a) => Producer a m () -> Producer a m ()
> threeGroups = concats . takes 3 . view groups

    Both splitting and joining preserve the streaming nature of 'Producer's and
    do not collect or buffer any values.  The transformed 'Producer' still
    outputs values immediately and does not wait for groups to complete before
    producing results.

>>> import Pipes
>>> import qualified Pipes.Prelude as P
>>> runEffect $ threeGroups P.stdinLn >-> P.stdoutLn
1<Enter>
1
1<Enter>
1
2<Enter>
2
3<Enter>
3
3<Enter>
3
4<Enter>
>>> -- Note that the 4 is not echoed

    Also, lenses simplify things even further.  The reason that 'groups' is a
    lens is because it actually packages both a splitter and joiner into a
    single package.  We can then use 'over' to handle both the splitting and
    joining for us:

>>> runEffect $ over groups (takes 3) P.stdinLn >-> P.stdoutLn

    This gives the exact same behavior because 'over' takes care of calling the
    splitter before applying the transformation, then calling the joiner
    afterward.
-}

{- $conclusion
    @pipes-parse@ introduces core idioms for @pipes@-based parsing.  These
    idioms reuse 'Producer's, but introduce two new abstractions:
    'Lens''es and 'Parser's.

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
