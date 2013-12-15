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
    ) where

import Pipes
import Pipes.Parse

{-| @pipes-parse@ centers on three abstractions:

    * 'Producer's, unchanged from @pipes@, such as:

> producer :: Producer a m x

    * 'Lens''es between 'Producer's, which play a role analogous to 'Pipe's:

> lens :: Lens' (Producer a m x) (Producer b m y)

    * 'Parser's, which play a role analogous to 'Consumer's:

> parser :: Parser b m r

    There are four ways to connect these three abstractions:

    * Connect 'Parser's and 'Producer's using 'runStateT' \/ 'evalStateT' \/
      'execStateT':

> -- Retrieve both the result and leftovers
> runStateT  :: Parser a m r -> Producer a m x -> m (r, Producer a m x)
>
> -- Retrieve only the result
> evalStateT :: Parser a m r -> Producer a m x -> m  r
>
> -- Retrieve only the leftovers
> execStateT :: Parser a m r -> Producer a m x -> m (   Producer a m x)

    This works because a 'Parser' is just a 'StateT' action whose state is a
    'Producer':

> type Parser a m r = forall x . StateT (Producer a m x) m r

    For example:

> evalStateT parser producer :: m r

    * Connect 'Parser's and 'Lens''es using 'zoom'

> zoom
>     :: Lens' (Producer a m x) (Producer b m y)
>     -> Parser b m r
>     -> Parser a m r

    For example:

> zoom lens parser :: Parser a m r

    * Connect 'Producer's and 'Lens''es using ('^.'):

> (^.)
>     :: Producer a m x
>     -> Lens' (Producer a m x) (Producer b m y)
>     -> Producer b m y

    For example:

> producer^.lens :: Producer b m r

    * Connect two 'Lens''es using function composition:

> (.) :: Lens' (Producer a m x) (Producer b m y)
>     -> Lens' (Producer b m y) (Producer c m z)
>     -> Lens' (Producer a m x) (Producer c m z)

    'Parser's differ from 'Consumer's because 'Parser's can:

    * detect end-of-input using 'draw' or 'endOfInput',

    * 'unDraw' unused leftovers, and:
    
    * retrieve unused input after connecting the 'Producer' and 'Parser' using
      'runStateT' \/ 'execStateT' so that you can resume parsing later.

    Additionally, @pipes-parse@ provides 'Lens''es from 'Producer's to groups
    of 'Producer's delimited by 'FreeT', called \"splitters\":

> splitter :: Lens' (Producer a m x) (FreeT (Producer b m) m x)

    The key idea is that:

> -- '~' means "is analogous to"
> Producer a m ()            ~   [a]
>
> FreeT (Producer a m) m ()  ~  [[a]]
>
> splitter ~ Lens' [a] [[a]]

    'FreeT' nests each subsequent 'Producer' within the return value of the
    previous 'Producer' so that you cannot access the next 'Producer' until you
    completely drain the current 'Producer'.  However, you rarely need to work
    with 'FreeT' directly.  Instead, you can use \"transformations\" :

> transformation
>     :: FreeT (Producer a m) m () -> FreeT (Producer a m) m ()

    Think of these as analogous to list-like transformations of type:
    
> transformation ~ [[a]] -> [[a]]

    Use 'over' to combine a transformation and a splitter:

> over
>     :: Lens' (Producer a m x) (FreeT (Producer b m) m y)
>     -> (FreeT (Producer b m) m y -> FreeT (Producer b m) m y)
>     -> Producer a m x
>     -> Producer a m x

    For example:

> over splitter transformation producer :: Producer a m x

    You can also use splitters as getters:

> producer^.splitter :: FreeT (Producer b m) m y

    All the lenses in this module are \"improper\", meaning that they do not
    obey certain lens laws.  The significance of these law violations is that:

    * @zoom improper@ is usually not a monad morphism.  Here is a concrete
      example:

> do zoom (splitAt 4) parser1  /=  zoom (splitAt 4) $ do parser1
>    zoom (splitAt 4) parser2                            parser2 x

    * Using improper lenses as setters will do weird things

    The reason I use improper lenses is that I am not aware of a similarly
    convenient abstraction that can transform both 'Producer' and 'Parser's.

    For example, if you wanted to group standard input by equal lines and take
    the first three groups, you would write:

> import Pipes
> import qualified Pipes.Parse as Parse
> import qualified Pipes.Prelude as Prelude
>
> threeGroups :: (Monad m, Eq a) => Producer a m () -> Producer a m ()
> threeGroups = Parse.concat . Parse.takeFree 3 . Parse.groupBy (==)
> --            ^ Joiner       ^ Transformation   ^ Splitter

    This then limits standard input to the first three consecutive groups of
    equal lines:

>>> runEffect $ threeGroups Prelude.stdinLn >-> Prelude.stdoutLn
Group1<Enter>
Group1
Group1<Enter>
Group1
Group2<Enter>
Group2
Group3<Enter>
Group3
Group3<Enter>
Group3
Group4<Enter>
>>> -- Done, because we began entering our fourth group

    The advantage of this style or programming is that you never bring more than
    a single element into memory.  This works because `FreeT` sub-divides the
    `Producer` without concatenating elements together, preserving the laziness
    of the underlying 'Producer'.

    The bottom half of this module lets you implement your own list-like
    transformations using monadic parsers.

    For example, if you wanted to repeatedly sum every 3 elements and yield the
    result, you would write:

> import Control.Monad (unless)
> import Pipes
> import qualified Pipes.Prelude as P
> import Pipes.Parse
>
> sum3 :: (Monad m, Num a) => Producer a (StateT (Producer a m ()) m) ()
> sum3 = do
>     eof <- lift isEndOfInput
>     unless eof $ do
>         n <- lift $ P.sum (input >-> P.take 3)
>         yield n
>         sum3

    When you are done building the parser, you convert your parser to a
    list-like function using `evalStateP`:

> import Pipes.Lift (evalStateP)
>
> -- sum3'  ~  (Num a) => [a] -> [a]
>
> sum3' :: (Monad m, Num a) => Producer a m () -> Producer a m ()
> sum3' p = evalStateP p sum3

    ... then apply it to the `Producer` you want to transform:

>>> runEffect $ sum3' (P.readLn >-> P.takeWhile (/= 0)) >-> P.print
1<Enter>
4<Enter>
5<Enter>
10
2<Enter>
0<Enter>
2
>>>

-}


