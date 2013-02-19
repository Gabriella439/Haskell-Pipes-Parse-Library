module Control.Proxy.Parse.Combinators (
    (<$>),
    (<$),
    Applicative(pure, (<*>), (*>), (<*)),
    (<**>),
    optional,
    Alternative(empty, (<|>), some, many),
    MonadPlus(mzero, mplus),
    msum,
    mfilter,
    guard
    ) where

import Control.Applicative (
    (<$>),
    (<$),
    Applicative(pure, (<*>), (*>), (<*)),
    (<**>),
    optional,
    Alternative(empty, (<|>), some, many) )
import Control.Monad (MonadPlus(mzero, mplus), msum, mfilter, guard)
