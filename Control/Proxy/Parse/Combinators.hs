{-| This module both exports convenient parser combinators from other libraries
    and provides several additional ones as well.

    The 'Alternative' and 'MonadPlus' combinators will only work with
    backtracking parsers.  Non-backtracking parsers do not support alternation.
-}
module Control.Proxy.Parse.Combinators (
    -- * Re-exports
    (<$>),
    (<$),
    Applicative(pure, (<*>), (*>), (<*)),
    (<**>),
    optional,
    S.replicateA,
    Alternative(empty, (<|>), some, many),
    replicateM_,
    MonadPlus(mzero, mplus),
    msum,
    mfilter,
    guard,
    -- * Additional combinators
    few,
    anything
    ) where

import Control.Applicative (
    (<$>),
    (<$),
    Applicative(pure, (<*>), (*>), (<*)),
    (<**>),
    optional,
    Alternative(empty, (<|>), some, many) )
import Control.Monad (
    replicateM_, MonadPlus(mzero, mplus), msum, mfilter, guard)
import Data.Foldable (toList)
import qualified Data.Sequence as S

-- | Like 'many', but orders results from fewest to most matches
few :: (Alternative f) => f a -> f [a]
few fa = go where
    go = pure [] <|> (fmap (:) fa <*> go)

-- | Like 'some', but orders results from fewest to most matches
anything :: (Alternative f) => f a -> f [a]
anything fa = ((:) <$> fa <*> go) where
    go = pure [] <|> (fmap (:) fa <*> go)
