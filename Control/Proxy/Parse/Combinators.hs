{-| This module both exports convenient parser combinators from other libraries
    and provides several additional ones as well.

    The 'Alternative' and 'MonadPlus' combinators will only work with
    backtracking parsers.
-}
module Control.Proxy.Parse.Combinators (
    -- * Re-exports
    (<$>),
    (<$),
    Applicative(pure, (<*>), (*>), (<*)),
    (<**>),
    optional,
    Alternative(empty, (<|>), some, many),
    MonadPlus(mzero, mplus),
    msum,
    mfilter,
    guard,
    -- * Additional combinators
    few,
    anything,
    replicateA
    ) where

import Control.Applicative (
    (<$>),
    (<$),
    Applicative(pure, (<*>), (*>), (<*)),
    (<**>),
    optional,
    Alternative(empty, (<|>), some, many) )
import Control.Monad (MonadPlus(mzero, mplus), msum, mfilter, guard)
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

-- | Like 'replicateM', but faster, and with an 'Applicative' constraint
replicateA :: (Applicative f) => Int -> f a -> f [a]
replicateA n fa = fmap toList (S.replicateA n fa)
