{-| This module defines the core machinery for non-backtracking parsers

    You can directly interleave non-backtracking parsing code with other @pipes@
    code, unlike backtracking parsers, which you must first 'commit'. -}

module Control.Proxy.Parse.Commit (
    -- * Non-backtracking parser
    ParseP(..),

    -- * Run functions
    runParseP,
    evalParseP,
    runParseK,
    evalParseK
    ) where

import qualified Control.Proxy as P
import Control.Proxy.Trans.State (StateP(StateP), runStateP, evalStateP)
import Control.Proxy.Trans.Maybe (MaybeP(MaybeP, runMaybeP))
import qualified Data.Sequence as S

{-| Use 'ParseP' for parsing if you want to:

    * stream input in as little memory as possible,

    * request input lazily and incrementally,

    * interleave side effects with parsing, and

    * diagnose parse failures with a stream of informative error messages.
-}
newtype ParseP i p a' a b' b m r =
    ParseP { unParseP :: StateP (S.Seq i) (MaybeP p) a' a b' b m r }
{- NOTE: Technically 'Seq' does not benefit the non-backtracking parser as much
         as the backtracking parser because you do not require efficient
         mappends or "postpends".  However, you still get strictness in the
         spine of the data structure which helps prevent space leaks, so it is
         slightly superior in that respect.  Also, I'm already paying for the
         containers dependency for the backtracking parser, and I've never seen
         lists outperform 'Seq', so I might as well use it here, too.  Plus, if
         I ever do need the more efficient operations later on then I don't need
         to make a breaking change to the API. -}

{-| Run a non-backtracking parser, returning the result and unconsumed input or
    failing with 'Nothing' -}
runParseP :: ParseP i p a' a b' b m r -> MaybeP p a' a b' b m (r, S.Seq i)
runParseP p = runStateP S.empty (unParseP p)

{-| Evaluate a non-backtracking parser, returning the result or failing with
    'Nothing' -}
evalParseP
 :: (Monad m, P.Proxy p) => ParseP i p a' a b' b m r -> MaybeP p a' a b' b m r
evalParseP p = evalStateP S.empty (unParseP p)

{-| Run a non-backtracking parser \'@K@\'leisli arrow, returning the result and
    unconsumed input or failing with 'Nothing' -}
runParseK
 :: (q -> ParseP i p a' a b' b m r) -> (q -> MaybeP p a' a b' b m (r, S.Seq i))
runParseK k q = runParseP (k q)

{-| Evaluate a non-backtracking parser \'@K@\'leisli arrow, returning the result
    or failing with 'Nothing' -}
evalParseK
 :: (Monad m, P.Proxy p)
 => (q -> ParseP i p a' a b' b m r) -> (q -> MaybeP p a' a b' b m r)
evalParseK k q = evalParseP (k q)
