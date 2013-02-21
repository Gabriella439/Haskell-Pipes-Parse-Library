{-| Recommenentry point for this library

    Read "Control.Proxy.Parse.Tutorial" for an extended tutorial. -}

module Control.Proxy.Parse (
    -- * Modules
    -- $modules
    module Control.Proxy.Parse.Core,
    module Control.Proxy.Parse.Combinators
    ) where

import Control.Proxy.Parse.Core
import Control.Proxy.Parse.Combinators

{- $modules
    "Control.Proxy.Parse.Core" exports the central generic parsing machinery.

    "Control.Proxy.Parse.Combinators" provides parsing combinators -}
