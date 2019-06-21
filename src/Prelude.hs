module Prelude
  ( LazyByteString
  , module X
  ) where

import BasePrelude            as X
import Control.Applicative    as X
import Control.Category       as X ((>>>))
import Control.Monad          as X
import Control.Monad.IO.Class as X
import Data.ByteString        as X (ByteString)
import Data.Foldable          as X
import Data.Function          as X
import Data.Kind              as X (Type)
import Data.Maybe             as X
import Data.Text              as X (Text)
import Data.Word              as X
import GHC.Clock              as X (getMonotonicTimeNSec)
import GHC.Generics           as X (Generic)
import System.Exit            as X (ExitCode)

import qualified Data.ByteString.Lazy as Lazy (ByteString)

type LazyByteString
  = Lazy.ByteString
