module Hs.Streaming
  ( unixProcessStream
  ) where

import Hs.Eff.Register

import Control.Concurrent.STM
import Control.Effect
-- import Control.Monad.IO.Unlift
import Streaming
import Streaming.Internal (Stream(..))
import Streaming.Prelude (yield)
import System.Exit (ExitCode)
import System.IO
import System.Process.Typed
import UnliftIO.Async
-- import UnliftIO.Exception (bracket)

import qualified Data.Text.IO as Text
import qualified Streaming.Prelude as Streaming


handleToLines
  :: MonadIO m
  => Handle
  -> Stream (Of Text) m ()
handleToLines handle =
  loop
  where
    loop =
      liftIO (hIsEOF handle) >>= \case
        False -> do
          line <- liftIO (Text.hGetLine handle)
          yield line
          loop

        True ->
          pure ()

streamToTQueue
  :: MonadIO m
  => TQueue a
  -> Stream (Of a) m r
  -> m r
streamToTQueue queue =
  Streaming.mapM_ (liftIO . atomically . writeTQueue queue)

unixProcessStream
  :: forall m sig.
     ( Carrier sig m
     , Member RegisterEffect sig
     , MonadIO m
     )
  => ProcessConfig () () ()
  -> Stream (Of (Either Text Text)) m ExitCode
unixProcessStream processConfig = do
  process :: Process () Handle Handle <-
    lift
      (register
        (startProcess
          (processConfig
            & setStdout createPipe
            & setStderr createPipe))
        stopProcess)

  queue :: TQueue (Either Text Text) <-
    liftIO newTQueueIO

  stdoutAsync :: Async () <-
    liftIO . async $
      streamToTQueue
        queue
        (Streaming.map Right (handleToLines (getStdout process)))

  stderrAsync :: Async () <-
    liftIO . async $
      streamToTQueue
        queue
        (Streaming.map Left (handleToLines (getStderr process)))

  fix $ \loop ->
    join . liftIO . atomically $
      do
        x <- readTQueue queue
        pure $ do
          yield x
          loop
      <|>
      do
        waitSTM stdoutAsync
        waitSTM stderrAsync
        pure (waitExitCode process)
