module Hs.Streaming
  ( unixProcessStream
  ) where

import Control.Concurrent.STM
import Streaming
import Streaming.Internal (Stream(..))
import Streaming.Prelude (yield)
import System.IO
import UnliftIO.Async

import qualified Data.Text.IO as Text
import qualified Streaming.Prelude as Streaming


handleToLines
  :: MonadIO m
  => Handle
  -> Stream (Of Text) m ()
handleToLines handle =
  loop
  where
    loop = do
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
  :: MonadIO m
  => Handle
  -> Handle
  -> Stream (Of (Either Text Text)) m ()
unixProcessStream stdoutHandle stderrHandle = do
  queue :: TQueue (Either Text Text) <-
    liftIO newTQueueIO

  stdoutAsync :: Async () <-
    liftIO . async $
      streamToTQueue queue (Streaming.map Right (handleToLines stdoutHandle))

  stderrAsync :: Async () <-
    liftIO . async $
      streamToTQueue queue (Streaming.map Left (handleToLines stderrHandle))

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
        pure (pure ())
