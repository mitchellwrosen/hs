{-# LANGUAGE UndecidableInstances #-}

module Hs.Main.Build.Render
  ( RenderEff
  , runRender
  , renderDownloadingDependency
  , renderBuildingDependency
  , renderCompletedDependency
  ) where

import Hs.Cabal.Component (Component(..))
import Hs.Cabal.Package (Package(..))

import Control.Concurrent.STM
import Control.Effect
import Control.Effect.Carrier
import Control.Effect.Reader
import Control.Effect.State
import Control.Effect.Sum
import Data.HashMap.Strict (HashMap)
import Data.Set (Set)
import System.Console.ANSI
import System.Console.Regions

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set
import qualified Data.Text as Text


type Dependency
  = Either Package Component

data RenderEff (m :: Type -> Type) (k :: Type) where
  RenderDownloadingDependency
    :: Dependency
    -> k
    -> RenderEff m k

  RenderBuildingDependency
    :: Dependency
    -> Word64
    -> k
    -> RenderEff m k

  RenderCompletedDependency
    :: Dependency
    -> Word64
    -> k
    -> RenderEff m k

  deriving stock (Functor)
  deriving anyclass (HFunctor)

renderDownloadingDependency ::
     ( Carrier sig m
     , Member RenderEff sig
     )
  => Dependency
  -> m ()
renderDownloadingDependency dep =
  send (RenderDownloadingDependency dep (pure ()))

renderBuildingDependency ::
     ( Carrier sig m
     , Member RenderEff sig
     )
  => Dependency
  -> Word64
  -> m ()
renderBuildingDependency dep time =
  send (RenderBuildingDependency dep time (pure ()))

renderCompletedDependency ::
     ( Carrier sig m
     , Member RenderEff sig
     )
  => Dependency
  -> Word64
  -> m ()
renderCompletedDependency dep time =
  send (RenderCompletedDependency dep time (pure ()))


newtype RenderCarrier m a
  = RenderCarrier
  { unRenderCarrier ::
       ReaderC (TVar (Set (Double, Dependency)))
      (StateC (HashMap Dependency ConsoleRegion)
      (StateC (HashMap Dependency Word64) m)) a
  } deriving newtype (Applicative, Functor, Monad, MonadIO)

instance (Carrier sig m, Effect sig, MonadIO m)
      => Carrier (RenderEff :+: sig) (RenderCarrier m) where
  eff ::
       (RenderEff :+: sig) (RenderCarrier m) (RenderCarrier m a)
    -> RenderCarrier m a
  eff = \case
    L (RenderDownloadingDependency dep next) ->
      RenderCarrier $ do
        region <- liftIO (openConsoleRegion Linear)
        liftIO (setConsoleRegion region (pprDependency dep))
        modify (HashMap.insert dep region)
        unRenderCarrier next

    L (RenderBuildingDependency dep time next) ->
      RenderCarrier $ do
        regions <-
          get @(HashMap Dependency ConsoleRegion)

        when (isNothing (HashMap.lookup dep regions)) $ do
          region <- liftIO (openConsoleRegion Linear)
          liftIO (setConsoleRegion region (pprDependency dep))
          modify (HashMap.insert dep region)

        modify (HashMap.insert dep time)
        unRenderCarrier next

    L (RenderCompletedDependency dep endTime next) ->
      RenderCarrier $ do
        regions :: HashMap Dependency ConsoleRegion <-
          get

        beginTimes :: HashMap Dependency Word64 <-
          get

        doneVar :: TVar (Set (Double, Dependency)) <-
          ask

        let
          mregion :: Maybe ConsoleRegion
          regions' :: HashMap Dependency ConsoleRegion
          (mregion, regions') =
            HashMap.alterF
              (\case
                Nothing -> (Nothing, Nothing)
                Just region -> (Just region, Nothing))
              dep
              regions

        liftIO . atomically $ do
          for_ mregion $ \region ->
            closeConsoleRegion region

          for_ (HashMap.lookup dep beginTimes) $ \beginTime ->
            let
              elapsedTime :: Double
              elapsedTime =
                fromIntegral (endTime - beginTime) / 1e9
            in
              modifyTVar'
                doneVar
                (Set.insert (elapsedTime, dep))

        put regions'

        unRenderCarrier next

    R other ->
      RenderCarrier (eff (R (R (R (handleCoercible other)))))

pprDependency :: Dependency -> Text
pprDependency =
  either pprPackage pprComponent

pprPackage :: Package -> Text
pprPackage (Package name ver) =
  brightWhite name <> " " <> brightBlack ver

pprComponent :: Component -> Text
pprComponent = \case
  Executable package exeName ->
    pprPackage package <> blue " executable " <> brightWhite exeName

  Library package@(Package pkgName _) name ->
    pprPackage package <>
      (if name /= pkgName then blue " library  " <> brightWhite name else mempty)

  TestSuite package exeName ->
    pprPackage package <> blue " test suite " <> brightWhite exeName

pprDoneDependency :: (Double, Dependency) -> Text
pprDoneDependency (n, dep) =
  Text.pack (show (round n :: Int)) <> "s " <> pprDependency dep

runRender ::
     ( Carrier sig m
     , MonadIO m
     )
  => RenderCarrier m a
  -> m a
runRender (RenderCarrier action) = do
  doneVar :: TVar (Set (Double, Dependency)) <-
    liftIO (newTVarIO Set.empty)

  -- TODO bracket region

  doneRegion :: ConsoleRegion <-
    liftIO (openConsoleRegion Linear)

  liftIO $ setConsoleRegion doneRegion $ do
    done <- readTVar doneVar
    done
      & Set.toDescList
      & map pprDoneDependency
      & Text.unlines
      & pure

  result <-
    action
      & runReader doneVar
      & evalState HashMap.empty
      & evalState HashMap.empty

  liftIO (closeConsoleRegion doneRegion)

  pure result


--------------------------------------------------------------------------------
-- Colors
--------------------------------------------------------------------------------

blue :: Text -> Text
blue s =
  Text.pack (setSGRCode [SetColor Foreground Dull Blue]) <> s <> reset

brightBlack :: Text -> Text
brightBlack s =
  Text.pack (setSGRCode [SetColor Foreground Vivid Black]) <> s <> reset

brightWhite :: Text -> Text
brightWhite s =
  Text.pack (setSGRCode [SetColor Foreground Vivid White]) <> s <> reset

reset :: Text
reset = Text.pack (setSGRCode [])
