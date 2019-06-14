{-# LANGUAGE UndecidableInstances #-}

module Hs.Main.Build.Render
  ( RenderEff
  , runRender
  , renderBuildingComponent
  , renderBuildingDependency
  , renderCompilingModule
  , renderCompletedDependency
  , renderConfiguringComponent
  , renderDownloadingDependency
  , renderPreprocessingComponent
  , renderStderr
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
import System.Console.Concurrent
import System.Console.Regions

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set
import qualified Data.Text as Text


type Dependency
  = Either Package Component

data RenderEff (m :: Type -> Type) (k :: Type) where
  RenderBuildingComponent
    :: Component
    -> k
    -> RenderEff m k

  RenderBuildingDependency
    :: Dependency
    -> Word64
    -> k
    -> RenderEff m k

  RenderCompilingModule
    :: Int
    -> Int
    -> Text
    -> Bool
    -> k
    -> RenderEff m k

  RenderCompletedDependency
    :: Dependency
    -> Word64
    -> k
    -> RenderEff m k

  RenderConfiguringComponent
    :: Component
    -> k
    -> RenderEff m k

  RenderDownloadingDependency
    :: Dependency
    -> k
    -> RenderEff m k

  RenderPreprocessingComponent
    :: Component
    -> k
    -> RenderEff m k

  RenderStderr
    :: Text
    -> k
    -> RenderEff m k

  deriving stock (Functor)
  deriving anyclass (HFunctor)

renderBuildingComponent ::
     ( Carrier sig m
     , Member RenderEff sig
     )
  => Component
  -> m ()
renderBuildingComponent component =
  send (RenderBuildingComponent component (pure ()))

renderBuildingDependency ::
     ( Carrier sig m
     , Member RenderEff sig
     )
  => Dependency
  -> Word64
  -> m ()
renderBuildingDependency dep time =
  send (RenderBuildingDependency dep time (pure ()))

renderCompilingModule ::
     ( Carrier sig m
     , Member RenderEff sig
     )
  => Int
  -> Int
  -> Text
  -> Bool
  -> m ()
renderCompilingModule n m name isBoot =
  send (RenderCompilingModule n m name isBoot (pure ()))

renderCompletedDependency ::
     ( Carrier sig m
     , Member RenderEff sig
     )
  => Dependency
  -> Word64
  -> m ()
renderCompletedDependency dep time =
  send (RenderCompletedDependency dep time (pure ()))

renderConfiguringComponent ::
     ( Carrier sig m
     , Member RenderEff sig
     )
  => Component
  -> m ()
renderConfiguringComponent component =
  send (RenderConfiguringComponent component (pure ()))

renderDownloadingDependency ::
     ( Carrier sig m
     , Member RenderEff sig
     )
  => Dependency
  -> m ()
renderDownloadingDependency dep =
  send (RenderDownloadingDependency dep (pure ()))

renderPreprocessingComponent ::
     ( Carrier sig m
     , Member RenderEff sig
     )
  => Component
  -> m ()
renderPreprocessingComponent component =
  send (RenderPreprocessingComponent component (pure ()))

renderStderr ::
     ( Carrier sig m
     , Member RenderEff sig
     )
  => Text
  -> m ()
renderStderr line =
  send (RenderStderr line (pure ()))


newtype RenderCarrier m a
  = RenderCarrier
  { unRenderCarrier ::
       ReaderC LocalsConsoleRegion
      (ReaderC DepsConsoleRegion
      (ReaderC (TVar (Set (Double, Dependency)))
      (StateC (Maybe Component)
      (StateC (HashMap Component ConsoleRegion)
      (StateC (HashMap Dependency ConsoleRegion)
      (StateC (HashMap Dependency Word64) m)))))) a
  } deriving newtype (Applicative, Functor, Monad, MonadIO)

instance (Carrier sig m, Effect sig, MonadIO m)
      => Carrier (RenderEff :+: sig) (RenderCarrier m) where
  eff ::
       (RenderEff :+: sig) (RenderCarrier m) (RenderCarrier m a)
    -> RenderCarrier m a
  eff = \case
    L (RenderBuildingComponent _component next) ->
      next

    L (RenderBuildingDependency dep time next) ->
      RenderCarrier $ do
        regions :: HashMap Dependency ConsoleRegion <-
          get

        -- Dependency may already exist in map because it was downloaded first.
        when (isNothing (HashMap.lookup dep regions)) $
          renderNewDependency dep

        modify (HashMap.insert dep time)
        unRenderCarrier next

    L (RenderCompilingModule n m name isBoot next) ->
      RenderCarrier $ do
        mcomponent :: Maybe Component <-
          get

        for_ mcomponent $ \component -> do
          mregion :: Maybe ConsoleRegion <-
            gets (HashMap.lookup component)

          for_ mregion $ \region ->
            liftIO
              (setConsoleRegion
                region
                (fold
                  [ pprComponent component
                  , " "
                  , Text.pack (show n)
                  , "/"
                  , Text.pack (show m)
                  , " "
                  , pprModule name isBoot
                  , "\n"
                  ]))

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

    L (RenderConfiguringComponent component next) ->
      RenderCarrier $ do
        renderNewComponent component
        unRenderCarrier next

    L (RenderDownloadingDependency dep next) ->
      RenderCarrier $ do
        -- do
        --   mlastComponent :: Maybe Component <- get
        --   for_ mlastComponent $ \lastComponent -> do
        --     regions <- get
        --     for_ (HashMap.lookup lastComponent regions) $ \region ->
        --       liftIO (setConsoleRegion region (pprComponent lastComponent))
        renderNewDependency dep
        unRenderCarrier next

    L (RenderPreprocessingComponent component next) ->
      RenderCarrier $ do
        regions :: HashMap Component ConsoleRegion <-
          get

        when (isNothing (HashMap.lookup component regions)) $
          renderNewComponent component

        unRenderCarrier next

    L (RenderStderr line next) ->
      RenderCarrier $ do
        liftIO (outputConcurrent (line <> "\n"))
        unRenderCarrier next

    R other ->
      RenderCarrier (eff (R (R (R (R (R (R (R (handleCoercible other)))))))))

renderNewComponent ::
     ( Carrier sig m
     , Member (Reader LocalsConsoleRegion) sig
     , Member (State (Maybe Component)) sig
     , Member (State (HashMap Component ConsoleRegion)) sig
     , MonadIO m
     )
  => Component
  -> m ()
renderNewComponent component = do
  -- do
  --   mprevComponent :: Maybe Component <- get
  --   for_ mprevComponent $ \prevComponent -> do
  --     mprevRegion <- gets (HashMap.lookup prevComponent)
  --     for_ mprevRegion $ \prevRegion ->
  --       liftIO (setConsoleRegion prevRegion (pprComponent prevComponent))

  LocalsConsoleRegion localsRegion <- ask
  region <- liftIO (openConsoleRegion (InLine localsRegion))
  liftIO (setConsoleRegion region (pprComponent component <> "\n"))
  put (Just component)
  modify (HashMap.insert component region)

renderNewDependency ::
     ( Carrier sig m
     , Member (Reader DepsConsoleRegion) sig
     , Member (State (HashMap Dependency ConsoleRegion)) sig
     , MonadIO m
     )
  => Dependency
  -> m ()
renderNewDependency dep = do
  DepsConsoleRegion depsRegion <- ask
  region <- liftIO (openConsoleRegion (InLine depsRegion))
  liftIO (setConsoleRegion region (pprDependency dep <> "\n"))
  modify (HashMap.insert dep region)

pprDependency :: Dependency -> Text
pprDependency =
  either pprPackage pprComponent

pprPackage :: Package -> Text
pprPackage (Package name ver) =
  brightWhite name <> " " <> brightBlack ver

prPackage :: Package -> Text
prPackage (Package name ver) =
  name <> " " <> ver

pprComponent :: Component -> Text
pprComponent = \case
  Executable package exeName ->
    pprPackage package <> blue " executable " <> brightWhite exeName

  Library package@(Package pkgName _) name ->
    pprPackage package <>
      (if name /= pkgName then blue " library  " <> brightWhite name else mempty)

  TestSuite package exeName ->
    pprPackage package <> blue " test suite " <> brightWhite exeName

prComponent :: Component -> Text
prComponent = \case
  Executable package exeName ->
    prPackage package <> " executable " <> exeName

  Library package@(Package pkgName _) name ->
    prPackage package <>
      (if name /= pkgName then " library  " <> name else mempty)

  TestSuite package exeName ->
    prPackage package <> " test suite " <> exeName

pprDoneDependency :: (Double, Dependency) -> Text
pprDoneDependency (n, dep) =
  brightBlack
    (Text.pack (show (round n :: Int)) <> "s " <>
      either prPackage prComponent dep)

pprModule :: Text -> Bool -> Text
pprModule name isBoot =
  name <> (if isBoot then brightBlack " boot" else Text.empty)

newtype DepsConsoleRegion = DepsConsoleRegion ConsoleRegion
newtype LocalsConsoleRegion = LocalsConsoleRegion ConsoleRegion

runRender ::
     ( Carrier sig m
     , MonadIO m
     )
  => RenderCarrier m a
  -> m a
runRender (RenderCarrier action) = do
  doneDepsVar :: TVar (Set (Double, Dependency)) <-
    liftIO (newTVarIO Set.empty)

  doneDepsRegion :: ConsoleRegion <-
    liftIO (openConsoleRegion Linear)

  depsRegion :: ConsoleRegion <-
    liftIO (openConsoleRegion Linear)

  localsRegion :: ConsoleRegion <-
    liftIO (openConsoleRegion Linear)

  liftIO $ setConsoleRegion doneDepsRegion $ do
    done <- readTVar doneDepsVar
    done
      & Set.toDescList
      & map pprDoneDependency
      & Text.unlines
      & pure

  (regions, (mlastComponent, result)) <-
    action
      & runReader (LocalsConsoleRegion localsRegion)
      & runReader (DepsConsoleRegion depsRegion)
      & runReader doneDepsVar
      & runState Nothing
      & runState HashMap.empty
      & evalState HashMap.empty
      & evalState HashMap.empty

  for_ mlastComponent $ \lastComponent ->
    for_ (HashMap.lookup lastComponent regions) $ \region ->
      liftIO (setConsoleRegion region (pprComponent lastComponent <> "\n"))

  pure result


--------------------------------------------------------------------------------
-- Colors
--------------------------------------------------------------------------------

blue, brightBlack, brightWhite :: Text -> Text
(blue, brightBlack, brightWhite) = (id, id, id)
-- blue :: Text -> Text
-- blue s =
--   Text.pack (setSGRCode [SetColor Foreground Dull Blue]) <> s <> reset

-- brightBlack :: Text -> Text
-- brightBlack s =
--   Text.pack (setSGRCode [SetColor Foreground Vivid Black]) <> s <> reset

-- brightWhite :: Text -> Text
-- brightWhite s =
--   Text.pack (setSGRCode [SetColor Foreground Vivid White]) <> s <> reset

-- reset :: Text
-- reset = Text.pack (setSGRCode [])
