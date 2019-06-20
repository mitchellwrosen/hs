{-# language UndecidableInstances #-}

module Hs.Eff.Register
  ( RegisterEffect
  , register
  , RegisterCarrier
  , runRegister
  ) where

import Control.Effect
import Control.Effect.Carrier
import Control.Effect.Sum
import Control.Concurrent.STM
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Trans.Resource.Internal (ResourceT(..))
import System.Console.Regions

import qualified Control.Monad.Trans.Resource as Resource


data RegisterEffect (m :: Type -> Type) (k :: Type) where
  Register
    :: IO a
    -> (a -> IO ())
    -> (a -> k)
    -> RegisterEffect m k
  deriving anyclass (Effect, HFunctor)

deriving stock instance Functor (RegisterEffect m)


register
  :: ( Carrier sig m
     , Member RegisterEffect sig
     )
  => IO a
  -> (a -> IO ())
  -> m a
register acquire release =
  send (Register acquire release pure)


newtype RegisterCarrier m a
  = RegisterCarrier { unRegisterCarrier :: ResourceT m a }
  deriving newtype (Applicative, Functor, Monad, MonadIO)

instance
     ( Carrier sig m
     , MonadIO m
     )
  => Carrier (RegisterEffect :+: sig) (RegisterCarrier m) where

  eff
    :: (RegisterEffect :+: sig) (RegisterCarrier m) (RegisterCarrier m a)
    -> RegisterCarrier m a
  eff = \case
    L (Register acquire release next) ->
      RegisterCarrier $ do
        (_key, x) <- Resource.allocate acquire release
        unRegisterCarrier (next x)

    R other ->
      RegisterCarrier $ ResourceT $ \releaseMap ->
        eff
          (handlePure
            (\m -> unResourceT (unRegisterCarrier m) releaseMap)
            other)

instance (LiftRegion m, Monad m) => LiftRegion (RegisterCarrier m) where
  liftRegion :: STM a -> RegisterCarrier m a
  liftRegion m =
    RegisterCarrier (lift (liftRegion m))

runRegister
  :: MonadUnliftIO m
  => RegisterCarrier m a
  -> m a
runRegister =
  runResourceT . unRegisterCarrier
