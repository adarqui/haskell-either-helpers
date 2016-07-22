{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE KindSignatures #-}

module Haskell.Helpers.Either (
  left,
  right,
  leftF,
  rightF,
  leftT,
  rightT,
  assertTrueT,
  assertFalseT,
  assertBoolT,
  assertRightT,
  assertT,
  mustPassT,
  assertRetryT
) where



import           Control.Monad.Trans        (lift)
import           Control.Monad.Trans.Either (EitherT)
import qualified Control.Monad.Trans.Either as EitherT
import           Data.Either                (Either (..))
import           Prelude                    hiding (Either)



left :: forall a e. e -> Either e a
left = Left



right :: forall a e. a -> Either e a
right = Right



leftF :: forall (f :: * -> *) a e. Applicative f => e -> f (Either e a)
leftF  = pure . Left



rightF :: forall (f :: * -> *) a e. Applicative f => a -> f (Either e a)
rightF = pure . Right



leftT :: forall (m :: * -> *) a e. Monad m => e -> EitherT e m a
leftT = EitherT.left



rightT :: forall a (m :: * -> *) e. Monad m => a -> EitherT e m a
rightT = EitherT.right



assertTrueT :: forall (m :: * -> *). Monad m => m Bool -> EitherT () m Bool
assertTrueT = assertBoolT True



assertFalseT :: forall (m :: * -> *). Monad m => m Bool -> EitherT () m Bool
assertFalseT = assertBoolT False



assertBoolT :: forall (m :: * -> *). Monad m => Bool -> m Bool -> EitherT () m Bool
assertBoolT bool go = do
  result <- lift go
  if result == bool
    then rightT True
    else leftT ()



assertRightT :: forall (m :: * -> *) a e. Monad m => m (Either e a) -> EitherT e m a
assertRightT go = do
  result <- lift go
  case result of
    Left e  -> leftT e
    Right a -> rightT a



assertT :: forall (m :: * -> *) a e. Monad m => (Either e a -> Bool) -> m (Either e a) -> EitherT () m a
assertT test go = do
  lr <- lift go
  case lr of
    Left _  -> if test lr then rightT undefined else leftT ()
    Right r -> if test lr then rightT r else leftT ()



mustPassT :: forall (m :: * -> *) a e. Monad m => m (Either e a) -> EitherT () m a
mustPassT go = do
  result <- lift go
  case result of
    Left _  -> leftT ()
    Right a -> rightT a



-- | Retry `retries` times until a success
--
assertRetryT
  :: forall (m :: * -> *) a e. Monad m
  => Int
  -> (Either e a -> Bool)
  -> m (Either e a)
  -> EitherT () m a
assertRetryT retries test go = do

  lr <- lift $ EitherT.runEitherT $ assertT test go

  case lr of
    Left _  ->
      if retries == 0
        then leftT ()
        else assertRetryT (retries-1) test go
    Right v -> rightT v
