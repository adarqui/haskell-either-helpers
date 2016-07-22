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
  assertBoolT
) where



import           Control.Monad.Trans        (lift)
import           Control.Monad.Trans.Either (EitherT)
import qualified Control.Monad.Trans.Either as EitherT
import           Data.Either
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
leftT e = EitherT.left e



rightT :: forall a (m :: * -> *) e. Monad m => a -> EitherT e m a
rightT = EitherT.right



assertTrueT :: forall (m :: * -> *). Monad m => m Bool -> EitherT () m Bool
assertTrueT go = assertBoolT True go



assertFalseT :: forall (m :: * -> *). Monad m => m Bool -> EitherT () m Bool
assertFalseT go = assertBoolT False go



assertBoolT :: forall (m :: * -> *). Monad m => Bool -> m Bool -> EitherT () m Bool
assertBoolT bool go = do
  result <- lift go
  if result == bool
    then rightT True
    else leftT ()
