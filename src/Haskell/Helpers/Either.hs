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
  mustPassT
) where



import           Control.Monad.Trans        (lift)
import           Control.Monad.Trans.Either (EitherT)
import qualified Control.Monad.Trans.Either as EitherT
import           Data.Either
import Data.Either.Combinators (fromRight', fromLeft')
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



assertRightT :: forall (m :: * -> *) a e. Monad m => m (Either e a) -> EitherT e m a
assertRightT go = do
  result <- lift go
  case result of
    Left e  -> leftT e
    Right a -> rightT a



assertT :: forall (m :: * -> *) a e. Monad m => (Either e a -> Bool) -> m (Either e a) -> EitherT e m a
assertT test go = do
  result <- lift go
  if test result
    then rightT $ fromRight' result
    else leftT $ fromLeft' result



mustPassT :: forall (m :: * -> *) a e. Monad m => m (Either e a) -> EitherT () m a
mustPassT go = do
  result <- lift go
  case result of
    Left _  -> leftT ()
    Right a -> rightT a
