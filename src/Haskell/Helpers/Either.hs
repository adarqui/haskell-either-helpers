{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE KindSignatures #-}

module Haskell.Helpers.Either (
  left,
  right,
  leftF,
  rightF,
  leftT,
  rightT
) where



import           Control.Monad.Trans.Either (EitherT)
import qualified Control.Monad.Trans.Either as Either
import           Data.Either



left :: forall a e. e -> Either e a
left = Left



right :: forall a e. a -> Either e a
right = Right



leftF :: forall (f :: * -> *) a e. Applicative f => e -> f (Either e a)
leftF  = pure . Left



rightF :: forall (f :: * -> *) a e. Applicative f => a -> f (Either e a)
rightF = pure . Right




leftT :: forall (m :: * -> *) a e. Monad m => e -> Either.EitherT e m a
leftT e = Either.left e



rightT :: forall a (m :: * -> *) e. Monad m => a -> Either.EitherT e m a
rightT = Either.right
