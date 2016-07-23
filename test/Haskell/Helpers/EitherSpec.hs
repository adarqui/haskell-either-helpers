{-# LANGUAGE OverloadedStrings #-}

module Haskell.Helpers.EitherSpec (
  main,
  spec
) where



import           Control.Concurrent.MVar    (modifyMVar_, newMVar, readMVar)
import           Control.Monad              (void)
import           Control.Monad.Identity     (Identity)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Either (EitherT, runEitherT)
import           Data.Either                (isRight)
import           Haskell.Helpers.Either
import           Test.Hspec



main :: IO ()
main = hspec spec



spec :: Spec
spec =

  describe "either" $
    it "either" $ do

      True `shouldBe` True
      (left False :: Either Bool ())             `shouldBe` Left False
      (right () :: Either Bool ())               `shouldBe` Right ()
      (leftA False :: Identity (Either Bool ())) `shouldBe` pure (Left False)
      (rightA () :: Identity (Either Bool ()))   `shouldBe` pure (Right ())

      runEitherT (do
        void $ leftT False
        pure ()) `shouldReturn` Left False

      ((runEitherT $
        rightT ()) :: IO (Either Bool ())) `shouldReturn` Right ()

      runEitherT
        (assertTrueT $ pure True) `shouldReturn` Right True

      runEitherT (do
        x <- assertTrueT $ pure False
        y <- assertTrueT $ pure True
        pure (x, y)) `shouldReturn` Left ()

      runEitherT (do
        x <- assertTrueT $ pure True
        y <- assertTrueT $ pure True
        pure (x, y)) `shouldReturn` Right (True, True)


      runEitherT (do
        y <- (assertRightT $ rightA True) :: EitherT () IO Bool
        pure y) `shouldReturn` Right True

      runEitherT (do
        a <- mustPassT $ rightA 'a'
        b <- mustPassT $ rightA 'b'
        c  <- mustPassT $ rightA 'c'
        pure [a,b,c]) `shouldReturn` Right "abc"

      runEitherT (do
        a2 <- mustPassT $ rightA 'a'
        b2 <- mustPassT $ rightA 'b'
        c2 <- mustPassT $ rightA 'c'
        _  <- mustPassT $ leftA ()
        pure [a2,b2,c2]) `shouldReturn` Left ()

      runEitherT (do
        a3 <- mustPassT $ rightA 'a'
        b3 <- mustPassT $ rightA 'b'
        _  <- mustPassT $ leftA ()
        c3 <- mustPassT $ rightA 'c'
        pure [a3,b3,c3]) `shouldReturn` Left ()

      runEitherT (do
        a <- mustT $ rightA 'a'
        b <- mustT $ rightA 'b'
        c <- mustT $ rightA 'c'
        pure [a,b,c]) `shouldReturn` Right "abc"

      runEitherT (do
        a2 <- mustT $ rightA 'a'
        b2 <- mustT $ rightA 'b'
        c2 <- mustT $ rightA 'c'
        _  <- mustT $ leftA ()
        pure [a2,b2,c2]) `shouldReturn` Left ()

      runEitherT (do
        a3 <- mustT $ rightA 'a'
        b3 <- mustT $ rightA 'b'
        _  <- mustT $ leftA ()
        c3 <- mustT $ rightA 'c'
        pure [a3,b3,c3]) `shouldReturn` Left ()

      runEitherT
        (assertRetryT 5 isRight (leftA () :: IO (Either () Bool))) `shouldReturn` Left ()

      runEitherT (do
        mvar <- liftIO $ newMVar (4 :: Int)
        assertRetryT 5 isRight $ do
          n <- liftIO $ readMVar mvar
          if n == 1
            then rightA n
            else liftIO $ modifyMVar_ mvar (pure . pred) *> leftA n
          ) `shouldReturn` Right 1

      runEitherT (do
        mvar <- liftIO $ newMVar (7 :: Int)
        assertRetryT 5 isRight $ do
          n <- liftIO $ readMVar mvar
          if n == 1
            then rightA n
            else liftIO $ modifyMVar_ mvar (pure . pred) *> leftA ()
          ) `shouldReturn` Left ()

      choiceEitherM' () [(leftA ()) :: IO (Either () Int), leftA ()] `shouldReturn` Left ()

      choiceEitherM' () [leftA (), rightA 'x'] `shouldReturn` Right 'x'
