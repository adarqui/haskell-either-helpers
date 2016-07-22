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
      (leftF False :: Identity (Either Bool ())) `shouldBe` pure (Left False)
      (rightF () :: Identity (Either Bool ()))   `shouldBe` pure (Right ())

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
        y <- (assertRightT $ rightF True) :: EitherT () IO Bool
        pure y) `shouldReturn` Right True

      runEitherT (do
        a <- mustPassT $ rightF 'a'
        b <- mustPassT $ rightF 'b'
        c <- mustPassT $ rightF 'c'
        pure [a,b,c]) `shouldReturn` Right "abc"

      runEitherT (do
        a2 <- mustPassT $ rightF 'a'
        b2 <- mustPassT $ rightF 'b'
        c2 <- mustPassT $ rightF 'c'
        _ <- mustPassT $ leftF ()
        pure [a2,b2,c2]) `shouldReturn` Left ()

      runEitherT (do
        a3 <- mustPassT $ rightF 'a'
        b3 <- mustPassT $ rightF 'b'
        _ <- mustPassT $ leftF ()
        c3 <- mustPassT $ rightF 'c'
        pure [a3,b3,c3]) `shouldReturn` Left ()

      runEitherT
        (assertRetryT 5 isRight (leftF () :: IO (Either () Bool))) `shouldReturn` Left ()

      runEitherT (do
        mvar <- liftIO $ newMVar (4 :: Int)
        assertRetryT 5 isRight $ do
          n <- liftIO $ readMVar mvar
          if n == 1
            then rightF n
            else liftIO $ modifyMVar_ mvar (pure . pred) *> leftF n
          ) `shouldReturn` Right 1

      runEitherT (do
        mvar <- liftIO $ newMVar (7 :: Int)
        assertRetryT 5 isRight $ do
          n <- liftIO $ readMVar mvar
          if n == 1
            then rightF n
            else liftIO $ modifyMVar_ mvar (pure . pred) *> leftF ()
          ) `shouldReturn` Left ()
