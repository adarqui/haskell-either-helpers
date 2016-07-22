{-# LANGUAGE OverloadedStrings #-}

module Haskell.Helpers.EitherSpec (
  main,
  spec
) where



import           Control.Monad.Identity
import           Control.Monad.Trans.Either (runEitherT)
import           Haskell.Helpers.Either
import           Test.Hspec



main :: IO ()
main = hspec spec



spec :: Spec
spec = do

  describe "either" $ do
    it "either" $ do

      True `shouldBe` True
      (left False :: Either Bool ())             `shouldBe` Left False
      (right () :: Either Bool ())             `shouldBe` Right ()
      (leftF False :: Identity (Either Bool ())) `shouldBe` (pure $ Left False)
      (rightF () :: Identity (Either Bool ())) `shouldBe` (pure $ Right ())

      (runEitherT $ do
        void $ leftT False
        pure ()) `shouldReturn` (Left False)

      ((runEitherT $ do
        rightT ()) :: IO (Either Bool ())) `shouldReturn` (Right ())

      -- (runEitherT $ do
      --   void $ leftT (leftF False :: Identity (Either Bool ()))
      --   pure ()) `shouldBe` (Left False)
