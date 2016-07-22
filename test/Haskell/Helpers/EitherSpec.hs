{-# LANGUAGE OverloadedStrings #-}

module Haskell.Helpers.EitherSpec (
  main,
  spec
) where



-- import           Haskell.Helpers.Either
import           Test.Hspec



main :: IO ()
main = hspec spec



spec :: Spec
spec = do

  describe "either" $ do
    it "either" $ do
      True `shouldBe` True
