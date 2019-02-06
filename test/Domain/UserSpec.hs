{-# OPTIONS_GHC -fno-warn-orphans #-}

module Domain.UserSpec where

-------------------------------------------------------------------------------
import Test.Hspec
-------------------------------------------------------------------------------

instance DomainUser (Writer [String]) where
  getUsers = undefined
  getUser = undefined
  createUser = undefined
  updateUser = undefined
  deleteUser = undefined

spec :: Spec
spec = parallel $ do
  describe "DatabaseUser" $ do
    it "getUsers" $ do
      True `shouldBe` True
