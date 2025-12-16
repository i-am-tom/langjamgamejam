{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module PropagatorSpec where

import Control.Applicative (Alternative (..), asum)
import Control.Monad (MonadPlus)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Logic (LogicT, observeAllT)
import Control.Monad.Primitive (PrimMonad (..), PrimState, primitive)
import Control.Monad.Trans.Class (lift)
import Data.Functor (($>))
import Data.Kind (Type)
import GHC.Prim (State#)
import Propagator
import Test.Hspec

type TestT :: Type -> Type
newtype TestT a = TestT {unTestT :: LogicT IO a}
  deriving newtype (Functor, Applicative, Monad, Alternative, MonadPlus, MonadIO)

instance PrimMonad TestT where
  type PrimState TestT = PrimState IO

  primitive :: (State# (PrimState TestT) -> (# State# (PrimState TestT), x #)) -> TestT x
  primitive = TestT . lift . primitive

runTestT :: TestT x -> IO [x]
runTestT = observeAllT . unTestT

spec_propagator :: Spec
spec_propagator = do
  describe "Propagator cells" do
    it "fresh write -> read" do
      results <- runTestT do
        x <- fresh @Int
        write x 2
        unsafeRead x

      results `shouldBe` [Just 2]

    it "conflicting unify before" do
      results <- runTestT do
        x <- fresh @Int
        y <- fresh @Int
        unify x y

        write x 1
        write y 2

      results `shouldBe` []

    it "conflicting unify after" do
      results <- runTestT do
        x <- fresh @Int
        y <- fresh @Int

        write x 1
        write y 2

        unify x y

      results `shouldBe` []

    it "left -> right unification" do
      results <- runTestT do
        x <- fresh @Int
        y <- fresh @Int

        unify x y
        write x 5

        unsafeRead y

      results `shouldBe` [Just 5]

    it "right -> left unification" do
      results <- runTestT do
        x <- fresh @Int
        y <- fresh @Int

        unify x y
        write y 7

        unsafeRead x

      results `shouldBe` [Just 7]

    it "branching" do
      results <- runTestT do
        x <- fresh @Int

        asum
          [ write x 10 $> "branch-10",
            write x 20 $> "branch-20"
          ]

      results `shouldBe` ["branch-10", "branch-20"]

    it "branching with left failure" do
      results <- runTestT do
        x <- fresh @Int

        asum
          [ write x 10 $> "branch-10" <* write x 0,
            write x 20 $> "branch-20"
          ]

      results `shouldBe` ["branch-20"]

    it "branching with right failure" do
      results <- runTestT do
        x <- fresh @Int

        asum
          [ write x 10 $> "branch-10",
            write x 20 $> "branch-20" <* write x 0
          ]

      results `shouldBe` ["branch-10"]
