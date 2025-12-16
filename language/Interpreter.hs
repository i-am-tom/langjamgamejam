{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

module Interpreter where

import AST
import Control.Applicative (Alternative (..), asum)
import Control.Monad (MonadPlus, guard, zipWithM_, (>=>))
import Control.Monad.Logic (LogicT, MonadLogic (..), observeAllT)
import Control.Monad.Primitive (PrimMonad (..))
import Control.Monad.Reader
import Control.Monad.ST (ST, runST)
import Control.Monad.State (MonadState (..), gets, modify, state)
import Data.Aeson qualified as JSON
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Monoid (Ap (Ap))
import Data.Primitive.MutVar (MutVar, atomicModifyMutVar', newMutVar)
import Data.Traversable (for)
import Data.Tuple (swap)
import GHC.Prim
import Propagator

type Env :: Type -> Type
type Env s = Map Variable (Ref s)

type State :: Type -> Type
data State s
  = State
  { _sEnvironment :: MutVar s (Env s),
    _sProgram :: Program
  }

type Interpret :: Type -> Type -> Type
newtype Interpret s x = Interpret {unInterpret :: State s -> LogicT (ST s) x}
  deriving
    (Functor, Applicative, Monad, Alternative, MonadPlus, MonadLogic)
    via ReaderT (State s) (LogicT (ST s))
  deriving (Semigroup, Monoid) via Ap (Interpret s) x

instance MonadReader Program (Interpret s) where
  ask :: Interpret s Program
  ask = Interpret \(State _ _sProgram) -> pure _sProgram

  local :: (Program -> Program) -> Interpret s a -> Interpret s a
  local f x = Interpret \(State _sEnvironment _sProgram) -> do
    unInterpret x (State _sEnvironment (f _sProgram))

instance MonadState (Env s) (Interpret s) where
  state :: (Env s -> (x, Env s)) -> Interpret s x
  state k = Interpret \(State environment _) -> lift do
    atomicModifyMutVar' environment (swap . k)

instance PrimMonad (Interpret s) where
  type PrimState (Interpret s) = s

  primitive :: (State# s -> (# State# s, x #)) -> Interpret s x
  primitive = Interpret . const . lift . primitive

query :: Program -> Fact -> [Solution]
query _sProgram fact = runST $ observeAllT do
  _sEnvironment <- lift (newMutVar mempty)

  let program :: Interpret s Solution
      program = do
        withFact fact

        get
          >>= traverse
            ( unsafeRead >=> \case
                Nothing -> empty
                Just xs -> pure xs
            )

  unInterpret program State {..}

observeAll :: Program -> (forall s. Interpret s x) -> [x]
observeAll _sProgram xs = runST $ observeAllT do
  _sEnvironment <- lift (newMutVar mempty)
  unInterpret xs State {..}

scope :: Interpret s a -> Interpret s a
scope x = Interpret \(State _ _sProgram) -> do
  _sEnvironment <- lift (newMutVar mempty)
  unInterpret x State {..}

register :: Variable -> Ref s -> Interpret s ()
register name cell = do
  gets (Map.lookup name) >>= \case
    Just cell' -> unify cell cell'
    Nothing -> modify (Map.insert name cell)

type Ref :: Type -> Type
type Ref s = Cell (Interpret s) JSON.Value

type Solution :: Type
type Solution = Map Variable JSON.Value

definition :: [Ref s] -> Definition -> Interpret s ()
definition arguments (Fact _ parameters :- statements) = do
  guard (length arguments == length parameters)

  zipWithM_ withArgument parameters arguments
  foldr withStatement mempty statements

withArgument :: Argument -> Ref s -> Interpret s ()
withArgument argument cell = do
  case argument of
    Value x -> write cell x
    Named v -> register v cell

withFact :: Fact -> Interpret s ()
withFact (Fact name parameters) = do
  arguments <- for parameters \parameter -> do
    cell <- fresh

    withArgument parameter cell
    pure cell

  scope do
    choice <- search name >>= asum . map pure
    definition arguments choice

withStatement :: Statement -> Interpret s () -> Interpret s ()
withStatement statement ks = do
  case statement of
    Cut -> once ks
    Search fact -> withFact fact *> ks
    Negation fact -> lnot (withFact fact) *> ks

search :: Identifier -> Interpret s [Definition]
search name = do
  definitions <- asks _pDefinitions

  let matches :: Definition -> Bool
      matches (Fact candidate _ :- _) = candidate == name

  pure (filter matches definitions)
