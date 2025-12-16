{-# LANGUAGE DerivingStrategies #-}

module AST where

import Data.Aeson (FromJSON, FromJSONKey)
import Data.Aeson qualified as JSON
import Data.Kind (Type)
import Data.Text (Text)

type Program :: Type
newtype Program = Program {_pDefinitions :: [Definition]}
  deriving newtype (Show, Eq, Ord, Semigroup, Monoid)

type Definition :: Type
data Definition
  = Definition
  { _dFact :: Fact,
    _dFrom :: [Statement]
  }
  deriving stock (Show, Eq, Ord)

type Statement :: Type
data Statement = Cut | Search Fact
  deriving stock (Show, Eq, Ord)

type Fact :: Type
data Fact = Fact
  { _fIdentifier :: Identifier,
    _fArguments :: [Argument]
  }
  deriving stock (Show, Eq, Ord)

type Argument :: Type
data Argument = Value JSON.Value | Named Variable
  deriving stock (Show, Eq, Ord)

type Identifier :: Type
newtype Identifier = Identifier Text
  deriving stock (Show, Eq, Ord)

type Variable :: Type
newtype Variable = Variable Text
  deriving stock (Show, Eq, Ord)
  deriving newtype (FromJSON, FromJSONKey)