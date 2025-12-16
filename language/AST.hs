{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}

module AST where

import Data.Aeson (FromJSON, FromJSONKey)
import Data.Aeson qualified as JSON
import Data.Kind (Type)
import Data.String (IsString)
import Data.Text (Text)
import GHC.Exts (IsList)

type Program :: Type
newtype Program = Program {_pDefinitions :: [Definition]}
  deriving newtype (Show, Eq, Ord, Semigroup, Monoid)
  deriving newtype (IsList)

type Definition :: Type
data Definition = Fact :- [Statement]
  deriving stock (Show, Eq, Ord)

type Statement :: Type
data Statement = Cut | Search Fact | Negation Fact
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
  deriving newtype (IsString)

type Variable :: Type
newtype Variable = Variable Text
  deriving stock (Show, Eq, Ord)
  deriving newtype (FromJSON, FromJSONKey, IsString)