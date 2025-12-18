{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Program where

import AST (Argument (..), Fact (..), Program)
import Control.Applicative (asum)
import Control.Monad.Reader (MonadReader (..))
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import Interpreter (Solution, query)

isWinState :: (MonadReader Program m) => m Bool
isWinState = do
  program <- ask

  let solutions :: [Solution]
      solutions = query program (Fact "is-win-state" [])

  pure $ not (null solutions)

---

locationContents :: (MonadReader Program m) => Text -> m [Text]
locationContents location = do
  program <- ask

  let argument :: Argument
      argument = Value location

  let solutions :: [Solution]
      solutions = query program (Fact "contains" [argument, Named "X"])

  pure $ mapMaybe (Map.lookup "X") solutions

locationOf :: (MonadReader Program m) => Text -> m (Maybe Text)
locationOf item = do
  program <- ask

  let argument :: Argument
      argument = Value item

  let solutions :: [Solution]
      solutions = query program (Fact "contains" [Named "X", argument])

  pure $ listToMaybe solutions >>= Map.lookup "X"

neighbouringLocations :: (MonadReader Program m) => Text -> m [Text]
neighbouringLocations location = do
  program <- ask

  let argument :: Argument
      argument = Value location

  let solutions :: [Solution]
      solutions = query program (Fact "can-travel-to" [argument, Named "X"])

  pure $ mapMaybe (Map.lookup "X") solutions

---

inventory :: (MonadReader Program m) => m [Text]
inventory = do
  program <- ask

  let solutions :: [Solution]
      solutions = query program (Fact "has-item" [Named "X"])

  pure $ mapMaybe (Map.lookup "X") solutions

opens :: (MonadReader Program m) => Text -> Text -> m Bool
opens door key = do
  program <- ask

  let solutions :: [Solution]
      solutions = query program (Fact "can-open-with" [Value door, Value key])

  pure $ not (null solutions)

isOpen :: (MonadReader Program m) => Text -> m Bool
isOpen door = do
  program <- ask

  let solutions :: [Solution]
      solutions = query program (Fact "state" [Value door, Value "is-open"])

  pure $ not (null solutions)

keyFor :: (MonadReader Program m) => Text -> m (Maybe Text)
keyFor door = do
  program <- ask

  let argument :: Argument
      argument = Value door

  let solutions :: [Solution]
      solutions = query program (Fact "can-open-with" [argument, Named "X"])

  pure $ listToMaybe solutions >>= Map.lookup "X"

canPickUp :: (MonadReader Program m) => Text -> m Bool
canPickUp item = do
  program <- ask

  let argument :: Argument
      argument = Value item

  let solutions :: [Solution]
      solutions = query program (Fact "can-pick-up" [argument])

  pure $ not (null solutions)

---

startLocation :: (MonadReader Program m) => m (Maybe Text)
startLocation = do
  program <- ask

  let solutions :: [Solution]
      solutions = query program (Fact "is-start-location" [Named "X"])

  pure $ listToMaybe solutions >>= Map.lookup "X"

currentLocation :: (MonadReader Program m) => m (Maybe Text)
currentLocation = do
  program <- ask

  let solutions :: [Solution]
      solutions = query program (Fact "current-location" [Named "X"])

  fallback <- startLocation
  pure $ asum [listToMaybe solutions >>= Map.lookup "X", fallback]

---

nameOf :: (MonadReader Program m) => Text -> m (Maybe Text)
nameOf location = do
  program <- ask

  let argument :: Argument
      argument = Value location

  let solutions :: [Solution]
      solutions = query program (Fact "has-name" [argument, Named "X"])

  pure $ listToMaybe solutions >>= Map.lookup "X"

descriptionOf :: (MonadReader Program m) => Text -> m (Maybe Text)
descriptionOf location = do
  program <- ask

  let argument :: Argument
      argument = Value location

  let solutions :: [Solution]
      solutions = query program (Fact "has-description" [argument, Named "X"])

  pure $ listToMaybe solutions >>= Map.lookup "X"

---

(??) :: (Functor m) => m (Maybe x) -> x -> m x
(??) = flip (fmap . fromMaybe)

infixl 4 ??
