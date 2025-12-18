{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module State where

import AST (Argument (..), Definition (..), Fact (..), Identifier (..), Program (..))
import Control.Lens (makeLenses)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Writer.CPS (MonadWriter (..), Writer, execWriter)
import Data.Foldable (for_)
import Data.Functor (void)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Program (currentLocation, (??))

type State :: Type
data State = State
  { _sCursorIndex :: Int,
    _sCurrentLocation :: Identifier,
    _sEvents :: Map Identifier [Identifier],
    _sInventory :: [Identifier]
  }
  deriving stock (Eq, Ord, Show)

initialState :: (MonadReader Program m) => m State
initialState = do
  let _sCursorIndex :: Int
      _sCursorIndex = 0

  let _sEvents :: Map Identifier [Identifier]
      _sEvents = Map.empty

  let _sInventory :: [Identifier]
      _sInventory = mempty

  _sCurrentLocation <- fmap Identifier do
    currentLocation ?? "UNKNOWN LOCATION"

  pure State {..}

makeLenses ''State

compile :: State -> Program
compile State {..} = execWriter do
  let declare :: Identifier -> [Argument] -> Writer Program ()
      declare item args = tell [Fact item args :- []]

  declare "current-location" [Value (unIdentifier _sCurrentLocation)]

  for_ _sInventory \item -> declare "has-item" [Value (unIdentifier item)]

  forWithKey_ _sEvents \item events ->
    for_ events \event ->
      declare
        "state"
        [ Value (unIdentifier item),
          Value (unIdentifier event)
        ]

forWithKey_ :: (Applicative f) => Map k v -> (k -> v -> f v') -> f ()
forWithKey_ xs f = void (Map.traverseWithKey f xs)