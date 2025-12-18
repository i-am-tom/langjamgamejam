{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Render where

import AST (Program)
import Brick hiding (getName)
import Brick.Widgets.Border (border)
import Brick.Widgets.Center
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (StateT (StateT))
import Data.Kind (Constraint, Type)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Traversable (for)
import Program (currentLocation, descriptionOf, inventory, isOpen, isWinState, keyFor, locationContents, locationOf, nameOf, neighbouringLocations, (??))
import State (State (..))

winWidget :: (Applicative m) => m (Widget ())
winWidget = pure $ center do
  vBox
    [ fill ' ',
      vBox
        [ hCenter $ bold $ str "🎉 Congratulations! 🎉",
          vLimit 1 (fill ' '),
          hCenter $ str "You've completed the game!"
        ],
      fill ' '
    ]

---

gameWidget :: (MonadReader Program m) => State -> m (Widget ())
gameWidget state = runSupplyT do
  isWinState >>= \case
    True -> winWidget
    False -> do
      leftPanel <- inventoryWidget
      rightPanel <- surroundingsWidget state

      pure $ hBox [hLimitPercent 30 leftPanel, rightPanel]

inventoryWidget :: (MonadReader Program m) => m (Widget ())
inventoryWidget = do
  items <- inventory >>= traverse (itemWidget False)

  pure $
    border do
      padLeftRight 1 do
        vBox
          [ bold (hCenter $ str "Inventory"),
            vLimit 1 (fill ' '),
            if null items
              then
                hCenter $ str "Nothing yet..."
              else
                vBox items,
            fill ' '
          ]

surroundingsWidget :: (MonadReader Program m, MonadSupply m) => State -> m (Widget ())
surroundingsWidget State {..} = do
  here <- currentLocation ?? "UNKNOWN LOCATION"

  name <- nameOf here ?? "UNKNOWN NAME"
  description <- descriptionOf here ?? "UNKNOWN DESCRIPTION"

  contents <- locationContents here
  connections <- liftA2 (<>) (neighbouringLocations here) (fmap maybeToList (locationOf here))
  key <- keyFor here

  let boundary :: Int
      boundary = length contents + length connections + length key

  let current :: Int
      current = (_sCursorIndex `mod` boundary + boundary) `mod` boundary

  contentList <- for contents \item ->
    fresh >>= \index -> itemWidget (current == index) item

  connectionList <- for connections \location ->
    fresh >>= \index -> itemWidget (current == index) location

  keyList <- case key of
    Just k' -> do
      isThisOpen <- isOpen here

      if isThisOpen
        then pure emptyWidget
        else do
          fresh >>= \index -> do
            inner <- itemWidget (current == index) k'
            pure $
              vBox
                [ txt "You can use the following keys:",
                  vLimit 1 (fill ' '),
                  padLeftRight 2 inner
                ]
    Nothing -> pure emptyWidget

  let title :: Widget ()
      title = do
        vBox
          [ hCenter $ txt "You have reached " <+> bold (txt name) <+> txt ".",
            vLimit 1 (fill ' '),
            italic $ txt $ capitalize description
          ]

      contentOptions :: Widget ()
      contentOptions =
        if null contentList
          then
            emptyWidget
          else
            vBox
              [ txt "In front of you, you can see:",
                vLimit 1 (fill ' '),
                padLeftRight 2 (vBox contentList)
              ]

      connectionOptions :: Widget ()
      connectionOptions =
        if null connectionList
          then
            emptyWidget
          else
            vBox
              [ txt "You can travel to:",
                vLimit 1 (fill ' '),
                padLeftRight 2 (vBox connectionList)
              ]

  pure $ border $ padLeftRight 1 do
    vBox
      [ title,
        vLimit 1 (fill ' '),
        contentOptions,
        vLimit 1 (fill ' '),
        connectionOptions,
        vLimit 1 (fill ' '),
        keyList,
        fill ' '
      ]

itemWidget :: (MonadReader Program m) => Bool -> Text -> m (Widget ())
itemWidget hasFocus item = do
  name <- nameOf item ?? "UNKNOWN NAME"
  description <- descriptionOf item ?? "UNKNOWN DESCRIPTION"

  let marker :: Text
      marker = if hasFocus then "> " else "- "

  pure $
    vBox
      [ txt marker <+> (withAttr (attrName "bold") $ txt $ capitalize name),
        padLeft (Pad 4) (withAttr (attrName "italic") $ txt $ capitalize description),
        vLimit 1 (fill ' ')
      ]

---

bold :: Widget () -> Widget ()
bold = withAttr (attrName "bold")

italic :: Widget () -> Widget ()
italic = withAttr (attrName "italic")

capitalize :: Text -> Text
capitalize xs = Text.toUpper (Text.take 1 xs) <> Text.drop 1 xs

---

type MonadSupply :: (Type -> Type) -> Constraint
class (Monad m) => MonadSupply m where
  fresh :: m Int

type SupplyT :: (Type -> Type) -> Type -> Type
newtype SupplyT m x = SupplyT {unSupplyT :: Int -> m (x, Int)}
  deriving (Functor, Applicative, Monad) via StateT Int m

deriving via
  StateT Int m
  instance
    (MonadReader r m) =>
    MonadReader r (SupplyT m)

instance (Monad m) => MonadSupply (SupplyT m) where
  fresh :: (Monad m) => SupplyT m Int
  fresh = SupplyT \n -> pure (n, n + 1)

runSupplyT :: (Monad m) => SupplyT m x -> m x
runSupplyT (SupplyT f) = f 0 >>= \(x, _) -> pure x