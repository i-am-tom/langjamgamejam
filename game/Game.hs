{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Game where

import AST (Program)
import Brick
import Control.Lens ((+~), (-~))
import Event (Event (..), update)
import Graphics.Vty (Event (EvKey), Key (..))
import Graphics.Vty.Attributes (bold, defAttr, italic, withStyle)
import Parser (program)
import Render (gameWidget)
import State (State (..), compile, initialState, sCursorIndex)
import System.FilePath ((</>))
import Text.Parsec (parse)

game :: IO State
game = do
  parsed <- loadGame

  let app :: App State () ()
      app =
        App
          { appDraw = \state -> [gameWidget state (compile state <> parsed)],
            appChooseCursor = showFirstCursor,
            appHandleEvent = handleEvent parsed,
            appStartEvent = pure (),
            appAttrMap = \_ ->
              attrMap
                defAttr
                [ (attrName "bold", defAttr `withStyle` bold),
                  (attrName "italic", defAttr `withStyle` italic)
                ]
          }

  defaultMain app (initialState parsed)

handleEvent :: Program -> BrickEvent () () -> EventM () State ()
handleEvent original = \case
  VtyEvent (EvKey key []) ->
    case key of
      KEsc -> halt
      KChar 'q' -> halt
      KUp ->
        modify (sCursorIndex -~ 1)
      KDown ->
        modify (sCursorIndex +~ 1)
      KEnter -> do
        loaded <- gets (mappend original . compile)
        modify (update loaded Select)
      _ -> return ()
  _ -> return ()

---

loadGame :: IO Program
loadGame = do
  let path :: FilePath
      path = "game" </> "source.prolog"

  content <- readFile path

  case parse program path content of
    Left message -> error ("parser: " ++ show message)
    Right source -> pure source