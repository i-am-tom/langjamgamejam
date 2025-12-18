{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Event where

import AST (Identifier (..), Program)
import Control.Lens (At (at), view, (&), (+~), (-~), (.~), (<>~))
import Data.Kind (Type)
import Data.Maybe (fromMaybe, maybeToList)
import Data.Text (Text)
import Program (canPickUp, currentLocation, keyFor, locationContents, locationOf, neighbouringLocations)
import State (State, sCurrentLocation, sCursorIndex, sEvents, sInventory)

type Event :: Type
data Event
  = NextSelection
  | PreviousSelection
  | Select

update :: Program -> Event -> State -> State
update program event state = case event of
  NextSelection ->
    state & sCursorIndex +~ 1
  PreviousSelection ->
    state & sCursorIndex -~ 1
  Select -> do
    let option :: Text
        option = everything !! (view sCursorIndex state `mod` length everything)
    if
      | key == [option] ->
          state & sEvents . at (Identifier here) <>~ Just ["is-open"]
      | canPickUp option program ->
          state & sInventory <>~ [Identifier option]
      | otherwise ->
          state & sCurrentLocation .~ Identifier option
  where
    here :: Text
    here = fromMaybe "UNKNOWN LOCATION" (currentLocation program)

    everything :: [Text]
    everything = contents ++ connections ++ key

    contents :: [Text]
    contents = locationContents here program

    connections :: [Text]
    connections =
      neighbouringLocations here program
        <> maybeToList (locationOf here program)

    key :: [Text]
    key = maybeToList (keyFor here program)
