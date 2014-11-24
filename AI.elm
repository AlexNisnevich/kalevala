module AI where

import List
import Dict

import Helpers (..)
import GameTypes (..)
import Board (..)

import Debug

getMove : State -> Maybe Move
getMove state =
  if Dict.getOrFail (playerName state.turn) state.hands |> List.isEmpty
  then Nothing
  else
    let p = playerName state.turn
        hand = Dict.getOrFail p state.hands
        idxs = [0..(List.length hand)-1]

        boardSize = getBoardSize state
        xs = map (\x -> toFloat (x - (boardSize // 2))) [0..(boardSize - 1)]
        locations = concatMap (\x -> (map (\y -> (x, y)) xs)) xs

        pieceAtIdx i = pieceFromString (hand !! i)
        validLocationsByPiece piece = List.filter (\loc -> isValidMove { piece = piece, idx = 0, location = loc } state) locations
        validMoves = concatMap (\i -> let piece = pieceAtIdx i
                                          move loc = { piece = piece, idx = i, location = loc}
                                          stateAfterMoveTo loc = { state | board <- Dict.insert loc piece state.board }
                                          moveWithScore loc = (move loc, scoreMove (move loc) (stateAfterMoveTo loc))
                                      in
                                        map moveWithScore (validLocationsByPiece piece)) idxs
        (bestMove, _) = sortBy snd validMoves |> reverse |> Debug.watch "best moves" |> head
    in
      Just bestMove
