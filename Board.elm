module Board where

import List
import Dict

import GameTypes (..)

data Direction = Vertical
               | Horizontal

getBoardSize : State -> Int
getBoardSize state =
  if List.isEmpty <| Dict.toList state.board
  then 5
  else
    let locations = Dict.keys state.board
        xs = map fst locations
        ys = map snd locations
        maxX = max (maximum xs) (abs <| minimum xs)
        maxY = max (maximum ys) (abs <| minimum ys)
        distFromCenter = (max maxX maxY) + 2
    in
      (distFromCenter * 2) + 1

findAbove : Location -> Board -> [Location]
findAbove (x,y) board =
  if Dict.member (x,y) board
  then [(x,y)] ++ findAbove (x,y-1) board
  else []

findBelow : Location -> Board -> [Location]
findBelow (x,y) board =
  if Dict.member (x,y) board
  then [(x,y)] ++ findBelow (x,y+1) board
  else []

findLeftward : Location -> Board -> [Location]
findLeftward (x,y) board =
  if Dict.member (x,y) board
  then [(x,y)] ++ findLeftward (x-1,y) board
  else []

findRightward : Location -> Board -> [Location]
findRightward (x,y) board =
  if Dict.member (x,y) board
  then [(x,y)] ++ findRightward (x+1,y) board
  else []

findColumn : Location -> Board -> [Location]
findColumn (x,y) board = (findAbove (x,y-1) board) ++ (findBelow (x,y+1) board)

findRow : Location -> Board -> [Location]
findRow (x,y) board = (findLeftward (x-1,y) board) ++ (findRightward (x+1,y) board)

isAdjacent : Location -> Location -> Bool
isAdjacent (x1, y1) (x2, y2) =
  (y1 == y2 && abs (x1 - x2) == 1) || (x1 == x2 && abs (y1 - y2) == 1)

adjacentTiles : Location -> Board -> [Location]
adjacentTiles (x, y) board =
  filter (\loc -> isAdjacent loc (x, y)) (Dict.keys board)

isValidMove : Move -> State -> Bool
isValidMove move state =
  let isUnoccupied = not <| Dict.member move.location state.board
      existingTile = Dict.get move.location state.board
      canOverlapExistingTile = (move.piece == Dragon || move.piece == Skadi)
                               && not (existingTile == Just move.piece)
                               -- can't Skadi a Skadi, can't Dragon a Dragon
      columnLength = length (findColumn move.location state.board) + 1
      rowLength = length (findRow move.location state.board) + 1
      longestLine = max columnLength rowLength
      adjacents = adjacentTiles move.location state.board
      hasAdjacentTile = not <| List.isEmpty adjacents
      adjacentToTroll = any (\loc -> Dict.getOrFail loc state.board == Troll) adjacents
  in
    (isUnoccupied || canOverlapExistingTile)
    && hasAdjacentTile
    && ((not adjacentToTroll) || move.piece == Troll) -- only Trolls can be placed next to other Trolls
    && longestLine <= 7

scoreMove : Move -> State -> Int
scoreMove move state =
  let column = findColumn move.location state.board
      columnSize = List.length column + 1
      columnScores = map (\loc -> getTileScore loc Vertical move state.board) column
      columnHighScore = if isEmpty column then 0 else maximum columnScores
      tileScoreInColumn = getTileScore move.location Vertical move state.board
      columnPoints = if (tileScoreInColumn > columnHighScore && columnSize >= 2) then columnSize else 0

      row = findRow move.location state.board
      rowSize = List.length row + 1
      rowScores = map (\loc -> getTileScore loc Horizontal move state.board) row
      rowHighScore = if isEmpty row then 0 else maximum rowScores
      tileScoreInRow = getTileScore move.location Horizontal move state.board
      rowPoints = if (tileScoreInRow > rowHighScore && rowSize >= 2) then rowSize else 0
  in
    columnPoints + rowPoints

getTileScore : Location -> Direction -> Move -> Board -> Int
getTileScore (x,y) dir move board =
  let piece = Dict.getOrFail (x,y) board
      adjacents = adjacentTiles (x,y) board
      adjacentToLoki = any (\loc -> Dict.getOrFail loc board == Loki) adjacents
      isCurrentTile = (move.location == (x,y)) -- is this the tile that was placed this turn?
  in
    if adjacentToLoki && not (piece == Loki)
    then 0 -- Loki makes all tiles around him 0 (except other Lokis)
    else
      case piece of
        Odin -> 8
        Thor -> 7
        Troll -> 6
        Dragon -> 5
        Fenrir -> let line = (case dir of Horizontal -> findRow
                                          Vertical -> findColumn) (x,y) board
                      piecesInLine = map (\loc -> Dict.getOrFail loc board) line
                      numOtherFenrirs = length <| filter (\p -> p == Fenrir) piecesInLine
                      numFenrirsToCount = numOtherFenrirs + if (isCurrentTile || not (move.piece == Fenrir)) then 1 else 0
                      -- count the tile itself, but don't count Fenrir placed this turn for other Fenrirs
                      -- (this is so that Fenrirs can beat other Fenrirs)
                  in
                    4 * numFenrirsToCount
                  -- TODO: Fenrir-Loki interaction is actually quite tricky.
                  -- "Fenrir tiles next to Loki tiles are worth zero and do not contribute to the value of other Fenrir tiles."
        Skadi -> 3
        Valkyrie -> if isCurrentTile && hasSamePieceAtOtherEnd (x,y) board dir
                    then 100 -- i.e. instantly score line
                    else 2
                  -- TODO: Loki shouldn't prevent Valkyrie from auto-scoring a line
        Loki -> 1

-- is this piece at one end of a line with the same kind of piece
-- at the other end? (used by Valkyrie)
hasSamePieceAtOtherEnd : Location -> Board -> Direction -> Bool
hasSamePieceAtOtherEnd (x,y) board dir =
  let pieceAt pos = Dict.getOrFail pos board
      samePieces pos1 pos2 = pieceAt pos1 == pieceAt pos2
      above = findAbove (x,y-1) board
      below = findBelow (x,y+1) board
      left = findLeftward (x-1,y) board
      right = findRightward (x+1,y) board
      samePieceBelow = isEmpty above && not (isEmpty below) && samePieces (last below) (x,y)
      samePieceAbove = isEmpty below && not (isEmpty above) && samePieces (last above) (x,y)
      samePieceLeft = isEmpty right && not (isEmpty left) && samePieces (last left) (x,y)
      samePieceRight = isEmpty left && not (isEmpty right) && samePieces (last right) (x,y)
  in
    case dir of
      Horizontal -> samePieceLeft || samePieceRight
      Vertical -> samePieceBelow || samePieceAbove

