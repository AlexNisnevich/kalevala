module Board where

import List
import List (..)
import Maybe (Maybe (..), withDefault)
import Dict

import GameTypes (..)
import State
import Piece
import Player

type Direction = Vertical
               | Horizontal

getBoardSize : Board -> Int
getBoardSize board =
  if List.isEmpty <| Dict.toList board
  then 5
  else
    let locations = Dict.keys board
        xs = map fst locations
        ys = map snd locations
        maxX = max (maximum xs) (abs <| minimum xs)
        maxY = max (maximum ys) (abs <| minimum ys)
        distFromCenter = (max maxX maxY) + 2
    in
      (distFromCenter * 2) + 1

pieceAt : Location -> Board -> Piece
pieceAt loc board = withDefault NoPiece (Dict.get loc board)

findAbove : Location -> Board -> List Location
findAbove (x,y) board =
  if Dict.member (x,y) board
  then [(x,y)] ++ findAbove (x,y-1) board
  else []

findBelow : Location -> Board -> List Location
findBelow (x,y) board =
  if Dict.member (x,y) board
  then [(x,y)] ++ findBelow (x,y+1) board
  else []

findLeftward : Location -> Board -> List Location
findLeftward (x,y) board =
  if Dict.member (x,y) board
  then [(x,y)] ++ findLeftward (x-1,y) board
  else []

findRightward : Location -> Board -> List Location
findRightward (x,y) board =
  if Dict.member (x,y) board
  then [(x,y)] ++ findRightward (x+1,y) board
  else []

findColumn : Location -> Board -> List Location
findColumn (x,y) board = (findAbove (x,y-1) board) ++ (findBelow (x,y+1) board)

findRow : Location -> Board -> List Location
findRow (x,y) board = (findLeftward (x-1,y) board) ++ (findRightward (x+1,y) board)

isAdjacent : Location -> Location -> Bool
isAdjacent (x1, y1) (x2, y2) =
  (y1 == y2 && abs (x1 - x2) == 1) || (x1 == x2 && abs (y1 - y2) == 1)

adjacentTiles : Location -> Board -> List Location
adjacentTiles (x, y) board =
  filter (\loc -> isAdjacent loc (x, y)) (Dict.keys board)

isValidMove : Move -> Board -> Bool
isValidMove move board =
  let isUnoccupied = not <| Dict.member move.location board
      existingTile = Dict.get move.location board
      canOverlapExistingTile = (move.piece == Kaarme || move.piece == SeppoIlmarinen)
                               && not (existingTile == Just move.piece)
                               -- can't SeppoIlmarinen a SeppoIlmarinen, can't Kaarme a Kaarme
      columnLength = length (findColumn move.location board) + 1
      rowLength = length (findRow move.location board) + 1
      longestLine = max columnLength rowLength
      adjacents = adjacentTiles move.location board
      hasAdjacentTile = not <| List.isEmpty adjacents
      adjacentToKullervo = any (\loc -> pieceAt loc board == Kullervo) adjacents
  in
    (isUnoccupied || canOverlapExistingTile)
    && hasAdjacentTile
    && ((not adjacentToKullervo) || move.piece == Kullervo) -- only Kullervos can be placed next to other Kullervos
    && longestLine <= 7

scoreMove : Move -> Board -> Int
scoreMove move board =
  let column = findColumn move.location board
      columnSize = List.length column + 1
      columnScores = map (\loc -> getTileValue loc Vertical move board) column
      columnHighScore = if isEmpty column then 0 else maximum columnScores
      tileScoreInColumn = getTileValue move.location Vertical move board
      columnPoints = if (tileScoreInColumn > columnHighScore && columnSize >= 2) then columnSize else 0

      row = findRow move.location board
      rowSize = List.length row + 1
      rowScores = map (\loc -> getTileValue loc Horizontal move board) row
      rowHighScore = if isEmpty row then 0 else maximum rowScores
      tileScoreInRow = getTileValue move.location Horizontal move board
      rowPoints = if (tileScoreInRow > rowHighScore && rowSize >= 2) then rowSize else 0
  in
    columnPoints + rowPoints

getTileValue : Location -> Direction -> Move -> Board -> Int
getTileValue (x,y) dir move board =
  let piece = pieceAt (x,y) board
      adjacentToLemminkainen loc = any (\l -> pieceAt l board == Lemminkainen) <| adjacentTiles loc board
      isCurrentTile = (move.location == (x,y)) -- is this the tile that was placed this turn?
  in
    if  | piece == Joukahainen ->
            let line = (case dir of Horizontal -> findRow
                                    Vertical -> findColumn) (x,y) board ++ [(x,y)]
                numJoukahainens = length <| filter (\loc -> not (adjacentToLemminkainen loc) &&
                                                       pieceAt loc board == Joukahainen &&
                                                       (not (loc == move.location) || isCurrentTile)) line
                -- don't count Joukahainen placed this turn for other Joukahainens (this is so that Joukahainens can beat other Joukahainens)
            in
              4 * numJoukahainens
        | piece == Louhi && isCurrentTile && hasSamePieceAtOtherEnd (x,y) board dir ->
            100 -- i.e. instantly score line
        | adjacentToLemminkainen (x,y) && not (piece == Lemminkainen) ->
            0 -- Lemminkainen makes all tiles around him 0 (except other Lemminkainens)
        | otherwise ->
            Piece.baseValue piece

-- this is just used for determining piece images
-- TODO: remove overlap between this and getTileValue (but note that there are subtle differences!)
getDisplayedTileValue : Location -> Board -> String
getDisplayedTileValue (x,y) board =
  let piece = pieceAt (x,y) board
      adjacentToLemminkainen = any (\l -> pieceAt l board == Lemminkainen) <| adjacentTiles (x,y) board
  in
    if | adjacentToLemminkainen && not (piece == Lemminkainen) -> 
            "0" 
       | piece == Joukahainen -> 
            let row = findRow (x,y) board
                column = findColumn (x,y) board
            in
              if any (\loc -> not (adjacentToLemminkainen) && pieceAt loc board == Joukahainen) (row ++ column)
              then "4_star"
              else "4"
       | otherwise ->
            toString <| Piece.baseValue piece

-- is this piece at one end of a line with the same kind of piece at the other end? (used by Louhi)
hasSamePieceAtOtherEnd : Location -> Board -> Direction -> Bool
hasSamePieceAtOtherEnd (x,y) board dir =
  let last list = head <| reverse list
      samePieces pos1 pos2 = pieceAt pos1 board == pieceAt pos2 board
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

isValidSquareToMove : State -> Location -> Int -> Bool
isValidSquareToMove state (x,y) size =
  if State.isPlayerTurn state
  then
    case state.heldPiece of
      Just idx ->
        let hand = Player.getHand state.turn state
            piece = Piece.fromString <| head <| drop idx hand
            location = (x - (size // 2), y - (size // 2))
        in
          isValidMove { piece = piece, idx = idx, location = location } state.board
      Nothing -> False
  else False
