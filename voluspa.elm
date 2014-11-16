
import Dict
import Dict (Dict)
import Mouse

type Board = Dict Location Piece

data Piece = Odin
           | Thor
           | Troll
           | Dragon
           | Fenrir
           | Skadi
           | Valkyrie
           | Loki

data Player = Red
            | Blue

type Score = Dict String Int
type State = { turn: Player, board : Board, score : Score }

type Move = { piece : Piece, location : Location }
type Location = (Int, Int)

tryMove : Move -> State -> State
tryMove move state =
  if (isValidMove move state) then (makeMove move state) else state

isValidMove : Move -> State -> Bool
isValidMove move state =
  not (Dict.member move.location state.board)
  -- TODO: there are more conditions for a move to not be valid 
  --       (e.g. not touching an existing tile)

makeMove : Move -> State -> State
makeMove move state =
  let p = playerName state.turn
      newBoard = Dict.insert move.location move.piece state.board
      newScore = (Dict.getOrFail p state.score) + (scoreMove move state)
  in
    {
      turn = nextPlayer state.turn,
      board = newBoard,
      score = Dict.insert p newScore state.score 
    }
    
scoreMove : Move -> State -> Int
scoreMove move state = 1      -- TODO: actually score moves

playerName : Player -> String
playerName player = 
  case player of
    Red -> "red"
    Blue -> "blue"
    
nextPlayer : Player -> Player
nextPlayer player =
  case player of
    Red -> Blue
    Blue -> Red

startState : State
startState = 
  { 
    turn = Red,
    board = Dict.empty, 
    score = Dict.fromList [("red", 0), ("blue", 0)]
  }
  
main : Signal Element
main = 
  -- Move made on every click
  let move = { piece = Odin, location = (0, 0) }
  in
    asText <~ (foldp tryMove startState ((\() -> move) <~ Mouse.clicks))
