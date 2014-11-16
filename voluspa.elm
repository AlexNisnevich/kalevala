
import Dict
import Dict (Dict)

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
type State = { board : Board, score : Score }

type Move = { player : Player, piece : Piece, location : Location }
type Location = (Int, Int)

makeMove : State -> Move -> State
makeMove state move =
  let p = playerName move.player
      newBoard = Dict.insert move.location move.piece state.board
      newScore = (Dict.getOrFail p state.score) + (scoreMove state move)
  in
    { board = newBoard, score = Dict.insert p newScore state.score }
    
scoreMove : State -> Move -> Int
scoreMove state move = 1      -- TODO: actually score moves

playerName : Player -> String
playerName player = 
  case player of
    Red -> "red"
    Blue -> "blue"

startState : State
startState = { board = Dict.empty, score = Dict.fromList [("red", 0), ("blue", 0)]}

main : Element
main = 
  let state = makeMove startState { player = Red, piece = Odin, location = (0, 0) }
  in 
    asText state
