
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
type State = { turn: Player, board : Board, score : Score }

type Move = { piece : Piece, location : Location }
type Location = (Int, Int)

makeMove : State -> Move -> State
makeMove state move =
  let p = playerName state.turn
      newBoard = Dict.insert move.location move.piece state.board
      newScore = (Dict.getOrFail p state.score) + (scoreMove state move)
  in
    {
      turn = nextPlayer state.turn,
      board = newBoard,
      score = Dict.insert p newScore state.score 
    }
    
scoreMove : State -> Move -> Int
scoreMove state move = 1      -- TODO: actually score moves

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

main : Element
main = 
  let state = makeMove startState { piece = Odin, location = (0, 0) }
  in 
    asText state
