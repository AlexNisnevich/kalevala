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

type Score = { red : Int, blue : Int }
type State = { board : Board, score : Score }

type Move = { piece : Piece, location : Location }
type Location = (Int, Int)

makeMove : State -> Move -> State
makeMove state move 
  = state -- TODO

startState : State
startState = { board = Dict.empty, score = { red = 0, blue = 0 }}

main : Element
main = asText startState
