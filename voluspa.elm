type Board = [(Pos, Piece)]

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

type Move = { piece : Piece, pos : Pos }
type Pos = { x : Int, y : Int }

makeMove : State -> Move -> State
makeMove state move 
  = state -- TODO

startState : State
startState = { board = [], score = { red = 0, blue = 0 }}

main : Element
main = asText startState
