
import Dict
import Dict (Dict)
import Mouse
import Graphics.Collage (Form)

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
    { turn = nextPlayer state.turn
    , board = newBoard
    , score = Dict.insert p newScore state.score 
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
  { turn = Red
  , board = Dict.empty
  , score = Dict.fromList [("red", 0), ("blue", 0)]
  }
  
drawGrid : Float -> Float -> [Form]
drawGrid num tileSize =
  let size = num * tileSize
      xShift = tileSize / 2 - size / 2
      yShift = tileSize / 2 - size / 2
      shape x y = move (tileSize * x + xShift, tileSize * y + yShift) (outlined (solid black) (square tileSize))
  in
    (concatMap (\x -> (map (\y -> shape x y) [0..(num - 1)])) [0..(num - 1)])

drawPiece : (Location, Piece) -> Float -> Form
drawPiece (location, piece) tileSize = 
  let pos =
        case piece of
          Odin -> (3 * tileSize, 1 * tileSize)
          Thor -> (2 * tileSize, 1 * tileSize)
          Troll -> (1 * tileSize, 1 * tileSize)
          Dragon -> (0, 1 * tileSize)
          Fenrir -> (3 * tileSize, 0)
          Skadi -> (2 * tileSize, 0)
          Valkyrie -> (1 * tileSize, 0)
          Loki -> (0, 0)
      image = croppedImage pos tileSize tileSize "http://i.imgur.com/5yLICgb.png?1"
  in
    toForm image

renderBoard : Board -> Element
renderBoard board =
  let num = 15
      tileSize = 50
      size = (round num * tileSize) + 1
      grid = drawGrid num tileSize
      pieces = map (\p -> drawPiece p tileSize) (Dict.toList board)
  in
    collage size size (grid ++ pieces)

display : State -> Element
display state =
  flow down 
    [ asText state
    , renderBoard state.board 
    ]
     

main : Signal Element
main = 
  -- Move made on every click
  let move = { piece = Odin, location = (0, 0) }
  in
    display <~ (foldp tryMove startState ((\() -> move) <~ Mouse.clicks))
