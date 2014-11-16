
import Array
import List
import Random
import Dict
import Dict (Dict)
import Mouse
import Graphics.Collage (Form)

-- TYPES

type Board = Dict Location Piece

type Hands = Dict String [String]
type Score = Dict String Int
type State = { turn: Player, board : Board, score : Score, deck: Deck, hands: Hands}

type Move = { piece : Piece, location : Location, shuffle : Deck }
type Location = (Float, Float)

data Piece = Odin
           | Thor
           | Troll
           | Dragon
           | Fenrir
           | Skadi
           | Valkyrie
           | Loki
           
data Deck = Unshuffled
          | Shuffled [String]

data Player = Red
            | Blue

data Action = PlacePiece Move 
            | StartGame [String]

-- MAGIC STRINGS

playerName : Player -> String
playerName player = 
  case player of
    Red -> "red"
    Blue -> "blue"
    
pieceFromString : String -> Piece
pieceFromString str =
  case str of
    "odin" -> Odin
    "thor" -> Thor
    "troll" -> Troll
    "dragon" -> Dragon
    "fenrir" -> Fenrir
    "skadi" -> Skadi
    "valkyrie" -> Valkyrie
    "loki" -> Loki

-- HELPERS

without : Int -> [a] -> [a]
without i arr = 
  let before = take i arr
      after = drop (i+1) arr
  in
    before ++ after

shuffle : [a] -> Signal b -> Signal [a]
shuffle list signal = 
  let randomsFromSignal signal = Random.floatList (lift (\x -> List.length list) signal)
      shuffleWithRandoms list randoms =
        if (List.isEmpty list)
        then []
        else
          let i = floor (head randoms * toFloat (List.length list))
              ith = head (drop i list) 
          in
            [ith] ++ (shuffleWithRandoms (without i list) (tail randoms))
  in
    lift2 shuffleWithRandoms (constant list) (randomsFromSignal signal)

-- MOVES

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
    , deck = state.deck
    , hands = state.hands
    }
    
scoreMove : Move -> State -> Int
scoreMove move state = 1      -- TODO: actually score moves

nextPlayer : Player -> Player
nextPlayer player =
  case player of
    Red -> Blue
    Blue -> Red 

-- GAME

tryStartGame : State -> [String] -> State
tryStartGame state deck =
  case state.deck of
      Unshuffled -> startGame state deck
      Shuffled d -> state

startGame : State -> [String] -> State
startGame state deck =
  let redHand = take 5 deck
      blueHand = take 5 (drop 5 deck)
      hands = Dict.fromList [("red", redHand), ("blue", blueHand)]
      remainder = drop 10 deck
  in 
    { state | hands <- hands, deck <- Shuffled remainder }
    
-- DISPLAY

pieceToImage: Piece -> Float -> Element
pieceToImage piece tileSize = 
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
  in
    croppedImage pos tileSize tileSize "http://i.imgur.com/5yLICgb.png?1"
    
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
  let x = (fst location) * tileSize
      y = (snd location) * tileSize
  in
    move (x, y) (toForm (pieceToImage piece tileSize))

renderBoard : Board -> Element
renderBoard board =
  let num = 15
      tileSize = 50
      size = (round num * tileSize) + 1
      grid = drawGrid num tileSize
      pieces = map (\p -> drawPiece p tileSize) (Dict.toList board)
  in
    collage size size (grid ++ pieces)

renderHand : Player -> State -> Element
renderHand player state =
  let hand = Dict.getOrFail (playerName player) state.hands
  in 
    flow right
     ([plainText (playerName player)
      ] ++ map (\p -> pieceToImage (pieceFromString p) 50) hand)

display : State -> Element
display state =
  flow down 
    [ asText state
    , renderBoard state.board 
    , renderHand Red state
    , renderHand Blue state
    ]

-- MAIN

once : Signal Time
once = fps 10 -- TODO: find how to actually make this trigger only once?

performAction : Action -> State -> State
performAction action state =
  case action of
    PlacePiece move -> tryMove move state
    StartGame deck -> tryStartGame state deck

deckContents : [String]
deckContents = (Array.toList (Array.repeat 6 "odin") ++
                Array.toList (Array.repeat 8 "thor") ++
                Array.toList (Array.repeat 6 "troll") ++ 
                Array.toList (Array.repeat 8 "dragon") ++
                Array.toList (Array.repeat 8 "fenrir") ++
                Array.toList (Array.repeat 9 "skadi") ++
                Array.toList (Array.repeat 9 "valkyrie") ++ 
                Array.toList (Array.repeat 6 "loki"))

startState : State
startState = 
  { turn = Red
  , board = Dict.empty
  , score = Dict.fromList [("red", 0), ("blue", 0)]
  , deck = Unshuffled
  , hands = Dict.fromList [("red", []), ("blue", [])]
  }

processStartGameSignal : Signal a -> Signal Action
processStartGameSignal signal =
  (\s -> StartGame s) <~ (shuffle deckContents signal)
  
processMouseSignal : Signal a -> Signal Action
processMouseSignal signal =
  (\s -> PlacePiece { piece = Odin, location = (0, 0), shuffle = Unshuffled}) <~ signal

main : Signal Element
main = 
  let actionSignal = (merge (processMouseSignal Mouse.clicks) (processStartGameSignal once))
  in
    display <~ (foldp performAction startState actionSignal)

