Elm.Voluspa = Elm.Voluspa || {};
Elm.Voluspa.make = function (_elm) {
   "use strict";
   _elm.Voluspa = _elm.Voluspa || {};
   if (_elm.Voluspa.values)
   return _elm.Voluspa.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Voluspa",
   $Array = Elm.Array.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Color = Elm.Color.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Dict = Elm.Dict.make(_elm),
   $Graphics$Collage = Elm.Graphics.Collage.make(_elm),
   $Graphics$Element = Elm.Graphics.Element.make(_elm),
   $Graphics$Input = Elm.Graphics.Input.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Mouse = Elm.Mouse.make(_elm),
   $Random = Elm.Random.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $String = Elm.String.make(_elm),
   $Text = Elm.Text.make(_elm);
   var deckContents = _L.append($Array.toList(A2($Array.repeat,
   6,
   "odin")),
   _L.append($Array.toList(A2($Array.repeat,
   8,
   "thor")),
   _L.append($Array.toList(A2($Array.repeat,
   6,
   "troll")),
   _L.append($Array.toList(A2($Array.repeat,
   8,
   "dragon")),
   _L.append($Array.toList(A2($Array.repeat,
   8,
   "fenrir")),
   _L.append($Array.toList(A2($Array.repeat,
   9,
   "skadi")),
   _L.append($Array.toList(A2($Array.repeat,
   9,
   "valkyrie")),
   $Array.toList(A2($Array.repeat,
   6,
   "loki")))))))));
   var isAdjacent = F2(function (_v0,
   _v1) {
      return function () {
         switch (_v1.ctor)
         {case "_Tuple2":
            return function () {
                 switch (_v0.ctor)
                 {case "_Tuple2":
                    return _U.eq(_v0._1,
                      _v1._1) && _U.eq($Basics.abs(_v0._0 - _v1._0),
                      1) || _U.eq(_v0._0,
                      _v1._0) && _U.eq($Basics.abs(_v0._1 - _v1._1),
                      1);}
                 _E.Case($moduleName,
                 "on line 183, column 4 to 70");
              }();}
         _E.Case($moduleName,
         "on line 183, column 4 to 70");
      }();
   });
   var isValidMove = F2(function (move,
   state) {
      return function () {
         var hasAdjacentTile = A2($List.any,
         function (loc) {
            return A2(isAdjacent,
            loc,
            move.location);
         },
         $Dict.keys(state.board));
         var isOccupied = A2($Dict.member,
         move.location,
         state.board);
         return $Basics.not(isOccupied) && (hasAdjacentTile || $List.isEmpty($Dict.toList(state.board)) && _U.eq(move.location,
         {ctor: "_Tuple2",_0: 0,_1: 0}));
      }();
   });
   var pickUpPiece = F2(function (idx,
   state) {
      return _U.replace([["heldPiece"
                         ,$Maybe.Just(idx)]],
      state);
   });
   var tryToPickUpPiece = F3(function (player,
   idx,
   state) {
      return _U.eq(state.turn,
      player) ? A2(pickUpPiece,
      idx,
      state) : _U.replace([["heldPiece"
                           ,$Maybe.Nothing]],
      state);
   });
   var findRightward = F2(function (_v8,
   board) {
      return function () {
         switch (_v8.ctor)
         {case "_Tuple2":
            return A2($Dict.member,
              {ctor: "_Tuple2"
              ,_0: _v8._0
              ,_1: _v8._1},
              board) ? _L.append(_L.fromArray([{ctor: "_Tuple2"
                                               ,_0: _v8._0
                                               ,_1: _v8._1}]),
              A2(findRightward,
              {ctor: "_Tuple2"
              ,_0: _v8._0 + 1
              ,_1: _v8._1},
              board)) : _L.fromArray([]);}
         _E.Case($moduleName,
         "between lines 154 and 156");
      }();
   });
   var findLeftward = F2(function (_v12,
   board) {
      return function () {
         switch (_v12.ctor)
         {case "_Tuple2":
            return A2($Dict.member,
              {ctor: "_Tuple2"
              ,_0: _v12._0
              ,_1: _v12._1},
              board) ? _L.append(_L.fromArray([{ctor: "_Tuple2"
                                               ,_0: _v12._0
                                               ,_1: _v12._1}]),
              A2(findLeftward,
              {ctor: "_Tuple2"
              ,_0: _v12._0 - 1
              ,_1: _v12._1},
              board)) : _L.fromArray([]);}
         _E.Case($moduleName,
         "between lines 148 and 150");
      }();
   });
   var findBelow = F2(function (_v16,
   board) {
      return function () {
         switch (_v16.ctor)
         {case "_Tuple2":
            return A2($Dict.member,
              {ctor: "_Tuple2"
              ,_0: _v16._0
              ,_1: _v16._1},
              board) ? _L.append(_L.fromArray([{ctor: "_Tuple2"
                                               ,_0: _v16._0
                                               ,_1: _v16._1}]),
              A2(findBelow,
              {ctor: "_Tuple2"
              ,_0: _v16._0
              ,_1: _v16._1 + 1},
              board)) : _L.fromArray([]);}
         _E.Case($moduleName,
         "between lines 142 and 144");
      }();
   });
   var findAbove = F2(function (_v20,
   board) {
      return function () {
         switch (_v20.ctor)
         {case "_Tuple2":
            return A2($Dict.member,
              {ctor: "_Tuple2"
              ,_0: _v20._0
              ,_1: _v20._1},
              board) ? _L.append(_L.fromArray([{ctor: "_Tuple2"
                                               ,_0: _v20._0
                                               ,_1: _v20._1}]),
              A2(findAbove,
              {ctor: "_Tuple2"
              ,_0: _v20._0
              ,_1: _v20._1 - 1},
              board)) : _L.fromArray([]);}
         _E.Case($moduleName,
         "between lines 136 and 138");
      }();
   });
   var findRow = F2(function (_v24,
   board) {
      return function () {
         switch (_v24.ctor)
         {case "_Tuple2":
            return _L.append(A2(findLeftward,
              {ctor: "_Tuple2"
              ,_0: _v24._0 - 1
              ,_1: _v24._1},
              board),
              A2(findRightward,
              {ctor: "_Tuple2"
              ,_0: _v24._0 + 1
              ,_1: _v24._1},
              board));}
         _E.Case($moduleName,
         "on line 132, column 24 to 83");
      }();
   });
   var findColumn = F2(function (_v28,
   board) {
      return function () {
         switch (_v28.ctor)
         {case "_Tuple2":
            return _L.append(A2(findAbove,
              {ctor: "_Tuple2"
              ,_0: _v28._0
              ,_1: _v28._1 - 1},
              board),
              A2(findBelow,
              {ctor: "_Tuple2"
              ,_0: _v28._0
              ,_1: _v28._1 + 1},
              board));}
         _E.Case($moduleName,
         "on line 129, column 27 to 79");
      }();
   });
   var getTileScore = F2(function (_v32,
   board) {
      return function () {
         switch (_v32.ctor)
         {case "_Tuple2":
            return function () {
                 var piece = A2($Dict.getOrFail,
                 {ctor: "_Tuple2"
                 ,_0: _v32._0
                 ,_1: _v32._1},
                 board);
                 return function () {
                    switch (piece.ctor)
                    {case "Dragon": return 5;
                       case "Fenrir": return 4;
                       case "Loki": return 1;
                       case "Odin": return 8;
                       case "Skadi": return 3;
                       case "Thor": return 7;
                       case "Troll": return 6;
                       case "Valkyrie": return 2;}
                    _E.Case($moduleName,
                    "between lines 118 and 126");
                 }();
              }();}
         _E.Case($moduleName,
         "between lines 116 and 126");
      }();
   });
   var scoreMove = F2(function (move,
   state) {
      return function () {
         var row = A2(findRow,
         move.location,
         state.board);
         var rowSize = $List.length(row) + 1;
         var rowScores = A2($List.map,
         function (r) {
            return A2(getTileScore,
            r,
            state.board);
         },
         row);
         var rowHighScore = $List.isEmpty(row) ? 0 : $List.maximum(rowScores);
         var column = A2(findColumn,
         move.location,
         state.board);
         var columnSize = $List.length(column) + 1;
         var columnScores = A2($List.map,
         function (c) {
            return A2(getTileScore,
            c,
            state.board);
         },
         column);
         var columnHighScore = $List.isEmpty(column) ? 0 : $List.maximum(columnScores);
         var tileScore = A2(getTileScore,
         move.location,
         state.board);
         var columnPoints = _U.cmp(tileScore,
         columnHighScore) > 0 && _U.cmp(columnSize,
         2) > -1 ? columnSize : 0;
         var rowPoints = _U.cmp(tileScore,
         rowHighScore) > 0 && _U.cmp(rowSize,
         2) > -1 ? rowSize : 0;
         return columnPoints + rowPoints;
      }();
   });
   var without = F2(function (i,
   arr) {
      return function () {
         var after = A2($List.drop,
         i + 1,
         arr);
         var before = A2($List.take,
         i,
         arr);
         return _L.append(before,after);
      }();
   });
   var get = F2(function (list,
   idx) {
      return $List.head(A2($List.drop,
      idx,
      list));
   });
   _op["!!"] = get;
   var shuffle = F2(function (list,
   signal) {
      return function () {
         var shuffleWithRandoms = F2(function (list,
         randoms) {
            return $List.isEmpty(list) ? _L.fromArray([]) : function () {
               var i = $Basics.floor($List.head(randoms) * $Basics.toFloat($List.length(list)));
               return _L.append(_L.fromArray([A2(_op["!!"],
               list,
               i)]),
               A2(shuffleWithRandoms,
               A2(without,i,list),
               $List.tail(randoms)));
            }();
         });
         var randomsFromSignal = function (signal) {
            return $Random.floatList(A2($Signal.lift,
            function (x) {
               return $List.length(list);
            },
            signal));
         };
         return A3($Signal.lift2,
         shuffleWithRandoms,
         $Signal.constant(list),
         randomsFromSignal(signal));
      }();
   });
   var playerName = function (player) {
      return function () {
         switch (player.ctor)
         {case "Blue": return "blue";
            case "Red": return "red";}
         _E.Case($moduleName,
         "between lines 69 and 71");
      }();
   };
   var handPadding = 5;
   var gameBoardSize = 15;
   var gameTileSize = 50;
   var pieceToImage = F2(function (piece,
   tileSize) {
      return function () {
         var pos = function () {
            switch (piece.ctor)
            {case "Dragon":
               return {ctor: "_Tuple2"
                      ,_0: 0
                      ,_1: 1};
               case "Fenrir":
               return {ctor: "_Tuple2"
                      ,_0: 3
                      ,_1: 0};
               case "Loki":
               return {ctor: "_Tuple2"
                      ,_0: 0
                      ,_1: 0};
               case "Odin":
               return {ctor: "_Tuple2"
                      ,_0: 3
                      ,_1: 1};
               case "Skadi":
               return {ctor: "_Tuple2"
                      ,_0: 2
                      ,_1: 0};
               case "Thor":
               return {ctor: "_Tuple2"
                      ,_0: 2
                      ,_1: 1};
               case "Troll":
               return {ctor: "_Tuple2"
                      ,_0: 1
                      ,_1: 1};
               case "Valkyrie":
               return {ctor: "_Tuple2"
                      ,_0: 1
                      ,_1: 0};}
            _E.Case($moduleName,
            "between lines 282 and 291");
         }();
         var tileSize = $Basics.round(gameTileSize);
         return function () {
            switch (pos.ctor)
            {case "_Tuple2":
               return A4($Graphics$Element.croppedImage,
                 {ctor: "_Tuple2"
                 ,_0: pos._0 * tileSize
                 ,_1: pos._1 * tileSize},
                 tileSize,
                 tileSize,
                 "http://i.imgur.com/5yLICgb.png?1");}
            _E.Case($moduleName,
            "between lines 292 and 293");
         }();
      }();
   });
   var drawPiece = F2(function (_v42,
   tileSize) {
      return function () {
         switch (_v42.ctor)
         {case "_Tuple2":
            switch (_v42._0.ctor)
              {case "_Tuple2":
                 return function () {
                      var y = _v42._0._1 * tileSize;
                      var x = _v42._0._0 * tileSize;
                      return A2($Graphics$Collage.move,
                      {ctor: "_Tuple2",_0: x,_1: y},
                      $Graphics$Collage.toForm(A2(pieceToImage,
                      _v42._1,
                      tileSize)));
                   }();}
              break;}
         _E.Case($moduleName,
         "between lines 307 and 310");
      }();
   });
   var drawGrid = function () {
      var num = $Basics.toFloat(gameBoardSize);
      var size = num * gameTileSize;
      var xShift = gameTileSize / 2 - size / 2;
      var yShift = gameTileSize / 2 - size / 2;
      var shape = F2(function (x,y) {
         return A2($Graphics$Collage.move,
         {ctor: "_Tuple2"
         ,_0: gameTileSize * x + xShift
         ,_1: gameTileSize * y + yShift},
         A2($Graphics$Collage.outlined,
         $Graphics$Collage.solid($Color.black),
         $Graphics$Collage.square(gameTileSize)));
      });
      return A2($List.concatMap,
      function (x) {
         return A2($List.map,
         function (y) {
            return A2(shape,x,y);
         },
         _L.range(0,num - 1));
      },
      _L.range(0,num - 1));
   }();
   var renderBoard = function (board) {
      return function () {
         var pieces = A2($List.map,
         function (p) {
            return A2(drawPiece,
            p,
            gameTileSize);
         },
         $Dict.toList(board));
         var size = gameBoardSize * $Basics.round(gameTileSize) + 1;
         return A3($Graphics$Collage.collage,
         size,
         size,
         _L.append(drawGrid,pieces));
      }();
   };
   var gameHeaderSize = 100;
   var mouseToBoardPosition = function (_v48) {
      return function () {
         switch (_v48.ctor)
         {case "_Tuple2":
            return function () {
                 var offset = gameBoardSize / 2 | 0;
                 var tileSize = $Basics.round(gameTileSize);
                 var y = _v48._1 - gameHeaderSize;
                 var boardY = $Basics.toFloat(0 - ((y / tileSize | 0) - offset));
                 var x = _v48._0;
                 var boardX = $Basics.toFloat((x / tileSize | 0) - offset);
                 return {ctor: "_Tuple2"
                        ,_0: boardX
                        ,_1: boardY};
              }();}
         _E.Case($moduleName,
         "between lines 386 and 392");
      }();
   };
   var None = {ctor: "None"};
   var clickInput = $Graphics$Input.input(None);
   var PieceInHand = F2(function (a,
   b) {
      return {ctor: "PieceInHand"
             ,_0: a
             ,_1: b};
   });
   var Board = {ctor: "Board"};
   var Start = {ctor: "Start"};
   var NoAction = {ctor: "NoAction"};
   var MakeRandomMove = function (a) {
      return {ctor: "MakeRandomMove"
             ,_0: a};
   };
   var StartGame = function (a) {
      return {ctor: "StartGame"
             ,_0: a};
   };
   var PlacePiece = function (a) {
      return {ctor: "PlacePiece"
             ,_0: a};
   };
   var PickUpPiece = F2(function (a,
   b) {
      return {ctor: "PickUpPiece"
             ,_0: a
             ,_1: b};
   });
   var processClick = function (signal) {
      return function () {
         var sampledMouse = A2($Signal.sampleOn,
         signal,
         $Mouse.position);
         var shuffled = A2(shuffle,
         deckContents,
         signal);
         var random = $Random.$float(signal);
         return A5($Signal.lift4,
         F4(function (clickType,
         randomFloat,
         shuffledDeck,
         mousePos) {
            return function () {
               var boardPos = A2($Debug.watch,
               "Board position",
               mouseToBoardPosition(mousePos));
               var click = A2($Debug.watch,
               "clickInput.signal",
               clickType);
               var pos = A2($Debug.watch,
               "Mouse.position",
               mousePos);
               return function () {
                  switch (clickType.ctor)
                  {case "Board":
                     return PlacePiece(boardPos);
                     case "None": return NoAction;
                     case "PieceInHand":
                     return A2(PickUpPiece,
                       clickType._0,
                       clickType._1);
                     case "Start":
                     return StartGame(shuffledDeck);}
                  _E.Case($moduleName,
                  "between lines 406 and 410");
               }();
            }();
         }),
         signal,
         random,
         shuffled,
         sampledMouse);
      }();
   };
   var Blue = {ctor: "Blue"};
   var Red = {ctor: "Red"};
   var nextPlayer = function (player) {
      return function () {
         switch (player.ctor)
         {case "Blue": return Red;
            case "Red": return Blue;}
         _E.Case($moduleName,
         "between lines 247 and 249");
      }();
   };
   var makeMove = F2(function (move,
   state) {
      return function () {
         var newBoard = A3($Dict.insert,
         move.location,
         move.piece,
         state.board);
         var p = playerName(state.turn);
         var newScore = A2($Dict.getOrFail,
         p,
         state.score) + A2(scoreMove,
         move,
         _U.replace([["board",newBoard]],
         state));
         var newHand = _L.append(A2($List.drop,
         1,
         A2($Dict.getOrFail,
         p,
         state.hands)),
         A2($List.take,1,state.deck));
         return {_: {}
                ,board: newBoard
                ,deck: A2($List.drop,
                1,
                state.deck)
                ,hands: A3($Dict.insert,
                p,
                newHand,
                state.hands)
                ,heldPiece: $Maybe.Nothing
                ,score: A3($Dict.insert,
                p,
                newScore,
                state.score)
                ,started: true
                ,turn: nextPlayer(state.turn)};
      }();
   });
   var startState = {_: {}
                    ,board: $Dict.empty
                    ,deck: _L.fromArray([])
                    ,hands: $Dict.fromList(_L.fromArray([{ctor: "_Tuple2"
                                                         ,_0: "red"
                                                         ,_1: _L.fromArray([])}
                                                        ,{ctor: "_Tuple2"
                                                         ,_0: "blue"
                                                         ,_1: _L.fromArray([])}]))
                    ,heldPiece: $Maybe.Nothing
                    ,score: $Dict.fromList(_L.fromArray([{ctor: "_Tuple2"
                                                         ,_0: "red"
                                                         ,_1: 0}
                                                        ,{ctor: "_Tuple2"
                                                         ,_0: "blue"
                                                         ,_1: 0}]))
                    ,started: false
                    ,turn: Red};
   var Loki = {ctor: "Loki"};
   var Valkyrie = {ctor: "Valkyrie"};
   var Skadi = {ctor: "Skadi"};
   var Fenrir = {ctor: "Fenrir"};
   var Dragon = {ctor: "Dragon"};
   var Troll = {ctor: "Troll"};
   var Thor = {ctor: "Thor"};
   var Odin = {ctor: "Odin"};
   var pieceFromString = function (str) {
      return function () {
         switch (str)
         {case "dragon": return Dragon;
            case "fenrir": return Fenrir;
            case "loki": return Loki;
            case "odin": return Odin;
            case "skadi": return Skadi;
            case "thor": return Thor;
            case "troll": return Troll;
            case "valkyrie":
            return Valkyrie;}
         _E.Case($moduleName,
         "between lines 75 and 83");
      }();
   };
   var tryMove = F2(function (location,
   state) {
      return function () {
         var _v57 = state.heldPiece;
         switch (_v57.ctor)
         {case "Just":
            return function () {
                 var p = playerName(state.turn);
                 var hand = A2($Dict.getOrFail,
                 p,
                 state.hands);
                 var pieceStr = $List.head(A2($List.drop,
                 _v57._0,
                 hand));
                 var piece = pieceFromString(pieceStr);
                 var move = {_: {}
                            ,location: location
                            ,piece: piece};
                 return A2(isValidMove,
                 move,
                 state) ? A2(makeMove,
                 move,
                 state) : _U.replace([["heldPiece"
                                      ,$Maybe.Nothing]],
                 state);
              }();
            case "Nothing": return state;}
         _E.Case($moduleName,
         "between lines 170 and 179");
      }();
   });
   var makeRandomMove = F2(function (state,
   seed) {
      return state.started ? function () {
         var halfGameBoardSize = gameBoardSize / 2 | 0;
         var xs = A2($List.map,
         function (x) {
            return $Basics.toFloat(x - halfGameBoardSize);
         },
         _L.range(0,gameBoardSize - 1));
         var locations = A2($List.concatMap,
         function (x) {
            return A2($List.map,
            function (y) {
               return {ctor: "_Tuple2"
                      ,_0: x
                      ,_1: y};
            },
            xs);
         },
         xs);
         var p = playerName(state.turn);
         var piece = pieceFromString($List.head(A2($Dict.getOrFail,
         p,
         state.hands)));
         var validLocations = A2($List.filter,
         function (loc) {
            return A2(isValidMove,
            {_: {}
            ,location: loc
            ,piece: piece},
            state);
         },
         locations);
         var idx = $Basics.floor(seed * $Basics.toFloat($List.length(validLocations)));
         var location = A2(_op["!!"],
         validLocations,
         idx);
         return A2(tryMove,
         location,
         state);
      }() : state;
   });
   var startGame = F2(function (state,
   deck) {
      return function () {
         var deckWithIndices = A2($List.zip,
         _L.range(0,
         $List.length(deck) - 1),
         deck);
         var idxFirstNonTroll = $Basics.fst($List.head(A2($List.filter,
         function (_v59) {
            return function () {
               switch (_v59.ctor)
               {case "_Tuple2":
                  return $Basics.not(_U.eq(_v59._1,
                    "troll"));}
               _E.Case($moduleName,
               "on line 260, column 66 to 87");
            }();
         },
         deckWithIndices)));
         var firstTile = pieceFromString(A2(_op["!!"],
         deck,
         idxFirstNonTroll));
         var deckMinusFirstTile = A2(without,
         idxFirstNonTroll,
         deck);
         var redHand = A2($List.take,
         5,
         deckMinusFirstTile);
         var blueHand = A2($List.take,
         5,
         A2($List.drop,
         5,
         deckMinusFirstTile));
         var hands = $Dict.fromList(_L.fromArray([{ctor: "_Tuple2"
                                                  ,_0: "red"
                                                  ,_1: redHand}
                                                 ,{ctor: "_Tuple2"
                                                  ,_0: "blue"
                                                  ,_1: blueHand}]));
         var remainder = A2($List.drop,
         10,
         deckMinusFirstTile);
         return _U.replace([["hands"
                            ,hands]
                           ,["deck",remainder]
                           ,["started",true]
                           ,["board"
                            ,A2($Dict.singleton,
                            {ctor: "_Tuple2",_0: 0,_1: 0},
                            firstTile)]],
         state);
      }();
   });
   var tryStartGame = F2(function (state,
   deck) {
      return $Basics.not(state.started) ? A2(startGame,
      state,
      deck) : state;
   });
   var performAction = F2(function (action,
   state) {
      return function () {
         switch (action.ctor)
         {case "MakeRandomMove":
            return A2(makeRandomMove,
              state,
              action._0);
            case "NoAction": return state;
            case "PickUpPiece":
            return A3(tryToPickUpPiece,
              action._0,
              action._1,
              state);
            case "PlacePiece":
            return A2(tryMove,
              action._0,
              state);
            case "StartGame":
            return A2(tryStartGame,
              state,
              action._0);}
         _E.Case($moduleName,
         "between lines 356 and 361");
      }();
   });
   var renderHand = F2(function (player,
   state) {
      return function () {
         var pieceImage = function (pieceStr) {
            return A2(pieceToImage,
            pieceFromString(pieceStr),
            gameTileSize);
         };
         var isPieceHeld = function (idx) {
            return _U.eq(state.turn,
            player) && _U.eq(state.heldPiece,
            $Maybe.Just(idx));
         };
         var tileSize = $Basics.round(gameTileSize);
         var pieceSize = tileSize + handPadding;
         var makePiece = F2(function (idx,
         pieceStr) {
            return A2($Graphics$Input.clickable,
            clickInput.handle,
            A2(PieceInHand,
            player,
            idx))($Graphics$Element.color(isPieceHeld(idx) ? $Color.blue : $Color.white)(A3($Graphics$Element.container,
            pieceSize,
            pieceSize,
            $Graphics$Element.middle)(pieceImage(pieceStr))));
         });
         var p = playerName(player);
         var hand = A2($Dict.getOrFail,
         p,
         state.hands);
         var handContents = A2($List.indexedMap,
         makePiece,
         hand);
         var handText = A3($Graphics$Element.container,
         70,
         pieceSize,
         $Graphics$Element.middle)($Text.leftAligned((_U.eq(state.turn,
         player) ? $Text.bold : $Basics.identity)($Text.toText($String.toUpper(p)))));
         var score = A3($Graphics$Element.container,
         40,
         pieceSize,
         $Graphics$Element.middle)($Text.asText(A2($Dict.getOrFail,
         p,
         state.score)));
         return A2($Graphics$Element.flow,
         $Graphics$Element.right,
         _L.append(_L.fromArray([handText]),
         _L.append(handContents,
         _L.fromArray([score]))));
      }();
   });
   var display = function (state) {
      return A2($Graphics$Element.flow,
      $Graphics$Element.down,
      _L.fromArray([A3($Graphics$Element.size,
                   750,
                   gameHeaderSize,
                   $Text.centered(A2($Text.height,
                   50,
                   A2($Text.typeface,
                   _L.fromArray(["Rock Salt"
                                ,"cursive"]),
                   $Text.toText("V&ouml;lusp&aacute;")))))
                   ,A2($Graphics$Element.flow,
                   $Graphics$Element.right,
                   _L.fromArray([A2($Graphics$Input.clickable,
                                clickInput.handle,
                                Board)(renderBoard(state.board))
                                ,A2($Graphics$Element.flow,
                                $Graphics$Element.down,
                                _L.fromArray([A2(renderHand,
                                             Red,
                                             state)
                                             ,A2($Graphics$Element.spacer,
                                             1,
                                             $Basics.round(gameTileSize) * (gameBoardSize - 2) - handPadding * 2)
                                             ,A2(renderHand,Blue,state)]))]))
                   ,$Basics.not(state.started) ? A3($Graphics$Input.button,
                   clickInput.handle,
                   Start,
                   "Begin game!") : $Graphics$Element.empty
                   ,$Text.asText(state)]));
   };
   var main = A2($Signal._op["<~"],
   display,
   A3($Signal.foldp,
   performAction,
   startState,
   processClick(clickInput.signal)));
   var Move = F2(function (a,b) {
      return {_: {}
             ,location: b
             ,piece: a};
   });
   var State = F7(function (a,
   b,
   c,
   d,
   e,
   f,
   g) {
      return {_: {}
             ,board: b
             ,deck: d
             ,hands: e
             ,heldPiece: g
             ,score: c
             ,started: f
             ,turn: a};
   });
   _elm.Voluspa.values = {_op: _op
                         ,State: State
                         ,Move: Move
                         ,Odin: Odin
                         ,Thor: Thor
                         ,Troll: Troll
                         ,Dragon: Dragon
                         ,Fenrir: Fenrir
                         ,Skadi: Skadi
                         ,Valkyrie: Valkyrie
                         ,Loki: Loki
                         ,Red: Red
                         ,Blue: Blue
                         ,PickUpPiece: PickUpPiece
                         ,PlacePiece: PlacePiece
                         ,StartGame: StartGame
                         ,MakeRandomMove: MakeRandomMove
                         ,NoAction: NoAction
                         ,Start: Start
                         ,Board: Board
                         ,PieceInHand: PieceInHand
                         ,None: None
                         ,gameHeaderSize: gameHeaderSize
                         ,gameTileSize: gameTileSize
                         ,gameBoardSize: gameBoardSize
                         ,handPadding: handPadding
                         ,playerName: playerName
                         ,pieceFromString: pieceFromString
                         ,get: get
                         ,without: without
                         ,shuffle: shuffle
                         ,getTileScore: getTileScore
                         ,findColumn: findColumn
                         ,findRow: findRow
                         ,findAbove: findAbove
                         ,findBelow: findBelow
                         ,findLeftward: findLeftward
                         ,findRightward: findRightward
                         ,tryToPickUpPiece: tryToPickUpPiece
                         ,pickUpPiece: pickUpPiece
                         ,tryMove: tryMove
                         ,isAdjacent: isAdjacent
                         ,isValidMove: isValidMove
                         ,makeMove: makeMove
                         ,scoreMove: scoreMove
                         ,makeRandomMove: makeRandomMove
                         ,nextPlayer: nextPlayer
                         ,tryStartGame: tryStartGame
                         ,startGame: startGame
                         ,clickInput: clickInput
                         ,pieceToImage: pieceToImage
                         ,drawGrid: drawGrid
                         ,drawPiece: drawPiece
                         ,renderBoard: renderBoard
                         ,renderHand: renderHand
                         ,display: display
                         ,performAction: performAction
                         ,deckContents: deckContents
                         ,startState: startState
                         ,mouseToBoardPosition: mouseToBoardPosition
                         ,processClick: processClick
                         ,main: main};
   return _elm.Voluspa.values;
};