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
   $Text = Elm.Text.make(_elm),
   $Window = Elm.Window.make(_elm);
   var pieceToImage = F2(function (piece,
   tileSize) {
      return function () {
         var imgPath = function () {
            switch (piece.ctor)
            {case "Dragon":
               return "images/tile_4.jpg";
               case "Fenrir":
               return "images/tile_3.jpg";
               case "Loki":
               return "images/tile_0.jpg";
               case "Odin":
               return "images/tile_7.jpg";
               case "Skadi":
               return "images/tile_2.jpg";
               case "Thor":
               return "images/tile_6.jpg";
               case "Troll":
               return "images/tile_5.jpg";
               case "Valkyrie":
               return "images/tile_1.jpg";}
            _E.Case($moduleName,
            "between lines 459 and 468");
         }();
         return A3($Graphics$Element.image,
         $Basics.round(tileSize),
         $Basics.round(tileSize),
         imgPath);
      }();
   });
   var drawPiece = F2(function (_v1,
   tileSize) {
      return function () {
         switch (_v1.ctor)
         {case "_Tuple2":
            switch (_v1._0.ctor)
              {case "_Tuple2":
                 return function () {
                      var y = _v1._0._1 * tileSize;
                      var x = _v1._0._0 * tileSize;
                      return A2($Graphics$Collage.move,
                      {ctor: "_Tuple2",_0: x,_1: y},
                      $Graphics$Collage.toForm(A2(pieceToImage,
                      _v1._1,
                      tileSize)));
                   }();}
              break;}
         _E.Case($moduleName,
         "between lines 483 and 486");
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
   var findRightward = F2(function (_v7,
   board) {
      return function () {
         switch (_v7.ctor)
         {case "_Tuple2":
            return A2($Dict.member,
              {ctor: "_Tuple2"
              ,_0: _v7._0
              ,_1: _v7._1},
              board) ? _L.append(_L.fromArray([{ctor: "_Tuple2"
                                               ,_0: _v7._0
                                               ,_1: _v7._1}]),
              A2(findRightward,
              {ctor: "_Tuple2"
              ,_0: _v7._0 + 1
              ,_1: _v7._1},
              board)) : _L.fromArray([]);}
         _E.Case($moduleName,
         "between lines 258 and 260");
      }();
   });
   var findLeftward = F2(function (_v11,
   board) {
      return function () {
         switch (_v11.ctor)
         {case "_Tuple2":
            return A2($Dict.member,
              {ctor: "_Tuple2"
              ,_0: _v11._0
              ,_1: _v11._1},
              board) ? _L.append(_L.fromArray([{ctor: "_Tuple2"
                                               ,_0: _v11._0
                                               ,_1: _v11._1}]),
              A2(findLeftward,
              {ctor: "_Tuple2"
              ,_0: _v11._0 - 1
              ,_1: _v11._1},
              board)) : _L.fromArray([]);}
         _E.Case($moduleName,
         "between lines 252 and 254");
      }();
   });
   var findBelow = F2(function (_v15,
   board) {
      return function () {
         switch (_v15.ctor)
         {case "_Tuple2":
            return A2($Dict.member,
              {ctor: "_Tuple2"
              ,_0: _v15._0
              ,_1: _v15._1},
              board) ? _L.append(_L.fromArray([{ctor: "_Tuple2"
                                               ,_0: _v15._0
                                               ,_1: _v15._1}]),
              A2(findBelow,
              {ctor: "_Tuple2"
              ,_0: _v15._0
              ,_1: _v15._1 + 1},
              board)) : _L.fromArray([]);}
         _E.Case($moduleName,
         "between lines 246 and 248");
      }();
   });
   var findAbove = F2(function (_v19,
   board) {
      return function () {
         switch (_v19.ctor)
         {case "_Tuple2":
            return A2($Dict.member,
              {ctor: "_Tuple2"
              ,_0: _v19._0
              ,_1: _v19._1},
              board) ? _L.append(_L.fromArray([{ctor: "_Tuple2"
                                               ,_0: _v19._0
                                               ,_1: _v19._1}]),
              A2(findAbove,
              {ctor: "_Tuple2"
              ,_0: _v19._0
              ,_1: _v19._1 - 1},
              board)) : _L.fromArray([]);}
         _E.Case($moduleName,
         "between lines 240 and 242");
      }();
   });
   var hasSamePieceAtOtherEnd = F3(function (_v23,
   board,
   dir) {
      return function () {
         switch (_v23.ctor)
         {case "_Tuple2":
            return function () {
                 var right = A2(findRightward,
                 {ctor: "_Tuple2"
                 ,_0: _v23._0 + 1
                 ,_1: _v23._1},
                 board);
                 var left = A2(findLeftward,
                 {ctor: "_Tuple2"
                 ,_0: _v23._0 - 1
                 ,_1: _v23._1},
                 board);
                 var below = A2(findBelow,
                 {ctor: "_Tuple2"
                 ,_0: _v23._0
                 ,_1: _v23._1 + 1},
                 board);
                 var above = A2(findAbove,
                 {ctor: "_Tuple2"
                 ,_0: _v23._0
                 ,_1: _v23._1 - 1},
                 board);
                 var pieceAt = function (pos) {
                    return A2($Dict.getOrFail,
                    pos,
                    board);
                 };
                 var samePieces = F2(function (pos1,
                 pos2) {
                    return _U.eq(pieceAt(pos1),
                    pieceAt(pos2));
                 });
                 var samePieceBelow = $List.isEmpty(above) && ($Basics.not($List.isEmpty(below)) && A2(samePieces,
                 $List.last(below),
                 {ctor: "_Tuple2"
                 ,_0: _v23._0
                 ,_1: _v23._1}));
                 var samePieceAbove = $List.isEmpty(below) && ($Basics.not($List.isEmpty(above)) && A2(samePieces,
                 $List.last(above),
                 {ctor: "_Tuple2"
                 ,_0: _v23._0
                 ,_1: _v23._1}));
                 var samePieceLeft = $List.isEmpty(right) && ($Basics.not($List.isEmpty(left)) && A2(samePieces,
                 $List.last(left),
                 {ctor: "_Tuple2"
                 ,_0: _v23._0
                 ,_1: _v23._1}));
                 var samePieceRight = $List.isEmpty(left) && ($Basics.not($List.isEmpty(right)) && A2(samePieces,
                 $List.last(right),
                 {ctor: "_Tuple2"
                 ,_0: _v23._0
                 ,_1: _v23._1}));
                 return function () {
                    switch (dir.ctor)
                    {case "Horizontal":
                       return samePieceLeft || samePieceRight;
                       case "Vertical":
                       return samePieceBelow || samePieceAbove;}
                    _E.Case($moduleName,
                    "between lines 277 and 279");
                 }();
              }();}
         _E.Case($moduleName,
         "between lines 266 and 279");
      }();
   });
   var findRow = F2(function (_v28,
   board) {
      return function () {
         switch (_v28.ctor)
         {case "_Tuple2":
            return _L.append(A2(findLeftward,
              {ctor: "_Tuple2"
              ,_0: _v28._0 - 1
              ,_1: _v28._1},
              board),
              A2(findRightward,
              {ctor: "_Tuple2"
              ,_0: _v28._0 + 1
              ,_1: _v28._1},
              board));}
         _E.Case($moduleName,
         "on line 236, column 24 to 83");
      }();
   });
   var findColumn = F2(function (_v32,
   board) {
      return function () {
         switch (_v32.ctor)
         {case "_Tuple2":
            return _L.append(A2(findAbove,
              {ctor: "_Tuple2"
              ,_0: _v32._0
              ,_1: _v32._1 - 1},
              board),
              A2(findBelow,
              {ctor: "_Tuple2"
              ,_0: _v32._0
              ,_1: _v32._1 + 1},
              board));}
         _E.Case($moduleName,
         "on line 233, column 27 to 79");
      }();
   });
   var isAdjacent = F2(function (_v36,
   _v37) {
      return function () {
         switch (_v37.ctor)
         {case "_Tuple2":
            return function () {
                 switch (_v36.ctor)
                 {case "_Tuple2":
                    return _U.eq(_v36._1,
                      _v37._1) && _U.eq($Basics.abs(_v36._0 - _v37._0),
                      1) || _U.eq(_v36._0,
                      _v37._0) && _U.eq($Basics.abs(_v36._1 - _v37._1),
                      1);}
                 _E.Case($moduleName,
                 "on line 193, column 4 to 70");
              }();}
         _E.Case($moduleName,
         "on line 193, column 4 to 70");
      }();
   });
   var adjacentTiles = F2(function (_v44,
   board) {
      return function () {
         switch (_v44.ctor)
         {case "_Tuple2":
            return A2($List.filter,
              function (loc) {
                 return A2(isAdjacent,
                 loc,
                 {ctor: "_Tuple2"
                 ,_0: _v44._0
                 ,_1: _v44._1});
              },
              $Dict.keys(board));}
         _E.Case($moduleName,
         "on line 197, column 3 to 58");
      }();
   });
   var getBoardSize = function (state) {
      return $List.isEmpty($Dict.toList(state.board)) ? 5 : function () {
         var locations = $Dict.keys(state.board);
         var xs = A2($List.map,
         $Basics.fst,
         locations);
         var maxX = A2($Basics.max,
         $List.maximum(xs),
         $Basics.abs($List.minimum(xs)));
         var ys = A2($List.map,
         $Basics.snd,
         locations);
         var maxY = A2($Basics.max,
         $List.maximum(ys),
         $Basics.abs($List.minimum(ys)));
         var distFromCenter = A2($Basics.max,
         maxX,
         maxY) + 2;
         return distFromCenter * 2 + 1;
      }();
   };
   var replaceAtIndex = F3(function (i,
   elt,
   arr) {
      return function () {
         var after = A2($List.drop,
         i + 1,
         arr);
         var before = A2($List.take,
         i,
         arr);
         return _L.append(before,
         _L.append(_L.fromArray([elt]),
         after));
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
   var repeat = F2(function (num,
   elt) {
      return $Array.toList(A2($Array.repeat,
      num,
      elt));
   });
   var deckContents = _L.append(A2(repeat,
   6,
   "Odin"),
   _L.append(A2(repeat,8,"Thor"),
   _L.append(A2(repeat,6,"Troll"),
   _L.append(A2(repeat,8,"Dragon"),
   _L.append(A2(repeat,8,"Fenrir"),
   _L.append(A2(repeat,9,"Skadi"),
   _L.append(A2(repeat,
   9,
   "Valkyrie"),
   A2(repeat,6,"Loki"))))))));
   _op["!!"] = F2(function (list,
   idx) {
      return $List.head(A2($List.drop,
      idx,
      list));
   });
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
   var pieceToString = function (piece) {
      return function () {
         switch (piece.ctor)
         {case "Dragon": return "Dragon";
            case "Fenrir": return "Fenrir";
            case "Loki": return "Loki";
            case "Odin": return "Odin";
            case "Skadi": return "Skadi";
            case "Thor": return "Thor";
            case "Troll": return "Troll";
            case "Valkyrie":
            return "Valkyrie";}
         _E.Case($moduleName,
         "between lines 112 and 120");
      }();
   };
   var rulesRow = F3(function (piece,
   value,
   description) {
      return function () {
         var text = A2($Graphics$Element.flow,
         $Graphics$Element.right,
         _L.fromArray([$Text.leftAligned($Text.bold($Text.toText($List.concat(_L.fromArray([pieceToString(piece)
                                                                                           ," ("
                                                                                           ,$String.show(value)
                                                                                           ,"): "])))))
                      ,$Text.plainText(description)]));
         var image = A2($Graphics$Element.beside,
         A2(pieceToImage,piece,50),
         A2($Graphics$Element.spacer,
         10,
         10));
         return A2($Graphics$Element.beside,
         image,
         A4($Graphics$Element.container,
         600,
         50,
         $Graphics$Element.midLeft,
         text));
      }();
   });
   var playerColor = function (player) {
      return function () {
         switch (player.ctor)
         {case "Blue":
            return $Color.blue;
            case "Red": return $Color.red;}
         _E.Case($moduleName,
         "between lines 94 and 96");
      }();
   };
   var playerName = function (player) {
      return function () {
         switch (player.ctor)
         {case "Blue": return "blue";
            case "Red": return "red";}
         _E.Case($moduleName,
         "between lines 88 and 90");
      }();
   };
   var noTilesInHand = F2(function (player,
   state) {
      return function () {
         var p = playerName(player);
         return $List.isEmpty(A2($Dict.getOrFail,
         p,
         state.hands));
      }();
   });
   var mustPass = function (state) {
      return A2(noTilesInHand,
      state.turn,
      state);
   };
   var handTileSize = 100;
   var handPadding = 10;
   var gameHeaderSize = 100;
   var getTotalBoardSize = function (_v51) {
      return function () {
         switch (_v51.ctor)
         {case "_Tuple2":
            return _v51._1 - gameHeaderSize;}
         _E.Case($moduleName,
         "on line 175, column 37 to 60");
      }();
   };
   var getTileSizeFromBoardSize = F2(function (boardSize,
   dims) {
      return $Basics.toFloat(getTotalBoardSize(dims) / boardSize | 0);
   });
   var drawGrid = F2(function (boardSize,
   dims) {
      return function () {
         var tileSize = A2(getTileSizeFromBoardSize,
         boardSize,
         dims);
         var num = $Basics.toFloat(boardSize);
         var size = num * tileSize;
         var offset = tileSize / 2 - size / 2;
         var shape = F2(function (x,y) {
            return A2($Graphics$Collage.move,
            {ctor: "_Tuple2"
            ,_0: tileSize * x + offset
            ,_1: tileSize * y + offset},
            A2($Graphics$Collage.outlined,
            $Graphics$Collage.solid($Color.black),
            $Graphics$Collage.square(tileSize)));
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
   });
   var mouseToBoardPosition = F3(function (_v55,
   state,
   dims) {
      return function () {
         switch (_v55.ctor)
         {case "_Tuple2":
            return function () {
                 var boardSize = getBoardSize(state);
                 var tileSize = $Basics.round(A2(getTileSizeFromBoardSize,
                 boardSize,
                 dims));
                 var offset = boardSize / 2 | 0;
                 var y = _v55._1 - gameHeaderSize;
                 var boardY = $Basics.toFloat(0 - ((y / tileSize | 0) - offset));
                 var x = _v55._0;
                 var boardX = $Basics.toFloat((x / tileSize | 0) - offset);
                 return {ctor: "_Tuple2"
                        ,_0: boardX
                        ,_1: boardY};
              }();}
         _E.Case($moduleName,
         "between lines 182 and 189");
      }();
   });
   var Horizontal = {ctor: "Horizontal"};
   var Vertical = {ctor: "Vertical"};
   var None = {ctor: "None"};
   var clickInput = $Graphics$Input.input(None);
   var PassButton = {ctor: "PassButton"};
   var PieceInHand = F2(function (a,
   b) {
      return {ctor: "PieceInHand"
             ,_0: a
             ,_1: b};
   });
   var Board = {ctor: "Board"};
   var Start = {ctor: "Start"};
   var NoAction = {ctor: "NoAction"};
   var Pass = {ctor: "Pass"};
   var StartGame = function (a) {
      return {ctor: "StartGame"
             ,_0: a};
   };
   var PlacePiece = F2(function (a,
   b) {
      return {ctor: "PlacePiece"
             ,_0: a
             ,_1: b};
   });
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
         return A5($Signal.lift4,
         F4(function (clickType,
         shuffledDeck,
         mousePos,
         dims) {
            return function () {
               var click = A2($Debug.watch,
               "clickInput.signal",
               clickType);
               var pos = A2($Debug.watch,
               "Mouse.position",
               mousePos);
               return function () {
                  switch (clickType.ctor)
                  {case "Board":
                     return A2(PlacePiece,
                       mousePos,
                       dims);
                     case "None": return NoAction;
                     case "PassButton": return Pass;
                     case "PieceInHand":
                     return A2(PickUpPiece,
                       clickType._0,
                       clickType._1);
                     case "Start":
                     return StartGame(shuffledDeck);}
                  _E.Case($moduleName,
                  "between lines 634 and 639");
               }();
            }();
         }),
         signal,
         shuffled,
         sampledMouse,
         $Window.dimensions);
      }();
   };
   var Cpu = {ctor: "Cpu"};
   var Human = {ctor: "Human"};
   var Blue = {ctor: "Blue"};
   var Red = {ctor: "Red"};
   var nextPlayer = function (player) {
      return function () {
         switch (player.ctor)
         {case "Blue": return Red;
            case "Red": return Blue;}
         _E.Case($moduleName,
         "between lines 395 and 397");
      }();
   };
   var drawLastPlacedOutline = F2(function (state,
   tileSize) {
      return function () {
         var _v63 = state.lastPlaced;
         switch (_v63.ctor)
         {case "Just":
            switch (_v63._0.ctor)
              {case "_Tuple2":
                 return function () {
                      var lastPlacedColor = playerColor(nextPlayer(state.turn));
                      var thick = function (c) {
                         return _U.replace([["color"
                                            ,c]
                                           ,["width",4]],
                         $Graphics$Collage.defaultLine);
                      };
                      var lastPlacedOutline = A2($Graphics$Collage.move,
                      {ctor: "_Tuple2"
                      ,_0: tileSize * _v63._0._0
                      ,_1: tileSize * _v63._0._1},
                      A2($Graphics$Collage.outlined,
                      thick(lastPlacedColor),
                      $Graphics$Collage.square(tileSize + 4)));
                      return _L.fromArray([lastPlacedOutline]);
                   }();}
              break;
            case "Nothing":
            return _L.fromArray([]);}
         _E.Case($moduleName,
         "between lines 490 and 496");
      }();
   });
   var renderBoard = F3(function (state,
   boardSize,
   dims) {
      return function () {
         var grid = A2(drawGrid,
         boardSize,
         dims);
         var tileSize = A2(getTileSizeFromBoardSize,
         boardSize,
         dims);
         var size = boardSize * $Basics.round(tileSize) + 1;
         var pieces = A2($List.map,
         function (p) {
            return A2(drawPiece,
            p,
            tileSize);
         },
         $Dict.toList(state.board));
         var outline = A2(drawLastPlacedOutline,
         state,
         tileSize);
         return A3($Graphics$Collage.collage,
         size,
         size,
         _L.append(grid,
         _L.append(pieces,outline)));
      }();
   });
   var isGameOver = function (state) {
      return A2(noTilesInHand,
      Red,
      state) && A2(noTilesInHand,
      Blue,
      state);
   };
   var startState = {_: {}
                    ,board: $Dict.empty
                    ,deck: _L.fromArray([])
                    ,delta: $Dict.fromList(_L.fromArray([{ctor: "_Tuple2"
                                                         ,_0: "red"
                                                         ,_1: ""}
                                                        ,{ctor: "_Tuple2"
                                                         ,_0: "blue"
                                                         ,_1: ""}]))
                    ,gameOver: false
                    ,hands: $Dict.fromList(_L.fromArray([{ctor: "_Tuple2"
                                                         ,_0: "red"
                                                         ,_1: _L.fromArray([])}
                                                        ,{ctor: "_Tuple2"
                                                         ,_0: "blue"
                                                         ,_1: _L.fromArray([])}]))
                    ,heldPiece: $Maybe.Nothing
                    ,lastPlaced: $Maybe.Nothing
                    ,players: $Dict.fromList(_L.fromArray([{ctor: "_Tuple2"
                                                           ,_0: "red"
                                                           ,_1: Human}
                                                          ,{ctor: "_Tuple2"
                                                           ,_0: "blue"
                                                           ,_1: Cpu}]))
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
   var getTileScore = F4(function (_v67,
   dir,
   move,
   board) {
      return function () {
         switch (_v67.ctor)
         {case "_Tuple2":
            return function () {
                 var isCurrentTile = _U.eq(move.location,
                 {ctor: "_Tuple2"
                 ,_0: _v67._0
                 ,_1: _v67._1});
                 var adjacents = A2(adjacentTiles,
                 {ctor: "_Tuple2"
                 ,_0: _v67._0
                 ,_1: _v67._1},
                 board);
                 var adjacentToLoki = A2($List.any,
                 function (loc) {
                    return _U.eq(A2($Dict.getOrFail,
                    loc,
                    board),
                    Loki);
                 },
                 adjacents);
                 var piece = A2($Dict.getOrFail,
                 {ctor: "_Tuple2"
                 ,_0: _v67._0
                 ,_1: _v67._1},
                 board);
                 return adjacentToLoki && $Basics.not(_U.eq(piece,
                 Loki)) ? 0 : function () {
                    switch (piece.ctor)
                    {case "Dragon": return 5;
                       case "Fenrir":
                       return function () {
                            var line = A2(function () {
                               switch (dir.ctor)
                               {case "Horizontal":
                                  return findRow;
                                  case "Vertical":
                                  return findColumn;}
                               _E.Case($moduleName,
                               "between lines 214 and 215");
                            }(),
                            {ctor: "_Tuple2"
                            ,_0: _v67._0
                            ,_1: _v67._1},
                            board);
                            var piecesInLine = A2($List.map,
                            function (loc) {
                               return A2($Dict.getOrFail,
                               loc,
                               board);
                            },
                            line);
                            var numOtherFenrirs = $List.length(A2($List.filter,
                            function (p) {
                               return _U.eq(p,Fenrir);
                            },
                            piecesInLine));
                            var numFenrirsToCount = numOtherFenrirs + (isCurrentTile || $Basics.not(_U.eq(move.piece,
                            Fenrir)) ? 1 : 0);
                            return 4 * numFenrirsToCount;
                         }();
                       case "Loki": return 1;
                       case "Odin": return 8;
                       case "Skadi": return 3;
                       case "Thor": return 7;
                       case "Troll": return 6;
                       case "Valkyrie":
                       return isCurrentTile && A3(hasSamePieceAtOtherEnd,
                         {ctor: "_Tuple2"
                         ,_0: _v67._0
                         ,_1: _v67._1},
                         board,
                         dir) ? 100 : 2;}
                    _E.Case($moduleName,
                    "between lines 209 and 230");
                 }();
              }();}
         _E.Case($moduleName,
         "between lines 201 and 230");
      }();
   });
   var scoreMove = F2(function (move,
   state) {
      return function () {
         var tileScoreInRow = A4(getTileScore,
         move.location,
         Horizontal,
         move,
         state.board);
         var row = A2(findRow,
         move.location,
         state.board);
         var rowSize = $List.length(row) + 1;
         var rowScores = A2($List.map,
         function (loc) {
            return A4(getTileScore,
            loc,
            Horizontal,
            move,
            state.board);
         },
         row);
         var rowHighScore = $List.isEmpty(row) ? 0 : $List.maximum(rowScores);
         var rowPoints = _U.cmp(tileScoreInRow,
         rowHighScore) > 0 && _U.cmp(rowSize,
         2) > -1 ? rowSize : 0;
         var tileScoreInColumn = A4(getTileScore,
         move.location,
         Vertical,
         move,
         state.board);
         var column = A2(findColumn,
         move.location,
         state.board);
         var columnSize = $List.length(column) + 1;
         var columnScores = A2($List.map,
         function (loc) {
            return A4(getTileScore,
            loc,
            Vertical,
            move,
            state.board);
         },
         column);
         var columnHighScore = $List.isEmpty(column) ? 0 : $List.maximum(columnScores);
         var columnPoints = _U.cmp(tileScoreInColumn,
         columnHighScore) > 0 && _U.cmp(columnSize,
         2) > -1 ? columnSize : 0;
         return columnPoints + rowPoints;
      }();
   });
   var makeMove = F2(function (move,
   state) {
      return function () {
         var existingTile = A2($Dict.get,
         move.location,
         state.board);
         var newBoard = A3($Dict.insert,
         move.location,
         move.piece,
         state.board);
         var delta = A2(scoreMove,
         move,
         _U.replace([["board",newBoard]],
         state));
         var p = playerName(state.turn);
         var newScore = A2($Dict.getOrFail,
         p,
         state.score) + delta;
         var hand = A2($Dict.getOrFail,
         p,
         state.hands);
         var handWithDrawnTile = _L.append(A2(without,
         move.idx,
         hand),
         $Basics.not($List.isEmpty(state.deck)) ? A2($List.take,
         1,
         state.deck) : _L.fromArray([]));
         var newHand = function () {
            switch (existingTile.ctor)
            {case "Just":
               return _U.eq(move.piece,
                 Skadi) ? A3(replaceAtIndex,
                 move.idx,
                 pieceToString(existingTile._0),
                 hand) : handWithDrawnTile;
               case "Nothing":
               return handWithDrawnTile;}
            _E.Case($moduleName,
            "between lines 335 and 338");
         }();
         return _U.replace([["turn"
                            ,nextPlayer(state.turn)]
                           ,["board",newBoard]
                           ,["score"
                            ,A3($Dict.insert,
                            p,
                            newScore,
                            state.score)]
                           ,["deck"
                            ,A2($List.drop,1,state.deck)]
                           ,["hands"
                            ,A3($Dict.insert,
                            p,
                            newHand,
                            state.hands)]
                           ,["started",true]
                           ,["heldPiece",$Maybe.Nothing]
                           ,["lastPlaced"
                            ,$Maybe.Just(move.location)]
                           ,["delta"
                            ,A3($Dict.insert,
                            p,
                            $String.concat(_L.fromArray(["(+"
                                                        ,$String.show(delta)
                                                        ,")"])),
                            state.delta)]],
         state);
      }();
   });
   var Dragon = {ctor: "Dragon"};
   var Troll = {ctor: "Troll"};
   var isValidMove = F2(function (move,
   state) {
      return function () {
         var adjacents = A2(adjacentTiles,
         move.location,
         state.board);
         var hasAdjacentTile = $Basics.not($List.isEmpty(adjacents));
         var adjacentToTroll = A2($List.any,
         function (loc) {
            return _U.eq(A2($Dict.getOrFail,
            loc,
            state.board),
            Troll);
         },
         adjacents);
         var rowLength = $List.length(A2(findRow,
         move.location,
         state.board)) + 1;
         var columnLength = $List.length(A2(findColumn,
         move.location,
         state.board)) + 1;
         var longestLine = A2($Basics.max,
         columnLength,
         rowLength);
         var existingTile = A2($Dict.get,
         move.location,
         state.board);
         var canOverlapExistingTile = (_U.eq(move.piece,
         Dragon) || _U.eq(move.piece,
         Skadi)) && $Basics.not(_U.eq(existingTile,
         $Maybe.Just(move.piece)));
         var isUnoccupied = $Basics.not(A2($Dict.member,
         move.location,
         state.board));
         return (isUnoccupied || canOverlapExistingTile) && (hasAdjacentTile && ($Basics.not(adjacentToTroll) && _U.cmp(longestLine,
         7) < 1));
      }();
   });
   var Thor = {ctor: "Thor"};
   var Odin = {ctor: "Odin"};
   var pieceFromString = function (str) {
      return function () {
         switch (str)
         {case "Dragon": return Dragon;
            case "Fenrir": return Fenrir;
            case "Loki": return Loki;
            case "Odin": return Odin;
            case "Skadi": return Skadi;
            case "Thor": return Thor;
            case "Troll": return Troll;
            case "Valkyrie":
            return Valkyrie;}
         _E.Case($moduleName,
         "between lines 100 and 108");
      }();
   };
   var makeCpuMove = function (state) {
      return $List.isEmpty(A2($Dict.getOrFail,
      playerName(state.turn),
      state.hands)) ? state : function () {
         var boardSize = getBoardSize(state);
         var xs = A2($List.map,
         function (x) {
            return $Basics.toFloat(x - (boardSize / 2 | 0));
         },
         _L.range(0,boardSize - 1));
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
         var validLocationsByPiece = function (piece) {
            return A2($List.filter,
            function (loc) {
               return A2(isValidMove,
               {_: {}
               ,idx: 0
               ,location: loc
               ,piece: piece},
               state);
            },
            locations);
         };
         var p = playerName(state.turn);
         var hand = A2($Dict.getOrFail,
         p,
         state.hands);
         var idxs = _L.range(0,
         $List.length(hand) - 1);
         var pieceAtIdx = function (i) {
            return pieceFromString(A2(_op["!!"],
            hand,
            i));
         };
         var validMoves = A2($List.concatMap,
         function (i) {
            return function () {
               var piece = pieceAtIdx(i);
               var move = function (loc) {
                  return {_: {}
                         ,idx: i
                         ,location: loc
                         ,piece: piece};
               };
               var stateAfterMoveTo = function (loc) {
                  return _U.replace([["board"
                                     ,A3($Dict.insert,
                                     loc,
                                     piece,
                                     state.board)]],
                  state);
               };
               var moveWithScore = function (loc) {
                  return {ctor: "_Tuple2"
                         ,_0: move(loc)
                         ,_1: A2(scoreMove,
                         move(loc),
                         stateAfterMoveTo(loc))};
               };
               return A2($List.map,
               moveWithScore,
               validLocationsByPiece(piece));
            }();
         },
         idxs);
         var $ = $List.head($Debug.watch("best moves")($List.reverse(A2($List.sortBy,
         $Basics.snd,
         validMoves)))),
         bestMove = $._0,
         bestScore = $._1;
         return A2(tryMove,
         bestMove.location,
         _U.replace([["heldPiece"
                     ,$Maybe.Just(bestMove.idx)]],
         state));
      }();
   };
   var tryMove = F2(function (location,
   state) {
      return function () {
         var _v76 = state.heldPiece;
         switch (_v76.ctor)
         {case "Just":
            return function () {
                 var nextPlayerType = A2($Dict.getOrFail,
                 playerName(nextPlayer(state.turn)),
                 state.players);
                 var nextAction = function () {
                    switch (nextPlayerType.ctor)
                    {case "Cpu": return makeCpuMove;
                       case "Human":
                       return $Basics.identity;}
                    _E.Case($moduleName,
                    "between lines 303 and 306");
                 }();
                 var p = playerName(state.turn);
                 var hand = A2($Dict.getOrFail,
                 p,
                 state.hands);
                 var pieceStr = $List.head(A2($List.drop,
                 _v76._0,
                 hand));
                 var piece = pieceFromString(pieceStr);
                 var move = {_: {}
                            ,idx: _v76._0
                            ,location: location
                            ,piece: piece};
                 return A2(isValidMove,
                 move,
                 state) ? nextAction(A2(makeMove,
                 move,
                 state)) : _U.replace([["heldPiece"
                                       ,$Maybe.Nothing]],
                 state);
              }();
            case "Nothing": return state;}
         _E.Case($moduleName,
         "between lines 295 and 308");
      }();
   });
   var startGame = function (deck) {
      return function () {
         var deckWithIndices = A2($List.zip,
         _L.range(0,
         $List.length(deck) - 1),
         deck);
         var idxFirstNonTroll = $Basics.fst($List.head(A2($List.filter,
         function (_v79) {
            return function () {
               switch (_v79.ctor)
               {case "_Tuple2":
                  return $Basics.not(_U.eq(_v79._1,
                    "troll"));}
               _E.Case($moduleName,
               "on line 434, column 66 to 87");
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
         var newState = _U.replace([["hands"
                                    ,hands]
                                   ,["deck",remainder]
                                   ,["started",true]
                                   ,["board"
                                    ,A2($Dict.singleton,
                                    {ctor: "_Tuple2",_0: 0,_1: 0},
                                    firstTile)]],
         startState);
         return _U.eq(A2($Dict.getOrFail,
         playerName(newState.turn),
         newState.players),
         Cpu) ? makeCpuMove(newState) : newState;
      }();
   };
   var performAction = F2(function (action,
   state) {
      return function () {
         var newState = function () {
            switch (action.ctor)
            {case "NoAction": return state;
               case "Pass":
               return _U.replace([["turn"
                                  ,nextPlayer(state.turn)]],
                 state);
               case "PickUpPiece":
               return A3(tryToPickUpPiece,
                 action._0,
                 action._1,
                 state);
               case "PlacePiece":
               return A2(tryMove,
                 A3(mouseToBoardPosition,
                 action._0,
                 state,
                 action._1),
                 state);
               case "StartGame":
               return startGame(action._0);}
            _E.Case($moduleName,
            "between lines 405 and 411");
         }();
         var p = playerName(state.turn);
         return isGameOver(newState) ? _U.replace([["gameOver"
                                                   ,true]],
         newState) : mustPass(newState) ? _U.replace([["turn"
                                                      ,nextPlayer(newState.turn)]
                                                     ,["delta"
                                                      ,A3($Dict.insert,
                                                      p,
                                                      "(+0)",
                                                      newState.delta)]],
         newState) : newState;
      }();
   });
   var renderHand = F2(function (player,
   state) {
      return function () {
         var hiddenPiece = A3($Graphics$Element.image,
         $Basics.round(handTileSize),
         $Basics.round(handTileSize),
         "images/tile_back.jpg");
         var pieceSize = $Basics.round(handTileSize) + handPadding;
         var pieceImage = function (pieceStr) {
            return pieceToImage(pieceFromString(pieceStr));
         };
         var isPieceHeld = function (idx) {
            return _U.eq(state.turn,
            player) && _U.eq(state.heldPiece,
            $Maybe.Just(idx));
         };
         var makePiece = F2(function (idx,
         pieceStr) {
            return A2($Graphics$Input.clickable,
            clickInput.handle,
            A2(PieceInHand,
            player,
            idx))($Graphics$Element.color(isPieceHeld(idx) ? playerColor(state.turn) : $Color.white)(A3($Graphics$Element.container,
            pieceSize,
            pieceSize,
            $Graphics$Element.middle)(A2(pieceImage,
            pieceStr,
            handTileSize))));
         });
         var p = playerName(player);
         var playerType = A2($Dict.getOrFail,
         p,
         state.players);
         var handText = A3($Graphics$Element.container,
         80,
         pieceSize,
         $Graphics$Element.middle)($Text.leftAligned($Text.color(playerColor(player))((_U.eq(state.turn,
         player) && $Basics.not(state.gameOver) ? $Text.bold : $Basics.identity)($Text.toText($String.toUpper($String.show(playerType)))))));
         var hand = A2($Dict.getOrFail,
         p,
         state.hands);
         var playerHand = $List.isEmpty(hand) && $Basics.not(state.gameOver) ? _L.fromArray([A3($Graphics$Element.container,
         100,
         100,
         $Graphics$Element.middle)(A3($Graphics$Input.button,
         clickInput.handle,
         PassButton,
         "Pass"))]) : A2($List.indexedMap,
         makePiece,
         hand);
         var cpuHand = A2($List.map,
         function (x) {
            return A3($Graphics$Element.container,
            pieceSize,
            pieceSize,
            $Graphics$Element.middle)(hiddenPiece);
         },
         hand);
         var handContents = _U.eq(A2($Dict.getOrFail,
         p,
         state.players),
         Human) ? playerHand : cpuHand;
         var score = A3($Graphics$Element.container,
         25,
         pieceSize,
         $Graphics$Element.midLeft)($Text.asText(A2($Dict.getOrFail,
         p,
         state.score)));
         var delta = A3($Graphics$Element.container,
         20,
         pieceSize,
         $Graphics$Element.midLeft)($Text.leftAligned($Text.height(9)($Text.toText(A2($Dict.getOrFail,
         p,
         state.delta)))));
         return A2($Graphics$Element.flow,
         $Graphics$Element.right,
         _L.append(_L.fromArray([handText]),
         _L.append(_L.fromArray([score]),
         _L.append(_L.fromArray([delta]),
         handContents))));
      }();
   });
   var display = F2(function (state,
   dims) {
      return function () {
         var pieceRules = A2($Graphics$Element.flow,
         $Graphics$Element.down,
         _L.fromArray([A3(rulesRow,
                      Odin,
                      8,
                      "No special power")
                      ,A3(rulesRow,
                      Thor,
                      7,
                      "No special power")
                      ,A3(rulesRow,
                      Troll,
                      6,
                      "No other tiles may be placed adjacent to a Troll.")
                      ,A3(rulesRow,
                      Dragon,
                      5,
                      "May be placed on top of other tiles (except other Dragons).")
                      ,A3(rulesRow,
                      Fenrir,
                      4,
                      "Value is the sum of all Fenrir tiles in the same row or column.")
                      ,A3(rulesRow,
                      Skadi,
                      3,
                      "You may exchange it with any tile on the table (except other Skadi).")
                      ,A3(rulesRow,
                      Valkyrie,
                      2,
                      "Automatically scores when there are Valkyries on both ends of a line.")
                      ,A3(rulesRow,
                      Loki,
                      1,
                      "All tiles adjacent to Loki have value 1.")]));
         var minRulesHeight = 570;
         var rulesAreaWidth = 650;
         var startButton = A3($Graphics$Element.container,
         rulesAreaWidth,
         50,
         $Graphics$Element.middle)(A3($Graphics$Input.button,
         clickInput.handle,
         Start,
         $Basics.not(state.started) ? "Begin game!" : "Restart game"));
         var rulesArea = A2($Graphics$Element.flow,
         $Graphics$Element.down,
         _L.fromArray([A2($Graphics$Element.size,
                      rulesAreaWidth,
                      50)($Text.centered(A2($Text.height,
                      25,
                      A2($Text.typeface,
                      _L.fromArray(["Rock Salt"
                                   ,"cursive"]),
                      $Text.toText("Rules")))))
                      ,A2($Graphics$Element.spacer,
                      5,
                      5)
                      ,$Graphics$Element.width(rulesAreaWidth)($Text.leftAligned($Text.toText("&bull; Players take turns placing tiles from their hand. You must place a tile next to an existing tile. Rows and columns cannot exceed seven tiles.")))
                      ,$Graphics$Element.width(rulesAreaWidth)($Text.leftAligned($Text.toText("&bull; If the tile you placed has the highest value in a row and/or column (ties don\'t count), you score one point for each tile in that row and/or column.")))
                      ,A2($Graphics$Element.spacer,
                      5,
                      5)
                      ,pieceRules
                      ,startButton]));
         var withSpacing = F2(function (padding,
         elt) {
            return A2($Graphics$Element.beside,
            A2($Graphics$Element.spacer,
            padding,
            padding),
            elt);
         });
         var totalBoardSize = getTotalBoardSize(dims);
         var handGap = totalBoardSize - 2 * $Basics.round(handTileSize) - handPadding * 2;
         var rightArea = _U.cmp(handGap,
         570) > -1 ? rulesArea : _U.cmp(handGap,
         440) > -1 ? A2($Graphics$Element.above,
         pieceRules,
         startButton) : startButton;
         var boardSize = getBoardSize(state);
         var tileSize = A2(getTileSizeFromBoardSize,
         boardSize,
         dims);
         return A2($Graphics$Element.flow,
         $Graphics$Element.down,
         _L.fromArray([A3($Graphics$Element.size,
                      totalBoardSize,
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
                                   Board)(A3(renderBoard,
                                   state,
                                   boardSize,
                                   dims))
                                   ,A2($Graphics$Element.flow,
                                   $Graphics$Element.down,
                                   _L.fromArray([A2(renderHand,
                                                Red,
                                                state)
                                                ,A2($Graphics$Element.spacer,
                                                1,
                                                5)
                                                ,A2(withSpacing,
                                                10,
                                                $Graphics$Element.color($Color.gray)(A2(withSpacing,
                                                10,
                                                A4($Graphics$Element.container,
                                                rulesAreaWidth,
                                                handGap - 10,
                                                $Graphics$Element.midLeft,
                                                rightArea))))
                                                ,A2($Graphics$Element.spacer,
                                                1,
                                                5)
                                                ,A2(renderHand,Blue,state)]))]))
                      ,$Text.asText(state)]));
      }();
   });
   var main = function () {
      var state = A3($Signal.foldp,
      performAction,
      startState,
      processClick(clickInput.signal));
      return A2($Signal._op["~"],
      A2($Signal._op["<~"],
      display,
      state),
      $Window.dimensions);
   }();
   var Move = F3(function (a,b,c) {
      return {_: {}
             ,idx: b
             ,location: c
             ,piece: a};
   });
   var State = function (a) {
      return function (b) {
         return function (c) {
            return function (d) {
               return function (e) {
                  return function (f) {
                     return function (g) {
                        return function (h) {
                           return function (i) {
                              return function (j) {
                                 return function (k) {
                                    return {_: {}
                                           ,board: c
                                           ,deck: e
                                           ,delta: j
                                           ,gameOver: k
                                           ,hands: f
                                           ,heldPiece: h
                                           ,lastPlaced: i
                                           ,players: a
                                           ,score: d
                                           ,started: g
                                           ,turn: b};
                                 };
                              };
                           };
                        };
                     };
                  };
               };
            };
         };
      };
   };
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
                         ,Human: Human
                         ,Cpu: Cpu
                         ,PickUpPiece: PickUpPiece
                         ,PlacePiece: PlacePiece
                         ,StartGame: StartGame
                         ,Pass: Pass
                         ,NoAction: NoAction
                         ,Start: Start
                         ,Board: Board
                         ,PieceInHand: PieceInHand
                         ,PassButton: PassButton
                         ,None: None
                         ,Vertical: Vertical
                         ,Horizontal: Horizontal
                         ,gameHeaderSize: gameHeaderSize
                         ,handPadding: handPadding
                         ,handTileSize: handTileSize
                         ,playerName: playerName
                         ,playerColor: playerColor
                         ,pieceFromString: pieceFromString
                         ,pieceToString: pieceToString
                         ,repeat: repeat
                         ,without: without
                         ,replaceAtIndex: replaceAtIndex
                         ,shuffle: shuffle
                         ,getBoardSize: getBoardSize
                         ,getTotalBoardSize: getTotalBoardSize
                         ,getTileSizeFromBoardSize: getTileSizeFromBoardSize
                         ,mouseToBoardPosition: mouseToBoardPosition
                         ,isAdjacent: isAdjacent
                         ,adjacentTiles: adjacentTiles
                         ,getTileScore: getTileScore
                         ,findColumn: findColumn
                         ,findRow: findRow
                         ,findAbove: findAbove
                         ,findBelow: findBelow
                         ,findLeftward: findLeftward
                         ,findRightward: findRightward
                         ,hasSamePieceAtOtherEnd: hasSamePieceAtOtherEnd
                         ,tryToPickUpPiece: tryToPickUpPiece
                         ,pickUpPiece: pickUpPiece
                         ,tryMove: tryMove
                         ,isValidMove: isValidMove
                         ,makeMove: makeMove
                         ,scoreMove: scoreMove
                         ,makeCpuMove: makeCpuMove
                         ,nextPlayer: nextPlayer
                         ,performAction: performAction
                         ,noTilesInHand: noTilesInHand
                         ,isGameOver: isGameOver
                         ,mustPass: mustPass
                         ,startGame: startGame
                         ,clickInput: clickInput
                         ,pieceToImage: pieceToImage
                         ,drawGrid: drawGrid
                         ,drawPiece: drawPiece
                         ,drawLastPlacedOutline: drawLastPlacedOutline
                         ,renderBoard: renderBoard
                         ,renderHand: renderHand
                         ,rulesRow: rulesRow
                         ,display: display
                         ,deckContents: deckContents
                         ,startState: startState
                         ,processClick: processClick
                         ,main: main};
   return _elm.Voluspa.values;
};