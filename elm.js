var Elm = Elm || { Native: {} };
Elm.AI = Elm.AI || {};
Elm.AI.make = function (_elm) {
   "use strict";
   _elm.AI = _elm.AI || {};
   if (_elm.AI.values)
   return _elm.AI.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _P = _N.Ports.make(_elm),
   $moduleName = "AI",
   $Basics = Elm.Basics.make(_elm),
   $Board = Elm.Board.make(_elm),
   $Dict = Elm.Dict.make(_elm),
   $GameTypes = Elm.GameTypes.make(_elm),
   $Helpers = Elm.Helpers.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Piece = Elm.Piece.make(_elm),
   $Player = Elm.Player.make(_elm);
   var getMove = function (state) {
      return $List.isEmpty(A2($Player.getHand,
      state.turn,
      state)) ? $Maybe.Nothing : function () {
         var boardSize = $Board.getBoardSize(state.board);
         var xs = A2($List.map,
         function (x) {
            return x - (boardSize / 2 | 0);
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
               return A2($Board.isValidMove,
               {_: {}
               ,idx: 0
               ,location: loc
               ,piece: piece},
               state.board);
            },
            locations);
         };
         var hand = A2($Player.getHand,
         state.turn,
         state);
         var idxs = _L.range(0,
         $List.length(hand) - 1);
         var pieceAtIdx = function (i) {
            return $Piece.fromString(A2($Helpers._op["!!"],
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
               var boardAfterMoveTo = function (loc) {
                  return A3($Dict.insert,
                  loc,
                  piece,
                  state.board);
               };
               var moveWithScore = function (loc) {
                  return {ctor: "_Tuple2"
                         ,_0: move(loc)
                         ,_1: A2($Board.scoreMove,
                         move(loc),
                         boardAfterMoveTo(loc))};
               };
               return A2($List.map,
               moveWithScore,
               validLocationsByPiece(piece));
            }();
         },
         idxs);
         var _ = $List.head($List.reverse(A2($List.sortBy,
         $Basics.snd,
         validMoves)));
         var bestMove = function () {
            switch (_.ctor)
            {case "_Tuple2": return _._0;}
            _U.badCase($moduleName,
            "between lines 36 and 38");
         }();
         var p = $Player.color(state.turn);
         return $Maybe.Just(bestMove);
      }();
   };
   _elm.AI.values = {_op: _op
                    ,getMove: getMove};
   return _elm.AI.values;
};
Elm.Array = Elm.Array || {};
Elm.Array.make = function (_elm) {
   "use strict";
   _elm.Array = _elm.Array || {};
   if (_elm.Array.values)
   return _elm.Array.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _P = _N.Ports.make(_elm),
   $moduleName = "Array",
   $Basics = Elm.Basics.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Array = Elm.Native.Array.make(_elm);
   var append = $Native$Array.append;
   var length = $Native$Array.length;
   var slice = $Native$Array.slice;
   var set = $Native$Array.set;
   var get = F2(function (i,
   array) {
      return _U.cmp(0,
      i) < 1 && _U.cmp(i,
      $Native$Array.length(array)) < 0 ? $Maybe.Just(A2($Native$Array.get,
      i,
      array)) : $Maybe.Nothing;
   });
   var push = $Native$Array.push;
   var empty = $Native$Array.empty;
   var filter = F2(function (isOkay,
   arr) {
      return function () {
         var update = F2(function (x,
         xs) {
            return isOkay(x) ? A2($Native$Array.push,
            x,
            xs) : xs;
         });
         return A3($Native$Array.foldl,
         update,
         $Native$Array.empty,
         arr);
      }();
   });
   var foldr = $Native$Array.foldr;
   var foldl = $Native$Array.foldl;
   var indexedMap = $Native$Array.indexedMap;
   var map = $Native$Array.map;
   var toIndexedList = function (array) {
      return A3($List.map2,
      F2(function (v0,v1) {
         return {ctor: "_Tuple2"
                ,_0: v0
                ,_1: v1};
      }),
      _L.range(0,
      $Native$Array.length(array) - 1),
      $Native$Array.toList(array));
   };
   var toList = $Native$Array.toList;
   var fromList = $Native$Array.fromList;
   var initialize = $Native$Array.initialize;
   var repeat = F2(function (n,e) {
      return A2(initialize,
      n,
      $Basics.always(e));
   });
   var Array = {ctor: "Array"};
   _elm.Array.values = {_op: _op
                       ,Array: Array
                       ,initialize: initialize
                       ,repeat: repeat
                       ,fromList: fromList
                       ,toList: toList
                       ,toIndexedList: toIndexedList
                       ,map: map
                       ,indexedMap: indexedMap
                       ,foldl: foldl
                       ,foldr: foldr
                       ,filter: filter
                       ,empty: empty
                       ,push: push
                       ,get: get
                       ,set: set
                       ,slice: slice
                       ,length: length
                       ,append: append};
   return _elm.Array.values;
};
Elm.Basics = Elm.Basics || {};
Elm.Basics.make = function (_elm) {
   "use strict";
   _elm.Basics = _elm.Basics || {};
   if (_elm.Basics.values)
   return _elm.Basics.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _P = _N.Ports.make(_elm),
   $moduleName = "Basics",
   $Native$Basics = Elm.Native.Basics.make(_elm),
   $Native$Show = Elm.Native.Show.make(_elm),
   $Native$Utils = Elm.Native.Utils.make(_elm);
   var uncurry = F2(function (f,
   _v0) {
      return function () {
         switch (_v0.ctor)
         {case "_Tuple2": return A2(f,
              _v0._0,
              _v0._1);}
         _U.badCase($moduleName,
         "on line 472, column 19 to 24");
      }();
   });
   var curry = F3(function (f,
   a,
   b) {
      return f({ctor: "_Tuple2"
               ,_0: a
               ,_1: b});
   });
   var flip = F3(function (f,b,a) {
      return A2(f,a,b);
   });
   var snd = function (_v4) {
      return function () {
         switch (_v4.ctor)
         {case "_Tuple2": return _v4._1;}
         _U.badCase($moduleName,
         "on line 456, column 13 to 14");
      }();
   };
   var fst = function (_v8) {
      return function () {
         switch (_v8.ctor)
         {case "_Tuple2": return _v8._0;}
         _U.badCase($moduleName,
         "on line 452, column 13 to 14");
      }();
   };
   var always = F2(function (a,
   _v12) {
      return function () {
         return a;
      }();
   });
   var identity = function (x) {
      return x;
   };
   _op["<|"] = F2(function (f,x) {
      return f(x);
   });
   _op["|>"] = F2(function (x,f) {
      return f(x);
   });
   _op[">>"] = F3(function (f,
   g,
   x) {
      return g(f(x));
   });
   _op["<<"] = F3(function (g,
   f,
   x) {
      return g(f(x));
   });
   _op["++"] = $Native$Utils.append;
   var toString = $Native$Show.toString;
   var isInfinite = $Native$Basics.isInfinite;
   var isNaN = $Native$Basics.isNaN;
   var toFloat = $Native$Basics.toFloat;
   var ceiling = $Native$Basics.ceiling;
   var floor = $Native$Basics.floor;
   var truncate = $Native$Basics.truncate;
   var round = $Native$Basics.round;
   var otherwise = true;
   var not = $Native$Basics.not;
   var xor = $Native$Basics.xor;
   _op["||"] = $Native$Basics.or;
   _op["&&"] = $Native$Basics.and;
   var max = $Native$Basics.max;
   var min = $Native$Basics.min;
   var GT = {ctor: "GT"};
   var EQ = {ctor: "EQ"};
   var LT = {ctor: "LT"};
   var compare = $Native$Basics.compare;
   _op[">="] = $Native$Basics.ge;
   _op["<="] = $Native$Basics.le;
   _op[">"] = $Native$Basics.gt;
   _op["<"] = $Native$Basics.lt;
   _op["/="] = $Native$Basics.neq;
   _op["=="] = $Native$Basics.eq;
   var e = $Native$Basics.e;
   var pi = $Native$Basics.pi;
   var clamp = $Native$Basics.clamp;
   var logBase = $Native$Basics.logBase;
   var abs = $Native$Basics.abs;
   var negate = $Native$Basics.negate;
   var sqrt = $Native$Basics.sqrt;
   var atan2 = $Native$Basics.atan2;
   var atan = $Native$Basics.atan;
   var asin = $Native$Basics.asin;
   var acos = $Native$Basics.acos;
   var tan = $Native$Basics.tan;
   var sin = $Native$Basics.sin;
   var cos = $Native$Basics.cos;
   _op["^"] = $Native$Basics.exp;
   _op["%"] = $Native$Basics.mod;
   var rem = $Native$Basics.rem;
   _op["//"] = $Native$Basics.div;
   _op["/"] = $Native$Basics.floatDiv;
   _op["*"] = $Native$Basics.mul;
   _op["-"] = $Native$Basics.sub;
   _op["+"] = $Native$Basics.add;
   var toPolar = $Native$Basics.toPolar;
   var fromPolar = $Native$Basics.fromPolar;
   var turns = $Native$Basics.turns;
   var degrees = $Native$Basics.degrees;
   var radians = function (t) {
      return t;
   };
   _elm.Basics.values = {_op: _op
                        ,max: max
                        ,min: min
                        ,compare: compare
                        ,not: not
                        ,xor: xor
                        ,otherwise: otherwise
                        ,rem: rem
                        ,negate: negate
                        ,abs: abs
                        ,sqrt: sqrt
                        ,clamp: clamp
                        ,logBase: logBase
                        ,e: e
                        ,pi: pi
                        ,cos: cos
                        ,sin: sin
                        ,tan: tan
                        ,acos: acos
                        ,asin: asin
                        ,atan: atan
                        ,atan2: atan2
                        ,round: round
                        ,floor: floor
                        ,ceiling: ceiling
                        ,truncate: truncate
                        ,toFloat: toFloat
                        ,degrees: degrees
                        ,radians: radians
                        ,turns: turns
                        ,toPolar: toPolar
                        ,fromPolar: fromPolar
                        ,isNaN: isNaN
                        ,isInfinite: isInfinite
                        ,toString: toString
                        ,fst: fst
                        ,snd: snd
                        ,identity: identity
                        ,always: always
                        ,flip: flip
                        ,curry: curry
                        ,uncurry: uncurry
                        ,LT: LT
                        ,EQ: EQ
                        ,GT: GT};
   return _elm.Basics.values;
};
Elm.Board = Elm.Board || {};
Elm.Board.make = function (_elm) {
   "use strict";
   _elm.Board = _elm.Board || {};
   if (_elm.Board.values)
   return _elm.Board.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _P = _N.Ports.make(_elm),
   $moduleName = "Board",
   $Basics = Elm.Basics.make(_elm),
   $Dict = Elm.Dict.make(_elm),
   $GameTypes = Elm.GameTypes.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Piece = Elm.Piece.make(_elm);
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
                 _U.badCase($moduleName,
                 "on line 63, column 4 to 70");
              }();}
         _U.badCase($moduleName,
         "on line 63, column 4 to 70");
      }();
   });
   var adjacentTiles = F2(function (_v8,
   board) {
      return function () {
         switch (_v8.ctor)
         {case "_Tuple2":
            return A2($List.filter,
              function (loc) {
                 return A2(isAdjacent,
                 loc,
                 {ctor: "_Tuple2"
                 ,_0: _v8._0
                 ,_1: _v8._1});
              },
              $Dict.keys(board));}
         _U.badCase($moduleName,
         "on line 67, column 3 to 58");
      }();
   });
   var findRightward = F2(function (_v12,
   board) {
      return function () {
         switch (_v12.ctor)
         {case "_Tuple2":
            return A2($Dict.member,
              {ctor: "_Tuple2"
              ,_0: _v12._0
              ,_1: _v12._1},
              board) ? A2($Basics._op["++"],
              _L.fromArray([{ctor: "_Tuple2"
                            ,_0: _v12._0
                            ,_1: _v12._1}]),
              A2(findRightward,
              {ctor: "_Tuple2"
              ,_0: _v12._0 + 1
              ,_1: _v12._1},
              board)) : _L.fromArray([]);}
         _U.badCase($moduleName,
         "between lines 51 and 53");
      }();
   });
   var findLeftward = F2(function (_v16,
   board) {
      return function () {
         switch (_v16.ctor)
         {case "_Tuple2":
            return A2($Dict.member,
              {ctor: "_Tuple2"
              ,_0: _v16._0
              ,_1: _v16._1},
              board) ? A2($Basics._op["++"],
              _L.fromArray([{ctor: "_Tuple2"
                            ,_0: _v16._0
                            ,_1: _v16._1}]),
              A2(findLeftward,
              {ctor: "_Tuple2"
              ,_0: _v16._0 - 1
              ,_1: _v16._1},
              board)) : _L.fromArray([]);}
         _U.badCase($moduleName,
         "between lines 45 and 47");
      }();
   });
   var findRow = F2(function (_v20,
   board) {
      return function () {
         switch (_v20.ctor)
         {case "_Tuple2":
            return A2($Basics._op["++"],
              A2(findLeftward,
              {ctor: "_Tuple2"
              ,_0: _v20._0 - 1
              ,_1: _v20._1},
              board),
              A2(findRightward,
              {ctor: "_Tuple2"
              ,_0: _v20._0 + 1
              ,_1: _v20._1},
              board));}
         _U.badCase($moduleName,
         "on line 59, column 24 to 83");
      }();
   });
   var findBelow = F2(function (_v24,
   board) {
      return function () {
         switch (_v24.ctor)
         {case "_Tuple2":
            return A2($Dict.member,
              {ctor: "_Tuple2"
              ,_0: _v24._0
              ,_1: _v24._1},
              board) ? A2($Basics._op["++"],
              _L.fromArray([{ctor: "_Tuple2"
                            ,_0: _v24._0
                            ,_1: _v24._1}]),
              A2(findBelow,
              {ctor: "_Tuple2"
              ,_0: _v24._0
              ,_1: _v24._1 + 1},
              board)) : _L.fromArray([]);}
         _U.badCase($moduleName,
         "between lines 39 and 41");
      }();
   });
   var findAbove = F2(function (_v28,
   board) {
      return function () {
         switch (_v28.ctor)
         {case "_Tuple2":
            return A2($Dict.member,
              {ctor: "_Tuple2"
              ,_0: _v28._0
              ,_1: _v28._1},
              board) ? A2($Basics._op["++"],
              _L.fromArray([{ctor: "_Tuple2"
                            ,_0: _v28._0
                            ,_1: _v28._1}]),
              A2(findAbove,
              {ctor: "_Tuple2"
              ,_0: _v28._0
              ,_1: _v28._1 - 1},
              board)) : _L.fromArray([]);}
         _U.badCase($moduleName,
         "between lines 33 and 35");
      }();
   });
   var findColumn = F2(function (_v32,
   board) {
      return function () {
         switch (_v32.ctor)
         {case "_Tuple2":
            return A2($Basics._op["++"],
              A2(findAbove,
              {ctor: "_Tuple2"
              ,_0: _v32._0
              ,_1: _v32._1 - 1},
              board),
              A2(findBelow,
              {ctor: "_Tuple2"
              ,_0: _v32._0
              ,_1: _v32._1 + 1},
              board));}
         _U.badCase($moduleName,
         "on line 56, column 27 to 79");
      }();
   });
   var pieceAt = F2(function (loc,
   board) {
      return A2($Maybe.withDefault,
      $GameTypes.NoPiece,
      A2($Dict.get,loc,board));
   });
   var isValidMove = F2(function (move,
   board) {
      return function () {
         var adjacents = A2(adjacentTiles,
         move.location,
         board);
         var hasAdjacentTile = $Basics.not($List.isEmpty(adjacents));
         var adjacentToTroll = A2($List.any,
         function (loc) {
            return _U.eq(A2(pieceAt,
            loc,
            board),
            $GameTypes.Troll);
         },
         adjacents);
         var rowLength = $List.length(A2(findRow,
         move.location,
         board)) + 1;
         var columnLength = $List.length(A2(findColumn,
         move.location,
         board)) + 1;
         var longestLine = A2($Basics.max,
         columnLength,
         rowLength);
         var existingTile = A2($Dict.get,
         move.location,
         board);
         var canOverlapExistingTile = (_U.eq(move.piece,
         $GameTypes.Dragon) || _U.eq(move.piece,
         $GameTypes.Skadi)) && $Basics.not(_U.eq(existingTile,
         $Maybe.Just(move.piece)));
         var isUnoccupied = $Basics.not(A2($Dict.member,
         move.location,
         board));
         return (isUnoccupied || canOverlapExistingTile) && (hasAdjacentTile && (($Basics.not(adjacentToTroll) || _U.eq(move.piece,
         $GameTypes.Troll)) && _U.cmp(longestLine,
         7) < 1));
      }();
   });
   var hasSamePieceAtOtherEnd = F3(function (_v36,
   board,
   dir) {
      return function () {
         switch (_v36.ctor)
         {case "_Tuple2":
            return function () {
                 var right = A2(findRightward,
                 {ctor: "_Tuple2"
                 ,_0: _v36._0 + 1
                 ,_1: _v36._1},
                 board);
                 var left = A2(findLeftward,
                 {ctor: "_Tuple2"
                 ,_0: _v36._0 - 1
                 ,_1: _v36._1},
                 board);
                 var below = A2(findBelow,
                 {ctor: "_Tuple2"
                 ,_0: _v36._0
                 ,_1: _v36._1 + 1},
                 board);
                 var above = A2(findAbove,
                 {ctor: "_Tuple2"
                 ,_0: _v36._0
                 ,_1: _v36._1 - 1},
                 board);
                 var samePieces = F2(function (pos1,
                 pos2) {
                    return _U.eq(A2(pieceAt,
                    pos1,
                    board),
                    A2(pieceAt,pos2,board));
                 });
                 var last = function (list) {
                    return $List.head($List.reverse(list));
                 };
                 var samePieceBelow = $List.isEmpty(above) && ($Basics.not($List.isEmpty(below)) && A2(samePieces,
                 last(below),
                 {ctor: "_Tuple2"
                 ,_0: _v36._0
                 ,_1: _v36._1}));
                 var samePieceAbove = $List.isEmpty(below) && ($Basics.not($List.isEmpty(above)) && A2(samePieces,
                 last(above),
                 {ctor: "_Tuple2"
                 ,_0: _v36._0
                 ,_1: _v36._1}));
                 var samePieceLeft = $List.isEmpty(right) && ($Basics.not($List.isEmpty(left)) && A2(samePieces,
                 last(left),
                 {ctor: "_Tuple2"
                 ,_0: _v36._0
                 ,_1: _v36._1}));
                 var samePieceRight = $List.isEmpty(left) && ($Basics.not($List.isEmpty(right)) && A2(samePieces,
                 last(right),
                 {ctor: "_Tuple2"
                 ,_0: _v36._0
                 ,_1: _v36._1}));
                 return function () {
                    switch (dir.ctor)
                    {case "Horizontal":
                       return samePieceLeft || samePieceRight;
                       case "Vertical":
                       return samePieceBelow || samePieceAbove;}
                    _U.badCase($moduleName,
                    "between lines 142 and 144");
                 }();
              }();}
         _U.badCase($moduleName,
         "between lines 131 and 144");
      }();
   });
   var getTileValue = F4(function (_v41,
   dir,
   move,
   board) {
      return function () {
         switch (_v41.ctor)
         {case "_Tuple2":
            return function () {
                 var isCurrentTile = _U.eq(move.location,
                 {ctor: "_Tuple2"
                 ,_0: _v41._0
                 ,_1: _v41._1});
                 var adjacentToLoki = function (loc) {
                    return $List.any(function (l) {
                       return _U.eq(A2(pieceAt,
                       l,
                       board),
                       $GameTypes.Loki);
                    })(A2(adjacentTiles,loc,board));
                 };
                 var piece = A2(pieceAt,
                 {ctor: "_Tuple2"
                 ,_0: _v41._0
                 ,_1: _v41._1},
                 board);
                 return _U.eq(piece,
                 $GameTypes.Fenrir) ? function () {
                    var line = A2($Basics._op["++"],
                    A2(function () {
                       switch (dir.ctor)
                       {case "Horizontal":
                          return findRow;
                          case "Vertical":
                          return findColumn;}
                       _U.badCase($moduleName,
                       "between lines 113 and 114");
                    }(),
                    {ctor: "_Tuple2"
                    ,_0: _v41._0
                    ,_1: _v41._1},
                    board),
                    _L.fromArray([{ctor: "_Tuple2"
                                  ,_0: _v41._0
                                  ,_1: _v41._1}]));
                    var numFenrirs = $List.length(A2($List.filter,
                    function (loc) {
                       return $Basics.not(adjacentToLoki(loc)) && (_U.eq(A2(pieceAt,
                       loc,
                       board),
                       $GameTypes.Fenrir) && ($Basics.not(_U.eq(loc,
                       move.location)) || isCurrentTile));
                    },
                    line));
                    return 4 * numFenrirs;
                 }() : _U.eq(piece,
                 $GameTypes.Valkyrie) && (isCurrentTile && A3(hasSamePieceAtOtherEnd,
                 {ctor: "_Tuple2"
                 ,_0: _v41._0
                 ,_1: _v41._1},
                 board,
                 dir)) ? 100 : adjacentToLoki({ctor: "_Tuple2"
                                              ,_0: _v41._0
                                              ,_1: _v41._1}) && $Basics.not(_U.eq(piece,
                 $GameTypes.Loki)) ? 0 : $Piece.baseValue(piece);
              }();}
         _U.badCase($moduleName,
         "between lines 108 and 126");
      }();
   });
   var getBoardSize = function (board) {
      return $List.isEmpty($Dict.toList(board)) ? 5 : function () {
         var locations = $Dict.keys(board);
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
   var Horizontal = {ctor: "Horizontal"};
   var Vertical = {ctor: "Vertical"};
   var scoreMove = F2(function (move,
   board) {
      return function () {
         var tileScoreInRow = A4(getTileValue,
         move.location,
         Horizontal,
         move,
         board);
         var row = A2(findRow,
         move.location,
         board);
         var rowSize = $List.length(row) + 1;
         var rowScores = A2($List.map,
         function (loc) {
            return A4(getTileValue,
            loc,
            Horizontal,
            move,
            board);
         },
         row);
         var rowHighScore = $List.isEmpty(row) ? 0 : $List.maximum(rowScores);
         var rowPoints = _U.cmp(tileScoreInRow,
         rowHighScore) > 0 && _U.cmp(rowSize,
         2) > -1 ? rowSize : 0;
         var tileScoreInColumn = A4(getTileValue,
         move.location,
         Vertical,
         move,
         board);
         var column = A2(findColumn,
         move.location,
         board);
         var columnSize = $List.length(column) + 1;
         var columnScores = A2($List.map,
         function (loc) {
            return A4(getTileValue,
            loc,
            Vertical,
            move,
            board);
         },
         column);
         var columnHighScore = $List.isEmpty(column) ? 0 : $List.maximum(columnScores);
         var columnPoints = _U.cmp(tileScoreInColumn,
         columnHighScore) > 0 && _U.cmp(columnSize,
         2) > -1 ? columnSize : 0;
         return columnPoints + rowPoints;
      }();
   });
   _elm.Board.values = {_op: _op
                       ,Vertical: Vertical
                       ,Horizontal: Horizontal
                       ,getBoardSize: getBoardSize
                       ,pieceAt: pieceAt
                       ,findAbove: findAbove
                       ,findBelow: findBelow
                       ,findLeftward: findLeftward
                       ,findRightward: findRightward
                       ,findColumn: findColumn
                       ,findRow: findRow
                       ,isAdjacent: isAdjacent
                       ,adjacentTiles: adjacentTiles
                       ,isValidMove: isValidMove
                       ,scoreMove: scoreMove
                       ,getTileValue: getTileValue
                       ,hasSamePieceAtOtherEnd: hasSamePieceAtOtherEnd};
   return _elm.Board.values;
};
Elm.Char = Elm.Char || {};
Elm.Char.make = function (_elm) {
   "use strict";
   _elm.Char = _elm.Char || {};
   if (_elm.Char.values)
   return _elm.Char.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _P = _N.Ports.make(_elm),
   $moduleName = "Char",
   $Native$Char = Elm.Native.Char.make(_elm);
   var fromCode = $Native$Char.fromCode;
   var toCode = $Native$Char.toCode;
   var toLocaleLower = $Native$Char.toLocaleLower;
   var toLocaleUpper = $Native$Char.toLocaleUpper;
   var toLower = $Native$Char.toLower;
   var toUpper = $Native$Char.toUpper;
   var isHexDigit = $Native$Char.isHexDigit;
   var isOctDigit = $Native$Char.isOctDigit;
   var isDigit = $Native$Char.isDigit;
   var isLower = $Native$Char.isLower;
   var isUpper = $Native$Char.isUpper;
   _elm.Char.values = {_op: _op
                      ,isUpper: isUpper
                      ,isLower: isLower
                      ,isDigit: isDigit
                      ,isOctDigit: isOctDigit
                      ,isHexDigit: isHexDigit
                      ,toUpper: toUpper
                      ,toLower: toLower
                      ,toLocaleUpper: toLocaleUpper
                      ,toLocaleLower: toLocaleLower
                      ,toCode: toCode
                      ,fromCode: fromCode};
   return _elm.Char.values;
};
Elm.Color = Elm.Color || {};
Elm.Color.make = function (_elm) {
   "use strict";
   _elm.Color = _elm.Color || {};
   if (_elm.Color.values)
   return _elm.Color.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _P = _N.Ports.make(_elm),
   $moduleName = "Color",
   $Basics = Elm.Basics.make(_elm);
   var Radial = F5(function (a,
   b,
   c,
   d,
   e) {
      return {ctor: "Radial"
             ,_0: a
             ,_1: b
             ,_2: c
             ,_3: d
             ,_4: e};
   });
   var radial = Radial;
   var Linear = F3(function (a,
   b,
   c) {
      return {ctor: "Linear"
             ,_0: a
             ,_1: b
             ,_2: c};
   });
   var linear = Linear;
   var fmod = F2(function (f,n) {
      return function () {
         var integer = $Basics.floor(f);
         return $Basics.toFloat(A2($Basics._op["%"],
         integer,
         n)) + f - $Basics.toFloat(integer);
      }();
   });
   var rgbToHsl = F3(function (red,
   green,
   blue) {
      return function () {
         var b = $Basics.toFloat(blue) / 255;
         var g = $Basics.toFloat(green) / 255;
         var r = $Basics.toFloat(red) / 255;
         var cMax = A2($Basics.max,
         A2($Basics.max,r,g),
         b);
         var cMin = A2($Basics.min,
         A2($Basics.min,r,g),
         b);
         var c = cMax - cMin;
         var lightness = (cMax + cMin) / 2;
         var saturation = _U.eq(lightness,
         0) ? 0 : c / (1 - $Basics.abs(2 * lightness - 1));
         var hue = $Basics.degrees(60) * (_U.eq(cMax,
         r) ? A2(fmod,
         (g - b) / c,
         6) : _U.eq(cMax,
         g) ? (b - r) / c + 2 : _U.eq(cMax,
         b) ? (r - g) / c + 4 : _U.badIf($moduleName,
         "between lines 141 and 143"));
         return {ctor: "_Tuple3"
                ,_0: hue
                ,_1: saturation
                ,_2: lightness};
      }();
   });
   var hslToRgb = F3(function (hue,
   saturation,
   lightness) {
      return function () {
         var hue$ = hue / $Basics.degrees(60);
         var chroma = (1 - $Basics.abs(2 * lightness - 1)) * saturation;
         var x = chroma * (1 - $Basics.abs(A2(fmod,
         hue$,
         2) - 1));
         var $ = _U.cmp(hue$,
         0) < 0 ? {ctor: "_Tuple3"
                  ,_0: 0
                  ,_1: 0
                  ,_2: 0} : _U.cmp(hue$,
         1) < 0 ? {ctor: "_Tuple3"
                  ,_0: chroma
                  ,_1: x
                  ,_2: 0} : _U.cmp(hue$,
         2) < 0 ? {ctor: "_Tuple3"
                  ,_0: x
                  ,_1: chroma
                  ,_2: 0} : _U.cmp(hue$,
         3) < 0 ? {ctor: "_Tuple3"
                  ,_0: 0
                  ,_1: chroma
                  ,_2: x} : _U.cmp(hue$,
         4) < 0 ? {ctor: "_Tuple3"
                  ,_0: 0
                  ,_1: x
                  ,_2: chroma} : _U.cmp(hue$,
         5) < 0 ? {ctor: "_Tuple3"
                  ,_0: x
                  ,_1: 0
                  ,_2: chroma} : _U.cmp(hue$,
         6) < 0 ? {ctor: "_Tuple3"
                  ,_0: chroma
                  ,_1: 0
                  ,_2: x} : {ctor: "_Tuple3"
                            ,_0: 0
                            ,_1: 0
                            ,_2: 0},
         r = $._0,
         g = $._1,
         b = $._2;
         var m = lightness - chroma / 2;
         return {ctor: "_Tuple3"
                ,_0: r + m
                ,_1: g + m
                ,_2: b + m};
      }();
   });
   var toRgb = function (color) {
      return function () {
         switch (color.ctor)
         {case "HSLA":
            return function () {
                 var $ = A3(hslToRgb,
                 color._0,
                 color._1,
                 color._2),
                 r = $._0,
                 g = $._1,
                 b = $._2;
                 return {_: {}
                        ,alpha: color._3
                        ,blue: $Basics.round(255 * b)
                        ,green: $Basics.round(255 * g)
                        ,red: $Basics.round(255 * r)};
              }();
            case "RGBA": return {_: {}
                                ,alpha: color._3
                                ,blue: color._2
                                ,green: color._1
                                ,red: color._0};}
         _U.badCase($moduleName,
         "between lines 115 and 123");
      }();
   };
   var toHsl = function (color) {
      return function () {
         switch (color.ctor)
         {case "HSLA": return {_: {}
                              ,alpha: color._3
                              ,hue: color._0
                              ,lightness: color._2
                              ,saturation: color._1};
            case "RGBA":
            return function () {
                 var $ = A3(rgbToHsl,
                 color._0,
                 color._1,
                 color._2),
                 h = $._0,
                 s = $._1,
                 l = $._2;
                 return {_: {}
                        ,alpha: color._3
                        ,hue: h
                        ,lightness: l
                        ,saturation: s};
              }();}
         _U.badCase($moduleName,
         "between lines 105 and 112");
      }();
   };
   var HSLA = F4(function (a,
   b,
   c,
   d) {
      return {ctor: "HSLA"
             ,_0: a
             ,_1: b
             ,_2: c
             ,_3: d};
   });
   var hsla = F4(function (hue,
   saturation,
   lightness,
   alpha) {
      return A4(HSLA,
      hue - $Basics.turns($Basics.toFloat($Basics.floor(hue / (2 * $Basics.pi)))),
      saturation,
      lightness,
      alpha);
   });
   var hsl = F3(function (hue,
   saturation,
   lightness) {
      return A4(hsla,
      hue,
      saturation,
      lightness,
      1);
   });
   var complement = function (color) {
      return function () {
         switch (color.ctor)
         {case "HSLA": return A4(hsla,
              color._0 + $Basics.degrees(180),
              color._1,
              color._2,
              color._3);
            case "RGBA":
            return function () {
                 var $ = A3(rgbToHsl,
                 color._0,
                 color._1,
                 color._2),
                 h = $._0,
                 s = $._1,
                 l = $._2;
                 return A4(hsla,
                 h + $Basics.degrees(180),
                 s,
                 l,
                 color._3);
              }();}
         _U.badCase($moduleName,
         "between lines 96 and 102");
      }();
   };
   var grayscale = function (p) {
      return A4(HSLA,0,0,1 - p,1);
   };
   var greyscale = function (p) {
      return A4(HSLA,0,0,1 - p,1);
   };
   var RGBA = F4(function (a,
   b,
   c,
   d) {
      return {ctor: "RGBA"
             ,_0: a
             ,_1: b
             ,_2: c
             ,_3: d};
   });
   var rgba = RGBA;
   var rgb = F3(function (r,g,b) {
      return A4(RGBA,r,g,b,1);
   });
   var lightRed = A4(RGBA,
   239,
   41,
   41,
   1);
   var red = A4(RGBA,204,0,0,1);
   var darkRed = A4(RGBA,
   164,
   0,
   0,
   1);
   var lightOrange = A4(RGBA,
   252,
   175,
   62,
   1);
   var orange = A4(RGBA,
   245,
   121,
   0,
   1);
   var darkOrange = A4(RGBA,
   206,
   92,
   0,
   1);
   var lightYellow = A4(RGBA,
   255,
   233,
   79,
   1);
   var yellow = A4(RGBA,
   237,
   212,
   0,
   1);
   var darkYellow = A4(RGBA,
   196,
   160,
   0,
   1);
   var lightGreen = A4(RGBA,
   138,
   226,
   52,
   1);
   var green = A4(RGBA,
   115,
   210,
   22,
   1);
   var darkGreen = A4(RGBA,
   78,
   154,
   6,
   1);
   var lightBlue = A4(RGBA,
   114,
   159,
   207,
   1);
   var blue = A4(RGBA,
   52,
   101,
   164,
   1);
   var darkBlue = A4(RGBA,
   32,
   74,
   135,
   1);
   var lightPurple = A4(RGBA,
   173,
   127,
   168,
   1);
   var purple = A4(RGBA,
   117,
   80,
   123,
   1);
   var darkPurple = A4(RGBA,
   92,
   53,
   102,
   1);
   var lightBrown = A4(RGBA,
   233,
   185,
   110,
   1);
   var brown = A4(RGBA,
   193,
   125,
   17,
   1);
   var darkBrown = A4(RGBA,
   143,
   89,
   2,
   1);
   var black = A4(RGBA,0,0,0,1);
   var white = A4(RGBA,
   255,
   255,
   255,
   1);
   var lightGrey = A4(RGBA,
   238,
   238,
   236,
   1);
   var grey = A4(RGBA,
   211,
   215,
   207,
   1);
   var darkGrey = A4(RGBA,
   186,
   189,
   182,
   1);
   var lightGray = A4(RGBA,
   238,
   238,
   236,
   1);
   var gray = A4(RGBA,
   211,
   215,
   207,
   1);
   var darkGray = A4(RGBA,
   186,
   189,
   182,
   1);
   var lightCharcoal = A4(RGBA,
   136,
   138,
   133,
   1);
   var charcoal = A4(RGBA,
   85,
   87,
   83,
   1);
   var darkCharcoal = A4(RGBA,
   46,
   52,
   54,
   1);
   _elm.Color.values = {_op: _op
                       ,RGBA: RGBA
                       ,HSLA: HSLA
                       ,rgba: rgba
                       ,rgb: rgb
                       ,hsla: hsla
                       ,hsl: hsl
                       ,grayscale: grayscale
                       ,greyscale: greyscale
                       ,complement: complement
                       ,toHsl: toHsl
                       ,toRgb: toRgb
                       ,fmod: fmod
                       ,rgbToHsl: rgbToHsl
                       ,hslToRgb: hslToRgb
                       ,Linear: Linear
                       ,Radial: Radial
                       ,linear: linear
                       ,radial: radial
                       ,lightRed: lightRed
                       ,red: red
                       ,darkRed: darkRed
                       ,lightOrange: lightOrange
                       ,orange: orange
                       ,darkOrange: darkOrange
                       ,lightYellow: lightYellow
                       ,yellow: yellow
                       ,darkYellow: darkYellow
                       ,lightGreen: lightGreen
                       ,green: green
                       ,darkGreen: darkGreen
                       ,lightBlue: lightBlue
                       ,blue: blue
                       ,darkBlue: darkBlue
                       ,lightPurple: lightPurple
                       ,purple: purple
                       ,darkPurple: darkPurple
                       ,lightBrown: lightBrown
                       ,brown: brown
                       ,darkBrown: darkBrown
                       ,black: black
                       ,white: white
                       ,lightGrey: lightGrey
                       ,grey: grey
                       ,darkGrey: darkGrey
                       ,lightGray: lightGray
                       ,gray: gray
                       ,darkGray: darkGray
                       ,lightCharcoal: lightCharcoal
                       ,charcoal: charcoal
                       ,darkCharcoal: darkCharcoal};
   return _elm.Color.values;
};
Elm.Debug = Elm.Debug || {};
Elm.Debug.make = function (_elm) {
   "use strict";
   _elm.Debug = _elm.Debug || {};
   if (_elm.Debug.values)
   return _elm.Debug.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _P = _N.Ports.make(_elm),
   $moduleName = "Debug",
   $Graphics$Collage = Elm.Graphics.Collage.make(_elm),
   $Native$Debug = Elm.Native.Debug.make(_elm);
   var trace = $Native$Debug.tracePath;
   var watchSummary = $Native$Debug.watchSummary;
   var watch = $Native$Debug.watch;
   var crash = $Native$Debug.crash;
   var log = $Native$Debug.log;
   _elm.Debug.values = {_op: _op
                       ,log: log
                       ,crash: crash
                       ,watch: watch
                       ,watchSummary: watchSummary
                       ,trace: trace};
   return _elm.Debug.values;
};
Elm.Deserialize = Elm.Deserialize || {};
Elm.Deserialize.make = function (_elm) {
   "use strict";
   _elm.Deserialize = _elm.Deserialize || {};
   if (_elm.Deserialize.values)
   return _elm.Deserialize.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _P = _N.Ports.make(_elm),
   $moduleName = "Deserialize",
   $Basics = Elm.Basics.make(_elm),
   $GameTypes = Elm.GameTypes.make(_elm),
   $Json$Decode = Elm.Json.Decode.make(_elm),
   $Player = Elm.Player.make(_elm);
   var windowDims = A3($Json$Decode.tuple2,
   F2(function (v0,v1) {
      return {ctor: "_Tuple2"
             ,_0: v0
             ,_1: v1};
   }),
   $Json$Decode.$int,
   $Json$Decode.$int);
   var mousePos = A3($Json$Decode.tuple2,
   F2(function (v0,v1) {
      return {ctor: "_Tuple2"
             ,_0: v0
             ,_1: v1};
   }),
   $Json$Decode.$int,
   $Json$Decode.$int);
   var player = A2($Json$Decode.map,
   $Player.fromString,
   $Json$Decode.string);
   var deck = $Json$Decode.list($Json$Decode.string);
   var actionInfo = function (actionType) {
      return function () {
         switch (actionType)
         {case "NoAction":
            return $Json$Decode.succeed($GameTypes.NoAction);
            case "OpponentDisconnected":
            return $Json$Decode.succeed($GameTypes.OpponentDisconnected);
            case "Pass":
            return $Json$Decode.succeed($GameTypes.Pass);
            case "PickUpPiece":
            return A3($Json$Decode.object2,
              $GameTypes.PickUpPiece,
              A2($Json$Decode._op[":="],
              "player",
              player),
              A2($Json$Decode._op[":="],
              "idx",
              $Json$Decode.$int));
            case "PlacePiece":
            return A3($Json$Decode.object2,
              $GameTypes.PlacePiece,
              A2($Json$Decode._op[":="],
              "mousePos",
              mousePos),
              A2($Json$Decode._op[":="],
              "dims",
              windowDims));
            case "StartGame":
            return A5($Json$Decode.object4,
              $GameTypes.GameStarted,
              A2($Json$Decode._op[":="],
              "deck",
              deck),
              A2($Json$Decode._op[":="],
              "player",
              player),
              A2($Json$Decode._op[":="],
              "color",
              player),
              A2($Json$Decode._op[":="],
              "opponentName",
              $Json$Decode.string));}
         return $Json$Decode.fail(A2($Basics._op["++"],
         actionType,
         " is not a recognized type of action"));
      }();
   };
   var action = A2($Json$Decode.andThen,
   A2($Json$Decode._op[":="],
   "action",
   $Json$Decode.string),
   actionInfo);
   _elm.Deserialize.values = {_op: _op
                             ,action: action
                             ,actionInfo: actionInfo
                             ,deck: deck
                             ,player: player
                             ,mousePos: mousePos
                             ,windowDims: windowDims};
   return _elm.Deserialize.values;
};
Elm.Dict = Elm.Dict || {};
Elm.Dict.make = function (_elm) {
   "use strict";
   _elm.Dict = _elm.Dict || {};
   if (_elm.Dict.values)
   return _elm.Dict.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _P = _N.Ports.make(_elm),
   $moduleName = "Dict",
   $Basics = Elm.Basics.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Debug = Elm.Native.Debug.make(_elm),
   $String = Elm.String.make(_elm);
   var foldr = F3(function (f,
   acc,
   t) {
      return function () {
         switch (t.ctor)
         {case "RBEmpty":
            switch (t._0.ctor)
              {case "LBlack": return acc;}
              break;
            case "RBNode": return A3(foldr,
              f,
              A3(f,
              t._1,
              t._2,
              A3(foldr,f,acc,t._4)),
              t._3);}
         _U.badCase($moduleName,
         "between lines 409 and 417");
      }();
   });
   var keys = function (dict) {
      return A3(foldr,
      F3(function (key,
      value,
      keyList) {
         return A2($List._op["::"],
         key,
         keyList);
      }),
      _L.fromArray([]),
      dict);
   };
   var values = function (dict) {
      return A3(foldr,
      F3(function (key,
      value,
      valueList) {
         return A2($List._op["::"],
         value,
         valueList);
      }),
      _L.fromArray([]),
      dict);
   };
   var toList = function (dict) {
      return A3(foldr,
      F3(function (key,value,list) {
         return A2($List._op["::"],
         {ctor: "_Tuple2"
         ,_0: key
         ,_1: value},
         list);
      }),
      _L.fromArray([]),
      dict);
   };
   var foldl = F3(function (f,
   acc,
   dict) {
      return function () {
         switch (dict.ctor)
         {case "RBEmpty":
            switch (dict._0.ctor)
              {case "LBlack": return acc;}
              break;
            case "RBNode": return A3(foldl,
              f,
              A3(f,
              dict._1,
              dict._2,
              A3(foldl,f,acc,dict._3)),
              dict._4);}
         _U.badCase($moduleName,
         "between lines 398 and 406");
      }();
   });
   var isBBlack = function (dict) {
      return function () {
         switch (dict.ctor)
         {case "RBEmpty":
            switch (dict._0.ctor)
              {case "LBBlack": return true;}
              break;
            case "RBNode":
            switch (dict._0.ctor)
              {case "BBlack": return true;}
              break;}
         return false;
      }();
   };
   var showFlag = function (f) {
      return function () {
         switch (f.ctor)
         {case "Insert": return "Insert";
            case "Remove": return "Remove";
            case "Same": return "Same";}
         _U.badCase($moduleName,
         "between lines 174 and 180");
      }();
   };
   var Same = {ctor: "Same"};
   var Remove = {ctor: "Remove"};
   var Insert = {ctor: "Insert"};
   var get = F2(function (targetKey,
   dict) {
      return function () {
         switch (dict.ctor)
         {case "RBEmpty":
            switch (dict._0.ctor)
              {case "LBlack":
                 return $Maybe.Nothing;}
              break;
            case "RBNode":
            return function () {
                 var _v29 = A2($Basics.compare,
                 targetKey,
                 dict._1);
                 switch (_v29.ctor)
                 {case "EQ":
                    return $Maybe.Just(dict._2);
                    case "GT": return A2(get,
                      targetKey,
                      dict._4);
                    case "LT": return A2(get,
                      targetKey,
                      dict._3);}
                 _U.badCase($moduleName,
                 "between lines 130 and 136");
              }();}
         _U.badCase($moduleName,
         "between lines 125 and 136");
      }();
   });
   var member = F2(function (key,
   dict) {
      return function () {
         var _v30 = A2(get,key,dict);
         switch (_v30.ctor)
         {case "Just": return true;
            case "Nothing": return false;}
         _U.badCase($moduleName,
         "between lines 139 and 141");
      }();
   });
   var max = function (dict) {
      return function () {
         switch (dict.ctor)
         {case "RBEmpty":
            return $Native$Debug.crash("(max Empty) is not defined");
            case "RBNode":
            switch (dict._4.ctor)
              {case "RBEmpty":
                 return {ctor: "_Tuple2"
                        ,_0: dict._1
                        ,_1: dict._2};}
              return max(dict._4);}
         _U.badCase($moduleName,
         "between lines 101 and 122");
      }();
   };
   var min = function (dict) {
      return function () {
         switch (dict.ctor)
         {case "RBEmpty":
            switch (dict._0.ctor)
              {case "LBlack":
                 return $Native$Debug.crash("(min Empty) is not defined");}
              break;
            case "RBNode":
            switch (dict._3.ctor)
              {case "RBEmpty":
                 switch (dict._3._0.ctor)
                   {case "LBlack":
                      return {ctor: "_Tuple2"
                             ,_0: dict._1
                             ,_1: dict._2};}
                   break;}
              return min(dict._3);}
         _U.badCase($moduleName,
         "between lines 88 and 96");
      }();
   };
   var RBEmpty = function (a) {
      return {ctor: "RBEmpty"
             ,_0: a};
   };
   var RBNode = F5(function (a,
   b,
   c,
   d,
   e) {
      return {ctor: "RBNode"
             ,_0: a
             ,_1: b
             ,_2: c
             ,_3: d
             ,_4: e};
   });
   var showLColor = function (color) {
      return function () {
         switch (color.ctor)
         {case "LBBlack":
            return "LBBlack";
            case "LBlack": return "LBlack";}
         _U.badCase($moduleName,
         "between lines 71 and 73");
      }();
   };
   var LBBlack = {ctor: "LBBlack"};
   var LBlack = {ctor: "LBlack"};
   var empty = RBEmpty(LBlack);
   var map = F2(function (f,dict) {
      return function () {
         switch (dict.ctor)
         {case "RBEmpty":
            switch (dict._0.ctor)
              {case "LBlack":
                 return RBEmpty(LBlack);}
              break;
            case "RBNode": return A5(RBNode,
              dict._0,
              dict._1,
              A2(f,dict._1,dict._2),
              A2(map,f,dict._3),
              A2(map,f,dict._4));}
         _U.badCase($moduleName,
         "between lines 386 and 395");
      }();
   });
   var showNColor = function (c) {
      return function () {
         switch (c.ctor)
         {case "BBlack": return "BBlack";
            case "Black": return "Black";
            case "NBlack": return "NBlack";
            case "Red": return "Red";}
         _U.badCase($moduleName,
         "between lines 57 and 61");
      }();
   };
   var reportRemBug = F4(function (msg,
   c,
   lgot,
   rgot) {
      return $Native$Debug.crash($String.concat(_L.fromArray(["Internal red-black tree invariant violated, expected "
                                                             ,msg
                                                             ," and got "
                                                             ,showNColor(c)
                                                             ,"/"
                                                             ,lgot
                                                             ,"/"
                                                             ,rgot
                                                             ,"\nPlease report this bug to <https://github.com/elm-lang/Elm/issues>"])));
   });
   var NBlack = {ctor: "NBlack"};
   var BBlack = {ctor: "BBlack"};
   var Black = {ctor: "Black"};
   var ensureBlackRoot = function (dict) {
      return function () {
         switch (dict.ctor)
         {case "RBEmpty":
            switch (dict._0.ctor)
              {case "LBlack": return dict;}
              break;
            case "RBNode":
            switch (dict._0.ctor)
              {case "Black": return dict;
                 case "Red": return A5(RBNode,
                   Black,
                   dict._1,
                   dict._2,
                   dict._3,
                   dict._4);}
              break;}
         _U.badCase($moduleName,
         "between lines 146 and 158");
      }();
   };
   var blackish = function (t) {
      return function () {
         switch (t.ctor)
         {case "RBEmpty": return true;
            case "RBNode":
            return _U.eq(t._0,
              Black) || _U.eq(t._0,BBlack);}
         _U.badCase($moduleName,
         "between lines 331 and 333");
      }();
   };
   var blacken = function (t) {
      return function () {
         switch (t.ctor)
         {case "RBEmpty":
            return RBEmpty(LBlack);
            case "RBNode": return A5(RBNode,
              Black,
              t._1,
              t._2,
              t._3,
              t._4);}
         _U.badCase($moduleName,
         "between lines 370 and 372");
      }();
   };
   var Red = {ctor: "Red"};
   var moreBlack = function (color) {
      return function () {
         switch (color.ctor)
         {case "BBlack":
            return $Native$Debug.crash("Can\'t make a double black node more black!");
            case "Black": return BBlack;
            case "NBlack": return Red;
            case "Red": return Black;}
         _U.badCase($moduleName,
         "between lines 236 and 240");
      }();
   };
   var lessBlack = function (color) {
      return function () {
         switch (color.ctor)
         {case "BBlack": return Black;
            case "Black": return Red;
            case "NBlack":
            return $Native$Debug.crash("Can\'t make a negative black node less black!");
            case "Red": return NBlack;}
         _U.badCase($moduleName,
         "between lines 245 and 249");
      }();
   };
   var lessBlackTree = function (dict) {
      return function () {
         switch (dict.ctor)
         {case "RBEmpty":
            switch (dict._0.ctor)
              {case "LBBlack":
                 return RBEmpty(LBlack);}
              break;
            case "RBNode": return A5(RBNode,
              lessBlack(dict._0),
              dict._1,
              dict._2,
              dict._3,
              dict._4);}
         _U.badCase($moduleName,
         "between lines 254 and 256");
      }();
   };
   var redden = function (t) {
      return function () {
         switch (t.ctor)
         {case "RBEmpty":
            return $Native$Debug.crash("can\'t make a Leaf red");
            case "RBNode": return A5(RBNode,
              Red,
              t._1,
              t._2,
              t._3,
              t._4);}
         _U.badCase($moduleName,
         "between lines 378 and 383");
      }();
   };
   var balance_node = function (t) {
      return function () {
         var assemble = function (col) {
            return function (xk) {
               return function (xv) {
                  return function (yk) {
                     return function (yv) {
                        return function (zk) {
                           return function (zv) {
                              return function (a) {
                                 return function (b) {
                                    return function (c) {
                                       return function (d) {
                                          return A5(RBNode,
                                          lessBlack(col),
                                          yk,
                                          yv,
                                          A5(RBNode,Black,xk,xv,a,b),
                                          A5(RBNode,Black,zk,zv,c,d));
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
         return blackish(t) ? function () {
            switch (t.ctor)
            {case "RBNode":
               switch (t._3.ctor)
                 {case "RBNode":
                    switch (t._3._0.ctor)
                      {case "Red":
                         switch (t._3._3.ctor)
                           {case "RBNode":
                              switch (t._3._3._0.ctor)
                                {case "Red":
                                   return assemble(t._0)(t._3._3._1)(t._3._3._2)(t._3._1)(t._3._2)(t._1)(t._2)(t._3._3._3)(t._3._3._4)(t._3._4)(t._4);}
                                break;}
                           switch (t._3._4.ctor)
                           {case "RBNode":
                              switch (t._3._4._0.ctor)
                                {case "Red":
                                   return assemble(t._0)(t._3._1)(t._3._2)(t._3._4._1)(t._3._4._2)(t._1)(t._2)(t._3._3)(t._3._4._3)(t._3._4._4)(t._4);}
                                break;}
                           break;}
                      break;}
                 switch (t._4.ctor)
                 {case "RBNode":
                    switch (t._4._0.ctor)
                      {case "Red":
                         switch (t._4._3.ctor)
                           {case "RBNode":
                              switch (t._4._3._0.ctor)
                                {case "Red":
                                   return assemble(t._0)(t._1)(t._2)(t._4._3._1)(t._4._3._2)(t._4._1)(t._4._2)(t._3)(t._4._3._3)(t._4._3._4)(t._4._4);}
                                break;}
                           switch (t._4._4.ctor)
                           {case "RBNode":
                              switch (t._4._4._0.ctor)
                                {case "Red":
                                   return assemble(t._0)(t._1)(t._2)(t._4._1)(t._4._2)(t._4._4._1)(t._4._4._2)(t._3)(t._4._3)(t._4._4._3)(t._4._4._4);}
                                break;}
                           break;}
                      break;}
                 switch (t._0.ctor)
                 {case "BBlack":
                    switch (t._4.ctor)
                      {case "RBNode":
                         switch (t._4._0.ctor)
                           {case "NBlack":
                              switch (t._4._3.ctor)
                                {case "RBNode":
                                   switch (t._4._3._0.ctor)
                                     {case "Black":
                                        return function () {
                                             switch (t._4._4.ctor)
                                             {case "RBNode":
                                                switch (t._4._4._0.ctor)
                                                  {case "Black":
                                                     return A5(RBNode,
                                                       Black,
                                                       t._4._3._1,
                                                       t._4._3._2,
                                                       A5(RBNode,
                                                       Black,
                                                       t._1,
                                                       t._2,
                                                       t._3,
                                                       t._4._3._3),
                                                       A5(balance,
                                                       Black,
                                                       t._4._1,
                                                       t._4._2,
                                                       t._4._3._4,
                                                       redden(t._4._4)));}
                                                  break;}
                                             return t;
                                          }();}
                                     break;}
                                break;}
                           break;}
                      switch (t._3.ctor)
                      {case "RBNode":
                         switch (t._3._0.ctor)
                           {case "NBlack":
                              switch (t._3._4.ctor)
                                {case "RBNode":
                                   switch (t._3._4._0.ctor)
                                     {case "Black":
                                        return function () {
                                             switch (t._3._3.ctor)
                                             {case "RBNode":
                                                switch (t._3._3._0.ctor)
                                                  {case "Black":
                                                     return A5(RBNode,
                                                       Black,
                                                       t._3._4._1,
                                                       t._3._4._2,
                                                       A5(balance,
                                                       Black,
                                                       t._3._1,
                                                       t._3._2,
                                                       redden(t._3._3),
                                                       t._3._4._3),
                                                       A5(RBNode,
                                                       Black,
                                                       t._1,
                                                       t._2,
                                                       t._3._4._4,
                                                       t._4));}
                                                  break;}
                                             return t;
                                          }();}
                                     break;}
                                break;}
                           break;}
                      break;}
                 break;}
            return t;
         }() : t;
      }();
   };
   var balance = F5(function (c,
   k,
   v,
   l,
   r) {
      return balance_node(A5(RBNode,
      c,
      k,
      v,
      l,
      r));
   });
   var bubble = F5(function (c,
   k,
   v,
   l,
   r) {
      return isBBlack(l) || isBBlack(r) ? A5(balance,
      moreBlack(c),
      k,
      v,
      lessBlackTree(l),
      lessBlackTree(r)) : A5(RBNode,
      c,
      k,
      v,
      l,
      r);
   });
   var remove_max = F5(function (c,
   k,
   v,
   l,
   r) {
      return function () {
         switch (r.ctor)
         {case "RBEmpty": return A3(rem,
              c,
              l,
              r);
            case "RBNode": return A5(bubble,
              c,
              k,
              v,
              l,
              A5(remove_max,
              r._0,
              r._1,
              r._2,
              r._3,
              r._4));}
         _U.badCase($moduleName,
         "between lines 315 and 320");
      }();
   });
   var rem = F3(function (c,l,r) {
      return function () {
         var _v169 = {ctor: "_Tuple2"
                     ,_0: l
                     ,_1: r};
         switch (_v169.ctor)
         {case "_Tuple2":
            switch (_v169._0.ctor)
              {case "RBEmpty":
                 switch (_v169._1.ctor)
                   {case "RBEmpty":
                      return function () {
                           switch (c.ctor)
                           {case "Black":
                              return RBEmpty(LBBlack);
                              case "Red":
                              return RBEmpty(LBlack);}
                           _U.badCase($moduleName,
                           "between lines 274 and 278");
                        }();
                      case "RBNode":
                      return function () {
                           var _v191 = {ctor: "_Tuple3"
                                       ,_0: c
                                       ,_1: _v169._0._0
                                       ,_2: _v169._1._0};
                           switch (_v191.ctor)
                           {case "_Tuple3":
                              switch (_v191._0.ctor)
                                {case "Black":
                                   switch (_v191._1.ctor)
                                     {case "LBlack":
                                        switch (_v191._2.ctor)
                                          {case "Red": return A5(RBNode,
                                               Black,
                                               _v169._1._1,
                                               _v169._1._2,
                                               _v169._1._3,
                                               _v169._1._4);}
                                          break;}
                                     break;}
                                break;}
                           return A4(reportRemBug,
                           "Black/LBlack/Red",
                           c,
                           showLColor(_v169._0._0),
                           showNColor(_v169._1._0));
                        }();}
                   break;
                 case "RBNode":
                 switch (_v169._1.ctor)
                   {case "RBEmpty":
                      return function () {
                           var _v195 = {ctor: "_Tuple3"
                                       ,_0: c
                                       ,_1: _v169._0._0
                                       ,_2: _v169._1._0};
                           switch (_v195.ctor)
                           {case "_Tuple3":
                              switch (_v195._0.ctor)
                                {case "Black":
                                   switch (_v195._1.ctor)
                                     {case "Red":
                                        switch (_v195._2.ctor)
                                          {case "LBlack":
                                             return A5(RBNode,
                                               Black,
                                               _v169._0._1,
                                               _v169._0._2,
                                               _v169._0._3,
                                               _v169._0._4);}
                                          break;}
                                     break;}
                                break;}
                           return A4(reportRemBug,
                           "Black/Red/LBlack",
                           c,
                           showNColor(_v169._0._0),
                           showLColor(_v169._1._0));
                        }();
                      case "RBNode":
                      return function () {
                           var l$ = A5(remove_max,
                           _v169._0._0,
                           _v169._0._1,
                           _v169._0._2,
                           _v169._0._3,
                           _v169._0._4);
                           var r = A5(RBNode,
                           _v169._1._0,
                           _v169._1._1,
                           _v169._1._2,
                           _v169._1._3,
                           _v169._1._4);
                           var l = A5(RBNode,
                           _v169._0._0,
                           _v169._0._1,
                           _v169._0._2,
                           _v169._0._3,
                           _v169._0._4);
                           var $ = max(l),
                           k = $._0,
                           v = $._1;
                           return A5(bubble,c,k,v,l$,r);
                        }();}
                   break;}
              break;}
         _U.badCase($moduleName,
         "between lines 272 and 301");
      }();
   });
   var update = F3(function (k,
   alter,
   dict) {
      return function () {
         var up = function (dict) {
            return function () {
               switch (dict.ctor)
               {case "RBEmpty":
                  switch (dict._0.ctor)
                    {case "LBlack":
                       return function () {
                            var _v206 = alter($Maybe.Nothing);
                            switch (_v206.ctor)
                            {case "Just":
                               return {ctor: "_Tuple2"
                                      ,_0: Insert
                                      ,_1: A5(RBNode,
                                      Red,
                                      k,
                                      _v206._0,
                                      empty,
                                      empty)};
                               case "Nothing":
                               return {ctor: "_Tuple2"
                                      ,_0: Same
                                      ,_1: empty};}
                            _U.badCase($moduleName,
                            "between lines 186 and 190");
                         }();}
                    break;
                  case "RBNode":
                  return function () {
                       var _v208 = A2($Basics.compare,
                       k,
                       dict._1);
                       switch (_v208.ctor)
                       {case "EQ": return function () {
                               var _v209 = alter($Maybe.Just(dict._2));
                               switch (_v209.ctor)
                               {case "Just":
                                  return {ctor: "_Tuple2"
                                         ,_0: Same
                                         ,_1: A5(RBNode,
                                         dict._0,
                                         dict._1,
                                         _v209._0,
                                         dict._3,
                                         dict._4)};
                                  case "Nothing":
                                  return {ctor: "_Tuple2"
                                         ,_0: Remove
                                         ,_1: A3(rem,
                                         dict._0,
                                         dict._3,
                                         dict._4)};}
                               _U.badCase($moduleName,
                               "between lines 193 and 198");
                            }();
                          case "GT": return function () {
                               var $ = up(dict._4),
                               flag = $._0,
                               newRight = $._1;
                               return function () {
                                  switch (flag.ctor)
                                  {case "Insert":
                                     return {ctor: "_Tuple2"
                                            ,_0: Insert
                                            ,_1: A5(balance,
                                            dict._0,
                                            dict._1,
                                            dict._2,
                                            dict._3,
                                            newRight)};
                                     case "Remove":
                                     return {ctor: "_Tuple2"
                                            ,_0: Remove
                                            ,_1: A5(bubble,
                                            dict._0,
                                            dict._1,
                                            dict._2,
                                            dict._3,
                                            newRight)};
                                     case "Same":
                                     return {ctor: "_Tuple2"
                                            ,_0: Same
                                            ,_1: A5(RBNode,
                                            dict._0,
                                            dict._1,
                                            dict._2,
                                            dict._3,
                                            newRight)};}
                                  _U.badCase($moduleName,
                                  "between lines 207 and 212");
                               }();
                            }();
                          case "LT": return function () {
                               var $ = up(dict._3),
                               flag = $._0,
                               newLeft = $._1;
                               return function () {
                                  switch (flag.ctor)
                                  {case "Insert":
                                     return {ctor: "_Tuple2"
                                            ,_0: Insert
                                            ,_1: A5(balance,
                                            dict._0,
                                            dict._1,
                                            dict._2,
                                            newLeft,
                                            dict._4)};
                                     case "Remove":
                                     return {ctor: "_Tuple2"
                                            ,_0: Remove
                                            ,_1: A5(bubble,
                                            dict._0,
                                            dict._1,
                                            dict._2,
                                            newLeft,
                                            dict._4)};
                                     case "Same":
                                     return {ctor: "_Tuple2"
                                            ,_0: Same
                                            ,_1: A5(RBNode,
                                            dict._0,
                                            dict._1,
                                            dict._2,
                                            newLeft,
                                            dict._4)};}
                                  _U.badCase($moduleName,
                                  "between lines 200 and 205");
                               }();
                            }();}
                       _U.badCase($moduleName,
                       "between lines 191 and 212");
                    }();}
               _U.badCase($moduleName,
               "between lines 184 and 212");
            }();
         };
         var $ = up(dict),
         flag = $._0,
         updatedDict = $._1;
         return function () {
            switch (flag.ctor)
            {case "Insert":
               return ensureBlackRoot(updatedDict);
               case "Remove":
               return blacken(updatedDict);
               case "Same":
               return updatedDict;}
            _U.badCase($moduleName,
            "between lines 214 and 220");
         }();
      }();
   });
   var insert = F3(function (key,
   value,
   dict) {
      return A3(update,
      key,
      $Basics.always($Maybe.Just(value)),
      dict);
   });
   var singleton = F2(function (key,
   value) {
      return A3(insert,
      key,
      value,
      RBEmpty(LBlack));
   });
   var union = F2(function (t1,
   t2) {
      return A3(foldl,
      insert,
      t2,
      t1);
   });
   var fromList = function (assocs) {
      return A3($List.foldl,
      F2(function (_v214,dict) {
         return function () {
            switch (_v214.ctor)
            {case "_Tuple2":
               return A3(insert,
                 _v214._0,
                 _v214._1,
                 dict);}
            _U.badCase($moduleName,
            "on line 458, column 38 to 59");
         }();
      }),
      empty,
      assocs);
   };
   var filter = F2(function (predicate,
   dictionary) {
      return function () {
         var add = F3(function (key,
         value,
         dict) {
            return A2(predicate,
            key,
            value) ? A3(insert,
            key,
            value,
            dict) : dict;
         });
         return A3(foldl,
         add,
         empty,
         dictionary);
      }();
   });
   var intersect = F2(function (t1,
   t2) {
      return A2(filter,
      F2(function (k,_v218) {
         return function () {
            return A2(member,k,t2);
         }();
      }),
      t1);
   });
   var partition = F2(function (predicate,
   dict) {
      return function () {
         var add = F3(function (key,
         value,
         _v220) {
            return function () {
               switch (_v220.ctor)
               {case "_Tuple2":
                  return A2(predicate,
                    key,
                    value) ? {ctor: "_Tuple2"
                             ,_0: A3(insert,
                             key,
                             value,
                             _v220._0)
                             ,_1: _v220._1} : {ctor: "_Tuple2"
                                              ,_0: _v220._0
                                              ,_1: A3(insert,
                                              key,
                                              value,
                                              _v220._1)};}
               _U.badCase($moduleName,
               "between lines 479 and 481");
            }();
         });
         return A3(foldl,
         add,
         {ctor: "_Tuple2"
         ,_0: empty
         ,_1: empty},
         dict);
      }();
   });
   var remove = F2(function (key,
   dict) {
      return A3(update,
      key,
      $Basics.always($Maybe.Nothing),
      dict);
   });
   var diff = F2(function (t1,t2) {
      return A3(foldl,
      F3(function (k,v,t) {
         return A2(remove,k,t);
      }),
      t1,
      t2);
   });
   _elm.Dict.values = {_op: _op
                      ,empty: empty
                      ,singleton: singleton
                      ,insert: insert
                      ,update: update
                      ,get: get
                      ,remove: remove
                      ,member: member
                      ,filter: filter
                      ,partition: partition
                      ,foldl: foldl
                      ,foldr: foldr
                      ,map: map
                      ,union: union
                      ,intersect: intersect
                      ,diff: diff
                      ,keys: keys
                      ,values: values
                      ,toList: toList
                      ,fromList: fromList};
   return _elm.Dict.values;
};
Elm.Display = Elm.Display || {};
Elm.Display.make = function (_elm) {
   "use strict";
   _elm.Display = _elm.Display || {};
   if (_elm.Display.values)
   return _elm.Display.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _P = _N.Ports.make(_elm),
   $moduleName = "Display",
   $Basics = Elm.Basics.make(_elm),
   $Board = Elm.Board.make(_elm),
   $Color = Elm.Color.make(_elm),
   $Dict = Elm.Dict.make(_elm),
   $GameTypes = Elm.GameTypes.make(_elm),
   $Graphics$Collage = Elm.Graphics.Collage.make(_elm),
   $Graphics$Element = Elm.Graphics.Element.make(_elm),
   $Graphics$Input = Elm.Graphics.Input.make(_elm),
   $Graphics$Input$Field = Elm.Graphics.Input.Field.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Piece = Elm.Piece.make(_elm),
   $Player = Elm.Player.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $String = Elm.String.make(_elm),
   $Text = Elm.Text.make(_elm);
   var drawLastPlacedOutline = F2(function (state,
   tileSize) {
      return function () {
         var _v0 = state.lastPlaced;
         switch (_v0.ctor)
         {case "Just":
            switch (_v0._0.ctor)
              {case "_Tuple2":
                 return function () {
                      var lastPlacedColor = $Player.toColor($Player.next(state.turn));
                      var thick = function (c) {
                         return _U.replace([["color"
                                            ,c]
                                           ,["width",4]],
                         $Graphics$Collage.defaultLine);
                      };
                      var lastPlacedOutline = A2($Graphics$Collage.move,
                      {ctor: "_Tuple2"
                      ,_0: tileSize * $Basics.toFloat(_v0._0._0)
                      ,_1: tileSize * $Basics.toFloat(_v0._0._1)},
                      A2($Graphics$Collage.outlined,
                      thick(lastPlacedColor),
                      $Graphics$Collage.square(tileSize + 4)));
                      return _L.fromArray([lastPlacedOutline]);
                   }();}
              break;
            case "Nothing":
            return _L.fromArray([]);}
         _U.badCase($moduleName,
         "between lines 120 and 126");
      }();
   });
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
            _U.badCase($moduleName,
            "between lines 70 and 79");
         }();
         return A3($Graphics$Element.image,
         $Basics.round(tileSize),
         $Basics.round(tileSize),
         imgPath);
      }();
   });
   var drawPiece = F2(function (_v5,
   tileSize) {
      return function () {
         switch (_v5.ctor)
         {case "_Tuple2":
            switch (_v5._0.ctor)
              {case "_Tuple2":
                 return function () {
                      var y = $Basics.toFloat(_v5._0._1) * tileSize;
                      var x = $Basics.toFloat(_v5._0._0) * tileSize;
                      return A2($Graphics$Collage.move,
                      {ctor: "_Tuple2",_0: x,_1: y},
                      $Graphics$Collage.toForm(A2(pieceToImage,
                      _v5._1,
                      tileSize)));
                   }();}
              break;}
         _U.badCase($moduleName,
         "between lines 113 and 116");
      }();
   });
   var rulesRow = F2(function (piece,
   description) {
      return function () {
         var value = $Piece.baseValue(piece);
         var nameAndValue = $Text.concat(A2($List.map,
         $Text.fromString,
         _L.fromArray([$Piece.toString(piece)
                      ," ("
                      ,$Basics.toString(value)
                      ,"): "])));
         var text = A2($Graphics$Element.flow,
         $Graphics$Element.right,
         _L.fromArray([$Text.leftAligned($Text.bold(nameAndValue))
                      ,$Text.plainText(description)]));
         var height = 30;
         var image = A2($Graphics$Element.beside,
         A2(pieceToImage,piece,height),
         A2($Graphics$Element.spacer,
         10,
         10));
         return A2($Graphics$Element.beside,
         image,
         A4($Graphics$Element.container,
         600,
         height,
         $Graphics$Element.midLeft,
         text));
      }();
   });
   var playerNameChannel = $Signal.channel($Graphics$Input$Field.noContent);
   var gameTypeChannel = $Signal.channel($GameTypes.HumanVsCpu);
   var clickChannel = $Signal.channel($GameTypes.None);
   var handTileSize = 100;
   var handPadding = 10;
   var renderHand = F2(function (player,
   state) {
      return function () {
         var hiddenPiece = A3($Graphics$Element.image,
         $Basics.round(handTileSize),
         $Basics.round(handTileSize),
         "images/tile_back.jpg");
         var pieceSize = $Basics.round(handTileSize) + handPadding;
         var pieceImage = function (pieceStr) {
            return pieceToImage($Piece.fromString(pieceStr));
         };
         var isPieceHeld = function (idx) {
            return _U.eq(state.turn,
            player) && _U.eq(state.heldPiece,
            $Maybe.Just(idx));
         };
         var makePiece = F2(function (idx,
         pieceStr) {
            return $Graphics$Input.clickable(A2($Signal.send,
            clickChannel,
            A2($GameTypes.PieceInHand,
            player,
            idx)))($Graphics$Element.color(isPieceHeld(idx) ? $Player.toColor(state.turn) : $Color.white)(A3($Graphics$Element.container,
            pieceSize,
            pieceSize,
            $Graphics$Element.middle)(A2(pieceImage,
            pieceStr,
            handTileSize))));
         });
         var hand = A2($Player.getHand,
         player,
         state);
         var playerHand = $List.isEmpty(hand) && _U.eq(state.gameState,
         $GameTypes.Ongoing) ? _L.fromArray([A3($Graphics$Element.container,
         100,
         100,
         $Graphics$Element.middle)(A2($Graphics$Input.button,
         A2($Signal.send,
         clickChannel,
         $GameTypes.PassButton),
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
         var p = $Player.color(player);
         var playerType = A2($Maybe.withDefault,
         $GameTypes.Human,
         A2($Dict.get,p,state.players));
         var handContents = _U.eq(playerType,
         $GameTypes.Human) ? playerHand : cpuHand;
         var handText = A3($Graphics$Element.container,
         80,
         pieceSize,
         $Graphics$Element.middle)($Text.leftAligned($Text.color($Player.toColor(player))((_U.eq(state.turn,
         player) && $GameTypes.isOngoing(state) ? $Text.bold : $Basics.identity)($Text.fromString($String.toUpper(function (t) {
            return _U.eq(t,
            "Human") ? "Player" : t;
         }($Basics.toString(playerType))))))));
         var score = A3($Graphics$Element.container,
         25,
         pieceSize,
         $Graphics$Element.midLeft)($Text.asText($Maybe.withDefault(0)(A2($Dict.get,
         p,
         state.score))));
         var delta = A3($Graphics$Element.container,
         20,
         pieceSize,
         $Graphics$Element.midLeft)($Text.leftAligned($Text.height(9)($Text.fromString($Maybe.withDefault("")(A2($Dict.get,
         p,
         state.delta))))));
         return A2($Graphics$Element.flow,
         $Graphics$Element.right,
         A2($Basics._op["++"],
         _L.fromArray([handText]),
         A2($Basics._op["++"],
         _L.fromArray([score]),
         A2($Basics._op["++"],
         _L.fromArray([delta]),
         handContents))));
      }();
   });
   var gameHeaderSize = 100;
   var getTotalBoardSize = function (_v11) {
      return function () {
         switch (_v11.ctor)
         {case "_Tuple2":
            return _v11._1 - gameHeaderSize;}
         _U.badCase($moduleName,
         "on line 51, column 37 to 60");
      }();
   };
   var getTileSizeFromBoardSize = F2(function (boardSize,
   dims) {
      return $Basics.toFloat(getTotalBoardSize(dims) / boardSize | 0);
   });
   var drawGrid = F3(function (state,
   boardSize,
   dims) {
      return function () {
         var transpGreen = A4($Color.rgba,
         0,
         255,
         0,
         0.5);
         var transparent = A4($Color.rgba,
         0,
         0,
         0,
         0.0);
         var tileSize = A2(getTileSizeFromBoardSize,
         boardSize,
         dims);
         var num = $Basics.toFloat(boardSize);
         var size = num * tileSize;
         var offset = tileSize / 2 - size / 2;
         var shape = F3(function (x,
         y,
         color) {
            return function () {
               var pos = {ctor: "_Tuple2"
                         ,_0: tileSize * x + offset
                         ,_1: tileSize * y + offset};
               return _L.fromArray([A2($Graphics$Collage.move,
                                   pos,
                                   A2($Graphics$Collage.outlined,
                                   $Graphics$Collage.solid($Color.black),
                                   $Graphics$Collage.square(tileSize)))
                                   ,A2($Graphics$Collage.move,
                                   pos,
                                   A2($Graphics$Collage.filled,
                                   color,
                                   $Graphics$Collage.square(tileSize)))]);
            }();
         });
         var isValidSquare = F2(function (x,
         y) {
            return $Player.isPlayerTurn(state) ? function () {
               var _v15 = state.heldPiece;
               switch (_v15.ctor)
               {case "Just":
                  return function () {
                       var location = {ctor: "_Tuple2"
                                      ,_0: $Basics.round(x) - $Basics.floor(num / 2)
                                      ,_1: $Basics.round(y) - $Basics.floor(num / 2)};
                       var hand = A2($Player.getHand,
                       state.turn,
                       state);
                       var piece = $Piece.fromString($List.head(A2($List.drop,
                       _v15._0,
                       hand)));
                       return A2($Board.isValidMove,
                       {_: {}
                       ,idx: _v15._0
                       ,location: location
                       ,piece: piece},
                       state.board);
                    }();
                  case "Nothing": return false;}
               _U.badCase($moduleName,
               "between lines 97 and 105");
            }() : false;
         });
         return A2($List.concatMap,
         function (x) {
            return A2($List.concatMap,
            function (y) {
               return A3(shape,
               x,
               y,
               A2(isValidSquare,
               x,
               y) ? transpGreen : transparent);
            },
            _L.range(0,num - 1));
         },
         _L.range(0,num - 1));
      }();
   });
   var renderBoard = F3(function (state,
   boardSize,
   dims) {
      return function () {
         var grid = A3(drawGrid,
         state,
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
         var board = A3($Graphics$Collage.collage,
         size,
         size,
         A2($Basics._op["++"],
         pieces,
         A2($Basics._op["++"],
         grid,
         outline)));
         return A2($Graphics$Input.clickable,
         A2($Signal.send,
         clickChannel,
         $GameTypes.BoardClick),
         board);
      }();
   });
   var mouseToBoardPosition = F3(function (_v17,
   state,
   dims) {
      return function () {
         switch (_v17.ctor)
         {case "_Tuple2":
            return function () {
                 var boardSize = $Board.getBoardSize(state.board);
                 var tileSize = $Basics.round(A2(getTileSizeFromBoardSize,
                 boardSize,
                 dims));
                 var offset = boardSize / 2 | 0;
                 var y = _v17._1 - gameHeaderSize;
                 var boardY = 0 - ((y / tileSize | 0) - offset);
                 var x = _v17._0;
                 var boardX = (x / tileSize | 0) - offset;
                 return {ctor: "_Tuple2"
                        ,_0: boardX
                        ,_1: boardY};
              }();}
         _U.badCase($moduleName,
         "between lines 58 and 65");
      }();
   });
   var render = F4(function (state,
   dims,
   gameType,
   playerName) {
      return function () {
         var logArea = $Graphics$Element.flow($Graphics$Element.down)($List.map(function (_v21) {
            return function () {
               switch (_v21.ctor)
               {case "_Tuple2":
                  return $Text.leftAligned($Text.color(_v21._0)($Text.fromString(_v21._1)));}
               _U.badCase($moduleName,
               "on line 221, column 49 to 99");
            }();
         })($List.take(5)(state.log)));
         var deckSizeArea = $GameTypes.isOngoing(state) ? A3($Graphics$Element.container,
         70,
         40,
         $Graphics$Element.middle)($Text.centered($Text.height(11)($Text.fromString(A2($Basics._op["++"],
         "Deck: ",
         $Basics.toString($List.length(state.deck))))))) : $Graphics$Element.empty;
         var remoteGameStatusText = function () {
            var _v25 = state.gameState;
            switch (_v25.ctor)
            {case "Connected":
               return A2($Basics._op["++"],
                 "Connected to ",
                 A2($Basics._op["++"],
                 _v25._0,
                 " "));
               case "Disconnected":
               return "Opponent disconnected ";
               case "WaitingForPlayers":
               return "Waiting for opponent ... ";}
            return "";
         }();
         var remoteGameStatusArea = _U.eq(state.gameState,
         $GameTypes.NotStarted) ? A4($Graphics$Input$Field.field,
         $Graphics$Input$Field.defaultStyle,
         $Signal.send(playerNameChannel),
         "Your name",
         playerName) : A3($Graphics$Element.container,
         150,
         40,
         $Graphics$Element.middle)($Text.centered($Text.height(11)($Text.fromString(remoteGameStatusText))));
         var statusArea = _U.eq(gameType,
         $GameTypes.HumanVsHumanRemote) ? remoteGameStatusArea : $Graphics$Element.empty;
         var gameTypeDropDown = A2($Graphics$Element.size,
         180,
         40)(A2($Graphics$Input.dropDown,
         $Signal.send(gameTypeChannel),
         _L.fromArray([{ctor: "_Tuple2"
                       ,_0: "Player vs AI"
                       ,_1: $GameTypes.HumanVsCpu}
                      ,{ctor: "_Tuple2"
                       ,_0: "Player vs Player (hotseat)"
                       ,_1: $GameTypes.HumanVsHumanLocal}
                      ,{ctor: "_Tuple2"
                       ,_0: "Player vs Player (online)"
                       ,_1: $GameTypes.HumanVsHumanRemote}])));
         var startButton = A2($Graphics$Input.button,
         A2($Signal.send,
         clickChannel,
         $GameTypes.Start),
         "New game");
         var rulesAreaWidth = 650;
         var controls = A3($Graphics$Element.container,
         rulesAreaWidth,
         40,
         $Graphics$Element.middle)(A2($Graphics$Element.flow,
         $Graphics$Element.right,
         _L.fromArray([statusArea
                      ,gameTypeDropDown
                      ,startButton
                      ,deckSizeArea])));
         var rightArea = A2($Graphics$Element.flow,
         $Graphics$Element.down,
         _L.fromArray([logArea
                      ,controls]));
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
         var boardSize = $Board.getBoardSize(state.board);
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
                      $Text.fromString("V&ouml;lusp&aacute;")))))
                      ,A2($Graphics$Element.flow,
                      $Graphics$Element.right,
                      _L.fromArray([A3(renderBoard,
                                   state,
                                   boardSize,
                                   dims)
                                   ,A2($Graphics$Element.flow,
                                   $Graphics$Element.down,
                                   _L.fromArray([A2(renderHand,
                                                $GameTypes.Red,
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
                                                ,A2(renderHand,
                                                $GameTypes.Blue,
                                                state)]))]))]));
      }();
   });
   _elm.Display.values = {_op: _op
                         ,gameHeaderSize: gameHeaderSize
                         ,handPadding: handPadding
                         ,handTileSize: handTileSize
                         ,clickChannel: clickChannel
                         ,gameTypeChannel: gameTypeChannel
                         ,playerNameChannel: playerNameChannel
                         ,getTotalBoardSize: getTotalBoardSize
                         ,getTileSizeFromBoardSize: getTileSizeFromBoardSize
                         ,mouseToBoardPosition: mouseToBoardPosition
                         ,pieceToImage: pieceToImage
                         ,drawGrid: drawGrid
                         ,drawPiece: drawPiece
                         ,drawLastPlacedOutline: drawLastPlacedOutline
                         ,renderBoard: renderBoard
                         ,renderHand: renderHand
                         ,rulesRow: rulesRow
                         ,render: render};
   return _elm.Display.values;
};
Elm.GameTypes = Elm.GameTypes || {};
Elm.GameTypes.make = function (_elm) {
   "use strict";
   _elm.GameTypes = _elm.GameTypes || {};
   if (_elm.GameTypes.values)
   return _elm.GameTypes.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _P = _N.Ports.make(_elm),
   $moduleName = "GameTypes",
   $Color = Elm.Color.make(_elm),
   $Dict = Elm.Dict.make(_elm),
   $Maybe = Elm.Maybe.make(_elm);
   var isOngoing = function (state) {
      return function () {
         var _v0 = state.gameState;
         switch (_v0.ctor)
         {case "Connected": return true;
            case "Ongoing": return true;}
         return false;
      }();
   };
   var None = {ctor: "None"};
   var PassButton = {ctor: "PassButton"};
   var PieceInHand = F2(function (a,
   b) {
      return {ctor: "PieceInHand"
             ,_0: a
             ,_1: b};
   });
   var BoardClick = {ctor: "BoardClick"};
   var Start = {ctor: "Start"};
   var ParseError = function (a) {
      return {ctor: "ParseError"
             ,_0: a};
   };
   var NoAction = {ctor: "NoAction"};
   var OpponentDisconnected = {ctor: "OpponentDisconnected"};
   var Pass = {ctor: "Pass"};
   var GameStarted = F4(function (a,
   b,
   c,
   d) {
      return {ctor: "GameStarted"
             ,_0: a
             ,_1: b
             ,_2: c
             ,_3: d};
   });
   var StartGame = F4(function (a,
   b,
   c,
   d) {
      return {ctor: "StartGame"
             ,_0: a
             ,_1: b
             ,_2: c
             ,_3: d};
   });
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
   var NoPiece = {ctor: "NoPiece"};
   var Loki = {ctor: "Loki"};
   var Valkyrie = {ctor: "Valkyrie"};
   var Skadi = {ctor: "Skadi"};
   var Fenrir = {ctor: "Fenrir"};
   var Dragon = {ctor: "Dragon"};
   var Troll = {ctor: "Troll"};
   var Thor = {ctor: "Thor"};
   var Odin = {ctor: "Odin"};
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
                                    return function (l) {
                                       return function (m) {
                                          return {_: {}
                                                 ,board: f
                                                 ,deck: h
                                                 ,delta: l
                                                 ,gameState: b
                                                 ,gameType: a
                                                 ,hands: i
                                                 ,heldPiece: j
                                                 ,lastPlaced: k
                                                 ,log: m
                                                 ,playerNames: d
                                                 ,players: c
                                                 ,score: g
                                                 ,turn: e};
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
      };
   };
   var Move = F3(function (a,b,c) {
      return {_: {}
             ,idx: b
             ,location: c
             ,piece: a};
   });
   var Blue = {ctor: "Blue"};
   var Red = {ctor: "Red"};
   var Remote = {ctor: "Remote"};
   var Cpu = {ctor: "Cpu"};
   var Human = {ctor: "Human"};
   var Disconnected = {ctor: "Disconnected"};
   var GameOver = {ctor: "GameOver"};
   var Connected = function (a) {
      return {ctor: "Connected"
             ,_0: a};
   };
   var Ongoing = {ctor: "Ongoing"};
   var WaitingForPlayers = {ctor: "WaitingForPlayers"};
   var NotStarted = {ctor: "NotStarted"};
   var HumanVsHumanRemote = {ctor: "HumanVsHumanRemote"};
   var HumanVsHumanLocal = {ctor: "HumanVsHumanLocal"};
   var HumanVsCpu = {ctor: "HumanVsCpu"};
   _elm.GameTypes.values = {_op: _op
                           ,HumanVsCpu: HumanVsCpu
                           ,HumanVsHumanLocal: HumanVsHumanLocal
                           ,HumanVsHumanRemote: HumanVsHumanRemote
                           ,NotStarted: NotStarted
                           ,WaitingForPlayers: WaitingForPlayers
                           ,Ongoing: Ongoing
                           ,Connected: Connected
                           ,GameOver: GameOver
                           ,Disconnected: Disconnected
                           ,Human: Human
                           ,Cpu: Cpu
                           ,Remote: Remote
                           ,Red: Red
                           ,Blue: Blue
                           ,Move: Move
                           ,State: State
                           ,Odin: Odin
                           ,Thor: Thor
                           ,Troll: Troll
                           ,Dragon: Dragon
                           ,Fenrir: Fenrir
                           ,Skadi: Skadi
                           ,Valkyrie: Valkyrie
                           ,Loki: Loki
                           ,NoPiece: NoPiece
                           ,PickUpPiece: PickUpPiece
                           ,PlacePiece: PlacePiece
                           ,StartGame: StartGame
                           ,GameStarted: GameStarted
                           ,Pass: Pass
                           ,OpponentDisconnected: OpponentDisconnected
                           ,NoAction: NoAction
                           ,ParseError: ParseError
                           ,Start: Start
                           ,BoardClick: BoardClick
                           ,PieceInHand: PieceInHand
                           ,PassButton: PassButton
                           ,None: None
                           ,isOngoing: isOngoing};
   return _elm.GameTypes.values;
};
Elm.Graphics = Elm.Graphics || {};
Elm.Graphics.Collage = Elm.Graphics.Collage || {};
Elm.Graphics.Collage.make = function (_elm) {
   "use strict";
   _elm.Graphics = _elm.Graphics || {};
   _elm.Graphics.Collage = _elm.Graphics.Collage || {};
   if (_elm.Graphics.Collage.values)
   return _elm.Graphics.Collage.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _P = _N.Ports.make(_elm),
   $moduleName = "Graphics.Collage",
   $Basics = Elm.Basics.make(_elm),
   $Color = Elm.Color.make(_elm),
   $Graphics$Element = Elm.Graphics.Element.make(_elm),
   $List = Elm.List.make(_elm),
   $Native$Graphics$Collage = Elm.Native.Graphics.Collage.make(_elm),
   $Transform2D = Elm.Transform2D.make(_elm);
   var ngon = F2(function (n,r) {
      return function () {
         var m = $Basics.toFloat(n);
         var t = 2 * $Basics.pi / m;
         var f = function (i) {
            return {ctor: "_Tuple2"
                   ,_0: r * $Basics.cos(t * i)
                   ,_1: r * $Basics.sin(t * i)};
         };
         return A2($List.map,
         f,
         _L.range(0,m - 1));
      }();
   });
   var oval = F2(function (w,h) {
      return function () {
         var hh = h / 2;
         var hw = w / 2;
         var n = 50;
         var t = 2 * $Basics.pi / n;
         var f = function (i) {
            return {ctor: "_Tuple2"
                   ,_0: hw * $Basics.cos(t * i)
                   ,_1: hh * $Basics.sin(t * i)};
         };
         return A2($List.map,
         f,
         _L.range(0,n - 1));
      }();
   });
   var circle = function (r) {
      return A2(oval,2 * r,2 * r);
   };
   var rect = F2(function (w,h) {
      return function () {
         var hh = h / 2;
         var hw = w / 2;
         return _L.fromArray([{ctor: "_Tuple2"
                              ,_0: 0 - hw
                              ,_1: 0 - hh}
                             ,{ctor: "_Tuple2"
                              ,_0: 0 - hw
                              ,_1: hh}
                             ,{ctor: "_Tuple2",_0: hw,_1: hh}
                             ,{ctor: "_Tuple2"
                              ,_0: hw
                              ,_1: 0 - hh}]);
      }();
   });
   var square = function (n) {
      return A2(rect,n,n);
   };
   var polygon = function (points) {
      return points;
   };
   var segment = F2(function (p1,
   p2) {
      return _L.fromArray([p1,p2]);
   });
   var path = function (ps) {
      return ps;
   };
   var collage = $Native$Graphics$Collage.collage;
   var alpha = F2(function (a,f) {
      return _U.replace([["alpha"
                         ,a]],
      f);
   });
   var rotate = F2(function (t,f) {
      return _U.replace([["theta"
                         ,f.theta + t]],
      f);
   });
   var scale = F2(function (s,f) {
      return _U.replace([["scale"
                         ,f.scale * s]],
      f);
   });
   var moveY = F2(function (y,f) {
      return _U.replace([["y"
                         ,f.y + y]],
      f);
   });
   var moveX = F2(function (x,f) {
      return _U.replace([["x"
                         ,f.x + x]],
      f);
   });
   var move = F2(function (_v0,f) {
      return function () {
         switch (_v0.ctor)
         {case "_Tuple2":
            return _U.replace([["x"
                               ,f.x + _v0._0]
                              ,["y",f.y + _v0._1]],
              f);}
         _U.badCase($moduleName,
         "on line 179, column 20 to 48");
      }();
   });
   var form = function (f) {
      return {_: {}
             ,alpha: 1
             ,form: f
             ,scale: 1
             ,theta: 0
             ,x: 0
             ,y: 0};
   };
   var Fill = function (a) {
      return {ctor: "Fill",_0: a};
   };
   var Line = function (a) {
      return {ctor: "Line",_0: a};
   };
   var FGroup = F2(function (a,b) {
      return {ctor: "FGroup"
             ,_0: a
             ,_1: b};
   });
   var group = function (fs) {
      return form(A2(FGroup,
      $Transform2D.identity,
      fs));
   };
   var groupTransform = F2(function (matrix,
   fs) {
      return form(A2(FGroup,
      matrix,
      fs));
   });
   var FElement = function (a) {
      return {ctor: "FElement"
             ,_0: a};
   };
   var toForm = function (e) {
      return form(FElement(e));
   };
   var FImage = F4(function (a,
   b,
   c,
   d) {
      return {ctor: "FImage"
             ,_0: a
             ,_1: b
             ,_2: c
             ,_3: d};
   });
   var sprite = F4(function (w,
   h,
   pos,
   src) {
      return form(A4(FImage,
      w,
      h,
      pos,
      src));
   });
   var FShape = F2(function (a,b) {
      return {ctor: "FShape"
             ,_0: a
             ,_1: b};
   });
   var fill = F2(function (style,
   shape) {
      return form(A2(FShape,
      Fill(style),
      shape));
   });
   var outlined = F2(function (style,
   shape) {
      return form(A2(FShape,
      Line(style),
      shape));
   });
   var FPath = F2(function (a,b) {
      return {ctor: "FPath"
             ,_0: a
             ,_1: b};
   });
   var traced = F2(function (style,
   path) {
      return form(A2(FPath,
      style,
      path));
   });
   var LineStyle = F6(function (a,
   b,
   c,
   d,
   e,
   f) {
      return {_: {}
             ,cap: c
             ,color: a
             ,dashOffset: f
             ,dashing: e
             ,join: d
             ,width: b};
   });
   var Clipped = {ctor: "Clipped"};
   var Sharp = function (a) {
      return {ctor: "Sharp",_0: a};
   };
   var Smooth = {ctor: "Smooth"};
   var Padded = {ctor: "Padded"};
   var Round = {ctor: "Round"};
   var Flat = {ctor: "Flat"};
   var defaultLine = {_: {}
                     ,cap: Flat
                     ,color: $Color.black
                     ,dashOffset: 0
                     ,dashing: _L.fromArray([])
                     ,join: Sharp(10)
                     ,width: 1};
   var solid = function (clr) {
      return _U.replace([["color"
                         ,clr]],
      defaultLine);
   };
   var dashed = function (clr) {
      return _U.replace([["color"
                         ,clr]
                        ,["dashing"
                         ,_L.fromArray([8,4])]],
      defaultLine);
   };
   var dotted = function (clr) {
      return _U.replace([["color"
                         ,clr]
                        ,["dashing"
                         ,_L.fromArray([3,3])]],
      defaultLine);
   };
   var Grad = function (a) {
      return {ctor: "Grad",_0: a};
   };
   var gradient = F2(function (grad,
   shape) {
      return A2(fill,
      Grad(grad),
      shape);
   });
   var Texture = function (a) {
      return {ctor: "Texture"
             ,_0: a};
   };
   var textured = F2(function (src,
   shape) {
      return A2(fill,
      Texture(src),
      shape);
   });
   var Solid = function (a) {
      return {ctor: "Solid",_0: a};
   };
   var filled = F2(function (color,
   shape) {
      return A2(fill,
      Solid(color),
      shape);
   });
   var Form = F6(function (a,
   b,
   c,
   d,
   e,
   f) {
      return {_: {}
             ,alpha: e
             ,form: f
             ,scale: b
             ,theta: a
             ,x: c
             ,y: d};
   });
   _elm.Graphics.Collage.values = {_op: _op
                                  ,Form: Form
                                  ,Solid: Solid
                                  ,Texture: Texture
                                  ,Grad: Grad
                                  ,Flat: Flat
                                  ,Round: Round
                                  ,Padded: Padded
                                  ,Smooth: Smooth
                                  ,Sharp: Sharp
                                  ,Clipped: Clipped
                                  ,LineStyle: LineStyle
                                  ,defaultLine: defaultLine
                                  ,solid: solid
                                  ,dashed: dashed
                                  ,dotted: dotted
                                  ,FPath: FPath
                                  ,FShape: FShape
                                  ,FImage: FImage
                                  ,FElement: FElement
                                  ,FGroup: FGroup
                                  ,Line: Line
                                  ,Fill: Fill
                                  ,form: form
                                  ,fill: fill
                                  ,filled: filled
                                  ,textured: textured
                                  ,gradient: gradient
                                  ,outlined: outlined
                                  ,traced: traced
                                  ,sprite: sprite
                                  ,toForm: toForm
                                  ,group: group
                                  ,groupTransform: groupTransform
                                  ,move: move
                                  ,moveX: moveX
                                  ,moveY: moveY
                                  ,scale: scale
                                  ,rotate: rotate
                                  ,alpha: alpha
                                  ,collage: collage
                                  ,path: path
                                  ,segment: segment
                                  ,polygon: polygon
                                  ,rect: rect
                                  ,square: square
                                  ,oval: oval
                                  ,circle: circle
                                  ,ngon: ngon};
   return _elm.Graphics.Collage.values;
};
Elm.Graphics = Elm.Graphics || {};
Elm.Graphics.Element = Elm.Graphics.Element || {};
Elm.Graphics.Element.make = function (_elm) {
   "use strict";
   _elm.Graphics = _elm.Graphics || {};
   _elm.Graphics.Element = _elm.Graphics.Element || {};
   if (_elm.Graphics.Element.values)
   return _elm.Graphics.Element.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _P = _N.Ports.make(_elm),
   $moduleName = "Graphics.Element",
   $Basics = Elm.Basics.make(_elm),
   $Color = Elm.Color.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Graphics$Element = Elm.Native.Graphics.Element.make(_elm);
   var DOut = {ctor: "DOut"};
   var outward = DOut;
   var DIn = {ctor: "DIn"};
   var inward = DIn;
   var DRight = {ctor: "DRight"};
   var right = DRight;
   var DLeft = {ctor: "DLeft"};
   var left = DLeft;
   var DDown = {ctor: "DDown"};
   var down = DDown;
   var DUp = {ctor: "DUp"};
   var up = DUp;
   var Position = F4(function (a,
   b,
   c,
   d) {
      return {_: {}
             ,horizontal: a
             ,vertical: b
             ,x: c
             ,y: d};
   });
   var Relative = function (a) {
      return {ctor: "Relative"
             ,_0: a};
   };
   var relative = Relative;
   var Absolute = function (a) {
      return {ctor: "Absolute"
             ,_0: a};
   };
   var absolute = Absolute;
   var N = {ctor: "N"};
   var bottomLeftAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: N
             ,vertical: N
             ,x: x
             ,y: y};
   });
   var Z = {ctor: "Z"};
   var middle = {_: {}
                ,horizontal: Z
                ,vertical: Z
                ,x: Relative(0.5)
                ,y: Relative(0.5)};
   var midLeft = _U.replace([["horizontal"
                             ,N]
                            ,["x",Absolute(0)]],
   middle);
   var middleAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: Z
             ,vertical: Z
             ,x: x
             ,y: y};
   });
   var midLeftAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: N
             ,vertical: Z
             ,x: x
             ,y: y};
   });
   var midBottomAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: Z
             ,vertical: N
             ,x: x
             ,y: y};
   });
   var P = {ctor: "P"};
   var topLeft = {_: {}
                 ,horizontal: N
                 ,vertical: P
                 ,x: Absolute(0)
                 ,y: Absolute(0)};
   var bottomLeft = _U.replace([["vertical"
                                ,N]],
   topLeft);
   var topRight = _U.replace([["horizontal"
                              ,P]],
   topLeft);
   var bottomRight = _U.replace([["horizontal"
                                 ,P]],
   bottomLeft);
   var midRight = _U.replace([["horizontal"
                              ,P]],
   midLeft);
   var midTop = _U.replace([["vertical"
                            ,P]
                           ,["y",Absolute(0)]],
   middle);
   var midBottom = _U.replace([["vertical"
                               ,N]],
   midTop);
   var topLeftAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: N
             ,vertical: P
             ,x: x
             ,y: y};
   });
   var topRightAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: P
             ,vertical: P
             ,x: x
             ,y: y};
   });
   var bottomRightAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: P
             ,vertical: N
             ,x: x
             ,y: y};
   });
   var midRightAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: P
             ,vertical: Z
             ,x: x
             ,y: y};
   });
   var midTopAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: Z
             ,vertical: P
             ,x: x
             ,y: y};
   });
   var Tiled = {ctor: "Tiled"};
   var Cropped = function (a) {
      return {ctor: "Cropped"
             ,_0: a};
   };
   var Fitted = {ctor: "Fitted"};
   var Plain = {ctor: "Plain"};
   var Custom = {ctor: "Custom"};
   var RawHtml = {ctor: "RawHtml"};
   var Spacer = {ctor: "Spacer"};
   var Flow = F2(function (a,b) {
      return {ctor: "Flow"
             ,_0: a
             ,_1: b};
   });
   var Container = F2(function (a,
   b) {
      return {ctor: "Container"
             ,_0: a
             ,_1: b};
   });
   var Image = F4(function (a,
   b,
   c,
   d) {
      return {ctor: "Image"
             ,_0: a
             ,_1: b
             ,_2: c
             ,_3: d};
   });
   var link = F2(function (href,
   e) {
      return function () {
         var p = e.props;
         return {_: {}
                ,element: e.element
                ,props: _U.replace([["href"
                                    ,href]],
                p)};
      }();
   });
   var tag = F2(function (name,e) {
      return function () {
         var p = e.props;
         return {_: {}
                ,element: e.element
                ,props: _U.replace([["tag"
                                    ,name]],
                p)};
      }();
   });
   var color = F2(function (c,e) {
      return function () {
         var p = e.props;
         return {_: {}
                ,element: e.element
                ,props: _U.replace([["color"
                                    ,$Maybe.Just(c)]],
                p)};
      }();
   });
   var opacity = F2(function (o,
   e) {
      return function () {
         var p = e.props;
         return {_: {}
                ,element: e.element
                ,props: _U.replace([["opacity"
                                    ,o]],
                p)};
      }();
   });
   var height = F2(function (nh,
   e) {
      return function () {
         var p = e.props;
         var props = function () {
            var _v0 = e.element;
            switch (_v0.ctor)
            {case "Image":
               return _U.replace([["width"
                                  ,$Basics.round($Basics.toFloat(_v0._1) / $Basics.toFloat(_v0._2) * $Basics.toFloat(nh))]],
                 p);}
            return p;
         }();
         return {_: {}
                ,element: e.element
                ,props: _U.replace([["height"
                                    ,nh]],
                p)};
      }();
   });
   var width = F2(function (nw,e) {
      return function () {
         var p = e.props;
         var props = function () {
            var _v5 = e.element;
            switch (_v5.ctor)
            {case "Image":
               return _U.replace([["height"
                                  ,$Basics.round($Basics.toFloat(_v5._2) / $Basics.toFloat(_v5._1) * $Basics.toFloat(nw))]],
                 p);
               case "RawHtml":
               return _U.replace([["height"
                                  ,$Basics.snd(A2($Native$Graphics$Element.htmlHeight,
                                  nw,
                                  e.element))]],
                 p);}
            return p;
         }();
         return {_: {}
                ,element: e.element
                ,props: _U.replace([["width"
                                    ,nw]],
                props)};
      }();
   });
   var size = F3(function (w,h,e) {
      return A2(height,
      h,
      A2(width,w,e));
   });
   var sizeOf = function (e) {
      return {ctor: "_Tuple2"
             ,_0: e.props.width
             ,_1: e.props.height};
   };
   var heightOf = function (e) {
      return e.props.height;
   };
   var widthOf = function (e) {
      return e.props.width;
   };
   var Element = F2(function (a,
   b) {
      return {_: {}
             ,element: b
             ,props: a};
   });
   var Properties = F9(function (a,
   b,
   c,
   d,
   e,
   f,
   g,
   h,
   i) {
      return {_: {}
             ,click: i
             ,color: e
             ,height: c
             ,hover: h
             ,href: f
             ,id: a
             ,opacity: d
             ,tag: g
             ,width: b};
   });
   var newElement = F3(function (w,
   h,
   e) {
      return {_: {}
             ,element: e
             ,props: A9(Properties,
             $Native$Graphics$Element.guid({ctor: "_Tuple0"}),
             w,
             h,
             1,
             $Maybe.Nothing,
             "",
             "",
             {ctor: "_Tuple0"},
             {ctor: "_Tuple0"})};
   });
   var image = F3(function (w,
   h,
   src) {
      return A3(newElement,
      w,
      h,
      A4(Image,Plain,w,h,src));
   });
   var fittedImage = F3(function (w,
   h,
   src) {
      return A3(newElement,
      w,
      h,
      A4(Image,Fitted,w,h,src));
   });
   var croppedImage = F4(function (pos,
   w,
   h,
   src) {
      return A3(newElement,
      w,
      h,
      A4(Image,Cropped(pos),w,h,src));
   });
   var tiledImage = F3(function (w,
   h,
   src) {
      return A3(newElement,
      w,
      h,
      A4(Image,Tiled,w,h,src));
   });
   var container = F4(function (w,
   h,
   pos,
   e) {
      return A3(newElement,
      w,
      h,
      A2(Container,pos,e));
   });
   var spacer = F2(function (w,h) {
      return A3(newElement,
      w,
      h,
      Spacer);
   });
   var empty = A2(spacer,0,0);
   var flow = F2(function (dir,
   es) {
      return function () {
         var newFlow = F2(function (w,
         h) {
            return A3(newElement,
            w,
            h,
            A2(Flow,dir,es));
         });
         var hs = A2($List.map,
         heightOf,
         es);
         var ws = A2($List.map,
         widthOf,
         es);
         return _U.eq(es,
         _L.fromArray([])) ? empty : function () {
            switch (dir.ctor)
            {case "DDown":
               return A2(newFlow,
                 $List.maximum(ws),
                 $List.sum(hs));
               case "DIn": return A2(newFlow,
                 $List.maximum(ws),
                 $List.maximum(hs));
               case "DLeft": return A2(newFlow,
                 $List.sum(ws),
                 $List.maximum(hs));
               case "DOut": return A2(newFlow,
                 $List.maximum(ws),
                 $List.maximum(hs));
               case "DRight":
               return A2(newFlow,
                 $List.sum(ws),
                 $List.maximum(hs));
               case "DUp": return A2(newFlow,
                 $List.maximum(ws),
                 $List.sum(hs));}
            _U.badCase($moduleName,
            "between lines 280 and 291");
         }();
      }();
   });
   var above = F2(function (hi,
   lo) {
      return A3(newElement,
      A2($Basics.max,
      widthOf(hi),
      widthOf(lo)),
      heightOf(hi) + heightOf(lo),
      A2(Flow,
      DDown,
      _L.fromArray([hi,lo])));
   });
   var below = F2(function (lo,
   hi) {
      return A3(newElement,
      A2($Basics.max,
      widthOf(hi),
      widthOf(lo)),
      heightOf(hi) + heightOf(lo),
      A2(Flow,
      DDown,
      _L.fromArray([hi,lo])));
   });
   var beside = F2(function (lft,
   rht) {
      return A3(newElement,
      widthOf(lft) + widthOf(rht),
      A2($Basics.max,
      heightOf(lft),
      heightOf(rht)),
      A2(Flow,
      right,
      _L.fromArray([lft,rht])));
   });
   var layers = function (es) {
      return function () {
         var hs = A2($List.map,
         heightOf,
         es);
         var ws = A2($List.map,
         widthOf,
         es);
         return A3(newElement,
         $List.maximum(ws),
         $List.maximum(hs),
         A2(Flow,DOut,es));
      }();
   };
   _elm.Graphics.Element.values = {_op: _op
                                  ,Properties: Properties
                                  ,Element: Element
                                  ,empty: empty
                                  ,widthOf: widthOf
                                  ,heightOf: heightOf
                                  ,sizeOf: sizeOf
                                  ,width: width
                                  ,height: height
                                  ,size: size
                                  ,opacity: opacity
                                  ,color: color
                                  ,tag: tag
                                  ,link: link
                                  ,newElement: newElement
                                  ,Image: Image
                                  ,Container: Container
                                  ,Flow: Flow
                                  ,Spacer: Spacer
                                  ,RawHtml: RawHtml
                                  ,Custom: Custom
                                  ,Plain: Plain
                                  ,Fitted: Fitted
                                  ,Cropped: Cropped
                                  ,Tiled: Tiled
                                  ,image: image
                                  ,fittedImage: fittedImage
                                  ,croppedImage: croppedImage
                                  ,tiledImage: tiledImage
                                  ,P: P
                                  ,Z: Z
                                  ,N: N
                                  ,Absolute: Absolute
                                  ,Relative: Relative
                                  ,Position: Position
                                  ,container: container
                                  ,spacer: spacer
                                  ,DUp: DUp
                                  ,DDown: DDown
                                  ,DLeft: DLeft
                                  ,DRight: DRight
                                  ,DIn: DIn
                                  ,DOut: DOut
                                  ,flow: flow
                                  ,above: above
                                  ,below: below
                                  ,beside: beside
                                  ,layers: layers
                                  ,absolute: absolute
                                  ,relative: relative
                                  ,middle: middle
                                  ,topLeft: topLeft
                                  ,topRight: topRight
                                  ,bottomLeft: bottomLeft
                                  ,bottomRight: bottomRight
                                  ,midLeft: midLeft
                                  ,midRight: midRight
                                  ,midTop: midTop
                                  ,midBottom: midBottom
                                  ,middleAt: middleAt
                                  ,topLeftAt: topLeftAt
                                  ,topRightAt: topRightAt
                                  ,bottomLeftAt: bottomLeftAt
                                  ,bottomRightAt: bottomRightAt
                                  ,midLeftAt: midLeftAt
                                  ,midRightAt: midRightAt
                                  ,midTopAt: midTopAt
                                  ,midBottomAt: midBottomAt
                                  ,up: up
                                  ,down: down
                                  ,left: left
                                  ,right: right
                                  ,inward: inward
                                  ,outward: outward};
   return _elm.Graphics.Element.values;
};
Elm.Graphics = Elm.Graphics || {};
Elm.Graphics.Input = Elm.Graphics.Input || {};
Elm.Graphics.Input.make = function (_elm) {
   "use strict";
   _elm.Graphics = _elm.Graphics || {};
   _elm.Graphics.Input = _elm.Graphics.Input || {};
   if (_elm.Graphics.Input.values)
   return _elm.Graphics.Input.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _P = _N.Ports.make(_elm),
   $moduleName = "Graphics.Input",
   $Graphics$Element = Elm.Graphics.Element.make(_elm),
   $Native$Graphics$Input = Elm.Native.Graphics.Input.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var clickable = $Native$Graphics$Input.clickable;
   var hoverable = $Native$Graphics$Input.hoverable;
   var dropDown = $Native$Graphics$Input.dropDown;
   var checkbox = $Native$Graphics$Input.checkbox;
   var customButton = $Native$Graphics$Input.customButton;
   var button = $Native$Graphics$Input.button;
   _elm.Graphics.Input.values = {_op: _op
                                ,button: button
                                ,customButton: customButton
                                ,checkbox: checkbox
                                ,dropDown: dropDown
                                ,hoverable: hoverable
                                ,clickable: clickable};
   return _elm.Graphics.Input.values;
};
Elm.Graphics = Elm.Graphics || {};
Elm.Graphics.Input = Elm.Graphics.Input || {};
Elm.Graphics.Input.Field = Elm.Graphics.Input.Field || {};
Elm.Graphics.Input.Field.make = function (_elm) {
   "use strict";
   _elm.Graphics = _elm.Graphics || {};
   _elm.Graphics.Input = _elm.Graphics.Input || {};
   _elm.Graphics.Input.Field = _elm.Graphics.Input.Field || {};
   if (_elm.Graphics.Input.Field.values)
   return _elm.Graphics.Input.Field.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _P = _N.Ports.make(_elm),
   $moduleName = "Graphics.Input.Field",
   $Color = Elm.Color.make(_elm),
   $Graphics$Element = Elm.Graphics.Element.make(_elm),
   $Native$Graphics$Input = Elm.Native.Graphics.Input.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $Text = Elm.Text.make(_elm);
   var email = $Native$Graphics$Input.email;
   var password = $Native$Graphics$Input.password;
   var field = $Native$Graphics$Input.field;
   var Backward = {ctor: "Backward"};
   var Forward = {ctor: "Forward"};
   var Selection = F3(function (a,
   b,
   c) {
      return {_: {}
             ,direction: c
             ,end: b
             ,start: a};
   });
   var Content = F2(function (a,
   b) {
      return {_: {}
             ,selection: b
             ,string: a};
   });
   var noContent = A2(Content,
   "",
   A3(Selection,0,0,Forward));
   var Style = F4(function (a,
   b,
   c,
   d) {
      return {_: {}
             ,highlight: c
             ,outline: b
             ,padding: a
             ,style: d};
   });
   var Highlight = F2(function (a,
   b) {
      return {_: {}
             ,color: a
             ,width: b};
   });
   var noHighlight = A2(Highlight,
   $Color.blue,
   0);
   var Outline = F3(function (a,
   b,
   c) {
      return {_: {}
             ,color: a
             ,radius: c
             ,width: b};
   });
   var Dimensions = F4(function (a,
   b,
   c,
   d) {
      return {_: {}
             ,bottom: d
             ,left: a
             ,right: b
             ,top: c};
   });
   var uniformly = function (n) {
      return A4(Dimensions,
      n,
      n,
      n,
      n);
   };
   var noOutline = A3(Outline,
   $Color.grey,
   uniformly(0),
   0);
   var defaultStyle = {_: {}
                      ,highlight: A2(Highlight,
                      $Color.blue,
                      1)
                      ,outline: A3(Outline,
                      $Color.grey,
                      uniformly(1),
                      2)
                      ,padding: uniformly(4)
                      ,style: $Text.defaultStyle};
   _elm.Graphics.Input.Field.values = {_op: _op
                                      ,uniformly: uniformly
                                      ,Dimensions: Dimensions
                                      ,Outline: Outline
                                      ,noOutline: noOutline
                                      ,Highlight: Highlight
                                      ,noHighlight: noHighlight
                                      ,Style: Style
                                      ,defaultStyle: defaultStyle
                                      ,Content: Content
                                      ,Selection: Selection
                                      ,Forward: Forward
                                      ,Backward: Backward
                                      ,noContent: noContent
                                      ,field: field
                                      ,password: password
                                      ,email: email};
   return _elm.Graphics.Input.Field.values;
};
Elm.Helpers = Elm.Helpers || {};
Elm.Helpers.make = function (_elm) {
   "use strict";
   _elm.Helpers = _elm.Helpers || {};
   if (_elm.Helpers.values)
   return _elm.Helpers.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _P = _N.Ports.make(_elm),
   $moduleName = "Helpers",
   $Basics = Elm.Basics.make(_elm),
   $List = Elm.List.make(_elm),
   $Random = Elm.Random.make(_elm);
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
         return A2($Basics._op["++"],
         before,
         A2($Basics._op["++"],
         _L.fromArray([elt]),
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
         return A2($Basics._op["++"],
         before,
         after);
      }();
   });
   _op["!!"] = F2(function (list,
   idx) {
      return $List.head(A2($List.drop,
      idx,
      list));
   });
   var shuffle = F2(function (list,
   seed) {
      return $List.isEmpty(list) ? _L.fromArray([]) : function () {
         var generator = A2($Random.$int,
         0,
         $List.length(list) - 1);
         var $ = A2($Random.generate,
         generator,
         seed),
         i = $._0,
         newSeed = $._1;
         return A2($Basics._op["++"],
         _L.fromArray([A2(_op["!!"],
         list,
         i)]),
         A2(shuffle,
         A2(without,i,list),
         newSeed));
      }();
   });
   var sample = F2(function (list,
   seed) {
      return $List.head(A2(shuffle,
      list,
      seed));
   });
   _elm.Helpers.values = {_op: _op
                         ,without: without
                         ,replaceAtIndex: replaceAtIndex
                         ,shuffle: shuffle
                         ,sample: sample};
   return _elm.Helpers.values;
};
Elm.Json = Elm.Json || {};
Elm.Json.Decode = Elm.Json.Decode || {};
Elm.Json.Decode.make = function (_elm) {
   "use strict";
   _elm.Json = _elm.Json || {};
   _elm.Json.Decode = _elm.Json.Decode || {};
   if (_elm.Json.Decode.values)
   return _elm.Json.Decode.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _P = _N.Ports.make(_elm),
   $moduleName = "Json.Decode",
   $Array = Elm.Array.make(_elm),
   $Dict = Elm.Dict.make(_elm),
   $Json$Encode = Elm.Json.Encode.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Json = Elm.Native.Json.make(_elm),
   $Result = Elm.Result.make(_elm);
   var tuple8 = $Native$Json.decodeTuple8;
   var tuple7 = $Native$Json.decodeTuple7;
   var tuple6 = $Native$Json.decodeTuple6;
   var tuple5 = $Native$Json.decodeTuple5;
   var tuple4 = $Native$Json.decodeTuple4;
   var tuple3 = $Native$Json.decodeTuple3;
   var tuple2 = $Native$Json.decodeTuple2;
   var tuple1 = $Native$Json.decodeTuple1;
   var succeed = $Native$Json.succeed;
   var fail = $Native$Json.fail;
   var andThen = $Native$Json.andThen;
   var customDecoder = $Native$Json.customDecoder;
   var decodeValue = $Native$Json.runDecoderValue;
   var value = $Native$Json.decodeValue;
   var maybe = $Native$Json.decodeMaybe;
   var $null = $Native$Json.decodeNull;
   var array = $Native$Json.decodeArray;
   var list = $Native$Json.decodeList;
   var bool = $Native$Json.decodeBool;
   var $int = $Native$Json.decodeInt;
   var $float = $Native$Json.decodeFloat;
   var string = $Native$Json.decodeString;
   var oneOf = $Native$Json.oneOf;
   var keyValuePairs = $Native$Json.decodeKeyValuePairs;
   var object8 = $Native$Json.decodeObject8;
   var object7 = $Native$Json.decodeObject7;
   var object6 = $Native$Json.decodeObject6;
   var object5 = $Native$Json.decodeObject5;
   var object4 = $Native$Json.decodeObject4;
   var object3 = $Native$Json.decodeObject3;
   var object2 = $Native$Json.decodeObject2;
   var object1 = $Native$Json.decodeObject1;
   _op[":="] = $Native$Json.decodeField;
   var at = F2(function (fields,
   decoder) {
      return A3($List.foldr,
      F2(function (x,y) {
         return A2(_op[":="],x,y);
      }),
      decoder,
      fields);
   });
   var decodeString = $Native$Json.runDecoderString;
   var map = $Native$Json.decodeObject1;
   var dict = function (decoder) {
      return A2(map,
      $Dict.fromList,
      keyValuePairs(decoder));
   };
   var Decoder = {ctor: "Decoder"};
   _elm.Json.Decode.values = {_op: _op
                             ,Decoder: Decoder
                             ,map: map
                             ,decodeString: decodeString
                             ,at: at
                             ,object1: object1
                             ,object2: object2
                             ,object3: object3
                             ,object4: object4
                             ,object5: object5
                             ,object6: object6
                             ,object7: object7
                             ,object8: object8
                             ,keyValuePairs: keyValuePairs
                             ,dict: dict
                             ,oneOf: oneOf
                             ,string: string
                             ,$float: $float
                             ,$int: $int
                             ,bool: bool
                             ,list: list
                             ,array: array
                             ,$null: $null
                             ,maybe: maybe
                             ,value: value
                             ,decodeValue: decodeValue
                             ,customDecoder: customDecoder
                             ,andThen: andThen
                             ,fail: fail
                             ,succeed: succeed
                             ,tuple1: tuple1
                             ,tuple2: tuple2
                             ,tuple3: tuple3
                             ,tuple4: tuple4
                             ,tuple5: tuple5
                             ,tuple6: tuple6
                             ,tuple7: tuple7
                             ,tuple8: tuple8};
   return _elm.Json.Decode.values;
};
Elm.Json = Elm.Json || {};
Elm.Json.Encode = Elm.Json.Encode || {};
Elm.Json.Encode.make = function (_elm) {
   "use strict";
   _elm.Json = _elm.Json || {};
   _elm.Json.Encode = _elm.Json.Encode || {};
   if (_elm.Json.Encode.values)
   return _elm.Json.Encode.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _P = _N.Ports.make(_elm),
   $moduleName = "Json.Encode",
   $Array = Elm.Array.make(_elm),
   $Native$Json = Elm.Native.Json.make(_elm);
   var list = $Native$Json.encodeList;
   var array = $Native$Json.encodeArray;
   var object = $Native$Json.encodeObject;
   var $null = $Native$Json.encodeNull;
   var bool = $Native$Json.identity;
   var $float = $Native$Json.identity;
   var $int = $Native$Json.identity;
   var string = $Native$Json.identity;
   var encode = $Native$Json.encode;
   var Value = {ctor: "Value"};
   _elm.Json.Encode.values = {_op: _op
                             ,Value: Value
                             ,encode: encode
                             ,string: string
                             ,$int: $int
                             ,$float: $float
                             ,bool: bool
                             ,$null: $null
                             ,object: object
                             ,array: array
                             ,list: list};
   return _elm.Json.Encode.values;
};
Elm.List = Elm.List || {};
Elm.List.make = function (_elm) {
   "use strict";
   _elm.List = _elm.List || {};
   if (_elm.List.values)
   return _elm.List.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _P = _N.Ports.make(_elm),
   $moduleName = "List",
   $Basics = Elm.Basics.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$List = Elm.Native.List.make(_elm);
   var sortWith = $Native$List.sortWith;
   var sortBy = $Native$List.sortBy;
   var sort = $Native$List.sort;
   var repeat = $Native$List.repeat;
   var drop = $Native$List.drop;
   var take = $Native$List.take;
   var map5 = $Native$List.map5;
   var map4 = $Native$List.map4;
   var map3 = $Native$List.map3;
   var map2 = $Native$List.map2;
   var append = $Native$List.append;
   var any = $Native$List.any;
   var all = $Native$List.all;
   var length = $Native$List.length;
   var filter = $Native$List.filter;
   var scanl1 = $Native$List.scanl1;
   var scanl = $Native$List.scanl;
   var foldr1 = $Native$List.foldr1;
   var foldl1 = $Native$List.foldl1;
   var maximum = foldl1($Basics.max);
   var minimum = foldl1($Basics.min);
   var foldr = $Native$List.foldr;
   var concat = function (lists) {
      return A3(foldr,
      append,
      _L.fromArray([]),
      lists);
   };
   var foldl = $Native$List.foldl;
   var sum = function (numbers) {
      return A3(foldl,
      F2(function (x,y) {
         return x + y;
      }),
      0,
      numbers);
   };
   var product = function (numbers) {
      return A3(foldl,
      F2(function (x,y) {
         return x * y;
      }),
      1,
      numbers);
   };
   var indexedMap = F2(function (f,
   xs) {
      return A3(map2,
      f,
      _L.range(0,length(xs) - 1),
      xs);
   });
   var map = $Native$List.map;
   var concatMap = F2(function (f,
   list) {
      return concat(A2(map,
      f,
      list));
   });
   var member = $Native$List.member;
   var isEmpty = function (xs) {
      return function () {
         switch (xs.ctor)
         {case "[]": return true;}
         return false;
      }();
   };
   var tail = $Native$List.tail;
   var head = $Native$List.head;
   _op["::"] = $Native$List.cons;
   var maybeCons = F3(function (f,
   mx,
   xs) {
      return function () {
         var _v1 = f(mx);
         switch (_v1.ctor)
         {case "Just":
            return A2(_op["::"],_v1._0,xs);
            case "Nothing": return xs;}
         _U.badCase($moduleName,
         "between lines 162 and 169");
      }();
   });
   var filterMap = F2(function (f,
   xs) {
      return A3(foldr,
      maybeCons(f),
      _L.fromArray([]),
      xs);
   });
   var reverse = A2(foldl,
   F2(function (x,y) {
      return A2(_op["::"],x,y);
   }),
   _L.fromArray([]));
   var partition = F2(function (pred,
   list) {
      return function () {
         var step = F2(function (x,
         _v3) {
            return function () {
               switch (_v3.ctor)
               {case "_Tuple2":
                  return pred(x) ? {ctor: "_Tuple2"
                                   ,_0: A2(_op["::"],x,_v3._0)
                                   ,_1: _v3._1} : {ctor: "_Tuple2"
                                                  ,_0: _v3._0
                                                  ,_1: A2(_op["::"],x,_v3._1)};}
               _U.badCase($moduleName,
               "between lines 271 and 273");
            }();
         });
         return A3(foldr,
         step,
         {ctor: "_Tuple2"
         ,_0: _L.fromArray([])
         ,_1: _L.fromArray([])},
         list);
      }();
   });
   var unzip = function (pairs) {
      return function () {
         var step = F2(function (_v7,
         _v8) {
            return function () {
               switch (_v8.ctor)
               {case "_Tuple2":
                  return function () {
                       switch (_v7.ctor)
                       {case "_Tuple2":
                          return {ctor: "_Tuple2"
                                 ,_0: A2(_op["::"],_v7._0,_v8._0)
                                 ,_1: A2(_op["::"],
                                 _v7._1,
                                 _v8._1)};}
                       _U.badCase($moduleName,
                       "on line 309, column 12 to 28");
                    }();}
               _U.badCase($moduleName,
               "on line 309, column 12 to 28");
            }();
         });
         return A3(foldr,
         step,
         {ctor: "_Tuple2"
         ,_0: _L.fromArray([])
         ,_1: _L.fromArray([])},
         pairs);
      }();
   };
   var intersperse = F2(function (sep,
   xs) {
      return function () {
         switch (xs.ctor)
         {case "::": return function () {
                 var step = F2(function (x,
                 rest) {
                    return A2(_op["::"],
                    sep,
                    A2(_op["::"],x,rest));
                 });
                 var spersed = A3(foldr,
                 step,
                 _L.fromArray([]),
                 xs._1);
                 return A2(_op["::"],
                 xs._0,
                 spersed);
              }();
            case "[]":
            return _L.fromArray([]);}
         _U.badCase($moduleName,
         "between lines 320 and 331");
      }();
   });
   _elm.List.values = {_op: _op
                      ,head: head
                      ,tail: tail
                      ,isEmpty: isEmpty
                      ,member: member
                      ,map: map
                      ,indexedMap: indexedMap
                      ,foldl: foldl
                      ,foldr: foldr
                      ,foldl1: foldl1
                      ,foldr1: foldr1
                      ,scanl: scanl
                      ,scanl1: scanl1
                      ,filter: filter
                      ,filterMap: filterMap
                      ,maybeCons: maybeCons
                      ,length: length
                      ,reverse: reverse
                      ,all: all
                      ,any: any
                      ,append: append
                      ,concat: concat
                      ,concatMap: concatMap
                      ,sum: sum
                      ,product: product
                      ,maximum: maximum
                      ,minimum: minimum
                      ,partition: partition
                      ,map2: map2
                      ,map3: map3
                      ,map4: map4
                      ,map5: map5
                      ,unzip: unzip
                      ,intersperse: intersperse
                      ,take: take
                      ,drop: drop
                      ,repeat: repeat
                      ,sort: sort
                      ,sortBy: sortBy
                      ,sortWith: sortWith};
   return _elm.List.values;
};
Elm.Maybe = Elm.Maybe || {};
Elm.Maybe.make = function (_elm) {
   "use strict";
   _elm.Maybe = _elm.Maybe || {};
   if (_elm.Maybe.values)
   return _elm.Maybe.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _P = _N.Ports.make(_elm),
   $moduleName = "Maybe";
   var withDefault = F2(function ($default,
   maybe) {
      return function () {
         switch (maybe.ctor)
         {case "Just": return maybe._0;
            case "Nothing":
            return $default;}
         _U.badCase($moduleName,
         "between lines 45 and 56");
      }();
   });
   var Nothing = {ctor: "Nothing"};
   var oneOf = function (maybes) {
      return function () {
         switch (maybes.ctor)
         {case "::": return function () {
                 switch (maybes._0.ctor)
                 {case "Just": return maybes._0;
                    case "Nothing":
                    return oneOf(maybes._1);}
                 _U.badCase($moduleName,
                 "between lines 64 and 73");
              }();
            case "[]": return Nothing;}
         _U.badCase($moduleName,
         "between lines 59 and 73");
      }();
   };
   var andThen = F2(function (maybeValue,
   callback) {
      return function () {
         switch (maybeValue.ctor)
         {case "Just":
            return callback(maybeValue._0);
            case "Nothing": return Nothing;}
         _U.badCase($moduleName,
         "between lines 110 and 112");
      }();
   });
   var Just = function (a) {
      return {ctor: "Just",_0: a};
   };
   var map = F2(function (f,
   maybe) {
      return function () {
         switch (maybe.ctor)
         {case "Just":
            return Just(f(maybe._0));
            case "Nothing": return Nothing;}
         _U.badCase($moduleName,
         "between lines 76 and 107");
      }();
   });
   _elm.Maybe.values = {_op: _op
                       ,andThen: andThen
                       ,map: map
                       ,withDefault: withDefault
                       ,oneOf: oneOf
                       ,Just: Just
                       ,Nothing: Nothing};
   return _elm.Maybe.values;
};
Elm.Mouse = Elm.Mouse || {};
Elm.Mouse.make = function (_elm) {
   "use strict";
   _elm.Mouse = _elm.Mouse || {};
   if (_elm.Mouse.values)
   return _elm.Mouse.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _P = _N.Ports.make(_elm),
   $moduleName = "Mouse",
   $Native$Mouse = Elm.Native.Mouse.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var clicks = $Native$Mouse.clicks;
   var isDown = $Native$Mouse.isDown;
   var y = $Native$Mouse.y;
   var x = $Native$Mouse.x;
   var position = $Native$Mouse.position;
   _elm.Mouse.values = {_op: _op
                       ,position: position
                       ,x: x
                       ,y: y
                       ,isDown: isDown
                       ,clicks: clicks};
   return _elm.Mouse.values;
};
Elm.Native.Array = {};
Elm.Native.Array.make = function(elm) {
    elm.Native = elm.Native || {};
    elm.Native.Array = elm.Native.Array || {};
    if (elm.Native.Array.values) return elm.Native.Array.values;
    if ('values' in Elm.Native.Array)
      return elm.Native.Array.values = Elm.Native.Array.values;

    var List = Elm.Native.List.make(elm);

    // A RRB-Tree has two distinct data types.
    // Leaf -> "height"  is always 0
    //         "table"   is an array of elements
    // Node -> "height"  is always greater than 0
    //         "table"   is an array of child nodes
    //         "lengths" is an array of accumulated lengths of the child nodes

    // M is the maximal table size. 32 seems fast. E is the allowed increase
    // of search steps when concatting to find an index. Lower values will 
    // decrease balancing, but will increase search steps.
    var M = 32;
    var E = 2;

    // An empty array.
    var empty = { ctor:"_Array", height:0, table:new Array() };

    function get(i, array) {
        if (i < 0 || i >= length(array)) {
            throw new Error("Index " + i + " is out of range. Check the length of " +
                            "your array first or use getMaybe or getWithDefault.");
        }
        return unsafeGet(i, array);
    }

    function unsafeGet(i, array) {
      for (var x = array.height; x > 0; x--) {
        var slot = i >> (x * 5);
        while (array.lengths[slot] <= i) {
          slot++;
        }
        if (slot > 0) {
          i -= array.lengths[slot - 1];
        }
        array = array.table[slot];
      }
      return array.table[i];
    }

    // Sets the value at the index i. Only the nodes leading to i will get
    // copied and updated.
    function set(i, item, array) {
      if (i < 0 || length(array) <= i) {
        return array;
      }
      return unsafeSet(i, item, array);
    }

    function unsafeSet(i, item, array) {
      array = nodeCopy(array);

      if (array.height == 0) {
        array.table[i] = item;
      } else {
        var slot = getSlot(i, array);
        if (slot > 0) {
          i -= array.lengths[slot - 1];
        }
        array.table[slot] = unsafeSet(i, item, array.table[slot]);
      }
      return array;
    }

    function initialize(len, f) {
      if (len == 0) { return empty; }
      var h = Math.floor(Math.log(len)/Math.log(M));
      return initialize_(f, h, 0, len);
    }

    function initialize_(f, h, from, to) {
      if (h == 0) {
        var table = new Array((to - from) % (M + 1));
        for (var i = 0; i < table.length; i++) {
          table[i] = f(from + i);
        }
        return { ctor:"_Array", height:0, table:table };
      }

      var step = Math.pow(M, h);
      var table = new Array(Math.ceil((to - from) / step));
      var lengths = new Array(table.length);
      for (var i = 0; i < table.length; i++) {
        table[i] = initialize_( f, h - 1, from + (i * step)
                              , Math.min(from + ((i + 1) * step), to));
        lengths[i] = length(table[i]) + (i > 0 ? lengths[i-1] : 0);
      }
      return { ctor:"_Array", height:h, table:table, lengths:lengths };
    }

    function fromList(list) {
      if (list == List.Nil) { return empty; }

      // Allocate M sized blocks (table) and write list elements to it.
      var table = new Array(M);
      var nodes = new Array();
      var i = 0;

      while (list.ctor !== '[]') {
        table[i] = list._0;
        list = list._1;
        i++;

        // table is full, so we can push a leaf containing it into the
        // next node.
        if (i == M) {
          fromListPush({ ctor:"_Array", height:0, table:table }
                      , nodes);
          table = new Array(M);
          i = 0;
        }
      }

      // Maybe there is something left on the table.
      if (i > 0) {
        fromListPush({ ctor:"_Array", height:0, table:table.splice(0,i) }
                    , nodes);
      }

      // Go through all of the nodes and eventually push them into higher nodes.
      for (var h = 0; h < nodes.length - 1; h++) {
        if (nodes[h].table.length > 0) {
          fromListPush(nodes[h], nodes);
        }
      }

      var head = nodes[nodes.length - 1];
      if (head.height > 0 && head.table.length == 1) {
        return head.table[0];
      } else {
        return head;
      }
    }

    // Push a node into a higher node as a child.
    function fromListPush(toPush, nodes) {
      var h = toPush.height;

      // Maybe the node on this height does not exist.
      if (nodes.length == h) {
        nodes.push({ ctor:"_Array", height:h + 1
                                  , table:new Array()
                                  , lengths:new Array() });
      }

      nodes[h].table.push(toPush);
      var len = length(toPush);
      if (nodes[h].lengths.length > 0) {
        len += nodes[h].lengths[nodes[h].lengths.length - 1];
      }
      nodes[h].lengths.push(len);

      if (nodes[h].table.length == M) {
        fromListPush(nodes[h], nodes);
        nodes[h] = { ctor:"_Array", height:h + 1
                                  , table:new Array()
                                  , lengths:new Array() };
      }
    }

    // Pushes an item via push_ to the bottom right of a tree.
    function push(item, a) {
      var pushed = push_(item, a);
      if (pushed !== null) {
        return pushed;
      }

      var newTree = create(item, a.height);
      return siblise(a, newTree);
    }

    // Recursively tries to push an item to the bottom-right most
    // tree possible. If there is no space left for the item,
    // null will be returned.
    function push_(item, a) {
      // Handle resursion stop at leaf level.
      if (a.height == 0) {
        if (a.table.length < M) {
          var newA = { ctor:"_Array", height:0, table:a.table.slice() };
          newA.table.push(item);
          return newA;
        } else {
          return null;
        }
      }

      // Recursively push
      var pushed = push_(item, botRight(a));

      // There was space in the bottom right tree, so the slot will
      // be updated.
      if (pushed != null) {
        var newA = nodeCopy(a);
        newA.table[newA.table.length - 1] = pushed;
        newA.lengths[newA.lengths.length - 1]++;
        return newA
      }

      // When there was no space left, check if there is space left
      // for a new slot with a tree which contains only the item
      // at the bottom.
      if (a.table.length < M) {
        var newSlot = create(item, a.height - 1);
        var newA = nodeCopy(a);
        newA.table.push(newSlot);
        newA.lengths.push(newA.lengths[newA.lengths.length - 1] + length(newSlot));
        return newA
      } else {
        return null;
      }
    }

    // Converts an array into a list of elements.
    function toList(a) {
      return toList_(List.Nil, a);
    }

    function toList_(list, a) {
      for (var i = a.table.length - 1; i >= 0; i--) {
        list = a.height == 0 ? List.Cons(a.table[i], list) : toList_(list, a.table[i]);
      }
      return list;
    }

    // Maps a function over the elements of an array.
    function map(f, a) {
      var newA = { ctor:"_Array", height:a.height, table:new Array(a.table) };
      if (a.height > 0) { newA.lengths = a.lengths; }
      for (var i = 0; i < a.table.length; i++) {
        newA.table[i] = a.height == 0 ? f(a.table[i]) : map(f, a.table[i]);
      }
      return newA;
    }

    // Maps a function over the elements with their index as first argument.
    function indexedMap(f, a) {
      return indexedMap_(f, a, 0);
    }

    function indexedMap_(f, a, from) {
      var newA = { ctor:"_Array", height:a.height, table:new Array(a.table) };
      if (a.height > 0) { newA.lengths = a.lengths; }
      for (var i = 0; i < a.table.length; i++) {
        newA.table[i] = a.height == 0 ? A2(f, from + i, a.table[i])
                                      : indexedMap_( f, a.table[i]
                                                   , i == 0 ? 0 : a.lengths[i - 1]);
      }
      return newA;
    }

    function foldl(f, b, a) {
      if (a.height == 0) {
        for (var i = 0; i < a.table.length; i++) {
          b = A2(f, a.table[i], b);
        }
      } else {
        for (var i = 0; i < a.table.length; i++) {
          b = foldl(f, b, a.table[i]);
        }
      }
      return b;
    }

    function foldr(f, b, a) {
      if (a.height == 0) {
        for (var i = a.table.length; i--; ) {
          b = A2(f, a.table[i], b);
        }
      } else {
        for (var i = a.table.length; i--; ) {
          b = foldr(f, b, a.table[i]);
        }
      }
      return b;
    }

    // TODO: currently, it slices the right, then the left. This can be
    // optimized.
    function slice(from, to, a) {
      if (from < 0) { from += length(a); }
      if (to < 0)   { to += length(a); }
      return sliceLeft(from, sliceRight(to, a));
    }

    function sliceRight(to, a) {
      if (to == length(a)) {
        return a;
      }

      // Handle leaf level.
      if (a.height == 0) {
        var newA = { ctor:"_Array", height:0 };
        newA.table = a.table.slice(0, to);
        return newA;
      }

      // Slice the right recursively.
      var right = getSlot(to, a);
      var sliced = sliceRight(to - (right > 0 ? a.lengths[right - 1] : 0), a.table[right]);

      // Maybe the a node is not even needed, as sliced contains the whole slice.
      if (right == 0) {
        return sliced;
      }

      // Create new node.
      var newA = { ctor:"_Array", height:a.height
                                , table:a.table.slice(0, right)
                                , lengths:a.lengths.slice(0, right) };
      if (sliced.table.length > 0) {
        newA.table[right] = sliced;
        newA.lengths[right] = length(sliced) + (right > 0 ? newA.lengths[right - 1] : 0);
      }
      return newA;
    }

    function sliceLeft(from, a) {
      if (from == 0) {
        return a;
      }

      // Handle leaf level.
      if (a.height == 0) {
        var newA = { ctor:"_Array", height:0 };
        newA.table = a.table.slice(from, a.table.length + 1);
        return newA;
      }

      // Slice the left recursively.
      var left = getSlot(from, a);
      var sliced = sliceLeft(from - (left > 0 ? a.lengths[left - 1] : 0), a.table[left]);

      // Maybe the a node is not even needed, as sliced contains the whole slice.
      if (left == a.table.length - 1) {
        return sliced;
      }

      // Create new node.
      var newA = { ctor:"_Array", height:a.height
                                , table:a.table.slice(left, a.table.length + 1)
                                , lengths:new Array(a.table.length - left) };
      newA.table[0] = sliced;
      var len = 0;
      for (var i = 0; i < newA.table.length; i++) {
        len += length(newA.table[i]);
        newA.lengths[i] = len;
      }

      return newA;
    }

    // Appends two trees.
    function append(a,b) {
      if (a.table.length === 0) {
        return b;
      }
      if (b.table.length === 0) {
        return a;
      }

      var c = append_(a, b);

      // Check if both nodes can be crunshed together.
      if (c[0].table.length + c[1].table.length <= M) {
        if (c[0].table.length === 0) {
          return c[1];
        }
        if (c[1].table.length === 0) {
          return c[0];
        }

        // Adjust .table and .lengths
        c[0].table = c[0].table.concat(c[1].table);
        if (c[0].height > 0) {
          var len = length(c[0]);
          for (var i = 0; i < c[1].lengths.length; i++) {
            c[1].lengths[i] += len;
          }
          c[0].lengths = c[0].lengths.concat(c[1].lengths);
        }

        return c[0];
      }

      if (c[0].height > 0) {
        var toRemove = calcToRemove(a, b);
        if (toRemove > E) {
          c = shuffle(c[0], c[1], toRemove);
        }
      }

      return siblise(c[0], c[1]);
    }

    // Returns an array of two nodes; right and left. One node _may_ be empty.
    function append_(a, b) {
      if (a.height === 0 && b.height === 0) {
        return [a, b];
      }

      if (a.height !== 1 || b.height !== 1) {
        if (a.height === b.height) {
          a = nodeCopy(a);
          b = nodeCopy(b);
          var appended = append_(botRight(a), botLeft(b));

          insertRight(a, appended[1]);
          insertLeft(b, appended[0]);
        } else if (a.height > b.height) {
          a = nodeCopy(a);
          var appended = append_(botRight(a), b);

          insertRight(a, appended[0]);
          b = parentise(appended[1], appended[1].height + 1);
        } else {
          b = nodeCopy(b);
          var appended = append_(a, botLeft(b));

          var left = appended[0].table.length === 0 ? 0 : 1;
          var right = left === 0 ? 1 : 0;
          insertLeft(b, appended[left]);
          a = parentise(appended[right], appended[right].height + 1);
        }
      }

      // Check if balancing is needed and return based on that.
      if (a.table.length === 0 || b.table.length === 0) {
        return [a,b];
      }

      var toRemove = calcToRemove(a, b);
      if (toRemove <= E) {
        return [a,b];
      }
      return shuffle(a, b, toRemove);
    }

    // Helperfunctions for append_. Replaces a child node at the side of the parent.
    function insertRight(parent, node) {
      var index = parent.table.length - 1;
      parent.table[index] = node;
      parent.lengths[index] = length(node)
      parent.lengths[index] += index > 0 ? parent.lengths[index - 1] : 0;
    }

    function insertLeft(parent, node) {
      if (node.table.length > 0) {
        parent.table[0] = node;
        parent.lengths[0] = length(node);

        var len = length(parent.table[0]);
        for (var i = 1; i < parent.lengths.length; i++) {
          len += length(parent.table[i]);
          parent.lengths[i] = len;
        }
      } else {
        parent.table.shift();
        for (var i = 1; i < parent.lengths.length; i++) {
          parent.lengths[i] = parent.lengths[i] - parent.lengths[0];
        }
        parent.lengths.shift();
      }
    }

    // Returns the extra search steps for E. Refer to the paper.
    function calcToRemove(a, b) {
      var subLengths = 0;
      for (var i = 0; i < a.table.length; i++) {
        subLengths += a.table[i].table.length;
      }
      for (var i = 0; i < b.table.length; i++) {
        subLengths += b.table[i].table.length;
      }

      var toRemove = a.table.length + b.table.length
      return toRemove - (Math.floor((subLengths - 1) / M) + 1);
    }

    // get2, set2 and saveSlot are helpers for accessing elements over two arrays.
    function get2(a, b, index) {
      return index < a.length ? a[index] : b[index - a.length];
    }

    function set2(a, b, index, value) {
      if (index < a.length) {
        a[index] = value;
      } else {
        b[index - a.length] = value;
      }
    }

    function saveSlot(a, b, index, slot) {
      set2(a.table, b.table, index, slot);

      var l = (index == 0 || index == a.lengths.length) ?
                0 : get2(a.lengths, a.lengths, index - 1);
      set2(a.lengths, b.lengths, index, l + length(slot));
    }

    // Creates a node or leaf with a given length at their arrays for perfomance.
    // Is only used by shuffle.
    function createNode(h, length) {
      if (length < 0) { length = 0; }
      var a = { ctor:"_Array", height:h, table:new Array(length) };
      if (h > 0) {
        a.lengths = new Array(length);
      }
      return a;
    }

    // Returns an array of two balanced nodes.
    function shuffle(a, b, toRemove) {
      var newA = createNode(a.height, Math.min(M, a.table.length + b.table.length - toRemove));
      var newB = createNode(a.height, newA.table.length - (a.table.length + b.table.length - toRemove));

      // Skip the slots with size M. More precise: copy the slot references
      // to the new node
      var read = 0;
      while (get2(a.table, b.table, read).table.length % M == 0) {
        set2(newA.table, newB.table, read, get2(a.table, b.table, read));
        set2(newA.lengths, newB.lengths, read, get2(a.lengths, b.lengths, read));
        read++;
      }

      // Pulling items from left to right, caching in a slot before writing
      // it into the new nodes.
      var write = read;
      var slot = new createNode(a.height - 1, 0);
      var from = 0;

      // If the current slot is still containing data, then there will be at
      // least one more write, so we do not break this loop yet.
      while (read - write - (slot.table.length > 0 ? 1 : 0) < toRemove) {
        // Find out the max possible items for copying.
        var source = get2(a.table, b.table, read);
        var to = Math.min(M - slot.table.length, source.table.length)

        // Copy and adjust size table.
        slot.table = slot.table.concat(source.table.slice(from, to));
        if (slot.height > 0) {
          var len = slot.lengths.length;
          for (var i = len; i < len + to - from; i++) {
            slot.lengths[i] = length(slot.table[i]);
            slot.lengths[i] += (i > 0 ? slot.lengths[i - 1] : 0);
          }
        }

        from += to;

        // Only proceed to next slots[i] if the current one was
        // fully copied.
        if (source.table.length <= to) {
          read++; from = 0;
        }

        // Only create a new slot if the current one is filled up.
        if (slot.table.length == M) {
          saveSlot(newA, newB, write, slot);
          slot = createNode(a.height - 1,0);
          write++;
        }
      }

      // Cleanup after the loop. Copy the last slot into the new nodes.
      if (slot.table.length > 0) {
        saveSlot(newA, newB, write, slot);
        write++;
      }

      // Shift the untouched slots to the left
      while (read < a.table.length + b.table.length ) {
        saveSlot(newA, newB, write, get2(a.table, b.table, read));
        read++; write++;
      }

      return [newA, newB];
    }

    // Navigation functions
    function botRight(a) { return a.table[a.table.length - 1]; }
    function botLeft(a)  { return a.table[0]; }

    // Copies a node for updating. Note that you should not use this if
    // only updating only one of "table" or "lengths" for performance reasons.
    function nodeCopy(a) {
      var newA = { ctor:"_Array", height:a.height
                                , table:a.table.slice() };
      if (a.height > 0) { newA.lengths = a.lengths.slice(); }
      return newA;
    }

    // Returns how many items are in the tree.
    function length(array) {
      if (array.height == 0) {
        return array.table.length;
      } else {
        return array.lengths[array.lengths.length - 1];
      }
    }

    // Calculates in which slot of "table" the item probably is, then
    // find the exact slot via forward searching in  "lengths". Returns the index.
    function getSlot(i, a) {
      var slot = i >> (5 * a.height);
      while (a.lengths[slot] <= i) {
        slot++;
      }
      return slot;
    }

    // Recursively creates a tree with a given height containing
    // only the given item.
    function create(item, h) {
      if (h == 0) {
        return { ctor:"_Array", height:0
                              , table:[item] };
      } else {
        return { ctor:"_Array", height:h
                              , table:[create(item, h - 1)]
                              , lengths:[1] };
      }
    }

    // Recursively creates a tree that contains the given tree.
    function parentise(tree, h) {
      if (h == tree.height) {
        return tree;
      } else {
        return { ctor:"_Array", height:h
                              , table:[parentise(tree, h - 1)]
                              , lengths:[length(tree)] };
      }
    }

    // Emphasizes blood brotherhood beneath two trees.
    function siblise(a, b) {
      return { ctor:"_Array", height:a.height + 1
                            , table:[a, b]
                            , lengths:[length(a), length(a) + length(b)] };
    }

    function toJSArray(a) {
      var jsArray = new Array(length(a));
      toJSArray_(jsArray, 0, a);
      return jsArray;
    }

    function toJSArray_(jsArray, i, a) {
      for (var t = 0; t < a.table.length; t++) {
        if (a.height == 0) {
          jsArray[i + t] = a.table[t];
        } else {
          var inc = t == 0 ? 0 : a.lengths[t - 1];
          toJSArray_(jsArray, i + inc, a.table[t]);
        }
      }
    }

    function fromJSArray(jsArray) {
      if (jsArray.length == 0) { return empty; }
      var h = Math.floor(Math.log(jsArray.length) / Math.log(M));
      return fromJSArray_(jsArray, h, 0, jsArray.length);
    }

    function fromJSArray_(jsArray, h, from, to) {
      if (h == 0) {
        return { ctor:"_Array", height:0
                              , table:jsArray.slice(from, to) };
      }

      var step = Math.pow(M, h);
      var table = new Array(Math.ceil((to - from) / step));
      var lengths = new Array(table.length);
      for (var i = 0; i < table.length; i++) {
        table[i] = fromJSArray_( jsArray, h - 1, from + (i * step)
                               , Math.min(from + ((i + 1) * step), to));
        lengths[i] = length(table[i]) + (i > 0 ? lengths[i-1] : 0);
      }
      return { ctor:"_Array", height:h, table:table, lengths:lengths };
    }

    Elm.Native.Array.values = {
      empty:empty,
      fromList:fromList,
      toList:toList,
      initialize:F2(initialize),
      append:F2(append),
      push:F2(push),
      slice:F3(slice),
      get:F2(get),
      set:F3(set),
      map:F2(map),
      indexedMap:F2(indexedMap),
      foldl:F3(foldl),
      foldr:F3(foldr),
      length:length,

      toJSArray:toJSArray,
      fromJSArray:fromJSArray
    };

    return elm.Native.Array.values = Elm.Native.Array.values;
}


Elm.Native.Basics = {};
Elm.Native.Basics.make = function(elm) {
  elm.Native = elm.Native || {};
  elm.Native.Basics = elm.Native.Basics || {};
  if (elm.Native.Basics.values) return elm.Native.Basics.values;

  var Utils = Elm.Native.Utils.make(elm);

  function div(a, b) {
      return (a/b)|0;
  }
  function rem(a, b) {
      return a % b;
  }
  function mod(a, b) {
        if (b === 0) {
            throw new Error("Cannot perform mod 0. Division by zero error.");
        }
        var r = a % b;
        var m = a === 0 ? 0 : (b > 0 ? (a >= 0 ? r : r+b) : -mod(-a,-b));

        return m === b ? 0 : m;
  }
  function logBase(base, n) {
      return Math.log(n) / Math.log(base);
  }
  function negate(n) {
      return -n;
  }
  function abs(n) {
      return n < 0 ? -n : n;
  }

  function min(a, b) {
      return Utils.cmp(a,b) < 0 ? a : b;
  }
  function max(a, b) {
      return Utils.cmp(a,b) > 0 ? a : b;
  }
  function clamp(lo, hi, n) {
      return Utils.cmp(n,lo) < 0 ? lo : Utils.cmp(n,hi) > 0 ? hi : n;
  }

  function xor(a, b) {
      return a !== b;
  }
  function not(b) {
      return !b;
  }
  function isInfinite(n) {
      return n === Infinity || n === -Infinity
  }

  function truncate(n) {
      return n|0;
  }

  function degrees(d) {
      return d * Math.PI / 180;
  }
  function turns(t) {
      return 2 * Math.PI * t;
  }
  function fromPolar(point) {
      var r = point._0;
      var t = point._1;
      return Utils.Tuple2(r * Math.cos(t), r * Math.sin(t));
  }
  function toPolar(point) {
      var x = point._0;
      var y = point._1;
      return Utils.Tuple2(Math.sqrt(x * x + y * y), Math.atan2(y,x));
  }

  var basics = {
      div: F2(div),
      rem: F2(rem),
      mod: F2(mod),

      pi: Math.PI,
      e: Math.E,
      cos: Math.cos,
      sin: Math.sin,
      tan: Math.tan,
      acos: Math.acos,
      asin: Math.asin,
      atan: Math.atan,
      atan2: F2(Math.atan2),

      degrees:  degrees,
      turns:  turns,
      fromPolar:  fromPolar,
      toPolar:  toPolar,

      sqrt: Math.sqrt,
      logBase: F2(logBase),
      negate: negate,
      abs: abs,
      min: F2(min),
      max: F2(max),
      clamp: F3(clamp),
      compare: Utils.compare,

      xor: F2(xor),
      not: not,

      truncate: truncate,
      ceiling: Math.ceil,
      floor: Math.floor,
      round: Math.round,
      toFloat: function(x) { return x; },
      isNaN: isNaN,
      isInfinite: isInfinite
  };

  return elm.Native.Basics.values = basics;
};

Elm.Native.Char = {};
Elm.Native.Char.make = function(elm) {
    elm.Native = elm.Native || {};
    elm.Native.Char = elm.Native.Char || {};
    if (elm.Native.Char.values) return elm.Native.Char.values;

    var Utils = Elm.Native.Utils.make(elm);

    function isBetween(lo,hi) { return function(chr) {
	var c = chr.charCodeAt(0);
	return lo <= c && c <= hi;
    };
                              }
    var isDigit = isBetween('0'.charCodeAt(0),'9'.charCodeAt(0));
    var chk1 = isBetween('a'.charCodeAt(0),'f'.charCodeAt(0));
    var chk2 = isBetween('A'.charCodeAt(0),'F'.charCodeAt(0));

    return elm.Native.Char.values = {
        fromCode : function(c) { return String.fromCharCode(c); },
        toCode   : function(c) { return c.charCodeAt(0); },
        toUpper  : function(c) { return Utils.chr(c.toUpperCase()); },
        toLower  : function(c) { return Utils.chr(c.toLowerCase()); },
        toLocaleUpper : function(c) { return Utils.chr(c.toLocaleUpperCase()); },
        toLocaleLower : function(c) { return Utils.chr(c.toLocaleLowerCase()); },
        isLower    : isBetween('a'.charCodeAt(0),'z'.charCodeAt(0)),
        isUpper    : isBetween('A'.charCodeAt(0),'Z'.charCodeAt(0)),
        isDigit    : isDigit,
        isOctDigit : isBetween('0'.charCodeAt(0),'7'.charCodeAt(0)),
        isHexDigit : function(c) { return isDigit(c) || chk1(c) || chk2(c); }
    };
};

Elm.Native.Color = {};
Elm.Native.Color.make = function(elm) {
    elm.Native = elm.Native || {};
    elm.Native.Color = elm.Native.Color || {};
    if (elm.Native.Color.values) return elm.Native.Color.values;

    function toCss(c) {
        var format = '';
        var colors = '';
        if (c.ctor === 'RGBA') {
            format = 'rgb';
            colors = c._0 + ', ' + c._1 + ', ' + c._2;
        } else {
            format = 'hsl';
            colors = (c._0 * 180 / Math.PI) + ', ' +
                     (c._1 * 100) + '%, ' +
                     (c._2 * 100) + '%';
        }
        if (c._3 === 1) {
            return format + '(' + colors + ')';
        } else {
            return format + 'a(' + colors + ', ' + c._3 + ')';
        }
    }

    return elm.Native.Color.values = {
        toCss:toCss
    };

};

Elm.Native.Debug = {};
Elm.Native.Debug.make = function(elm) {
    elm.Native = elm.Native || {};
    elm.Native.Debug = elm.Native.Debug || {};
    if (elm.Native.Debug.values)
    {
        return elm.Native.Debug.values;
    }

    var toString = Elm.Native.Show.make(elm).toString;

    function log(tag, value)
    {
        var msg = tag + ': ' + toString(value);
        var process = process || {};
        if (process.stdout) {
            process.stdout.write(msg);
        } else {
            console.log(msg);
        }
        return value;
    }

    function crash(message)
    {
        throw new Error(message);
    }

    function tracePath(tag, form)
    {
        if (elm.debug)
        {
            return elm.debug.trace(tag, form);
        }
        return form;
    }

    function watch(tag, value)
    {
        if (elm.debug)
        {
            elm.debug.watch(tag, value);
        }
        return value;
    }

    function watchSummary(tag, summarize, value)
    {
        if (elm.debug)
        {
            elm.debug.watch(tag, summarize(value));
        }
        return value;
    }

    return elm.Native.Debug.values = {
        crash: crash,
        tracePath: F2(tracePath),
        log: F2(log),
        watch: F2(watch),
        watchSummary:F3(watchSummary),
    };
};


// setup
Elm.Native = Elm.Native || {};
Elm.Native.Graphics = Elm.Native.Graphics || {};
Elm.Native.Graphics.Collage = Elm.Native.Graphics.Collage || {};

// definition
Elm.Native.Graphics.Collage.make = function(localRuntime) {
    'use strict';

    // attempt to short-circuit
    localRuntime.Native = localRuntime.Native || {};
    localRuntime.Native.Graphics = localRuntime.Native.Graphics || {};
    localRuntime.Native.Graphics.Collage = localRuntime.Native.Graphics.Collage || {};
    if ('values' in localRuntime.Native.Graphics.Collage) {
        return localRuntime.Native.Graphics.Collage.values;
    }

    // okay, we cannot short-ciruit, so now we define everything
    var Color = Elm.Native.Color.make(localRuntime);
    var List = Elm.Native.List.make(localRuntime);
    var Transform = Elm.Transform2D.make(localRuntime);

    var Element = Elm.Graphics.Element.make(localRuntime);
    var NativeElement = Elm.Native.Graphics.Element.make(localRuntime);


    function trace(ctx, path) {
        var points = List.toArray(path);
        var i = points.length - 1;
        if (i <= 0) {
            return;
        }
        ctx.moveTo(points[i]._0, points[i]._1);
        while (i--) {
            ctx.lineTo(points[i]._0, points[i]._1);
        }
        if (path.closed) {
            i = points.length - 1;
            ctx.lineTo(points[i]._0, points[i]._1);
        }
    }

    function line(ctx,style,path) {
        (style.dashing.ctor === '[]')
            ? trace(ctx, path)
            : customLineHelp(ctx, style, path);
        ctx.scale(1,-1);
        ctx.stroke();
    }

    function customLineHelp(ctx, style, path) {
        var points = List.toArray(path);
        if (path.closed) {
            points.push(points[0]);
        }
        var pattern = List.toArray(style.dashing);
        var i = points.length - 1;
        if (i <= 0) {
            return;
        }
        var x0 = points[i]._0, y0 = points[i]._1;
        var x1=0, y1=0, dx=0, dy=0, remaining=0, nx=0, ny=0;
        var pindex = 0, plen = pattern.length;
        var draw = true, segmentLength = pattern[0];
        ctx.moveTo(x0,y0);
        while (i--) {
            x1 = points[i]._0; y1 = points[i]._1;
            dx = x1 - x0; dy = y1 - y0;
            remaining = Math.sqrt(dx * dx + dy * dy);
            while (segmentLength <= remaining) {
                x0 += dx * segmentLength / remaining;
                y0 += dy * segmentLength / remaining;
                ctx[draw ? 'lineTo' : 'moveTo'](x0, y0);
                // update starting position
                dx = x1 - x0; dy = y1 - y0;
                remaining = Math.sqrt(dx * dx + dy * dy);
                // update pattern
                draw = !draw;
                pindex = (pindex + 1) % plen;
                segmentLength = pattern[pindex];
            }
            if (remaining > 0) {
                ctx[draw ? 'lineTo' : 'moveTo'](x1, y1);
                segmentLength -= remaining;
            }
            x0 = x1; y0 = y1;
        }
    }

    function drawLine(ctx, style, path) {
        ctx.lineWidth = style.width;

        var cap = style.cap.ctor;
        ctx.lineCap = cap === 'Flat'
            ? 'butt'
            : cap === 'Round'
                ? 'round'
                : 'square';

        var join = style.join.ctor;
        ctx.lineJoin = join === 'Smooth'
            ? 'round'
            : join === 'Sharp'
                ? 'miter'
                : 'bevel';

        ctx.miterLimit = style.join._0 || 10;
        ctx.strokeStyle = Color.toCss(style.color);

        return line(ctx, style, path);
    }

    function texture(redo, ctx, src) {
        var img = new Image();
        img.src = src;
        img.onload = redo;
        return ctx.createPattern(img, 'repeat');
    }

    function gradient(ctx, grad) {
        var g;
        var stops = [];
        if (grad.ctor === 'Linear') {
            var p0 = grad._0, p1 = grad._1;
            g = ctx.createLinearGradient(p0._0, -p0._1, p1._0, -p1._1);
            stops = List.toArray(grad._2);
        } else {
            var p0 = grad._0, p2 = grad._2;
            g = ctx.createRadialGradient(p0._0, -p0._1, grad._1, p2._0, -p2._1, grad._3);
            stops = List.toArray(grad._4);
        }
        var len = stops.length;
        for (var i = 0; i < len; ++i) {
            var stop = stops[i];
            g.addColorStop(stop._0, Color.toCss(stop._1));
        }
        return g;
    }

    function drawShape(redo, ctx, style, path) {
        trace(ctx, path);
        var sty = style.ctor;
        ctx.fillStyle = sty === 'Solid'
            ? Color.toCss(style._0)
            : sty === 'Texture'
                ? texture(redo, ctx, style._0)
                : gradient(ctx, style._0);

        ctx.scale(1,-1);
        ctx.fill();
    }

    function drawImage(redo, ctx, form) {
        var img = new Image();
        img.onload = redo;
        img.src = form._3;
        var w = form._0,
            h = form._1,
            pos = form._2,
            srcX = pos._0,
            srcY = pos._1,
            srcW = w,
            srcH = h,
            destX = -w/2,
            destY = -h/2,
            destW = w,
            destH = h;

        ctx.scale(1,-1);
        ctx.drawImage(img, srcX, srcY, srcW, srcH, destX, destY, destW, destH);
    }

    function renderForm(redo, ctx, form) {
        ctx.save();
        var x = form.x, y = form.y, theta = form.theta, scale = form.scale;
        if (x !== 0 || y !== 0) ctx.translate(x, y);
        if (theta !== 0) ctx.rotate(theta);
        if (scale !== 1) ctx.scale(scale,scale);
        if (form.alpha !== 1) ctx.globalAlpha = ctx.globalAlpha * form.alpha;
        ctx.beginPath();
        var f = form.form;
        switch(f.ctor) {
        case 'FPath' : drawLine(ctx, f._0, f._1); break;
        case 'FImage': drawImage(redo, ctx, f); break;
        case 'FShape':
          if (f._0.ctor === 'Line') {
            f._1.closed = true;
            drawLine(ctx, f._0._0, f._1);
          } else {
            drawShape(redo, ctx, f._0._0, f._1);
          }
        break;
        }
        ctx.restore();
    }

    function formToMatrix(form) {
       var scale = form.scale;
       var matrix = A6( Transform.matrix, scale, 0, 0, scale, form.x, form.y );

       var theta = form.theta
       if (theta !== 0) {
           matrix = A2( Transform.multiply, matrix, Transform.rotation(theta) );
       }

       return matrix;
    }

    function str(n) {
        if (n < 0.00001 && n > -0.00001) return 0;
        return n;
    }

    function makeTransform(w, h, form, matrices) {
        var props = form.form._0.props;
        var m = A6( Transform.matrix, 1, 0, 0, -1,
                    (w - props.width ) / 2,
                    (h - props.height) / 2 );
        var len = matrices.length;
        for (var i = 0; i < len; ++i) {
            m = A2( Transform.multiply, m, matrices[i] );
        }
        m = A2( Transform.multiply, m, formToMatrix(form) );

        return 'matrix(' + str( m[0]) + ', ' + str( m[3]) + ', ' +
                           str(-m[1]) + ', ' + str(-m[4]) + ', ' +
                           str( m[2]) + ', ' + str( m[5]) + ')';
    }

    function stepperHelp(list) {
        var arr = List.toArray(list);
        var i = 0;
        function peekNext() {
            return i < arr.length ? arr[i].form.ctor : '';
        }
        // assumes that there is a next element
        function next() {
            var out = arr[i];
            ++i;
            return out;
        }
        return {
            peekNext:peekNext,
            next:next
        };
    }

    function formStepper(forms) {
        var ps = [stepperHelp(forms)];
        var matrices = [];
        var alphas = [];
        function peekNext() {
            var len = ps.length;
            var formType = '';
            for (var i = 0; i < len; ++i ) {
                if (formType = ps[i].peekNext()) return formType;
            }
            return '';
        }
        // assumes that there is a next element
        function next(ctx) {
            while (!ps[0].peekNext()) {
                ps.shift();
                matrices.pop();
                alphas.shift();
                if (ctx) { ctx.restore(); }
            }
            var out = ps[0].next();
            var f = out.form;
            if (f.ctor === 'FGroup') {
                ps.unshift(stepperHelp(f._1));
                var m = A2(Transform.multiply, f._0, formToMatrix(out));
                ctx.save();
                ctx.transform(m[0], m[3], m[1], m[4], m[2], m[5]);
                matrices.push(m);

                var alpha = (alphas[0] || 1) * out.alpha;
                alphas.unshift(alpha);
                ctx.globalAlpha = alpha;
            }
            return out;
        }
        function transforms() { return matrices; }
        function alpha() { return alphas[0] || 1; }
        return {
            peekNext:peekNext,
            next:next,
            transforms:transforms,
            alpha:alpha
        };
    }

    function makeCanvas(w,h) {
        var canvas = NativeElement.createNode('canvas');
        canvas.style.width  = w + 'px';
        canvas.style.height = h + 'px';
        canvas.style.display = "block";
        canvas.style.position = "absolute";
        canvas.width  = w;
        canvas.height = h;
        return canvas;
    }

    function render(model) {
        var div = NativeElement.createNode('div');
        div.style.overflow = 'hidden';
        div.style.position = 'relative';
        update(div, model, model);
        return div;
    }

    function nodeStepper(w,h,div) {
        var kids = div.childNodes;
        var i = 0;
        function transform(transforms, ctx) {
            ctx.translate(w/2, h/2);
            ctx.scale(1,-1);
            var len = transforms.length;
            for (var i = 0; i < len; ++i) {
                var m = transforms[i];
                ctx.save();
                ctx.transform(m[0], m[3], m[1], m[4], m[2], m[5]);
            }
            return ctx;
        }
        function nextContext(transforms) {
            while (i < kids.length) {
                var node = kids[i];
                if (node.getContext) {
                    node.width = w;
                    node.height = h;
                    node.style.width = w + 'px';
                    node.style.height = h + 'px';
                    ++i;
                    return transform(transforms, node.getContext('2d'));
                }
                div.removeChild(node);
            }
            var canvas = makeCanvas(w,h);
            div.appendChild(canvas);
            // we have added a new node, so we must step our position
            ++i;
            return transform(transforms, canvas.getContext('2d'));
        }
        function addElement(matrices, alpha, form) {
            var kid = kids[i];
            var elem = form.form._0;

            var node = (!kid || kid.getContext)
                ? NativeElement.render(elem)
                : NativeElement.update(kid, kid.oldElement, elem);

            node.style.position = 'absolute';
            node.style.opacity = alpha * form.alpha * elem.props.opacity;
            NativeElement.addTransform(node.style, makeTransform(w, h, form, matrices));
            node.oldElement = elem;
            ++i;
            if (!kid) {
                div.appendChild(node);
            } else {
                div.insertBefore(node, kid);
            }
        }
        function clearRest() {
            while (i < kids.length) {
                div.removeChild(kids[i]);
            }
        }
        return { nextContext:nextContext, addElement:addElement, clearRest:clearRest };
    }


    function update(div, _, model) {
        var w = model.w;
        var h = model.h;

        var forms = formStepper(model.forms);
        var nodes = nodeStepper(w,h,div);
        var ctx = null;
        var formType = '';

        while (formType = forms.peekNext()) {
            // make sure we have context if we need it
            if (ctx === null && formType !== 'FElement') {
                ctx = nodes.nextContext(forms.transforms());
                ctx.globalAlpha = forms.alpha();
            }

            var form = forms.next(ctx);
            // if it is FGroup, all updates are made within formStepper when next is called.
            if (formType === 'FElement') {
                // update or insert an element, get a new context
                nodes.addElement(forms.transforms(), forms.alpha(), form);
                ctx = null;
            } else if (formType !== 'FGroup') {
                renderForm(function() { update(div, model, model); }, ctx, form);
            }
        }
        nodes.clearRest();
        return div;
    }


    function collage(w,h,forms) {
        return A3(Element.newElement, w, h, {
            ctor: 'Custom',
            type: 'Collage',
            render: render,
            update: update,
            model: {w:w, h:h, forms:forms}
      	});
    }

    return localRuntime.Native.Graphics.Collage.values = {
        collage:F3(collage)
    };
};


// setup
Elm.Native = Elm.Native || {};
Elm.Native.Graphics = Elm.Native.Graphics || {};
Elm.Native.Graphics.Element = Elm.Native.Graphics.Element || {};

// definition
Elm.Native.Graphics.Element.make = function(localRuntime) {
    'use strict';

    // attempt to short-circuit
    localRuntime.Native = localRuntime.Native || {};
    localRuntime.Native.Graphics = localRuntime.Native.Graphics || {};
    localRuntime.Native.Graphics.Element = localRuntime.Native.Graphics.Element || {};
    if ('values' in localRuntime.Native.Graphics.Element) {
        return localRuntime.Native.Graphics.Element.values;
    }

    var Color = Elm.Native.Color.make(localRuntime);
    var List = Elm.Native.List.make(localRuntime);
    var Utils = Elm.Native.Utils.make(localRuntime);


    function createNode(elementType) {
        var node = document.createElement(elementType);
        node.style.padding = "0";
        node.style.margin = "0";
        return node;
    }

    function setProps(elem, node) {
        var props = elem.props;

        var element = elem.element;
        var width = props.width - (element.adjustWidth || 0);
        var height = props.height - (element.adjustHeight || 0);
        node.style.width  = (width |0) + 'px';
        node.style.height = (height|0) + 'px';

        if (props.opacity !== 1) {
            node.style.opacity = props.opacity;
        }

        if (props.color.ctor === 'Just') {
            node.style.backgroundColor = Color.toCss(props.color._0);
        }

        if (props.tag !== '') {
            node.id = props.tag;
        }

        if (props.hover.ctor !== '_Tuple0') {
            addHover(node, props.hover);
        }

        if (props.click.ctor !== '_Tuple0') {
            addClick(node, props.click);
        }

        if (props.href !== '') {
            var anchor = createNode('a');
            anchor.href = props.href;
            anchor.style.display = 'block';
            anchor.style.pointerEvents = 'auto';
            anchor.appendChild(node);
            node = anchor;
        }

        return node;
    }

    function addClick(e, handler) {
        e.style.pointerEvents = 'auto';
        e.elm_click_handler = handler;
        function trigger(ev) {
            e.elm_click_handler(Utils.Tuple0);
            ev.stopPropagation();
        }
        e.elm_click_trigger = trigger;
        e.addEventListener('click', trigger);
    }

    function removeClick(e, handler) {
        if (e.elm_click_trigger) {
            e.removeEventListener('click', e.elm_click_trigger);
            e.elm_click_trigger = null;
            e.elm_click_handler = null;
        }
    }

    function addHover(e, handler) {
        e.style.pointerEvents = 'auto';
        e.elm_hover_handler = handler;
        e.elm_hover_count = 0;

        function over(evt) {
            if (e.elm_hover_count++ > 0) return;
            e.elm_hover_handler(true);
            evt.stopPropagation();
        }
        function out(evt) {
            if (e.contains(evt.toElement || evt.relatedTarget)) return;
            e.elm_hover_count = 0;
            e.elm_hover_handler(false);
            evt.stopPropagation();
        }
        e.elm_hover_over = over;
        e.elm_hover_out = out;
        e.addEventListener('mouseover', over);
        e.addEventListener('mouseout', out);
    }

    function removeHover(e) {
        e.elm_hover_handler = null;
        if (e.elm_hover_over) {
            e.removeEventListener('mouseover', e.elm_hover_over);
            e.elm_hover_over = null;
        }
        if (e.elm_hover_out) {
            e.removeEventListener('mouseout', e.elm_hover_out);
            e.elm_hover_out = null;
        }
    }

    function image(props, img) {
        switch (img._0.ctor) {
        case 'Plain':   return plainImage(img._3);
        case 'Fitted':  return fittedImage(props.width, props.height, img._3);
        case 'Cropped': return croppedImage(img,props.width,props.height,img._3);
        case 'Tiled':   return tiledImage(img._3);
        }
    }

    function plainImage(src) {
        var img = createNode('img');
        img.src = src;
        img.name = src;
        img.style.display = "block";
        return img;
    }

    function tiledImage(src) {
        var div = createNode('div');
        div.style.backgroundImage = 'url(' + src + ')';
        return div;
    }

    function fittedImage(w, h, src) {
        var div = createNode('div');
        div.style.background = 'url(' + src + ') no-repeat center';
        div.style.webkitBackgroundSize = 'cover';
        div.style.MozBackgroundSize = 'cover';
        div.style.OBackgroundSize = 'cover';
        div.style.backgroundSize = 'cover';
        return div;
    }

    function croppedImage(elem, w, h, src) {
        var pos = elem._0._0;
        var e = createNode('div');
        e.style.overflow = "hidden";

        var img = createNode('img');
        img.onload = function() {
            var sw = w / elem._1, sh = h / elem._2;
            img.style.width = ((this.width * sw)|0) + 'px';
            img.style.height = ((this.height * sh)|0) + 'px';
            img.style.marginLeft = ((- pos._0 * sw)|0) + 'px';
            img.style.marginTop = ((- pos._1 * sh)|0) + 'px';
        };
        img.src = src;
        img.name = src;
        e.appendChild(img);
        return e;
    }

    function goOut(node) {
        node.style.position = 'absolute';
        return node;
    }
    function goDown(node) {
        return node;
    }
    function goRight(node) {
        node.style.styleFloat = 'left';
        node.style.cssFloat = 'left';
        return node;
    }

    var directionTable = {
        DUp    : goDown,
        DDown  : goDown,
        DLeft  : goRight,
        DRight : goRight,
        DIn    : goOut,
        DOut   : goOut
    };
    function needsReversal(dir) {
        return dir == 'DUp' || dir == 'DLeft' || dir == 'DIn';
    }

    function flow(dir,elist) {
        var array = List.toArray(elist);
        var container = createNode('div');
        var goDir = directionTable[dir];
        if (goDir == goOut) {
            container.style.pointerEvents = 'none';
        }
        if (needsReversal(dir)) {
            array.reverse();
        }
        var len = array.length;
        for (var i = 0; i < len; ++i) {
            container.appendChild(goDir(render(array[i])));
        }
        return container;
    }

    function toPos(pos) {
        switch(pos.ctor) {
        case "Absolute": return  pos._0 + "px";
        case "Relative": return (pos._0 * 100) + "%";
        }
    }

    // must clear right, left, top, bottom, and transform
    // before calling this function
    function setPos(pos,elem,e) {
        var element = elem.element;
        var props = elem.props;
        var w = props.width + (element.adjustWidth ? element.adjustWidth : 0);
        var h = props.height + (element.adjustHeight ? element.adjustHeight : 0);

        e.style.position = 'absolute';
        e.style.margin = 'auto';
        var transform = '';
        switch(pos.horizontal.ctor) {
        case 'P': e.style.right = toPos(pos.x); e.style.removeProperty('left'); break;
        case 'Z': transform = 'translateX(' + ((-w/2)|0) + 'px) ';
        case 'N': e.style.left = toPos(pos.x); e.style.removeProperty('right'); break;
        }
        switch(pos.vertical.ctor) {
        case 'N': e.style.bottom = toPos(pos.y); e.style.removeProperty('top'); break;
        case 'Z': transform += 'translateY(' + ((-h/2)|0) + 'px)';
        case 'P': e.style.top = toPos(pos.y); e.style.removeProperty('bottom'); break;
        }
        if (transform !== '') {
            addTransform(e.style, transform);
        }
        return e;
    }

    function addTransform(style, transform) {
        style.transform       = transform;
        style.msTransform     = transform;
        style.MozTransform    = transform;
        style.webkitTransform = transform;
        style.OTransform      = transform;
    }

    function container(pos,elem) {
        var e = render(elem);
        setPos(pos, elem, e);
        var div = createNode('div');
        div.style.position = 'relative';
        div.style.overflow = 'hidden';
        div.appendChild(e);
        return div;
    }

    function rawHtml(elem) {
        var html = elem.html;
        var guid = elem.guid;
        var align = elem.align;

        var div = createNode('div');
        div.innerHTML = html;
        div.style.visibility = "hidden";
        if (align) {
            div.style.textAlign = align;
        }
        div.style.visibility = 'visible';
        div.style.pointerEvents = 'auto';
        return div;
    }

    function render(elem) {
        return setProps(elem, makeElement(elem));
    }
    function makeElement(e) {
        var elem = e.element;
        switch(elem.ctor) {
        case 'Image':     return image(e.props, elem);
        case 'Flow':      return flow(elem._0.ctor, elem._1);
        case 'Container': return container(elem._0, elem._1);
        case 'Spacer':    return createNode('div');
        case 'RawHtml':   return rawHtml(elem);
        case 'Custom':    return elem.render(elem.model);
        }
    }

    function updateAndReplace(node, curr, next) {
        var newNode = update(node, curr, next);
        if (newNode !== node) {
            node.parentNode.replaceChild(newNode, node);
        }
        return newNode;
    }

    function update(node, curr, next) {
        var rootNode = node;
        if (node.tagName === 'A') {
            node = node.firstChild;
        }
        if (curr.props.id === next.props.id) {
            updateProps(node, curr, next);
            return rootNode;
        }
        if (curr.element.ctor !== next.element.ctor) {
            return render(next);
        }
        var nextE = next.element;
        var currE = curr.element;
        switch(nextE.ctor) {
        case "Spacer":
            updateProps(node, curr, next);
            return rootNode;

        case "RawHtml":
            if(currE.html.valueOf() !== nextE.html.valueOf()) {
                node.innerHTML = nextE.html;
            }
            updateProps(node, curr, next);
            return rootNode;

        case "Image":
            if (nextE._0.ctor === 'Plain') {
                if (nextE._3 !== currE._3) {
                    node.src = nextE._3;
                }
            } else if (!Utils.eq(nextE,currE) ||
                       next.props.width !== curr.props.width ||
                       next.props.height !== curr.props.height) {
                return render(next);
            }
            updateProps(node, curr, next);
            return rootNode;

        case "Flow":
            var arr = List.toArray(nextE._1);
            for (var i = arr.length; i--; ) {
                arr[i] = arr[i].element.ctor;
            }
            if (nextE._0.ctor !== currE._0.ctor) {
                return render(next);
            }
            var nexts = List.toArray(nextE._1);
            var kids = node.childNodes;
            if (nexts.length !== kids.length) {
                return render(next);
            }
            var currs = List.toArray(currE._1);
            var dir = nextE._0.ctor;
            var goDir = directionTable[dir];
            var toReverse = needsReversal(dir);
            var len = kids.length;
            for (var i = len; i-- ;) {
                var subNode = kids[toReverse ? len - i - 1 : i];
                goDir(updateAndReplace(subNode, currs[i], nexts[i]));
            }
            updateProps(node, curr, next);
            return rootNode;

        case "Container":
            var subNode = node.firstChild;
            var newSubNode = updateAndReplace(subNode, currE._1, nextE._1);
            setPos(nextE._0, nextE._1, newSubNode);
            updateProps(node, curr, next);
            return rootNode;

        case "Custom":
            if (currE.type === nextE.type) {
                var updatedNode = nextE.update(node, currE.model, nextE.model);
                updateProps(updatedNode, curr, next);
                return updatedNode;
            } else {
                return render(next);
            }
        }
    }

    function updateProps(node, curr, next) {
        var nextProps = next.props;
        var currProps = curr.props;

        var element = next.element;
        var width = nextProps.width - (element.adjustWidth || 0);
        var height = nextProps.height - (element.adjustHeight || 0);
        if (width !== currProps.width) {
            node.style.width = (width|0) + 'px';
        }
        if (height !== currProps.height) {
            node.style.height = (height|0) + 'px';
        }

        if (nextProps.opacity !== currProps.opacity) {
            node.style.opacity = nextProps.opacity;
        }

        var nextColor = nextProps.color.ctor === 'Just'
            ? Color.toCss(nextProps.color._0)
            : '';
        if (node.style.backgroundColor !== nextColor) {
            node.style.backgroundColor = nextColor;
        }

        if (nextProps.tag !== currProps.tag) {
            node.id = nextProps.tag;
        }

        if (nextProps.href !== currProps.href) {
            if (currProps.href === '') {
                // add a surrounding href
                var anchor = createNode('a');
                anchor.href = nextProps.href;
                anchor.style.display = 'block';
                anchor.style.pointerEvents = 'auto';

                node.parentNode.replaceChild(anchor, node);
                anchor.appendChild(node);
            } else if (nextProps.href === '') {
                // remove the surrounding href
                var anchor = node.parentNode;
                anchor.parentNode.replaceChild(node, anchor);
            } else {
                // just update the link
                node.parentNode.href = nextProps.href;
            }
        }

        // update click and hover handlers
        var removed = false;

        // update hover handlers
        if (currProps.hover.ctor === '_Tuple0') {
            if (nextProps.hover.ctor !== '_Tuple0') {
                addHover(node, nextProps.hover);
            }
        }
        else {
            if (nextProps.hover.ctor === '_Tuple0') {
                removed = true;
                removeHover(node);
            }
            else {
                node.elm_hover_handler = nextProps.hover;
            }
        }

        // update click handlers
        if (currProps.click.ctor === '_Tuple0') {
            if (nextProps.click.ctor !== '_Tuple0') {
                addClick(node, nextProps.click);
            }
        }
        else {
            if (nextProps.click.ctor === '_Tuple0') {
                removed = true;
                removeClick(node);
            } else {
                node.elm_click_handler = nextProps.click;
            }
        }

        // stop capturing clicks if 
        if (removed
            && nextProps.hover.ctor === '_Tuple0'
            && nextProps.click.ctor === '_Tuple0')
        {
            node.style.pointerEvents = 'none';
        }
    }


    function htmlHeight(width, rawHtml) {
        // create dummy node
        var temp = document.createElement('div');
        temp.innerHTML = rawHtml.html;
        if (width > 0) {
            temp.style.width = width + "px";
        }
        temp.style.visibility = "hidden";
        temp.style.styleFloat = "left";
        temp.style.cssFloat   = "left";

        document.body.appendChild(temp);

        // get dimensions
        var style = window.getComputedStyle(temp, null);
        var w = Math.ceil(style.getPropertyValue("width").slice(0,-2) - 0);
        var h = Math.ceil(style.getPropertyValue("height").slice(0,-2) - 0);
        document.body.removeChild(temp);
        return Utils.Tuple2(w,h);
    }


    return localRuntime.Native.Graphics.Element.values = {
        render: render,
        update: update,
        updateAndReplace: updateAndReplace,

        createNode: createNode,
        addTransform: addTransform,
        htmlHeight: F2(htmlHeight),
        guid: Utils.guid
    };

};

// setup
Elm.Native = Elm.Native || {};
Elm.Native.Graphics = Elm.Native.Graphics || {};
Elm.Native.Graphics.Input = Elm.Native.Graphics.Input || {};

// definition
Elm.Native.Graphics.Input.make = function(localRuntime) {
    'use strict';

    // attempt to short-circuit
    if ('values' in Elm.Native.Graphics.Input) {
        return Elm.Native.Graphics.Input.values;
    }

    var Color = Elm.Native.Color.make(localRuntime);
    var List = Elm.Native.List.make(localRuntime);
    var Text = Elm.Native.Text.make(localRuntime);
    var Utils = Elm.Native.Utils.make(localRuntime);

    var Element = Elm.Graphics.Element.make(localRuntime);
    var NativeElement = Elm.Native.Graphics.Element.make(localRuntime);


    function renderDropDown(model) {
        var drop = NativeElement.createNode('select');
        drop.style.border = '0 solid';
        drop.style.pointerEvents = 'auto';
        drop.style.display = 'block';

        drop.elm_values = List.toArray(model.values);
        drop.elm_handler = model.handler;
        var values = drop.elm_values;

        for (var i = 0; i < values.length; ++i) {
            var option = NativeElement.createNode('option');
            var name = values[i]._0;
            option.value = name;
            option.innerHTML = name;
            drop.appendChild(option);
        }
        drop.addEventListener('change', function() {
            drop.elm_handler(drop.elm_values[drop.selectedIndex]._1)();
        });

        return drop;
    }

    function updateDropDown(node, oldModel, newModel) {
        node.elm_values = List.toArray(newModel.values);
        node.elm_handler = newModel.handler;

        var values = node.elm_values;
        var kids = node.childNodes;
        var kidsLength = kids.length;

        var i = 0;
        for (; i < kidsLength && i < values.length; ++i) {
            var option = kids[i];
            var name = values[i]._0;
            option.value = name;
            option.innerHTML = name;
        }
        for (; i < kidsLength; ++i) {
            node.removeChild(node.lastChild);
        }
        for (; i < values.length; ++i) {
            var option = NativeElement.createNode('option');
            var name = values[i]._0;
            option.value = name;
            option.innerHTML = name;
            node.appendChild(option);
        }
        return node;
    }

    function dropDown(handler, values) {
        return A3(Element.newElement, 100, 24, {
            ctor: 'Custom',
            type: 'DropDown',
            render: renderDropDown,
            update: updateDropDown,
            model: {
                values: values,
                handler: handler
            }
        });
    }

    function renderButton(model) {
        var node = NativeElement.createNode('button');
        node.style.display = 'block';
        node.style.pointerEvents = 'auto';
        node.elm_message = model.message;
        function click() {
            node.elm_message();
        }
        node.addEventListener('click', click);
        node.innerHTML = model.text;
        return node;
    }

    function updateButton(node, oldModel, newModel) {
        node.elm_message = newModel.message;
        var txt = newModel.text;
        if (oldModel.text !== txt) {
            node.innerHTML = txt;
        }
        return node;
    }

    function button(message, text) {
        return A3(Element.newElement, 100, 40, {
            ctor: 'Custom',
            type: 'Button',
            render: renderButton,
            update: updateButton,
            model: {
                message: message,
                text:text
            }
        });
    }

    function renderCustomButton(model) {
        var btn = NativeElement.createNode('div');
        btn.style.pointerEvents = 'auto';
        btn.elm_message = model.message;

        btn.elm_up    = NativeElement.render(model.up);
        btn.elm_hover = NativeElement.render(model.hover);
        btn.elm_down  = NativeElement.render(model.down);

        btn.elm_up.style.display = 'block';
        btn.elm_hover.style.display = 'none';
        btn.elm_down.style.display = 'none';
  
        btn.appendChild(btn.elm_up);
        btn.appendChild(btn.elm_hover);
        btn.appendChild(btn.elm_down);

        function swap(visibleNode, hiddenNode1, hiddenNode2) {
            visibleNode.style.display = 'block';
            hiddenNode1.style.display = 'none';
            hiddenNode2.style.display = 'none';
        }

        var overCount = 0;
        function over(e) {
            if (overCount++ > 0) return;
            swap(btn.elm_hover, btn.elm_down, btn.elm_up);
        }
        function out(e) {
            if (btn.contains(e.toElement || e.relatedTarget)) return;
            overCount = 0;
            swap(btn.elm_up, btn.elm_down, btn.elm_hover);
        }
        function up() {
            swap(btn.elm_hover, btn.elm_down, btn.elm_up);
            btn.elm_message();
        }
        function down() {
            swap(btn.elm_down, btn.elm_hover, btn.elm_up);
        }

        btn.addEventListener('mouseover', over);
        btn.addEventListener('mouseout' , out);
        btn.addEventListener('mousedown', down);
        btn.addEventListener('mouseup'  , up);

        return btn;
    }

    function updateCustomButton(node, oldModel, newModel) {
        node.elm_message = newModel.message;

        var kids = node.childNodes;
        var styleUp    = kids[0].style.display;
        var styleHover = kids[1].style.display;
        var styleDown  = kids[2].style.display;

        NativeElement.updateAndReplace(kids[0], oldModel.up, newModel.up);
        NativeElement.updateAndReplace(kids[1], oldModel.hover, newModel.hover);
        NativeElement.updateAndReplace(kids[2], oldModel.down, newModel.down);

        var kids = node.childNodes;
        kids[0].style.display = styleUp;
        kids[1].style.display = styleHover;
        kids[2].style.display = styleDown;

        return node;
    }

    function max3(a,b,c) {
        var ab = a > b ? a : b;
        return ab > c ? ab : c;
    }

    function customButton(message, up, hover, down) {
        return A3(Element.newElement,
                  max3(up.props.width, hover.props.width, down.props.width),
                  max3(up.props.height, hover.props.height, down.props.height),
                  { ctor: 'Custom',
                    type: 'CustomButton',
                    render: renderCustomButton,
                    update: updateCustomButton,
                    model: {
                        message: message,
                        up: up,
                        hover: hover,
                        down: down
                    }
                  });
    }

    function renderCheckbox(model) {
        var node = NativeElement.createNode('input');
        node.type = 'checkbox';
        node.checked = model.checked;
        node.style.display = 'block';
        node.style.pointerEvents = 'auto';
        node.elm_handler = model.handler;
        function change() {
            node.elm_handler(node.checked)();
        }
        node.addEventListener('change', change);
        return node;
    }

    function updateCheckbox(node, oldModel, newModel) {
        node.elm_handler = newModel.handler;
        node.checked = newModel.checked;
        return node;
    }

    function checkbox(handler, checked) {
        return A3(Element.newElement, 13, 13, {
            ctor: 'Custom',
            type: 'CheckBox',
            render: renderCheckbox,
            update: updateCheckbox,
            model: { handler:handler, checked:checked }
        });
    }

    function setRange(node, start, end, dir) {
        if (node.parentNode) {
            node.setSelectionRange(start, end, dir);
        } else {
            setTimeout(function(){node.setSelectionRange(start, end, dir);}, 0);
        }
    }

    function updateIfNeeded(css, attribute, latestAttribute) {
        if (css[attribute] !== latestAttribute) {
            css[attribute] = latestAttribute;
        }
    }
    function cssDimensions(dimensions) {
        return dimensions.top    + 'px ' +
               dimensions.right  + 'px ' +
               dimensions.bottom + 'px ' +
               dimensions.left   + 'px';
    }
    function updateFieldStyle(css, style) {
        updateIfNeeded(css, 'padding', cssDimensions(style.padding));

        var outline = style.outline;
        updateIfNeeded(css, 'border-width', cssDimensions(outline.width));
        updateIfNeeded(css, 'border-color', Color.toCss(outline.color));
        updateIfNeeded(css, 'border-radius', outline.radius + 'px');

        var highlight = style.highlight;
        if (highlight.width === 0) {
            css.outline = 'none';
        } else {
            updateIfNeeded(css, 'outline-width', highlight.width + 'px');
            updateIfNeeded(css, 'outline-color', Color.toCss(highlight.color));
        }

        var textStyle = style.style;
        updateIfNeeded(css, 'color', Color.toCss(textStyle.color));
        if (textStyle.typeface.ctor !== '[]') {
            updateIfNeeded(css, 'font-family', Text.toTypefaces(textStyle.typeface));
        }
        if (textStyle.height.ctor !== "Nothing") {
            updateIfNeeded(css, 'font-size', textStyle.height._0 + 'px');
        }
        updateIfNeeded(css, 'font-weight', textStyle.bold ? 'bold' : 'normal');
        updateIfNeeded(css, 'font-style', textStyle.italic ? 'italic' : 'normal');
        if (textStyle.line.ctor !== 'Nothing') {
            updateIfNeeded(css, 'text-decoration', Text.toLine(textStyle.line._0));
        }
    }

    function renderField(model) {
        var field = NativeElement.createNode('input');
        updateFieldStyle(field.style, model.style);
        field.style.borderStyle = 'solid';
        field.style.pointerEvents = 'auto';

        field.type = model.type;
        field.placeholder = model.placeHolder;
        field.value = model.content.string;

        field.elm_handler = model.handler;
        field.elm_old_value = field.value;

        function inputUpdate(event) {
            var curr = field.elm_old_value;
            var next = field.value;
            if (curr === next) {
                return;
            }

            var direction = field.selectionDirection === 'forward' ? 'Forward' : 'Backward';
            var start = field.selectionStart;
            var end = field.selectionEnd;
            field.value = field.elm_old_value;

            field.elm_handler({
                _:{},
                string: next,
                selection: {
                    _:{},
                    start: start,
                    end: end,
                    direction: { ctor: direction }
                }
            })();
        }

        field.addEventListener('input', inputUpdate);
        field.addEventListener('focus', function() {
            field.elm_hasFocus = true;
        });
        field.addEventListener('blur', function() {
            field.elm_hasFocus = false;
        });

        return field;
    }

    function updateField(field, oldModel, newModel) {
        if (oldModel.style !== newModel.style) {
            updateFieldStyle(field.style, newModel.style);
        }
        field.elm_handler = newModel.handler;

        field.type = newModel.type;
        field.placeholder = newModel.placeHolder;
        var value = newModel.content.string;
        field.value = value;
        field.elm_old_value = value;
        if (field.elm_hasFocus) {
            var selection = newModel.content.selection;
            var direction = selection.direction.ctor === 'Forward' ? 'forward' : 'backward';
            setRange(field, selection.start, selection.end, direction);
        }
        return field;
    }

    function mkField(type) {
        function field(style, handler, placeHolder, content) {
            var padding = style.padding;
            var outline = style.outline.width;
            var adjustWidth = padding.left + padding.right + outline.left + outline.right;
            var adjustHeight = padding.top + padding.bottom + outline.top + outline.bottom;
            return A3(Element.newElement, 200, 30, {
                ctor: 'Custom',
                type: type + 'Field',
                adjustWidth: adjustWidth,
                adjustHeight: adjustHeight,
                render: renderField,
                update: updateField,
                model: {
                    handler:handler,
                    placeHolder:placeHolder,
                    content:content,
                    style:style,
                    type:type
                }
            });
        }
        return F4(field);
    }

    function hoverable(handler, elem) {
        function onHover(bool) {
            handler(bool)();
        }
        var props = Utils.replace([['hover',onHover]], elem.props);
        return { props:props, element:elem.element };
    }

    function clickable(message, elem) {
        function onClick() {
            message();
        }
        var props = Utils.replace([['click',onClick]], elem.props);
        return { props:props, element:elem.element };
    }

    return Elm.Native.Graphics.Input.values = {
        button: F2(button),
        customButton: F4(customButton),
        checkbox: F2(checkbox),
        dropDown: F2(dropDown),
        field: mkField('text'),
        email: mkField('email'),
        password: mkField('password'),
        hoverable: F2(hoverable),
        clickable: F2(clickable)
    };

};

Elm.Native.Json = {};
Elm.Native.Json.make = function(localRuntime) {
    localRuntime.Native = localRuntime.Native || {};
    localRuntime.Native.Json = localRuntime.Native.Json || {};
    if (localRuntime.Native.Json.values) {
        return localRuntime.Native.Json.values;
    }

    var ElmArray = Elm.Native.Array.make(localRuntime);
    var List = Elm.Native.List.make(localRuntime);
    var Maybe = Elm.Maybe.make(localRuntime);
    var Result = Elm.Result.make(localRuntime);
    var Utils = Elm.Native.Utils.make(localRuntime);


    function crash(expected, actual) {
        throw new Error(
            'expecting ' + expected + ' but got ' + JSON.stringify(actual)
        );
    }


    // PRIMITIVE VALUES

    function decodeNull(successValue) {
        return function(value) {
            if (value === null) {
                return successValue;
            }
            crash('null', value);
        };
    }


    function decodeString(value) {
        if (typeof value === 'string' || value instanceof String) {
            return value;
        }
        crash('a String', value);
    }


    function decodeFloat(value) {
        if (typeof value === 'number') {
            return value;
        }
        crash('a Float', value);
    }


    function decodeInt(value) {
        if (typeof value === 'number' && (value|0) === value) {
            return value;
        }
        crash('an Int', value);
    }


    function decodeBool(value) {
        if (typeof value === 'boolean') {
            return value;
        }
        crash('a Bool', value);
    }


    // ARRAY

    function decodeArray(decoder) {
        return function(value) {
            if (value instanceof Array) {
                var len = value.length;
                var array = new Array(len);
                for (var i = len; i-- ; ) {
                    array[i] = decoder(value[i]);
                }
                return ElmArray.fromJSArray(array);
            }
            crash('an Array', value);
        };
    }


    // LIST

    function decodeList(decoder) {
        return function(value) {
            if (value instanceof Array) {
                var len = value.length;
                var list = List.Nil;
                for (var i = len; i-- ; ) {
                    list = List.Cons( decoder(value[i]), list );
                }
                return list;
            }
            crash('a List', value);
        };
    }


    // MAYBE

    function decodeMaybe(decoder) {
        return function(value) {
            try {
                return Maybe.Just(decoder(value));
            } catch(e) {
                return Maybe.Nothing;
            }
        };
    }


    // FIELDS

    function decodeField(field, decoder) {
        return function(value) {
            var subValue = value[field];
            if (subValue !== undefined) {
                return decoder(subValue);
            }
            crash("an object with field '" + field + "'", value);
        };
    }


    // OBJECTS

    function decodeKeyValuePairs(decoder) {
        return function(value) {
            var isObject =
                typeof value === 'object'
                    && value !== null
                    && !(value instanceof Array);

            if (isObject) {
                var keyValuePairs = List.Nil;
                for (var key in value) {
                    var elmValue = decoder(value[key]);
                    var pair = Utils.Tuple2(key, elmValue);
                    keyValuePairs = List.Cons(pair, keyValuePairs);
                }
                return keyValuePairs;
            }

            crash("an object", value);
        };
    }

    function decodeObject1(f, d1) {
        return function(value) {
            return f(d1(value));
        };
    }

    function decodeObject2(f, d1, d2) {
        return function(value) {
            return A2( f, d1(value), d2(value) );
        };
    }

    function decodeObject3(f, d1, d2, d3) {
        return function(value) {
            return A3( f, d1(value), d2(value), d3(value) );
        };
    }

    function decodeObject4(f, d1, d2, d3, d4) {
        return function(value) {
            return A4( f, d1(value), d2(value), d3(value), d4(value) );
        };
    }

    function decodeObject5(f, d1, d2, d3, d4, d5) {
        return function(value) {
            return A5( f, d1(value), d2(value), d3(value), d4(value), d5(value) );
        };
    }

    function decodeObject6(f, d1, d2, d3, d4, d5, d6) {
        return function(value) {
            return A6( f,
                d1(value),
                d2(value),
                d3(value),
                d4(value),
                d5(value),
                d6(value)
            );
        };
    }

    function decodeObject7(f, d1, d2, d3, d4, d5, d6, d7) {
        return function(value) {
            return A7( f,
                d1(value),
                d2(value),
                d3(value),
                d4(value),
                d5(value),
                d6(value),
                d7(value)
            );
        };
    }

    function decodeObject8(f, d1, d2, d3, d4, d5, d6, d7, d8) {
        return function(value) {
            return A8( f,
                d1(value),
                d2(value),
                d3(value),
                d4(value),
                d5(value),
                d6(value),
                d7(value),
                d8(value)
            );
        };
    }


    // TUPLES

    function decodeTuple1(f, d1) {
        return function(value) {
            if ( !(value instanceof Array) || value.length !== 1 ) {
                crash('a Tuple of length 1', value);
            }
            return f( d1(value[0]) );
        };
    }

    function decodeTuple2(f, d1, d2) {
        return function(value) {
            if ( !(value instanceof Array) || value.length !== 2 ) {
                crash('a Tuple of length 2', value);
            }
            return A2( f, d1(value[0]), d2(value[1]) );
        };
    }

    function decodeTuple3(f, d1, d2, d3) {
        return function(value) {
            if ( !(value instanceof Array) || value.length !== 3 ) {
                crash('a Tuple of length 3', value);
            }
            return A3( f, d1(value[0]), d2(value[1]), d3(value[2]) );
        };
    }


    function decodeTuple4(f, d1, d2, d3, d4) {
        return function(value) {
            if ( !(value instanceof Array) || value.length !== 4 ) {
                crash('a Tuple of length 4', value);
            }
            return A4( f, d1(value[0]), d2(value[1]), d3(value[2]), d4(value[3]) );
        };
    }


    function decodeTuple5(f, d1, d2, d3, d4, d5) {
        return function(value) {
            if ( !(value instanceof Array) || value.length !== 5 ) {
                crash('a Tuple of length 5', value);
            }
            return A5( f,
                d1(value[0]),
                d2(value[1]),
                d3(value[2]),
                d4(value[3]),
                d5(value[4])
            );
        };
    }


    function decodeTuple6(f, d1, d2, d3, d4, d5, d6) {
        return function(value) {
            if ( !(value instanceof Array) || value.length !== 6 ) {
                crash('a Tuple of length 6', value);
            }
            return A6( f,
                d1(value[0]),
                d2(value[1]),
                d3(value[2]),
                d4(value[3]),
                d5(value[4]),
                d6(value[5])
            );
        };
    }

    function decodeTuple7(f, d1, d2, d3, d4, d5, d6, d7) {
        return function(value) {
            if ( !(value instanceof Array) || value.length !== 7 ) {
                crash('a Tuple of length 7', value);
            }
            return A7( f,
                d1(value[0]),
                d2(value[1]),
                d3(value[2]),
                d4(value[3]),
                d5(value[4]),
                d6(value[5]),
                d7(value[6])
            );
        };
    }


    function decodeTuple8(f, d1, d2, d3, d4, d5, d6, d7, d8) {
        return function(value) {
            if ( !(value instanceof Array) || value.length !== 8 ) {
                crash('a Tuple of length 8', value);
            }
            return A8( f,
                d1(value[0]),
                d2(value[1]),
                d3(value[2]),
                d4(value[3]),
                d5(value[4]),
                d6(value[5]),
                d7(value[6]),
                d8(value[7])
            );
        };
    }


    // CUSTOM DECODERS

    function decodeValue(value) {
        return value;
    }

    function runDecoderValue(decoder, value) {
        try {
            return Result.Ok(decoder(value));
        } catch(e) {
            return Result.Err(e.message);
        }
    }

    function customDecoder(decoder, callback) {
        return function(value) {
            var result = callback(decoder(value));
            if (result.ctor === 'Err') {
                throw new Error('custom decoder failed: ' + result._0);
            }
            return result._0;
        }
    }

    function andThen(decode, callback) {
        return function(value) {
            var result = decode(value);
            return callback(result)(value);
        }
    }

    function fail(msg) {
        return function(value) {
            throw new Error(msg);
        }
    }

    function succeed(successValue) {
        return function(value) {
            return successValue;
        }
    }


    // ONE OF MANY

    function oneOf(decoders) {
        return function(value) {
            var errors = [];
            var temp = decoders;
            while (temp.ctor !== '[]') {
                try {
                    return temp._0(value);
                } catch(e) {
                    errors.push(e.message);
                }
                temp = temp._1;
            }
            throw new Error('expecting one of the following:\n    ' + errors.join('\n    '));
        }
    }

    function get(decoder, value) {
        try {
            return Result.Ok(decoder(value));
        } catch(e) {
            return Result.Err(e.message);
        }
    }


    // ENCODE / DECODE

    function runDecoderString(decoder, string) {
        try {
            return Result.Ok(decoder(JSON.parse(string)));
        } catch(e) {
            return Result.Err(e.message);
        }
    }

    function encode(indentLevel, value) {
        return JSON.stringify(value, null, indentLevel);
    }

    function identity(value) {
        return value;
    }

    function encodeObject(keyValuePairs) {
        var obj = {};
        while (keyValuePairs.ctor !== '[]') {
            var pair = keyValuePairs._0;
            obj[pair._0] = pair._1;
            keyValuePairs = keyValuePairs._1;
        }
        return obj;
    }

    return localRuntime.Native.Json.values = {
        encode: F2(encode),
        runDecoderString: F2(runDecoderString),
        runDecoderValue: F2(runDecoderValue),

        get: F2(get),
        oneOf: oneOf,

        decodeNull: decodeNull,
        decodeInt: decodeInt,
        decodeFloat: decodeFloat,
        decodeString: decodeString,
        decodeBool: decodeBool,

        decodeMaybe: decodeMaybe,

        decodeList: decodeList,
        decodeArray: decodeArray,

        decodeField: F2(decodeField),

        decodeObject1: F2(decodeObject1),
        decodeObject2: F3(decodeObject2),
        decodeObject3: F4(decodeObject3),
        decodeObject4: F5(decodeObject4),
        decodeObject5: F6(decodeObject5),
        decodeObject6: F7(decodeObject6),
        decodeObject7: F8(decodeObject7),
        decodeObject8: F9(decodeObject8),
        decodeKeyValuePairs: decodeKeyValuePairs,

        decodeTuple1: F2(decodeTuple1),
        decodeTuple2: F3(decodeTuple2),
        decodeTuple3: F4(decodeTuple3),
        decodeTuple4: F5(decodeTuple4),
        decodeTuple5: F6(decodeTuple5),
        decodeTuple6: F7(decodeTuple6),
        decodeTuple7: F8(decodeTuple7),
        decodeTuple8: F9(decodeTuple8),

        andThen: F2(andThen),
        decodeValue: decodeValue,
        customDecoder: F2(customDecoder),
        fail: fail,
        succeed: succeed,

        identity: identity,
        encodeNull: null,
        encodeArray: ElmArray.toJSArray,
        encodeList: List.toArray,
        encodeObject: encodeObject

    };

};

Elm.Native.List = {};
Elm.Native.List.make = function(elm) {
    elm.Native = elm.Native || {};
    elm.Native.List = elm.Native.List || {};
    if (elm.Native.List.values) return elm.Native.List.values;
    if ('values' in Elm.Native.List)
        return elm.Native.List.values = Elm.Native.List.values;

    var Utils = Elm.Native.Utils.make(elm);

    var Nil = Utils.Nil;
    var Cons = Utils.Cons;

    function throwError(f) {
        throw new Error("Function '" + f + "' expects a non-empty list!");
    }

    function toArray(xs) {
        var out = [];
        while (xs.ctor !== '[]') {
            out.push(xs._0);
            xs = xs._1;
        }
        return out;
    }

    function fromArray(arr) {
        var out = Nil;
        for (var i = arr.length; i--; ) {
            out = Cons(arr[i], out);
        }
        return out;
    }

    function range(lo,hi) {
        var lst = Nil;
        if (lo <= hi) {
            do { lst = Cons(hi,lst) } while (hi-->lo);
        }
        return lst
    }

    function head(v) {
        return v.ctor === '[]' ? throwError('head') : v._0;
    }
    function tail(v) {
        return v.ctor === '[]' ? throwError('tail') : v._1;
    }

    function map(f, xs) {
        var arr = [];
        while (xs.ctor !== '[]') {
            arr.push(f(xs._0));
            xs = xs._1;
        }
        return fromArray(arr);
    }

    // f defined similarly for both foldl and foldr (NB: different from Haskell)
    // ie, foldl : (a -> b -> b) -> b -> [a] -> b
    function foldl(f, b, xs) {
        var acc = b;
        while (xs.ctor !== '[]') {
            acc = A2(f, xs._0, acc);
            xs = xs._1;
        }
        return acc;
    }

    function foldr(f, b, xs) {
        var arr = toArray(xs);
        var acc = b;
        for (var i = arr.length; i--; ) {
            acc = A2(f, arr[i], acc);
        }
        return acc;
    }

    function foldl1(f, xs) {
        return xs.ctor === '[]' ? throwError('foldl1') : foldl(f, xs._0, xs._1);
    }

    function foldr1(f, xs) {
        if (xs.ctor === '[]') { throwError('foldr1'); }
        var arr = toArray(xs);
        var acc = arr.pop();
        for (var i = arr.length; i--; ) {
            acc = A2(f, arr[i], acc);
        }
        return acc;
    }

    function scanl(f, b, xs) {
        var arr = toArray(xs);
        arr.unshift(b);
        var len = arr.length;
        for (var i = 1; i < len; ++i) {
            arr[i] = A2(f, arr[i], arr[i-1]);
        }
        return fromArray(arr);
    }

    function scanl1(f, xs) {
        return xs.ctor === '[]' ? throwError('scanl1') : scanl(f, xs._0, xs._1);
    }

    function filter(pred, xs) {
        var arr = [];
        while (xs.ctor !== '[]') {
            if (pred(xs._0)) { arr.push(xs._0); }
            xs = xs._1;
        }
        return fromArray(arr);
    }

    function length(xs) {
        var out = 0;
        while (xs.ctor !== '[]') {
            out += 1;
            xs = xs._1;
        }
        return out;
    }

    function member(x, xs) {
        while (xs.ctor !== '[]') {
            if (Utils.eq(x,xs._0)) return true;
            xs = xs._1;
        }
        return false;
    }

    function append(xs, ys) {
        if (xs.ctor === '[]') {
            return ys;
        }
        var root = Cons(xs._0, Nil);
        var curr = root;
        xs = xs._1;
        while (xs.ctor !== '[]') {
            curr._1 = Cons(xs._0, Nil);
            xs = xs._1;
            curr = curr._1;
        }
        curr._1 = ys;
        return root;
    }

    function all(pred, xs) {
        while (xs.ctor !== '[]') {
            if (!pred(xs._0)) return false;
            xs = xs._1;
        }
        return true;
    }

    function any(pred, xs) {
        while (xs.ctor !== '[]') {
            if (pred(xs._0)) return true;
            xs = xs._1;
        }
        return false;
    }

    function map2(f, xs, ys) {
        var arr = [];
        while (xs.ctor !== '[]' && ys.ctor !== '[]') {
            arr.push(A2(f, xs._0, ys._0));
            xs = xs._1;
            ys = ys._1;
        }
        return fromArray(arr);
    }

    function map3(f, xs, ys, zs) {
        var arr = [];
        while (xs.ctor !== '[]' && ys.ctor !== '[]' && zs.ctor !== '[]') {
            arr.push(A3(f, xs._0, ys._0, zs._0));
            xs = xs._1;
            ys = ys._1;
            zs = zs._1;
        }
        return fromArray(arr);
    }

    function map4(f, ws, xs, ys, zs) {
        var arr = [];
        while (   ws.ctor !== '[]'
               && xs.ctor !== '[]'
               && ys.ctor !== '[]'
               && zs.ctor !== '[]')
        {
            arr.push(A4(f, ws._0, xs._0, ys._0, zs._0));
            ws = ws._1;
            xs = xs._1;
            ys = ys._1;
            zs = zs._1;
        }
        return fromArray(arr);
    }

    function map5(f, vs, ws, xs, ys, zs) {
        var arr = [];
        while (   vs.ctor !== '[]'
               && ws.ctor !== '[]'
               && xs.ctor !== '[]'
               && ys.ctor !== '[]'
               && zs.ctor !== '[]')
        {
            arr.push(A5(f, vs._0, ws._0, xs._0, ys._0, zs._0));
            vs = vs._1;
            ws = ws._1;
            xs = xs._1;
            ys = ys._1;
            zs = zs._1;
        }
        return fromArray(arr);
    }

    function sort(xs) {
        return fromArray(toArray(xs).sort(Utils.cmp));
    }

    function sortBy(f, xs) {
        return fromArray(toArray(xs).sort(function(a,b){
            return Utils.cmp(f(a), f(b));
        }));
    }

    function sortWith(f, xs) {
        return fromArray(toArray(xs).sort(function(a,b){
            var ord = f(a)(b).ctor;
            return ord === 'EQ' ? 0 : ord === 'LT' ? -1 : 1;
        }));
    }

    function take(n, xs) {
        var arr = [];
        while (xs.ctor !== '[]' && n > 0) {
            arr.push(xs._0);
            xs = xs._1;
            --n;
        }
        return fromArray(arr);
    }

    function drop(n, xs) {
        while (xs.ctor !== '[]' && n > 0) {
            xs = xs._1;
            --n;
        }
        return xs;
    }

    function repeat(n, x) {
        var arr = [];
        var pattern = [x];
        while (n > 0) {
            if (n & 1) arr = arr.concat(pattern);
            n >>= 1, pattern = pattern.concat(pattern);
        }
        return fromArray(arr);
    }


    Elm.Native.List.values = {
        Nil:Nil,
        Cons:Cons,
        cons:F2(Cons),
        toArray:toArray,
        fromArray:fromArray,
        range:range,
        append: F2(append),

        head:head,
        tail:tail,

        map:F2(map),
        foldl:F3(foldl),
        foldr:F3(foldr),

        foldl1:F2(foldl1),
        foldr1:F2(foldr1),
        scanl:F3(scanl),
        scanl1:F2(scanl1),
        filter:F2(filter),
        length:length,
        member:F2(member),

        all:F2(all),
        any:F2(any),
        map2:F3(map2),
        map3:F4(map3),
        map4:F5(map4),
        map5:F6(map5),
        sort:sort,
        sortBy:F2(sortBy),
        sortWith:F2(sortWith),
        take:F2(take),
        drop:F2(drop),
        repeat:F2(repeat)
    };
    return elm.Native.List.values = Elm.Native.List.values;

};

Elm.Native = Elm.Native || {};
Elm.Native.Mouse = {};
Elm.Native.Mouse.make = function(localRuntime) {

    localRuntime.Native = localRuntime.Native || {};
    localRuntime.Native.Mouse = localRuntime.Native.Mouse || {};
    if (localRuntime.Native.Mouse.values) {
        return localRuntime.Native.Mouse.values;
    }

    var Signal = Elm.Signal.make(localRuntime);
    var Utils = Elm.Native.Utils.make(localRuntime);

    var position = Signal.constant(Utils.Tuple2(0,0));
    position.defaultNumberOfKids = 2;

    // do not move x and y into Elm. By setting their default number
    // of kids, it is possible to detatch the mouse listeners if
    // they are not needed.
    function fst(pair) {
        return pair._0;
    }
    function snd(pair) {
        return pair._1;
    }

    var x = A2( Signal.map, fst, position );
    x.defaultNumberOfKids = 0;

    var y = A2( Signal.map, snd, position );
    y.defaultNumberOfKids = 0;

    var isDown = Signal.constant(false);
    var clicks = Signal.constant(Utils.Tuple0);

    var node = localRuntime.isFullscreen()
        ? document
        : localRuntime.node;

    localRuntime.addListener([clicks.id], node, 'click', function click() {
        localRuntime.notify(clicks.id, Utils.Tuple0);
    });
    localRuntime.addListener([isDown.id], node, 'mousedown', function down() {
        localRuntime.notify(isDown.id, true);
    });
    localRuntime.addListener([isDown.id], node, 'mouseup', function up() {
        localRuntime.notify(isDown.id, false);
    });
    localRuntime.addListener([position.id], node, 'mousemove', function move(e) {
        localRuntime.notify(position.id, Utils.getXY(e));
    });

    return localRuntime.Native.Mouse.values = {
        position: position,
        x: x,
        y: y,
        isDown: isDown,
        clicks: clicks
    };
};

Elm.Native.Ports = {};
Elm.Native.Ports.make = function(localRuntime) {
    localRuntime.Native = localRuntime.Native || {};
    localRuntime.Native.Ports = localRuntime.Native.Ports || {};
    if (localRuntime.Native.Ports.values) {
        return localRuntime.Native.Ports.values;
    }

    var Signal;

    function incomingSignal(converter) {
        converter.isSignal = true;
        return converter;
    }

    function outgoingSignal(converter) {
        if (!Signal) {
            Signal = Elm.Signal.make(localRuntime);
        }
        return function(signal) {
            var subscribers = []
            function subscribe(handler) {
                subscribers.push(handler);
            }
            function unsubscribe(handler) {
                subscribers.pop(subscribers.indexOf(handler));
            }
            A2( Signal.map, function(value) {
                var val = converter(value);
                var len = subscribers.length;
                for (var i = 0; i < len; ++i) {
                    subscribers[i](val);
                }
            }, signal);
            return { subscribe:subscribe, unsubscribe:unsubscribe };
        }
    }

    function portIn(name, converter) {
        var jsValue = localRuntime.ports.incoming[name];
        if (jsValue === undefined) {
            throw new Error("Initialization Error: port '" + name +
                            "' was not given an input!");
        }
        localRuntime.ports.uses[name] += 1;
        try {
            var elmValue = converter(jsValue);
        } catch(e) {
            throw new Error("Initialization Error on port '" + name + "': \n" + e.message);
        }

        // just return a static value if it is not a signal
        if (!converter.isSignal) {
            return elmValue;
        }

        // create a signal if necessary
        if (!Signal) {
            Signal = Elm.Signal.make(localRuntime);
        }
        var signal = Signal.constant(elmValue);
        function send(jsValue) {
            try {
                var elmValue = converter(jsValue);
            } catch(e) {
                throw new Error("Error sending to port '" + name + "': \n" + e.message);
            }
            setTimeout(function() {
                localRuntime.notify(signal.id, elmValue);
            }, 0);
        }
        localRuntime.ports.outgoing[name] = { send:send };
        return signal;
    }

    function portOut(name, converter, value) {
        try {
            localRuntime.ports.outgoing[name] = converter(value);
        } catch(e) {
            throw new Error("Initialization Error on port '" + name + "': \n" + e.message);
        }
        return value;
    }

    return localRuntime.Native.Ports.values = {
        incomingSignal: incomingSignal,
        outgoingSignal: outgoingSignal,
        portOut: portOut,
        portIn: portIn
    };
};



if (!Elm.fullscreen) {

    (function() {
        'use strict';

        var Display = { FULLSCREEN: 0, COMPONENT: 1, NONE: 2 };

        Elm.fullscreen = function(module, ports) {
            var container = document.createElement('div');
            document.body.appendChild(container);
            return init(Display.FULLSCREEN, container, module, ports || {});
        };

        Elm.embed = function(module, container, ports) {
            var tag = container.tagName;
            if (tag !== 'DIV') {
                throw new Error('Elm.node must be given a DIV, not a ' + tag + '.');
            }
            return init(Display.COMPONENT, container, module, ports || {});
        };

        Elm.worker = function(module, ports) {
            return init(Display.NONE, {}, module, ports || {});
        };

        function init(display, container, module, ports, moduleToReplace) {
            // defining state needed for an instance of the Elm RTS
            var inputs = [];

            /* OFFSET
             * Elm's time traveling debugger lets you pause time. This means
             * "now" may be shifted a bit into the past. By wrapping Date.now()
             * we can manage this.
             */
            var timer = {
                now: function() {
                    return Date.now();
                }
            };

            var updateInProgress = false;
            function notify(id, v) {
                if (updateInProgress) {
                    throw new Error(
                        'The notify function has been called synchronously!\n' +
                        'This can lead to frames being dropped.\n' +
                        'Definitely report this to <https://github.com/elm-lang/Elm/issues>\n');
                }
                updateInProgress = true;
                var timestep = timer.now();
                for (var i = inputs.length; i--; ) {
                    inputs[i].recv(timestep, id, v);
                }
                updateInProgress = false;
            }
            function setTimeout(func, delay) {
                window.setTimeout(func, delay);
            }

            var listeners = [];
            function addListener(relevantInputs, domNode, eventName, func) {
                domNode.addEventListener(eventName, func);
                var listener = {
                    relevantInputs: relevantInputs,
                    domNode: domNode,
                    eventName: eventName,
                    func: func
                };
                listeners.push(listener);
            }

            var portUses = {}
            for (var key in ports) {
                portUses[key] = 0;
            }
            // create the actual RTS. Any impure modules will attach themselves to this
            // object. This permits many Elm programs to be embedded per document.
            var elm = {
                notify: notify,
                setTimeout: setTimeout,
                node: container,
                addListener: addListener,
                inputs: inputs,
                timer: timer,
                ports: { incoming:ports, outgoing:{}, uses:portUses },

                isFullscreen: function() { return display === Display.FULLSCREEN; },
                isEmbed: function() { return display === Display.COMPONENT; },
                isWorker: function() { return display === Display.NONE; }
            };

            function swap(newModule) {
                removeListeners(listeners);
                var div = document.createElement('div');
                var newElm = init(display, div, newModule, ports, elm);
                inputs = [];
                // elm.swap = newElm.swap;
                return newElm;
            }

            function dispose() {
                removeListeners(listeners);
                inputs = [];
            }

            var Module = {};
            try {
                Module = module.make(elm);
                checkPorts(elm);
            }
            catch (error) {
                if (typeof container.appendChild == 'undefined') {
                    console.log(error.message);
                } else {
                    container.appendChild(errorNode(error.message));
                }
                throw error;
            }
            inputs = filterDeadInputs(inputs);
            filterListeners(inputs, listeners);
            addReceivers(elm.ports.outgoing);
            if (display !== Display.NONE) {
                var graphicsNode = initGraphics(elm, Module);
            }
            if (typeof moduleToReplace !== 'undefined') {
                hotSwap(moduleToReplace, elm);

                // rerender scene if graphics are enabled.
                if (typeof graphicsNode !== 'undefined') {
                    graphicsNode.recv(0, true, 0);
                }
            }

            return {
                swap:swap,
                ports:elm.ports.outgoing,
                dispose:dispose
            };
        };

        function checkPorts(elm) {
            var portUses = elm.ports.uses;
            for (var key in portUses) {
                var uses = portUses[key]
                if (uses === 0) {
                    throw new Error(
                        "Initialization Error: provided port '" + key +
                        "' to a module that does not take it as in input.\n" +
                        "Remove '" + key + "' from the module initialization code.");
                } else if (uses > 1) {
                    throw new Error(
                        "Initialization Error: port '" + key +
                        "' has been declared multiple times in the Elm code.\n" +
                        "Remove declarations until there is exactly one.");
                }
            }
        }

        function errorNode(message) {
            var code = document.createElement('code');

            var lines = message.split('\n');
            code.appendChild(document.createTextNode(lines[0]));
            code.appendChild(document.createElement('br'));
            code.appendChild(document.createElement('br'));
            for (var i = 1; i < lines.length; ++i) {
                code.appendChild(document.createTextNode('\u00A0 \u00A0 ' + lines[i]));
                code.appendChild(document.createElement('br'));
            }
            code.appendChild(document.createElement('br'));
            code.appendChild(document.createTextNode("Open the developer console for more details."));
            return code;
        }


        //// FILTER SIGNALS ////

        // TODO: move this code into the signal module and create a function
        // Signal.initializeGraph that actually instantiates everything.

        function filterListeners(inputs, listeners) {
            loop:
            for (var i = listeners.length; i--; ) {
                var listener = listeners[i];
                for (var j = inputs.length; j--; ) {
                    if (listener.relevantInputs.indexOf(inputs[j].id) >= 0) {
                        continue loop;
                    }
                }
                listener.domNode.removeEventListener(listener.eventName, listener.func);
            }
        }

        function removeListeners(listeners) {
            for (var i = listeners.length; i--; ) {
                var listener = listeners[i];
                listener.domNode.removeEventListener(listener.eventName, listener.func);
            }
        }

        // add receivers for built-in ports if they are defined
        function addReceivers(ports) {
            if ('log' in ports) {
                ports.log.subscribe(function(v) { console.log(v) });
            }
            if ('stdout' in ports) {
                var process = process || {};
                var handler = process.stdout
                    ? function(v) { process.stdout.write(v); }
                    : function(v) { console.log(v); };
                ports.stdout.subscribe(handler);
            }
            if ('stderr' in ports) {
                var process = process || {};
                var handler = process.stderr
                    ? function(v) { process.stderr.write(v); }
                    : function(v) { console.log('Error:' + v); };
                ports.stderr.subscribe(handler);
            }
            if ('title' in ports) {
                if (typeof ports.title === 'string') {
                    document.title = ports.title;
                } else {
                    ports.title.subscribe(function(v) { document.title = v; });
                }
            }
            if ('redirect' in ports) {
                ports.redirect.subscribe(function(v) {
                    if (v.length > 0) window.location = v;
                });
            }
            if ('favicon' in ports) {
                if (typeof ports.favicon === 'string') {
                    changeFavicon(ports.favicon);
                } else {
                    ports.favicon.subscribe(changeFavicon);
                }
            }
            function changeFavicon(src) {
                var link = document.createElement('link');
                var oldLink = document.getElementById('elm-favicon');
                link.id = 'elm-favicon';
                link.rel = 'shortcut icon';
                link.href = src;
                if (oldLink) {
                    document.head.removeChild(oldLink);
                }
                document.head.appendChild(link);
            }
        }


        function filterDeadInputs(inputs) {
            var temp = [];
            for (var i = inputs.length; i--; ) {
                if (isAlive(inputs[i])) temp.push(inputs[i]);
            }
            return temp;
        }


        function isAlive(input) {
            if (!('defaultNumberOfKids' in input)) return true;
            var len = input.kids.length;
            if (len === 0) return false;
            if (len > input.defaultNumberOfKids) return true;
            var alive = false;
            for (var i = len; i--; ) {
                alive = alive || isAlive(input.kids[i]);
            }
            return alive;
        }


        ////  RENDERING  ////

        function initGraphics(elm, Module) {
            if (!('main' in Module)) {
                throw new Error("'main' is missing! What do I display?!");
            }

            var signalGraph = Module.main;

            // make sure the signal graph is actually a signal & extract the visual model
            var Signal = Elm.Signal.make(elm);
            if (!('recv' in signalGraph)) {
                signalGraph = Signal.constant(signalGraph);
            }
            var initialScene = signalGraph.value;

            // Figure out what the render functions should be
            var render;
            var update;
            if (initialScene.props) {
                var Element = Elm.Native.Graphics.Element.make(elm);
                render = Element.render;
                update = Element.updateAndReplace;
            } else {
                var VirtualDom = Elm.Native.VirtualDom.make(elm);
                render = VirtualDom.render;
                update = VirtualDom.updateAndReplace;
            }

            // Add the initialScene to the DOM
            var container = elm.node;
            var node = render(initialScene);
            while (container.firstChild) {
                container.removeChild(container.firstChild);
            }
            container.appendChild(node);

            var _requestAnimationFrame =
                typeof requestAnimationFrame !== 'undefined'
                    ? requestAnimationFrame
                    : function(cb) { setTimeout(cb, 1000/60); }
                    ;

            // domUpdate is called whenever the main Signal changes.
            //
            // domUpdate and drawCallback implement a small state machine in order
            // to schedule only 1 draw per animation frame. This enforces that
            // once draw has been called, it will not be called again until the
            // next frame.
            //
            // drawCallback is scheduled whenever
            // 1. The state transitions from PENDING_REQUEST to EXTRA_REQUEST, or
            // 2. The state transitions from NO_REQUEST to PENDING_REQUEST
            //
            // Invariants:
            // 1. In the NO_REQUEST state, there is never a scheduled drawCallback.
            // 2. In the PENDING_REQUEST and EXTRA_REQUEST states, there is always exactly 1
            //    scheduled drawCallback.
            var NO_REQUEST = 0;
            var PENDING_REQUEST = 1;
            var EXTRA_REQUEST = 2;
            var state = NO_REQUEST;
            var savedScene = initialScene;
            var scheduledScene = initialScene;

            function domUpdate(newScene) {
                scheduledScene = newScene;

                switch (state) {
                    case NO_REQUEST:
                        _requestAnimationFrame(drawCallback);
                        state = PENDING_REQUEST;
                        return;
                    case PENDING_REQUEST:
                        state = PENDING_REQUEST;
                        return;
                    case EXTRA_REQUEST:
                        state = PENDING_REQUEST;
                        return;
                }
            }

            function drawCallback() {
                switch (state) {
                    case NO_REQUEST:
                        // This state should not be possible. How can there be no
                        // request, yet somehow we are actively fulfilling a
                        // request?
                        throw new Error(
                            "Unexpected draw callback.\n" +
                            "Please report this to <https://github.com/elm-lang/core/issues>."
                        );

                    case PENDING_REQUEST:
                        // At this point, we do not *know* that another frame is
                        // needed, but we make an extra request to rAF just in
                        // case. It's possible to drop a frame if rAF is called
                        // too late, so we just do it preemptively.
                        _requestAnimationFrame(drawCallback);
                        state = EXTRA_REQUEST;

                        // There's also stuff we definitely need to draw.
                        draw();
                        return;

                    case EXTRA_REQUEST:
                        // Turns out the extra request was not needed, so we will
                        // stop calling rAF. No reason to call it all the time if
                        // no one needs it.
                        state = NO_REQUEST;
                        return;
                }
            }

            function draw() {
                update(elm.node.firstChild, savedScene, scheduledScene);
                if (elm.Native.Window) {
                    elm.Native.Window.values.resizeIfNeeded();
                }
                savedScene = scheduledScene;
            }

            var renderer = A2(Signal.map, domUpdate, signalGraph);

            // must check for resize after 'renderer' is created so
            // that changes show up.
            if (elm.Native.Window) {
                elm.Native.Window.values.resizeIfNeeded();
            }

            return renderer;
        }

        //// HOT SWAPPING ////

        // Returns boolean indicating if the swap was successful.
        // Requires that the two signal graphs have exactly the same
        // structure.
        function hotSwap(from, to) {
            function similar(nodeOld,nodeNew) {
                var idOkay = nodeOld.id === nodeNew.id;
                var lengthOkay = nodeOld.kids.length === nodeNew.kids.length;
                return idOkay && lengthOkay;
            }
            function swap(nodeOld,nodeNew) {
                nodeNew.value = nodeOld.value;
                return true;
            }
            var canSwap = depthFirstTraversals(similar, from.inputs, to.inputs);
            if (canSwap) {
                depthFirstTraversals(swap, from.inputs, to.inputs);
            }
            from.node.parentNode.replaceChild(to.node, from.node);

            return canSwap;
        }

        // Returns false if the node operation f ever fails.
        function depthFirstTraversals(f, queueOld, queueNew) {
            if (queueOld.length !== queueNew.length) return false;
            queueOld = queueOld.slice(0);
            queueNew = queueNew.slice(0);

            var seen = [];
            while (queueOld.length > 0 && queueNew.length > 0) {
                var nodeOld = queueOld.pop();
                var nodeNew = queueNew.pop();
                if (seen.indexOf(nodeOld.id) < 0) {
                    if (!f(nodeOld, nodeNew)) return false;
                    queueOld = queueOld.concat(nodeOld.kids);
                    queueNew = queueNew.concat(nodeNew.kids);
                    seen.push(nodeOld.id);
                }
            }
            return true;
        }
    }());

    function F2(fun) {
        function wrapper(a) { return function(b) { return fun(a,b) } }
        wrapper.arity = 2;
        wrapper.func = fun;
        return wrapper;
    }

    function F3(fun) {
        function wrapper(a) {
            return function(b) { return function(c) { return fun(a,b,c) }}
        }
        wrapper.arity = 3;
        wrapper.func = fun;
        return wrapper;
    }

    function F4(fun) {
        function wrapper(a) { return function(b) { return function(c) {
            return function(d) { return fun(a,b,c,d) }}}
        }
        wrapper.arity = 4;
        wrapper.func = fun;
        return wrapper;
    }

    function F5(fun) {
        function wrapper(a) { return function(b) { return function(c) {
            return function(d) { return function(e) { return fun(a,b,c,d,e) }}}}
        }
        wrapper.arity = 5;
        wrapper.func = fun;
        return wrapper;
    }

    function F6(fun) {
        function wrapper(a) { return function(b) { return function(c) {
            return function(d) { return function(e) { return function(f) {
            return fun(a,b,c,d,e,f) }}}}}
        }
        wrapper.arity = 6;
        wrapper.func = fun;
        return wrapper;
    }

    function F7(fun) {
        function wrapper(a) { return function(b) { return function(c) {
            return function(d) { return function(e) { return function(f) {
            return function(g) { return fun(a,b,c,d,e,f,g) }}}}}}
        }
        wrapper.arity = 7;
        wrapper.func = fun;
        return wrapper;
    }

    function F8(fun) {
        function wrapper(a) { return function(b) { return function(c) {
            return function(d) { return function(e) { return function(f) {
            return function(g) { return function(h) {
            return fun(a,b,c,d,e,f,g,h)}}}}}}}
        }
        wrapper.arity = 8;
        wrapper.func = fun;
        return wrapper;
    }

    function F9(fun) {
        function wrapper(a) { return function(b) { return function(c) {
            return function(d) { return function(e) { return function(f) {
            return function(g) { return function(h) { return function(i) {
            return fun(a,b,c,d,e,f,g,h,i) }}}}}}}}
        }
        wrapper.arity = 9;
        wrapper.func = fun;
        return wrapper;
    }

    function A2(fun,a,b) {
        return fun.arity === 2
            ? fun.func(a,b)
            : fun(a)(b);
    }
    function A3(fun,a,b,c) {
        return fun.arity === 3
            ? fun.func(a,b,c)
            : fun(a)(b)(c);
    }
    function A4(fun,a,b,c,d) {
        return fun.arity === 4
            ? fun.func(a,b,c,d)
            : fun(a)(b)(c)(d);
    }
    function A5(fun,a,b,c,d,e) {
        return fun.arity === 5
            ? fun.func(a,b,c,d,e)
            : fun(a)(b)(c)(d)(e);
    }
    function A6(fun,a,b,c,d,e,f) {
        return fun.arity === 6
            ? fun.func(a,b,c,d,e,f)
            : fun(a)(b)(c)(d)(e)(f);
    }
    function A7(fun,a,b,c,d,e,f,g) {
        return fun.arity === 7
            ? fun.func(a,b,c,d,e,f,g)
            : fun(a)(b)(c)(d)(e)(f)(g);
    }
    function A8(fun,a,b,c,d,e,f,g,h) {
        return fun.arity === 8
            ? fun.func(a,b,c,d,e,f,g,h)
            : fun(a)(b)(c)(d)(e)(f)(g)(h);
    }
    function A9(fun,a,b,c,d,e,f,g,h,i) {
        return fun.arity === 9
            ? fun.func(a,b,c,d,e,f,g,h,i)
            : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
    }
}
Elm.Native.Show = {};
Elm.Native.Show.make = function(elm) {
    elm.Native = elm.Native || {};
    elm.Native.Show = elm.Native.Show || {};
    if (elm.Native.Show.values)
    {
        return elm.Native.Show.values;
    }

    var _Array;
    var Dict;
    var List;
    var Utils = Elm.Native.Utils.make(elm);

    var toString = function(v) {
        var type = typeof v;
        if (type === "function") {
            var name = v.func ? v.func.name : v.name;
            return '<function' + (name === '' ? '' : ': ') + name + '>';
        }
        else if (type === "boolean") {
            return v ? "True" : "False";
        }
        else if (type === "number") {
            return v + "";
        }
        else if ((v instanceof String) && v.isChar) {
            return "'" + addSlashes(v, true) + "'";
        }
        else if (type === "string") {
            return '"' + addSlashes(v, false) + '"';
        }
        else if (type === "object" && '_' in v && probablyPublic(v)) {
            var output = [];
            for (var k in v._) {
                for (var i = v._[k].length; i--; ) {
                    output.push(k + " = " + toString(v._[k][i]));
                }
            }
            for (var k in v) {
                if (k === '_') continue;
                output.push(k + " = " + toString(v[k]));
            }
            if (output.length === 0) {
                return "{}";
            }
            return "{ " + output.join(", ") + " }";
        }
        else if (type === "object" && 'ctor' in v) {
            if (v.ctor.substring(0,6) === "_Tuple") {
                var output = [];
                for (var k in v) {
                    if (k === 'ctor') continue;
                    output.push(toString(v[k]));
                }
                return "(" + output.join(",") + ")";
            }
            else if (v.ctor === "_Array") {
                if (!_Array) {
                    _Array = Elm.Array.make(elm);
                }
                var list = _Array.toList(v);
                return "Array.fromList " + toString(list);
            }
            else if (v.ctor === "::") {
                var output = '[' + toString(v._0);
                v = v._1;
                while (v.ctor === "::") {
                    output += "," + toString(v._0);
                    v = v._1;
                }
                return output + ']';
            }
            else if (v.ctor === "[]") {
                return "[]";
            }
            else if (v.ctor === "RBNode" || v.ctor === "RBEmpty") {
                if (!Dict) {
                    Dict = Elm.Dict.make(elm);
                }
                if (!List) {
                    List = Elm.List.make(elm);
                }
                var list = Dict.toList(v);
                var name = "Dict";
                if (list.ctor === "::" && list._0._1.ctor === "_Tuple0") {
                    name = "Set";
                    list = A2(List.map, function(x){return x._0}, list);
                }
                return name + ".fromList " + toString(list);
            }
            else {
                var output = "";
                for (var i in v) {
                    if (i === 'ctor') continue;
                    var str = toString(v[i]);
                    var parenless = str[0] === '{' || str[0] === '<' || str.indexOf(' ') < 0;
                    output += ' ' + (parenless ? str : '(' + str + ')');
                }
                return v.ctor + output;
            }
        }
        if (type === 'object' && 'recv' in v) {
            return '<signal>';
        }
        return "<internal structure>";
    };

    function addSlashes(str, isChar) {
        var s = str.replace(/\\/g, '\\\\')
                  .replace(/\n/g, '\\n')
                  .replace(/\t/g, '\\t')
                  .replace(/\r/g, '\\r')
                  .replace(/\v/g, '\\v')
                  .replace(/\0/g, '\\0');
        if (isChar) {
            return s.replace(/\'/g, "\\'")
        } else {
            return s.replace(/\"/g, '\\"');
        }
    }

    function probablyPublic(v) {
        var keys = Object.keys(v);
        var len = keys.length;
        if (len === 3
            && 'props' in v
            && 'element' in v)
        {
            return false;
        }
        else if (len === 5
            && 'horizontal' in v
            && 'vertical' in v
            && 'x' in v
            && 'y' in v)
        {
            return false;
        }
        else if (len === 7
            && 'theta' in v
            && 'scale' in v
            && 'x' in v
            && 'y' in v
            && 'alpha' in v
            && 'form' in v)
        {
            return false;
        }
        return true;
    }

    return elm.Native.Show.values = {
        toString: toString
    };
};


Elm.Native.Signal = {};
Elm.Native.Signal.make = function(localRuntime) {

  localRuntime.Native = localRuntime.Native || {};
  localRuntime.Native.Signal = localRuntime.Native.Signal || {};
  if (localRuntime.Native.Signal.values) {
      return localRuntime.Native.Signal.values;
  }

  var Utils = Elm.Native.Utils.make(localRuntime);

  function broadcastToKids(node, timestep, changed) {
    var kids = node.kids;
    for (var i = kids.length; i--; ) {
      kids[i].recv(timestep, changed, node.id);
    }
  }

  function Input(base) {
    this.id = Utils.guid();
    this.value = base;
    this.kids = [];
    this.defaultNumberOfKids = 0;
    this.recv = function(timestep, eid, v) {
      var changed = eid === this.id;
      if (changed) {
        this.value = v;
      }
      broadcastToKids(this, timestep, changed);
      return changed;
    };
    localRuntime.inputs.push(this);
  }

  function LiftN(update, args) {
    this.id = Utils.guid();
    this.value = update();
    this.kids = [];

    var n = args.length;
    var count = 0;
    var isChanged = false;

    this.recv = function(timestep, changed, parentID) {
      ++count;
      if (changed) { isChanged = true; }
      if (count == n) {
        if (isChanged) { this.value = update(); }
        broadcastToKids(this, timestep, isChanged);
        isChanged = false;
        count = 0;
      }
    };
    for (var i = n; i--; ) { args[i].kids.push(this); }
  }

  function map(func, a) {
    function update() { return func(a.value); }
    return new LiftN(update, [a]);
  }
  function map2(func, a, b) {
    function update() { return A2( func, a.value, b.value ); }
    return new LiftN(update, [a,b]);
  }
  function map3(func, a, b, c) {
    function update() { return A3( func, a.value, b.value, c.value ); }
    return new LiftN(update, [a,b,c]);
  }
  function map4(func, a, b, c, d) {
    function update() { return A4( func, a.value, b.value, c.value, d.value ); }
    return new LiftN(update, [a,b,c,d]);
  }
  function map5(func, a, b, c, d, e) {
    function update() { return A5( func, a.value, b.value, c.value, d.value, e.value ); }
    return new LiftN(update, [a,b,c,d,e]);
  }

  function Foldp(step, state, input) {
    this.id = Utils.guid();
    this.value = state;
    this.kids = [];

    this.recv = function(timestep, changed, parentID) {
      if (changed) {
          this.value = A2( step, input.value, this.value );
      }
      broadcastToKids(this, timestep, changed);
    };
    input.kids.push(this);
  }

  function foldp(step, state, input) {
      return new Foldp(step, state, input);
  }

  function DropIf(pred,base,input) {
    this.id = Utils.guid();
    this.value = pred(input.value) ? base : input.value;
    this.kids = [];
    this.recv = function(timestep, changed, parentID) {
      var chng = changed && !pred(input.value);
      if (chng) { this.value = input.value; }
      broadcastToKids(this, timestep, chng);
    };
    input.kids.push(this);
  }

  function DropRepeats(input) {
    this.id = Utils.guid();
    this.value = input.value;
    this.kids = [];
    this.recv = function(timestep, changed, parentID) {
      var chng = changed && !Utils.eq(this.value,input.value);
      if (chng) { this.value = input.value; }
      broadcastToKids(this, timestep, chng);
    };
    input.kids.push(this);
  }

  function timestamp(a) {
    function update() { return Utils.Tuple2(localRuntime.timer.now(), a.value); }
    return new LiftN(update, [a]);
  }

  function SampleOn(s1,s2) {
    this.id = Utils.guid();
    this.value = s2.value;
    this.kids = [];

    var count = 0;
    var isChanged = false;

    this.recv = function(timestep, changed, parentID) {
      if (parentID === s1.id) isChanged = changed;
      ++count;
      if (count == 2) {
        if (isChanged) { this.value = s2.value; }
        broadcastToKids(this, timestep, isChanged);
        count = 0;
        isChanged = false;
      }
    };
    s1.kids.push(this);
    s2.kids.push(this);
  }

  function sampleOn(s1,s2) { return new SampleOn(s1,s2); }

  function delay(t,s) {
      var delayed = new Input(s.value);
      var firstEvent = true;
      function update(v) {
        if (firstEvent) {
            firstEvent = false; return;
        }
        setTimeout(function() {
            localRuntime.notify(delayed.id, v);
        }, t);
      }
      function first(a,b) { return a; }
      return new SampleOn(delayed, map2(F2(first), delayed, map(update,s)));
  }

  function Merge(s1,s2) {
      this.id = Utils.guid();
      this.value = s1.value;
      this.kids = [];

      var next = null;
      var count = 0;
      var isChanged = false;

      this.recv = function(timestep, changed, parentID) {
        ++count;
        if (changed) {
            isChanged = true;
            if (parentID == s2.id && next === null) { next = s2.value; }
            if (parentID == s1.id) { next = s1.value; }
        }

        if (count == 2) {
            if (isChanged) { this.value = next; next = null; }
            broadcastToKids(this, timestep, isChanged);
            isChanged = false;
            count = 0;
        }
      };
      s1.kids.push(this);
      s2.kids.push(this);
  }

  function merge(s1,s2) {
      return new Merge(s1,s2);
  }


    // SIGNAL INPUTS

    function input(initialValue) {
        return new Input(initialValue);
    }

    function send(input, value) {
        return function() {
            localRuntime.notify(input.id, value);
        };
    }

    function subscribe(input) {
        return input;
    }


  return localRuntime.Native.Signal.values = {
    constant : function(v) { return new Input(v); },
    map  : F2(map ),
    map2 : F3(map2),
    map3 : F4(map3),
    map4 : F5(map4),
    map5 : F6(map5),
    foldp : F3(foldp),
    delay : F2(delay),
    merge : F2(merge),
    keepIf : F3(function(pred,base,sig) {
      return new DropIf(function(x) {return !pred(x);},base,sig); }),
    dropIf : F3(function(pred,base,sig) { return new DropIf(pred,base,sig); }),
    dropRepeats : function(s) { return new DropRepeats(s);},
    sampleOn : F2(sampleOn),
    timestamp : timestamp,
    input: input,
    send: F2(send),
    subscribe: subscribe
  };
};

Elm.Native.String = {};
Elm.Native.String.make = function(elm) {
    elm.Native = elm.Native || {};
    elm.Native.String = elm.Native.String || {};
    if (elm.Native.String.values) return elm.Native.String.values;
    if ('values' in Elm.Native.String) {
        return elm.Native.String.values = Elm.Native.String.values;
    }

    var Char = Elm.Char.make(elm);
    var List = Elm.Native.List.make(elm);
    var Maybe = Elm.Maybe.make(elm);
    var Result = Elm.Result.make(elm);
    var Utils = Elm.Native.Utils.make(elm);

    function isEmpty(str) {
        return str.length === 0;
    }
    function cons(chr,str) {
        return chr + str;
    }
    function uncons(str) {
        var hd;
        return (hd = str[0])
            ? Maybe.Just(Utils.Tuple2(Utils.chr(hd), str.slice(1)))
            : Maybe.Nothing;
    }
    function append(a,b) {
        return a + b;
    }
    function concat(strs) {
        return List.toArray(strs).join('');
    }
    function length(str) {
        return str.length;
    }
    function map(f,str) {
        var out = str.split('');
        for (var i = out.length; i--; ) {
            out[i] = f(Utils.chr(out[i]));
        }
        return out.join('');
    }
    function filter(pred,str) {
        return str.split('').map(Utils.chr).filter(pred).join('');
    }
    function reverse(str) {
        return str.split('').reverse().join('');
    }
    function foldl(f,b,str) {
        var len = str.length;
        for (var i = 0; i < len; ++i) {
            b = A2(f, Utils.chr(str[i]), b);
        }
        return b;
    }
    function foldr(f,b,str) {
        for (var i = str.length; i--; ) {
            b = A2(f, Utils.chr(str[i]), b);
        }
        return b;
    }

    function split(sep, str) {
        return List.fromArray(str.split(sep));
    }
    function join(sep, strs) {
        return List.toArray(strs).join(sep);
    }
    function repeat(n, str) {
        var result = '';
        while (n > 0) {
            if (n & 1) result += str;
            n >>= 1, str += str;
        }
        return result;
    }

    function slice(start, end, str) {
        return str.slice(start,end);
    }
    function left(n, str) {
        return n < 1 ? "" : str.slice(0,n);
    }
    function right(n, str) {
        return n < 1 ? "" : str.slice(-n);
    }
    function dropLeft(n, str) {
        return n < 1 ? str : str.slice(n);
    }
    function dropRight(n, str) {
        return n < 1 ? str : str.slice(0,-n);
    }

    function pad(n,chr,str) {
        var half = (n - str.length) / 2;
        return repeat(Math.ceil(half),chr) + str + repeat(half|0,chr);
    }
    function padRight(n,chr,str) {
        return str + repeat(n - str.length, chr);
    }
    function padLeft(n,chr,str) {
        return repeat(n - str.length, chr) + str;
    }

    function trim(str) {
        return str.trim();
    }
    function trimLeft(str) {
        return str.trimLeft();
    }
    function trimRight(str) {
        return str.trimRight();
    }

    function words(str) {
        return List.fromArray(str.trim().split(/\s+/g));
    }
    function lines(str) {
        return List.fromArray(str.split(/\r\n|\r|\n/g));
    }

    function toUpper(str) {
        return str.toUpperCase();
    }
    function toLower(str) {
        return str.toLowerCase();
    }

    function any(pred, str) {
        for (var i = str.length; i--; ) {
            if (pred(Utils.chr(str[i]))) return true;
        }
        return false;
    }
    function all(pred, str) {
        for (var i = str.length; i--; ) {
            if (!pred(Utils.chr(str[i]))) return false;
        }
        return true;
    }

    function contains(sub, str) {
        return str.indexOf(sub) > -1;
    }
    function startsWith(sub, str) {
        return str.indexOf(sub) === 0;
    }
    function endsWith(sub, str) {
        return str.length >= sub.length &&
               str.lastIndexOf(sub) === str.length - sub.length;
    }
    function indexes(sub, str) {
        var subLen = sub.length;
        var i = 0;
        var is = [];
        while ((i = str.indexOf(sub, i)) > -1) {
            is.push(i);
            i = i + subLen;
        }
        return List.fromArray(is);
    }

    function toInt(s) {
        var len = s.length;
        if (len === 0) {
            return Result.Err("could not convert string '" + s + "' to an Int" );
        }
        var start = 0;
        if (s[0] == '-') {
            if (len === 1) {
                return Result.Err("could not convert string '" + s + "' to an Int" );
            }
            start = 1;
        }
        for (var i = start; i < len; ++i) {
            if (!Char.isDigit(s[i])) {
                return Result.Err("could not convert string '" + s + "' to an Int" );
            }
        }
        return Result.Ok(parseInt(s, 10));
    }

    function toFloat(s) {
        var len = s.length;
        if (len === 0) {
            return Result.Err("could not convert string '" + s + "' to a Float" );
        }
        var start = 0;
        if (s[0] == '-') {
            if (len === 1) {
                return Result.Err("could not convert string '" + s + "' to a Float" );
            }
            start = 1;
        }
        var dotCount = 0;
        for (var i = start; i < len; ++i) {
            if (Char.isDigit(s[i])) {
                continue;
            }
            if (s[i] === '.') {
                dotCount += 1;
                if (dotCount <= 1) {
                    continue;
                }
            }
            return Result.Err("could not convert string '" + s + "' to a Float" );
        }
        return Result.Ok(parseFloat(s));
    }

    function toList(str) {
        return List.fromArray(str.split('').map(Utils.chr));
    }
    function fromList(chars) {
        return List.toArray(chars).join('');
    }

    return Elm.Native.String.values = {
        isEmpty: isEmpty,
        cons: F2(cons),
        uncons: uncons,
        append: F2(append),
        concat: concat,
        length: length,
        map: F2(map),
        filter: F2(filter),
        reverse: reverse,
        foldl: F3(foldl),
        foldr: F3(foldr),

        split: F2(split),
        join: F2(join),
        repeat: F2(repeat),

        slice: F3(slice),
        left: F2(left),
        right: F2(right),
        dropLeft: F2(dropLeft),
        dropRight: F2(dropRight),

        pad: F3(pad),
        padLeft: F3(padLeft),
        padRight: F3(padRight),

        trim: trim,
        trimLeft: trimLeft,
        trimRight: trimRight,

        words: words,
        lines: lines,

        toUpper: toUpper,
        toLower: toLower,

        any: F2(any),
        all: F2(all),

        contains: F2(contains),
        startsWith: F2(startsWith),
        endsWith: F2(endsWith),
        indexes: F2(indexes),

        toInt: toInt,
        toFloat: toFloat,
        toList: toList,
        fromList: fromList
    };
};

Elm.Native.Text = {};
Elm.Native.Text.make = function(elm) {
    elm.Native = elm.Native || {};
    elm.Native.Text = elm.Native.Text || {};
    if (elm.Native.Text.values) return elm.Native.Text.values;

    var toCss = Elm.Native.Color.make(elm).toCss;
    var Element = Elm.Graphics.Element.make(elm);
    var NativeElement = Elm.Native.Graphics.Element.make(elm);
    var List = Elm.Native.List.make(elm);
    var Utils = Elm.Native.Utils.make(elm);

    function makeSpaces(s) {
        if (s.length == 0) { return s; }
        var arr = s.split('');
        if (arr[0] == ' ') { arr[0] = "&nbsp;" }      
        for (var i = arr.length; --i; ) {
            if (arr[i][0] == ' ' && arr[i-1] == ' ') {
                arr[i-1] = arr[i-1] + arr[i];
                arr[i] = '';
            }
        }
        for (var i = arr.length; i--; ) {
            if (arr[i].length > 1 && arr[i][0] == ' ') {
                var spaces = arr[i].split('');
                for (var j = spaces.length - 2; j >= 0; j -= 2) {
                    spaces[j] = '&nbsp;';
                }
                arr[i] = spaces.join('');
            }
        }
        arr = arr.join('');
        if (arr[arr.length-1] === " ") {
            return arr.slice(0,-1) + '&nbsp;';
        }
        return arr;
    }

    function properEscape(str) {
        if (str.length == 0) return str;
        str = str //.replace(/&/g,  "&#38;")
            .replace(/"/g,  '&#34;')
            .replace(/'/g,  "&#39;")
            .replace(/</g,  "&#60;")
            .replace(/>/g,  "&#62;")
            .replace(/\n/g, "<br/>");
        var arr = str.split('<br/>');
        for (var i = arr.length; i--; ) {
            arr[i] = makeSpaces(arr[i]);
        }
        return arr.join('<br/>');
    }

    function fromString(str) {
        return Utils.txt(properEscape(str));
    }

    function append(xs, ys) {
        return Utils.txt(Utils.makeText(xs) + Utils.makeText(ys));
    }

    // conversions from Elm values to CSS
    function toTypefaces(list) {
        var typefaces = List.toArray(list);
        for (var i = typefaces.length; i--; ) {
            var typeface = typefaces[i];
            if (typeface.indexOf(' ') > -1) {
                typefaces[i] = "'" + typeface + "'";
            }
        }
        return typefaces.join(',');
    }
    function toLine(line) {
        var ctor = line.ctor;
        return ctor === 'Under' ? 'underline' :
               ctor === 'Over'  ? 'overline'  : 'line-through';
    }

    // setting styles of Text
    function style(style, text) {
        var newText = '<span style="color:' + toCss(style.color) + ';'
        if (style.typeface.ctor !== '[]') {
            newText += 'font-family:' + toTypefaces(style.typeface) + ';'
        }
        if (style.height.ctor !== "Nothing") {
            newText += 'font-size:' + style.height._0 + 'px;';
        }
        if (style.bold) {
            newText += 'font-weight:bold;';
        }
        if (style.italic) {
            newText += 'font-style:italic;';
        }
        if (style.line.ctor !== 'Nothing') {
            newText += 'text-decoration:' + toLine(style.line._0) + ';';
        }
        newText += '">' + Utils.makeText(text) + '</span>'
        return Utils.txt(newText);
    }
    function height(px, text) {
        return { style: 'font-size:' + px + 'px;', text:text }
    }
    function typeface(names, text) {
        return { style: 'font-family:' + toTypefaces(names) + ';', text:text }
    }
    function monospace(text) {
        return { style: 'font-family:monospace;', text:text }
    }
    function italic(text) {
        return { style: 'font-style:italic;', text:text }
    }
    function bold(text) {
        return { style: 'font-weight:bold;', text:text }
    }
    function link(href, text) {
        return { href: fromString(href), text:text };
    }
    function line(line, text) {
        return { style: 'text-decoration:' + toLine(line) + ';', text:text };
    }

    function color(color, text) {
        return { style: 'color:' + toCss(color) + ';', text:text };
    }

    function block(align) {
        return function(text) {
            var raw = {
                ctor :'RawHtml',
                html : Utils.makeText(text),
                align: align
            };
            var pos = A2(NativeElement.htmlHeight, 0, raw);
            return A3(Element.newElement, pos._0, pos._1, raw);
        }
    }

    function markdown(text) {
        var raw = {
            ctor:'RawHtml',
            html: text,
            align: null
        };
        var pos = A2(NativeElement.htmlHeight, 0, raw);
        return A3(Element.newElement, pos._0, pos._1, raw);
    }

    return elm.Native.Text.values = {
        fromString: fromString,
        append: F2(append),

        height : F2(height),
        italic : italic,
        bold : bold,
        line : F2(line),
        monospace : monospace,
        typeface : F2(typeface),
        color : F2(color),
        link : F2(link),
        style : F2(style),

        leftAligned  : block('left'),
        rightAligned : block('right'),
        centered     : block('center'),
        justified    : block('justify'),
        markdown     : markdown,

        toTypefaces:toTypefaces,
        toLine:toLine
    };
};

Elm.Native.Time = {};
Elm.Native.Time.make = function(elm) {

  elm.Native = elm.Native || {};
  elm.Native.Time = elm.Native.Time || {};
  if (elm.Native.Time.values) return elm.Native.Time.values;

  var Signal = Elm.Signal.make(elm);
  var NS = Elm.Native.Signal.make(elm);
  var Maybe = Elm.Maybe.make(elm);
  var Utils = Elm.Native.Utils.make(elm);

  function fpsWhen(desiredFPS, isOn) {
    var msPerFrame = 1000 / desiredFPS;
    var prev = elm.timer.now(), curr = prev, diff = 0, wasOn = true;
    var ticker = NS.input(diff);
    function tick(zero) {
      return function() {
        curr = elm.timer.now();
        diff = zero ? 0 : curr - prev;
        if (prev > curr) {
          diff = 0;
        }
        prev = curr;
        elm.notify(ticker.id, diff);
      };
    }
    var timeoutID = 0;
    function f(isOn, t) {
      if (isOn) {
        timeoutID = elm.setTimeout(tick(!wasOn && isOn), msPerFrame);
      } else if (wasOn) {
        clearTimeout(timeoutID);
      }
      wasOn = isOn;
      return t;
    }
    return A3( Signal.map2, F2(f), isOn, ticker );
  }

  function every(t) {
    var clock = NS.input(elm.timer.now());
    function tellTime() {
        elm.notify(clock.id, elm.timer.now());
    }
    setInterval(tellTime, t);
    return clock;
  }

  function read(s) {
      var t = Date.parse(s);
      return isNaN(t) ? Maybe.Nothing : Maybe.Just(t);
  }
  return elm.Native.Time.values = {
      fpsWhen : F2(fpsWhen),
      fps : function(t) { return fpsWhen(t, Signal.constant(true)); },
      every : every,
      delay : NS.delay,
      timestamp : NS.timestamp,
      toDate : function(t) { return new window.Date(t); },
      read   : read
  };

};

Elm.Native.Transform2D = {};
Elm.Native.Transform2D.make = function(elm) {

 elm.Native = elm.Native || {};
 elm.Native.Transform2D = elm.Native.Transform2D || {};
 if (elm.Native.Transform2D.values) return elm.Native.Transform2D.values;

 var A;
 if (typeof Float32Array === 'undefined') {
     A = function(arr) {
         this.length = arr.length;
         this[0] = arr[0];
         this[1] = arr[1];
         this[2] = arr[2];
         this[3] = arr[3];
         this[4] = arr[4];
         this[5] = arr[5];
     };
 } else {
     A = Float32Array;
 }

 // layout of matrix in an array is
 //
 //   | m11 m12 dx |
 //   | m21 m22 dy |
 //   |  0   0   1 |
 //
 //  new A([ m11, m12, dx, m21, m22, dy ])

 var identity = new A([1,0,0,0,1,0]);
 function matrix(m11, m12, m21, m22, dx, dy) {
     return new A([m11, m12, dx, m21, m22, dy]);
 }
 function rotation(t) {
     var c = Math.cos(t);
     var s = Math.sin(t);
     return new A([c, -s, 0, s, c, 0]);
 }
 function rotate(t,m) {
     var c = Math.cos(t);
     var s = Math.sin(t);
     var m11 = m[0], m12 = m[1], m21 = m[3], m22 = m[4];
     return new A([m11*c + m12*s, -m11*s + m12*c, m[2],
                   m21*c + m22*s, -m21*s + m22*c, m[5]]);
 }
 /*
 function move(xy,m) {
     var x = xy._0;
     var y = xy._1;
     var m11 = m[0], m12 = m[1], m21 = m[3], m22 = m[4];
     return new A([m11, m12, m11*x + m12*y + m[2],
                   m21, m22, m21*x + m22*y + m[5]]);
 }
 function scale(s,m) { return new A([m[0]*s, m[1]*s, m[2], m[3]*s, m[4]*s, m[5]]); }
 function scaleX(x,m) { return new A([m[0]*x, m[1], m[2], m[3]*x, m[4], m[5]]); }
 function scaleY(y,m) { return new A([m[0], m[1]*y, m[2], m[3], m[4]*y, m[5]]); }
 function reflectX(m) { return new A([-m[0], m[1], m[2], -m[3], m[4], m[5]]); }
 function reflectY(m) { return new A([m[0], -m[1], m[2], m[3], -m[4], m[5]]); }

 function transform(m11, m21, m12, m22, mdx, mdy, n) {
     var n11 = n[0], n12 = n[1], n21 = n[3], n22 = n[4], ndx = n[2], ndy = n[5];
     return new A([m11*n11 + m12*n21,
                   m11*n12 + m12*n22,
                   m11*ndx + m12*ndy + mdx,
                   m21*n11 + m22*n21,
                   m21*n12 + m22*n22,
                   m21*ndx + m22*ndy + mdy]);
 }
 */
 function multiply(m, n) {
     var m11 = m[0], m12 = m[1], m21 = m[3], m22 = m[4], mdx = m[2], mdy = m[5];
     var n11 = n[0], n12 = n[1], n21 = n[3], n22 = n[4], ndx = n[2], ndy = n[5];
     return new A([m11*n11 + m12*n21,
                   m11*n12 + m12*n22,
                   m11*ndx + m12*ndy + mdx,
                   m21*n11 + m22*n21,
                   m21*n12 + m22*n22,
                   m21*ndx + m22*ndy + mdy]);
 }

 return elm.Native.Transform2D.values = {
     identity:identity,
     matrix:F6(matrix),
     rotation:rotation,
     multiply:F2(multiply)
     /*
     transform:F7(transform),
     rotate:F2(rotate),
     move:F2(move),
     scale:F2(scale),
     scaleX:F2(scaleX),
     scaleY:F2(scaleY),
     reflectX:reflectX,
     reflectY:reflectY
     */
 };

};

Elm.Native = Elm.Native || {};
Elm.Native.Utils = {};
Elm.Native.Utils.make = function(localRuntime) {

    localRuntime.Native = localRuntime.Native || {};
    localRuntime.Native.Utils = localRuntime.Native.Utils || {};
    if (localRuntime.Native.Utils.values) {
        return localRuntime.Native.Utils.values;
    }

    function eq(l,r) {
        var stack = [{'x': l, 'y': r}]
        while (stack.length > 0) {
            var front = stack.pop();
            var x = front.x;
            var y = front.y;
            if (x === y) continue;
            if (typeof x === "object") {
                var c = 0;
                for (var i in x) {
                    ++c;
                    if (i in y) {
                        if (i !== 'ctor') {
                            stack.push({ 'x': x[i], 'y': y[i] });
                        }
                    } else {
                        return false;
                    }
                }
                if ('ctor' in x) {
                    stack.push({'x': x.ctor, 'y': y.ctor});
                }
                if (c !== Object.keys(y).length) {
                    return false;
                };
            } else if (typeof x === 'function') {
                throw new Error('Equality error: general function equality is ' +
                                'undecidable, and therefore, unsupported');
            } else {
                return false;
            }
        }
        return true;
    }

    // code in Generate/JavaScript.hs depends on the particular
    // integer values assigned to LT, EQ, and GT
    var LT = -1, EQ = 0, GT = 1, ord = ['LT','EQ','GT'];
    function compare(x,y) { return { ctor: ord[cmp(x,y)+1] } }
    function cmp(x,y) {
        var ord;
        if (typeof x !== 'object'){
            return x === y ? EQ : x < y ? LT : GT;
        }
        else if (x.isChar){
            var a = x.toString();
            var b = y.toString();
            return a === b ? EQ : a < b ? LT : GT;
        }
        else if (x.ctor === "::" || x.ctor === "[]") {
            while (true) {
                if (x.ctor === "[]" && y.ctor === "[]") return EQ;
                if (x.ctor !== y.ctor) return x.ctor === '[]' ? LT : GT;
                ord = cmp(x._0, y._0);
                if (ord !== EQ) return ord;
                x = x._1;
                y = y._1;
            }
        }
        else if (x.ctor.slice(0,6) === '_Tuple') {
            var n = x.ctor.slice(6) - 0;
            var err = 'cannot compare tuples with more than 6 elements.';
            if (n === 0) return EQ;
            if (n >= 1) { ord = cmp(x._0, y._0); if (ord !== EQ) return ord;
            if (n >= 2) { ord = cmp(x._1, y._1); if (ord !== EQ) return ord;
            if (n >= 3) { ord = cmp(x._2, y._2); if (ord !== EQ) return ord;
            if (n >= 4) { ord = cmp(x._3, y._3); if (ord !== EQ) return ord;
            if (n >= 5) { ord = cmp(x._4, y._4); if (ord !== EQ) return ord;
            if (n >= 6) { ord = cmp(x._5, y._5); if (ord !== EQ) return ord;
            if (n >= 7) throw new Error('Comparison error: ' + err); } } } } } }
            return EQ;
        }
        else {
            throw new Error('Comparison error: comparison is only defined on ints, ' +
                            'floats, times, chars, strings, lists of comparable values, ' +
                            'and tuples of comparable values.');
        }
    }


    var Tuple0 = { ctor: "_Tuple0" };
    function Tuple2(x,y) {
        return {
            ctor: "_Tuple2",
            _0: x,
            _1: y
        };
    }

    function chr(c) {
        var x = new String(c);
        x.isChar = true;
        return x;
    }

    function txt(str) {
        var t = new String(str);
        t.text = true;
        return t;
    }

    function makeText(text) {
        var style = '';
        var href = '';
        while (true) {
            if (text.style) {
                style += text.style;
                text = text.text;
                continue;
            }
            if (text.href) {
                href = text.href;
                text = text.text;
                continue;
            }
            if (href) {
                text = '<a href="' + href + '">' + text + '</a>';
            }
            if (style) {
                text = '<span style="' + style + '">' + text + '</span>';
            }
            return text;
        }
    }

    var count = 0;
    function guid(_) {
        return count++
    }

    function copy(oldRecord) {
        var newRecord = {};
        for (var key in oldRecord) {
            var value = key === '_'
                ? copy(oldRecord._)
                : oldRecord[key]
                ;
            newRecord[key] = value;
        }
        return newRecord;
    }

    function remove(key, oldRecord) {
        var record = copy(oldRecord);
        if (key in record._) {
            record[key] = record._[key][0];
            record._[key] = record._[key].slice(1);
            if (record._[key].length === 0) {
                delete record._[key];
            }
        } else {
            delete record[key];
        }
        return record;
    }

    function replace(keyValuePairs, oldRecord) {
        var record = copy(oldRecord);
        for (var i = keyValuePairs.length; i--; ) {
            var pair = keyValuePairs[i];
            record[pair[0]] = pair[1];
        }
        return record;
    }

    function insert(key, value, oldRecord) {
        var newRecord = copy(oldRecord);
        if (key in newRecord) {
            var values = newRecord._[key];
            var copiedValues = values ? values.slice(0) : [];
            newRecord._[key] = [newRecord[key]].concat(copiedValues);
        }
        newRecord[key] = value;
        return newRecord;
    }

    function getXY(e) {
        var posx = 0;
        var posy = 0;
        if (e.pageX || e.pageY) {
            posx = e.pageX;
            posy = e.pageY;
        } else if (e.clientX || e.clientY) {
            posx = e.clientX + document.body.scrollLeft + document.documentElement.scrollLeft;
            posy = e.clientY + document.body.scrollTop + document.documentElement.scrollTop;
        }

        if (localRuntime.isEmbed()) {
            var rect = localRuntime.node.getBoundingClientRect();
            var relx = rect.left + document.body.scrollLeft + document.documentElement.scrollLeft;
            var rely = rect.top + document.body.scrollTop + document.documentElement.scrollTop;
            // TODO: figure out if there is a way to avoid rounding here
            posx = posx - Math.round(relx) - localRuntime.node.clientLeft;
            posy = posy - Math.round(rely) - localRuntime.node.clientTop;
        }
        return Tuple2(posx, posy);
    }


    //// LIST STUFF ////

    var Nil = { ctor:'[]' };

    function Cons(hd,tl) {
        return {
            ctor: "::",
            _0: hd,
            _1: tl
        };
    }

    function append(xs,ys) {
        // append Text
        if (xs.text || ys.text) {
            return txt(makeText(xs) + makeText(ys));
        }

        // append Strings
        if (typeof xs === "string") {
            return xs + ys;
        }

        // append Lists
        if (xs.ctor === '[]') {
            return ys;
        }
        var root = Cons(xs._0, Nil);
        var curr = root;
        xs = xs._1;
        while (xs.ctor !== '[]') {
            curr._1 = Cons(xs._0, Nil);
            xs = xs._1;
            curr = curr._1;
        }
        curr._1 = ys;
        return root;
    }

    //// RUNTIME ERRORS ////

    function indent(lines) {
        return '\n' + lines.join('\n');
    }

    function badCase(moduleName, span) { 
        var msg = indent([
            'Non-exhaustive pattern match in case-expression.',
            'Make sure your patterns cover every case!'
        ]);
        throw new Error('Runtime error in module ' + moduleName + ' (' + span + ')' + msg);
    }

    function badIf(moduleName, span) { 
        var msg = indent([
            'Non-exhaustive pattern match in multi-way-if expression.',
            'It is best to use \'otherwise\' as the last branch of multi-way-if.'
        ]);
        throw new Error('Runtime error in module ' + moduleName + ' (' + span + ')' + msg);
    }


    function badPort(expected, received) { 
        var msg = indent([
            'Expecting ' + expected + ' but was given ',
            JSON.stringify(received)
        ]);
        throw new Error('Runtime error when sending values through a port.' + msg);
    }


    return localRuntime.Native.Utils.values = {
        eq:eq,
        cmp:cmp,
        compare:F2(compare),
        Tuple0:Tuple0,
        Tuple2:Tuple2,
        chr:chr,
        txt:txt,
        makeText:makeText,
        copy: copy,
        remove: remove,
        replace: replace,
        insert: insert,
        guid: guid,
        getXY: getXY,

        Nil: Nil,
        Cons: Cons,
        append: F2(append),

        badCase: badCase,
        badIf: badIf,
        badPort: badPort
    };
};

Elm.Native.WebSocket = {};
Elm.Native.WebSocket.make = function(elm) {

  elm.Native = elm.Native || {};
  elm.Native.WebSocket = elm.Native.WebSocket || {};
  if (elm.Native.WebSocket.values) return elm.Native.WebSocket.values;

  var Signal = Elm.Signal.make(elm);
  var List = Elm.Native.List.make(elm);

  function open(url, outgoing) {
    var incoming = Signal.constant("");
    var ws = new WebSocket(url);

    var pending = [];
    var ready = false;
    
    ws.onopen = function(e) {
      var len = pending.length;
      for (var i = 0; i < len; ++i) { ws.send(pending[i]); }
      ready = true;
    };
    ws.onmessage = function(event) {
      elm.notify(incoming.id, event.data);
    };
    
    function send(msg) {
      ready ? ws.send(msg) : pending.push(msg);
    }
    
    function take1(x,y) { return x }
    return A3(Signal.map2, F2(take1), incoming, A2(Signal.map, send, outgoing));
  }

  return elm.Native.WebSocket.values = { connect: F2(open) };
};

Elm.Native = Elm.Native || {};
Elm.Native.Window = {};
Elm.Native.Window.make = function(localRuntime) {

    localRuntime.Native = localRuntime.Native || {};
    localRuntime.Native.Window = localRuntime.Native.Window || {};
    if (localRuntime.Native.Window.values) {
        return localRuntime.Native.Window.values;
    }

    var Signal = Elm.Signal.make(localRuntime);
    var NS = Elm.Native.Signal.make(localRuntime);
    var Tuple2 = Elm.Native.Utils.make(localRuntime).Tuple2;

    function getWidth() {
        return localRuntime.node.clientWidth;
    }
    function getHeight() {
        if (localRuntime.isFullscreen()) {
            return window.innerHeight;
        }
        return localRuntime.node.clientHeight;
    }

    var dimensions = NS.input(Tuple2(getWidth(), getHeight()));
    dimensions.defaultNumberOfKids = 2;

    // Do not move width and height into Elm. By setting the default number of kids,
    // the resize listener can be detached.
    var width  = A2(Signal.map, function(p){return p._0;}, dimensions);
    width.defaultNumberOfKids = 0;

    var height = A2(Signal.map, function(p){return p._1;}, dimensions);
    height.defaultNumberOfKids = 0;

    function resizeIfNeeded() {
        // Do not trigger event if the dimensions have not changed.
        // This should be most of the time.
        var w = getWidth();
        var h = getHeight();
        if (dimensions.value._0 === w && dimensions.value._1 === h) return;

        setTimeout(function () {
            // Check again to see if the dimensions have changed.
            // It is conceivable that the dimensions have changed
            // again while some other event was being processed.
            var w = getWidth();
            var h = getHeight();
            if (dimensions.value._0 === w && dimensions.value._1 === h) return;
            localRuntime.notify(dimensions.id, Tuple2(w,h));
        }, 0);
    }
    localRuntime.addListener([dimensions.id], window, 'resize', resizeIfNeeded);

    return localRuntime.Native.Window.values = {
        dimensions: dimensions,
        width: width,
        height: height,
        resizeIfNeeded: resizeIfNeeded
    };

};

Elm.Piece = Elm.Piece || {};
Elm.Piece.make = function (_elm) {
   "use strict";
   _elm.Piece = _elm.Piece || {};
   if (_elm.Piece.values)
   return _elm.Piece.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _P = _N.Ports.make(_elm),
   $moduleName = "Piece",
   $GameTypes = Elm.GameTypes.make(_elm);
   var baseValue = function (piece) {
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
         _U.badCase($moduleName,
         "between lines 31 and 39");
      }();
   };
   var toString = function (piece) {
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
         _U.badCase($moduleName,
         "between lines 19 and 27");
      }();
   };
   var fromString = function (str) {
      return function () {
         switch (str)
         {case "Dragon":
            return $GameTypes.Dragon;
            case "Fenrir":
            return $GameTypes.Fenrir;
            case "Loki":
            return $GameTypes.Loki;
            case "Odin":
            return $GameTypes.Odin;
            case "Skadi":
            return $GameTypes.Skadi;
            case "Thor":
            return $GameTypes.Thor;
            case "Troll":
            return $GameTypes.Troll;
            case "Valkyrie":
            return $GameTypes.Valkyrie;}
         _U.badCase($moduleName,
         "between lines 7 and 15");
      }();
   };
   _elm.Piece.values = {_op: _op
                       ,fromString: fromString
                       ,toString: toString
                       ,baseValue: baseValue};
   return _elm.Piece.values;
};
Elm.Player = Elm.Player || {};
Elm.Player.make = function (_elm) {
   "use strict";
   _elm.Player = _elm.Player || {};
   if (_elm.Player.values)
   return _elm.Player.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _P = _N.Ports.make(_elm),
   $moduleName = "Player",
   $Basics = Elm.Basics.make(_elm),
   $Color = Elm.Color.make(_elm),
   $Dict = Elm.Dict.make(_elm),
   $GameTypes = Elm.GameTypes.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm);
   var next = function (player) {
      return function () {
         switch (player.ctor)
         {case "Blue":
            return $GameTypes.Red;
            case "Red":
            return $GameTypes.Blue;}
         _U.badCase($moduleName,
         "between lines 32 and 34");
      }();
   };
   var color = function (player) {
      return function () {
         switch (player.ctor)
         {case "Blue": return "blue";
            case "Red": return "red";}
         _U.badCase($moduleName,
         "between lines 26 and 28");
      }();
   };
   var getType = F2(function (player,
   state) {
      return A2($Maybe.withDefault,
      $GameTypes.Human,
      A2($Dict.get,
      color(player),
      state.players));
   });
   var isPlayerTurn = function (state) {
      return $GameTypes.isOngoing(state) && $Basics.not(_U.eq(A2(getType,
      state.turn,
      state),
      $GameTypes.Remote));
   };
   var getHand = F2(function (player,
   state) {
      return A2($Maybe.withDefault,
      _L.fromArray([]),
      A2($Dict.get,
      color(player),
      state.hands));
   });
   var noTilesInHand = F2(function (player,
   state) {
      return $List.isEmpty(A2(getHand,
      player,
      state));
   });
   var fromString = function (str) {
      return function () {
         switch (str)
         {case "blue":
            return $GameTypes.Blue;
            case "red":
            return $GameTypes.Red;}
         _U.badCase($moduleName,
         "between lines 20 and 22");
      }();
   };
   var toColor = function (player) {
      return function () {
         switch (player.ctor)
         {case "Blue":
            return $Color.blue;
            case "Red": return $Color.red;}
         _U.badCase($moduleName,
         "between lines 14 and 16");
      }();
   };
   _elm.Player.values = {_op: _op
                        ,toColor: toColor
                        ,fromString: fromString
                        ,color: color
                        ,next: next
                        ,getType: getType
                        ,getHand: getHand
                        ,noTilesInHand: noTilesInHand
                        ,isPlayerTurn: isPlayerTurn};
   return _elm.Player.values;
};
Elm.Random = Elm.Random || {};
Elm.Random.make = function (_elm) {
   "use strict";
   _elm.Random = _elm.Random || {};
   if (_elm.Random.values)
   return _elm.Random.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _P = _N.Ports.make(_elm),
   $moduleName = "Random",
   $Basics = Elm.Basics.make(_elm),
   $List = Elm.List.make(_elm);
   var magicNum8 = 2147483562;
   var range = function (_v0) {
      return function () {
         return {ctor: "_Tuple2"
                ,_0: 0
                ,_1: magicNum8};
      }();
   };
   var magicNum7 = 2137383399;
   var magicNum6 = 2147483563;
   var magicNum5 = 3791;
   var magicNum4 = 40692;
   var magicNum3 = 52774;
   var magicNum2 = 12211;
   var magicNum1 = 53668;
   var magicNum0 = 40014;
   var generate = F2(function (_v2,
   seed) {
      return function () {
         switch (_v2.ctor)
         {case "Generator":
            return _v2._0(seed);}
         _U.badCase($moduleName,
         "on line 247, column 5 to 19");
      }();
   });
   var Seed = F4(function (a,
   b,
   c,
   d) {
      return {_: {}
             ,next: b
             ,range: d
             ,split: c
             ,state: a};
   });
   var State = F2(function (a,b) {
      return {ctor: "State"
             ,_0: a
             ,_1: b};
   });
   var initState = function (s$) {
      return function () {
         var s = A2($Basics.max,
         s$,
         0 - s$);
         var q = s / (magicNum6 - 1) | 0;
         var s2 = A2($Basics._op["%"],
         q,
         magicNum7 - 1);
         var s1 = A2($Basics._op["%"],
         s,
         magicNum6 - 1);
         return A2(State,s1 + 1,s2 + 1);
      }();
   };
   var next = function (_v5) {
      return function () {
         switch (_v5.ctor)
         {case "State":
            return function () {
                 var k$ = _v5._1 / magicNum3 | 0;
                 var s2$ = magicNum4 * (_v5._1 - k$ * magicNum3) - k$ * magicNum5;
                 var s2$$ = _U.cmp(s2$,
                 0) < 0 ? s2$ + magicNum7 : s2$;
                 var k = _v5._0 / magicNum1 | 0;
                 var s1$ = magicNum0 * (_v5._0 - k * magicNum1) - k * magicNum2;
                 var s1$$ = _U.cmp(s1$,
                 0) < 0 ? s1$ + magicNum6 : s1$;
                 var z = s1$$ - s2$$;
                 var z$ = _U.cmp(z,
                 1) < 0 ? z + magicNum8 : z;
                 return {ctor: "_Tuple2"
                        ,_0: z$
                        ,_1: A2(State,s1$$,s2$$)};
              }();}
         _U.badCase($moduleName,
         "between lines 291 and 300");
      }();
   };
   var split = function (_v9) {
      return function () {
         switch (_v9.ctor)
         {case "State":
            return function () {
                 var _raw = $Basics.snd(next(_v9)),
                 $ = _raw.ctor === "State" ? _raw : _U.badCase($moduleName,
                 "on line 307, column 25 to 38"),
                 t1 = $._0,
                 t2 = $._1;
                 var new_s2 = _U.eq(_v9._1,
                 1) ? magicNum7 - 1 : _v9._1 - 1;
                 var new_s1 = _U.eq(_v9._0,
                 magicNum6 - 1) ? 1 : _v9._0 + 1;
                 return {ctor: "_Tuple2"
                        ,_0: A2(State,new_s1,t2)
                        ,_1: A2(State,t1,new_s2)};
              }();}
         _U.badCase($moduleName,
         "between lines 305 and 309");
      }();
   };
   var initialSeed = function (n) {
      return A4(Seed,
      initState(n),
      next,
      split,
      range);
   };
   var Generator = function (a) {
      return {ctor: "Generator"
             ,_0: a};
   };
   var customGenerator = function (generate) {
      return Generator(generate);
   };
   var listHelp = F4(function (list,
   n,
   generate,
   seed) {
      return _U.cmp(n,
      1) < 0 ? {ctor: "_Tuple2"
               ,_0: $List.reverse(list)
               ,_1: seed} : function () {
         var $ = generate(seed),
         value = $._0,
         seed$ = $._1;
         return A4(listHelp,
         A2($List._op["::"],value,list),
         n - 1,
         generate,
         seed$);
      }();
   });
   var list = F2(function (n,
   _v13) {
      return function () {
         switch (_v13.ctor)
         {case "Generator":
            return Generator(function (seed) {
                 return A4(listHelp,
                 _L.fromArray([]),
                 n,
                 _v13._0,
                 seed);
              });}
         _U.badCase($moduleName,
         "between lines 183 and 184");
      }();
   });
   var pair = F2(function (_v16,
   _v17) {
      return function () {
         switch (_v17.ctor)
         {case "Generator":
            return function () {
                 switch (_v16.ctor)
                 {case "Generator":
                    return Generator(function (seed) {
                         return function () {
                            var $ = _v16._0(seed),
                            left = $._0,
                            seed$ = $._1;
                            var $ = _v17._0(seed$),
                            right = $._0,
                            seed$$ = $._1;
                            return {ctor: "_Tuple2"
                                   ,_0: {ctor: "_Tuple2"
                                        ,_0: left
                                        ,_1: right}
                                   ,_1: seed$$};
                         }();
                      });}
                 _U.badCase($moduleName,
                 "between lines 160 and 164");
              }();}
         _U.badCase($moduleName,
         "between lines 160 and 164");
      }();
   });
   var minInt = -2147483648;
   var maxInt = 2147483647;
   var iLogBase = F2(function (b,
   i) {
      return _U.cmp(i,
      b) < 0 ? 1 : 1 + A2(iLogBase,
      b,
      i / b | 0);
   });
   var $int = F2(function (a,b) {
      return Generator(function (seed) {
         return function () {
            var base = 2147483561;
            var f = F3(function (n,
            acc,
            state) {
               return function () {
                  switch (n)
                  {case 0: return {ctor: "_Tuple2"
                                  ,_0: acc
                                  ,_1: state};}
                  return function () {
                     var $ = seed.next(state),
                     x = $._0,
                     state$ = $._1;
                     return A3(f,
                     n - 1,
                     x + acc * base,
                     state$);
                  }();
               }();
            });
            var $ = _U.cmp(a,
            b) < 0 ? {ctor: "_Tuple2"
                     ,_0: a
                     ,_1: b} : {ctor: "_Tuple2"
                               ,_0: b
                               ,_1: a},
            lo = $._0,
            hi = $._1;
            var k = hi - lo + 1;
            var n = A2(iLogBase,base,k);
            var $ = A3(f,n,1,seed.state),
            v = $._0,
            state$ = $._1;
            return {ctor: "_Tuple2"
                   ,_0: lo + A2($Basics._op["%"],
                   v,
                   k)
                   ,_1: _U.replace([["state"
                                    ,state$]],
                   seed)};
         }();
      });
   });
   var $float = F2(function (a,b) {
      return Generator(function (seed) {
         return function () {
            var $ = A2(generate,
            A2($int,minInt,maxInt),
            seed),
            number = $._0,
            seed$ = $._1;
            var negativeOneToOne = $Basics.toFloat(number) / $Basics.toFloat(maxInt - minInt);
            var $ = _U.cmp(a,
            b) < 0 ? {ctor: "_Tuple2"
                     ,_0: a
                     ,_1: b} : {ctor: "_Tuple2"
                               ,_0: b
                               ,_1: a},
            lo = $._0,
            hi = $._1;
            var scaled = (lo + hi) / 2 + (hi - lo) * negativeOneToOne;
            return {ctor: "_Tuple2"
                   ,_0: scaled
                   ,_1: seed$};
         }();
      });
   });
   _elm.Random.values = {_op: _op
                        ,$int: $int
                        ,$float: $float
                        ,list: list
                        ,pair: pair
                        ,minInt: minInt
                        ,maxInt: maxInt
                        ,generate: generate
                        ,initialSeed: initialSeed
                        ,customGenerator: customGenerator
                        ,Seed: Seed};
   return _elm.Random.values;
};
Elm.Result = Elm.Result || {};
Elm.Result.make = function (_elm) {
   "use strict";
   _elm.Result = _elm.Result || {};
   if (_elm.Result.values)
   return _elm.Result.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _P = _N.Ports.make(_elm),
   $moduleName = "Result",
   $Maybe = Elm.Maybe.make(_elm);
   var toMaybe = function (result) {
      return function () {
         switch (result.ctor)
         {case "Err":
            return $Maybe.Nothing;
            case "Ok":
            return $Maybe.Just(result._0);}
         _U.badCase($moduleName,
         "between lines 158 and 171");
      }();
   };
   var Err = function (a) {
      return {ctor: "Err",_0: a};
   };
   var andThen = F2(function (result,
   callback) {
      return function () {
         switch (result.ctor)
         {case "Err":
            return Err(result._0);
            case "Ok":
            return callback(result._0);}
         _U.badCase($moduleName,
         "between lines 120 and 139");
      }();
   });
   var Ok = function (a) {
      return {ctor: "Ok",_0: a};
   };
   var map = F2(function (func,
   ra) {
      return function () {
         switch (ra.ctor)
         {case "Err": return Err(ra._0);
            case "Ok":
            return Ok(func(ra._0));}
         _U.badCase($moduleName,
         "between lines 35 and 46");
      }();
   });
   var map2 = F3(function (func,
   ra,
   rb) {
      return function () {
         var _v9 = {ctor: "_Tuple2"
                   ,_0: ra
                   ,_1: rb};
         switch (_v9.ctor)
         {case "_Tuple2":
            switch (_v9._0.ctor)
              {case "Err":
                 return Err(_v9._0._0);
                 case "Ok": switch (_v9._1.ctor)
                   {case "Ok": return Ok(A2(func,
                        _v9._0._0,
                        _v9._1._0));}
                   break;}
              switch (_v9._1.ctor)
              {case "Err":
                 return Err(_v9._1._0);}
              break;}
         _U.badCase($moduleName,
         "between lines 49 and 52");
      }();
   });
   var map3 = F4(function (func,
   ra,
   rb,
   rc) {
      return function () {
         var _v16 = {ctor: "_Tuple3"
                    ,_0: ra
                    ,_1: rb
                    ,_2: rc};
         switch (_v16.ctor)
         {case "_Tuple3":
            switch (_v16._0.ctor)
              {case "Err":
                 return Err(_v16._0._0);
                 case "Ok": switch (_v16._1.ctor)
                   {case "Ok":
                      switch (_v16._2.ctor)
                        {case "Ok": return Ok(A3(func,
                             _v16._0._0,
                             _v16._1._0,
                             _v16._2._0));}
                        break;}
                   break;}
              switch (_v16._1.ctor)
              {case "Err":
                 return Err(_v16._1._0);}
              switch (_v16._2.ctor)
              {case "Err":
                 return Err(_v16._2._0);}
              break;}
         _U.badCase($moduleName,
         "between lines 57 and 61");
      }();
   });
   var map4 = F5(function (func,
   ra,
   rb,
   rc,
   rd) {
      return function () {
         var _v26 = {ctor: "_Tuple4"
                    ,_0: ra
                    ,_1: rb
                    ,_2: rc
                    ,_3: rd};
         switch (_v26.ctor)
         {case "_Tuple4":
            switch (_v26._0.ctor)
              {case "Err":
                 return Err(_v26._0._0);
                 case "Ok": switch (_v26._1.ctor)
                   {case "Ok":
                      switch (_v26._2.ctor)
                        {case "Ok":
                           switch (_v26._3.ctor)
                             {case "Ok": return Ok(A4(func,
                                  _v26._0._0,
                                  _v26._1._0,
                                  _v26._2._0,
                                  _v26._3._0));}
                             break;}
                        break;}
                   break;}
              switch (_v26._1.ctor)
              {case "Err":
                 return Err(_v26._1._0);}
              switch (_v26._2.ctor)
              {case "Err":
                 return Err(_v26._2._0);}
              switch (_v26._3.ctor)
              {case "Err":
                 return Err(_v26._3._0);}
              break;}
         _U.badCase($moduleName,
         "between lines 66 and 71");
      }();
   });
   var map5 = F6(function (func,
   ra,
   rb,
   rc,
   rd,
   re) {
      return function () {
         var _v39 = {ctor: "_Tuple5"
                    ,_0: ra
                    ,_1: rb
                    ,_2: rc
                    ,_3: rd
                    ,_4: re};
         switch (_v39.ctor)
         {case "_Tuple5":
            switch (_v39._0.ctor)
              {case "Err":
                 return Err(_v39._0._0);
                 case "Ok": switch (_v39._1.ctor)
                   {case "Ok":
                      switch (_v39._2.ctor)
                        {case "Ok":
                           switch (_v39._3.ctor)
                             {case "Ok":
                                switch (_v39._4.ctor)
                                  {case "Ok": return Ok(A5(func,
                                       _v39._0._0,
                                       _v39._1._0,
                                       _v39._2._0,
                                       _v39._3._0,
                                       _v39._4._0));}
                                  break;}
                             break;}
                        break;}
                   break;}
              switch (_v39._1.ctor)
              {case "Err":
                 return Err(_v39._1._0);}
              switch (_v39._2.ctor)
              {case "Err":
                 return Err(_v39._2._0);}
              switch (_v39._3.ctor)
              {case "Err":
                 return Err(_v39._3._0);}
              switch (_v39._4.ctor)
              {case "Err":
                 return Err(_v39._4._0);}
              break;}
         _U.badCase($moduleName,
         "between lines 76 and 117");
      }();
   });
   var formatError = F2(function (f,
   result) {
      return function () {
         switch (result.ctor)
         {case "Err":
            return Err(f(result._0));
            case "Ok":
            return Ok(result._0);}
         _U.badCase($moduleName,
         "between lines 142 and 155");
      }();
   });
   var fromMaybe = F2(function (err,
   maybe) {
      return function () {
         switch (maybe.ctor)
         {case "Just":
            return Ok(maybe._0);
            case "Nothing":
            return Err(err);}
         _U.badCase($moduleName,
         "between lines 174 and 176");
      }();
   });
   _elm.Result.values = {_op: _op
                        ,Ok: Ok
                        ,Err: Err
                        ,map: map
                        ,map2: map2
                        ,map3: map3
                        ,map4: map4
                        ,map5: map5
                        ,andThen: andThen
                        ,formatError: formatError
                        ,toMaybe: toMaybe
                        ,fromMaybe: fromMaybe};
   return _elm.Result.values;
};
Elm.Serialize = Elm.Serialize || {};
Elm.Serialize.make = function (_elm) {
   "use strict";
   _elm.Serialize = _elm.Serialize || {};
   if (_elm.Serialize.values)
   return _elm.Serialize.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _P = _N.Ports.make(_elm),
   $moduleName = "Serialize",
   $Basics = Elm.Basics.make(_elm),
   $GameTypes = Elm.GameTypes.make(_elm),
   $Json$Encode = Elm.Json.Encode.make(_elm),
   $List = Elm.List.make(_elm),
   $Player = Elm.Player.make(_elm);
   var intPair = function (_v0) {
      return function () {
         switch (_v0.ctor)
         {case "_Tuple2":
            return $Json$Encode.list(A2($List.map,
              $Json$Encode.$int,
              _L.fromArray([_v0._0
                           ,_v0._1])));}
         _U.badCase($moduleName,
         "on line 32, column 18 to 40");
      }();
   };
   var player = function ($) {
      return $Json$Encode.string($Player.color($));
   };
   var deck = function ($) {
      return $Json$Encode.list($List.map($Json$Encode.string)($));
   };
   var action = function (a) {
      return function () {
         switch (a.ctor)
         {case "NoAction":
            return $Json$Encode.object(_L.fromArray([{ctor: "_Tuple2"
                                                     ,_0: "action"
                                                     ,_1: $Json$Encode.string("NoAction")}]));
            case "Pass":
            return $Json$Encode.object(_L.fromArray([{ctor: "_Tuple2"
                                                     ,_0: "action"
                                                     ,_1: $Json$Encode.string("Pass")}]));
            case "PickUpPiece":
            return $Json$Encode.object(_L.fromArray([{ctor: "_Tuple2"
                                                     ,_0: "action"
                                                     ,_1: $Json$Encode.string("PickUpPiece")}
                                                    ,{ctor: "_Tuple2"
                                                     ,_0: "player"
                                                     ,_1: player(a._0)}
                                                    ,{ctor: "_Tuple2"
                                                     ,_0: "idx"
                                                     ,_1: $Json$Encode.$int(a._1)}]));
            case "PlacePiece":
            return $Json$Encode.object(_L.fromArray([{ctor: "_Tuple2"
                                                     ,_0: "action"
                                                     ,_1: $Json$Encode.string("PlacePiece")}
                                                    ,{ctor: "_Tuple2"
                                                     ,_0: "mousePos"
                                                     ,_1: intPair(a._0)}
                                                    ,{ctor: "_Tuple2"
                                                     ,_0: "dims"
                                                     ,_1: intPair(a._1)}]));
            case "StartGame":
            return $Json$Encode.object(_L.fromArray([{ctor: "_Tuple2"
                                                     ,_0: "action"
                                                     ,_1: $Json$Encode.string("StartGame")}
                                                    ,{ctor: "_Tuple2"
                                                     ,_0: "deck"
                                                     ,_1: deck(a._1)}
                                                    ,{ctor: "_Tuple2"
                                                     ,_0: "player"
                                                     ,_1: player(a._2)}
                                                    ,{ctor: "_Tuple2"
                                                     ,_0: "playerName"
                                                     ,_1: $Json$Encode.string(a._3)}]));}
         _U.badCase($moduleName,
         "between lines 13 and 23");
      }();
   };
   _elm.Serialize.values = {_op: _op
                           ,action: action
                           ,deck: deck
                           ,player: player
                           ,intPair: intPair};
   return _elm.Serialize.values;
};
Elm.Signal = Elm.Signal || {};
Elm.Signal.make = function (_elm) {
   "use strict";
   _elm.Signal = _elm.Signal || {};
   if (_elm.Signal.values)
   return _elm.Signal.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _P = _N.Ports.make(_elm),
   $moduleName = "Signal",
   $Basics = Elm.Basics.make(_elm),
   $List = Elm.List.make(_elm),
   $Native$Signal = Elm.Native.Signal.make(_elm);
   var subscribe = $Native$Signal.subscribe;
   var send = $Native$Signal.send;
   var channel = $Native$Signal.input;
   var Message = {ctor: "Message"};
   var Channel = {ctor: "Channel"};
   _op["~"] = F2(function (sf,s) {
      return A3($Native$Signal.map2,
      F2(function (f,x) {
         return f(x);
      }),
      sf,
      s);
   });
   _op["<~"] = F2(function (f,s) {
      return A2($Native$Signal.map,
      f,
      s);
   });
   var sampleOn = $Native$Signal.sampleOn;
   var dropRepeats = $Native$Signal.dropRepeats;
   var dropIf = $Native$Signal.dropIf;
   var keepIf = $Native$Signal.keepIf;
   var keepWhen = F3(function (bs,
   def,
   sig) {
      return A2(_op["<~"],
      $Basics.snd,
      A3(keepIf,
      $Basics.fst,
      {ctor: "_Tuple2"
      ,_0: false
      ,_1: def},
      A2(_op["~"],
      A2(_op["<~"],
      F2(function (v0,v1) {
         return {ctor: "_Tuple2"
                ,_0: v0
                ,_1: v1};
      }),
      A2(sampleOn,sig,bs)),
      sig)));
   });
   var dropWhen = function (bs) {
      return keepWhen(A2(_op["<~"],
      $Basics.not,
      bs));
   };
   var merge = $Native$Signal.merge;
   var mergeMany = function (signals) {
      return A2($List.foldr1,
      merge,
      signals);
   };
   var foldp = $Native$Signal.foldp;
   var map5 = $Native$Signal.map5;
   var map4 = $Native$Signal.map4;
   var map3 = $Native$Signal.map3;
   var map2 = $Native$Signal.map2;
   var map = $Native$Signal.map;
   var constant = $Native$Signal.constant;
   var Signal = {ctor: "Signal"};
   _elm.Signal.values = {_op: _op
                        ,Signal: Signal
                        ,constant: constant
                        ,map: map
                        ,map2: map2
                        ,map3: map3
                        ,map4: map4
                        ,map5: map5
                        ,foldp: foldp
                        ,merge: merge
                        ,mergeMany: mergeMany
                        ,keepIf: keepIf
                        ,dropIf: dropIf
                        ,keepWhen: keepWhen
                        ,dropWhen: dropWhen
                        ,dropRepeats: dropRepeats
                        ,sampleOn: sampleOn
                        ,Channel: Channel
                        ,Message: Message
                        ,channel: channel
                        ,send: send
                        ,subscribe: subscribe};
   return _elm.Signal.values;
};
Elm.String = Elm.String || {};
Elm.String.make = function (_elm) {
   "use strict";
   _elm.String = _elm.String || {};
   if (_elm.String.values)
   return _elm.String.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _P = _N.Ports.make(_elm),
   $moduleName = "String",
   $Maybe = Elm.Maybe.make(_elm),
   $Native$String = Elm.Native.String.make(_elm),
   $Result = Elm.Result.make(_elm);
   var fromList = $Native$String.fromList;
   var toList = $Native$String.toList;
   var toFloat = $Native$String.toFloat;
   var toInt = $Native$String.toInt;
   var indices = $Native$String.indexes;
   var indexes = $Native$String.indexes;
   var endsWith = $Native$String.endsWith;
   var startsWith = $Native$String.startsWith;
   var contains = $Native$String.contains;
   var all = $Native$String.all;
   var any = $Native$String.any;
   var toLower = $Native$String.toLower;
   var toUpper = $Native$String.toUpper;
   var lines = $Native$String.lines;
   var words = $Native$String.words;
   var trimRight = $Native$String.trimRight;
   var trimLeft = $Native$String.trimLeft;
   var trim = $Native$String.trim;
   var padRight = $Native$String.padRight;
   var padLeft = $Native$String.padLeft;
   var pad = $Native$String.pad;
   var dropRight = $Native$String.dropRight;
   var dropLeft = $Native$String.dropLeft;
   var right = $Native$String.right;
   var left = $Native$String.left;
   var slice = $Native$String.slice;
   var repeat = $Native$String.repeat;
   var join = $Native$String.join;
   var split = $Native$String.split;
   var foldr = $Native$String.foldr;
   var foldl = $Native$String.foldl;
   var reverse = $Native$String.reverse;
   var filter = $Native$String.filter;
   var map = $Native$String.map;
   var length = $Native$String.length;
   var concat = $Native$String.concat;
   var append = $Native$String.append;
   var uncons = $Native$String.uncons;
   var cons = $Native$String.cons;
   var fromChar = function ($char) {
      return A2(cons,$char,"");
   };
   var isEmpty = $Native$String.isEmpty;
   _elm.String.values = {_op: _op
                        ,isEmpty: isEmpty
                        ,cons: cons
                        ,fromChar: fromChar
                        ,uncons: uncons
                        ,append: append
                        ,concat: concat
                        ,length: length
                        ,map: map
                        ,filter: filter
                        ,reverse: reverse
                        ,foldl: foldl
                        ,foldr: foldr
                        ,split: split
                        ,join: join
                        ,repeat: repeat
                        ,slice: slice
                        ,left: left
                        ,right: right
                        ,dropLeft: dropLeft
                        ,dropRight: dropRight
                        ,pad: pad
                        ,padLeft: padLeft
                        ,padRight: padRight
                        ,trim: trim
                        ,trimLeft: trimLeft
                        ,trimRight: trimRight
                        ,words: words
                        ,lines: lines
                        ,toUpper: toUpper
                        ,toLower: toLower
                        ,any: any
                        ,all: all
                        ,contains: contains
                        ,startsWith: startsWith
                        ,endsWith: endsWith
                        ,indexes: indexes
                        ,indices: indices
                        ,toInt: toInt
                        ,toFloat: toFloat
                        ,toList: toList
                        ,fromList: fromList};
   return _elm.String.values;
};
Elm.Text = Elm.Text || {};
Elm.Text.make = function (_elm) {
   "use strict";
   _elm.Text = _elm.Text || {};
   if (_elm.Text.values)
   return _elm.Text.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _P = _N.Ports.make(_elm),
   $moduleName = "Text",
   $Basics = Elm.Basics.make(_elm),
   $Color = Elm.Color.make(_elm),
   $Graphics$Element = Elm.Graphics.Element.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Text = Elm.Native.Text.make(_elm);
   var markdown = $Native$Text.markdown;
   var justified = $Native$Text.justified;
   var centered = $Native$Text.centered;
   var rightAligned = $Native$Text.rightAligned;
   var leftAligned = $Native$Text.leftAligned;
   var line = $Native$Text.line;
   var italic = $Native$Text.italic;
   var bold = $Native$Text.bold;
   var color = $Native$Text.color;
   var height = $Native$Text.height;
   var link = $Native$Text.link;
   var monospace = $Native$Text.monospace;
   var typeface = $Native$Text.typeface;
   var style = $Native$Text.style;
   var append = $Native$Text.append;
   var fromString = $Native$Text.fromString;
   var empty = fromString("");
   var concat = function (texts) {
      return A3($List.foldr,
      append,
      empty,
      texts);
   };
   var join = F2(function (seperator,
   texts) {
      return concat(A2($List.intersperse,
      seperator,
      texts));
   });
   var plainText = function (str) {
      return leftAligned(fromString(str));
   };
   var asText = function (value) {
      return leftAligned(monospace(fromString($Basics.toString(value))));
   };
   var defaultStyle = {_: {}
                      ,bold: false
                      ,color: $Color.black
                      ,height: $Maybe.Nothing
                      ,italic: false
                      ,line: $Maybe.Nothing
                      ,typeface: _L.fromArray([])};
   var Style = F6(function (a,
   b,
   c,
   d,
   e,
   f) {
      return {_: {}
             ,bold: d
             ,color: c
             ,height: b
             ,italic: e
             ,line: f
             ,typeface: a};
   });
   var Through = {ctor: "Through"};
   var Over = {ctor: "Over"};
   var Under = {ctor: "Under"};
   var Text = {ctor: "Text"};
   _elm.Text.values = {_op: _op
                      ,Text: Text
                      ,Under: Under
                      ,Over: Over
                      ,Through: Through
                      ,Style: Style
                      ,defaultStyle: defaultStyle
                      ,fromString: fromString
                      ,empty: empty
                      ,append: append
                      ,concat: concat
                      ,join: join
                      ,style: style
                      ,typeface: typeface
                      ,monospace: monospace
                      ,link: link
                      ,height: height
                      ,color: color
                      ,bold: bold
                      ,italic: italic
                      ,line: line
                      ,leftAligned: leftAligned
                      ,rightAligned: rightAligned
                      ,centered: centered
                      ,justified: justified
                      ,plainText: plainText
                      ,markdown: markdown
                      ,asText: asText};
   return _elm.Text.values;
};
Elm.Time = Elm.Time || {};
Elm.Time.make = function (_elm) {
   "use strict";
   _elm.Time = _elm.Time || {};
   if (_elm.Time.values)
   return _elm.Time.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _P = _N.Ports.make(_elm),
   $moduleName = "Time",
   $Basics = Elm.Basics.make(_elm),
   $Native$Time = Elm.Native.Time.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var delay = $Native$Time.delay;
   var timestamp = $Native$Time.timestamp;
   var since = F2(function (t,s) {
      return function () {
         var stop = A2($Signal.map,
         $Basics.always(-1),
         A2(delay,t,s));
         var start = A2($Signal.map,
         $Basics.always(1),
         s);
         var delaydiff = A3($Signal.foldp,
         F2(function (x,y) {
            return x + y;
         }),
         0,
         A2($Signal.merge,start,stop));
         return A2($Signal.map,
         F2(function (x,y) {
            return !_U.eq(x,y);
         })(0),
         delaydiff);
      }();
   });
   var every = $Native$Time.every;
   var fpsWhen = $Native$Time.fpsWhen;
   var fps = $Native$Time.fps;
   var inMilliseconds = function (t) {
      return t;
   };
   var millisecond = 1;
   var second = 1000 * millisecond;
   var minute = 60 * second;
   var hour = 60 * minute;
   var inHours = function (t) {
      return t / hour;
   };
   var inMinutes = function (t) {
      return t / minute;
   };
   var inSeconds = function (t) {
      return t / second;
   };
   _elm.Time.values = {_op: _op
                      ,millisecond: millisecond
                      ,second: second
                      ,minute: minute
                      ,hour: hour
                      ,inMilliseconds: inMilliseconds
                      ,inSeconds: inSeconds
                      ,inMinutes: inMinutes
                      ,inHours: inHours
                      ,fps: fps
                      ,fpsWhen: fpsWhen
                      ,every: every
                      ,since: since
                      ,timestamp: timestamp
                      ,delay: delay};
   return _elm.Time.values;
};
Elm.Transform2D = Elm.Transform2D || {};
Elm.Transform2D.make = function (_elm) {
   "use strict";
   _elm.Transform2D = _elm.Transform2D || {};
   if (_elm.Transform2D.values)
   return _elm.Transform2D.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _P = _N.Ports.make(_elm),
   $moduleName = "Transform2D",
   $Native$Transform2D = Elm.Native.Transform2D.make(_elm);
   var multiply = $Native$Transform2D.multiply;
   var rotation = $Native$Transform2D.rotation;
   var matrix = $Native$Transform2D.matrix;
   var translation = F2(function (x,
   y) {
      return A6(matrix,
      1,
      0,
      0,
      1,
      x,
      y);
   });
   var scale = function (s) {
      return A6(matrix,
      s,
      0,
      0,
      s,
      0,
      0);
   };
   var scaleX = function (x) {
      return A6(matrix,
      x,
      0,
      0,
      1,
      0,
      0);
   };
   var scaleY = function (y) {
      return A6(matrix,
      1,
      0,
      0,
      y,
      0,
      0);
   };
   var identity = $Native$Transform2D.identity;
   var Transform2D = {ctor: "Transform2D"};
   _elm.Transform2D.values = {_op: _op
                             ,identity: identity
                             ,matrix: matrix
                             ,multiply: multiply
                             ,rotation: rotation
                             ,translation: translation
                             ,scale: scale
                             ,scaleX: scaleX
                             ,scaleY: scaleY};
   return _elm.Transform2D.values;
};
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
   _P = _N.Ports.make(_elm),
   $moduleName = "Voluspa",
   $AI = Elm.AI.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Board = Elm.Board.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Deserialize = Elm.Deserialize.make(_elm),
   $Dict = Elm.Dict.make(_elm),
   $Display = Elm.Display.make(_elm),
   $GameTypes = Elm.GameTypes.make(_elm),
   $Graphics$Element = Elm.Graphics.Element.make(_elm),
   $Graphics$Input$Field = Elm.Graphics.Input.Field.make(_elm),
   $Helpers = Elm.Helpers.make(_elm),
   $Json$Decode = Elm.Json.Decode.make(_elm),
   $Json$Encode = Elm.Json.Encode.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Mouse = Elm.Mouse.make(_elm),
   $Piece = Elm.Piece.make(_elm),
   $Player = Elm.Player.make(_elm),
   $Random = Elm.Random.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Serialize = Elm.Serialize.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $Time = Elm.Time.make(_elm),
   $WebSocket = Elm.WebSocket.make(_elm),
   $Window = Elm.Window.make(_elm);
   var server = "ws://ec2-52-10-22-64.us-west-2.compute.amazonaws.com:22000";
   var startState = {_: {}
                    ,board: $Dict.empty
                    ,deck: _L.fromArray([])
                    ,delta: $Dict.fromList(_L.fromArray([{ctor: "_Tuple2"
                                                         ,_0: "red"
                                                         ,_1: ""}
                                                        ,{ctor: "_Tuple2"
                                                         ,_0: "blue"
                                                         ,_1: ""}]))
                    ,gameState: $GameTypes.NotStarted
                    ,gameType: $GameTypes.HumanVsCpu
                    ,hands: $Dict.fromList(_L.fromArray([{ctor: "_Tuple2"
                                                         ,_0: "red"
                                                         ,_1: _L.fromArray([])}
                                                        ,{ctor: "_Tuple2"
                                                         ,_0: "blue"
                                                         ,_1: _L.fromArray([])}]))
                    ,heldPiece: $Maybe.Nothing
                    ,lastPlaced: $Maybe.Nothing
                    ,log: _L.fromArray([])
                    ,playerNames: $Dict.fromList(_L.fromArray([{ctor: "_Tuple2"
                                                               ,_0: "red"
                                                               ,_1: "Player"}
                                                              ,{ctor: "_Tuple2"
                                                               ,_0: "blue"
                                                               ,_1: "CPU"}]))
                    ,players: $Dict.fromList(_L.fromArray([{ctor: "_Tuple2"
                                                           ,_0: "red"
                                                           ,_1: $GameTypes.Human}
                                                          ,{ctor: "_Tuple2"
                                                           ,_0: "blue"
                                                           ,_1: $GameTypes.Cpu}]))
                    ,score: $Dict.fromList(_L.fromArray([{ctor: "_Tuple2"
                                                         ,_0: "red"
                                                         ,_1: 0}
                                                        ,{ctor: "_Tuple2"
                                                         ,_0: "blue"
                                                         ,_1: 0}]))
                    ,turn: $GameTypes.Red};
   var deckContents = function () {
      var r = $List.repeat;
      return A2($Basics._op["++"],
      A2(r,6,"Odin"),
      A2($Basics._op["++"],
      A2(r,8,"Thor"),
      A2($Basics._op["++"],
      A2(r,6,"Troll"),
      A2($Basics._op["++"],
      A2(r,8,"Dragon"),
      A2($Basics._op["++"],
      A2(r,8,"Fenrir"),
      A2($Basics._op["++"],
      A2(r,9,"Skadi"),
      A2($Basics._op["++"],
      A2(r,9,"Valkyrie"),
      A2(r,6,"Loki"))))))));
   }();
   var constructAction = F6(function (clickType,
   seed,
   mousePos,
   dims,
   gameType,
   playerName) {
      return function () {
         var click = A2($Debug.watch,
         "clickInput.signal",
         clickType);
         var pos = A2($Debug.watch,
         "Mouse.position",
         mousePos);
         return function () {
            switch (clickType.ctor)
            {case "BoardClick":
               return A2($GameTypes.PlacePiece,
                 mousePos,
                 dims);
               case "None":
               return $GameTypes.NoAction;
               case "PassButton":
               return $GameTypes.Pass;
               case "PieceInHand":
               return A2($GameTypes.PickUpPiece,
                 clickType._0,
                 clickType._1);
               case "Start":
               return A4($GameTypes.StartGame,
                 gameType,
                 A2($Helpers.shuffle,
                 deckContents,
                 seed),
                 A2($Helpers.sample,
                 _L.fromArray([$GameTypes.Red
                              ,$GameTypes.Blue]),
                 seed),
                 playerName.string);}
            _U.badCase($moduleName,
            "between lines 243 and 250");
         }();
      }();
   });
   var processClick = function (signal) {
      return function () {
         var sampledPlayerName = $Signal.sampleOn(signal)($Signal.subscribe($Display.playerNameChannel));
         var sampledGameType = $Signal.sampleOn(signal)($Signal.subscribe($Display.gameTypeChannel));
         var sampledMouse = A2($Signal.sampleOn,
         signal,
         $Mouse.position);
         var seedSignal = A2($Signal._op["<~"],
         function ($) {
            return $Random.initialSeed($Basics.round($Basics.fst($)));
         },
         $Time.timestamp(signal));
         return A2($Signal._op["~"],
         A2($Signal._op["~"],
         A2($Signal._op["~"],
         A2($Signal._op["~"],
         A2($Signal._op["~"],
         A2($Signal._op["<~"],
         constructAction,
         signal),
         seedSignal),
         sampledMouse),
         $Window.dimensions),
         sampledGameType),
         sampledPlayerName);
      }();
   };
   var getFirstTileHandsAndDeck = function (deck) {
      return function () {
         var deckWithIndices = A3($List.map2,
         F2(function (v0,v1) {
            return {ctor: "_Tuple2"
                   ,_0: v0
                   ,_1: v1};
         }),
         _L.range(0,
         $List.length(deck) - 1),
         deck);
         var idxFirstNonTroll = $Basics.fst($List.head(A2($List.filter,
         function (_v3) {
            return function () {
               switch (_v3.ctor)
               {case "_Tuple2":
                  return $Basics.not(_U.eq(_v3._1,
                    "Troll"));}
               _U.badCase($moduleName,
               "on line 139, column 66 to 87");
            }();
         },
         deckWithIndices)));
         var firstTile = $Piece.fromString(A2($Helpers._op["!!"],
         deck,
         idxFirstNonTroll));
         var deckMinusFirstTile = A2($Helpers.without,
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
         return {ctor: "_Tuple3"
                ,_0: firstTile
                ,_1: hands
                ,_2: remainder};
      }();
   };
   var gameStarted = F4(function (deck,
   startPlayer,
   localPlayer,
   opponentName) {
      return function () {
         var $ = getFirstTileHandsAndDeck(deck),
         firstTile = $._0,
         hands = $._1,
         remainder = $._2;
         var playerNames = $Dict.fromList(_L.fromArray([{ctor: "_Tuple2"
                                                        ,_0: $Player.color(localPlayer)
                                                        ,_1: "You"}
                                                       ,{ctor: "_Tuple2"
                                                        ,_0: $Player.color($Player.next(localPlayer))
                                                        ,_1: opponentName}]));
         var players = $Dict.fromList(_L.fromArray([{ctor: "_Tuple2"
                                                    ,_0: "red"
                                                    ,_1: _U.eq(localPlayer,
                                                    $GameTypes.Red) ? $GameTypes.Human : $GameTypes.Remote}
                                                   ,{ctor: "_Tuple2"
                                                    ,_0: "blue"
                                                    ,_1: _U.eq(localPlayer,
                                                    $GameTypes.Blue) ? $GameTypes.Human : $GameTypes.Remote}]));
         return _U.replace([["gameType"
                            ,$GameTypes.HumanVsHumanRemote]
                           ,["gameState"
                            ,$GameTypes.Connected(opponentName)]
                           ,["players",players]
                           ,["playerNames",playerNames]
                           ,["hands",hands]
                           ,["deck",remainder]
                           ,["board"
                            ,A2($Dict.singleton,
                            {ctor: "_Tuple2",_0: 0,_1: 0},
                            firstTile)]
                           ,["turn",startPlayer]],
         startState);
      }();
   });
   var mustPass = function (state) {
      return A2($Player.noTilesInHand,
      state.turn,
      state);
   };
   var isGameOver = function (state) {
      return $GameTypes.isOngoing(state) && (A2($Player.noTilesInHand,
      $GameTypes.Red,
      state) && A2($Player.noTilesInHand,
      $GameTypes.Blue,
      state));
   };
   var makeMove = F2(function (move,
   state) {
      return function () {
         var existingTile = A2($Dict.get,
         move.location,
         state.board);
         var hand = A2($Player.getHand,
         state.turn,
         state);
         var handWithDrawnTile = A2($Basics._op["++"],
         A2($Helpers.without,
         move.idx,
         hand),
         $Basics.not($List.isEmpty(state.deck)) ? A2($List.take,
         1,
         state.deck) : _L.fromArray([]));
         var newHand = function () {
            switch (existingTile.ctor)
            {case "Just":
               return _U.eq(move.piece,
                 $GameTypes.Skadi) ? A3($Helpers.replaceAtIndex,
                 move.idx,
                 $Piece.toString(existingTile._0),
                 hand) : handWithDrawnTile;
               case "Nothing":
               return handWithDrawnTile;}
            _U.badCase($moduleName,
            "between lines 88 and 91");
         }();
         var newBoard = A3($Dict.insert,
         move.location,
         move.piece,
         state.board);
         var delta = A2($Board.scoreMove,
         move,
         newBoard);
         var p = $Player.color(state.turn);
         var newScore = $Maybe.withDefault(0)(A2($Dict.get,
         p,
         state.score)) + delta;
         var logText = A2($Basics._op["++"],
         $Maybe.withDefault("")(A2($Dict.get,
         $Player.color(state.turn),
         state.playerNames)),
         A2($Basics._op["++"],
         " placed a ",
         A2($Basics._op["++"],
         $Piece.toString(move.piece),
         A2($Basics._op["++"],
         " for ",
         A2($Basics._op["++"],
         $Basics.toString(delta),
         A2($Basics._op["++"],
         " points",
         A2($Basics._op["++"],
         " (total: ",
         A2($Basics._op["++"],
         $Basics.toString(newScore),
         ")"))))))));
         return _U.replace([["turn"
                            ,$Player.next(state.turn)]
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
                           ,["heldPiece",$Maybe.Nothing]
                           ,["lastPlaced"
                            ,$Maybe.Just(move.location)]
                           ,["delta"
                            ,A3($Dict.insert,
                            p,
                            A2($Basics._op["++"],
                            "(+",
                            A2($Basics._op["++"],
                            $Basics.toString(delta),
                            ")")),
                            state.delta)]
                           ,["log"
                            ,A2($List._op["::"],
                            {ctor: "_Tuple2"
                            ,_0: $Player.toColor(state.turn)
                            ,_1: logText},
                            state.log)]],
         state);
      }();
   });
   var pass = function (state) {
      return function () {
         var p = $Player.color(state.turn);
         return _U.replace([["turn"
                            ,$Player.next(state.turn)]
                           ,["delta"
                            ,A3($Dict.insert,
                            p,
                            "(+0)",
                            state.delta)]],
         state);
      }();
   };
   var tryAIMove = function (state) {
      return function () {
         var _v9 = $AI.getMove(state);
         switch (_v9.ctor)
         {case "Just": return A2(tryMove,
              _v9._0.location,
              _U.replace([["heldPiece"
                          ,$Maybe.Just(_v9._0.idx)]],
              state));
            case "Nothing":
            return pass(state);}
         _U.badCase($moduleName,
         "between lines 74 and 78");
      }();
   };
   var tryMove = F2(function (location,
   state) {
      return function () {
         var _v11 = state.heldPiece;
         switch (_v11.ctor)
         {case "Just":
            return function () {
                 var nextPlayerType = A2($Player.getType,
                 $Player.next(state.turn),
                 state);
                 var nextAction = function () {
                    switch (nextPlayerType.ctor)
                    {case "Cpu": return tryAIMove;
                       case "Human":
                       return $Basics.identity;
                       case "Remote":
                       return $Basics.identity;}
                    _U.badCase($moduleName,
                    "between lines 61 and 65");
                 }();
                 var hand = A2($Player.getHand,
                 state.turn,
                 state);
                 var pieceStr = $List.head(A2($List.drop,
                 _v11._0,
                 hand));
                 var piece = $Piece.fromString(pieceStr);
                 var move = {_: {}
                            ,idx: _v11._0
                            ,location: location
                            ,piece: piece};
                 return A2($Board.isValidMove,
                 move,
                 state.board) ? nextAction(A2(makeMove,
                 move,
                 state)) : _U.replace([["heldPiece"
                                       ,$Maybe.Nothing]],
                 state);
              }();
            case "Nothing": return state;}
         _U.badCase($moduleName,
         "between lines 54 and 71");
      }();
   });
   var startGame = F4(function (gameType,
   deck,
   player,
   playerName) {
      return function () {
         var $ = getFirstTileHandsAndDeck(deck),
         firstTile = $._0,
         hands = $._1,
         remainder = $._2;
         var playerNames = $Dict.fromList(_L.fromArray([{ctor: "_Tuple2"
                                                        ,_0: $Player.color(player)
                                                        ,_1: _U.eq(gameType,
                                                        $GameTypes.HumanVsCpu) ? "You" : $Player.color(player)}
                                                       ,{ctor: "_Tuple2"
                                                        ,_0: $Player.color($Player.next(player))
                                                        ,_1: _U.eq(gameType,
                                                        $GameTypes.HumanVsCpu) ? "CPU" : $Player.color($Player.next(player))}]));
         var players = $Dict.fromList(_L.fromArray([{ctor: "_Tuple2"
                                                    ,_0: $Player.color(player)
                                                    ,_1: $GameTypes.Human}
                                                   ,{ctor: "_Tuple2"
                                                    ,_0: $Player.color($Player.next(player))
                                                    ,_1: _U.eq(gameType,
                                                    $GameTypes.HumanVsCpu) ? $GameTypes.Cpu : $GameTypes.Human}]));
         var state = _U.eq(gameType,
         $GameTypes.HumanVsHumanRemote) ? _U.replace([["gameType"
                                                      ,gameType]
                                                     ,["gameState"
                                                      ,$GameTypes.WaitingForPlayers]
                                                     ,["players",players]
                                                     ,["playerNames"
                                                      ,playerNames]
                                                     ,["turn",player]],
         startState) : _U.replace([["gameType"
                                   ,gameType]
                                  ,["gameState"
                                   ,$GameTypes.Ongoing]
                                  ,["players",players]
                                  ,["playerNames",playerNames]
                                  ,["hands",hands]
                                  ,["deck",remainder]
                                  ,["board"
                                   ,A2($Dict.singleton,
                                   {ctor: "_Tuple2",_0: 0,_1: 0},
                                   firstTile)]
                                  ,["turn",player]],
         startState);
         return _U.eq(A2($Player.getType,
         state.turn,
         state),
         $GameTypes.Cpu) ? tryAIMove(state) : state;
      }();
   });
   var tryToPickUpPiece = F3(function (player,
   idx,
   state) {
      return _U.eq(state.turn,
      player) && $GameTypes.isOngoing(state) ? _U.replace([["heldPiece"
                                                           ,$Maybe.Just(idx)]],
      state) : state;
   });
   var performAction = F2(function (action,
   state) {
      return function () {
         var newState = function () {
            switch (action.ctor)
            {case "GameStarted":
               return A4(gameStarted,
                 action._0,
                 action._1,
                 action._2,
                 action._3);
               case "NoAction": return state;
               case "OpponentDisconnected":
               return _U.replace([["gameState"
                                  ,$GameTypes.Disconnected]],
                 state);
               case "ParseError": return state;
               case "Pass":
               return _U.replace([["turn"
                                  ,$Player.next(state.turn)]],
                 state);
               case "PickUpPiece":
               return A3(tryToPickUpPiece,
                 action._0,
                 action._1,
                 state);
               case "PlacePiece":
               return A2(tryMove,
                 A3($Display.mouseToBoardPosition,
                 action._0,
                 state,
                 action._1),
                 state);
               case "StartGame":
               return A4(startGame,
                 action._0,
                 action._1,
                 action._2,
                 action._3);}
            _U.badCase($moduleName,
            "between lines 110 and 119");
         }();
         var p = $Player.color(state.turn);
         return isGameOver(newState) ? _U.replace([["gameState"
                                                   ,$GameTypes.GameOver]],
         newState) : mustPass(newState) ? pass(newState) : newState;
      }();
   });
   var main = function () {
      var action = processClick($Signal.subscribe($Display.clickChannel));
      var actionWithGameType = A2($Signal._op["~"],
      A2($Signal._op["<~"],
      F2(function (a,t) {
         return {ctor: "_Tuple2"
                ,_0: a
                ,_1: t};
      }),
      action),
      $Signal.subscribe($Display.gameTypeChannel));
      var actionForRemote = A2($Signal._op["<~"],
      function (_v28) {
         return function () {
            switch (_v28.ctor)
            {case "_Tuple2":
               return _v28._0;}
            _U.badCase($moduleName,
            "on line 281, column 35 to 36");
         }();
      },
      A3($Signal.keepIf,
      function (_v32) {
         return function () {
            switch (_v32.ctor)
            {case "_Tuple2":
               return _U.eq(_v32._1,
                 $GameTypes.HumanVsHumanRemote);}
            _U.badCase($moduleName,
            "on line 281, column 60 to 83");
         }();
      },
      {ctor: "_Tuple2"
      ,_0: $GameTypes.NoAction
      ,_1: $GameTypes.HumanVsCpu},
      actionWithGameType));
      var decode = function (actionJson) {
         return function () {
            var _v36 = A2($Json$Decode.decodeString,
            $Deserialize.action,
            actionJson);
            switch (_v36.ctor)
            {case "Err":
               return $GameTypes.ParseError(_v36._0);
               case "Ok": return _v36._0;}
            _U.badCase($moduleName,
            "between lines 275 and 278");
         }();
      };
      var encode = function (action) {
         return A2($Json$Encode.encode,
         0,
         $Serialize.action(action));
      };
      var request = A2($Signal._op["<~"],
      $Debug.watch("request"),
      A2($Signal._op["<~"],
      encode,
      actionForRemote));
      var response = A2($Signal._op["<~"],
      $Debug.watch("response"),
      A2($WebSocket.connect,
      server,
      request));
      var responseAction = A2($Signal._op["<~"],
      $Debug.watch("deserialized"),
      A2($Signal._op["<~"],
      decode,
      response));
      var state = A3($Signal.foldp,
      performAction,
      startState,
      A2($Signal.merge,
      action,
      responseAction));
      return A2($Signal._op["~"],
      A2($Signal._op["~"],
      A2($Signal._op["~"],
      A2($Signal._op["<~"],
      $Display.render,
      state),
      $Window.dimensions),
      $Signal.subscribe($Display.gameTypeChannel)),
      $Signal.subscribe($Display.playerNameChannel));
   }();
   _elm.Voluspa.values = {_op: _op
                         ,tryToPickUpPiece: tryToPickUpPiece
                         ,pass: pass
                         ,tryMove: tryMove
                         ,tryAIMove: tryAIMove
                         ,makeMove: makeMove
                         ,performAction: performAction
                         ,isGameOver: isGameOver
                         ,mustPass: mustPass
                         ,getFirstTileHandsAndDeck: getFirstTileHandsAndDeck
                         ,startGame: startGame
                         ,gameStarted: gameStarted
                         ,deckContents: deckContents
                         ,startState: startState
                         ,constructAction: constructAction
                         ,processClick: processClick
                         ,server: server
                         ,main: main};
   return _elm.Voluspa.values;
};
Elm.WebSocket = Elm.WebSocket || {};
Elm.WebSocket.make = function (_elm) {
   "use strict";
   _elm.WebSocket = _elm.WebSocket || {};
   if (_elm.WebSocket.values)
   return _elm.WebSocket.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _P = _N.Ports.make(_elm),
   $moduleName = "WebSocket",
   $Native$WebSocket = Elm.Native.WebSocket.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var connect = $Native$WebSocket.connect;
   _elm.WebSocket.values = {_op: _op
                           ,connect: connect};
   return _elm.WebSocket.values;
};
Elm.Window = Elm.Window || {};
Elm.Window.make = function (_elm) {
   "use strict";
   _elm.Window = _elm.Window || {};
   if (_elm.Window.values)
   return _elm.Window.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _P = _N.Ports.make(_elm),
   $moduleName = "Window",
   $Native$Window = Elm.Native.Window.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var height = $Native$Window.height;
   var width = $Native$Window.width;
   var dimensions = $Native$Window.dimensions;
   _elm.Window.values = {_op: _op
                        ,dimensions: dimensions
                        ,width: width
                        ,height: height};
   return _elm.Window.values;
};
