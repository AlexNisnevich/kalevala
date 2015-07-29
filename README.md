# Kalevala

*Kalevala* is a tile-laying game for two players inspired by the board game _[
Völuspá](http://www.whitegoblingames.com/game/126/Vlusp)_ by Scott Caputo.

**[>>> PLAY ONLINE <<<](http://alexnisnevich.github.io/kalevala)**

## Features

- Fast-paced strategic gameplay for two players
- Cute petroglyph-inspired art
- Dynamically resizing board
- Detailed log of all moves made
- AI opponent with one-ply lookahead
- Online multiplayer mode using WebSockets (server code [here](https://github.com/neunenak/voluspa-server))
- Hotseat mode for local multiplayer
- All in only ~1500 clean, well-documented lines of [Elm](http://elm-lang.org) code

## Rules

See [the rules page](rules.md).

## Development.

First [install Elm](http://elm-lang.org/install).

Then:
```
elm compile Kalevala.elm Rules.elm
elm reactor   # to serve HTML and allow debugging
```
and go to `localhost:8000`.

## Credits

*Kalevala* was programmed by [Alex Nisnevich](http://alex.nisnevich.com) and [Greg Shuflin](http://github.everydayimshuflin.com/) with art by [Jordan Arnesen](http://byjor.com/).

It's based on the board game _[
Völuspá](http://www.whitegoblingames.com/game/126/Vlusp)_, designed by [Scott Caputo](https://boardgamegeek.com/boardgamedesigner/8862/scott-caputo) and published by [White Goblin Games](http://www.whitegoblingames.com). _Völuspá_ supports up to four players and has many expansions that add new tiles - if you like *Kalevala*, you should check it out!
