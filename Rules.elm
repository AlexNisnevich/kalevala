module Rules where

import Markdown

main =
  Markdown.toHtml markdown


markdown = """
- - -

# Kalevala

*Kalevala* is a tile-laying game for two players inspired by the board game [
V&ouml;lusp&aacute;](http://www.whitegoblingames.com/game/126/Vlusp) by Scott Caputo.

## Placing Tiles

Players take turns placing tiles from their hand on the board. A tile must 
be placed adjacent to an existing tile, and there cannot be more than 7 tiles
in a horizontal or vertical line. 

For example, this *V&auml;in&auml;m&ouml;inen* tile can be placed anywhere above or below the existing line of tiles, but not to the left or right of them, because there are already 7 tiles in that row:

![Example illustrating length limit](https://raw.githubusercontent.com/AlexNisnevich/voluspa/master/images/Gifs/Length_Example.gif)

After you place a tile, you draw a new tile from the deck (if there are any tiles left in it).

If you can't place any tiles or you have none in your hand, your turn is passed. The game ends when neither player can place any more tiles.

## Scoring

After you place a tile, if it's the single highest-value tile in its row, you score points equal to the number of tiles in the row, and if it's the single highest-value tile in its column, you score points equal to the number of tiles in the column. You can score for a row, a column, both, or neither. Lines must have at least two tiles in them to earn points. Tying the highest value in a row or column doesn't earn you points.

For example, this *Ukko* tile scores 2 points, because it's the single highest-value tile in its column of 2 tiles, but there are no other tiles in its row (2 points for column + 0 points for row = 2 points total).

![](https://raw.githubusercontent.com/AlexNisnevich/voluspa/master/images/Gifs/7-Ukko_Example.gif)

On the other hand, this *V&auml;in&auml;m&ouml;inen* tile scores 3 points, because it's the single highest-value tile in its row of 3 tiles, but it's only tied for highest value in its column due to the other V&auml;in&auml;m&ouml;inen there (0 points for column + 3 points for row = 3 points total).

![](https://raw.githubusercontent.com/AlexNisnevich/voluspa/master/images/Gifs/8-Vain_Example.gif)

## Special Tiles

Many of the tiles in *Kalevala* have special rules associated with them.

### V&auml;in&auml;m&ouml;inen (8)

No special rules.

### Ukko (7)

No special rules.

### Kullervo (6)

No other tiles may be placed next to a *Kullervo*, unless it is another *Kullervo*.

In this example, a *Kullervo* tile is able to be be placed next to another one, but no other tile could go there:

![](https://raw.githubusercontent.com/AlexNisnevich/voluspa/master/images/Gifs/6-Kullervo_Example.gif)

### K&auml;&auml;rme (5)

*K&auml;&auml;rme* tiles may be placed on top of other tiles (except other *K&auml;&auml;rme*s). If you place a *K&auml;&auml;rme* on top of another tile, that tile is treated as though it's no longer on the board, and its special abilities no longer take effect. You can still place a *K&auml;&auml;rme* tile normally.

In this example, a *K&auml;&auml;rme* swoops in on top of a stubborn *V&auml;in&auml;m&ouml;inen* to score 4 points (2 for the column + 2 for the row):

![](https://raw.githubusercontent.com/AlexNisnevich/voluspa/master/images/Gifs/5-Kaarme_Example.gif)

### Joukahainen (4)

The value of a *Joukahainen* tile in a row or column is equal to the sum of **all** of the *Joukahainen* tiles in a row or column. This means that a *Joukahainen* tile may have a different value in its row and in its column. *Joukahainen* tiles that have an increased value in their row or column are indicated by a **4+** symbol.

*Joukahainen* tiles adjacent to *Lemminkainen* **(1)** tiles have zero value and don't contribute to the values of other *Joukahainen*s in their row and column. (More on *Lemminkainen* later!)

In this example, the *Joukahainen* tile gains a value of 8 in its row when it's placed due to the other *Joukahainen* in its row, thus winning its row for 3 points. But it only has a value of 4 in its column (because the other *Joukahainen* in its column has zero value and doesn't contribute to it), so it doesn't score any points for its column:

![](https://raw.githubusercontent.com/AlexNisnevich/voluspa/master/images/Gifs/4-Jouk_Example.gif)

### Seppo Ilmarinen (3)

*Seppo Ilmarinen* tiles may be exchanged for tiles on the board (except other *Seppo Ilmarinen*s). If you choose to do that, place a *Seppo Ilmarinen* tile where another tile is already placed, and you will receive that other tile in your hand.

You can still place a *Seppo Ilmarinen* tile normally.

In this example, a *Seppo Ilmarinen* is exchanged for a *K&auml;&auml;rme* tile on the board to both score 4 points and gain a useful *K&auml;&auml;rme* tile. Way to go, *Seppo Ilmarinen*!

![](https://raw.githubusercontent.com/AlexNisnevich/voluspa/master/images/Gifs/3-Ilmar_Example.gif)

### Louhi (2)

*Louhi* tiles can score a row or column automatically if there are *Louhi*s at both ends of it.

In this example, a *Louhi* scores a row of 4 tiles because there's another *Louhi* at the other end of the row (even though they're not the highest-value tiles in the row):

![](https://raw.githubusercontent.com/AlexNisnevich/voluspa/master/images/Gifs/2-Louhi_Example.gif)

### Lemminkainen (1)

All other tiles adjacent to *Lemminkainen* tiles have zero value (except other *Lemminkainen*s).

In this example, a *Lemminkainen* scores 4 points (2 for its row and 2 for its column) by reducing all of its neighbors to zero value:

![](https://raw.githubusercontent.com/AlexNisnevich/voluspa/master/images/Gifs/1-Lemmi_Example.gif)

## Credits

*Kalevala* was programmed by [Alex Nisnevich](http://alex.nisnevich.com) and [Greg Shuflin](http://github.everydayimshuflin.com/) with art by [Jordan Arnesen](http://byjor.com/).

It's based on the board game _[
V&ouml;lusp&aacute;](http://www.whitegoblingames.com/game/126/Vlusp)_, designed by [Scott Caputo](https://boardgamegeek.com/boardgamedesigner/8862/scott-caputo) and published by [White Goblin Games](http://www.whitegoblingames.com). _V&ouml;lusp&aacute;_ supports up to four players and has many expansions that add new tiles - if you like *Kalevala*, you should check it out!

- - -
"""