## Sokoban

Sokoban (倉庫番, sōko-ban, "warehouse keeper") is a puzzle video game in which the player pushes crates or boxes around in a warehouse, trying to get them to storage locations. Sokoban was created in 1981 by Hiroyuki Imabayashi.

This is an implementation of Sokoban in Haskell. 

## The Rules

The game is played on a board of squares, where each square is a floor or a wall. Some floor squares contain boxes and some floor squares are marked as storage locations.

The player is confined to the board and may move horizontally or vertically onto empty squares (never through walls or boxes). The player moves a box by walking up to it and pushing it to the square beyond. Boxes cannot be pulled, and they cannot be pushed to squares with walls or other boxes. The number of boxes equals the number of storage locations. The puzzle is solved when all boxes are placed at storage locations.

## Running the Game

The code, in Haskell, has a dependancy on CodeWorld v4.0. This can be installed using:

```
cabal install codeworld-api
```

By default, running the code through:

```
ghci -Wall sokoban.hs
play
``` 

will start the game locally on port 3000.
