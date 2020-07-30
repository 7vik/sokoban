## Sokoban

Sokoban (倉庫番, sōko-ban, "warehouse keeper") is a puzzle video game in which the player pushes crates or boxes around in a warehouse, trying to get them to storage locations. Sokoban was created in 1981 by Hiroyuki Imabayashi.

This is an implementation of Sokoban in Haskell. 

<img src="https://github.com/7vik/sokoban/blob/master/sokoban.gif" width="600" height="400" align="middle">

## The Rules

The game is played on a board of squares, where each square is a floor or a wall. Some floor squares contain boxes and some floor squares are marked as storage locations.

The player is confined to the board and may move horizontally or vertically onto empty squares (never through walls or boxes). The player moves a box by walking up to it and pushing it to the square beyond. Boxes cannot be pulled, and they cannot be pushed to squares with walls or other boxes. The number of boxes equals the number of storage locations. The puzzle is solved when all boxes are placed at storage locations.

## Running the Game

The code, in Haskell, uses Google's CodeWorld v4.0. This can be installed using:

```
cabal install codeworld-api
```

By default, running the code through (inside app):

```
ghci -Wall Main.hs
play
``` 

will start the game locally on port 3000.

## Design

The following are the top-level definitions used in the implementation:
```
data Activity world = Activity world (Event -> world -> world) (world -> Picture)
data StartScreenState world = StartScreen | Running world
data Location = Loc Int Int deriving Eq
data Direction = RightDir | UpDir | LeftDir | DownDir deriving Eq
data ListOf a = Empty | Entry a (ListOf a) deriving Show
data Tile = Wall | Ground | Storage | Box | Blank deriving Eq
data State = St Location Direction (ListOf Location)
```

- `pictureOfBoxes`, `atLocation`, `wall`, `ground`, `storage`, `box`, `pictureOfMaze`, `pictureOfMazeRow`, `pictureOfMazeRowCol`, `noBoxMaze`, `mazeWithBoxes`, `draw`, and `drawTile` create the GUI for the game using the CodeWorld API.

- `startState`, `appendList`, `boxesOfMaze`, `boxesOfMazeRow`, `boxesOfMazeRowCol`, `maze`, `mazeL1` and `mazeL2` set up the internal state of the game.

- `handleEvent`, `runActivity` and `play` comprise of the actual gameplay loop.

- There are two levels in this game, and the level can be changed by changing [maze](https://github.com/7vik/sokoban/blob/216db2d62f3a5a041ea32785851b8c23aac0d6d6/app/Main.hs#L61) to `mazeL2` or `mazeL1`.

- The code uses classic functional idioms like HOFs, Polymorphism, Currying, Maps, IO and Folds but does not require use of Applicatives. 
