{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

import CodeWorld        -- cabal install codeworld-api

main :: IO ()
main = resetableActivityOf startState handleEvent drawState

resetableActivityOf :: world -> (Event -> world -> world) -> (world -> Picture) -> IO ()
resetableActivityOf = activityOf

handleEvent :: Event -> State -> State
handleEvent (KeyPress key) c
    | key == "Right" = adjacentLocation R c
    | key == "Up"    = adjacentLocation U c
    | key == "Left"  = adjacentLocation L c
    | key == "Down"  = adjacentLocation D c
    | key == "R"   = startState            -- reset the game
handleEvent _ c      = c

drawState :: State -> Picture
drawState (Join l d) =  (atLocation l (directedPlayer d)) & pictureOfMaze

data Tile = Wall | Ground | Storage | Box | Blank deriving Eq
data Direction = R | U | L | D
data Location = C Int Int
data State = Join Location Direction

-- hard-code this with a change in level (think about automating this)
startState :: State
startState = Join (C 0 1) U

atLocation :: Location -> Picture -> Picture
atLocation (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic

adjacentLocation :: Direction -> State -> State
adjacentLocation R (Join (C x y) _) 
    | ((maze (C  (x+1)   y)) == Ground)  =  (Join (C  (x+1)   y) R) 
    | (maze (C  (x+1)   y)) == Storage =  (Join (C  (x+1)   y) R)
    | otherwise = (Join (C x y) R)
adjacentLocation U (Join (C x y) _)
    | (maze (C  (x)   (y+1))) == Ground  =  (Join (C  (x)   (y+1) ) U)
    | (maze (C  (x)   (y+1))) == Storage =  (Join (C  (x)   (y+1) ) U)
    | otherwise = (Join (C x y) U)
adjacentLocation L (Join (C x y) _)
    | (maze (C  (x-1)   y)) == Ground  =    (Join (C  (x-1)   y ) L)
    | (maze (C  (x-1)   y)) == Storage =    (Join (C  (x-1)   y ) L)
    | otherwise = (Join (C x y) L)
adjacentLocation D (Join (C x y)  _)
    | (maze (C  (x)   (y-1))) == Ground  =  (Join (C  (x)   (y-1) ) D)
    | (maze (C  (x)   (y-1))) == Storage =  (Join (C  (x)   (y-1) ) D)
    | otherwise = (Join (C x y) D)

-- different squares that may occur in Sokoban
wall :: Picture
wall = (colored (dark gray) (solidRectangle 1 1))

ground :: Picture
ground = (colored (yellow) (solidRectangle 1 1))

storage :: Picture
storage = (solidCircle 0.3) & (colored (yellow) (solidRectangle 1 1))

box :: Picture
box = (colored (brown) (solidRectangle 1 1))

drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = blank

maze :: Location -> Tile
maze (C x y)
    | abs x > 4  || abs y > 4  = Blank
    | abs x == 4 || abs y == 4 = Wall
    | x ==  2 && y <= 0        = Wall
    | x ==  3 && y <= 0        = Storage
    | x >= -2 && y == 0        = Box
    | otherwise                = Ground

pictureOfMaze :: Picture
pictureOfMaze = foldl (&) blank (map pictureOfMazeRow [-10..10])

pictureOfMazeRow :: Int -> Picture
pictureOfMazeRow x = foldl (&) blank (map (pictureOfMazeRowCol x) [-10..10])

pictureOfMazeRowCol :: Int -> Int -> Picture
pictureOfMazeRowCol x y = translated (fromIntegral x) (fromIntegral y) (drawTile (maze (C x y)))

-- make player fancier later
player :: Picture
player = (colored green (translated 0 0.3 (solidRectangle 0.1 0.3))) & (colored pink (solidCircle 0.3))

directedPlayer :: Direction -> Picture
directedPlayer U = rotated (0.0 * pi) player
directedPlayer L = rotated (0.5 * pi) player
directedPlayer D = rotated (1.0 * pi) player
directedPlayer R = rotated (1.5 * pi) player
