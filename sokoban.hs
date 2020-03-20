{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

import CodeWorld        -- cabal install codeworld-api

data Activity world = Activity world (Event -> world -> world) (world -> Picture)
data StartScreenState world = StartScreen | Running world
data Tile = Wall | Ground | Storage | Box | Blank deriving Eq
data Direction = R | U | L | D
data Location = C Int Int
data State = Join Location Direction

main :: IO ()
-- main = startScreenActivityOf startState handleEvent drawState
main = runActivity (resetable (withStartScreen exercise2))

runActivity :: Activity s -> IO ()
runActivity (Activity state0 handle draw) = activityOf state0 handle draw

exercise2 :: Activity State
exercise2 = Activity startState handleEvent drawState

resetable :: Activity s -> Activity s
resetable (Activity state0 handle draw) = Activity state0 handle' draw 
    where 
        handle' (KeyPress key) _ | key == "M" = state0
        handle' e s = handle e s

withStartScreen :: Activity s -> Activity (StartScreenState s)
withStartScreen (Activity state0 handle draw) = Activity state0' handle' draw'
    where
        state0' = StartScreen
        handle' (KeyPress key) StartScreen
             | key == " "                  = Running state0
        handle' _              StartScreen = StartScreen
        handle' e              (Running s) = Running (handle e s)

        draw' StartScreen = startScreen
        draw' (Running s) = draw s


startScreen :: Picture
startScreen = scaled 3 3 (lettering "Sokoban!")

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


-- hard-code this with a change in level (think about automating this)
startState :: State
-- startState = Join (C 0 1) U  -- level 1
startState = Join (C 3 4) D  -- level 2

maze :: Location -> Tile
maze = mazeL2

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


mazeL1 :: Location -> Tile                  -- level 1
mazeL1 (C x y)
    | abs x > 4  || abs y > 4  = Blank
    | abs x == 4 || abs y == 4 = Wall
    | x ==  2 && y <= 0        = Wall
    | x ==  3 && y <= 0        = Storage
    | x >= -2 && y == 0        = Box
    | otherwise                = Ground

mazeL2 :: Location -> Tile                  -- level 2
mazeL2 (C (-5) (1)) = Wall
mazeL2 (C (-5) (2)) = Wall
mazeL2 (C (-5) (3)) = Wall
mazeL2 (C (-5) (4)) = Wall

mazeL2 (C (-4) (-5)) = Wall
mazeL2 (C (-4) (-4)) = Wall
mazeL2 (C (-4) (-3)) = Wall
mazeL2 (C (-4) (-2)) = Wall
mazeL2 (C (-4) (-1)) = Wall
mazeL2 (C (-4) (0)) = Wall
mazeL2 (C (-4) (1)) = Wall
mazeL2 (C (-4) (4)) = Wall

mazeL2 (C (-3) (1)) = Box
mazeL2 (C (-3) (-3)) = Box
mazeL2 (C (-3) (4)) = Wall
mazeL2 (C (-3) (-5)) = Wall

mazeL2 (C (-2) (0)) = Box
mazeL2 (C (-2) (-1)) = Wall
mazeL2 (C (-2) (1)) = Wall
mazeL2 (C (-2) (4)) = Wall
mazeL2 (C (-2) (-5)) = Wall

mazeL2 (C (-1) 0) = Storage
mazeL2 (C (-1) 1) = Storage
mazeL2 (C (-1) (-1)) = Storage
mazeL2 (C (-1) (2)) = Box
mazeL2 (C (-1) (4)) = Wall
mazeL2 (C (-1) (-5)) = Wall
mazeL2 (C (-1) (-4)) = Wall

mazeL2 (C 0 0) = Storage
mazeL2 (C 0 1) = Storage
mazeL2 (C (0) (-3)) = Box
mazeL2 (C (0) (2)) = Wall
mazeL2 (C (0) (-2)) = Wall
mazeL2 (C (0) (4)) = Wall
mazeL2 (C (0) (-4)) = Wall

mazeL2 (C 1  0) = Storage
mazeL2 (C 1  1) = Storage
mazeL2 (C (1) (5)) = Wall
mazeL2 (C 1 (-1)) = Storage
mazeL2 (C (1) (4)) = Wall
mazeL2 (C (1) (3)) = Box
mazeL2 (C (1) (-4)) = Wall
mazeL2 (C (1) (2)) = Wall

mazeL2 (C (2) (-1)) = Wall
mazeL2 (C (2) (-2)) = Wall
mazeL2 (C (2) (1)) = Wall
mazeL2 (C (2) (5)) = Wall
mazeL2 (C (2) (-4)) = Wall

mazeL2 (C (3) (2)) = Box
mazeL2 (C (3) (-2)) = Box
mazeL2 (C (3) (5)) = Wall
mazeL2 (C (3) (-4)) = Wall

mazeL2 (C (4) (-4)) = Wall
mazeL2 (C (4) (-1)) = Wall
mazeL2 (C (4) (0)) = Wall
mazeL2 (C (4) (1)) = Wall
mazeL2 (C (4) (2)) = Wall
mazeL2 (C (4) (3)) = Wall
mazeL2 (C (4) (5)) = Wall
mazeL2 (C (4) (4)) = Wall

mazeL2 (C (5) (-1)) = Wall
mazeL2 (C (5) (-2)) = Wall
mazeL2 (C (5) (-3)) = Wall
mazeL2 (C (5) (-4)) = Wall

mazeL2 (C (_) (_)) = Ground

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
