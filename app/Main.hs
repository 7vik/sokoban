-- step - 1

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

import CodeWorld        -- cabal install codeworld-api

data Activity world = Activity world (Event -> world -> world) (world -> Picture)
data StartScreenState world = StartScreen | Running world
data Location = Loc Int Int deriving Eq
data Direction = RightDir | UpDir | LeftDir | DownDir deriving Eq
data ListOf a = Empty | Entry a (ListOf a) deriving Show
data Tile = Wall | Ground | Storage | Box | Blank deriving Eq
data State = St Location Direction (ListOf Location)

-- step - 2

-- main :: IO ()
-- main = drawingOf (pictureOfBoxes (boxesOfMaze maze))

pictureOfBoxes :: (ListOf Location) -> Picture
pictureOfBoxes Empty = blank
pictureOfBoxes (Entry loc cs) = atLocation loc (drawTile Box) & (pictureOfBoxes cs)

atLocation :: Location -> Picture -> Picture
atLocation (Loc x y) pic = translated (fromIntegral x) (fromIntegral y) pic

-- different squares that may occur in Sokoban
wall :: Picture
wall = (colored (dark gray) (solidRectangle 1 1))

ground :: Picture
ground = (colored (light pink) (solidRectangle 1 1))

storage :: Picture
storage = (solidCircle 0.3) & (colored (light red) (solidRectangle 1 1))

box :: Picture
box = (colored (brown) (solidRectangle 1 1))

drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = blank

startState :: State
-- startState = St (Loc 0 1) UpDir (boxesOfMaze mazeL1)  -- level 1
startState = St (Loc 3 4) DownDir (boxesOfMaze maze)  -- level 2

appendList :: ListOf a -> ListOf a -> ListOf a
appendList Empty l = l
appendList (Entry x l1) l2 = Entry x (appendList l1 l2)

boxesOfMaze :: (Location -> Tile) -> (ListOf Location)
boxesOfMaze m = foldl (appendList) Empty (map (boxesOfMazeRow m) [-10..10])

boxesOfMazeRow :: (Location -> Tile) -> Int -> (ListOf Location)
boxesOfMazeRow m x = foldl (appendList) Empty (map (boxesOfMazeRowCol m x) [-10..10])

boxesOfMazeRowCol :: (Location -> Tile) -> Int -> Int -> (ListOf Location)
boxesOfMazeRowCol m x y
    | m (Loc x y) == Box    = Entry (Loc x y) Empty
    | otherwise             = Empty

maze :: Location -> Tile
maze = mazeL2

mazeL1 :: Location -> Tile                  -- level 1
mazeL1 (Loc x y)
    | abs x > 4  || abs y > 4  = Blank
    | abs x == 4 || abs y == 4 = Wall
    | x ==  2 && y <= 0        = Wall
    | x ==  3 && y <= 0        = Storage
    | x >= -2 && y == 0        = Box
    | otherwise                = Ground


mazeL2 :: Location -> Tile                  -- level 2
mazeL2 (Loc (-5) (1)) = Wall
mazeL2 (Loc (-5) (2)) = Wall
mazeL2 (Loc (-5) (3)) = Wall
mazeL2 (Loc (-5) (4)) = Wall

mazeL2 (Loc (-4) (-5)) = Wall
mazeL2 (Loc (-4) (-4)) = Wall
mazeL2 (Loc (-4) (-3)) = Wall
mazeL2 (Loc (-4) (-2)) = Wall
mazeL2 (Loc (-4) (-1)) = Wall
mazeL2 (Loc (-4) (0)) = Wall
mazeL2 (Loc (-4) (1)) = Wall
mazeL2 (Loc (-4) (4)) = Wall

mazeL2 (Loc (-3) (1)) = Box
mazeL2 (Loc (-3) (-3)) = Box
mazeL2 (Loc (-3) (4)) = Wall
mazeL2 (Loc (-3) (-5)) = Wall

mazeL2 (Loc (-2) (0)) = Box
mazeL2 (Loc (-2) (-1)) = Wall
mazeL2 (Loc (-2) (1)) = Wall
mazeL2 (Loc (-2) (4)) = Wall
mazeL2 (Loc (-2) (-5)) = Wall

mazeL2 (Loc (-1) 0) = Storage
mazeL2 (Loc (-1) 1) = Storage
mazeL2 (Loc (-1) (-1)) = Storage
mazeL2 (Loc (-1) (2)) = Box
mazeL2 (Loc (-1) (4)) = Wall
mazeL2 (Loc (-1) (-5)) = Wall
mazeL2 (Loc (-1) (-4)) = Wall

mazeL2 (Loc 0 0) = Storage
mazeL2 (Loc 0 1) = Storage
mazeL2 (Loc (0) (-3)) = Box
mazeL2 (Loc (0) (2)) = Wall
mazeL2 (Loc (0) (-2)) = Wall
mazeL2 (Loc (0) (4)) = Wall
mazeL2 (Loc (0) (-4)) = Wall

mazeL2 (Loc 1  0) = Storage
mazeL2 (Loc 1  1) = Storage
mazeL2 (Loc (1) (5)) = Wall
mazeL2 (Loc 1 (-1)) = Storage
mazeL2 (Loc (1) (4)) = Wall
mazeL2 (Loc (1) (3)) = Box
mazeL2 (Loc (1) (-4)) = Wall
mazeL2 (Loc (1) (2)) = Wall

mazeL2 (Loc (2) (-1)) = Wall
mazeL2 (Loc (2) (-2)) = Wall
mazeL2 (Loc (2) (1)) = Wall
mazeL2 (Loc (2) (5)) = Wall
mazeL2 (Loc (2) (-4)) = Wall

mazeL2 (Loc (3) (2)) = Box
mazeL2 (Loc (3) (-2)) = Box
mazeL2 (Loc (3) (5)) = Wall
mazeL2 (Loc (3) (-4)) = Wall

mazeL2 (Loc (4) (-4)) = Wall
mazeL2 (Loc (4) (-1)) = Wall
mazeL2 (Loc (4) (0)) = Wall
mazeL2 (Loc (4) (1)) = Wall
mazeL2 (Loc (4) (2)) = Wall
mazeL2 (Loc (4) (3)) = Wall
mazeL2 (Loc (4) (5)) = Wall
mazeL2 (Loc (4) (4)) = Wall

mazeL2 (Loc (5) (-1)) = Wall
mazeL2 (Loc (5) (-2)) = Wall
mazeL2 (Loc (5) (-3)) = Wall
mazeL2 (Loc (5) (-4)) = Wall

mazeL2 (Loc (_) (_)) = Ground

-- step 3

-- main :: IO ()
-- main = drawingOf pictureOfMaze

pictureOfMaze :: Picture
pictureOfMaze = foldl (&) blank (map pictureOfMazeRow [-10..10])

pictureOfMazeRow :: Int -> Picture
pictureOfMazeRow x = foldl (&) blank (map (pictureOfMazeRowCol x) [-10..10])

pictureOfMazeRowCol :: Int -> Int -> Picture
pictureOfMazeRowCol x y = translated (fromIntegral x) (fromIntegral y) (drawTile (noBoxMaze (Loc x y)))

noBoxMaze :: Location -> Tile
noBoxMaze c = noBox (maze c)
    where
        noBox :: Tile -> Tile
        noBox Box = Ground
        noBox t   = t

mazeWithBoxes :: (ListOf Location) -> Location -> Tile
mazeWithBoxes boxes loc 
    | inList boxes loc  = Box
    | otherwise         = noBoxMaze loc

inList :: Eq a => ListOf a -> a -> Bool
inList Empty _ = False
inList (Entry a1 l1) a2 = (a1 == a2) || (inList l1 a2)

-- step 4

-- main :: IO ()
-- main = drawingOf (draw startState)

draw :: State -> Picture
draw (St loc dir boxes) = (atLocation loc (directedPlayer dir)) & (pictureOfBoxes boxes) & pictureOfMaze 

-- make player fancier later
player :: Picture
player = (colored blue (translated 0 0.3 (solidRectangle 0.1 0.3))) & (colored green (solidCircle 0.3))

directedPlayer :: Direction -> Picture
directedPlayer UpDir = rotated (0.0 * pi) player
directedPlayer LeftDir = rotated (0.5 * pi) player
directedPlayer DownDir = rotated (1.0 * pi) player
directedPlayer RightDir = rotated (1.5 * pi) player

-- step 5

handleEvent :: Event -> State -> State
handleEvent _ (St _ _ bx)
    | gameWon bx     = startState 
handleEvent (KeyPress key) s
    | key == "Right" = tryGoTo s RightDir
    | key == "Up"    = tryGoTo s UpDir
    | key == "Left"  = tryGoTo s LeftDir
    | key == "Down"  = tryGoTo s DownDir
    | key == "R"     = startState
handleEvent _ s      = s

tryGoTo :: State -> Direction -> State
tryGoTo (St from _ bx) dir
    = case currentMaze to of
        Box -> case currentMaze beyond of
            Ground -> movedState
            Storage -> movedState
            _ -> didn'tMove
        Ground -> movedState
        Storage -> movedState
        _ -> didn'tMove
    where 
        currentMaze = mazeWithBoxes bx 
        to          = adjacentLocation dir from
        beyond      = adjacentLocation dir to
        movedState  = St to dir movedBx
        movedBx     = mapList (moveFromTo to beyond) bx
        didn'tMove  = St from dir bx
        
-- step 6

play :: IO ()
play = runActivity (resetable (withStartScreen sokoban))

sokoban :: Activity State
sokoban = Activity startState handleEvent draw        
        
runActivity :: Activity s -> IO ()
runActivity (Activity state0 handle draw1) = activityOf state0 handle draw1

resetable :: Activity s -> Activity s
resetable (Activity state0 handle draw2) = Activity state0 handle' draw2 
    where 
        handle' (KeyPress key) _ | key == "M" = state0
        handle' e s = handle e s

withStartScreen :: Activity s -> Activity (StartScreenState s)
withStartScreen (Activity state0 handle draw3) = Activity state0' handle' draw'
    where
        state0' = StartScreen
        handle' (KeyPress key) StartScreen
             | key == " "                  = Running state0
        handle' _              StartScreen = StartScreen
        handle' e              (Running s) = Running (handle e s)

        draw' StartScreen = startScreen
        draw' (Running s) = draw3 s


startScreen :: Picture
startScreen = (scaled 3 3 (lettering "Sokoban!")) 
    & translated 0 (-2) (scaled 1 1 (styledLettering Italic Handwriting "Press space to begin."))
    & translated 0 (-3) (scaled 1 1 (styledLettering Italic Handwriting "Press R for reset."))
    & translated 0 (-4) (scaled 1 1 (styledLettering Italic Handwriting "Press M for menu.")) 

-- step 7

isOnStorage :: Location -> Bool
isOnStorage c = case maze c of
    Storage -> True
    _       -> False
                               
gameWon :: ListOf Location -> Bool
gameWon cs = allList (mapList isOnStorage cs)

adjacentLocation :: Direction -> Location -> Location
adjacentLocation RightDir (Loc x y) = Loc (x+1) y
adjacentLocation UpDir (Loc x y) = Loc  x   (y+1)
adjacentLocation LeftDir (Loc x y) = Loc (x-1) y
adjacentLocation DownDir (Loc x y) = Loc  x   (y-1)

moveFromTo :: Location -> Location -> Location -> Location
moveFromTo c1 c2 c 
    | c1 == c        = c2
    | otherwise      = c

mapList :: (a -> b) -> ListOf a -> ListOf b
mapList _ Empty = Empty
mapList f (Entry c cs) = Entry (f c) (mapList f cs)

combine :: ListOf Picture -> Picture
combine Empty = blank
combine (Entry p ps) = p & combine ps

allList :: ListOf Bool -> Bool
allList Empty = True
allList (Entry b bs) = b && allList bs