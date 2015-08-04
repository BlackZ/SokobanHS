module Sokoban where

import Prelude hiding (Either(..))
import Data.List (delete, sort)

type Coord = (Int, Int)

type MovedWithCrate = Bool

data Command = Up | Down | Left | Right deriving (Show)

inv :: Command -> Command
inv Up = Down
inv Down = Up
inv Left = Right
inv Right = Left

data Level = Level {walls :: [Coord], 
                    crates :: [Coord], 
                    targets :: [Coord], 
                    player :: Coord, 
                    moves :: [(Command, MovedWithCrate)], 
                    steps :: Int,
                    maxSize :: Coord } deriving (Show)


maxCoord :: Coord -> Coord -> Coord
maxCoord (x1,y1) (x2,y2) = (max x1 x2, max y1 y2)


emptyLevel :: Level
emptyLevel = Level {walls = [], crates = [], targets = [], player =(0,0), moves = [], steps = 0, maxSize = (0,0)}


--Consumes elements of the form (coord, char) and the level
consumeElems :: [(Coord, Char)] -> Level -> Level
consumeElems [] l = l
consumeElems ((cord, char):els) l = case char of
                                            '@' -> consumeElems els l {player = cord, maxSize = updateSize l cord}
                                            '#' -> consumeElems els l {walls=cord:walls l, maxSize = updateSize l cord}
                                            '.' -> consumeElems els l {targets = cord:targets l, maxSize = updateSize l cord}
                                            '$' -> consumeElems els l {crates = cord:crates l, maxSize = updateSize l cord}
                                            '*' -> consumeElems els l {crates = cord:crates l, targets = cord:targets l, maxSize = updateSize l cord}
                                            '+' -> consumeElems els l {player = cord, targets = cord:targets l, maxSize = updateSize l cord}
                                            ' ' -> consumeElems els l {maxSize = updateSize l cord}
                                            otherwise -> error (show char ++ " not recognized")                                 
                                            where
                                                updateSize l c = maxCoord c (maxSize l)

loadLevel :: String -> Level
loadLevel s = consumeElems elems emptyLevel
            where elems = concat $ zipWith zip ([[(x,y) | x<-[0..]] | y<-[0..]]) (lines s)

--Applies a command to a coordinate
updateCoord :: Coord -> Command -> Coord
updateCoord (x,y) c = case c of
                            Up -> (x, y-1)
                            Down -> (x, y+1)
                            Left -> (x-1,y)
                            Right -> (x+1, y)

step :: Level -> Command -> Level
step l c = case testLevel of
            Nothing -> l
            Just lvl -> lvl
           where testLevel = updateLevel l c

updateLevel :: Level -> Command -> Maybe Level
updateLevel l c 
    | elem newPos (walls l) = Nothing --Hit a wall -> No valid level
    | elem newPos (crates l) = if elem newCratePos (walls l) || elem newCratePos (crates l) 
                                    then Nothing --Crate would hit wall or other crate -> No valid level
                                    else Just l{player = newPos, steps = (steps l) +1, 
                                                crates=newCratePos:(delete newPos (crates l)), 
                                                moves= (c, True):(moves l)} --Return level with updated player and crate position, by removing old crate position and adding new one
    | otherwise = Just l{player = newPos, moves = (c,False):(moves l), steps = (steps l) + 1} --create new level by updating player position, adding the move and increase step count
        where   newPos = updateCoord (player l) c
                newCratePos = updateCoord newPos c
              
stepBack :: Level -> Level
stepBack l
    | null (moves l) = l
    | otherwise = l {player = newPos, moves = tail (moves l), crates = newCrates}
                    where
                        (cmd, crateMoved) = head (moves l)
                        oldPos = player l
                        newPos = updateCoord oldPos (inv cmd)
                        oldCratePos = updateCoord oldPos cmd
                        newCrates = case crateMoved of
                                        True -> oldPos:(delete oldCratePos (crates l))
                                        False -> crates l

isSolved :: Level -> Bool
isSolved l = sort (crates l) == sort (targets l)
