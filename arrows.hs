module Arrows (Direction, DirectionValue, Arrow, getArrowDirection, getArrowDirectionValue) where

type Direction = String
type DirectionValue = Int
type Arrow = (Direction, DirectionValue)

arrows :: [Arrow]
arrows = [("NW",0),("N",1),("NE",2),("E",3),("SE",4),("S",5),("SW",6),("W",7)]

getArrowDirection :: Int -> Direction
getArrowDirection index | ((index >= 0) && (index <= 7)) = (getDirection (getArrow index))
						| otherwise = "NaD" -- NaD = Not a direction

getArrowDirectionValue :: Int -> DirectionValue
getArrowDirectionValue index | ((index >= 0) && (index <= 7)) = (getDirectionValue (getArrow index))
							 | otherwise =  -1

getArrow :: Int -> Arrow
getArrow index = arrows!!index

getDirection :: Arrow -> Direction
getDirection (direction, _) = direction

getDirectionValue :: Arrow -> DirectionValue
getDirectionValue (_, value) = value
