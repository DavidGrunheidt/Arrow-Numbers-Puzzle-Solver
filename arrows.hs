module Arrows (Direction, DirectionValue, Arrow, getArrow, arrowsLength, getDirection, getDirectionValue) where
 
type Direction = String
type DirectionValue = Int
type Arrow = (Direction, DirectionValue)

{- List with pairs representing all possible states of an
   arrow, as all auxiliar values correlated to that state -}
arrows :: [Arrow]
arrows = [("NW",0),("N",1),("NE",2),("E",3),("SE",4),("S",5),("SW",6),("W",7)]

{- Returns the number of possible state of an arrow -}
arrowsLength :: Int
arrowsLength = 8

{- Get the pair with a string representing the direction of
   a given an the corresponding value of that direction -}
getArrow :: Int -> Arrow
getArrow index = arrows!!index

{- Auxiliar function of getArrowDirection, wich gets the
   first  element of the pair that composes an arrow. -}
getDirection :: Arrow -> Direction
getDirection (direction, _) = direction

{- Auxiliar function of getArrowDirection, wich gets the  
   second element of the pair that composes an arrow. -}
getDirectionValue :: Arrow -> DirectionValue
getDirectionValue (_, value) = value
