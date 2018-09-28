module Borders (Borders, emptyBorders, checkBordersConstrains, bordersLength, getArrowFromBorder, borderToCordinate) where

import Arrows
import Matrix

type Borders = [Arrow]

{- Returns an empty array with type Borders. -}
emptyBorders :: Borders
emptyBorders = []

{- Returns the number of borders based on the length
   of a matrix. -}
bordersLength :: Int -> Int
bordersLength matrixLen = 4 * matrixLen

getArrowFromBorder :: Int -> Borders -> Arrow
getArrowFromBorder index borders = borders!!index

borderToCordinate :: Int -> Int -> Coordinate
borderToCordinate matrixLen borderIndex | (borderIndex < matrixLen) = (0 , borderIndex)
										| ((borderIndex >= matrixLen) && (borderIndex < (2 * matrixLen))) = ((borderIndex - matrixLen) , (matrixLen-1))
										| ((borderIndex >= (2 * matrixLen)) && (borderIndex < (3 * matrixLen))) = ((matrixLen-1) , (borderIndex - (2*matrixLen)))
										| ((borderIndex >= (3 * matrixLen)) && (borderIndex < (4 * matrixLen))) = ((borderIndex - (3*matrixLen)) , 0)

{- 1º param. = matrix length (number of lines or columns (equal)). 
   2º param. = actual borders index beeing tested
   3º param. = Arrow that will be put on that position 
   Returns if that border position admits that arrow. -}
checkBordersConstrains :: Int -> Int -> DirectionValue -> Bool
checkBordersConstrains n i d | (i == 0) = if ((d == 4) || (d == 5)) then True else False
							 | ((i > 0) && (i < (n-1))) = if ((d >= 4) && (d <= 6)) then True else False 
					   		 | (i == n-1) = if ((d == 5) || (d == 6)) then True else False
							 | (i == n) = if ((d == 6) || (d == 7)) then True else False
							 | ((i > n) && (i < (2*n-1))) = if ((d == 0) || (d == 6) || (d == 7)) then True else False
							 | (i == (2*n-1)) = if ((d == 0) || (d == 7)) then True else False
							 | (i == (2*n)) = if ((d == 0) || (d == 1)) then True else False
							 | ((i > (2*n)) && (i < (3*n-1))) = if ((d >= 0) && (d <= 2)) then True else False
							 | (i == (3*n-1)) = if ((d == 1) || (d == 2)) then True else False
							 | (i == (3*n)) = if ((d == 2) || (d == 3)) then True else False
							 | ((i > (3*n)) && (i < (4*n-1))) = if ((d >= 2) && (d <= 4)) then True else False
							 | (i == (4*n-1)) = if ((d == 3) || (d == 4)) then True else False
							 | otherwise = False