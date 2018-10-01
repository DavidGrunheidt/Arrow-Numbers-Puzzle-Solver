module Borders (Borders, emptyBorders, bordersLength, getArrowFromBorder, borderToCordinate) where

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
										| ((borderIndex >= (2 * matrixLen)) && (borderIndex < (3 * matrixLen))) = ((matrixLen-1) , (abs ((mod borderIndex matrixLen) - (matrixLen-1))))
										| ((borderIndex >= (3 * matrixLen)) && (borderIndex < (4 * matrixLen))) = ((abs ((mod borderIndex matrixLen) - (matrixLen-1))), 0)
										| otherwise = (-1,-1)