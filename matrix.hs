module Matrix (Matrix, Coordinate, matrixDefault, matrixLength, getMatrixLine) where

import Elements

type Matrix = [[Element]]
type Coordinate = (Int, Int)

matrixDefault :: Matrix
matrixDefault = [[(4,0),(4,0)],[(4,0),(4,0)]]

matrixLength :: Matrix -> Int
matrixLength (lx: []) = 1
matrixLength (lx : ln) = 1 + (matrixLength ln)

getMatrixLine :: Matrix -> Int -> Int -> [Element]
getMatrixLine (a : []) index line = if (index == line) then a else emptyElementList
getMatrixLine (a : b) index line = if (index == line) then a else (getMatrixLine b (index+1) line)