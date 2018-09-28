module Matrix (Matrix, Coordinate, matrixDefault, matrixLength, getMatrixLine, updateAllNW, updateAllN
updateAllNE, updateAllE, updateAllSE, updateAllS, updateAllSW, updateAllW) where

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

updateLine :: [Element] -> Int -> Int -> Int -> [Element]
updateLine (a : b) matrixLen index y | (index == y) = [((getTotalArrows a) , ((getPointingArrows a) + 1))] ++ b
								     | (index < y) = [a] ++ (iterate b matrixLen (index+1) y)

updateAllNW :: Matrix -> Int -> Coordinate -> Matrix
updateAllNW (a : b) matrixLen (x, y) | ((x > 0) && (y > 0)) = (updateLine a matrixLen 0 y) ++ (updateAllNW b matrixLen ((x-1), (y-1)))
									 | otherwise = (updateLine a matrixLen 0 y)

updateAllN :: Matrix -> Int -> Coordinate -> Matrix
updateAllN (a : b) matrixLen (x, y) 

updateAllNE :: Matrix -> Int -> Coordinate -> Matrix
updateAllNE (a : b) matrixLen (x, y)

updateAllE :: Matrix -> Int -> Coordinate -> Matrix
updateAllE (a : b) matrixLen (x, y)

updateAllSE :: Matrix -> Int -> Coordinate -> Matrix
updateAllSE (a : b) matrixLen (x, y)

updateAllS :: Matrix -> Int -> Coordinate -> Matrix
updateAllS (a : b) matrixLen (x, y)

updateAllSW :: Matrix -> Int -> Coordinate -> Matrix
updateAllSW (a : b) matrixLen (x, y)

updateAllW :: Matrix -> Int -> Coordinate -> Matrix
updateAllW (a : b) matrixLen (x, y)