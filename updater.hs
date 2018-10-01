module Updater (matrixUpdate, bordersUpdate) where

import Matrix
import Elements
import Borders
import Arrows

matrixUpdate :: Matrix -> Int -> Int -> Int -> Matrix
matrixUpdate matrix matrixLen borderIndex arrow | (arrow == 0) = (updateAllNW matrix matrixLen (borderToCordinate matrixLen borderIndex))
											    | (arrow == 1) = (updateAllN matrix matrixLen (borderToCordinate matrixLen borderIndex))
											    | (arrow == 2) = (updateAllNE matrix matrixLen (borderToCordinate matrixLen borderIndex))
											 	| (arrow == 3) = (updateOneLine matrix matrixLen 0 (borderToCordinate matrixLen borderIndex))
											 	| (arrow == 4) = (updateAllSE matrix matrixLen (borderToCordinate matrixLen borderIndex))
											 	| (arrow == 5) = (updateAllS matrix matrixLen (borderToCordinate matrixLen borderIndex))
											 	| (arrow == 6) = (updateAllSW matrix matrixLen (borderToCordinate matrixLen borderIndex))
											 	| (arrow == 7) = (updateOneLine matrix matrixLen 0 (borderToCordinate matrixLen borderIndex))
											 	| otherwise = matrix


bordersUpdate :: Borders -> Int -> Int -> Arrow -> Borders
bordersUpdate (a : b) index borderIndex arrow | (index == borderIndex) = [arrow] ++ b
											  | otherwise = [a] ++ (bordersUpdate b (index+1) borderIndex arrow)

updateLine :: [Element] -> Int -> Int -> Int -> [Element]
updateLine (a : []) matrixLen index y = if (index == y) then  [((getTotalArrows a) , ((getPointingArrows a) + 1))] else [a] 
updateLine (a : b) matrixLen index y = if (index == y) then [((getTotalArrows a) , ((getPointingArrows a) + 1))] ++ b else [a] ++ (updateLine b matrixLen (index+1) y)

updateAllLine :: [Element] -> [Element]
updateAllLine (a : []) = [((getTotalArrows a) , ((getPointingArrows a) + 1))]
updateAllLine (a : b) = [((getTotalArrows a) , ((getPointingArrows a) + 1))] ++ (updateAllLine b)

updateAllNW :: Matrix -> Int -> Coordinate -> Matrix
updateAllNW (a : b) matrixLen (x, y) | ((x > 0) && ((y-1) > 0)) = [(updateLine a matrixLen 0 (y-1))] ++ (updateAllNW b matrixLen ((x-1), (y-1)))
									 | otherwise = [(updateLine a matrixLen 0 (y-1))]

updateAllN :: Matrix -> Int -> Coordinate -> Matrix
updateAllN (a : b) matrixLen (x, y) | (x > 0) = [(updateLine a matrixLen 0 y)] ++ (updateAllN b matrixLen ((x-1), y))
									| otherwise = [(updateLine a matrixLen 0 y)]

updateAllNE :: Matrix -> Int -> Coordinate -> Matrix
updateAllNE (a : b) matrixLen (x, y) | ((x > 0) && ((y+1) < (matrixLen-1))) = [(updateLine a matrixLen 0 (y+1))] ++ (updateAllNE b matrixLen ((x-1), (y+1)))
									 | otherwise = [(updateLine a matrixLen 0 (y+1))]

updateOneLine :: Matrix -> Int -> Int -> Coordinate -> Matrix
updateOneLine (a : b) matrixLen index (x, y) | (index == x) = [(updateAllLine a)] ++ b
										     | otherwise = [a] ++ (updateOneLine b matrixLen (index+1) (x,y))

updateAllSE :: Matrix -> Int -> Coordinate -> Matrix
updateAllSE (a : b) matrixLen (x, y) | ((x < (matrixLen-1)) && ((y+1) < (matrixLen-1))) = [(updateLine a matrixLen 0 (y+1))] ++ (updateAllSE b matrixLen ((x+1), (y+1)))
									 | otherwise = [(updateLine a matrixLen 0 (y+1))]

updateAllS :: Matrix -> Int -> Coordinate -> Matrix
updateAllS (a : b) matrixLen (x, y) | (x < (matrixLen-1)) = [(updateLine a matrixLen 0 y)] ++ (updateAllS b matrixLen ((x+1), y))
									| otherwise = [(updateLine a matrixLen 0 y)]

updateAllSW :: Matrix -> Int -> Coordinate -> Matrix
updateAllSW (a : b) matrixLen (x, y) | ((x < (matrixLen-1)) && ((y-1) > 0)) = [(updateLine a matrixLen 0 (y-1))] ++ (updateAllSW b matrixLen ((x+1), (y-1)))
									 | otherwise = [(updateLine a matrixLen 0 (y-1))]