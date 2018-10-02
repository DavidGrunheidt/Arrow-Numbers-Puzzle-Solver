module Updater (matrixUpdate) where

import Matrix
import Elements
import Borders
import Arrows

{- Atualiza a matriz de acordo com a flecha selecionada em uma determinada
   borda da matriz. Utilza-se das funções auxiliares descritas abaixo para
   realizar isso, porem, cada função é especifica para uma flecha. Logo, não
   é necessario descreve-las aqui. (Já está no relatorio)-}
matrixUpdate :: Matrix -> Int -> Int -> Int -> Matrix
matrixUpdate matrix matrixLen borderIndex arrow | (arrow == 0) = (updateDiagonalRight matrix matrixLen (not (isVertical borderIndex matrixLen)) (equivalentCoordinate (borderToCordinate matrixLen borderIndex) arrow matrixLen borderIndex))
											    | ((arrow == 1) || (arrow == 5)) = (updateColumn matrix (borderToCordinate matrixLen borderIndex))
											    | (arrow == 2) = (updateDiagonalLeft matrix matrixLen (not (isVertical borderIndex matrixLen)) (equivalentCoordinate (borderToCordinate matrixLen borderIndex) arrow matrixLen borderIndex))
											 	| ((arrow == 3) || (arrow == 7)) = (updateLine matrix 0 (getCordinateRow (borderToCordinate matrixLen borderIndex)))
											 	| (arrow == 4) = (updateDiagonalRight matrix matrixLen (isVertical borderIndex matrixLen) (borderToCordinate matrixLen borderIndex))
											 	| (arrow == 6) = (updateDiagonalLeft matrix matrixLen (isVertical borderIndex matrixLen) (borderToCordinate matrixLen borderIndex))
											 	| otherwise = matrix

updateColumnAux :: [Element] -> Int -> Int -> [Element]
updateColumnAux (a : []) index column = if (index == column) then [((getTotalArrows a) , ((getPointingArrows a) + 1))] else [a]
updateColumnAux (a : b) index column = if (index == column) then [((getTotalArrows a) , ((getPointingArrows a) + 1))] ++ b else [a] ++ (updateColumnAux b (index+1) column)

updateColumn :: Matrix -> Coordinate -> Matrix
updateColumn (a : []) (x,y) =  [(updateColumnAux a 0 y)]
updateColumn (a : b) (x,y) = [(updateColumnAux a 0 y)] ++ (updateColumn b ((x+1), y))

updateLineAux :: [Element] -> [Element]
updateLineAux (a : []) = [((getTotalArrows a) , ((getPointingArrows a) + 1))]
updateLineAux (a : b) = [((getTotalArrows a) , ((getPointingArrows a) + 1))] ++ (updateLineAux b)

updateLine :: Matrix -> Int -> Int -> Matrix
updateLine (a : []) index line = if (index == line) then [(updateLineAux a)] else [a] 
updateLine (a : b) index line | (index == line) = [(updateLineAux a)] ++ b
							  | (index < line) = [a] ++ (updateLine b (index+1) line)

updateDiagonalRightAux :: Matrix -> Coordinate -> Matrix
updateDiagonalRightAux (a:[]) (x,y) = [(updateColumnAux a 0 y)]
updateDiagonalRightAux (a:b) (x,y) =  [(updateColumnAux a 0 y)] ++ (updateDiagonalRightAux b ((x+1),(y+1)))

updateDiagonalRight :: Matrix -> Int -> Bool -> Coordinate -> Matrix
updateDiagonalRight (a:b) matrixLen isVertical (x, y) | (isVertical) = (getMatrixUntilLine (a:b) 0 x) ++ (updateDiagonalRightAux (getMatrixAfterLine (a:b) 0 x) (x,y))
													  | otherwise = if ((x < (matrixLen-1)) && ((y+1) < (matrixLen-1))) then [(updateColumnAux a 0 (y+1))] ++ (updateDiagonalRight b matrixLen isVertical ((x+1), (y+1))) else [(updateColumnAux a 0 (y+1))] ++ b

updateDiagonalLeftAux :: Matrix -> Coordinate -> Matrix
updateDiagonalLeftAux (a:[]) (x,y) = [(updateColumnAux a 0 y)]
updateDiagonalLeftAux (a:b) (x,y) =  [(updateColumnAux a 0 y)] ++ (updateDiagonalLeftAux b ((x+1),(y-1)))

updateDiagonalLeft :: Matrix -> Int -> Bool -> Coordinate -> Matrix
updateDiagonalLeft (a:b) matrixLen isVertical (x, y) | (isVertical) = (getMatrixUntilLine (a:b) 0 x) ++ (updateDiagonalLeftAux (getMatrixAfterLine (a:b) 0 x) (x,y))
													 | otherwise = if ((x < (matrixLen-1)) && ((y-1) > 0)) then [(updateColumnAux a 0 (y-1))] ++ (updateDiagonalLeft b matrixLen isVertical ((x+1), (y-1))) else [(updateColumnAux a 0 (y-1))] ++ b