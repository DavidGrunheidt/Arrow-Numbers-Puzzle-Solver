module Updater (matrixUpdate, bordersUpdate) where

import Matrix
import Elements
import Borders
import Arrows

{- Atualiza a matriz de acordo com a flecha selecionada em uma determinada
   borda da matriz. Utilza-se das funções auxiliares descritas abaixo para
   realizar isso, porem, cada função é especifica para uma flecha. Logo, não
   é necessario descreve-las aqui. (Já está no relatorio)-}
matrixUpdate :: Matrix -> Int -> Int -> Int -> Matrix
matrixUpdate matrix matrixLen borderIndex arrow | (arrow == 0) = (updateAllNW matrix matrixLen (isHorizontal borderIndex matrixLen) (borderToCordinate matrixLen borderIndex))
											    | (arrow == 1) = (updateAllN matrix matrixLen (borderToCordinate matrixLen borderIndex))
											    | (arrow == 2) = (updateAllNE matrix matrixLen (isHorizontal borderIndex matrixLen) (borderToCordinate matrixLen borderIndex))
											 	| (arrow == 3) = (updateOneLine matrix matrixLen 0 (borderToCordinate matrixLen borderIndex))
											 	| (arrow == 4) = (updateAllSE matrix matrixLen (isHorizontal borderIndex matrixLen) (borderToCordinate matrixLen borderIndex))
											 	| (arrow == 5) = (updateAllS matrix matrixLen (borderToCordinate matrixLen borderIndex))
											 	| (arrow == 6) = (updateAllSW matrix matrixLen (isHorizontal borderIndex matrixLen) (borderToCordinate matrixLen borderIndex))
											 	| (arrow == 7) = (updateOneLine matrix matrixLen 0 (borderToCordinate matrixLen borderIndex))
											 	| otherwise = matrix


{- Atualiza o array de bordas adicionando uma nova flecha em uma posição. -}
bordersUpdate :: Borders -> Int -> Int -> Arrow -> Borders
bordersUpdate (a : b) index borderIndex arrow | (index == borderIndex) = [arrow] ++ b
											  | otherwise = [a] ++ (bordersUpdate b (index+1) borderIndex arrow)

updateLine :: [Element] -> Int -> Int -> Int -> [Element]
updateLine (a : []) matrixLen index y = if (index == y) then  [((getTotalArrows a) , ((getPointingArrows a) + 1))] else [a] 
updateLine (a : b) matrixLen index y = if (index == y) then [((getTotalArrows a) , ((getPointingArrows a) + 1))] ++ b else [a] ++ (updateLine b matrixLen (index+1) y)

updateAllLine :: [Element] -> [Element]
updateAllLine (a : []) = [((getTotalArrows a) , ((getPointingArrows a) + 1))]
updateAllLine (a : b) = [((getTotalArrows a) , ((getPointingArrows a) + 1))] ++ (updateAllLine b)

updateAllN :: Matrix -> Int -> Coordinate -> Matrix
updateAllN (a : b) matrixLen (x, y) | (x > 0) = [(updateLine a matrixLen 0 y)] ++ (updateAllN b matrixLen ((x-1), y))
									| otherwise = [(updateLine a matrixLen 0 y)]

updateOneLine :: Matrix -> Int -> Int -> Coordinate -> Matrix
updateOneLine (a : b) matrixLen index (x, y) | (index == x) = [(updateAllLine a)] ++ b
										     | otherwise = [a] ++ (updateOneLine b matrixLen (index+1) (x,y))

updateAllS :: Matrix -> Int -> Coordinate -> Matrix
updateAllS (a : b) matrixLen (x, y) | (x < (matrixLen-1)) = [(updateLine a matrixLen 0 y)] ++ (updateAllS b matrixLen ((x+1), y))
									| otherwise = [(updateLine a matrixLen 0 y)]

updateAllNW :: Matrix -> Int -> Bool -> Coordinate -> Matrix
updateAllNW (a : b) matrixLen isHorizontal (x, y) | (isHorizontal && (x > 0) && ((y-1) > 0)) = [(updateLine a matrixLen 0 (y-1))] ++ (updateAllNW b matrixLen isHorizontal ((x-1), (y-1)))
												  | (not isHorizontal) = (updateAllNWVertical (a:b) matrixLen ((x-1), y)) ++ (getMatrixAfterLine (a:b) 0 x)
									 			  | otherwise = [(updateLine a matrixLen 0 (y-1))]
									 			  
updateAllNWVertical :: Matrix -> Int -> Coordinate -> Matrix
updateAllNWVertical (a : b) matrixLen (x, y) | (((x-1) > 0) && ((y-1) > 0)) = [(updateLine (getMatrixLine (a:b) 0 (x-1)) matrixLen 0 y)] ++ (updateAllNWVertical (a:b) matrixLen ((x-1), (y-1)))
											 | otherwise = [(updateLine (getMatrixLine (a:b) 0 (x-1)) matrixLen 0 y)]


updateAllNE :: Matrix -> Int -> Bool -> Coordinate -> Matrix
updateAllNE (a : b) matrixLen isHorizontal (x, y) | (isHorizontal && (x > 0) && ((y+1) < (matrixLen-1))) = [(updateLine a matrixLen 0 (y+1))] ++ (updateAllNE b matrixLen isHorizontal ((x-1), (y+1)))
												  
												  | otherwise = [(updateLine a matrixLen 0 (y+1))]
									 
updateAllSE :: Matrix -> Int -> Bool -> Coordinate -> Matrix
updateAllSE (a : b) matrixLen isHorizontal (x, y) | (isHorizontal && (x < (matrixLen-1)) && ((y+1) < (matrixLen-1))) = [(updateLine a matrixLen 0 (y+1))] ++ (updateAllSE b matrixLen isHorizontal ((x+1), (y+1)))
									 			  
									 			  | otherwise = [(updateLine a matrixLen 0 (y+1))]

updateAllSW :: Matrix -> Int -> Bool -> Coordinate -> Matrix
updateAllSW (a : b) matrixLen isHorizontal (x, y) | (isHorizontal && (x < (matrixLen-1)) && ((y-1) > 0)) = [(updateLine a matrixLen 0 (y-1))] ++ (updateAllSW b matrixLen isHorizontal ((x+1), (y-1)))
									 			  
									 			  | otherwise = [(updateLine a matrixLen 0 (y-1))]