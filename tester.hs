module Tester (testBordersConstraints, testPossibleUpdate) where

import Matrix
import Borders
import Elements
import Arrows

{- 1º Parametro = Tamanho de uma linha da matriz
   2º Parametro = Index da borda atual a ser testada
   3º Parametro = Flecha candidata a ser posta nesta posição de borda 
   Teste inicial para determinar se uma flecha pode ser colocada em uma
   borda, ou seja, se ela, naquela borda, aponta pra dentro ou pra fora 
   da matriz.-}
testBordersConstraints :: Int -> Int -> DirectionValue -> Bool
testBordersConstraints n i d | (i == 0) = if ((d == 4) || (d == 5)) then True else False
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

{- Teste para saber se uma flecha colocada em uma determinada borda
   aponta pra algum elemento que não pode ter mais nenhuma flecha
   apontando pra ele, pois o numero de flechas apontando já é igual
   ao numero de flechas que devem apontar. Utiliza-se das funções
   auxiliares abaixo, as quais fazem a memsa coisa, porem, cada
   uma para uma flecha especifica e um tipo de percorrimento
   especifico. -}
testPossibleUpdate :: Matrix -> Int -> Int -> Int -> Bool
testPossibleUpdate matrix matrixLen borderIndex arrow | ((arrow == 0) || (arrow == 4)) = (testDiagonalRight matrix (matrixLength matrix) (isVertical borderIndex matrixLen) (equivalentCoordinate (borderToCordinate matrixLen borderIndex) arrow matrixLen borderIndex))
											      	  | ((arrow == 1) || (arrow == 5)) = (testColumn matrix (borderToCordinate matrixLen borderIndex))
											          | ((arrow == 2) || (arrow == 6)) = (testDiagonalLeft matrix (matrixLength matrix) (isVertical borderIndex matrixLen) (equivalentCoordinate (borderToCordinate matrixLen borderIndex) arrow matrixLen borderIndex))
											 	      | ((arrow == 3) || (arrow == 7)) = (testLine (matrix!!(getCordinateRow (borderToCordinate matrixLen borderIndex))))
											 	      | otherwise = False

verifyElement :: [Element] -> Int -> Bool
verifyElement line column = ((getPointingArrows (line!!column)) < (getTotalArrows (line!!column)))

testLine ::[Element] -> Bool
testLine (a : []) = if (getPointingArrows a) < (getTotalArrows a) then True else False
testLine (a : b) = if (getPointingArrows a) < (getTotalArrows a) then (testLine b) else False

testColumn :: Matrix -> Coordinate -> Bool
testColumn (a : []) (_, y) = (verifyElement a y)
testColumn (a : b) (_, y)  = if (verifyElement a y) then (testColumn b (0, y)) else False

testDiagonalRight :: Matrix -> Int -> Bool -> Coordinate -> Bool
testDiagonalRight (a : b) matrixLen isVertical (x, y) | (isVertical && ((x+1) < (matrixLen-1)) && (y < (matrixLen-1))) = if (verifyElement (b!!0) y) then (testDiagonalRight b matrixLen isVertical ((x+1), (y+1))) else False
											          | (isVertical && (((x+1) == (matrixLen-1)) || (y == (matrixLen-1)))) = (verifyElement (b!!0) y)
	                                                  | ((not isVertical) && (x < (matrixLen-1)) && ((y+1) < (matrixLen-1))) = if (verifyElement a (y+1)) then (testDiagonalRight b matrixLen isVertical ((x+1), (y+1))) else False
											          | otherwise = (verifyElement a (y+1))

testDiagonalLeft :: Matrix -> Int -> Bool -> Coordinate -> Bool
testDiagonalLeft (a : b) matrixLen isVertical (x, y) | (isVertical && ((x+1) < (matrixLen-1)) && (y > 0)) = if (verifyElement (b!!0) y) then (testDiagonalLeft b matrixLen isVertical ((x+1), (y-1))) else False
										             | (isVertical && (((x+1) == (matrixLen-1)) || (y == 0))) = (verifyElement (b!!0) y)
	                                                 | ((not isVertical) && (x < (matrixLen-1)) && ((y-1) > 0)) = if (verifyElement a (y-1)) then (testDiagonalLeft b matrixLen isVertical ((x+1), (y-1))) else False
										             | otherwise = (verifyElement a (y-1))
