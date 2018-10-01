module Tester (testElement, testBordersConstraints, testPossibleUpdate, testAllNW, testAllN, testAllNE, testAllE, testAllSE, testAllS, testAllSW, testAllW) where

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
   uma para uma flecha especifica. Então, não é necessario descreve-las. -}
testPossibleUpdate :: Matrix -> Int -> Int -> Int -> Bool
testPossibleUpdate matrix matrixLen borderIndex arrow | (arrow == 0) = (testAllNW matrix matrixLen (borderToCordinate matrixLen borderIndex))
											      	  | (arrow == 1) = (testAllN matrix matrixLen (borderToCordinate matrixLen borderIndex))
											          | (arrow == 2) = (testAllNE matrix matrixLen (borderToCordinate matrixLen borderIndex))
											 	      | (arrow == 3) = (testAllE matrix matrixLen (borderToCordinate matrixLen borderIndex))
											 	      | (arrow == 4) = (testAllSE matrix matrixLen (borderToCordinate matrixLen borderIndex))
											 	      | (arrow == 5) = (testAllS matrix matrixLen (borderToCordinate matrixLen borderIndex))
											 	      | (arrow == 6) = (testAllSW matrix matrixLen (borderToCordinate matrixLen borderIndex))
											 	      | (arrow == 7) = (testAllW matrix matrixLen (borderToCordinate matrixLen borderIndex))
											 	      | otherwise = False

testElement :: Matrix -> Coordinate -> Bool
testElement matrix (x,y) = if ((getTotalArrows ((getMatrixLine matrix 0 x)!!y)) > (getPointingArrows ((getMatrixLine matrix 0 x)!!y))) then True else False

testAllNW :: Matrix -> Int -> Coordinate -> Bool
testAllNW matrix matrixLen (x, y) | ((x > 0) && ((y-1) > 0)) = if (testElement matrix (x,(y-1))) then (testAllNW matrix matrixLen ((x-1), (y-1))) else False
								  | otherwise = (testElement matrix (x,(y-1)))

testAllN :: Matrix -> Int -> Coordinate -> Bool
testAllN matrix matrixLen (x, y) | (x > 0) = if (testElement matrix (x,y)) then (testAllN matrix matrixLen ((x-1), y)) else False
								 | otherwise = (testElement matrix (x,y))

testAllNE :: Matrix -> Int -> Coordinate -> Bool
testAllNE matrix matrixLen (x, y) | ((x > 0) && ((y+1) < (matrixLen-1))) = if (testElement matrix (x,(y+1))) then (testAllNE matrix matrixLen ((x-1), (y+1))) else False
								  | otherwise = (testElement matrix (x,(y+1)))

testAllE :: Matrix -> Int -> Coordinate -> Bool
testAllE matrix matrixLen (x, y) | (y < (matrixLen-1)) = if (testElement matrix (x,y)) then (testAllE matrix matrixLen (x, (y+1))) else False
								 | otherwise = (testElement matrix (x,y))

testAllSE :: Matrix -> Int -> Coordinate -> Bool
testAllSE matrix matrixLen (x, y) | ((x < (matrixLen-1)) && ((y+1) < (matrixLen-1))) = if (testElement matrix (x,(y+1))) then (testAllSE matrix matrixLen ((x+1), (y+1))) else False
								  | otherwise = (testElement matrix (x,(y+1)))

testAllS :: Matrix -> Int -> Coordinate -> Bool
testAllS matrix matrixLen (x, y) | (x < (matrixLen-1)) = if (testElement matrix (x,y)) then (testAllS matrix matrixLen ((x+1), y)) else False
								 | otherwise = (testElement matrix (x,y))

testAllSW :: Matrix -> Int -> Coordinate -> Bool
testAllSW matrix matrixLen (x, y) | ((x < (matrixLen-1)) && ((y-1) > 0)) = if (testElement matrix (x,(y-1))) then (testAllSW matrix matrixLen ((x+1), (y-1))) else False
								  | otherwise = (testElement matrix (x,(y-1)))

testAllW :: Matrix -> Int -> Coordinate -> Bool
testAllW matrix matrixLen (x, y) | (y > 0) = if (testElement matrix (x,y)) then (testAllW matrix matrixLen (x, (y-1))) else False
								 | otherwise = (testElement matrix (x,y))