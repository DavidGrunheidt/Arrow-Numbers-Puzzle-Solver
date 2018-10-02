module Matrix (Matrix, Coordinate, matrixDefault, matrixLength, getCordinateRow, equivalentCoordinate, getMatrixUntilLine, getMatrixAfterLine) where

import Elements

type Matrix = [[Element]]
type Coordinate = (Int, Int)

{- Retorna uma matriz default utilizada para os testes
   do funcionamento do programa. -}
matrixDefault :: Matrix
matrixDefault = [[(4,0),(4,0)],[(4,0),(4,0)]]

{- Retorna o tamanho de uma linha de uma matriz. -}
matrixLength :: Matrix -> Int
matrixLength (lx: []) = 1
matrixLength (lx : ln) = 1 + (matrixLength ln)

{- Retorna a linha de uma cordenada -}
getCordinateRow :: Coordinate -> Int
getCordinateRow (a, _) = a

{- Converte uma coordenada para uma equivalente de melhor percorrimento
   (cima p/ baixo) -}
equivalentCoordinate :: Coordinate -> Int -> Int -> Int -> Coordinate
equivalentCoordinate (x, y) arrow matrixLen borderIndex | ((arrow == 0) && (borderIndex > matrixLen) && (borderIndex < (2 * matrixLen))) = (0, ((matrixLen-1) - x))
														| ((arrow == 0) && (borderIndex >= (2 * matrixLen)) && (borderIndex < (3 * matrixLen))) = (((matrixLen-1) - y), 0)
														| ((arrow == 2) && (borderIndex >= (2 * matrixLen)) && (borderIndex < (3 * matrixLen))) = (y, (matrixLen-1))
														| ((arrow == 2) && (borderIndex >= (3 * matrixLen)) && (borderIndex < (4 * matrixLen))) = (0, x)
														| otherwise = (x,y)

getMatrixUntilLine :: Matrix -> Int -> Int -> Matrix
getMatrixUntilLine (a : []) _ _ = [a]
getMatrixUntilLine (a : b) index line | (index < line) = [a] ++ (getMatrixUntilLine b (index+1) line)
						   			  | (index == line) = [a]
						   			  | (index > line) = []

getMatrixAfterLine :: Matrix -> Int -> Int -> Matrix
getMatrixAfterLine (a : []) _ _ = [a]
getMatrixAfterLine (a : b) index line | (index > line) = (a:b)
									  | (index == line) = b
									  | otherwise = (getMatrixAfterLine b (index+1) line)