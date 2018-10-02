module Matrix (Matrix, Coordinate, matrixDefault, matrixLength, getMatrixLine, getMatrixAfterLine, getCordinateRow, equivalentCoordinate0) where

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

{- Retorna uma linha da matriz, baseando-se no index. 
   Considera-se que a primeira linha da matriz equivale 
   ao index 0, assim como todas as outras funções que trabalham
   com retorno de elementos dentro de uma lista neste programa. -}
getMatrixLine :: Matrix -> Int -> Int -> [Element]
getMatrixLine (a : []) index line = if (index == line) then a else emptyElementList
getMatrixLine (a : b) index line = if (index == line) then a else (getMatrixLine b (index+1) line)

{- Retorna uma submatriz a partir da linha indicada por index -}
getMatrixAfterLine :: Matrix -> Int -> Int -> Matrix
getMatrixAfterLine (a : b) index line | (index < line) = (getMatrixAfterLine b (index+1) line)
									  | (index == line) = b

{- Retorna a linha de uma cordenada -}
getCordinateRow :: Coordinate -> Int
getCordinateRow (a, _) = a

{- Converte uma coordenada para uma equivalente de melhor percorrimento
   (cima p/ baixo) -}
equivalentCoordinate :: Coordinate -> Int -> Int -> Int -> Coordinate
equivalentCoordinate (x, y) arrow matrixLen borderIndex | ((arrow == 0) && (borderIndex > matrixLen) && (borderIndex < (2 * matrixLen))) = (((matrixLen-1) - x),y)
														| ((arrow == 0) && (borderIndex >= (2 * matrixLen)) && (borderIndex < (3 * matrixLen))) = (0, ((matrixLen-1) - y))
														| ((arrow == 2) && (borderIndex >= (3 * matrixLen)) && (borderIndex < (4 * matrixLen))) = (0, y)
														| ((arrow == 2) && )