module Matrix (Matrix, Coordinate, matrix, matrixLength) where

import Elements

type Matrix = [[Element]]
type Coordinate = (Int, Int)

matrix :: Matrix
matrix = [[(4,0),(4,0)],[(4,0),(4,0)]]

matrixLength :: Matrix -> Int
matrixLength (lx: []) = 1
matrixLength (lx : ln) = 1 + (matrixLength ln)

getAllNW :: Matrix -> Coordinate -> [Element]
getAllNW matrix (x, y) | ((x < ) && () && () && ())


