module Matrix (Matrix, matrix, matrixLength) where

import Elements

type Matrix = [[Element]]

matrix :: Matrix
matrix = [[(4,0),(4,0)],[(4,0),(4,0)]]

matrixLength :: Matrix -> Int
matrixLength (lx: []) = 1
matrixLength (lx : ln) = 1 + (matrixLength ln)

