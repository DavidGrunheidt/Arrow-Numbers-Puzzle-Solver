module Tester (testAllNW, testAllN, testAllNE, testAllE, testAllSE, testAllS, testAllSW, testAllW) where

import Matrix

testElement :: Matrix -> Int -> Int -> Bool
testElement matrix x y = if ((getTotalArrows (getMatrixLine matrix 0 x)!!y) < (getPointingArrows (getMatrixLine matrix 0 x)!!y)) then True else False

testAllNW :: Matrix -> Int -> Coordinate -> Bool
testAllNW matrix matrixLen (x, y) | ((x > 0) && (y > 0)) = if (testElement matrix x y) then (testAllNW matrix matrixLen ((x-1), (y-1))) else False
								  | otherwise = (testElement matrix x y)

testAllN :: Matrix -> Int -> Coordinate -> Bool
testAllN matrix matrixLen (x, y) | (y > 0) = if (testElement matrix x y) then (testAllN matrix matrixLen (x, (y-1))) else False
								 | otherwise = (testElement matrix x y)

testAllNE :: Matrix -> Int -> Coordinate -> Bool
testAllNE matrix matrixLen (x, y) | ((x < matrixLen) && (y > 0)) = if (testElement matrix x y) then (testAllNE matrix matrixLen ((x+1), (y-1))) else False
								  | otherwise = (testElement matrix x y)

testAllE :: Matrix -> Int -> Coordinate -> Bool
testAllE matrix matrixLen (x, y) | (x < matrixLen) = if (testElement matrix x y) then (testAllNE matrix matrixLen ((x+1), y)) else False
								  | otherwise = (testElement matrix x y)

testAllSE :: Matrix -> Int -> Coordinate -> Bool
testAllSE matrix matrixLen (x, y) | ((x < matrixLen) && (y < matrixLen)) = if (testElement matrix x y) then (testAllSE matrix matrixLen ((x+1), (y+1))) else False
								  | otherwise = (testElement matrix x y)

testAllS :: Matrix -> Int -> Coordinate -> Bool
testAllS matrix matrixLen (x, y) | (y < matrixLen) = if (testElement matrix x y) then (testAllS matrix matrixLen (x, (y+1))) else False
								  | otherwise = (testElement matrix x y)

testAllSW :: Matrix -> Int -> Coordinate -> Bool
testAllSW matrix matrixLen (x, y) | ((x > 0) && (y < matrixLen)) = if (testElement matrix x y) then (testAllSW matrix matrixLen ((x-1), (y+1))) else False
								  | otherwise = (testElement matrix x y)

testAllW :: Matrix -> Int -> Coordinate -> Bool
testAllW matrix matrixLen (x, y) | (x > 0) = if (testElement matrix x y) then (testAllW matrix matrixLen ((x-1), y)) else False
								  | otherwise = (testElement matrix x y)