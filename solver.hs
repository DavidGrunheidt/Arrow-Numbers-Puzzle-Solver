module Solver () where

import Arrows
import Borders
import Matrix

{- Initial function called from main. It starts the recursive 
   function "tesAllPossibilities" with the first possible state,
   that is, when we are in the first border of the matrix (upper 
   left corner), testing the first arrow "NW". -}
solve :: Borders
solve = (testAllPossibilities emptyBorders 0 0)

{- Recursive brute force function with backtracking. Test 
   possibilities putting direction values on the borders and
   updates the number of arrows pointed to the elements of the
   matrix. -}
testAllPossibilities :: Borders -> Int -> DirectionValue -> Borders
tesAllPossibilities borders bordersIndex arrow | (bordersIndex == bordesLength) = -- end solving 
											   | (arrow == arrowsLength) = -- backtracking
											   | (checkBordersConstrains (matrixLength matrix) bordersIndex arrow) = -- Update matrix
											   | otherwise = (tesAllPossibilities borders bordersIndex (arrow + 1))