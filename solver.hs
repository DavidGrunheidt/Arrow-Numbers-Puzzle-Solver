module Solver () where

import Arrows
import Borders
import Matrix
import Element

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
											   {- Passou do ultimo index das bordas, entao tem solução -}
tesAllPossibilities borders bordersIndex arrow | (bordersIndex == (bordesLength (matrixLength matrix))) = borders
											   {- Testou todas as arrows, volta uma borda via backtracking 
											      e testa a proxima arrow dessa borda anterior. -}
											   | (arrow == arrowsLength) = (tesAllPossibilities borders (bordersIndex - 1) ((getDirectionValue (getArrowFromBorders (bordersIndex - 1) borders)) + 1))
												{- Se passar nas constrains e for possivel atualizar, entao atualiza e 
											      testa recursivamente, se não, só testa recursivamente -}
											   | (checkBordersConstrains (matrixLength matrix) bordersIndex arrow) = 
											   if (checkList (checkPossibleUpdate matrix borderIndex arrow)) 
											   then (testAllPossibilities (updateBorders borders 0 bordersIndex arrow))
											   else (tesAllPossibilities borders bordersIndex (arrow + 1)) 
											   	{- Sem solução, testou todas da primeira border e voltou p/
											   	   border index -1. -}
											   | (bordersIndex == -1) = []
											    {- Não conseguiu com arrow em determinado index, testa
											       proxima arrow possivel -}
											   | otherwise = (tesAllPossibilities borders bordersIndex (arrow + 1))

checkList :: [Element] -> Bool
checkList (a : []) = if ((getTotalArrows a) <= (getPointingArrows a)) then False else True
checkList (a : b) = if ((getTotalArrows a) <= (getPointingArrows a)) then False else (checkList b)

checkPossibleUpdate :: Matrix -> Int -> Int -> [Element]
checkPossibleUpdate matrix borderIndex arrow | (arrow = 1) = 
											 | (arrow = 2) = 
											 | (arrow = 3) = 
											 | (arrow = 4) = 
											 | (arrow = 5) = 
											 | (arrow = 6) = 
											 | (arrow = 7) = 
											 | (arrow = 8) = 

updateBorders :: Borders -> Int -> Int -> Arrow -> Borders
updateBorders (a : b) index bordersIndex arrow | (index == borderIndex) = [arrow] ++ b
											   | otherwise = [a] ++ (updateBorders b (index+1) bordersIndex arrow)