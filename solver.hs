module Solver () where

import Arrows
import Borders
import Matrix
import Element
import Tester

{- Initial function called from main. It starts the recursive 
   function "tesAllPossibilities" with the first possible state,
   that is, when we are in the first border of the matrix (upper 
   left corner), testing the first arrow "NW". -}
solve :: Borders
solve = (testAllPossibilities matrixDefault emptyBorders 0 0)

{- Recursive brute force function with backtracking. Test 
   possibilities putting direction values on the borders and
   updates the number of arrows pointed to the elements of the
   matrix. -}
testAllPossibilities :: Matrix -> Borders -> Int -> DirectionValue -> Borders
											  		 {- Passou do ultimo index das bordas, entao tem solução -}
tesAllPossibilities matrix borders bordersIndex arrow | (bordersIndex == (bordesLength (matrixLength matrix))) = borders
											   		  {- Testou todas as arrows, volta uma borda via backtracking 
											      	  e testa a proxima arrow dessa borda anterior. -}
											   		  | (arrow == arrowsLength) = emptyBorders
													  {- Se passar nas constrains e for possivel atualizar, entao atualiza e 
											      	  testa recursivamente, se não, só testa recursivamente -}
											  		  | (checkBordersConstrains (matrixLength matrix) bordersIndex arrow) = 
											  	 	  if (checkPossibleUpdate matrix (matrixLength matrix) borderIndex arrow) 
											   			then do
											   				let mAux = matrixUpdate
											   				let bUpdated = (updateBorders borders 0 bordersIndex arrow)
											   				let bAux = (testAllPossibilities mAux bUpdated (bordersIndex+1) 0)
											   				if (bAux == emptyBorders) 
											   					then (testAllPossibilities matrix borders bordersIndex (arrow+1))
											   					else bAux 
											   		  else (tesAllPossibilities matrix borders bordersIndex (arrow + 1)) 
											   		  {- Sem solução, testou todas da primeira border e voltou p/
											   	   	  border index -1. -}
											   		  | (bordersIndex == -1) = emptyBorders
											    	  {- Não conseguiu com arrow em determinado index, testa
											          proxima arrow possivel -}
											   		  | otherwise = (tesAllPossibilities matrix borders bordersIndex (arrow + 1))

checkPossibleUpdate :: Matrix -> Int -> Int -> Int -> [Element]
checkPossibleUpdate matrix matrixLen borderIndex arrow | (arrow = 1) = (testAllNW matrix matrixLen (borderToCordinate matrixLen bordersIndex))
											      	   | (arrow = 2) = (testAllN matrix matrixLen (borderToCordinate matrixLen bordersIndex))
											           | (arrow = 3) = (testAllNE matrix matrixLen (borderToCordinate matrixLen bordersIndex))
											 	       | (arrow = 4) = (testAllE matrix matrixLen (borderToCordinate matrixLen bordersIndex))
											 	       | (arrow = 5) = (testAllSE matrix matrixLen (borderToCordinate matrixLen bordersIndex))
											 	       | (arrow = 6) = (testAllS matrix matrixLen (borderToCordinate matrixLen bordersIndex))
											 	       | (arrow = 7) = (testAllSW matrix matrixLen (borderToCordinate matrixLen bordersIndex))
											 	       | (arrow = 8) = (testAllW matrix matrixLen (borderToCordinate matrixLen bordersIndex))

matrixUpdate :: Matrix -> Int -> Int -> Int -> Matrix
matrixUpdate matrix matrixLen borderIndex arrow | (arrow = 1) = (updateAllNW matrix matrixLen (borderToCordinate matrixLen bordersIndex))
											    | (arrow = 2) = (updateAllN matrix matrixLen (borderToCordinate matrixLen bordersIndex))
											    | (arrow = 3) = (updateAllNE matrix matrixLen (borderToCordinate matrixLen bordersIndex))
											 	| (arrow = 4) = (updateAllE matrix matrixLen (borderToCordinate matrixLen bordersIndex))
											 	| (arrow = 5) = (updateAllSE matrix matrixLen (borderToCordinate matrixLen bordersIndex))
											 	| (arrow = 6) = (updateAllS matrix matrixLen (borderToCordinate matrixLen bordersIndex))
											 	| (arrow = 7) = (updateAllSW matrix matrixLen (borderToCordinate matrixLen bordersIndex))
											 	| (arrow = 8) = (updateAllW matrix matrixLen (borderToCordinate matrixLen bordersIndex))


updateBorders :: Borders -> Int -> Int -> Arrow -> Borders
updateBorders (a : b) index bordersIndex arrow | (index == borderIndex) = [arrow] ++ b
											   | otherwise = [a] ++ (updateBorders b (index+1) bordersIndex arrow)