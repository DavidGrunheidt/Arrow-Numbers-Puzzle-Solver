module Solver (solve) where

import Arrows
import Borders/
import Matrix
import Elements
import Tester
import Updater

{- Initial function called from main. It starts the recursive 
   function "tesAllPossibilities" with the first possible state,
   that is, when we are in the first border of the matrix (upper 
   left corner), testing the first arrow "NW". -}
solve :: Borders
solve = (testAllPossibilities matrixDefault emptyBorders 0 0)


{- Recursive brute force function with backtracking. Test 
   possibilities putting direction values on the borders and
   
{- Initial function called from main. It starts the recursive 
   function "tesAllPossibilities" with the first possible state,
   that is, when we updates the number of arrows pointed to the elements of the
   matrix. -}
testAllPossibilities :: Matrix -> Borders -> Int -> DirectionValue -> Borders
											  		 {- Passou do ultimo index das bordas, entao tem solução -}
testAllPossibilities matrix borders borderIndex arrow | (borderIndex == (bordersLength (matrixLength matrix))) = borders
											   		  {- Testou todas as arrows, volta uma borda via backtracking 
											      	  e testa a proxima arrow dessa borda anterior. -}
											   		  | (arrow == arrowsLength) = emptyBorders
													  {- Se passar nas constrains e for possivel atualizar, entao atualiza e 
											      	  testa recursivamente, se não, só testa recursivamente -}
											  		  | (testBordersConstraints (matrixLength matrix) borderIndex arrow) = 
											  	 	  if (testPossibleUpdate matrix (matrixLength matrix) borderIndex arrow) 
											   			then do
											   				let mAux = (matrixUpdate matrix (matrixLength matrix) borderIndex arrow)
											   				let bUpdated = (bordersUpdate borders 0 borderIndex (getArrow arrow))
											   				let bAux = (testAllPossibilities mAux bUpdated (borderIndex+1) 0)
											   				if (bAux == emptyBorders) 
											   					then (testAllPossibilities matrix borders borderIndex (arrow+1))
											   					else bAux 
											   		  else (testAllPossibilities matrix borders borderIndex (arrow + 1)) 
											   		  {- Sem solução, testou todas da primeira border e voltou p/
											   	   	  border index -1. -}
											   		  | (borderIndex == -1) = emptyBorders
											    	  {- Não conseguiu com arrow em determinado index, testa
											          proxima arrow possivel -}
											   		  | otherwise = (testAllPossibilities matrix borders borderIndex (arrow + 1))
{- Initial function called from main. It starts the recursive 
   function "tesAllPossibilities" with the first possible state,
   that is, when we 
{- Initial function called from main. It starts the recursive 
   function "tesAllPossibilities" with the first possible state,
   that is, when we 