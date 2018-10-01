module Solver (solve) where

import Arrows
import Borders
import Matrix
import Elements
import Tester
import Updater

{- Função inicial chamada do main. Inicia o algoritmo
   recursivo de tentativas descrito na funcao 
   testAllPossibilities passando como argumentos
   uma matrix, um array que representa as "bordas", no
   inicio vazio, o index do array de bordas inicial, "0"
   e a primeira flecha a ser testada, a "0" representando a
   "NW". -}
solve :: Borders
solve = (testAllPossibilities matrixDefault emptyBorders 0 0)


{- Funcao recursiva usada para iterar e testar se um
   determinado estado é aceitavel segundo os requisitos
   do problema das flechas. Retorna um array contendo
   as flechas as quais representam a solução do problema. -}
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