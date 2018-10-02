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
solve :: Matrix -> Borders
solve matrix = (testAllPossibilities matrix emptyBorders 0 0)

{- Função auxiliar para verificar se uma linha da matriz foi
   resolvida, ou seja, se tem o numero maximo de flechas possiveis
   apontando para ela. -}
isSolvedAux :: [Element] -> Bool
isSolvedAux (a : []) = if ((getPointingArrows a) == (getTotalArrows a)) then True else False
isSolvedAux (a : b) = if ((getPointingArrows a) == (getTotalArrows a)) then (isSolvedAux b) else False

{- Função que verifica se cada elemento da matriz possui o numero maximo de 
   flechas apontando para ela. -}
isSolved :: Matrix -> Bool
isSolved (a : []) = (isSolvedAux a)
isSolved (a : b) = if (isSolvedAux a) then (isSolved b) else False


{- Funcao recursiva usada para iterar e testar se um
   determinado estado é aceitavel segundo os requisitos
   do problema das flechas. Retorna um array contendo
   as flechas as quais representam a solução do problema. -}
testAllPossibilities :: Matrix -> Borders -> Int -> DirectionValue -> Borders
											  		 {- Passou do ultimo index das bordas, entao tem solução -}
testAllPossibilities matrix borders borderIndex arrow | (borderIndex == (bordersLength (matrixLength matrix))) = if (isSolved matrix) then borders else emptyBorders
											   		  {- Testou todas as arrows, volta uma borda via backtracking 
											      	  e testa a proxima arrow dessa borda anterior. -}
											   		  | (arrow == arrowsLength) = emptyBorders
													  {- Se passar nas constrains e for possivel atualizar, entao atualiza e 
											      	  testa recursivamente, se não, só testa recursivamente -}
											  		  | (testBordersConstraints (matrixLength matrix) borderIndex arrow) = 
											  	 	  if (testPossibleUpdate matrix (matrixLength matrix) borderIndex arrow) 
											   			then do
											   				let mAux = (matrixUpdate matrix (matrixLength matrix) borderIndex arrow)
											   				let bUpdated = borders ++ [(getArrow arrow)]
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