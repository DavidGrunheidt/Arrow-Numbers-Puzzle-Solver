module Main (main) where

import Solver
import Matrix 
import Borders 
import Arrows
import Updater
import Tester 
import Elements

revList :: Borders -> Borders
revList [] = []
revList (x:xs) = (revList xs) ++ [x]

getBorder :: Borders -> Int -> Int -> Int -> Borders
getBorder (a : b) side borderIndex matrixLen | ((side == 0) && (borderIndex < matrixLen)) = if ((borderIndex + 1) < matrixLen) then [a] ++ (getBorder b side (borderIndex + 1) matrixLen) else [a]
										     | ((side == 1) && (borderIndex >= matrixLen) && (borderIndex < (2 * matrixLen))) = if ((borderIndex + 1) < (2 * matrixLen)) then [a] ++ (getBorder b side (borderIndex + 1) matrixLen) else [a]
									         | ((side == 2) && (borderIndex >= (2 * matrixLen)) && (borderIndex < (3 * matrixLen))) = if ((borderIndex + 1) < (3 * matrixLen)) then [a] ++ (getBorder b side (borderIndex + 1) matrixLen) else [a]
						   	                 | ((side == 3) && (borderIndex >= (3 * matrixLen)) && (borderIndex < (4 * matrixLen))) = if ((borderIndex + 1) < (4 * matrixLen)) then [a] ++ (getBorder b side (borderIndex + 1) matrixLen) else [a]
						   	                 | otherwise = (getBorder b side (borderIndex + 1) matrixLen)
wasSolved :: Borders -> Bool
wasSolved borders = if (null borders) then False else True

matrixToString :: Matrix -> String
matrixToString (a : []) = (show a) ++ "\n"
matrixToString (a : b) = (show a) ++ "\n" ++ (matrixToString b)

printResult :: Matrix -> Borders -> IO ()
printResult matrix borders = do 
						putStrLn ("Matriz = \n" ++ (matrixToString matrix))
						if (wasSolved borders) then do 
							putStrLn ("Resolvido! Flechas nas bordas: (cima p/ baixo ou esquerda p/ direita)")
							putStrLn ("Flechas de cima = " ++ (show (getBorder borders 0 0 (matrixLength matrix))))
							putStrLn ("Flechas da direita = " ++ (show (getBorder borders 1 0 (matrixLength matrix))))
							putStrLn ("Flechas de baixo = " ++ (show (revList (getBorder borders 2 0 (matrixLength matrix)))))
							putStrLn ("Flechas da direita = " ++ (show (revList (getBorder borders 3 0 (matrixLength matrix)))))
						else putStrLn ("Sem solução")
main = do
	-- Instancia 1
	putStrLn("\nInstancia 1:")
	let matrix = [[(5,0),(2,0),(5,0)],
				  [(3,0),(0,0),(1,0)],
				  [(4,0),(3,0),(4,0)]]
	let borders = (solve matrix)
	(printResult matrix borders)

	-- Instancia 2
	putStrLn("\nInstancia 2:")
	let matrix = [[(0,0),(0,0)],
				  [(0,0),(0,0)]]
	let borders = (solve matrix)
	(printResult matrix borders)

	-- Instancia 3
	putStrLn("\nInstancia 3:")
	let matrix = [[(4,0),(3,0),(3,0),(0,0)],
				  [(7,0),(3,0),(3,0),(2,0)],
				  [(5,0),(3,0),(3,0),(2,0)],
				  [(3,0),(1,0),(3,0),(0,0)]]
	let borders = (solve matrix)
	(printResult matrix borders)

	-- Instancia 6
	putStrLn("\nInstancia 6:")
	let matrix = [[(4,0),(4,0)],
				  [(4,0),(4,0)]]
	let borders = (solve matrix)
	(printResult matrix borders)

	-- Instancia 7
	putStrLn("\nInstancia 7:")
	let matrix = [[(3,0),(5,0)],
				  [(3,0),(4,0)]]
	let borders = (solve matrix)
	(printResult matrix borders)

	-- Instancia 8
	putStrLn("\nInstancia 8:")
	let matrix = [[(0,0),(3,0)],
				  [(3,0),(6,0)]]
	let borders = (solve matrix)
	(printResult matrix borders)

	-- Teste meu 1
	putStrLn("\nTeste meu 1:")
	let matrix = [[(4,0),(4,0),(4,0)],
				  [(4,0),(4,0),(4,0)],
				  [(4,0),(4,0),(4,0)]]
	let borders = (solve matrix)
	(printResult matrix borders)

	-- Teste meu 2
	putStrLn("\nTeste meu 2:")
	let matrix = [[(4,0),(4,0),(4,0),(4,0)],
				  [(4,0),(4,0),(4,0),(4,0)],
				  [(4,0),(4,0),(4,0),(4,0)],
				  [(4,0),(4,0),(4,0),(4,0)]]
	let borders = (solve matrix)
	(printResult matrix borders)



