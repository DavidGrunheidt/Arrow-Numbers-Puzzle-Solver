module Main (main) where

import Solver
import Matrix 
import Borders 
import Arrows
import Updater
import Tester 
import Elements

main = do
	let matrix = [[(4,0),(4,0)],[(4,0),(4,0)]]
	putStrLn ("borders length = " ++ (show (bordersLength (matrixLength matrix))))
	putStrLn ("Matrix length = " ++ (show (matrixLength matrix)))
	putStrLn ("Matrix = " ++ (show matrix))
	putStrLn ("Arrows Length = " ++ (show arrowsLength))
	putStrLn (show (testPossibleUpdate matrix (matrixLength matrix) 4 6))
	--putStrLn ("New matrix = " ++ (show (matrixUpdate matrix (matrixLength matrix) 6 2)))
	--putStrLn ("Solved " ++ (show solve))