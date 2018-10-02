module Borders (Borders, emptyBorders, bordersLength, getArrowFromBorder, borderToCordinate, isVertical, isHorizontal) where

import Arrows
import Matrix

type Borders = [Arrow]

{- Retorna um array vazio do tipo borders. -}
emptyBorders :: Borders
emptyBorders = []

{- Retorna o tamanho do array de bordas de acordo com
   o tamanho da matriz a ser utilizada. Como sabemos que
   uma matriz tem 4 lados, o tamanho do array será
   4 * o tamanho de uma linha de uma matriz, considerando-a
   como quadrada. -}
bordersLength :: Int -> Int
bordersLength matrixLen = 4 * matrixLen

{- Retorna uma flecha do array de bordas. -}
getArrowFromBorder :: Int -> Borders -> Arrow
getArrowFromBorder index borders = borders!!index

{- Converte o index de uma borda dentro do array
   de bordas em uma cordenada inicial do tipo (linha,coluna),
   a qual representa a cordenada inicial daquela borda
   dentro da matriz, e é utilizada nas funções em tester
   e updater para testar certas flechas e atualizar
   a matriz caso o teste retorne valido. -}
borderToCordinate :: Int -> Int -> Coordinate
borderToCordinate matrixLen borderIndex | (borderIndex < matrixLen) = (0 , borderIndex)
										                    | ((borderIndex >= matrixLen) && (borderIndex < (2 * matrixLen))) = ((borderIndex - matrixLen) , (matrixLen-1))
									                      | ((borderIndex >= (2 * matrixLen)) && (borderIndex < (3 * matrixLen))) = (0 , (abs ((mod borderIndex matrixLen) - (matrixLen-1))))
						   	                        | ((borderIndex >= (3 * matrixLen)) && (borderIndex < (4 * matrixLen))) = ((abs ((mod borderIndex matrixLen) - (matrixLen-1))), 0)
										                    | otherwise = (-1,-1)

{- Verifica se uma posição do array de bordas é uma posição pertencente
   as bordas verticais (direita ou esquerda) ou não. -}
isVertical :: Int -> Int -> Bool
isVertical borderIndex matrixLen | (borderIndex < matrixLen) = False
                                   | ((borderIndex >= (2 * matrixLen)) && (borderIndex < (3 * matrixLen))) = False
                                   | otherwise = True

{- Verifica se uma posição do array de bordas é uma posição pertencente
   as bordas verticais (direita ou esquerda) ou não. -}
isHorizontal :: Int -> Int -> Bool
isHorizontal borderIndex matrixLen | (borderIndex < matrixLen) = False
                                   | ((borderIndex >= (2 * matrixLen)) && (borderIndex < (3 * matrixLen))) = False
                                   | otherwise = True