module Arrows (Direction, DirectionValue, Arrow, getArrow, arrowsLength, getDirection, getDirectionValue) where
 
type Direction = String
type DirectionValue = Int
type Arrow = (Direction, DirectionValue)

{- Lista com os pares representando todos os possiveis estados
   de uma flecha, assim como valores auxiliares pra comparação
   na hora de mostrar a solução do problema. -}
arrows :: [Arrow]
arrows = [("NW",0),("N",1),("NE",2),("E",3),("SE",4),("S",5),("SW",6),("W",7)]

{- Retorna o numero de possiveis estados de uma flecha. -}
arrowsLength :: Int
arrowsLength = 8

{- Retorna o par contendo a string que representa a direção
   de uma flecha assim como seu valor, de acordo com um index
   dado, o qual selecionara uma flecha das flechas padrões
   definidas na função arrows. -}
getArrow :: Int -> Arrow
getArrow index = arrows!!index

{- Retorna a string representando a direção da flecha. -}
getDirection :: Arrow -> Direction
getDirection (direction, _) = direction

{- Retorna o valor int representando a direção da flecha. -}
getDirectionValue :: Arrow -> DirectionValue
getDirectionValue (_, value) = value
