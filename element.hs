module Elements (Element, getTotalArrows, getPointingArrows, emptyElementList) where

{- First element of the pair - Total number of arrow that have
   to point to this element. Second - Actual number of arrows
   pointing to this element. -}
type Element = (Int, Int)

{- Retorna o numero total de flechas que podem
   apontar para aquele elemento. -}
getTotalArrows :: Element -> Int
getTotalArrows (nTotal, _) = nTotal

{- Retorna o numero total de flechas que estão
   apontando atualmente para aquele elemento -}
getPointingArrows :: Element -> Int
getPointingArrows (_, nPointing) = nPointing

{- Retorna uma lista vazia do tipo elemet .-}
emptyElementList :: [Element]
emptyElementList = []
