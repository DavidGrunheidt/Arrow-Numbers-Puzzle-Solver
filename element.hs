module Elements (Element) where

{- First element of the pair - Total number of arrow that have
   to point to this element. Second - Actual number of arrows
   pointing to this element. -}
type Element = (Int, Int)

getTotalArrows :: Element -> Int
getTotalArrows (nTotal, _) = nTotal

getPointingArrows :: Element -> Int
getPointingArrows (_, nPointing) = nPointing