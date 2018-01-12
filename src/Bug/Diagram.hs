{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE LambdaCase #-}

module Bug.Diagram  where

import Linear.V2
import Diagrams.Prelude
import Bug.Types
import qualified Data.Vector.Unboxed as V

bugDia = position . foldMap (\p -> [(coordToPoint p, hex)])

gridDia (Grid s g) = position $ V.ifoldl' (\ps i -> maybe ps (:ps) . bar (frmIdx s i)) mempty g

bar p = \case
  3 -> Nothing
  pl -> Just (coordToPoint p, hex # fc (playerColor pl))

playerColor 0 = white
playerColor 1 = lightgrey
playerColor 2 = darkslategray

hex = regPoly 6 1 # rotateBy (1/12)

coordToPoint :: BPoint -> P2 Double
coordToPoint (V2 q r) =  p2 (x,y)
  where
    q' = fromIntegral q
    r' = fromIntegral r
    x = sqrt 3 * (q' + (r'/2))
    y = -3/2 * r'