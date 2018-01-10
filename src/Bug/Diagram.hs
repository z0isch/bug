{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE LambdaCase #-}

module Bug.Diagram  where

import Linear.V2
import Diagrams.Prelude
import Bug.Types
import qualified Data.Vector.Unboxed as V

foo (Grid s g) = position $ V.ifoldl' (\ps i -> \case
                                      3 -> ps
                                      pl -> ((coordToPoint (frmIdx s i),hex # fc (playerColor pl)):ps)) [] g
playerColor 0 = white
playerColor 1 = lightgrey
playerColor 2 = black

hex = regPoly 6 1 # rotateBy (1/12)

coordToPoint :: BPoint -> P2 Double
coordToPoint (V2 q r) =  p2 (x,y)
  where
    q' = fromIntegral q
    r' = fromIntegral r
    x = sqrt 3 * (q' + (r'/2))
    y = -3/2 * r'