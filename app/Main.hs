{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG
import Bug.Diagram
import Bug.Types
import qualified Data.HashSet as H

d :: Diagram B
d = gridDia testGrid2

t :: Diagram B
t = d 
  ||| 
  (   foldMap (foldl (\p b -> p ||| (b # showEnvelope # showOrigin)) mempty . map bugDia) (H.map perms $ fst $ bugs testGrid2)
  === foldMap (foldl (\p b -> p ||| (b # showEnvelope # showOrigin)) mempty . map bugDia) (H.map perms $ snd $ bugs testGrid2)
  )

main :: IO ()
main = renderSVG "out.svg" (dims (V2 600 600)) t