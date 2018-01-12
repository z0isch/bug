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
d = gridDia testGrid

t :: Diagram B
t = d 
  ||| foldMap bugDia (fst $ bugs testGrid)
  ||| foldMap (foldl (\t b -> t === (b # showEnvelope)) mempty . map bugDia) (H.map (map cannonicalBug . rotations) $ fst $ bugs testGrid)
    


main :: IO ()
main = renderSVG "out.svg" (dims (V2 300 300)) t