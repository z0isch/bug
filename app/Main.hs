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
  ||| foldMap ((# showEnvelope) . (# showOrigin) . bugDia) (H.map cannonicalBug $ snd $ bugs testGrid)
  ||| foldMap ((# showEnvelope) . (# showOrigin) . bugDia) (H.map (cannonicalBug . reflect) $ snd $ bugs testGrid)
  ||| foldMap (foldl (\t b -> t === (b # showEnvelope # showOrigin)) mempty . map bugDia) (H.map (map cannonicalBug . rotations) $ snd $ bugs testGrid)
  ||| foldMap (foldl (\t b -> t === (b # showEnvelope # showOrigin)) mempty . map bugDia) (H.map (map (cannonicalBug . reflect) . rotations) $ snd $ bugs testGrid)
    
main :: IO ()
main = renderSVG "out.svg" (dims (V2 300 300)) t