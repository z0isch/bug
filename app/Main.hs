{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Bug.Diagram
import Bug.Types
import qualified Data.HashSet as H

d :: Diagram B
d = gridDia testGrid

t :: Diagram B
t = d 
  ||| (
    (foldMap bugDia (fst $ bugs testGrid) 
    ||| foldMap bugDia (H.map cannonicalBug $ fst $ bugs testGrid))
    === (foldMap bugDia (snd $ bugs testGrid) 
    ||| foldMap bugDia (H.map cannonicalBug $ snd $ bugs testGrid)))

main :: IO ()
main = mainWith t