{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG
import Bug.Diagram
import Bug.Types
import qualified Data.HashSet as H
import Data.Maybe

g = testGrid3

t :: Diagram B
t = (
  gridDia g 
    ||| 
    (   foldMap (foldl (\p b -> p ||| (b # showEnvelope # showOrigin)) mempty . map bugDia) (H.map perms $ fst $ bugs g)
    === foldMap (foldl (\p b -> p ||| (b # showEnvelope # showOrigin)) mempty . map bugDia) (H.map perms $ snd $ bugs g)
    )
  )
  === gridDia (snd $ eat 1 g)
  

main :: IO ()
main = renderSVG "out.svg" (dims (V2 600 600)) t