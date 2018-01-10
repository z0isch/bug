{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Bug.Diagram
import Bug.Types

d :: Diagram B
d = foo testGrid

main :: IO ()
main = mainWith d