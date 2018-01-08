{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Bug.Types where

import Control.Lens
import Data.Vector.Unboxed ((!), (//))
import qualified Data.Vector.Unboxed as V
import Data.Word

type Point = (Int, Int)

type GridSize = Int

data Player = P1 | P2
    deriving (Eq, Show)

data Grid = Grid 
    { _gSize :: GridSize
    , _grid :: V.Vector Word8
    }
    deriving (Eq, Show)
makeLenses ''Grid

frmIdx :: Int -> Int -> Point
frmIdx s i = let (d,m) = i `divMod` (s*2 - 1) in (d - (s-1),m - (s-1))

idx :: Int -> Point -> Int
idx s (x,y) = ((x + (s-1)) * (s*2 - 1)) + ((y + (s-1)))

piece :: Grid -> Point -> Either () (Maybe Player)
piece (Grid s g) p = case x of
    0 -> Right Nothing
    1 -> Right $ Just P1
    2 -> Right $ Just P2
    _ -> Left ()
    where i = idx s p 
          x = g ! i

distance :: Point -> Point -> Int
distance (q,r) (q',r') = maximum $ map abs [q-q',r-r',q + r - q' - r']

mkGrid :: GridSize -> Grid
mkGrid s = Grid s $ V.generate (sideLength*sideLength) (\x -> if distance (0,0) (frmIdx s x) <= (s-1) then 0 else 3)
    where 
        sideLength = s*2 - 1

bugs :: Grid -> ([Point], [Point])
bugs (Grid s g) = V.ifoldl' mkPs (mempty,mempty) g
    where
        mkPs :: ([Point],[Point]) -> Int -> Word8 -> ([Point],[Point])
        mkPs p i = \case
            1 -> over _1 (frmIdx s i:) p
            2 -> over _2 (frmIdx s i:) p
            _ -> p