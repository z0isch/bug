{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Bug.Types where

import Data.Foldable
import Control.Lens
import Data.Vector.Unboxed ((!), Vector)
import qualified Data.Vector.Unboxed as V
import Data.Word
import Data.List
import Linear.V2
import Linear.V3
import Linear.Matrix
import Linear.Vector
import Data.HashSet (HashSet)
import qualified Data.HashSet as H
import Safe
import Data.Monoid

type BPoint = V2 Int

type GridSize = Int

type Bug = HashSet BPoint

data Player = P1 | P2
    deriving (Eq, Show)

data Grid = Grid 
    { _gSize :: GridSize
    , _grid :: Vector Word8
    }
    deriving (Eq, Show)
makeLenses ''Grid

toBPoint :: GridSize -> Iso' Int BPoint
toBPoint s = iso (frmIdx s) (idx s)

frmIdx :: GridSize -> Int -> BPoint
frmIdx s i = let (d,m) = i `divMod` (s*2 - 1) in V2 (d - (s-1)) (m - (s-1))

idx :: GridSize -> BPoint -> Int
idx s (V2 x y) = ((x + (s-1)) * (s*2 - 1)) + (y + (s-1))

piece :: Grid -> BPoint -> Either () (Maybe Player)
piece (Grid s g) p = case x of
    0 -> Right Nothing
    1 -> Right $ Just P1
    2 -> Right $ Just P2
    _ -> Left ()
    where i = idx s p 
          x = g ! i

isNeighbor :: BPoint -> BPoint -> Bool
isNeighbor p = (1 ==) . distance p

distance :: BPoint -> BPoint -> Int
distance (V2 q r) (V2 q' r') = maximum $ map abs [q-q',r-r',q + r - q' - r']

mkGrid :: GridSize -> Grid
mkGrid s = Grid s $ V.generate (sideLength*sideLength) mkPiece
    where
        mkPiece x = if distance (V2 0 0) (frmIdx s x) <= (s-1) then 0 else 3
        sideLength = s*2 - 1

bugs :: Grid -> (HashSet Bug, HashSet Bug)
bugs (Grid s g) = V.ifoldl' mkPs (mempty,mempty) g
    where
        smash = foldl' f mempty
        f ps p = maybe (H.insert p ps) (\b' -> H.insert (H.union b' p) $ H.delete b' ps) b
            where b = headMay $ toList $ H.filter (getAny . foldMap (Any . (\p' -> getAny $ foldMap (Any . isNeighbor p') p))) ps
        mkPs p i = \case
            1 -> over _1 (smash . H.insert (H.singleton (frmIdx s i))) p
            2 -> over _2 (smash . H.insert (H.singleton (frmIdx s i))) p
            _ -> p

isSameBug :: Bug -> Bug -> Bool
isSameBug b1 b2 
    | sameSize = H.size b1 == 1 || H.size b1 == 2 || cb1 `elem` rotated ++ reflected
    | otherwise = False
    where
        sameSize = H.size b1 == H.size b2
        cb1 = cannonicalBug b1
        rotated = (map cannonicalBug . rotations) b2
        reflected = (map (cannonicalBug . reflect) . rotations) b2

rotations :: Bug -> [Bug]
rotations b = [rotate60 b, rotate60 $ rotate60 b,rotate60 $ rotate60 $ rotate60 b,rotate60 $ rotate60 $ rotate60 $ rotate60 b,rotate60 $ rotate60 $ rotate60 $ rotate60 $ rotate60 b]

reflect :: Bug -> Bug
reflect = H.map f
    where f (V2 x z) = V2 x' z'
            where y = -z - x
                  y' = y
                  x' = y'
                  z' = z
centerPoint :: Bug -> BPoint
centerPoint = minimumBy (\(V2 q r) (V2 q' r') -> if q > q' then GT else if q < q' then LT else compare r r')

rotate60 :: Bug -> Bug
rotate60 b = H.map (\p -> let (V2 x z) = p ^-^ c in c ^+^ V2 (-z) (x + z)) b
    where 
        c = centerPoint b

cannonicalBug :: Bug -> Bug
cannonicalBug b = H.map (^+^ negated (centerPoint b)) b

setToPlayer :: Word8 -> [BPoint] -> Grid -> Grid
setToPlayer pl pts g@(Grid s _) = over grid (`V.update` V.fromList (map (\x-> (idx s x, pl)) pts)) g

testGrid :: Grid
testGrid = setToPlayer 2 [V2 0 (-1),V2 (-2) 0,V2 1 (-2), V2 (-1) 0,V2 0 (-2), V2 (-2) 1] 
         $ setToPlayer 1 [V2 (-2) 2, V2 (-1) 1,V2 0 1, V2 2 (-1),V2 1 (-1), V2 0 0] 
         $ mkGrid 3