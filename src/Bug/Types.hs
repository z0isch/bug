{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Bug.Types where

import Data.Foldable
import Control.Lens
import Data.Vector.Unboxed ((!?), Vector)
import qualified Data.Vector.Unboxed as V
import Data.Word
import Linear.V2
import Linear.Vector
import Data.HashSet (HashSet)
import qualified Data.HashSet as H
import Data.Monoid
import Data.Semigroup
import Data.Graph (Graph)
import qualified Data.Graph as G
import qualified Data.Tree as T
import Data.Maybe
import Safe

type BPoint = V2 Int

type GPoint = Int

type GridSize = Int

type Bug = HashSet BPoint

data Grid = Grid 
    { _gSize :: !GridSize
    , _grid :: !(Vector Word8)
    }
    deriving (Eq, Show)
makeLenses ''Grid

toBPoint :: GridSize -> Iso' GPoint BPoint
toBPoint s = iso (frmIdx s) (idx s)

frmIdx :: GridSize -> GPoint -> BPoint
frmIdx s i = let (d,m) = i `divMod` (s*2 - 1) in V2 (d - (s-1)) (m - (s-1))

idx :: GridSize -> BPoint -> GPoint
idx s (V2 x y) = ((x + (s-1)) * (s*2 - 1)) + (y + (s-1))

neighbors :: BPoint -> [BPoint]
neighbors (V2 x z) = [V2 (x+1) z, V2 (x+1) (z-1), V2 x (z-1), V2 (x-1) z, V2 (x-1) (z-1), V2 x (z+1)]

isNeighbor :: BPoint -> BPoint -> Bool
isNeighbor p = (1 ==) . distance p

distance :: BPoint -> BPoint -> Int
distance (V2 q r) (V2 q' r') = maximum $ map abs [q-q',r-r',q + r - q' - r']

mkGrid :: GridSize -> Grid
mkGrid s = Grid s $ V.generate (sideLength*sideLength) mkPiece
    where
        mkPiece x 
            | distance (V2 0 0) (frmIdx s x) <= (s-1) = 0 
            | otherwise =  3
        sideLength = s*2 - 1

bugs :: Grid -> (HashSet Bug, HashSet Bug)
bugs gr@(Grid s g) = (forPl 1, forPl 2)
    where
        forPl x = H.fromList $ map (H.fromList . map (frmIdx s)) $ filter (maybe False (\c -> g !? c == Just x) . headMay) cs
        cs = map T.flatten $ G.components $ gridGraph gr

gridGraph :: Grid -> Graph
gridGraph (Grid s g) = G.buildG (0,V.length g - 1) edges
    where 
        edges = V.ifoldl' mkEdges [] g
        mkEdges es i pl = es ++ mapMaybe mkEdge (neighbors (frmIdx s i))
            where mkEdge n = let i2 = idx s n 
                             in do
                                pl2 <- g !? i2
                                if pl2 == pl then Just (i, i2) else Nothing

canEat :: Bug -> Bug -> Bool
canEat b1 b2 = isAdjacentBug b1 b2 && isIsomorphicBug b1 b2

isAdjacentBug :: Bug -> Bug -> Bool
isAdjacentBug b1 = getAny . foldMap (\p1 -> foldMap (Any . isNeighbor p1) b1)

isIsomorphicBug :: Bug -> Bug -> Bool
isIsomorphicBug b1 b2 
    | sameSize = H.size b1 == 1 || H.size b1 == 2 || cannonicalBug b1 `elem` perms b2
    | otherwise = False
    where
        sameSize = H.size b1 == H.size b2

perms :: Bug -> [Bug]
perms = concatMap (map cannonicalBug . \rot ->  [rot,reflect rot]) . rotations

rotations :: Bug -> [Bug]
rotations = take 6 . iterate (rotate60 (V2 0 0))

reflect :: Bug -> Bug
reflect = H.map f
    where f (V2 x z) = let y = -z - x
                           x' = y
                           z' = z
                       in V2 x' z'

centerPoint :: Bug -> BPoint
centerPoint = minimumBy (\(V2 q r) (V2 q' r') -> if q > q' then GT else if q < q' then LT else compare r r')

rotate60 :: BPoint -> Bug -> Bug
rotate60 c = H.map (\p -> let (V2 x z) = p ^-^ c in c ^+^ V2 (-z) (x + z))

cannonicalBug :: Bug -> Bug
cannonicalBug b = H.map (^+^ negated (centerPoint b)) b

setToPlayer :: Word8 -> [BPoint] -> Grid -> Grid
setToPlayer pl pts g@(Grid s _) = over grid (`V.update` V.fromList (map (\x-> (idx s x, pl)) pts)) g

testGrid :: Grid
testGrid = setToPlayer 2 [V2 0 (-1),V2 (-2) 0,V2 1 (-2), V2 (-1) 0,V2 0 (-2), V2 (-2) 1] 
         $ setToPlayer 1 [V2 (-1) 1,V2 0 1, V2 2 (-1),V2 1 (-1), V2 0 0, V2 2 (-2)] 
         $ mkGrid 3
testGrid2 :: Grid
testGrid2 = setToPlayer 2 [V2 0 (-1),V2 (-2) 0,V2 1 (-2), V2 (-1) 0,V2 0 (-2), V2 (-2) 1] 
        $ setToPlayer 1 [V2 (-2) 2, V2 (-1) 1,V2 0 1, V2 2 (-1),V2 1 (-1), V2 0 0] 
        $ mkGrid 3