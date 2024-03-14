module Main (main) where

import Data.Function
import Debug.Trace
import Graphics.Implicit as I
import Graphics.Implicit.Primitives as P
import qualified Linear as L

x :: SymbolicObj3
x =
  I.extrude (P.circle 8) 35
    & flip (I.differenceR 1) [I.translate (V3 0 0 1) (I.extrude (P.circle 7) 35)]
    & with union (I.extrudeM (Left 0) (C1 0) (Left 1) (square True $ V2 1 2) (Left 10))
    & (`difference` [edge (V2 10 5) (V2 20 30) 1 0.5])
    & (`difference` [edge (V2 10 5) (V2 10 30) 1 0.5])
    & (`difference` [edge (V2 0 5) (V2 10 30) 1 0.5])

edge ::
  V2 Double ->
  V2 Double ->
  Double ->
  Double ->
  SymbolicObj3
edge from@(V2 fromX fromY) to@(V2 toX _) width _fullWidthRatio =
  let vec = to - from
      extrudeLength = L.dot vec (V2 0 1)
   in I.extrudeM
        (Left $ traceShowId $ 360 * ((fromX - toX) / 50.265))
        (C1 1)
        (Right $ \_ -> V2 0 0)
        (P.circle 1 & I.scale (V2 0.6 width) & translate (V2 8 0))
        (Left extrudeLength)
        & translate (V3 0 0 fromY)
        & rotate3 (V3 0 0 (2 * pi * fromX / 50.265))

with :: ([a] -> b) -> a -> a -> b
with f a b = f [a, b]

-- height, width :: Double
-- height = 35
-- width = 50.265 -- 8 * 2 * pi

main :: IO ()
main = do
  I.writeSTL 0.5 "kiriko.stl" x
