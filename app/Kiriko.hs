module Kiriko (makeKiriko) where

import Data.Function
import Graphics.Implicit as I
import Graphics.Implicit.Primitives as P
import qualified Linear as L

x :: SymbolicObj3
x =
  let height = 50
      edgeRight a = edge (V2 a 5) (V2 (a + 10) (height - 5)) 1 0.5
   in I.extrude (P.circle 8) height
        & flip (I.differenceR 1) [I.translate (V3 0 0 1) (I.extrude (P.circle 7) height)]
        -- & \t -> union (t : [edgeRight a | a <- [0, 5 .. 51]])
        & (`difference` [edgeRight a | a <- [0, 5 .. 50]])

edge ::
  V2 Double ->
  V2 Double ->
  Double ->
  Double ->
  SymbolicObj3
edge from@(V2 fromX fromY) to@(V2 toX _) lineWidth _fullWidthRatio =
  let width = 50.265 -- 8 * 2 * pi
      vec = to - from
      extrudeLength = L.dot vec (V2 0 1)
   in I.extrudeM
        (Left $ 360 * ((fromX - toX) / width)) -- なぜかここだけ弧度法 ナンデ？
        (C1 1)
        (Right $ \_ -> V2 0 0)
        (P.rect (V2 0 0) (V2 1 1) & translate (V2 (-0.5) (-0.5)) & rotate (pi / 4) & I.scale (V2 0.6 lineWidth) & translate (V2 8 0))
        (Left extrudeLength)
        & translate (V3 0 0 fromY)
        & rotate3 (V3 0 0 (2 * pi * fromX / width))

makeKiriko :: IO ()
makeKiriko = do
  -- 0.5は陰関数表示 -> 出力モデルの近似精度
  -- 0.5だとかなり荒いが速い 0.1だときれいだが遅い
  I.writeSTL 0.5 "kiriko.stl" x
