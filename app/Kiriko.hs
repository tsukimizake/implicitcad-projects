module Kiriko (makeKiriko, obj) where

import Data.Function
import Graphics.Implicit as I
import Graphics.Implicit.Primitives as P
import qualified Linear as L

obj :: SymbolicObj3
obj =
  let height = 50
      edgeRight a = edge (V2 a 5) (V2 (a + 10) (height - 5)) 1 0.9
      edgeLeft a = edge (V2 (a + 10) 5) (V2 a (height - 5)) 1 0.9
   in I.extrude (P.circle 10) height
        & flip (I.differenceR 1) [I.translate (V3 0 0 1) (I.extrude (P.circle 9) height)]
        -- & \t -> union (t : [edgeRight a | a <- [0, 5 .. 51]])
        & ( `difference`
              ( [edgeLeft $ pi * a | a <- [0, 2 .. 20]]
                  ++ [edgeRight $ pi * a | a <- [0, 2 .. 20]]
              )
          )

edge ::
  V2 Double ->
  V2 Double ->
  Double ->
  Double ->
  SymbolicObj3
edge from@(V2 fromX fromY) to@(V2 toX _) lineWidth fullWidthRatio =
  let width = 62.832 -- 10 * 2 * pi
      vec = to - from
      extrudeLength = L.dot vec (V2 0 1)
   in I.extrudeM
        (Left $ 360 * ((fromX - toX) / width)) -- なぜかここだけ弧度法 ナンデ？
        (C1 1)
        (Left $ V2 0 0)
        (P.rect (V2 0 0) (V2 0.6 lineWidth) & translate (V2 (-0.5) (-0.5)) & rotate (pi / 4) & translate (V2 10 0))
        (Left extrudeLength)
        & translate (V3 0 0 fromY)
        & rotate3 (V3 0 0 (2 * pi * fromX / width))

makeKiriko :: IO ()
makeKiriko = do
  -- 0.5は陰関数表示 -> 出力モデルの近似精度
  -- 0.5だとかなり荒いが速い 0.1だときれいだが遅い
  I.writeSTL 0.5 "kiriko.stl" obj
