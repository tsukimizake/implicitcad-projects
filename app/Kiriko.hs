module Kiriko (makeKiriko, obj) where

import Data.Function
import Graphics.Implicit as I
import Graphics.Implicit.Primitives as P
import qualified Linear as L

obj :: SymbolicObj3
obj =
  let height = 50
      edgeRight a = edge (V2 a 5) (V2 (a + 10) (height - 5)) 1 0.8
      edgeLeft a = edge (V2 (a + 10) 5) (V2 a (height - 5)) 1 0.8
   in I.extrude height (P.circle 10)
        & flip (I.differenceR 1) [I.translate (V3 0 0 1) (I.extrude height (P.circle 8.6))]
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
        (Fn $ \z -> Left $ taper fullWidthRatio extrudeLength z)
        (Left $ V2 0 0)
        (P.rect (V2 0 0) (V2 lineWidth lineWidth) & translate (V2 (-0.5) (-0.5)) & rotate (pi / 4) & translate (V2 10 0))
        (Left extrudeLength)
        & translate (V3 0 0 fromY)
        & rotate3 (V3 0 0 (2 * pi * fromX / width))

taper :: Double -> Double -> Double -> Double
taper fullWidthRatio zMax =
  let fullWidthLen = zMax * fullWidthRatio
      taperLength = zMax * (1 - fullWidthRatio) / 2
   in \z ->
        if taperLength < z && z < fullWidthLen + taperLength
          then 1
          else
            if z <= taperLength
              then 1 + 0.05 * ((taperLength - z) / taperLength)
              else 1 + 0.05 * ((z - fullWidthLen - taperLength) / taperLength)

makeKiriko :: IO ()
makeKiriko = do
  -- 第一引数は陰関数表示 -> 出力モデルの近似精度
  -- 0.5だとかなり荒いが速い 多分最終出力は0.1とか
  I.writeBinSTL 0.5 "kiriko.stl" obj
