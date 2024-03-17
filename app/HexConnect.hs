module HexConnect (obj, makeHexConnect) where

import Control.Lens ((^.))
import Data.Function
import Graphics.Implicit as I
import Graphics.Implicit.Primitives as P
import Linear as L

obj :: SymbolicObj3
obj =
  let twoDimentional =
        line (V2 0 0) (V2 0 12) 1
          <> line (V2 0 12.5) (V2 5 12.5) 1
          <> line (V2 0 0) (V2 5 0) 1
          <> line (V2 4 13.5) (V2 5.5 12) 1
          <> line (V2 4 (-1)) (V2 5.5 0.5) 1
   in extrude 7 $ union [twoDimentional, twoDimentional & I.mirror (V2 1 0)]

line :: V2 Double -> V2 Double -> Double -> SymbolicObj2
line from to width =
  let dir = to - from
      len = L.norm dir
      horizontal = P.rect (V2 0 (-width / 2)) (V2 len (width / 2))
   in horizontal
        & I.rotate (atan2 (dir ^. _y) (dir ^. _x))
        & I.translate from

makeHexConnect :: IO ()
makeHexConnect = do
  I.writeSTL 0.5 "HexConnect.stl" obj
