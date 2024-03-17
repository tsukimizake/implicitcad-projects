module HexConnect (obj, makeHexConnect) where

import Data.Function
import Graphics.Implicit as I
import Graphics.Implicit.Primitives as P
import qualified Linear as L

obj = P.cube True 2

makeHexConnect :: IO ()
makeHexConnect = do
  I.writeSTL 0.5 "HexConnect.stl" $ obj
