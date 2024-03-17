module Template (obj, makeTemplate) where

import Data.Function
import Graphics.Implicit as I
import Graphics.Implicit.Primitives as P
import qualified Linear as L

obj :: SymbolicObj3
obj = P.cube True 2

makeTemplate :: IO ()
makeTemplate = do
  I.writeSTL 0.5 "Template.stl" obj
