module Template where

import Data.Function
import Graphics.Implicit as I
import Graphics.Implicit.Primitives as P
import qualified Linear as L

makeTemplate :: IO ()
makeTemplate = do
  I.writeSTL 0.5 "Template.stl" $ P.cube True 2
