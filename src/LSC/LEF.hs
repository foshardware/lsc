
module LSC.LEF where

import LEF.Syntax
import LSC.Types


fromLEF :: LEF -> Bootstrap ()
fromLEF (LEF _ _ _ _ _ _) = bootstrap $ \ technology -> technology
  { dimensions = (20, 20)
  , wireWidth = 1
  }