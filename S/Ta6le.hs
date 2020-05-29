
module S.Ta6le(build) where

import System.Directory
import qualified S.Base as B
import Development.Rattle

m = "ta6le"
archincludes = ["x86_64.ss"]

build :: Run ()
build = B.build m archincludes
