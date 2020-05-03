
module S.Ta6le(build) where

import qualified S.Base as B

m = "ta6le"
archincludes = ["x86_64.ss"]

build :: IO ()
build = B.build m archincludes
