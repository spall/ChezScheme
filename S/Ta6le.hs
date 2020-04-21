
module S.Ta6le(build) where

import qualified S.Base as S

m = "ta6le"
archincludes = ["x86_64.ss"]

build :: IO ()
build = S.build m archincludes
