
module Ta6le(build) where

import System.Directory
import System.Info.Extra
import Development.Rattle
import System.FilePath
import qualified S.Ta6le as S.Ta6le
import qualified C.Ta6le as C.Ta6le

build :: IO ()
build = do
  -- cd c && make
  C.Ta6le.build
  -- cd s && make bootstrap
  S.Ta6le.build

