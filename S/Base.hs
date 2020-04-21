
module S.Base(build) where

import System.Directory
import System.FilePath
import System.Info.Extra
import Control.Monad.Extra
import System.Posix.Files

import Development.Rattle
import Development.Shake.Command

revision m = "../boot" </> m </> "revision"

{- ordering constraints:
first: library, prims, mathprims, front, 5_?
last: back
newhash before read
io before read
event before 4
ftype after syntax
layout and record before strnum (first define-record)
date before 7
(there may be other constraints as well) -}

basesrc = ["library.ss", "prims.ss", "mathprims.ss", "record.ss", "5_1.ss", "5_2.ss"
          ,"5_3.ss", "strnum.ss", "bytevector.ss", "5_4.ss", "5_6.ss", "5_7.ss", "event.ss"
          ,"4.ss", "front.ss", "foreign.ss", "6.ss", "print.ss", "newhash.ss", "format.ss"
          ,"date.ss", "7.ss", "cafe.ss", "trace.ss", "engine.ss", "interpret.ss", "cprep.ss"
          ,"cpcheck.ss", "cp0.ss", "cpvalid.ss", "cptypes.ss", "cpcommonize.ss", "cpletrec.ss"
          ,"inspect.ss", "enum.ss", "io.ss", "read.ss", "primvars.ss", "syntax.ss", "costctr.ss"
          ,"expeditor.ss", "exceptions.ss", "pretty.ss", "env.ss", "fasl.ss", "reloc.ss"
          ,"pdhtml.ss", "strip.ss", "ftype.ss", "back.ss"]

compilersrc = ["cpnanopass.ss", "compile.ss", "cback.ss"]

allsrc :: [FilePath] -> [FilePath]
allsrc archincludes = basesrc ++ compilersrc ++ ["cmacros.ss"] ++ archincludes
  ++ ["setup.ss", "debug.ss", "priminfo.ss", "primdata.ss", "layout.ss", "base-lang.ss"
     ,"expand-lang.ss", "primref.ss", "types.ss", "io-types.ss", "fasl-helpers.ss"
     ,"hashtable-types.ss", "np-languages.ss", "bitset.ss", "fxmap.ss"]

build :: String -> [FilePath] -> IO ()
build m aic = withCurrentDirectory "s" $ do
  mapM_ (\f -> if isWindows
               then cmd_ ["cp", "-p", "../../s" </> f, f]
               else ifM (doesPathExist f)
                    (return ())
                    $ createSymbolicLink ("../../s" </> f) f) $ allsrc aic
  -- revision
  cmd_ Shell ["./update-revision", ">", revision m]
  cmd ["make", "allx"]
