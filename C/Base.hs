
module C.Base(nCursesLib, cppflags, cflags, include, zLibInc, lz4Inc, cpsrc
             ,kernelsrc, ar, arFlags, kernelLib, kernelObj,mainsrc,mainObj
             ,main, scheme, kernel, kernelLinkLibs, ldflags) where

import System.Directory
import System.FilePath
import System.Info.Extra
import System.FilePattern.Directory
import System.Posix.Files
import Control.Monad.Extra

import Development.Rattle
import C.Config

{- todo fix srcfiles
buildliblz4a :: IO ()
buildliblz4a = withCurrentDirectory "../lz4/lib" $ do
   cmd [cc, cppflags, cflags, "-c", srcfiles]
   ofiles <- getDirectoryFiles "." ["*.o"]
   cmd $ [ar, "rcs", "liblz4.a"] ++ ofiles
-}

include m = "../boot" </> m
petiteBoot m = "../boot" </> m </> "petite.boot"
schemeBoot m = "../boot" </> m </> "scheme.boot"
main m o = "../boot" </> m </> "main" <.> o
scheme m = "../bin" </> m </> "scheme"

-- # One of these sets is referenced in Mf-config to select between
-- # linking with kernel.o or libkernel.a

kernelO m o = "../boot" </> m </> "kernel" <.> o
kernelOLinkDeps = ""
kernelOLinkLibs = ""

kernel = kernelLib
kernelLib m = "../boot" </> m </> "libkernel.a"
kernelLibLinkDeps = [zLibDep, lz4Dep]
kernelLibLinkLibs = [zLibLib, lz4Lib]
kernelLinkLibs = kernelLibLinkLibs

kernelsrc = ["statics.c", "segment.c", "alloc.c", "symbol.c", "intern.c", "gcwrapper.c", "gc-ocd.c"
            ,"gc-oce.c", "number.c", "schsig.c", "io.c", "new-io.c", "print.c", "fasl.c", "vfasl.c"
            ,"stats.c", "foreign.c", "prim.c", "prim5.c", "flushcache.c", "schlib.c", "thread.c"
            ,"expeditor.c", "scheme.c", "compress-io.c"]

kernelObj o mdobj = mdobj ++ map (-<.> o) kernelsrc

kernelHdr = ["system.h", "types.h", "version.h", "globals.h", "externs.h", "segment.h", "gc.c"
            ,"sort.h", "thread.h", "config.h", "compress-io.h", "itest.c", "nocurses.h"]

mainsrc = "main.c"

mainObj o = mainsrc -<.> o

cpsrc :: FilePath -> IO ()
cpsrc f = if isWindows
          then cmd ["cp", "-p", "../../c" </> f, f]
          else ifM (doesPathExist f)
               (return ())
               $ createSymbolicLink ("../../c" </> f) f

-- doit: ${Scheme}

-- source: ${kernelsrc} ${kernelhdr} ${mdsrc} ${mainsrc}

{-${Main}: ${mainobj}
	cp -p ${mainobj} ${Main}
-}

rootsrc = getDirectoryFiles "../../c"

-- scheme.o: itest.c
-- scheme.o main.o: config.h
-- ${kernelobj}: system.h types.h version.h externs.h globals.h segment.h thread.h sort.h compress-io.h nocurses.h
-- ${kernelobj}: ${Include}/equates.h ${Include}/scheme.h
-- ${mainobj}: ${Include}/scheme.h
-- ${kernelobj}: ${zlibHeaderDep} ${LZ4HeaderDep}
-- gc-ocd.o gc-oce.o: gc.c

{-
../zlib/zlib.h ../zlib/zconf.h: ../zlib/configure.log

../zlib/libz.a: ../zlib/configure.log
	(cd ../zlib; ${MAKE})
-}
lZ4Sources = ["../lz4/lib/lz4.h", "../lz4/lib/lz4frame.h", "../lz4/lib/lz4.c"
             ,"../lz4/lib/lz4frame.c", "../lz4/lib/lz4hc.c", "../lz4/lib/xxhash.c"]

{-
clean:
	rm -f *.$o ${mdclean}
	rm -f Make.out
-}
