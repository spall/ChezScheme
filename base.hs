
import config.hs

include = "../boot" </> m
petiteBoot = "../boot" </> m </> "petite.boot"
schemeBoot = "../boot" </> m </> "scheme.boot"
main = "../boot" </> m </> $ "main" <.> o
scheme = "../bin" </> m </> scheme

# One of these sets is referenced in Mf-config to select between
# linking with kernel.o or libkernel.a

kernelO = "../boot" </> m </> "kernel" <.> o
kernelOLinkDeps = ""
kernelOLinkLibs = ""

kernelLib = "../boot" </> m </> "libkernel.a"
kernelLibLinkDeps = [zlibDep, lZ4Dep]
kernelLibLinkLibs = [zlibLib, lZ4Lib]

kernelsrc = ["statics.c", "segment.c", "alloc.c", "symbol.c", "intern.c", "gcwrapper.c"
            ,"gc-ocd.c", "gc-oce.c", "number.c", "schsig.c", "io.c", "new-io.c", "print.c"
            ,"fasl.c", "vfasl.c", "stats.c", "foreign.c", "prim.c", "prim5.c", "flushcache.c"
            ,"schlib.c", "thread.c", "expeditor.c", "scheme.c", "compress-io.c"]

kernelobj = mdobj ++ map (-<.> "o") kernelsrc

kernelhdr = ["system.h", "types.h", "version.h", "globals.h", "externs.h", "segment.h", "gc.c",
             "sort.h", "thread.h", "config.h", "compress-io.h", "itest.c", "nocurses.h"]

mainsrc = "main.c"

mainobj = mainsrc -<.> "o"

main = "../boot" </> m </> "main" <.> o

-- doit: ${Scheme}

source = kernelsrc ++ kernelhdr ++ mdsrc ++ mainsrc

-- depends on mainobj
cpMain :: IO ()
cpMain mn = cmd [cp, "-p", mainobj, mn]

-- all src files in ../../c
rootsrc = getDirectoryFiles "../../c"

cpsrc :: FileName -> IO ()
cpsrc f = if isWindows
          then cmd ["cp", "-p", "../../c" </> f, f]
          else cmd ["ln", "-s", "../../c" </> f, f]
--rootsrc=$(shell cd ../../c; echo *)

-- scheme.o: itest.c
-- scheme.o main.o: config.h
-- ${kernelobj}: system.h types.h version.h externs.h globals.h segment.h thread.h sort.h compress-io.h nocurses.h
-- ${kernelobj}: ${Include}/equates.h ${Include}/scheme.h
-- ${mainobj}: ${Include}/scheme.h
-- ${kernelobj}: ${zlibHeaderDep} ${LZ4HeaderDep}
-- gc-ocd.o gc-oce.o: gc.c

-- ../zlib/zlib.h ../zlib/zconf.h: ../zlib/configure.log

{-
../zlib/libz.a: ../zlib/configure.log
	(cd ../zlib; ${MAKE})
-}
buildzliblibzDota :: IO ()
buildzliblibzDota = withCurrentDirectory "../zlib" $ do
  todo

lZ4Sources = ["../lz4/lib/lz4.h", "../lz4/lib/lz4frame.h", "../lz4/lib/lz4.c"
             ,"../lz4/lib/lz4frame.c", "../lz4/lib/lz4hc.c", "../lz4/lib/xxhash.c"]

clean :: IO ()
clean = do
  cmd ["rm", "-f"] ++ (getDirectoryFiles "." ["*"<.>o]) ++ [mdclean]
  cmd ["rm", "-f", "Make.out"]
