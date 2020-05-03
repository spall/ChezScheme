{-# LANGUAGE RecordWildCards #-}

module C.Ta6le(build) where

import System.Directory
import Development.Rattle
import Development.Shake.Command
import Data.Maybe
import qualified Configure as C
import qualified C.Base as B

-- were in base but 

m = "ta6le"
cpu = "X86_64"

mdclib ncurseslib = ["-lm", "-ldl", ncurseslib, "-lpthread", "-lrt", "-luuid"]
cc = "cc" -- todo check what this really is
ccFlags c@C.Config{..} = cppflags ++ ["-m64", "-msse2", "-Wpointer-arith", "-Wall", "-Wextra", "-Werror", "-Wno-implicit-fallthrough", "-O2", "-D_REENTRANT", "-pthread"] ++ cflags
o = "o"
mdsrc = "i3le.c"
mdobj = "i3le.o"

include = B.include m
kernelLib = B.kernelLib m
kernelObj = B.kernelObj o [mdobj]
mainObj = B.mainObj o
main = B.main m o
scheme = B.scheme m
kernel = B.kernel m

{-
.c.o:
	$C -c -D${Cpu} -I${Include} ${zlibInc} ${LZ4Inc} $*.c
-}
buildObj :: C.Config -> FilePath -> IO ()
buildObj config@C.Config{..} cf = cmd $ [cc] ++ ccFlags config ++ ["-c", "-D" ++ cpu, "-I" ++ include , zlibInc, lz4Inc, cf]

build :: C.Config -> IO ()
build config@C.Config{..} = withCurrentDirectory "c" $ do
  -- were in base but rely on defs from config 
  let kernelLibLinkDeps = [zlibDep, lz4Dep]
      kernelLibLinkLibs = [zlibLib, lz4Lib]
      kernelLinkLibs = kernelLibLinkLibs
      kernel_ = fromJust $ either (`lookup` [("kernelLib", kernelLib), ("kernelO", B.kernelO m o)]) Just kernel
  
  -- copy files
  fs <- listDirectory "../../c"
  mapM_ B.cpsrc fs -- for all files in ../../c
  -- build kernel object files
  mapM_ (buildObj config) $ mdsrc:B.kernelsrc -- build kernelobjs
  cmd_ $ [ar] ++ arflags ++ [kernelLib] ++ kernelObj -- run ar on those object files we just built
  
  -- kernellinkdeps = kernelliblinkdeps = [zlibdep, lz4dep]
  -- zlibdep = "../zlib/libz.a"
  -- ../zlib/configure.log
  cmd_ [(Cwd "../zlib"),(AddEnv "CFLAGS" $ (unwords cflags) ++ " -m64")] ["./configure", "--64"]
  -- TODO ZLib.build  ; for now just call make since the info is generated from the above configure
  cmd_ (Cwd "../zlib") ["make"]
  -- lz4dep
  -- TODO when buildStatic buildliblz4a

  -- main which is object file
  {- ${Main}: ${mainobj}
	cp -p ${mainobj} ${Main}
  -}
  -- build mainobj
  buildObj config B.mainsrc
  -- copy main obj to Main
  cmd_ ["cp", "-p", mainObj, main]
  cmd $ [cc] ++ ccFlags config ++ ["-rdynamic", "-o", scheme, main, kernel_] ++ mdclib ncursesLib ++ kernelLinkLibs ++ ldflags  

{-
${KernelO}: ${kernelobj} ${zlibDep} ${LZ4Dep}
	${LD} -melf_x86_64 -r -X -o ${KernelO} ${kernelobj} ${zlibLib} ${LZ4Lib}

${KernelLib}: ${kernelobj}
	${AR} ${ARFLAGS} ${KernelLib} ${kernelobj}

${Scheme}: ${Kernel} ${KernelLinkDeps} ${Main}
	$C -rdynamic -o ${Scheme} ${Main} ${Kernel} ${mdclib} ${KernelLinkLibs} ${LDFLAGS}

../zlib/configure.log:
	(cd ../zlib; CFLAGS="${CFLAGS} -m64" ./configure --64)

../lz4/lib/liblz4.a: ${LZ4Sources}
	(cd ../lz4/lib; CFLAGS="${CFLAGS} -m64" ${MAKE} liblz4.a)


buildliblz4a :: IO ()
buildliblz4a = withCurrentDirectory "../lz4/lib" $ do
   cmd [cc, cppflags, cflags, "-c", srcfiles]
   ofiles <- getDirectoryFiles "." ["*.o"]
   cmd $ [ar, "rcs", "liblz4.a"] ++ ofiles

buildlibza :: IO ()
buildlibza = withCurrentDirectory "../zlib" $ do
  map (\objf -> cmd [cc, cflags, zinc, "-c", "-o", objf, objf -<.> "c"]) objc -- objs
  cmd $ [ar, arflags, "libz.a"] ++ objs
  -- -@ ($(RANLIB) $@ || true) >/dev/null 2>&1  huh?
  -- example.o
  cmd [cc, cflags, zincout, "-c", "-o", "example.o", "test/example.c"]
  --example.exe
  cmd [cc, cflags, "-o", exampleexe, "example.o", testLDflags]
  -- sharedlibv: pic_objs libz.a
  -- pic_objs
  cmd Shell [mkdir, "objs", "2>/dev/null", "||", "test", "-d", "objs"]
  map (\lo -> do
          cmd [cc, sflags, zinc, "-DPIC", "-c", " -o", "objs" </> lo -<.> "o", srcdir </> lo -<.> "c"]
          cmd [mv, "objs" </> lo -<.> "o", lo]) pic_objc
  --
  cmd $ [ldshared, sflags,  "-o", sharedlibv] ++ pic_objs ++ [ldsharedlibc, ldflags]
  cmd [rm, "-f", sharedlib, sharedlibm]
  cmd [ln, "-s", sharedlibv, sharedlib]
  cmd [ln, "-s", sharedlibv, sharedlibm]
  cmd [rmdir, objs]
  
  --examplesh.exe
  cmd [cc, cflags, "-o", exampleshexe, "example.o", "-L.", sharedlibv]
  --minigzip.o
  cmd [cc, cflags, zincout, "-c", "-o", "minigzip.o", "test/minigzip.c"]
  --minigzip.exe
  cmd [cc, cflags, "-o", minigzipexe, "minigzip.o", testLDflags]
  --minigzipsh.exe
  cmd [cc, cflags, "-o", minigzipshexe, "minigzip.o", "-L.", sharedlibv]
  -- shared
  -- sharedlibv = libz.so.1.2.11 ; does it already exist?
  -- all64
  -- exmaple64.o
  cmd [cc, cflags, zincout, "-D_FILE_OFFSET_BITS=64", "-c", "-o", "example64.o", "test/example.c"]
  -- minigzip64.o
  cmd [cc, cflags, zincout, "-D_FILE_OFFSET_BITS=64", "-c", "-o", "minigzip64.o", "test/minigzip.c"]
  -- example64.exe
  cmd [cc, cflags, "-o", example64exe, "example64.o", test_ldflags]
  -- minigzip64.exe
  cmd [cc, cflags, "-o", minigzip64exe, "minigzip64.o", test_ldflags]


pic_objs = pic_objc ++ pic_obja
pic_objc = pic_objz ++ pic_objg
pic_objz = ["adler32.lo", "crc32.lo", "deflate.lo", "infback.lo", "inffast.lo", "inflate.lo"
           ,"inftrees.lo", "trees.lo", "zutil.lo"]
pic_objg = ["compress.lo", "uncompr.lo", "gzclose.lo", "gzlib.lo", "gzread.lo", "gzwrite.lo"]
pic_obja = []
  
objs = objc ++ obja
objc = objz ++ objg
objz = ["adler32.o", "crc32.o", "deflate.o", "infback.o", "inffast.o", "inflate.o"
       ,"inftrees.o", "trees.o", "zutil.o"]
objg = ["compress.o", "uncompr.o", "gzclose.o", "gzlib.o", "gzread.o", "gzwrite.o"]
obja = []

-}
