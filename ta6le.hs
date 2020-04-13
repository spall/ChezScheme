import base.hs -- Mf-base

module Ta6le() where

-- as if you called make
top :: IO ()
top = do
  -- copy files
  fs <- listDirectory "../../c"
  map cpsrc fs -- for all files in ../../c
  -- build kernel object files
  map co $ mdsrc ++ kernelsrc -- build kernelobjs
  cmd [ar, arFlags, kernelLib, kernelObj] -- run ar on those object files we just built
  
  -- kernellinkdeps = kernelliblinkdeps
  -- zlibdep
  buildzliba
  -- lz4dep
  when buildStatic buildliblz4a

  -- main which is object file
  {- ${Main}: ${mainobj}
	cp -p ${mainobj} ${Main}
  -}
  -- build mainobj
  
  -- copy main obj to Main
  cmd $ [c, "-rdynamic", "-o", scheme, main, kernel, mdclib] ++ kernelLinkLibs ++ [ldflags]
  

buildliblz4a :: IO ()
buildliblz4a = withCurrentDirectory "../lz4/lib" $ do
   cmd [cc, cppflags, cflags, "-c", srcfiles]
   ofiles <- getDirectoryFiles "." ["*.o"]
   cmd $ [ar, "rcs", "liblz4.a"] ++ ofiles

buildlibza :: IO ()
buildlibza = withCurrentDirectory "../zlib" $ do
  map (\objf -> cmd [cc, cflags, zinc, "-c", "-o", objf, objf -<.> "c") objc -- objs
  cmd $ [ar, arflags, "libz.a"] ++ objs
  -- -@ ($(RANLIB) $@ || true) >/dev/null 2>&1  huh?
  -- example.o
  cmd [cc, cflags, zincout, "-c", "-o", "example.o", "test/example.c"]
  --example.exe
  cmd [cc, cflags, "-o", exampleexe, "example.o", testLDflags]
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
  
  
objs = objc ++ obja
objc = objz ++ objg
objz = ["adler32.o", "crc32.o", "deflate.o", "infback.o", "inffast.o", "inflate.o"
       ,"inftrees.o", "trees.o", "zutil.o"]
objg = ["compress.o", "uncompr.o", "gzclose.o", "gzlib.o", "gzread.o", "gzwrite.o"]
obja = []


m = "ta6le"
cpu = "X86_64"

mdclib = "-lm -ldl ${ncursesLib} -lpthread -lrt -luuid"
c = [cc, cppflags, "-m64 -msse2 -Wpointer-arith -Wall -Wextra -Werror -Wno-implicit-fallthrough -O2 -D_REENTRANT -pthread", cflags]
o = "o"
mdsrc = "i3le.c"
mdobj = "i3le.o"

{-
.c.o:
	$C -c -D${Cpu} -I${Include} ${zlibInc} ${LZ4Inc} $*.c
-}
co :: FileName -> IO ()
co fn = cmd [c, "-c", "-D" ++ cpu, "-I" ++ include, zlibInc, lz4Inc, fn]

-- depends on kernelobj zlibdep lz4dep
kernelO :: todo -> IO ()
kernelO ko = cmd [ld, "-melf_x86_64", "-r", "-X", "-o", ko, kernelObj, zliblib, lz4Lib]

{-
../zlib/configure.log:
	(cd ../zlib; CFLAGS="${CFLAGS} -m64" ./configure --64)
-}
zlibConfigureLog :: IO ()
zlibConfigureLog = cmd (Cwd "../zlib/configure.log") (addEnv "CFLAGS" $ cflags ++ " -m64")
                   ["./configure", "--64"]
