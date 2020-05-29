{-# LANGUAGE RecordWildCards #-}

module Zlib.Build(build) where

import Development.Shake.Command
import Development.Rattle
import Zlib.Configure
import Development.Shake.FilePath

objs objc = objc ++ obja

objc_ = objz ++ objg
obja = []
objz = ["adler32.o", "crc32.o", "deflate.o", "infback.o", "inffast.o", "inflate.o", "inftrees.o"
       ,"trees.o", "zutil.o"]
objg = ["compress.o", "uncompr.o", "gzclose.o", "gzlib.o", "gzread.o", "gzwrite.o"]

picObjs_ = picObjc_ ++ picObja
picObjc_ = picObjz ++ picObjg
picObja = []
picObjz = ["adler32.lo", "crc32.lo", "deflate.lo", "infback.lo", "inffast.lo", "inflate.lo"
          ,"inftrees.lo", "trees.lo", "zutil.lo"]
picObjg = ["compress.lo", "uncompr.lo", "gzclose.lo", "gzlib.lo", "gzread.lo", "gzwrite.lo"]

zincout = "-I."
zinc = []
testLdFlags = ["-L.", "libz.a"]

build :: Config -> Run ()
build Config{..} = withCmdOptions [Cwd "../zlib"] $ do
  -- all: static shared all64

  -- static: example$(EXE) minigzip$(EXE)

  -- example.o
  cmd $ [cc] ++ cFlags ++ [zincout, "-c", "-o", "example.o", srcDir </> "test/example.c"]

  -- staticlib = libz.a: objs
  -- objs
  let f o = cmd $ [cc] ++ cFlags ++ zinc ++ ["-c", "-o", o, srcDir </> o -<.> "c"]
  mapM_ f objc_

  
  cmd $ [ar] ++ arFlags ++ ["libz.a"] ++ (objs objc_)
  cmd Shell $ ["(", ranlib, "libz.a", "||", "true", ")", ">", "/dev/null", "2>&1"]
  

  cmd $ [cc] ++ cFlags ++ ["-o", "example" ++ exe, "example.o"] ++ testLdFlags

  -- minigzip$(EXE) : minigzip.o staticlib
  --minigzip.o
  cmd $ [cc] ++ cFlags ++ [zincout, "-c", "-o", "minigzip.o", srcDir </> "test/minigzip.c"]

  cmd $ [cc] ++ cFlags ++ ["-o", "minigzip" ++ exe, "minigzip.o"] ++ testLdFlags
{-
  -- shared: examplesh$(EXE) minigzipsh$(EXE)

  -- examplesh$(EXE): example.o sharedLibV

  let sharedLibV = "libz.so.1.2.11"
  -- sharedLibV = libz.so.1.2.11 : pic_objs libz.a
  cmd Shell ["mkdir", "objs", "2>/dev/null", "||", "test", "-d", "objs"]
  -- pic_objs
  
  let g :: String -> IO ()
      g o = do
        cmd $ [cc] ++ sFlags ++ [zinc, "-DPIC", "-c", "-o", "objs" </> o -<.> "o", srcDir </> o -<.> "c"]
        cmd ["mv", "objs" </> o -<.> "o", o]
        
  mapM_ g picObjs_

  cmd $ [ldShared] ++ sFlags ++ ["-o", sharedLibV] ++ picObjs_ ++ [ldSharedLibC] ++ ldFlags
  cmd Shell $ ["rm", "-f", sharedLib, "&&", "ln", "-s", sharedLibV, sharedLib]
  cmd Shell $ ["rm", "-f", sharedLibM, "&&", "ln", "-s", sharedLibV, sharedLibM]
  cmd ["rmdir", "objs"] picObjc_

  cmd $ [cc] ++ cFlags ++ ["-o", "examplesh" ++ exe, "example.o", "-L.", sharedLibV]
  -- minigzipsh$(EXE)
  cmd $ [cc] ++ cFlags ++ ["-o", "minigzipsh" ++ exe, "minigzip.o", "-L.", sharedLibV]
-}

-- all64: example64$(EXE) minigzip64$(EXE)

  -- example64(EXE): example64.o staticlib

  -- example64.o
  cmd $ [cc] ++ cFlags ++ [zincout, "-D_FILE_OFFSET_BITS=64", "-c", "-o", "example64.o"
                           ,srcDir </> "test/example.c"]

  cmd $ [cc] ++ cFlags ++ ["-o", "example64" ++ exe, "example64.o"] ++ testLdFlags

  -- minigzip64(EXE): minigzip64.o staticlib

  -- minigzip64.o
  cmd $ [cc] ++ cFlags ++ [zincout, "-D_FILE_OFFSET_BITS=64", "-c", "-o", "minigzip64.o"
                          ,srcDir </> "test/minigzip.c"]

  cmd $ [cc] ++ cFlags ++ ["-o", "minigzip64" ++ exe, "minigzip64.o"] ++ testLdFlags
  
