{-# LANGUAGE RecordWildCards #-}

module Zlib.Configure(config,Config(..)) where

import Development.Shake.Command
import Data.List.Extra
import Data.List
import Data.Maybe


{-
config :: String -> IO ()
config srcdir = do
  let (zinc,zincout, srcdir) = if srcdir == "."
                               then ([], "-I.", [])
                               else (["-include", "zconf.h"], ["-I.", "-I" ++ srcdir], srcdir)
  env <- getEnvironment
  (uname, crossPrefix) <- case lookup "CHOST" env of
                            Just (x:xs) -> do
                              Stdout out <- cmd Shell ["echo", "\"${CHOST}\"", "|", "sed", "-e", "'s/^[^-]*-\\([^-]*\\)$/\1/'", "-e", "'s/^[^-]*-[^-]*-\\([^-]*\\)$/\1/'", "-e", "'s/^[^-]*-[^-]*-\\([^-]*\\)-.*$/\1/'"]
                              return (Just $ trim out, Just $ (x:xs) ++ "-")
                            _ -> return (Nothing, Nothing)

  let staticLib = "libz.a"
  Stdout out <- cmd Shell ["sed", "-n", "-e", "'/VERSION \"/s/.*\"\\(.*\\)\".*/\1/p'", "<", srcdir </> "zlib.h"]
  let ver = trim out
  Stdout out <- cmd Shell ["sed", "-n", "-e", "'/VERSION \"/s/.*\"\\([0-9]*\\.[0-9]*\\.[0-9]*\\).*/\1/p'", "<", srcdir </> "zlib.h"]
  let ver3 = trim out
  Stdout out <- cmd Shell ["sed", "-n", "-e", "'/VERSION \"/s/.*\"\\([0-9]*\\.[0-9]*\\)\\..*/\1/p'", "<", srcdir </> "zlib.h"]
  let ver2 = trim out
  Stdout out <- cmd Shell ["sed", "-n", "-e", "'/VERSION \"/s/.*\"\\([0-9]*\\)\\..*/\1/p'", "<", srcdir </> "zlib.h"]
  let ver1 = trim out

  let cpstr = fromMaybe "" crossPrefix
  Exit e <- cmd Shell [cpstr ++ "ar", "--version", ">/dev/null", "2>/dev/null", "||", "test", "$?", "-lt", "126"]
  Exit e2 <- cmd Shell [cpstr ++ "ranlib", "--version", ">/dev/null", "2>/dev/null", "||", "test", "$?", "-lt", "126"]
  let ar = case e of
             ExitSuccess -> fromMaybe (crossPrefix ++ "ar") $ lookup "AR" env
             _ -> fromMaybe "ar" $ lookup "AR" env
      arFlags = fromMaybe "rc" $ lookup "ARFLAGS" env
      ranlib = case e2 of
                 ExitSuccess -> fromMaybe (crossPrefix ++ "ranlib") $ lookup "RANLIB" env
                 _ -> fromMaybe "ranlib" $ lookup "RANLIB" env
      nm = case e3 of
             ExitSuccess -> fromMaybe (crossPrefix ++ "nm") $ lookup "NM" env
             _ -> fromMaybe "nm" $ lookup "NM" env
      -- set defaults before processing command line options
      ldConfig = fromMaybe "ldconfig" $ lookup "LDCONFIG" env
      ldSharedLibc = todo
      archs = Nothing
      prefix = fromMaybe "/usr/local" $ lookup "prefix" env
      execPrefix = fromMaybe prefix $ lookup "exec_prefix" env
      libDir = fromMaybe (execPrefix </> "lib") $ lookup "libdir" env
      sharedLibDir = fromMaybe libDir $ lookup "sharedlibdir" env
      includeDir = fromMaybe (prefix </> "include") $ lookup "includedir" env
      manDir = fromMaybe (prefix </> "share/man") $ lookup "mandir" env
      sharedExt = "so"
      shared = 1
      solo = 0
      cover = 0
      zPrefix = 0
      zConst = 0
      build64 = 0
      gcc = 0
      warn = 0
      debug = 0
      oldCC = cc
      oldCFlags = cFlgas
      objc = objz ++ objg
      picObjc = picObjz ++ picObjg

      -- TODO process command line

  random <- randomIO
  let test = "ztest" ++ (show random) 
  writeFile (test <.> "c") "extern int getchar();\n int hello() {return getchar();}\n"
  let cc_ = fromMaybe (crossPrefix ++ "gcc") $ lookup "CC" env
      cFlags = fromMaybe ["-O3"] $ unwords $ lookup "CFLAGS" env
      gcc1 = if isInfixOf "gcc" cc_
             then True
             else if isInfixOf "clang" cc_ then True else False
  Stdout out <- cmd Shell [cc_, "-v", "2>&1"]
  let gcc2 = if isInfixOf "gcc" out
             then True
             else if isInfixOf "clang" out then True else False
      gcc = gcc1 || gcc2
  (cc, cFlags, sFlags, ldShared) <- if gcc then do
    Exit e <- cmd [cc_, "-c", test <.> "c"]
    case e of
      ExitSuccess -> let cc = cc_
                         cFlags = (fromMaybe ["-O3"] $ unwords $ lookup "CFLAGS" env)
                                  ++ maybeToList archs
                                  ++ (if build64 then ["-m64"] else [])
                                  ++ (if warn then if zConst
                                                   then ["-Wall", "-Wextra", "-Wcast-qual", "-pedantic", "-DZLIB_CONST"]
                                                   else ["-Wall", "-Wextra", "-pedantic"]
                                       else [])
                                  ++ (if debug then ["-DZLIB_DEBUG"] else [])
                         
                         sFlags = (fromMaybe ["-O3"] $ unwords $ lookup "CFLAGS" env) ++ ["-fPIC"]
                                  ++ (if build64 then ["-m64"] else [])
                                  ++ (if debug then ["-DZLIB_DEBUG"] else [])
                         ldflags = maybeToList (lookup "LDFLAGS" env) ++ maybeToList archs in
                       do
                         uname <- if isNothing uname
                                  then do
                           Stdout out <- cmd Shell ["(","uname", "-s", "||", "echo", "unknown", ")", "2>/dev/null"]
                           return $ trim out
                                  else return Nothing
                         (ldShared, ldConfig) <- f $ trim uname
      _ -> g
    else g

  let sharedLib = fromMaybe ("libz" <.> sharedExt) $ lookup "SHAREDLIB" env
  let sharedLibV = fromMaybe ("libz" <.> sharedExt <.> ver) $ lookup "SHAREDLIBV" env
  let sharedlibM = fromMaybe ("libz" <.> sharedExt <.> ver1) $ lookup "SHAREDLIBM" env

  writeFile (test <.> "c") "#error error"
  Exit e <- cmd Shell $ ["(", cc, "-c"] ++ cFlags ++ [test <.> "c", ")", "2>/dev/null"]
  let try_ = try $ ExitSuccess == e

  writeFile (test <.> "c") "int foo() { return 0; }
  whenM (try_ $ [cc, "-c"] ++ cFlags ++ [test <.> "c"]) $ exitFailure "Compiler error reporting is too harsh for $0 (perhaps remove -Werror).

  writeFile (test <.> "c") "extern int getchar(); \n int hello() {return getchar();}"
  

g :: [(String,String)] -> IO ()
g env = do 
  let cc = fromMaybe "cc" $ lookup env
  let gcc = False
  uname <- if isNothing uname
           then do
    Stdout out <- cmd Shell ["(", "uname", "-sr", "||", "echo", "unknown",")", "2>/dev/null"]
    return $ trim out
           else return ""
  (sFlags, cFlags, ldShared) <- case uname of
                                  "UNIX_System_V 4.2.0" -> (fromMaybe ["-KPIC", "-O"] $ lookup "CFLAGS" env, fromMaybe ["-O"] $ lookup "CFLAGS" env, fromMaybe ["cc", "-G"] $ lookup "LDSHARED" env)
                                  "UNIX_SV 4.2MP" -> (fromMaybe ["-Kconform_pic", "-O"] $ lookup "CFLAGS" env, fromMaybe ["-O"] $ lookup "CFLAGS" env, fromMaybe ["cc", "-G"] $ lookup "LDSHARED" env)
                                  "OpenUNIX 5" -> (fromMaybe ["-KPIC", "-O"] $ lookup "CFLAGS" env, fromMaybe ["-O"] $ lookup "CFLAGS" env, fromMaybe ["cc", "-G"] $ lookup "LDSHARED" env)
                                  _ -> die $ "system not supported: " ++ uname
  return (cc, cFlags, sFlags, ldShared)

f :: String -> IO (Maybe [String], Maybe [String])
f uname
  | isPrefixOf "Linux" uname || isPrefixOf "linux" uname
    || isPrefixOf "GNU" uname || isPrefixOf "solaris" uname
  = return (Just $ fromMaybe [cc, "-shared", "-Wl,-soname,libz.so.1,--version-script,"++ srcdir </> "zlib.map"] $ lookup "LDSHARED" env, Nothing)
  | isSuffixOf "BSD" uname || isInfixOf "bsd" uname || uname == "DragonFly"
  = return (Just $ fromMaybe [cc, "-shared", "-Wl,-soname,libz.so.1.,--version-script," ++ srcdir </> "zlib.map"] $ lookup "LDSHARED" env, Just ["ldconfig", "-m"])
  | otherwise = die $ "not supported: " ++ uname

-- checks both output and exit code
tryBoth :: [String] -> IO Bool
tryBoth cmd = do
  (Exit e, Stdout out) <- cmd Shell $ ["("] ++ cmd ++ [")", "2>&1"]
  case e of
    ExitSuccess -> return $ out == ""
    _ -> return False

-- checks either output or exit code
try :: Bool -> [String] -> IO Bool
try error cmd | error = do
                  Stdout out <- cmd Shell $ ["("] ++ cmd ++ [")", "2>&1"]
                  return $ out == ""
              | otherwise = do
                  Exit e <- cmd Shell $ ["("] ++ cmd ++ [")", "2>&1"]
                  case e of
                    ExitSuccess -> return True
                    _ -> return False
                         
                         
prefixFlag :: Flag Config
prefixFlag = flagReq ["--prefix", "-p"] update "PREFIX" "prefix"
  where update val config = Right config{prefix=val}

ePrefixFlag :: Flag Config
ePrefixFlag = flagReq ["--eprefix", "-e"] update "EXPREFIX" "eprefix"
  where update val config = Right config{ePrefix=val}

libDirFlag :: Flag Config
libDirFlag = flagReq ["--libdir", "-l"] update "LIBDIR" "libdir"
  where update val config = Right config{libDir=val}

sharedLibDirFlag :: Flag Config
sharedLibDirFlag = flagReq ["--sharedlibdir"] update "SHAREDLIBDIR" "sharedlibdir"
  where update val config = Right config{sharedLibDir=val}

includeDirFlag :: Flag Config
includeDirFlag = flagReq ["--includedir", "-i"] update "INCLUDEDIR" "includedir"
  where update val config = Right config{includeDir=val}

unameFlag :: Flag Config
unameFlag = flagReq ["--uname", "-u"] update "UNAME" "uname"
  where update val config = Right config{uname=val}

sharedFlag :: Flag Config
sharedFlag = flagNone ["--shared", "-s"] update "shared"
  where update config = Right config{shared=True}

staticFlag :: Flag Config
staticFlag = flagNone ["--static", "-t"] update "static"
  where update config = Right config{shared=False}

soloFlag :: Flag Config
soloFlag = flagNone ["--solo"] update "solo"
  where update config = Right config{solo=True}

coverFlag :: Flag Config
coverFlag = flagNone ["--cover"] update "cover"
  where update config = Right config{cover=True}

zPrefixFlag :: Flag Config
zPrefixFlag = flagNone ["--zprefix", "-z"] update "zprefix"
  where update config = Right config{zPrefix=True}

build64Flag :: Flag Config
build64Flag = flagNone ["--64", "-6"] update "build 64"
  where update config = Right config{build64=True}

archsFlag :: Flag Config
archsFlag = flagReq ["--archs", "-a"] update "ARCHS" "archs"
  where update val config = Right config{archs=val}

constFlag :: Flag Config
constFlag = flagNone ["--const", "-c"] update "const"
  where update config = Right config{zconst=True}

warnFlag :: Flag Config
warnFlag = flagNone ["--warn", "-w"] update "warn"
  where update config = Right config{warn=True}

debugFlag :: Flag Config
debugFlag = flagNone ["--debug", "-d"] update "debug"
  where update config = Right config{debug=True}
      
-}

data Config = Config {all :: [String] -- list of targets ; ignore for now
                     ,ar :: String
                     ,arFlags :: [String]
                     ,cc :: String
                     ,cFlags :: [String]
                     ,cpp :: String
                     ,exe :: String
                     ,ldConfig :: String
                     ,ldFlags :: [String]
                     ,ldShared :: String
                     ,ldSharedLibC :: String
                     ,objC :: [String] -- list of variables
                     ,picObjC :: [String] -- list of variables
                     ,ranlib :: String
                     ,sFlags :: [String]
                     ,sharedLib :: String
                     ,sharedLibM :: String
                     ,sharedLibV :: String
                     ,staticLib :: String
                     ,test :: String
                     ,ver :: String
                     ,zU4 :: String
                     ,srcDir :: String
                     ,execPrefix :: String
                     ,includeDir :: String
                     ,libDir :: String
                     ,manDir :: String
                     ,prefix :: String
                     ,sharedLibDir :: String
                     ,uname :: String}

config :: [String] -> IO Config
config cflags = do
  putStrLn "here2"
  (Stdout out) <- cmd (AddEnv "CFLAGS" $ (unwords cflags)) ["./configure"]

  putStrLn out
  let strs = lines out
  
  let all = words $ f "all =" strs
      ar = f "ar =" strs
      arFlags = words $ f "arFlags =" strs
      cc = f "cc =" strs
      cFlags = words $ f "cFlags =" strs
      cpp = f "cpp =" strs
      exe = f "exe =" strs
      ldConfig = f "ldConfig =" strs
      ldFlags = words $ f "ldFlags =" strs
      ldShared = f "ldShared =" strs
      ldSharedLibC = f "ldSharedLibC =" strs
      objC = words $ f "objC =" strs
      picObjC = words $ f "picObjC =" strs
      ranlib = f "ranlib =" strs
      sFlags = words $ f "sFlags =" strs
      sharedLib = f "sharedLib =" strs
      sharedLibM = f "sharedLibM =" strs
      sharedLibV = f "sharedLibV =" strs
      staticLib = f "staticLib =" strs
      test = f "test =" strs
      ver = f "ver =" strs
      zU4 = f "zU4 =" strs
      srcDir = f "srcDir =" strs
      execPrefix = f "execPrefix =" strs
      includeDir = f "includeDir =" strs
      libDir = f "libDir =" strs
      manDir = f "manDir =" strs
      prefix = f "prefix =" strs
      sharedLibDir = f "sharedLibDir =" strs
      uname = f "uname =" strs

  return Config{..}
                   
  where f str strs = let m1 = find (isPrefixOf str) strs in
                       case m1 of
                         Nothing -> error $ "did not find: " ++ str ++ "\n searched" ++ show strs
                         Just x -> let m2 = stripPrefix str x in
                                     case m2 of
                                       Nothing -> error $ "did not strip prefix '" ++ str ++ "' from " ++ x
                                       Just y -> trim y
  
