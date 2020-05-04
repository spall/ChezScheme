{-# LANGUAGE RecordWildCards, DeriveDataTypeable #-}

module Configure(configure, Config(..)) where

import System.Console.CmdArgs.Explicit
import qualified System.Directory as S
import Control.Monad.Extra
import Control.Monad.IO.Class
import System.FilePath
import System.Info.Extra
import System.Exit
import Development.Shake
import System.Environment
import Data.Maybe
import System.IO.Unsafe
import Data.List.Extra

type Variable = String

-- for all directories that have a scheme.boot file.... get directory name
-- 1. get list of machs from boot directory
getMachs :: IO [String]
getMachs = do
  ifM (S.doesDirectoryExist "boot")
    (do
        putStrLn "here"
        dirs <- getDirectoryFilesIO "boot" ["//scheme.boot"]
        putStrLn $ show dirs
        return $ map takeDirectory dirs)
    $ return  []

  {- 1. set a bunch of vars to init values
     2. figure out if windows
     3. call uname to get certain system info
     4. set threads="" and bits= ""
     5. set vars from command line args
     6. if command line didn't set bits; then use uname to set bits
     7. if command line didn't set threads; then set threads to no
     8. if m= "" then if bits = 64 then if threads = yes then m=tm64 else m = m64
                                   else if threads = yes then m=tm32 else m = m32
     9. if w = "" then w = m
    10. if installbin = "" then installbin = installprefix/bin
    11. if installlib = ....
    12. if instlalman = ....
    13. if disablex11 = no then if m = a6osx or m=ta6osx then if ! -d /opt/x11/include then disablex11 = yes
    14. if m = "" -o ! -f boot/m/scheme.boot then no suitable machine.....
    15. if -d .git ....  (Long one)
    16. ./workarea m w
    17. create makefiles
    18. write to config.h
    19. if disablex11 = yes then write something to config.h
    20. if disablecurses = yes then write something to config.h and curseslib= ""  ncurseslib= ""
    21. write to mf-config
-}

initConfig :: IO Config
initConfig = do
  machs <- getMachs
  -- 2. figure out if windows
  configUname <- if isWindows then return "CYGWIN_NT-"
                 else do
    Stdout out <- cmd "uname"
    return out

  env <- getEnvironment -- to avoid io errors hopefully

  -- init values of configArgs fields
  let m_ = ""
      workArea = ""
      threads = False
      tempRoot_ = ""
      installOwner_ = ""
      installGroup_ = ""
      installBin_ = ""
      installLib_ = ""
      installMan_ = ""
      installSchemeName_ = "scheme"
      installPetiteName_ = "petite"
      installScriptName_ = "scheme-script"
      disableX11 = False
      disableCurses = False
      cc_ = fromMaybe "gcc" $ lookup "CC" env
      cppFlags_ = fromMaybe "" $ lookup "CPPFLAGS" env
      cFlags_ = fromMaybe "" $ lookup "CFLAGS" env
      ld_ = fromMaybe "ld" $ lookup "LD" env
      ldFlags_ = fromMaybe "" $ lookup "LDFLAGS" env
      ar_ = fromMaybe "ar" $ lookup "AR" env
      arFlags_ = fromMaybe "rc" $ lookup "ARFLAGS" env
      ranlib_ = fromMaybe "ranlib" $ lookup "RANLIB" env
      windres_ = fromMaybe "windres" $ lookup "WINDRES" env
      zlibInc_ = "-I../zlib"
      lz4Inc_ = "-I../lz4/lib"
      zlibDep_ = "../zlib/libz.a"
      lz4Dep_ = "../lz4/lib/liblz4.a"
      zlibLib_ = "../zlib//libz.a"
      lz4Lib_ = "../lz4/lib/liblz4.a"
      zLibHeaderDep_ = ["../zlib/zconf.h", "../zlib/zlib.h"]
      lz4HeaderDep_ = ["../lz4/lib/lz4.h", "../lz4/lib/lz4frame.h"]
      kernel_ = "kernelLib"
      installKernelTarget_ = Left "installKernelLib"
      installzLibTarget_ = Left "installZLib"
      installlz4Target_ = Left "installlz4"

  bits <- f ["uname", "-a", "|", "egrep", "\'amd64|x86_64\'", ">", "/dev/null", "2>&1"]
          (return BITS64) (return BITS32)

  -- 3. call uname to get system info    
  (m32, m64, tm32, tm64, installPrefix, installManSuffix, gzipManPages) <-
    case trim configUname of
      "Linux" -> f ["uname", "-a", "|", "egrep", "\'i386|i686|amd64|athlon|x86_64\'", ">", "/dev/null", "2>&1"]
                   (return ("i3le", "a6le", "ti3le", "ta6le", "/usr", "share/man", True))
                   $ f ["uname", "-a", "|", "grep", "-i", "power", ">", "/dev/null", "2>&1"]
                   (return ("ppc32le", "", "tppc32le", "", "/usr", "share/man", True))
                   (return ("", "", "", "", "/usr", "share/man", True))        
      "QNX" -> f ["uname", "-a", "|", "egrep", "\'x86\'", ">", "/dev/null", "2>&1"]
                 (return ("i3qnx", "", "ti3qnx", "", "/usr/local", "man", True))
                 (return ("", "", "", "", "/usr/local", "man", True))
      "FreeBSD" -> f ["uname", "-a", "|", "egrep", "\'i386|i686|amd64|athlon|x86_64\'", ">", "/dev/null", "2>&1"]
                     (return ("i3fb", "a6fb", "ti3fb", "ta6fb", "/usr/local", "man", True))
                     (return ("", "", "", "", "/usr/local", "man", True))
      "OpenBSD" -> f ["uname", "-a", "|", "egrep", "\'i386|i686|amd64|athlon|x86_64\'", ">", "/dev/null", "2>&1"]
                     (return ("i3ob", "a6ob", "ti3ob", "ta6ob", "/usr/local", "man", True))
                     (return ("", "", "", "", "/usr/local", "man", True))
      "NetBSD" -> f ["uname", "-a", "|", "egrep", "\'i386|i686|amd64|athlon|x86_64\'", ">", "/dev/null", "2>&1"]
                    (return ("i3nb", "a6nb", "ti3nb", "ta6nb", "/usr", "share/man", False))
                    (return ("", "", "", "", "/usr", "share/man", False))
                    -- gzipmanpages=no
      "Darwin" -> f ["uname", "-a", "|", "egrep", "\'i386|i686|amd64|athlon|x86_64\'", ">", "/dev/null", "2>&1"]
                    (return ("i3osx", "a6osx", "ti3osx", "ta6osx", "/usr/local", "share/man", True))
                    (return ("", "", "", "", "/usr/local", "share/man", True))
      "SunOS" -> f ["uname", "-a", "|", "egrep", "\'i386|i686|amd64|athlon|x86_64\'", ">", "/dev/null", "2>&1"]
                   (return ("i3s2", "a6s2", "ti3s2", "ta6s2", "/usr", "share/man", False))
                   (return ("","","","", "/usr", "share/man", False))
                   --gzipmanpages=no
      "CYGWIN_NT-" -> f ["uname", "-a", "|", "egrep", "\'i386|i686|amd64|athlon|x86_64\'", ">", "/dev/null", "2>&1"]
                        (return ("i3nt", "a6nt", "ti3nt", "ta6nt", "/usr/local", "share/man", True))
                        (return ("", "", "", "", "/usr/local", "share/man", True))
      _ -> die $ "Unrecognized system: " ++ configUname

  -- 5. go through args
  config@ConfigArgs{..} <- processArgs $ cargs machs ConfigArgs{..}
  
 
  
  --   8. if m= "" then if bits = 64 then if threads = yes then m=tm64 else m = m64
                                --   else if threads = yes then m=tm32 else m = m32

  let m = if m_ == "" then if bits == BITS64
                           then if threads then tm64 else m64
                           else if threads then tm32 else m32
           else m_
  --9. if w = "" then w = m
      w = if workArea == "" then m else workArea
                
     
  -- 10. if installbin = "" then installbin = installprefix/bin
      installBin = if installBin_ == "" then installPrefix </> "bin"
                   else installBin_
      
  -- 11. if installlib = ....
      installLib = if installLib_ == "" then installPrefix </> "lib"
                   else installLib_
                         
  -- 12. if instlalman = ....
      installMan = if installMan_ == "" then installPrefix </> installManSuffix
                   else installMan_
                         
  -- 13. if disablex11 = no then if m = a6osx or m=ta6osx then if ! -d /opt/x11/include then disablex11 = yes
  nDisableX11 <- if (not disableX11) && (m == "a6osx" || m == "ta6osx")
                 then ifM (S.doesDirectoryExist "/opt/X11/include/")
                      (return disableX11)
                      (return True)
                 else return disableX11
      
  -- 14. if m = "" -o ! -f boot/m/scheme.boot then no suitable machine.....
  return $ when (m == "")
    unsafePerformIO $ die $ "no suitable machine type found \n try rerunning with -m=<machine type> \n available machine types: " ++ show machs

  unlessM (S.doesFileExist $ "boot" </> m </> "scheme.boot") $ 
    die $ "no suitable machine type found \n try rerunning with -m=<machine type> \n available machine types: " ++ show machs
  
  -- 15. if -d .git ....  (Long one)
  ifM (S.doesDirectoryExist ".git")
    (cmd Shell ["git", "submodule ", "init", "&&", "git", "submodule", "update", "||", "exit", "1"])
    (do
        (unlessM (S.doesFileExist "nanopass/nanopass.ss") $ 
         do
           cmd_ Shell ["rmdir", "nanopass", ">", "/dev/null", "2>&1"]
           cmd_ Shell ["(", "curl", "-L", "-o", "v1.9.tar.gz", "https://github.com/nanopass/nanopass-framework-scheme/archive/v1.9.tar.gz", "&&", "tar", "-zxf", "v1.9.tar.gz", "&&", "mv", "nanopass-framework-scheme-1.9", "nanopass", "&&", "rm", "v1.9.tar.gz", ")", "||", "exit", "1"])

        (whenM (andM [return $ zlibDep_ /= "", notM $ S.doesFileExist "zlib/configure"]) $ 
         do
           cmd_ Shell ["rmdir", "zlib", ">", "/dev/null", "2>&1"]
           cmd_ Shell ["(", "curl", "-L", "-o", "v1.2.11.tar.gz", "https://github.com/madler/zlib/archive/v1.2.11.tar.gz", "&&", "tar", "-xzf", "v1.2.11.tar.gz", "&&", "mv", "zlib-1.2.11", "zlib", "&&", "rm", "v1.2.11.tar.gz", ")", "||", "exit", "1"])

        (whenM (andM [return $ lz4Dep_ /= "", notM $ S.doesFileExist "lz4/lib/Makefile"]) $
          do
            cmd_ Shell ["rmdir", "lz4", ">", "/dev/null", "2>&1"]
            cmd_ Shell ["(", "curl", "-L", "-o", "v1.8.3.tar.gz", "https://github.com/lz4/lz4/archive/v1.8.3.tar.gz", "&&", "tar", "-xzf", "v1.8.3.tar.gz", "&&", "mv", "lz4-1.8.3", "lz4", "&&", "rm", "v1.8.3.tar.gz", ")", "||", "exit", "1"])

        (unlessM (S.doesFileExist "stex/Mf-stex") $
          do
            cmd_ Shell ["rmdir", "stex", ">", "/dev/null", "2>&1"]
            cmd_ Shell ["(", "curl", "-L", "-o", "v1.2.1.tar.gz", "https://github.com/dybvig/stex/archive/v1.2.1.tar.gz", "&&", "tar", "-zxf", "v1.2.1.tar.gz", "&&", "mv", "stex-1.2.1", "stex", "&&", "rm", "v1.2.1.tar.gz", ")", "||", "exit", "1"]))
      
  -- 16. ./workarea m w
  cmd_ ["./workarea", m, w]
    
  -- 17. create makefiles; skipping
  -- 18. write to config.h
  writeFile (w </> "c/config.h") $ "#define SCHEME_SCRIPT \"" ++ installScriptName_ ++ "\""
    ++ "\n #ifndef WIN32 \n #define DEFAULT_HEAP_PATH \"" ++ installLib </> "csv%v/%m\""
    ++ "\n #endif"                                                                
  
  -- 19. if disablex11 = yes then write something to config.h
  return $ when nDisableX11
    unsafePerformIO $ appendFile (w </> "c/config.h") "define DISABLE_X11"

  -- 20
  (cursesLib, ncursesLib) <- if disableCurses then do
    appendFile (workArea </> "c/config.h") "define DISABLE_CURSES"
    return ("", "")
                             else do
    return ("-lcurses", "-lncurses")
 
  -- 21. write to mf-config
  -- todo make sure variables are correct so we can pass our structure ....
  let cc = cc_
      cppflags = words cppFlags_
      cflags = words cFlags_
      ld = ld_
      ldflags = words ldFlags_
      ar = ar_
      arflags = words arFlags_
      ranlib = ranlib_
      windres = windres_
      kernel = Left kernel_
      zlibInc = zlibInc_
      lz4Inc = lz4Inc_
      zlibDep = zlibDep_
      lz4Dep = lz4Dep_
      zlibLib = zlibLib_
      lz4Lib = lz4Lib_
      zLibHeaderDep = zLibHeaderDep_
      lz4HeaderDep = lz4HeaderDep_
      kernelLinkDeps = Left $ kernel_ ++ "LinkDeps"
      kernelLinkLibs = Left $ kernel_ ++ "LinkLibs"
      installOwner = installOwner_
      installGroup = installGroup_
      tempRoot = tempRoot_
      gZipManPages = gzipManPages
      installSchemeName = installSchemeName_
      installPetiteName = installPetiteName_ 
      installScriptName = installScriptName_
      installKernelTarget = installKernelTarget_
      installzLibTarget = installzLibTarget_
      installlz4Target = installlz4Target_
  
  return Config{..}
  {-
CC=$CC
CPPFLAGS=$CPPFLAGS
CFLAGS=$CFLAGS
LD=$LD
LDFLAGS=$LDFLAGS
AR=$AR
ARFLAGS=$ARFLAGS
RANLIB=$RANLIB
WINDRES=$WINDRES
cursesLib=$cursesLib
ncursesLib=$ncursesLib
zlibInc=$zlibInc
LZ4Inc=$LZ4Inc
zlibDep=$zlibDep
LZ4Dep=$LZ4Dep
zlibLib=$zlibLib
LZ4Lib=$LZ4Lib
zlibHeaderDep=$zlibHeaderDep
LZ4HeaderDep=$LZ4HeaderDep
Kernel=\${${Kernel}}
KernelLinkDeps=\${${Kernel}LinkDeps}
KernelLinkLibs=\${${Kernel}LinkLibs}
-}

        
data Config = Config
  {m :: String
  ,w :: String
  ,cc :: String
  ,cppflags :: [String]
  ,cflags :: [String]
  ,ld :: String
  ,ldflags :: [String]
  ,ar :: String
  ,arflags :: [String]
  ,ranlib :: String
  ,windres :: String
  ,cursesLib :: String
  ,ncursesLib :: String
  ,zlibInc :: FilePath
  ,lz4Inc :: FilePath
  ,zlibDep :: FilePath
  ,lz4Dep :: FilePath
  ,zlibLib :: FilePath
  ,lz4Lib :: FilePath
  ,zLibHeaderDep :: [FilePath]
  ,lz4HeaderDep :: [FilePath]
  ,kernel :: Either Variable String
  ,kernelLinkDeps :: Either Variable [String]
  ,kernelLinkLibs :: Either Variable [String]
  -- for installation
  ,installBin :: String -- todo fix related bugs
  ,installLib :: String
  ,installMan :: String
  ,installOwner :: String
  ,installGroup :: String
  ,tempRoot :: String
  ,gZipManPages :: Bool
  ,installSchemeName :: String
  ,installPetiteName :: String
  ,installScriptName :: String
  ,installKernelTarget :: Either Variable String
  ,installzLibTarget :: Either Variable String
  ,installlz4Target :: Either Variable String
  } deriving (Show, Read)
  
data BITS = BITS64 | BITS32 deriving Eq
          
data ConfigArgs = ConfigArgs
  {m_ :: String -- x
  ,workArea :: String -- x
  ,threads :: Bool -- x
  ,tempRoot_ :: String -- x
  ,installPrefix :: String -- x
  ,installOwner_ :: String -- x
  ,installGroup_ :: String -- x
  ,installBin_ :: String -- x
  ,installLib_ :: String -- x
  ,installMan_ :: String -- x
  ,installSchemeName_ :: String -- x
  ,installPetiteName_ :: String -- x
  ,installScriptName_ :: String -- x
  ,gzipManPages :: Bool -- x
  ,disableX11 :: Bool -- x
  ,disableCurses :: Bool -- x
  ,cc_ :: String -- x
  ,cppFlags_ :: String -- x
  ,cFlags_ :: String -- x
  ,ld_ :: String -- x
  ,ldFlags_ :: String -- x
  ,ar_ :: String -- x
  ,arFlags_ :: String -- x
  ,ranlib_ :: String -- x 
  ,windres_ :: String -- x
  ,zlibInc_ :: FilePath -- x
  ,lz4Inc_ :: FilePath -- x
  ,zlibDep_ :: FilePath -- x
  ,lz4Dep_ :: FilePath -- x
  ,zlibLib_ :: FilePath -- x
  ,lz4Lib_ :: FilePath -- x
  ,zLibHeaderDep_ :: [FilePath] -- x
  ,lz4HeaderDep_ :: [FilePath] -- x
  ,kernel_ :: String -- x
  ,installKernelTarget_ :: Either Variable String -- x
  ,installzLibTarget_ :: Either Variable String -- x
  ,installlz4Target_ :: Either Variable String -- x
  ,bits :: BITS -- x
  }

-- the flags
mFlag :: [String] -> Flag ConfigArgs
mFlag machs = flagReq ["m", "machine"] update "machine type" "explicitly specify machine type"
  where update val config = if elem val machs
                            then Right config{m_=val}
                            else Left $ "no suitable machine type found; available machine types: " ++ show machs

threadsFlag :: Flag ConfigArgs
threadsFlag = flagNone ["threads"] update "specify threaded version"
  where update config = config{threads = True}

bitsFlag :: Flag ConfigArgs
bitsFlag = flagReq ["bits"] update "64 | 32" "specify 32/64-bit version"
  where update "64" config = Right config{bits=BITS64}
        update "32" config = Right config{bits=BITS32}
        update _ config = Left "choose either 32 or 64 bits"

installPrefixFlag :: Flag ConfigArgs
installPrefixFlag = flagReq ["installprefix"] update "pathname" "final installation root"
  where update v config = Right config{installPrefix=v}

installBinFlag :: Flag ConfigArgs
installBinFlag = flagReq ["installbin"] update "pathname" "bin directory"
  where update v config = Right config{installBin_=v}

installLibFlag :: Flag ConfigArgs
installLibFlag = flagReq ["installlib"] update "pathname" "lib directory"
  where update v config = Right config{installLib_=v}

installManFlag :: Flag ConfigArgs
installManFlag = flagReq ["installman"] update "pathname" "manpage directory"
  where update v config = Right config{installMan_=v}

installOwnerFlag :: Flag ConfigArgs
installOwnerFlag = flagReq ["installowner"] update "ownername" "install with owner"
  where update v config = Right config{installOwner_=v}

installGroupFlag :: Flag ConfigArgs
installGroupFlag = flagReq ["installgroup"] update "groupname" "install with group"
  where update v config = Right config{installGroup_=v}

installSchemeNameFlag :: Flag ConfigArgs
installSchemeNameFlag = flagReq ["installschemename"] update "schemename" "install with group"
  where update v config = Right config{installSchemeName_=v}

installPetiteNameFlag :: Flag ConfigArgs
installPetiteNameFlag = flagReq ["installpetitename"] update "petitename" "install with group"
  where update v config = Right config{installPetiteName_=v}

installScriptNameFlag :: Flag ConfigArgs
installScriptNameFlag = flagReq ["installscriptname"] update "scriptname" "install with group"
  where update v config = Right config{installScriptName_=v}

toolPrefixFlag :: Flag ConfigArgs
toolPrefixFlag = flagReq ["toolprefix"] update "prefix" "prefix tool (compiler, linker, ...) names"
  where update v config@ConfigArgs{..} = Right config{cc_=v ++ cc_, ld_ = v ++ ld_, ar_ = v ++ ar_
                                                 ,ranlib_ = v ++ ranlib_, windres_ = v ++ windres_}

gzipManPagesFlag :: Flag ConfigArgs
gzipManPagesFlag = flagReq ["gzip-man-pages"] update "yes | no" "compress manual pages"
  where update "no" config = Right config{gzipManPages = False}
        update "yes" config = Right config{gzipManPages = True}
        update _ config = Left "expected yes or no"

tempRootFlag :: Flag ConfigArgs
tempRootFlag = flagReq ["temproot"] update "pathname" "staging root"
  where update v config = Right config{tempRoot_=v}

workAreaFlag :: Flag ConfigArgs
workAreaFlag = flagReq ["workarea"] update "pathname" "build directory"
  where update v config = Right config{workArea=v}

disableX11Flag :: Flag ConfigArgs
disableX11Flag = flagNone ["disable-x11"] update "disable X11 support"
  where update config = config{disableX11 = True}

disableCursesFlag :: Flag ConfigArgs
disableCursesFlag = flagNone ["disable-curses"] update "disable [n]curses support"
  where update config = config{disableCurses = True}

libKernelFlag :: Flag ConfigArgs
libKernelFlag = flagNone ["libkernel"] update "build libkernel.a (the default)"
  where update config@ConfigArgs{..} =
          config{kernel_="kernelLib", installKernelTarget_= Left "installKernelLib"
                ,installzLibTarget_ = if (zlibInc_ /= "")
                                     then Left "installZLib"
                                     else installzLibTarget_
                ,installlz4Target_ = if (lz4Inc_ /= "")
                                    then Left "installlz4"
                                    else installlz4Target_}

kernelObjFlag :: Flag ConfigArgs
kernelObjFlag = flagNone ["kernelobj"] update "build kernel.o instead of libkernel.a"
  where update config = config{kernel_="kernelO", installKernelTarget_=Left "installKernelObj"
                              ,installzLibTarget_=Right "", installlz4Target_=Right ""}

ccFlag :: Flag ConfigArgs
ccFlag = flagReq ["CC"] update "C compiler" "C compiler"
  where update v config = Right config{cc_=v}

cppFlagsFlag :: Flag ConfigArgs
cppFlagsFlag = flagReq ["CPPFLAGS"] update "C preprocessor flags" "additional C preprocessor flags"
  where update v config = Right config{cppFlags_=v}

cFlagsFlag :: Flag ConfigArgs
cFlagsFlag = flagReq ["CFLAGS"] update "C compiler flags" "additional C compiler flags"
  where update v config = Right config{cFlags_=v}

ldFlag :: Flag ConfigArgs
ldFlag = flagReq ["LD"] update "linker" "linker"
  where update v config = Right config{ld_=v}

ldFlagsFlag :: Flag ConfigArgs
ldFlagsFlag = flagReq ["LDFLAGS"] update "linker flags" "additional linker flags"
  where update v config = Right config{ldFlags_=v}

arFlag :: Flag ConfigArgs
arFlag = flagReq ["AR"] update "archiver" "archiver"
  where update v config = Right config{ar_=v}

arFlagsFlag :: Flag ConfigArgs
arFlagsFlag = flagReq ["ARFLAGS"] update "archiver flags" "archiver flags"
  where update v config = Right config{arFlags_=v}

ranLibFlag :: Flag ConfigArgs
ranLibFlag = flagReq ["RANLIB"] update "archive indexer" "archive indexer"
  where update v config = Right config{ranlib_=v}

windresFlag :: Flag ConfigArgs
windresFlag = flagReq ["WINDRES"] update "resource compiler" "resource compiler"
  where update v config = Right config{windres_=v}

zLibFlag :: Flag ConfigArgs
zLibFlag = flagReq ["ZLIB"] update "lib" "link to <lib> instead of own zlib"
  where update v config = Right config{zlibLib_=v, zlibInc_="", zlibDep_="", zLibHeaderDep_=[]
                                      ,installzLibTarget_=Right ""}

lz4Flag :: Flag ConfigArgs
lz4Flag = flagReq ["LZ4"] update "lib" "link to <lib> instead of own LZ4"
  where update v config = Right config{lz4Lib_=v, lz4Inc_="", lz4Dep_="", lz4HeaderDep_=[]
                                      ,installlz4Target_=Right ""}
          
cargs :: [String] -> ConfigArgs -> Mode ConfigArgs
cargs machs ic = (modeEmpty ic)
           {modeNames=["explicit"], modeHelp= "help"
           ,modeGroupFlags = toGroup [mFlag machs, threadsFlag, bitsFlag, installPrefixFlag
                                     ,installBinFlag, installLibFlag, installManFlag
                                     ,installOwnerFlag, installGroupFlag, installSchemeNameFlag
                                     ,installPetiteNameFlag, installScriptNameFlag, toolPrefixFlag
                                     ,gzipManPagesFlag, tempRootFlag, workAreaFlag, disableX11Flag
                                     ,disableCursesFlag, libKernelFlag, kernelObjFlag, ccFlag
                                     ,cppFlagsFlag, cFlagsFlag, ldFlag, ldFlagsFlag, arFlag
                                     ,arFlagsFlag, ranLibFlag, windresFlag, zLibFlag, lz4Flag
                                     ,flagHelpSimple id]}

-- run command and on success do success otherwise do failure
f :: [String] -> IO b -> IO b -> IO b
f c success failure = do
  Exit e <- cmd Shell c
  case e of
    ExitSuccess -> success
    _ -> failure

configure :: IO Config
configure = initConfig
