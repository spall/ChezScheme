{-# LANGUAGE RecordWildCards #-}

module S.Base(build) where

import qualified System.Directory as D
import System.FilePath
import System.Info.Extra
import Control.Monad.Extra
import System.Posix.Files
import System.Exit
import Data.List.Extra

import Development.Rattle
import Development.Shake

-- revision m = "../boot" </> m </> "revision"

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

baseobj m = map (-<.> m) basesrc

compilersrc = ["cpnanopass.ss", "compile.ss", "cback.ss"]
compilerobj m = map (-<.> m) compilersrc

source aic = allsrc aic ++ ["mkheader.ss"]
allsrc :: [FilePath] -> [FilePath]
allsrc archincludes = basesrc ++ compilersrc ++ ["cmacros.ss"] ++ archincludes
  ++ ["setup.ss", "debug.ss", "priminfo.ss", "primdata.ss", "layout.ss", "base-lang.ss"
     ,"expand-lang.ss", "primref.ss", "types.ss", "io-types.ss", "fasl-helpers.ss"
     ,"hashtable-types.ss", "np-languages.ss", "bitset.ss", "fxmap.ss"]

macroobj = ["cmacros.so", "priminfo.so", "primvars.so", "env.so", "setup.so"]

patchfile = ""
patch = "patch"
petiteBoot_ m = "../boot" </> m </> "petite.boot"
schemeBoot_ m = "../boot" </> m </> "scheme.boot"
tmppetiteBoot_ m = "../boot" </> m </> "tmp" </> "petite.boot"
tmpschemeBoot_ m = "../boot" </> m </> "tmp" </> "scheme.boot"
cheader_ m = "../boot" </> m </> "scheme.h"
cequates_ m = "../boot" </> m </> "equates.h"
revision_ m = "../boot" </> m </> "revision"
o = "3"
d = "0"
cl = "(commonization-level)"
cc = "t"
xf = "(compress-format)"
xl = "(compress-level)"
i = "f"
scheme_ m exeSuffix = "../bin" </> m </> "scheme" ++ exeSuffix
p = "f"
bp = "f"
loadspd = "f"
profileDumpSource = "source.pd"
loadbpd = "f"
profileDumpBlock = "block.pd"
gac = "f"
gic = "f"
pps = "f"
--makeBootFile
--baseobj
--compilerobj
pdhtml = "f"
dumpspd = "f"
dumpbpd = "f"
cp0 = "2"
compile = "compile-file"
src = basesrc ++ compilersrc
obj m = baseobj m ++ compilerobj m

data Config = Config {m :: String
                     ,petiteBoot :: String
                     ,schemeBoot :: String
                     ,tmppetiteBoot :: String
                     ,tmpschemeBoot :: String
                     ,scheme :: String
                     ,dirsep :: String
                     ,exeSuffix :: String
                     ,cheader :: String
                     ,cequates :: String
                     ,revision :: String}


build :: String -> [FilePath] -> Run ()
build m aic = withCmdOptions [Cwd "s"] $ do
  let petiteBoot = petiteBoot_ m
      schemeBoot = schemeBoot_ m
      tmppetiteBoot = tmppetiteBoot_ m
      tmpschemeBoot = tmpschemeBoot_ m
      scheme = scheme_ m exeSuffix
      dirsep = if isWindows then ";" else ":"
      exeSuffix = if isWindows then ".exe" else ""
      cheader = cheader_ m
      cequates = cequates_ m
      revision = revision_ m
      config = Config{..}
  mapM_ (\f -> if isWindows
               then cmd ["cp", "-p", "../../s" </> f, f]
               else liftIO $ D.withCurrentDirectory "s" $ ifM (D.doesPathExist f)
                    (return ())
                    (createSymbolicLink ("../../s" </> f) f)) $
    (source aic) ++ ["update-revision"]
    
  -- revision 
  cmd Shell ["./update-revision", ">", revision]
  -- make allx
  -- prettyclean
  ms <- liftIO $ getDirectoryFilesIO "s" ["*" <.> m]
  patches <- liftIO $ getDirectoryFilesIO "s" ["*.patch"]
  sos <- liftIO $ getDirectoryFilesIO "s" ["*.so"]
  asms <- liftIO $ getDirectoryFilesIO "s" ["*.asm"]
  htmls <- liftIO $ getDirectoryFilesIO "s" ["*.html"]
  -- cmd $ ["rm", "-f"] ++ ms ++ ["xpatch", patch] ++ patches ++ sos ++ asms ++ ["script.all", "header.tmp"] ++ htmls
  cmd ["rm", "-rf", "nanopass"]
  -- saveboot
  cmd ["cp", "-p", "-f", tmppetiteBoot, "../boot" </> m </> "sbb"]
  cmd ["cp", "-p", "-f", tmpschemeBoot, "../boot" </> m </> "scb"]

  -- BEGIN attempt 1

  liftIO $ D.withCurrentDirectory "s" $ D.createDirectoryIfMissing False "a1"
  e <- attempt1 config
  if e then do
    doCopy
    else do
    liftIO $ D.withCurrentDirectory "s" $ D.createDirectoryIfMissing False "a2"
    e2 <- attempt2 config
    if e2 then do
      doCopy
      else do
      liftIO $ D.withCurrentDirectory "s" $ D.createDirectoryIfMissing False "a3"
      e3 <- attempt3 config
      if e3 then do
        doCopy
        else do
        liftIO $ die "fail"

{-
  if e then do
    liftIO $ die "fail"
    liftIO $ D.withCurrentDirectory "s" $ D.createDirectoryIfMissing False "a2"
    e2 <- attempt2 config
    if e2 then do
      liftIO $ D.withCurrentDirectory "s" $ D.createDirectoryIfMissing False "a3"
      e3 <- attempt3 config
      if e3 then do liftIO $ putStrLn "bootstrap succeeded"
                    doCopy
        else liftIO $ putStrLn "bootstrapping failed"
      else doCopy
    else doCopy
-}
attempt1 :: Config -> Run Bool
attempt1 Config{..} = do
  
  let scheme_ = scheme 
      schemeBoot = "../boot" </> m </> "scheme.boot" -- prev version
      schemeBoot_ = "a1" </> "scheme.boot"
      petiteboot = "../boot" </> m </> "petite.boot" -- prev version
      petiteBoot_ = "a1" </> "petite.boot"
      cheader_ = "a1" </> "scheme.h"
      cheader2_ = "a1" </> "scheme.h.bak"
      cequates_ = "a1" </> "equates.h"
      cequates2_ = "a1" </> "equates.h.bak"
  -- make all : bootall cheader cequates revision
  -- bootall: allsrc patchfile macroobj nanopass.so makescript
  --macroobj: cmacros.so priminfo.so primvars.so env.so setup.so
  let f d2 ls so = cmd (AddEnv "SCHEMEHEAPDIRS" d2) (AddEnv "CHEZSCHEMELIBDIRS" "..") Shell $ ["echo", "'(reset-handler abort)'"
                             ,"'(base-exception-handler (lambda (c) (fresh-line) (display-condition c) (newline) (reset)))'"
                             ,"'(keyboard-interrupt-handler (lambda () (display \"interrupted---aborting\\n\") (reset)))'"
                             ,"'(optimize-level " ++ o ++ ")'"
                             ,"'(debug-level " ++ d ++ ")'"
                             ,"'(commonization-level " ++ cl ++ ")'"
                             ,"'(compile-compressed #" ++ cc ++ ")'"
                             ,"'(compress-format " ++ xf ++ ")'"
                             ,"'(compress-level " ++ xl ++ ")'"
                             ,"'(generate-inspector-information #" ++ i ++ ")'"
                             ,"'(subset-mode (quote system))'"
                             ,"'(compile-file \""  ++ so-<.>"ss" ++ "\" \"" ++ "a1" </> so ++ "\")'"
                             ,"|", scheme_, "-q"] ++ ls
  f ("../boot" </> m </> "tmp")[] "cmacros.so" -- cmacros.so
  f ("../boot" </> m </> "tmp")["a1/cmacros.so"] "priminfo.so" --priminfo.so
  mapM_ (f ("../boot" </> m </> "tmp") ["a1/cmacros.so", "a1/priminfo.so"]) ["primvars.so", "env.so", "setup.so"]
  -- nanopass.so
  cmd (AddEnv "SCHEMEHEAPDIRS" ("../boot" </> m </> "tmp")) (AddEnv "CHEZSCHEMELIBDIRS" "a1") Shell ["echo", "'(reset-handler abort)'"
             ,"'(base-exception-handler (lambda (c) (fresh-line) (display-condition c) (newline) (reset)))'"
             ,"'(keyboard-interrupt-handler (lambda () (display \"interrupted---aborting\n\") (reset)))'"
             ,"'(optimize-level " ++ o ++ ")'"
             ,"'(debug-level " ++ d ++ ")'"
             ,"'(commonization-level " ++ cl ++ ")'"
             ,"'(compile-compressed #" ++ cc ++ ")'"
             ,"'(compress-format " ++ xf ++ ")'"
             ,"'(compress-level " ++ xl ++ ")'"
             ,"'(generate-inspector-information #" ++ i ++ ")'"
             ,"'(collect-trip-bytes (expt 2 24))'"
             ,"'(collect-request-handler (lambda () (collect 0 1)))'"
             ,"'(collect 1 2)'"
             ,"'(compile-library \"../../nanopass/nanopass.ss\" \"a1/nanopass.so\")'"
             ,"|", scheme_, "-q", "--libdirs", "\"../nanopass" ++ dirsep ++ dirsep ++ ".\""
             ,"--compile-imported-libraries"]
  -- makescript
  cmd (AddEnv "SCHEMEHEAPDIRS" ("../boot" </> m)) (AddEnv "CHEZSCHEMELIBDIRS" "a1:.") Shell
    ["echo", "'(reset-handler abort)'"
    ,"'(for-each load (command-line-arguments))'"
    ,"'(optimize-level " ++ o ++ ")'"
    ,"'(debug-level " ++ d ++ ")'"
    ,"'(commonization-level " ++ cl ++ ")'"
    ,"'(compile-compressed #" ++ cc ++ ")'"
    ,"'(compress-format " ++ xf ++ ")'"
    ,"'(compress-level " ++ xl ++ ")'"
    ,"'(when #" ++ p ++ " (compile-profile (quote source)))'"
    ,"'(when #" ++ bp ++ " (compile-profile (quote block)))'"
    ,"'(when #" ++ loadspd ++ " (profile-load-data \"" ++ profileDumpSource ++ "\"))'"
    ,"'(when #" ++ loadbpd ++ " (profile-load-data \"" ++ profileDumpBlock ++ "\"))'"
    ,"'(generate-inspector-information #" ++ i ++ ")'"
    ,"'(generate-allocation-counts #" ++ gac ++ ")'"
    ,"'(generate-instruction-counts #" ++ gic ++ ")'"
    ,"'(#%$enable-pass-timing  #" ++ pps ++ ")'"
    ,"'(run-cp0 (lambda (cp0 x) (do ([i " ++ cp0 ++ " (fx- i 1)] [x x (cp0 x)]) ((fx= i 0) x))))'"
    ,"'(collect-trip-bytes (expt 2 24))'"
    ,"'(collect-request-handler (lambda () (collect 0 1)))'"
    ,"'(time (for-each (lambda (x y) (collect 1 2) (" ++ compile ++ " (symbol->string x) (symbol->string y) (quote " ++ m ++ "))) (quote (" ++ unwords src ++ ")) (quote (" ++ (unwords $ map ("a1" </>) (obj m)) ++ "))))'"
    ,"'(when #" ++ pps ++ " (#%$print-pass-stats))'"
    ,"'(apply #%$make-boot-file \"" ++ petiteBoot_ ++ "\" (quote " ++ m ++ ") (quote ()) (map symbol->string (quote (" ++ (unwords $ map ("a1" </>) (baseobj m)) ++ "))))'"
    ,"'(apply #%$make-boot-file \"" ++ schemeBoot_ ++ "\" (quote " ++ m ++ ") (quote (\"petite\")) (map symbol->string (quote (" ++ (unwords $ map ("a1" </>) (compilerobj m)) ++ "))))'"
    ,"'(when #" ++ pdhtml ++ " (profile-dump-html))'"
    ,"'(when #" ++ dumpspd ++ " (profile-dump-data \"" ++ profileDumpSource ++ "\"))'"
    ,"'(when #" ++ dumpbpd ++ " (profile-dump-data \"" ++ profileDumpBlock ++ "\"))'"
    ,">", "a1/script.all"]

  cmd (AddEnv "SCHEMEHEAPDIRS" ("../boot" </> m)) (AddEnv "CHEZSCHEMELIBDIRS" "a1:.") $ [scheme_, "-q"] ++ map ("a1" </>) macroobj ++ ["--script", "a1/script.all"] -- patchfile goes before --script but its empty so omitted
  -- cheader
  f ("a1")["a1/cmacros.so", "a1/priminfo.so", "a1/primvars.so", "a1/env.so"] "mkheader.so"
  cmd (AddEnv "SCHEMEHEAPDIRS" ("a1")) (AddEnv "CHEZSCHEMELIBDIRS" ".") Shell $
    ["(","if", "[", "-r", cheader_, "];", "then", "mv", "-f", cheader_, cheader2_, ";", "fi)", "&&","echo", "'(reset-handler abort) (mkscheme.h \"" ++ cheader_ ++ "\" (quote " ++ m ++ "))'"
               ,"|", scheme_, "-q"] ++ map (\f -> "a1" </> f) macroobj ++ ["a1/mkheader.so", "&&", "(if", "`cmp", "-s", cheader_, cheader2_ ++ "`;", "then", "mv", "-f", cheader2_, cheader_ ++ ";", "else", "rm", "-f", cheader2_ ++ ";", "fi)"]
  -- cequates
  cmd (AddEnv "SCHEMEHEAPDIRS" ("a1")) (AddEnv "CHEZSCHEMELIBDIRS" ".") Shell $
    ["(", "if", "[", "-r", cequates_, "];", "then", "mv", "-f", cequates_, cequates2_, ";", "fi)", "&&", "echo", "'(reset-handler abort) (mkequates.h \"" ++ cequates_ ++ "\")'"
               ,"|", scheme_, "-q"] ++ map (\f -> "a1" </> f) macroobj ++ ["a1/mkheader.so", "&&", "(if", "`cmp", "-s", cequates_, cequates2_ ++ "`;", "then", "mv", "-f", cequates2_, cequates_ ++ ";", "else", "rm", "-f", cequates2_ ++ ";", "fi)"]

  -- if make checkboot > blah blah then fine
  cmd (AddEnv "SCHEMEHEAPDIRS" ("a1")) (AddEnv "CHEZSCHEMELIBDIRS" ".") Shell ["(", "echo", "'(reset-handler abort)'"
                         ,"'(base-exception-handler (lambda (c) (fresh-line) (display-condition c) (newline) (reset)))'"
                         ,"'(begin'"
                         ,"'(#%$fasl-file-equal? \"" ++ schemeBoot ++ "\"", "\"" ++ schemeBoot_ ++ "\"", "#t)'"
                         ,"'(#%$fasl-file-equal? \"" ++ petiteBoot ++ "\"", "\"" ++ petiteBoot_ ++ "\"", "#t)'"
                         ,"'(printf \"bootfile comparison succeeded\n\"))'"
                         ,"|", "../bin" </> m </> "scheme" ++ exeSuffix, "-b", petiteBoot, "-q", ")", ";", "echo", "$?", ">", "a1/attempt1.ec"]

  e1 <- liftIO $ D.withCurrentDirectory "s" $ readFile "a1/attempt1.ec"
  return $ (trim e1) == "0"


--  attempt2
attempt2 :: Config -> Run Bool
attempt2 Config{..} = do
  
  let scheme_ = scheme
      schemeBoot = "a1" </> "scheme.boot" -- from last attempt
      schemeBoot_ = "a2" </> "scheme.boot"
      petiteBoot_ = "a2" </> "petite.boot"
      petiteBoot = "a1" </> "petite.boot" -- from last attempt
  -- make all : bootall cheader cequates revision
  -- bootall: allsrc patchfile macroobj nanopass.so makescript
  --macroobj: cmacros.so priminfo.so primvars.so env.so setup.so
  let f d2 ls so = cmd (AddEnv "SCHEMEHEAPDIRS" d2) (AddEnv "CHEZSCHEMELIBDIRS" "..") Shell $ ["echo", "'(reset-handler abort)'"
                             ,"'(base-exception-handler (lambda (c) (fresh-line) (display-condition c) (newline) (reset)))'"
                             ,"'(keyboard-interrupt-handler (lambda () (display \"interrupted---aborting\\n\") (reset)))'"
                             ,"'(optimize-level " ++ o ++ ")'"
                             ,"'(debug-level " ++ d ++ ")'"
                             ,"'(commonization-level " ++ cl ++ ")'"
                             ,"'(compile-compressed #" ++ cc ++ ")'"
                             ,"'(compress-format " ++ xf ++ ")'"
                             ,"'(compress-level " ++ xl ++ ")'"
                             ,"'(generate-inspector-information #" ++ i ++ ")'"
                             ,"'(subset-mode (quote system))'"
                             ,"'(compile-file \""  ++ so-<.>"ss" ++ "\" \"" ++ "a2" </> so ++ "\")'"
                             ,"|", scheme_, "-q"] ++ ls
  f ("a1")[] "cmacros.so" -- cmacros.so
  f ("a1")["a2/cmacros.so"] "priminfo.so" --priminfo.so
  mapM_ (f ("a1") ["a2/cmacros.so", "a2/priminfo.so"]) ["primvars.so", "env.so", "setup.so"]
  -- nanopass.so
  cmd (AddEnv "SCHEMEHEAPDIRS" ("a1")) (AddEnv "CHEZSCHEMELIBDIRS" "a2") Shell ["echo", "'(reset-handler abort)'"
             ,"'(base-exception-handler (lambda (c) (fresh-line) (display-condition c) (newline) (reset)))'"
             ,"'(keyboard-interrupt-handler (lambda () (display \"interrupted---aborting\n\") (reset)))'"
             ,"'(optimize-level " ++ o ++ ")'"
             ,"'(debug-level " ++ d ++ ")'"
             ,"'(commonization-level " ++ cl ++ ")'"
             ,"'(compile-compressed #" ++ cc ++ ")'"
             ,"'(compress-format " ++ xf ++ ")'"
             ,"'(compress-level " ++ xl ++ ")'"
             ,"'(generate-inspector-information #" ++ i ++ ")'"
             ,"'(collect-trip-bytes (expt 2 24))'"
             ,"'(collect-request-handler (lambda () (collect 0 1)))'"
             ,"'(collect 1 2)'"
             ,"'(compile-library \"../../nanopass/nanopass.ss\" \"a2/nanopass.so\")'"
             ,"|", scheme_, "-q", "--libdirs", "\"../nanopass" ++ dirsep ++ dirsep ++ ".\""
             ,"--compile-imported-libraries"]
  -- makescript
  cmd (AddEnv "SCHEMEHEAPDIRS" ("a1")) (AddEnv "CHEZSCHEMELIBDIRS" "a2") Shell
    ["echo", "'(reset-handler abort)'"
    ,"'(for-each load (command-line-arguments))'"
    ,"'(optimize-level " ++ o ++ ")'"
    ,"'(debug-level " ++ d ++ ")'"
    ,"'(commonization-level " ++ cl ++ ")'"
    ,"'(compile-compressed #" ++ cc ++ ")'"
    ,"'(compress-format " ++ xf ++ ")'"
    ,"'(compress-level " ++ xl ++ ")'"
    ,"'(when #" ++ p ++ " (compile-profile (quote source)))'"
    ,"'(when #" ++ bp ++ " (compile-profile (quote block)))'"
    ,"'(when #" ++ loadspd ++ " (profile-load-data \"" ++ profileDumpSource ++ "\"))'"
    ,"'(when #" ++ loadbpd ++ " (profile-load-data \"" ++ profileDumpBlock ++ "\"))'"
    ,"'(generate-inspector-information #" ++ i ++ ")'"
    ,"'(generate-allocation-counts #" ++ gac ++ ")'"
    ,"'(generate-instruction-counts #" ++ gic ++ ")'"
    ,"'(#%$enable-pass-timing  #" ++ pps ++ ")'"
    ,"'(run-cp0 (lambda (cp0 x) (do ([i " ++ cp0 ++ " (fx- i 1)] [x x (cp0 x)]) ((fx= i 0) x))))'"
    ,"'(collect-trip-bytes (expt 2 24))'"
    ,"'(collect-request-handler (lambda () (collect 0 1)))'"
    ,"'(time (for-each (lambda (x y) (collect 1 2) (" ++ compile ++ " (symbol->string x) (symbol->string y) (quote " ++ m ++ "))) (quote (" ++ unwords src ++ ")) (quote (" ++ (unwords $ map ("a2" </>) (obj m)) ++ "))))'"
    ,"'(when #" ++ pps ++ " (#%$print-pass-stats))'"
    ,"'(apply #%$make-boot-file \"" ++ petiteBoot_ ++ "\" (quote " ++ m ++ ") (quote ()) (map symbol->string (quote (" ++ (unwords $ map ("a2" </>) (baseobj m)) ++ "))))'"
    ,"'(apply #%$make-boot-file \"" ++ schemeBoot_ ++ "\" (quote " ++ m ++ ") (quote (\"petite\")) (map symbol->string (quote (" ++ (unwords $ map ("a2" </>) (compilerobj m)) ++ "))))'"
    ,"'(when #" ++ pdhtml ++ " (profile-dump-html))'"
    ,"'(when #" ++ dumpspd ++ " (profile-dump-data \"" ++ profileDumpSource ++ "\"))'"
    ,"'(when #" ++ dumpbpd ++ " (profile-dump-data \"" ++ profileDumpBlock ++ "\"))'"
    ,">", "a2/script.all"]

  cmd (AddEnv "SCHEMEHEAPDIRS" ("a1")) (AddEnv "CHEZSCHEMELIBDIRS" "a2:.") $ [scheme_, "-q"] ++ map ("a2" </>) macroobj ++ ["--script", "a2/script.all"] -- patchfile goes before --script but its empty so omitted
  -- cheader
  f ("../boot" </> m)["a2/cmacros.so", "a2/priminfo.so", "a2/primvars.so", "a2/env.so"] "mkheader.so"
  cmd (AddEnv "SCHEMEHEAPDIRS" ("a2")) (AddEnv "CHEZSCHEMELIBDIRS" ".") Shell $
    ["(","if", "[", "-r", cheader, "];", "then", "mv", "-f", cheader, cheader <.> "bak", ";", "fi)", "&&","echo", "'(reset-handler abort) (mkscheme.h \"" ++ cheader ++ "\" (quote " ++ m ++ "))'"
               ,"|", scheme_, "-q"] ++ map (\f -> "a2" </> f) macroobj ++ ["a2/mkheader.so", "&&", "(if", "`cmp", "-s", cheader, cheader <.> "bak" ++ "`;", "then", "mv", "-f", cheader <.> "bak", cheader ++ ";", "else", "rm", "-f", cheader <.> "bak" ++ ";", "fi)"]
  -- cequates
  cmd (AddEnv "SCHEMEHEAPDIRS" ("a2")) (AddEnv "CHEZSCHEMELIBDIRS" ".") Shell $
    ["(", "if", "[", "-r", cequates, "];", "then", "mv", "-f", cequates, cequates <.> "bak", ";", "fi)", "&&", "echo", "'(reset-handler abort) (mkequates.h \"" ++ cequates ++ "\")'"
               ,"|", scheme_, "-q"] ++ map (\f -> "a2" </> f) macroobj ++ ["a2/mkheader.so", "&&", "(if", "`cmp", "-s", cequates, cequates <.> "bak" ++ "`;", "then", "mv", "-f", cequates <.> "bak", cequates ++ ";", "else", "rm", "-f", cequates <.> "bak" ++ ";", "fi)"]

  -- if make checkboot > blah blah then fine
  cmd (AddEnv "SCHEMEHEAPDIRS" ("a2")) (AddEnv "CHEZSCHEMELIBDIRS" ".") Shell ["(", "echo", "'(reset-handler abort)'"
                         ,"'(base-exception-handler (lambda (c) (fresh-line) (display-condition c) (newline) (reset)))'"
                         ,"'(begin'"
                         ,"'(#%$fasl-file-equal? \"" ++ schemeBoot ++ "\"", "\"" ++ schemeBoot_ ++ "\"", "#t)'"
                         ,"'(#%$fasl-file-equal? \"" ++ petiteBoot ++ "\"", "\"" ++ petiteBoot_ ++ "\"", "#t)'"
                         ,"'(printf \"bootfile comparison succeeded\n\"))'"
                         ,"|", "../bin" </> m </> "scheme" ++ exeSuffix, "-b", petiteBoot, "-q", ")", ";", "echo", "$?", ">", "a2/attempt2.ec"]

  e1 <- liftIO $ D.withCurrentDirectory "s" $ readFile "a2/attempt2.ec"
  return $ (trim e1) == "0"

attempt3 :: Config -> Run Bool
attempt3 Config{..} = do
  
  let scheme_ = scheme 
      schemeBoot = "a2" </> "scheme.boot" -- prev version
      schemeBoot_ = "a3" </> "scheme.boot"
      petiteBoot = "a2" </> "petite.boot" -- prev version
      petiteBoot_ = "a3" </> "petite.boot"
      cheader_ = "a3" </> "scheme.h"
      cheader2_ = "a3" </> "scheme.h.bak"
      cequates_ = "a3" </> "equates.h"
      cequates2_ = "a3" </> "equates.h.bak"
  -- make all : bootall cheader cequates revision
  -- bootall: allsrc patchfile macroobj nanopass.so makescript
  --macroobj: cmacros.so priminfo.so primvars.so env.so setup.so
  let f d2 ls so = cmd (AddEnv "SCHEMEHEAPDIRS" d2) (AddEnv "CHEZSCHEMELIBDIRS" "..") Shell $ ["echo", "'(reset-handler abort)'"
                             ,"'(base-exception-handler (lambda (c) (fresh-line) (display-condition c) (newline) (reset)))'"
                             ,"'(keyboard-interrupt-handler (lambda () (display \"interrupted---aborting\\n\") (reset)))'"
                             ,"'(optimize-level " ++ o ++ ")'"
                             ,"'(debug-level " ++ d ++ ")'"
                             ,"'(commonization-level " ++ cl ++ ")'"
                             ,"'(compile-compressed #" ++ cc ++ ")'"
                             ,"'(compress-format " ++ xf ++ ")'"
                             ,"'(compress-level " ++ xl ++ ")'"
                             ,"'(generate-inspector-information #" ++ i ++ ")'"
                             ,"'(subset-mode (quote system))'"
                             ,"'(compile-file \""  ++ so-<.>"ss" ++ "\" \"" ++ "a3" </> so ++ "\")'"
                             ,"|", scheme_, "-q"] ++ ls
  f ("a2")[] "cmacros.so" -- cmacros.so
  f ("a2")["a3/cmacros.so"] "priminfo.so" --priminfo.so
  mapM_ (f "a2" ["a3/cmacros.so", "a3/priminfo.so"]) ["primvars.so", "env.so", "setup.so"]
  -- nanopass.so
  cmd (AddEnv "SCHEMEHEAPDIRS" ("a2")) (AddEnv "CHEZSCHEMELIBDIRS" "a3") Shell ["echo", "'(reset-handler abort)'"
             ,"'(base-exception-handler (lambda (c) (fresh-line) (display-condition c) (newline) (reset)))'"
             ,"'(keyboard-interrupt-handler (lambda () (display \"interrupted---aborting\n\") (reset)))'"
             ,"'(optimize-level " ++ o ++ ")'"
             ,"'(debug-level " ++ d ++ ")'"
             ,"'(commonization-level " ++ cl ++ ")'"
             ,"'(compile-compressed #" ++ cc ++ ")'"
             ,"'(compress-format " ++ xf ++ ")'"
             ,"'(compress-level " ++ xl ++ ")'"
             ,"'(generate-inspector-information #" ++ i ++ ")'"
             ,"'(collect-trip-bytes (expt 2 24))'"
             ,"'(collect-request-handler (lambda () (collect 0 1)))'"
             ,"'(collect 1 2)'"
             ,"'(compile-library \"../../nanopass/nanopass.ss\" \"a3/nanopass.so\")'"
             ,"|", scheme_, "-q", "--libdirs", "\"../nanopass" ++ dirsep ++ dirsep ++ ".\""
             ,"--compile-imported-libraries"]
  -- makescript
  cmd (AddEnv "SCHEMEHEAPDIRS" ("a2")) (AddEnv "CHEZSCHEMELIBDIRS" "a3") Shell
    ["echo", "'(reset-handler abort)'"
    ,"'(for-each load (command-line-arguments))'"
    ,"'(optimize-level " ++ o ++ ")'"
    ,"'(debug-level " ++ d ++ ")'"
    ,"'(commonization-level " ++ cl ++ ")'"
    ,"'(compile-compressed #" ++ cc ++ ")'"
    ,"'(compress-format " ++ xf ++ ")'"
    ,"'(compress-level " ++ xl ++ ")'"
    ,"'(when #" ++ p ++ " (compile-profile (quote source)))'"
    ,"'(when #" ++ bp ++ " (compile-profile (quote block)))'"
    ,"'(when #" ++ loadspd ++ " (profile-load-data \"" ++ profileDumpSource ++ "\"))'"
    ,"'(when #" ++ loadbpd ++ " (profile-load-data \"" ++ profileDumpBlock ++ "\"))'"
    ,"'(generate-inspector-information #" ++ i ++ ")'"
    ,"'(generate-allocation-counts #" ++ gac ++ ")'"
    ,"'(generate-instruction-counts #" ++ gic ++ ")'"
    ,"'(#%$enable-pass-timing  #" ++ pps ++ ")'"
    ,"'(run-cp0 (lambda (cp0 x) (do ([i " ++ cp0 ++ " (fx- i 1)] [x x (cp0 x)]) ((fx= i 0) x))))'"
    ,"'(collect-trip-bytes (expt 2 24))'"
    ,"'(collect-request-handler (lambda () (collect 0 1)))'"
    ,"'(time (for-each (lambda (x y) (collect 1 2) (" ++ compile ++ " (symbol->string x) (symbol->string y) (quote " ++ m ++ "))) (quote (" ++ unwords src ++ ")) (quote (" ++ (unwords $ map ("a3" </>) (obj m)) ++ "))))'"
    ,"'(when #" ++ pps ++ " (#%$print-pass-stats))'"
    ,"'(apply #%$make-boot-file \"" ++ petiteBoot_ ++ "\" (quote " ++ m ++ ") (quote ()) (map symbol->string (quote (" ++ (unwords $ map ("a3" </>) (baseobj m)) ++ "))))'"
    ,"'(apply #%$make-boot-file \"" ++ schemeBoot_ ++ "\" (quote " ++ m ++ ") (quote (\"petite\")) (map symbol->string (quote (" ++ (unwords $ map ("a3" </>) (compilerobj m)) ++ "))))'"
    ,"'(when #" ++ pdhtml ++ " (profile-dump-html))'"
    ,"'(when #" ++ dumpspd ++ " (profile-dump-data \"" ++ profileDumpSource ++ "\"))'"
    ,"'(when #" ++ dumpbpd ++ " (profile-dump-data \"" ++ profileDumpBlock ++ "\"))'"
    ,">", "a3/script.all"]

  cmd (AddEnv "SCHEMEHEAPDIRS" ("a2")) (AddEnv "CHEZSCHEMELIBDIRS" "a3:.") $ [scheme_, "-q"] ++ map ("a3" </>) macroobj ++ ["--script", "a3/script.all"] -- patchfile goes before --script but its empty so omitted -- where we make new .boot files?
  -- cheader
  f ("a3")["a3/cmacros.so", "a3/priminfo.so", "a3/primvars.so", "a3/env.so"] "mkheader.so"
  cmd (AddEnv "SCHEMEHEAPDIRS" ("a3")) (AddEnv "CHEZSCHEMELIBDIRS" ".") Shell $
    ["(","if", "[", "-r", cheader_, "];", "then", "mv", "-f", cheader_, cheader2_, ";", "fi)", "&&","echo", "'(reset-handler abort) (mkscheme.h \"" ++ cheader_ ++ "\" (quote " ++ m ++ "))'"
               ,"|", scheme_, "-q"] ++ map (\f -> "a3" </> f) macroobj ++ ["a3/mkheader.so", "&&", "(if", "`cmp", "-s", cheader_, cheader2_ ++ "`;", "then", "mv", "-f", cheader2_, cheader_ ++ ";", "else", "rm", "-f", cheader2_ ++ ";", "fi)"]
  -- cequates
  cmd (AddEnv "SCHEMEHEAPDIRS" ("a3")) (AddEnv "CHEZSCHEMELIBDIRS" ".") Shell $
    ["(", "if", "[", "-r", cequates_, "];", "then", "mv", "-f", cequates_, cequates2_, ";", "fi)", "&&", "echo", "'(reset-handler abort) (mkequates.h \"" ++ cequates_ ++ "\")'"
               ,"|", scheme_, "-q"] ++ map (\f -> "a3" </> f) macroobj ++ ["a3/mkheader.so", "&&", "(if", "`cmp", "-s", cequates_, cequates2_ ++ "`;", "then", "mv", "-f", cequates2_, cequates_ ++ ";", "else", "rm", "-f", cequates2_ ++ ";", "fi)"]

  -- if make checkboot > blah blah then fine
  cmd (AddEnv "SCHEMEHEAPDIRS" ("a3:.")) (AddEnv "CHEZSCHEMELIBDIRS" ".") Shell ["(", "echo", "'(reset-handler abort)'"
                         ,"'(base-exception-handler (lambda (c) (fresh-line) (display-condition c) (newline) (reset)))'"
                         ,"'(begin'"
                         ,"'(#%$fasl-file-equal? \"" ++ schemeBoot ++ "\"", "\"" ++ schemeBoot_ ++ "\"", "#t)'"
                         ,"'(#%$fasl-file-equal? \"" ++ petiteBoot ++ "\"", "\"" ++ petiteBoot_ ++ "\"", "#t)'"
                         ,"'(printf \"bootfile comparison succeeded\n\"))'"
                         ,"|", "../bin" </> m </> "scheme" ++ exeSuffix, "-b", petiteBoot, "-q", ")", ";", "echo", "$?", ">", "a3/attempt3.ec"]

  e1 <- liftIO $ D.withCurrentDirectory "s" $ readFile "a3/attempt3.ec"
  return $ (trim e1) == "0"


doCopy :: Run ()
doCopy = liftIO $ putStrLn "todo"
