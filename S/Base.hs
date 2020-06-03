
module S.Base(build) where

import qualified System.Directory as D
import System.FilePath
import System.Info.Extra
import Control.Monad.Extra
import System.Posix.Files
import System.Exit

import Development.Rattle
import Development.Shake

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
  cmd $ ["rm", "-f"] ++ ms ++ ["xpatch", patch] ++ patches ++ sos ++ asms ++ ["script.all", "header.tmp"] ++ htmls
  cmd ["rm", "-rf", "nanopass"]
  -- saveboot
  cmd ["cp", "-p", "-f", tmppetiteBoot, "../boot" </> m </> "sbb"]
  cmd ["cp", "-p", "-f", tmpschemeBoot, "../boot" </> m </> "scb"]
  -- make all : bootall cheader cequates revision
  -- bootall: allsrc patchfile macroobj nanopass.so makescript
  --macroobj: cmacros.so priminfo.so primvars.so env.so setup.so
  let f d2 ls so = cmd (AddEnv "SCHEMEHEAPDIRS" d2) (AddEnv "CHEZSCHEMELIBDIRS" ".") Shell $ ["echo", "'(reset-handler abort)'"
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
                             ,"'(compile-file \""  ++ so-<.>"ss" ++ "\" \"" ++ so ++ "\")'"
                             ,"|", scheme, "-q"] ++ ls
  f ("../boot" </> m </> "tmp")[] "cmacros.so" -- cmacros.so
  f ("../boot" </> m </> "tmp")["cmacros.so"] "priminfo.so" --priminfo.so
  mapM_ (f ("../boot" </> m </> "tmp") ["cmacros.so", "priminfo.so"]) ["primvars.so", "env.so", "setup.so"]
  -- nanopass.so
  cmd (AddEnv "SCHEMEHEAPDIRS" ("../boot" </> m </> "tmp")) (AddEnv "CHEZSCHEMELIBDIRS" ".") Shell ["echo", "'(reset-handler abort)'"
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
             ,"'(compile-library \"../nanopass/nanopass.ss\" \"nanopass.so\")'"
             ,"|", scheme, "-q", "--libdirs", "\"../nanopass" ++ dirsep ++ dirsep ++ ".\""
             ,"--compile-imported-libraries"]
  -- makescript
  cmd (AddEnv "SCHEMEHEAPDIRS" ("../boot" </> m)) (AddEnv "CHEZSCHEMELIBDIRS" ".") Shell
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
    ,"'(time (for-each (lambda (x y) (collect 1 2) (" ++ compile ++ " (symbol->string x) (symbol->string y) (quote " ++ m ++ "))) (quote (" ++ unwords src ++ ")) (quote (" ++ unwords (obj m) ++ "))))'"
    ,"'(when #" ++ pps ++ " (#%$print-pass-stats))'"
    ,"'(apply #%$make-boot-file \"" ++ petiteBoot ++ "\" (quote " ++ m ++ ") (quote ()) (map symbol->string (quote (" ++ unwords (baseobj m) ++ "))))'"
    ,"'(apply #%$make-boot-file \"" ++ schemeBoot ++ "\" (quote " ++ m ++ ") (quote (\"petite\")) (map symbol->string (quote (" ++ unwords (compilerobj m) ++ "))))'"
    ,"'(when #" ++ pdhtml ++ " (profile-dump-html))'"
    ,"'(when #" ++ dumpspd ++ " (profile-dump-data \"" ++ profileDumpSource ++ "\"))'"
    ,"'(when #" ++ dumpbpd ++ " (profile-dump-data \"" ++ profileDumpBlock ++ "\"))'"
    ,">", "script.all"]

  cmd (AddEnv "SCHEMEHEAPDIRS" ("../boot" </> m)) (AddEnv "CHEZSCHEMELIBDIRS" ".") $ [scheme, "-q"] ++ macroobj ++ ["--script", "script.all"] -- patchfile goes before --script but its empty so omitted
  -- cheader
  f ("../boot" </> m)["cmacros.so", "priminfo.so", "primvars.so", "env.so"] "mkheader.so"
  cmd (AddEnv "SCHEMEHEAPDIRS" ("../boot" </> m)) (AddEnv "CHEZSCHEMELIBDIRS" ".") Shell $
    ["(","if", "[", "-r", cheader, "];", "then", "mv", "-f", cheader, cheader <.> "bak", ";", "fi)", "&&","echo", "'(reset-handler abort) (mkscheme.h \"" ++ cheader ++ "\" (quote " ++ m ++ "))'"
               ,"|", scheme, "-q"] ++ macroobj ++ ["mkheader.so", "&&", "(if", "`cmp", "-s", cheader, cheader <.> "bak" ++ "`;", "then", "mv", "-f", cheader <.> "bak", cheader ++ ";", "else", "rm", "-f", cheader <.> "bak" ++ ";", "fi)"]
  -- cequates
  cmd (AddEnv "SCHEMEHEAPDIRS" ("../boot" </> m)) (AddEnv "CHEZSCHEMELIBDIRS" ".") Shell $
    ["(", "if", "[", "-r", cequates, "];", "then", "mv", "-f", cequates, cequates <.> "bak", ";", "fi)", "&&", "echo", "'(reset-handler abort) (mkequates.h \"" ++ cequates ++ "\")'"
               ,"|", scheme, "-q"] ++ macroobj ++ ["mkheader.so", "&&", "(if", "`cmp", "-s", cequates, cequates <.> "bak" ++ "`;", "then", "mv", "-f", cequates <.> "bak", cequates ++ ";", "else", "rm", "-f", cequates <.> "bak" ++ ";", "fi)"]

  -- if make checkboot > blah blah then fine
  cmd (AddEnv "SCHEMEHEAPDIRS" ("../boot" </> m)) (AddEnv "CHEZSCHEMELIBDIRS" ".") Shell ["echo", "'(reset-handler abort)'"
                         ,"'(base-exception-handler (lambda (c) (fresh-line) (display-condition c) (newline) (reset)))'"
                         ,"'(begin'"
                         ,"'(#%$fasl-file-equal? \"../boot" </> m </> "sbb\"", "\"../boot" </> m </> "petite.boot\"", "#t)'"
                         ,"'(#%$fasl-file-equal? \"../boot" </> m </> "scb\"", "\"../boot" </> m </> "scheme.boot\"", "#t)'"
                         ,"'(printf \"bootfile comparison succeeded\n\"))'"
                         ,"|", "../bin" </> m </> "scheme" ++ exeSuffix, "-b", "../boot" </> m </> "sbb", "-q", ";", "if", "$?", ";", "then", "echo 'bootstrap succeeded'", ";", "(exit $?)", ";", "else", "echo 'failed to bootstrap'", ";", "(exit $?)", ";", "fi"]
                         
  
