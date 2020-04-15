import base.hs -- Mf-base

module Ta6le() where

build :: IO ()
build = do
  -- cd c && make
  buildC
  -- cd s && make bootstrap
  buildS

-- calling bootstrap in ta6le/s
buildS :: IO ()
buildS = withCurrentDirectory "s" $ do
  -- copy source files allsrc
  map (\f -> if isWindows
             then cmd ["cp", "-p", "../../s" </> f, f]
             else cmd ["ln", "-s", "../../s" </> f, f]) allsrc
  -- revision
  cmd Shell ["./update-revision", ">", revision]
  -- allx
  --   saveboot
  cmd ["cp", "-p", "-f", petiteboot, "../boot" </> m </> "sbb"]
  cmd ["cp", "-p", "-f", schemeboot, "../boot" </> m </> "scb"]
  --   all: bootall cheader cequates revision
  --     bootall: allsrc patchfile macroobj nanopass.so makescript
  --       macroobj
  buildso "cmacros.so" []
  buildso "priminfo.so" ["cmacros.so"]
  map (`buildso` ["cmacros.so", "priminfo.so"]) ["primvars.so", "env.so", "setup.so"]
  --       nanopass.so
  cmd Shell $ ["echo", "(reset-handler abort) (base-exception-handler (lambda (c) (fresh-line) (display-condition c) (newline) (reset))) (keyboard-interrupt-handler (lambda () (display \"interrupted---aborting\n\") (reset))) (optimize-level " ++ o ++ ") (debug-level " ++ d ++ ") (commonization-level " ++ cl ++ ")) (compile-compressed #" ++ cc ++ ")) (compress-format " ++ xf ++ ")) (compress-level " ++ xl ++ ")) (generate-inspector-information #" ++ i ++ ") (subset-mode (quote system)) (compile-file \"../nanopass/nanopass.ss\" \"nanopass.so\")", "|", scheme, "-q", "--libdirs", "\"../nanopass" ++ dirsep ++ dirsep ++ ".", "--compile-imported-libraries"]
  --       makescript
  build_makescript
  --     bootall
  cmd [scheme, "-q", macroobj, patchfile, "--script", "script.all"]
  -- end bootall
  --     cheader: mkheader.so ${macroobj} nanopass.so base-lang.ss expand-lang.ss primref.ss types.ss io-types.ss fasl-helpers.ss hashtable-types.ss
  --       mkheader.so
  buildso "mkheader.so" ["cmacros.so", "priminfo.so", "primvars.so", "env.so"]
  --     cheader
  cmd Shell $ "if [ -r" ++ cheader ++ " ]; then mv -f " ++ cheader ++ " " ++ cheader <.> "bak" ++ "; fi"
  cmd Shell ["echo", "(reset-handler abort) (mkscheme.h \"" ++ cheader ++ "\" (quote " ++ m ++ "))", "|", scheme, "-q", macroobj, "mkheader.so"]
  cmd Shell $ "if `cmp -s " ++ cheader ++ " " ++ cheader <.> "bak" ++ "`; then mv -f " ++ cheader <.> "bak" ++ " " ++ cheader ++ "; else rm -f " ++ cheader <.> "bak" ++ "; fi"
  --     cequates
  cmd Shell $ "if [ -r" ++ cequates ++ "]; then mv -f " ++ cequates ++ " " ++ cequates <.> "bak" ++ "; fi"
  cmd Shell ["echo", "(reset-handler abort) (mkequates.h \"" ++ cequates ++ "\")", "|", scheme, "-q", macroobj, "mkheader.so"]
  cmd Shell $ "if `cmp -s " ++ cequates <.> "bak" ++ " " ++ cequates ++ "; else rm -f " ++ cequates <.> "bak" ++ "; fi"
  -- allx
  Exit c <- cmd Shell ["echo", "(reset-handler abort) (base-exception-handler (lambda (c) (fresh-line) (display-condition c) (newline) (reset))) (begin (#%$fasl-file-equal? \"../boot/" ++ m ++ "/sbb\" \"../boot/" ++ m ++ "/scheme.boot\" #t) (#%$fasl-file-equal? \"../boot/" ++ m ++ "/scb\" \"../boot/" ++ m ++ "/scheme.boot\" #t) (printf \"bootfile comparison succeeded\n\"))", "|", "../bin" </> m </> "scheme" ++ exesuffix, "-b", "../boot" </> m </> "sbb", "-q"]
  when (c /= 0) $ error "bootstrapping failed."
  --   restoreboot
  cmd ["mv", "-f", "../boot" </> m </> "sbb", petiteboot]
  cmd ["mv", "-f", "../boot" </> m </> "scb", schemeboot]
  -- resetbootlinks
  when (not isWindows) $
    cmd Shell ["echo", "(reset-handler abort) (for-each (lambda (fn) (let ([fn (symbol->string fn)]) (unless (file-symbolic-link? fn) (when (guard (c [else #f]) (#%$fasl-file-equal? (format \"../~a\" fn) fn)) (system (format \"ln -sf ../../~a ~a\" fn fn)) (void))))) (list (quote " ++ schemeboot ++ ") (quote " ++ petiteboot ++ ")))", "|", scheme, "-q"]


build_makescript :: IO ()
build_makescript = writeFile "script.all" "(reset-handler abort) (for-each load (command-line-arguments)) (optimize-level " ++ o ++ ") (debug-level " ++ d ++ ") (commonization-level " ++ cl ++ ") (compile-compressed #" ++ cc ++ ") (compress-format " ++ xf ++ ") (compress-level " ++ xl ++ ") (when #" ++ p ++ " (compile-profile (quote source))) (when #" ++ bp ++ " (compile-profile (quote block))) (when #" ++ loadspd ++ " (profile-load-data \"" ++ profileDumpSource ++ "\")) (when #" ++ loadbpd ++ " (profile-load-data \"" ++ profileDumpBlock ++ "\")) (generate-inspector-information #" ++ i ++ ") (generate-allocation-counts #" ++ gac ++ ") (generate-instruction-counts #" ++ gic ++ ") (#%$enable-pass-timing #" ++ pps ++ ") (run-cp0 (lambda (cp0 x) (do ([i " ++ cp0 ++ " (fx- i 1)] [x x (cp0 x)]) ((fx= i 0) x)))) (collect-trip-bytes (expt 2 24)) (collect-request-handler (lambda () (collect 0 1))) (time (for-each (lambda (x y) (collect 1 2) (" ++ compile ++ " (symbol->string x) (symbol->string y) (quote " ++ m ++ "))) (quote (" ++ src ++ ")) (quote (" ++ obj ++ ")))) (when #" ++ pps ++ " (#%$print-pass-stats)) (apply #%$make-boot-file \"" ++ petiteBoot ++ "\" (quote " ++ m ++ ") (quote ()) (map symbol->string (quote (" ++ baseobj ++ ")))) (apply #%$make-boot-file \"" ++ schemeBoot ++ "\" (quote " ++ m ++ ") (quote (\"petite\")) (map symbol->string (quote (" ++ compilerobj ++ ")))) (when #" ++ pdhtml ++ " (profile-dump-html)) (when #" ++ dumpspd ++ " (profile-dump-data \"" ++ profileDumpSource ++ "\")) (when #" ++ dumpbpd ++ " (profile-dump-data \"" ++ profileDumpBlock ++ "\"))"

allsrc = basesrc compilersrc ++ ["cmacros.ss"] ++ archincludes ++ ["setup.ss", "debug.ss", "priminfo.ss", "primdata.ss", "layout.ss", "base-lang.ss", "expand-lang.ss", "primref.ss", "types.ss", "io-types.ss", "fasl-helpers.ss", "hashtable-types.ss", "np-languages.ss", "bitset.ss", "fxmap.ss"]

basesrc = ["library.ss", "prims.ss", "mathprims.ss", "record.ss", "5_1.ss", "5_2.ss", "5_3.ss"
          ,"strnum.ss", "bytevector.ss", "5_4.ss", "5_6.ss", "5_7.ss", "event.ss", "4.ss", "front.ss"
          ,"foreign.ss", "6.ss", "print.ss", "newhash.ss", "format.ss", "date.ss", "7.ss", "cafe.ss"
          ,"trace.ss", "engine.ss", "interpret.ss", "cprep.ss", "cpcheck.ss", "cp0.ss", "cpvalid.ss"
          ,"cptypes.ss", "cpcommonize.ss", "cpletrec.ss", "inspect.ss", "enum.ss", "io.ss", "read.ss"
          ,"primvars.ss", "syntax.ss", "costctr.ss", "expeditor.ss", "exceptions.ss", "pretty.ss"
          ,"env.ss", "fasl.ss", "reloc.ss", "pdhtml.ss", "strip.ss", "ftype.ss", "back.ss"]

compilersrc = ["cpnanopass.ss", "compile.ss", "cback.ss"]
archincludes = ["x86_64.ss"]

macroobj = ["cmacros.so", "priminfo.so", "primvars.so", "env.so", "setup.so"]

buildso :: IO ()
buildso so ls = do
  cmd Shell $ ["echo", "(reset-handler abort) (base-exception-handler (lambda (c) (fresh-line) (display-condition c) (newline) (reset))) (keyboard-interrupt-handler (lambda () (display \"interrupted---aborting\n\") (reset))) (optimize-level " ++ o ++ ") (debug-level " ++ d ++ ") (commonization-level " ++ cl ++ ")) (compile-compressed #" ++ cc ++ ")) (compress-format " ++ xf ++ ")) (compress-level " ++ xl ++ ")) (generate-inspector-information #" ++ i ++ ") (subset-mode (quote system)) (compile-file \"" ++ so -<.> "ss" ++ "\" \"" ++ so ++ "\")", "|", scheme, "-q"] ++ ls
            
{- # bootstrap runs allx if any sources have changed since the last bootstrap
bootstrap: ${allsrc} | ${Revision}
	$(MAKE) allx
	touch bootstrap
-}

{- # allx runs all up to three times and checks to see if the new boot file is the
 # same as the last, i.e., the system is properly bootstrapped.
 allx: prettyclean saveboot
	$(MAKE) all
	if $(MAKE) checkboot > /dev/null 2>&1; then echo fine ; else\
          $(MAKE) prettyclean saveboot &&\
          $(MAKE) all &&\
          if $(MAKE) checkboot > /dev/null 2>&1; then echo fine ; else\
            $(MAKE) prettyclean saveboot &&\
            $(MAKE) all &&\
            $(MAKE) checkboot ;\
          fi\
        fi
	$(MAKE) restoreboot
ifneq ($(OS),Windows_NT)
	$(MAKE) resetbootlinks
endif

-}

-- calling make in ta6le/c
buildC :: IO ()
buildC = withCurrentdirectory "c" $ do
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
  co mainsrc
  -- copy main obj to Main
  cmd [cp, "-p", mainobj, main]
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
