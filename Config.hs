
module Config(cc, cppflags, cflags, ld, ldflags, ar, arflags
             ,ranlib, windres, cursesLib, ncursesLib, zlibInc
             ,lZ4Inc, zlibDep, lZ4Dep, zlibLib, lZ4Lib, zlibHeaderDep
             ,lZ4HeaderDep, kernel, kernelLinkDeps, kernelLinkLibs) where

cc = "gcc"
cppflags = ""
cflags = ""
ld = "ld"
ldflags = ""
ar = "ar"
arflags = "rc"
ranlib = "ranlib"
windres = "windres"
cursesLib = "-lcurses"
ncursesLib = "-lncurses"
zlibInc = "-I../zlib"
lZ4Inc ="-I../lz4/lib"
zlibDep = "../zlib/libz.a"
lZ4Dep = "../lz4/lib/liblz4.a"
zlibLib = "../zlib/libz.a"
lZ4Lib = "../lz4/lib/liblz4.a"
zlibHeaderDep = ["../zlib/zconf.h", "../zlib/zlib.h"]
lZ4HeaderDep = ["../lz4/lib/lz4.h", "../lz4/lib/lz4frame.h"]
kernel = kernelLib
kernelLinkDeps = kernelLibLinkDeps
kernelLinkLibs = kernelLibLinkLibs
