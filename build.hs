
module Build() where

import System.Environment

mach = "ta6le"

machs = let (*) = (,) in
  ["ta6le" * 

-- default
build :: IO ()
build = do -- go into machine dir and do build
  -- cd mach/c && make
  withCurrentDirectory (mach "/" "c") $ do
    -- run the appropraite mach build commands
  
  -- cd mach/s && make bootstrap
  
  -- cd mach && make build
  --    cd c && make; cd s && make bootstrap
  
  
run :: IO ()
run = todo

install :: IO ()
install = todo

uninstall :: IO ()
uninstall = todo

test :: IO ()
test = todo

bootfiles :: IO ()
bootfiles = todo

-- Supply XM=<machine> to build boot files for <machine>
boot :: IO ()
boot = todo

-- Supply ORIG=<dir> to build using existing at <dir>
fromOrig :: IO ()
fromOrig = todo

docs :: IO ()
docs = todo

createBintar :: IO ()
createBintar = todo

createRpm :: IO ()
createRpm = todo

createPkg :: IO ()
createPkg = todo

clean :: IO ()
clean = todo

distclean :: IO ()
distclean = todo
