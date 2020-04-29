
module Build(main) where

import System.Environment
import System.Directory
import System.Exit

import Ta6le
import Configure

machs = let (*) = (,) in
  ["ta6le" * Ta6le.build]

main :: IO ()
main = do
  [mach] <- getArgs
  case lookup mach machs of
    Just act -> do
      withCurrentDirectory mach act
    _ -> do
      putStrLn $ "Unknown machine type, expected one of\n " ++ unwords (map fst machs)
      exitFailure

-- default
{-
build :: String -> IO ()
build = todo
  
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
-}
