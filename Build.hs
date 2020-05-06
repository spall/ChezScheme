
module Build(main) where

import System.Environment
import System.Directory
import System.Exit
import Control.Monad.Extra

import Ta6le
import Configure

machsBuild = let (*) = (,) in
  ["ta6le" * Ta6le.build]

machsInstall = let (*) = (,) in
  ["ta6le" * Ta6le.install]

main :: IO ()
main = do
  args <- getArgs
  config <- case args of
              "--configure":rest -> do
                config <- withArgs rest configure
                ifM (doesFileExist ".config")
                  (do
                      removeFile ".config"
                      writeFile ".config" $ show config)
                  (writeFile ".config" $ show config)
                return config
                  
              _ -> ifM (doesFileExist ".config")
                   (do
                       str <- readFile ".config"
                       return $ read str) -- read config from file
                   (do
                       putStrLn $ "Could not find configuration; please run with --configure"
                       exitFailure)
  putStrLn $ show config

  case lookup (m config) machsBuild of
    Just act -> do
      withCurrentDirectory (m config) $ act config
    _ -> do
      putStrLn $ "Unknown machine type, expected one of\n " ++ unwords (map fst machsBuild)
      exitFailure

  case lookup (m config) machsInstall of
    Just act -> do
      withCurrentDirectory (m config) $ act config
    _ -> do
      putStrLn $ "Unknown machine type, expected one of\n " ++ unwords (map fst machsInstall)
      exitFailure

-- default
{-
  
run :: IO ()
run = todo

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
