{-# LANGUAGE RecordWildCards #-}

module Ta6le(build, install) where

import System.Directory
import System.Info.Extra
import Development.Rattle
import System.FilePath
import Control.Monad.Extra
import qualified Configure as C
import qualified S.Ta6le as S.Ta6le
import qualified C.Ta6le as C.Ta6le

m = "ta6le"

build :: C.Config -> Run ()
build config = do
  -- cd c && make
  C.Ta6le.build config
  -- cd s && make bootstrap
  S.Ta6le.build

version = "csv9.5.3"
installKernelTarget = "installkernellib"

install :: C.Config -> Run ()
install config@C.Config{..} = do
  let include = "boot" </> m
      petiteBoot = "boot" </> m </> "petite.boot"
      schemeBoot = "boot" </> m </> "scheme.boot"
      revision = "boot" </> m </> "revision"
      scheme = "bin" </> m </> "scheme"
      petite = "bin" </> m </> "petite"
      installLibExamples = installLib </> version </> "examples"
      installLibBin = installLib </> version </> m

      bin = tempRoot ++ installBin
      lib = tempRoot ++ installLib </> version
      libExamples = tempRoot ++ installLibExamples
      libBin = tempRoot ++ installLibBin
      man = tempRoot ++ installMan
      petitePath = bin </> installPetiteName
      schemePath = bin </> installSchemeName
      schemeScriptPath = bin </> installScriptName
      installsh = ["./installsh", "-o", installOwner, "-g", installGroup]
  
-- install: bininstall libbininstall maninstall liblibinstall ${InstallKernelTarget}

  -- bininstall
  cmd $ installsh ++ ["-d", "-m", "755", bin]
  cmd $ installsh ++ ["-m", "555", scheme, schemePath]
  cmd ["ln", "-f", schemePath, petitePath]
  cmd ["ln", "-f", schemePath, schemeScriptPath]

  -- libbininstall
  cmd $ installsh ++ ["-d", "-m", "755", lib]
  cmd $ installsh ++ ["-d", "-m", "755", libBin]
  cmd $ installsh ++ ["-m", "444", petiteBoot, libBin </> "petite.boot"]
  cmd Shell ["if", "[", installPetiteName, "!=", "\"petite\"", "];", "then"
             ,"/bin/rm", "-f", libBin </> installPetiteName <.> "boot" ++ ";"
             ,"ln", "-f", libBin </> "petite.boot", libBin </> installPetiteName <.> "boot" ++ ";", "fi"]
  cmd $ installsh ++ ["-m", "444", schemeBoot, libBin </> "scheme.boot"]
  cmd Shell ["if", "[", installSchemeName, "!=", "\"scheme\"", "];", "then"
             ,"/bin/rm", "-f", libBin </> installSchemeName <.> "boot" ++ ";"
             ,"ln", "-f", libBin </> "scheme.boot", libBin </> installSchemeName <.> "boot" ++ ";", "fi"]
  cmd ["ln", "-f", libBin </> "scheme.boot", libBin </> installScriptName <.> "boot"]
  cmd $ installsh ++ ["-m", "444", include </> "main.o", libBin]
  cmd $ installsh ++ ["-m", "444", include </> "scheme.h", libBin]
  cmd $ installsh ++ ["-m", "444", revision, libBin </> "revision"]

  -- maninstall
  cmd Shell ["sed", "-e", "\"s;" ++ installBin ++ ";" ++ installBin ++ ";g\""
             ,"-e", "\"s;" ++ installLibExamples ++ ";" ++ installLibExamples ++ ";g\""
             ,"-e", "\"s;" ++ installLibBin ++ ";" ++ installLibBin ++ ";g\""
             ,"-e", "\"s;" ++ installPetiteName ++ ";" ++ installPetiteName ++ ";g\""
             ,"-e", "\"s;" ++ installSchemeName ++ ";" ++ installSchemeName ++ ";g\""
             ,"-e", "\"s;" ++ installScriptName ++ ";" ++ installScriptName ++ ";g\""
             ,"scheme.1.in", ">", "scheme.1"]
  cmd Shell ["sed", "-e", "\"s;" ++ installBin ++ ";" ++ installBin ++ ";g\""
             ,"-e", "\"s;" ++ installLibExamples ++ ";" ++ installLibExamples ++ ";g\""
             ,"-e", "\"s;" ++ installLibBin ++ ";" ++ installLibBin ++ ";g\""
             ,"-e", "\"s;" ++ installPetiteName ++ ";" ++ installPetiteName ++ ";g\""
             ,"-e", "\"s;" ++ installSchemeName ++ ";" ++ installSchemeName ++ ";g\""
             ,"-e", "\"s;" ++ installScriptName ++ ";" ++ installScriptName ++ ";g\""
             ,"scheme.1.in", ">", "petite.1"]
  cmd $ installsh ++ ["-d", "-m", "755", man]
  cmd $ installsh ++ ["-m", "444", "scheme.1", man </> installSchemeName <.> "1"]
  let gzmp = if gZipManPages then "yes" else "no"
  whenM (return gZipManPages) $
    cmd ["gzip", "-f", man </> installSchemeName <.> "1"]
  cmd $ installsh ++ ["-m" ,"444", "petite.1", man </> installPetiteName <.> "1"]
  whenM (return gZipManPages) $ 
    cmd ["gzip", "-f", man </> installPetiteName <.> "1"]

  -- liblibinstall
  cmd $ installsh ++ ["-d", "-m", "755", libExamples]
  cmd $ installsh ++ ["-m", "444", "examples" </> "*", libExamples]
  
  -- installKernelTarget: todo
  cmd $ installsh ++ ["-d", "-m", "755", libBin]
  -- installzlib for now but should not hard code this in future
  cmd $ installsh ++ ["-m", "444", "zlib/libz.a", libBin]
  -- same with installlz4
  cmd $ installsh ++ ["-m", "444", "lz4/lib/liblz4.a", libBin]

  cmd $ installsh ++ ["-m", "444", include </> "libkernel.a", libBin]
