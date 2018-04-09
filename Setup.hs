{-# LANGUAGE CPP #-} 

import Distribution.Simple ( defaultMainWithHooks )

import Distribution.Simple.PreProcess 

giCairoRenderUserHooks = simpleUserHooks {
    hookedPreProcessors = [("chs", ourC2hs)]
  }      

main = defaultMainWithHooks giCairoRenderUserHooks 

#if MIN_VERSION_Cabal(2,0,0)
ourC2hs :: BuildInfo -> LocalBuildInfo -> ComponentLocalBuildInfo -> PreProcessor
ourC2hs bi lbi _ = PreProcessor {
#else
ourC2hs :: BuildInfo -> LocalBuildInfo -> PreProcessor
ourC2hs bi lbi = PreProcessor {
#endif
  platformIndependent = False,
  runPreProcessor = runC2HS bi lbi
}  

runC2HS :: BuildInfo -> LocalBuildInfo ->
           (FilePath, FilePath) -> (FilePath, FilePath) -> Verbosity -> IO ()
runC2HS bi lbi (inDir, inFile)  (outDir, outFile) verbosity = do
  -- have the header file name if we don't have the precompiled header yet
  header <- case lookup "x-c2hs-header" (customFieldsBI bi) of
    Just h -> return h
    Nothing -> die ("Need x-c2hs-Header definition in the .cabal Library section "++
                    "that sets the C header file to process .chs.pp files.")

  -- c2hs will output files in out dir, removing any leading path of the input file.
  -- Thus, append the dir of the input file to the output dir.
  let (outFileDir, newOutFile) = splitFileName outFile
  let newOutDir = outDir </> outFileDir
  -- additional .chi files might be needed that other packages have installed;
  -- we assume that these are installed in the same place as .hi files
  let chiDirs = [ dir |
                  ipi <- maybe [] (map fst . componentPackageDeps) (libraryConfig lbi),
                  dir <- maybe [] importDirs (lookupUnitId (installedPkgs lbi) ipi) ]
  (gccProg, _) <- requireProgram verbosity gccProgram (withPrograms lbi)
  unsafeResetRootNameSupply
  c2hsMain $
       map ("--include=" ++) (outDir:chiDirs)
    ++ [ "--cpp=" ++ programPath gccProg, "--cppopts=-E" ]
    ++ ["--cppopts=" ++ opt | opt <- getCppOptions bi lbi]
    ++ ["--output-dir=" ++ newOutDir,
        "--output=" ++ newOutFile,
        "--precomp=" ++ buildDir lbi </> precompFile,
        header, inDir </> inFile]
  return ()      
