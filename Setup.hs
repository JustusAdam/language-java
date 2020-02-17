import Distribution.Simple
import Distribution.Simple.PreProcess
import System.FilePath ((</>),(<.>))
import System.Directory (renameFile, removeFile)
import Data.List (isPrefixOf, isInfixOf)
import System.Process

cleanup :: FilePath -> IO ()
cleanup parserFile = do
    let altName = parserFile <.> "temp"
    renameFile parserFile altName
    putStrLn "Parser Cleanup Running"
    readFile altName
        >>= writeFile parserFile . unlines . remOffenders . lines
    removeFile altName
  where
    remOffenders = map $ \ l ->
                              if "#if __GLASGOW_HASKELL__ >= 710" `isPrefixOf` l
                              then "#if 0" else l

main = defaultMainWithHooks $ simpleUserHooks
    { hookedPreProcessors =
      [ ("y", \a b c ->
            let pp = ppHappy a b c
            in pp {
                runPreProcessor = \inp outp verb -> do
                        runPreProcessor pp inp outp verb
                        cleanup $ uncurry (</>) outp
                  })
      ]
    }
