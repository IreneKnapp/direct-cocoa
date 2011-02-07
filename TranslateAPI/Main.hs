module Main (main) where

import Control.Monad
import Data.Generics
import qualified Data.List as L
import Prelude hiding (error)
import qualified Prelude as Prelude
import System.Directory
import System.Environment
import System.IO

import Types
import Util
import BridgeSupport


data OperationMode = Help HelpTopic
                   | Report ReportType FilePath
                   | Translate FilePath


data HelpTopic = HelpUsage
               | HelpReports


data ReportType = UnknownLinkageReport


main :: IO ()
main = do
  arguments <- getArgs
  let operationMode
        = case arguments of
            ["help"] -> Help HelpUsage
            ["report"] -> Help HelpReports
            ["report", "unknown-linkage", filename]
              -> Report UnknownLinkageReport filename
            ["translate", directory]
              -> Translate directory
            _ -> Help HelpUsage
  case operationMode of
    Help topic -> help topic
    Report reportType filePath -> do
      banner "Loading API..."
      let architecture = Architecture SixtyFourBit LittleEndian
      frameworks <- processFramework architecture "Cocoa"
      banner "Writing report..."
      withFile filePath WriteMode
               $ (case reportType of
                    UnknownLinkageReport -> unknownLinkageReport)
                 frameworks
      banner "Done."
    Translate directoryPath -> do
      existsAsFile <- doesFileExist directoryPath
      existsAsDirectory <- doesDirectoryExist directoryPath
      if existsAsFile || existsAsDirectory
        then error "Refusing to overwrite preexisting output directory."
        else do
          banner "Loading API..."
          let architecture = Architecture SixtyFourBit LittleEndian
          frameworks <- processFramework architecture "Cocoa"
          banner "Writing translation..."
          translate directoryPath
          banner "Success deserves an exclamation point!"


banner :: String -> IO ()
banner string = do
  putStrLn $ ""
  putStrLn $ ""
  putStrLn $ replicate (length string) '='
  putStrLn $ string
  putStrLn $ replicate (length string) '='
  putStrLn $ ""


help :: HelpTopic -> IO ()
help HelpUsage = do
  programName <- getProgName
  putStrLn $ programName ++ " help"
  putStrLn $ "    This message."
  putStrLn $ programName ++ " report"
  putStrLn $ "    A list of all possible reports."
  putStrLn $ programName ++ " report <report-name> <output.txt>"
  putStrLn $ "    Output the named report to the given file."
  putStrLn $ programName ++ " translate <directory>"
  putStrLn $ "    Create and populate the given directory with "
             ++ "the translated API."
help HelpReports = do
  putStrLn $ "unknown-linkage"
  putStrLn $ "    All type definitions with linkage types that are unknown."
  putStrLn $ "  Useful for maintaining the exceptions file."


unknownLinkageReport :: [Framework] -> Handle -> IO ()
unknownLinkageReport frameworks handle = do
  mapM_ (\framework -> do
           mapM_ (\theType -> do
                    let unknownCount
                          = gcount (mkQ False
                                        $ \linkage ->
                                          case linkage of
                                            UnknownLinkageType -> True
                                            _ -> False)
                                   theType
                    if unknownCount > 0
                      then hPutStrLn handle $ show theType
                      else return ())
                 $ frameworkTypes framework)
        frameworks


translate :: FilePath -> IO ()
translate directoryPath = do
  createDirectory directoryPath


processFramework :: Architecture -> String -> IO [Framework]
processFramework architecture frameworkName = do
  let recoverFrameworkName :: FilePath -> String
      recoverFrameworkName frameworkLocation =
        let lastComponent = case L.elemIndices '/' frameworkLocation of
                              [] -> frameworkLocation
                              slashIndices
                                -> drop (last slashIndices + 1)
                                        frameworkLocation
            frameworkName = case L.elemIndex '.' lastComponent of
                              Nothing -> lastComponent
                              Just index -> take index lastComponent
        in frameworkName
      
      process :: FilePath -> [String] -> IO ([Framework], [String])
      process frameworkLocation visitedFrameworkNames = do
        let frameworkName = recoverFrameworkName frameworkLocation
        if not $ elem frameworkName visitedFrameworkNames
          then do
            maybeBridgeSupportLocation
              <- findBridgeSupport frameworkName frameworkLocation
            case maybeBridgeSupportLocation of
              Nothing -> return ([], visitedFrameworkNames ++ [frameworkName])
              Just bridgeSupportLocation -> do
                putStrLn $ "Processing " ++ frameworkName ++ "."
                framework
                  <- loadBridgeSupport architecture
                                       frameworkName
                                       bridgeSupportLocation
                visitedFrameworkNames
                  <- return $ visitedFrameworkNames ++ [frameworkName]
                (dependencies, visitedFrameworkNames)
                  <- chaseDependencies framework visitedFrameworkNames
                framework <- return $ fixDependencyNames framework
                return ([framework] ++ dependencies, visitedFrameworkNames)
          else return ([], visitedFrameworkNames)
      
      chaseDependencies :: Framework -> [String] -> IO ([Framework], [String])
      chaseDependencies framework visitedFrameworkNames = do
        foldM (\(results, visitedFrameworkNames) dependency -> do
                  if not $ elem (recoverFrameworkName dependency)
                                visitedFrameworkNames
                    then do
                      (result, visitedFrameworkNames)
                        <- process dependency visitedFrameworkNames
                      return (results ++ result,
                              visitedFrameworkNames)
                    else return (results, visitedFrameworkNames))
               ([], visitedFrameworkNames)
               (frameworkDependencies framework)
      
      fixDependencyNames :: Framework -> Framework
      fixDependencyNames framework
        = framework {
              frameworkDependencies
                = map recoverFrameworkName $ frameworkDependencies framework
            }
  
  putStrLn $ "Processing " ++ frameworkName ++ "."
  frameworkLocation <- findFrameworkOrError frameworkName
  bridgeSupportLocation
    <- findBridgeSupportOrError frameworkName frameworkLocation
  framework
    <- loadBridgeSupport architecture frameworkName bridgeSupportLocation
  (dependencies, _) <- chaseDependencies framework [frameworkName]
  framework <- return $ fixDependencyNames framework
  return $ [framework] ++ dependencies


findFramework :: String -> IO (Maybe FilePath)
findFramework frameworkName = do
  home <- getEnv "HOME"
  foldM (\maybeResult parentDirectory -> do
           case maybeResult of
             Just _ -> return maybeResult
             Nothing -> do
               let fullPath = parentDirectory ++ frameworkName ++ ".framework/"
               found <- doesDirectoryExist fullPath
               if found
                 then return $ Just fullPath
                 else return Nothing)
        Nothing
        [home ++ "/Library/Frameworks/",
         "/Library/Frameworks/",
         "/System/Library/Frameworks/"]


findFrameworkOrError :: String -> IO FilePath
findFrameworkOrError frameworkName = do
  maybeLocation <- findFramework frameworkName
  case maybeLocation of
    Nothing -> error $ "Unable to find framework " ++ frameworkName ++ "."
    Just location -> return location


findBridgeSupport :: String -> FilePath -> IO (Maybe FilePath)
findBridgeSupport frameworkName frameworkLocation = do
  frameworkLocation <- return $ case last frameworkLocation of
                         '/' -> frameworkLocation
                         _ -> frameworkLocation ++ "/"
  home <- getEnv "HOME"
  foldM (\maybeResult (parentDirectory, suffix) -> do
           case maybeResult of
             Just _ -> return maybeResult
             Nothing -> do
               let fullPath = parentDirectory
                              ++ frameworkName
                              ++ suffix
                              ++ ".bridgesupport"
               found <- doesFileExist fullPath
               if found
                 then return $ Just fullPath
                 else return Nothing)
        Nothing
        [(frameworkLocation ++ "Resources/BridgeSupport/", "Full"),
         ("/System/Library/BridgeSupport/", "Full"),
         ("/Library/BridgeSupport/", "Full"),
         (home ++ "/Library/BridgeSupport/", "Full"),
         (frameworkLocation ++ "Resources/BridgeSupport/", ""),
         ("/System/Library/BridgeSupport/", ""),
         ("/Library/BridgeSupport/", ""),
         (home ++ "/Library/BridgeSupport/", "")]


findBridgeSupportOrError :: String -> FilePath -> IO FilePath
findBridgeSupportOrError frameworkName frameworkLocation = do
  maybeLocation <- findBridgeSupport frameworkName frameworkLocation
  case maybeLocation of
    Nothing -> error $ "Unable to find bridge support for framework "
                       ++ frameworkName ++ "."
    Just location -> return location
