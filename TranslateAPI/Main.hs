module Main (main) where

import Control.Monad
import Data.Function
import Data.Generics
import qualified Data.List as L
import Prelude hiding (error)
import qualified Prelude as Prelude
import System.Directory
import System.Environment
import System.IO

import Types
import Util
import Paths
import BridgeSupport
import Reflection


data OperationMode = Help HelpTopic
                   | Report ReportType FilePath
                   | Translate FilePath


data HelpTopic = HelpUsage
               | HelpReports


data ReportType = NamesReport
                | UnknownLinkageReport


main :: IO ()
main = do
  arguments <- getArgs
  let operationMode
        = case arguments of
            ["help"] -> Help HelpUsage
            ["report"] -> Help HelpReports
            ["report", "names", filename]
              -> Report NamesReport filename
            ["report", "unknown-linkage", filename]
              -> Report UnknownLinkageReport filename
            ["translate", directory]
              -> Translate directory
            _ -> Help HelpUsage
  let architecture = Architecture SixtyFourBit LittleEndian
  case operationMode of
    Help topic -> help topic
    Report reportType filePath -> do
      frameworks <- processFramework architecture "Cocoa"
      banner "Writing report..."
      withFile filePath WriteMode
               $ (case reportType of
                    NamesReport -> namesReport
                    UnknownLinkageReport -> unknownLinkageReport)
                 frameworks
      banner "Done."
    Translate directoryPath -> do
      existsAsFile <- doesFileExist directoryPath
      existsAsDirectory <- doesDirectoryExist directoryPath
      if existsAsFile || existsAsDirectory
        then error "Refusing to overwrite preexisting output directory."
        else do
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


hBanner :: Handle -> String -> IO ()
hBanner handle string = do
  hPutStrLn handle $ ""
  hPutStrLn handle $ ""
  hPutStrLn handle $ replicate (length string) '='
  hPutStrLn handle $ string
  hPutStrLn handle $ replicate (length string) '='
  hPutStrLn handle $ ""


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


namesReport :: [Framework] -> Handle -> IO ()
namesReport frameworks handle = do
  mapM_ (\framework -> do
          let typeItems =
                map (\theType ->
                       case theType of
                         OpaqueType name _ _ -> ("opaque", name)
                         StructureType name _ _ -> ("struct", name)
                         CoreFoundationType name _ _ _ -> ("cftype", name))
                    $ frameworkTypes framework
              constantItems =
                map (\(Constant name _ _) -> ("constant", name))
                    $ frameworkConstants framework
              enumItems =
                map (\enum -> case enum of
                                IntEnum name _ -> ("enum", name)
                                FloatEnum name _ -> ("enum", name))
                    $ frameworkEnums framework
              stringConstantItems =
                map (\(StringConstant name _ nsString) ->
                       (if nsString
                          then "nsstring"
                          else "string",
                        name))
                    $ frameworkStringConstants framework
              functionItems =
                map (\(Function name _ _ _ _) -> ("function", name))
                    $ frameworkFunctions framework
              functionAliasItems =
                map (\(FunctionAlias name _) -> ("function", name))
                    $ frameworkFunctionAliases framework
              classItems =
                concat $ map (\theClass ->
                                [("class", className theClass)]
                                ++ (map methodItem $ classMethods theClass))
                             $ frameworkClasses framework
              informalProtocolItems =
                concat $ map (\informalProtocol ->
                                [("informal",
                                  informalProtocolName informalProtocol)]
                                ++ (map methodItem
                                        $ informalProtocolMethods
                                           informalProtocol))
                             $ frameworkInformalProtocols framework
              methodItem method =
                case method of
                  ClassMethod (Selector selector) _ _ _ _ ->
                    ("method", selector)
                  InstanceMethod (Selector selector) _ _ _ _ ->
                    ("method", selector)
              allItems = L.sortBy (on compare snd)
                                  $ concat [typeItems,
                                            constantItems,
                                            enumItems,
                                            stringConstantItems,
                                            functionItems,
                                            functionAliasItems,
                                            classItems,
                                            informalProtocolItems]
          hBanner handle $ frameworkName framework
          mapM (\(nameType, name) -> do
                  hPutStrLn handle
                            $ nameType
                              ++ (replicate (10 - length nameType) ' ')
                              ++ name)
               allItems)
      frameworks


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
  
  banner "Loading BridgeSupport files..."
  putStrLn $ "Processing " ++ frameworkName ++ "."
  frameworkLocation <- findFrameworkOrError frameworkName
  bridgeSupportLocation
    <- findBridgeSupportOrError frameworkName frameworkLocation
  framework
    <- loadBridgeSupport architecture frameworkName bridgeSupportLocation
  (dependencies, _) <- chaseDependencies framework [frameworkName]
  framework <- return $ fixDependencyNames framework
  let frameworks = [framework] ++ dependencies
  
  banner "Augmenting with data from runtime reflection..."
  frameworks <- loadReflectionData frameworks
  
  return frameworks
