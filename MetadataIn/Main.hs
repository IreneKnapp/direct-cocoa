module Main (main) where

import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Int
import Data.IORef
import qualified Data.List as L
import Data.Maybe
import qualified Data.Text.Lazy as TL
import Prelude hiding (error, Enum)
import System.Directory
import System.Exit
import System.Environment
import System.IO
import Text.XML.Expat.SAX


data Framework = Framework {
    frameworkName :: String,
    frameworkDependencies :: [String],
    frameworkTypes :: [Type],
    frameworkConstants :: [Constant],
    frameworkEnums :: [Enum]
  }
  deriving (Show)


data Type = OpaqueType String String Bool
          deriving (Show)


data Constant = Constant String String String Bool
              deriving (Show)


data Enum = IntEnum String Int64
          | FloatEnum String Double
          deriving (Show)


data ParseState = ParseState {
    parseStateFramework :: Framework
  }


main :: IO ()
main = do
  frameworks <- processFramework "Cocoa"
  mapM_ (\framework -> do
           putStrLn $ "In " ++ frameworkName framework ++ ":"
           mapM_ (\enum -> do
                    case enum of
                      IntEnum theName theValue -> do
                        putStrLn $ "  IntEnum " ++ theName ++ " " ++ show theValue
                      FloatEnum theName theValue -> do
                        putStrLn $ "  FloatEnum " ++ theName ++ " " ++ show theValue)
                 $ frameworkEnums framework)
       frameworks


processFramework :: String -> IO [Framework]
processFramework frameworkName = do
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
                  <- loadBridgeSupport frameworkName bridgeSupportLocation
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
    <- loadBridgeSupport frameworkName bridgeSupportLocation
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


loadBridgeSupport :: String -> FilePath -> IO Framework
loadBridgeSupport frameworkName location = do
  ioRef <- newIORef $ ParseState {
                               parseStateFramework
                                 = emptyFramework frameworkName
                             }
  parser <- newParser error
  setCallback parser parsedBeginElement $ gotBeginElement ioRef
  bytes <- BS.readFile location
  parseBytes parser bytes
  parseComplete parser
  finalParseState <- readIORef ioRef
  return $ parseStateFramework finalParseState


emptyFramework :: String -> Framework
emptyFramework name = Framework {
                        frameworkName = name,
                        frameworkDependencies = [],
                        frameworkTypes = [],
                        frameworkConstants = [],
                        frameworkEnums = []
                      }


gotBeginElement :: IORef ParseState -> Name -> [Attribute] -> IO Bool
gotBeginElement ioRef elementName' attributes' = do
  let elementName = TL.unpack $ nameLocalName $ elementName'
      attributes
        = map (\attribute ->
                 (TL.unpack $ nameLocalName $ attributeName attribute,
                  concat $ map (\content ->
                                  case content of
                                    ContentText text -> TL.unpack text
                                    ContentEntity text ->
                                      "&" ++ TL.unpack text ++ ";")
                               $ attributeContent attribute))
              attributes'
  parseState <- readIORef ioRef
  let framework = parseStateFramework parseState
      framework'
        = case elementName of
            "depends_on" -> let dependency = fromJust $ lookup "path" attributes
                            in framework {
                                   frameworkDependencies
                                     = frameworkDependencies framework
                                       ++ [dependency]
                                 }
            "opaque" -> let theName = fromJust $ lookup "name" attributes
                            maybeType32 = lookup "type" attributes
                            maybeType64 = lookup "type64" attributes
                            theType = head $ catMaybes [maybeType64,
                                                        maybeType32]
                            isMagic = case lookup "magic_cookie" attributes of
                                        Just "true" -> True
                                        _ -> False
                        in framework {
                               frameworkTypes
                                 = frameworkTypes framework
                                   ++ [OpaqueType theName theType isMagic]
                             }
            "constant" -> let theName = fromJust $ lookup "name" attributes
                              maybeType32 = lookup "type" attributes
                              maybeType64 = lookup "type64" attributes
                              theType = head $ catMaybes [maybeType64,
                                                          maybeType32]
                              isMagic = case lookup "magic_cookie" attributes of
                                          Just "true" -> True
                                          _ -> False
                              declaredType
                                = fromJust $ lookup "declared_type" attributes
                          in framework {
                                 frameworkConstants
                                   = frameworkConstants framework
                                     ++ [Constant theName
                                                  theType
                                                  declaredType
                                                  isMagic]
                               }
            "enum" -> let theName = fromJust $ lookup "name" attributes
                          maybeValue32 = lookup "value" attributes
                          maybeValue64 = lookup "value64" attributes
                          maybeLEValue = lookup "le_value" attributes
                          maybeBEValue = lookup "be_value" attributes
                          theValue = head $ catMaybes [maybeValue64,
                                                       maybeValue32,
                                                       maybeLEValue,
                                                       maybeBEValue]
                          enum = if elem '.' theValue
                                   then FloatEnum theName (read theValue)
                                   else IntEnum theName (read theValue)
                      in framework {
                             frameworkEnums
                               = frameworkEnums framework
                                 ++ [enum]
                           }
            _ -> framework
  writeIORef ioRef $ parseState {
                          parseStateFramework = framework'
                       }
  return True


error :: String -> IO a
error message = do
  putStrLn $ "Error: " ++ message
  exitFailure
  undefined
