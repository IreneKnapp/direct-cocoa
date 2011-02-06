{-# LANGUAGE DeriveDataTypeable #-}
module Main (main) where

import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char
import Data.Generics
import Data.Int
import Data.IORef
import qualified Data.List as L
import Data.Maybe
import qualified Data.Text.Lazy as TL
import Prelude hiding (error, Enum)
import qualified Prelude as Prelude
import System.Directory
import System.Exit
import System.Environment
import System.IO
import Text.XML.Expat.SAX


data Architecture = Architecture BitSize Endianness


data BitSize = ThirtyTwoBit
             | SixtyFourBit


data Endianness = LittleEndian
                | BigEndian


data Framework = Framework {
    frameworkName :: String,
    frameworkDependencies :: [String],
    frameworkTypes :: [TypeDefinition],
    frameworkConstants :: [ConstantDefinition],
    frameworkEnums :: [EnumDefinition],
    frameworkFunctions :: [FunctionDefinition],
    frameworkFunctionAliases :: [FunctionAliasDefinition],
    frameworkStringConstants :: [StringConstantDefinition],
    frameworkClasses :: [ClassDefinition]
  }
  deriving (Show, Data, Typeable)


data LinkageType = CharLinkageType
                 | Int8LinkageType
                 | Int16LinkageType
                 | Int32LinkageType
                 | Int64LinkageType
                 | Word8LinkageType
                 | Word16LinkageType
                 | Word32LinkageType
                 | Word64LinkageType
                 | FloatLinkageType
                 | DoubleLinkageType
                 | BoolLinkageType
                 | VoidLinkageType
                 | CStringLinkageType
                 | ObjectLinkageType
                 | ClassLinkageType
                 | SelectorLinkageType
                 | ArrayLinkageType Int LinkageType
                 | StructureLinkageType String
                                        (Maybe [(Maybe String, LinkageType)])
                 | UnionLinkageType String
                                    [(Maybe String, LinkageType)]
                 | BitfieldLinkageType Int
                 | PointerLinkageType LinkageType
                 | UnknownLinkageType
                 | QualifiedLinkageType [LinkageQualifier] LinkageType
                 deriving (Show, Data, Typeable)


data LinkageQualifier = ConstQualifier
                      | InQualifier
                      | InOutQualifier
                      | OutQualifier
                      | ByCopyQualifier
                      | ByReferenceQualifier
                      | OneWayQualifier
                      deriving (Show, Data, Typeable)

data DeclaredType = DeclaredType String
                  deriving (Show, Data, Typeable)


data TypeDefinition = OpaqueType String LinkageType Bool
                    | StructureType String LinkageType Bool
                    | CoreFoundationType String
                                         LinkageType
                                         (Maybe String)
                                         (Maybe String)
                    deriving (Show, Data, Typeable)


data TypeReference = Type LinkageType (Maybe DeclaredType)
                   | Void
                   deriving (Show, Data, Typeable)


data ConstantDefinition = Constant String TypeReference Bool
                        deriving (Show, Data, Typeable)


data EnumDefinition = IntEnum String Int64
                    | FloatEnum String Double
                    deriving (Show, Data, Typeable)


data FunctionDefinition = Function String
                                   TypeReference
                                   [(String, TypeReference)]
                        deriving (Show, Data, Typeable)


data FunctionAliasDefinition = FunctionAlias String String
                             deriving (Show, Data, Typeable)


data StringConstantDefinition = StringConstant String String Bool
                              deriving (Show, Data, Typeable)


data ClassDefinition = Class {
    className :: String
  }
  deriving (Show, Data, Typeable)


data ParseState = ParseState {
    parseStateArchitecture :: Architecture,
    parseStateFramework :: Framework,
    parseStateCurrentFunction :: Maybe FunctionDefinition,
    parseStateCurrentClass :: Maybe ClassDefinition
  }


main :: IO ()
main = do
  let architecture = Architecture SixtyFourBit LittleEndian
  frameworks <- processFramework architecture "Cocoa"
  everywhereM (mkM $ \it -> do
                 case it of
                   Class { } -> putStrLn $ show it
                 return it)
              frameworks
  putStrLn $ "All done!"


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


loadBridgeSupport :: Architecture -> String -> FilePath -> IO Framework
loadBridgeSupport architecture frameworkName location = do
  ioRef <- newIORef $ ParseState {
                          parseStateArchitecture
                            = architecture,
                          parseStateFramework
                            = emptyFramework frameworkName,
                          parseStateCurrentFunction
                            = Nothing,
                          parseStateCurrentClass
                            = Nothing
                        }
  parser <- newParser error
  setCallback parser parsedBeginElement $ gotBeginElement ioRef
  setCallback parser parsedEndElement $ gotEndElement ioRef
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
                        frameworkEnums = [],
                        frameworkFunctions = [],
                        frameworkFunctionAliases = [],
                        frameworkStringConstants = [],
                        frameworkClasses = []
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
  let architecture = parseStateArchitecture parseState
      framework = parseStateFramework parseState
      currentFunction = parseStateCurrentFunction parseState
      currentClass = parseStateCurrentClass parseState
      chooseByBitSize maybeDefault maybe64
        = case architecture of
            Architecture SixtyFourBit _ ->
              listToMaybe $ catMaybes [maybe64, maybeDefault]
            Architecture ThirtyTwoBit _ ->
              listToMaybe $ catMaybes [maybeDefault]
      chooseByBitSizeAndEndianness maybeDefault maybe64 maybeLE maybeBE
        = case architecture of
            Architecture SixtyFourBit BigEndian ->
              listToMaybe $ catMaybes [maybe64, maybeBE, maybeDefault]
            Architecture SixtyFourBit LittleEndian ->
              listToMaybe $ catMaybes [maybe64, maybeLE, maybeDefault]
            Architecture ThirtyTwoBit BigEndian ->
              listToMaybe $ catMaybes [maybeBE, maybeDefault]
            Architecture ThirtyTwoBit LittleEndian ->
              listToMaybe $ catMaybes [maybeLE, maybeDefault]
      typeByArchitecture
        = case chooseByBitSize (lookup "type" attributes)
                               (lookup "type64" attributes) of
            Just theType -> parseLinkageType architecture theType
            Nothing -> Nothing
      valueByArchitecture
        = chooseByBitSizeAndEndianness (lookup "value" attributes)
                                       (lookup "value64" attributes)
                                       (lookup "le_value" attributes)
                                       (lookup "be_value" attributes)
      (framework', currentFunction', currentClass')
        = case elementName of
            "depends_on" -> let maybeDependency = lookup "path" attributes
                            in case maybeDependency of
                                 Just dependency ->
                                   (framework {
                                        frameworkDependencies
                                          = frameworkDependencies framework
                                            ++ [dependency]
                                      },
                                    currentFunction,
                                    currentClass)
            "opaque" -> let maybeName = lookup "name" attributes
                            maybeType = typeByArchitecture
                            isMagic = case lookup "magic_cookie" attributes of
                                        Just "true" -> True
                                        _ -> False
                        in case (maybeName, maybeType) of
                             (Just name, Just theType) ->
                               (framework {
                                    frameworkTypes
                                      = frameworkTypes framework
                                        ++ [OpaqueType name theType isMagic]
                                  },
                                currentFunction,
                                currentClass)
                             _ -> (framework, currentFunction, currentClass)
            "struct" -> let maybeName = lookup "name" attributes
                            maybeType = typeByArchitecture
                            opaque = case lookup "opaque" attributes of
                                       Just "true" -> True
                                       _ -> False
                        in case (maybeName, maybeType) of
                             (Just name, Just theType) ->
                               (framework {
                                    frameworkTypes
                                      = frameworkTypes framework
                                        ++ [StructureType name theType opaque]
                                  },
                                currentFunction,
                                currentClass)
                             _ -> (framework, currentFunction, currentClass)
            "cftype" -> let maybeName = lookup "name" attributes
                            maybeType = typeByArchitecture
                            maybeTollfree = lookup "tollfree" attributes
                            maybeTypeIDFunctionName
                              = lookup "gettypeid_func" attributes
                        in case (maybeName, maybeType) of
                             (Just name, Just theType) ->
                               (framework {
                                    frameworkTypes
                                      = frameworkTypes framework
                                        ++ [CoreFoundationType
                                             name
                                             theType
                                             maybeTollfree
                                             maybeTypeIDFunctionName]
                                  },
                                currentFunction,
                                currentClass)
                             _ -> (framework, currentFunction, currentClass)
            "constant" -> let maybeName = lookup "name" attributes
                              maybeType = typeByArchitecture
                              isMagic = case lookup "magic_cookie" attributes of
                                          Just "true" -> True
                                          _ -> False
                              declaredType =
                                fmap parseDeclaredType
                                     $ lookup "declared_type" attributes
                          in case (maybeName, maybeType) of
                               (Just name, Just theType) ->
                                 (framework {
                                      frameworkConstants
                                        = frameworkConstants framework
                                          ++ [Constant name
                                                       (Type theType
                                                             declaredType)
                                                       isMagic]
                                    },
                                  currentFunction,
                                  currentClass)
                               _ -> (framework, currentFunction, currentClass)
            "enum" -> let maybeName = lookup "name" attributes
                          maybeValue = valueByArchitecture
                      in case (maybeName, maybeValue) of
                           (Just name, Just value) ->
                             let enum = if elem '.' value
                                          then FloatEnum name (read value)
                                          else IntEnum name (read value)
                             in (framework {
                                     frameworkEnums
                                       = frameworkEnums framework
                                         ++ [enum]
                                   },
                                 currentFunction, currentClass)
                           _ -> (framework, currentFunction, currentClass)
            "string_constant" ->
              let maybeName = lookup "name" attributes
                  maybeValue = lookup "value" attributes
                  isNSString = case lookup "nsstring" attributes of
                                 Just "true" -> True
                                 _ -> False
              in case (maybeName, maybeValue) of
                   (Just name, Just value) ->
                     (framework {
                          frameworkStringConstants
                            = frameworkStringConstants framework
                              ++ [StringConstant name value isNSString]
                        },
                      currentFunction, currentClass)
                   _ -> (framework, currentFunction, currentClass)
            "function" -> let maybeName = lookup "name" attributes
                          in case maybeName of
                               Just name ->
                                 (framework,
                                  Just $ Function name Void [],
                                  currentClass)
                               _ -> (framework, currentFunction, currentClass)
            "function_alias" ->
              let maybeName = lookup "name" attributes
                  maybeOriginal = lookup "original" attributes
              in case (maybeName, maybeOriginal) of
                   (Just name, Just original) ->
                     (framework {
                          frameworkFunctionAliases
                            = frameworkFunctionAliases framework
                              ++ [FunctionAlias name original]
                        },
                      currentFunction,
                      currentClass)
                   _ -> (framework, currentFunction, currentClass)
            "class" ->
              let maybeName = lookup "name" attributes
              in case maybeName of
                   Just name ->
                     (framework,
                      currentFunction,
                      Just $ Class {
                                 className = name
                               })
                   _ -> (framework, currentFunction, currentClass)
            "retval" -> 
              case currentFunction of
                Nothing -> (framework, currentFunction, currentClass)
                Just _ ->
                  let maybeType = typeByArchitecture
                      declaredType
                        = fmap parseDeclaredType
                               $ lookup "declared_type" attributes
                      Just (Function functionName _ arguments)
                        = currentFunction
                  in case maybeType of
                       Just theType ->
                         (framework,
                          Just $ Function functionName
                                          (Type theType declaredType)
                                          arguments,
                          currentClass)
                       _ -> (framework, currentFunction, currentClass)
            "arg" ->
              case currentFunction of
                Nothing -> (framework, currentFunction, currentClass)
                Just _ ->
                  let theName = fromMaybe "argument" $ lookup "name" attributes
                      maybeType = typeByArchitecture
                      declaredType
                        = fmap parseDeclaredType
                               $ lookup "declared_type" attributes
                  in case maybeType of
                       Just theType ->
                         let argument = (theName, Type theType declaredType)
                             Just (Function functionName returnType arguments)
                               = currentFunction
                         in (framework,
                             Just $ Function functionName
                                             returnType
                                             (arguments ++ [argument]),
                             currentClass)
                       _ -> (framework, currentFunction, currentClass)
            _ -> (framework, currentFunction, currentClass)
  writeIORef ioRef $ parseState {
                          parseStateFramework = framework',
                          parseStateCurrentFunction = currentFunction',
                          parseStateCurrentClass = currentClass'
                       }
  return True


gotEndElement :: IORef ParseState -> Name-> IO Bool
gotEndElement ioRef elementName' = do
  let elementName = TL.unpack $ nameLocalName $ elementName'
  parseState <- readIORef ioRef
  let framework = parseStateFramework parseState
      maybeCurrentFunction = parseStateCurrentFunction parseState
      maybeCurrentClass = parseStateCurrentClass parseState
      (framework', maybeCurrentFunction', maybeCurrentClass')
        = case elementName of
            "function" ->
              case maybeCurrentFunction of
                Just currentFunction -> (framework {
                                             frameworkFunctions
                                               = frameworkFunctions framework
                                                 ++ [currentFunction]
                                           },
                                         Nothing,
                                         maybeCurrentClass)
            "class" ->
              case maybeCurrentClass of
                Just currentClass -> (framework {
                                          frameworkClasses
                                            = frameworkClasses framework
                                              ++ [currentClass]
                                        },
                                      maybeCurrentFunction,
                                      Nothing)
            _ -> (framework, maybeCurrentFunction, maybeCurrentClass)
  writeIORef ioRef $ parseState {
                          parseStateFramework = framework',
                          parseStateCurrentFunction = maybeCurrentFunction',
                          parseStateCurrentClass = maybeCurrentClass'
                       }
  return True


parseLinkageType :: Architecture -> String -> Maybe LinkageType
parseLinkageType _ topLevelString =
  let takeLinkageType :: String -> Maybe (LinkageType, String)
      takeLinkageType string =
        case head string of
          'c' -> Just (Int8LinkageType, tail string)
          'i' -> Just (Int32LinkageType, tail string)
          's' -> Just (Int16LinkageType, tail string)
          'l' -> Just (Int32LinkageType, tail string)
          'q' -> Just (Int64LinkageType, tail string)
          'C' -> Just (Word8LinkageType, tail string)
          'I' -> Just (Word32LinkageType, tail string)
          'S' -> Just (Word16LinkageType, tail string)
          'L' -> Just (Word32LinkageType, tail string)
          'Q' -> Just (Word64LinkageType, tail string)
          'f' -> Just (FloatLinkageType, tail string)
          'd' -> Just (DoubleLinkageType, tail string)
          'B' -> Just (BoolLinkageType, tail string)
          'v' -> Just (VoidLinkageType, tail string)
          '*' -> Just (CStringLinkageType, tail string)
          '@' -> Just (ObjectLinkageType, tail string)
          '#' -> Just (ClassLinkageType, tail string)
          ':' -> Just (SelectorLinkageType, tail string)
          '[' ->
            let (countString, rest) = span isDigit $ tail string
                count = read countString
            in case takeLinkageType $ rest of
                 Nothing -> Nothing
                 Just (contentType, rest) ->                 
                   case head rest of
                     ']' -> Just (ArrayLinkageType count contentType, tail rest)
                     _ -> Nothing
          '{' ->
            case parseSubtypes '}' $ tail string of
              Just (name, maybeSubtypes, rest) ->
                Just (StructureLinkageType name maybeSubtypes, rest)
              _ -> Nothing
          '(' ->
            case parseSubtypes ')' $ tail string of
              Just (name, Just subtypes, rest) ->
                Just (UnionLinkageType name subtypes, rest)
              _ -> Nothing
          'b' ->
            let (widthString, rest) = span isDigit $ tail string
                width = read widthString
            in Just (BitfieldLinkageType width, rest)
          '^' ->
            case takeLinkageType $ tail string of
                 Nothing -> Nothing
                 Just (contentType, rest) ->
                   Just (PointerLinkageType contentType, rest)
          '?' -> Just (UnknownLinkageType, tail string)
          'r' -> case takeLinkageType $ tail string of
                   Nothing -> Nothing
                   Just (linkageType, rest) ->
                     Just (qualifyLinkageType ConstQualifier linkageType,
                           rest)
          'n' -> case takeLinkageType $ tail string of
                   Nothing -> Nothing
                   Just (linkageType, rest) ->
                     Just (qualifyLinkageType InQualifier linkageType,
                           rest)
          'N' -> case takeLinkageType $ tail string of
                   Nothing -> Nothing
                   Just (linkageType, rest) ->
                     Just (qualifyLinkageType InOutQualifier linkageType,
                           rest)
          'o' -> case takeLinkageType $ tail string of
                   Nothing -> Nothing
                   Just (linkageType, rest) ->
                     Just (qualifyLinkageType OutQualifier linkageType,
                           rest)
          'O' -> case takeLinkageType $ tail string of
                   Nothing -> Nothing
                   Just (linkageType, rest) ->
                     Just (qualifyLinkageType ByCopyQualifier linkageType,
                           rest)
          'R' -> case takeLinkageType $ tail string of
                   Nothing -> Nothing
                   Just (linkageType, rest) ->
                     Just (qualifyLinkageType ByReferenceQualifier linkageType,
                           rest)
          'V' -> case takeLinkageType $ tail string of
                   Nothing -> Nothing
                   Just (linkageType, rest) ->
                     Just (qualifyLinkageType OneWayQualifier linkageType,
                           rest)
      
      qualifyLinkageType :: LinkageQualifier -> LinkageType -> LinkageType
      qualifyLinkageType qualifier linkageType =
        case linkageType of
          QualifiedLinkageType otherQualifiers underlyingType ->
            QualifiedLinkageType (otherQualifiers ++ [qualifier]) underlyingType
          _ -> QualifiedLinkageType [qualifier] linkageType
      
      parseSubtypes :: Char
                    -> String
                    -> Maybe (String,
                              Maybe [(Maybe String, LinkageType)],
                              String)
      parseSubtypes delimiter string =
        let loop :: [(Maybe String, LinkageType)]
                 -> String
                 -> Maybe ([(Maybe String, LinkageType)], String)
            loop results string =
              if head string == delimiter
                then Just (results, tail string)
                else
                  let (maybeFieldName, rest) =
                        if head string == '"'
                          then let (fieldName, rest) = break (\c -> c == '"')
                                                             $ drop 1 string
                               in (Just fieldName, tail rest)
                          else (Nothing, string)
                      in case takeLinkageType rest of
                           Nothing -> Nothing
                           Just (result, restRest) ->
                             loop (results ++ [(maybeFieldName, result)])
                                  restRest
            maybeSubtypes =
              case subtypeEncodings of
                ('=':subtypeEncodings') -> loop [] subtypeEncodings'
                rest -> Nothing
            (name, subtypeEncodings) = break (\c -> c == '=' || c== delimiter)
                                             string
        in case maybeSubtypes of
             Nothing -> Just (name, Nothing, tail subtypeEncodings)
             Just (subtypes, rest) -> Just (name, Just subtypes, rest)
      
  in case takeLinkageType topLevelString of
       Just (theType, "") -> Just theType
       _ -> Nothing


parseDeclaredType :: String -> DeclaredType
parseDeclaredType string =
  DeclaredType string


error :: String -> IO a
error message = do
  putStrLn $ "Error: " ++ message
  exitFailure
  undefined
