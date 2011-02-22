module BridgeSupport (loadBridgeSupport)
  where

import qualified Data.ByteString as BS
import Data.Char
import Data.IORef
import qualified Data.Text.Lazy as TL
import Data.Maybe
import Prelude hiding (error)
import Text.XML.Expat.SAX

import Types
import Util


data ParseState = ParseState {
    parseStateArchitecture :: Architecture,
    parseStateFramework :: Framework,
    parseStateCurrentFunction :: Maybe FunctionDefinition,
    parseStateCurrentClass :: Maybe ClassDefinition,
    parseStateCurrentInformalProtocol :: Maybe InformalProtocolDefinition,
    parseStateCurrentMethodCollection :: Maybe [MethodDefinition],
    parseStateCurrentMethod :: Maybe MethodDefinition,
    parseStateCurrentReturnValue :: Maybe TypeReference,
    parseStateCurrentArguments :: Maybe [(Maybe String, TypeReference)]
  }


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
                            = Nothing,
                          parseStateCurrentInformalProtocol
                            = Nothing,
                          parseStateCurrentMethodCollection
                            = Nothing,
                          parseStateCurrentMethod
                            = Nothing,
                          parseStateCurrentReturnValue
                            = Nothing,
                          parseStateCurrentArguments
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
                        frameworkClasses = [],
                        frameworkInformalProtocols = []
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
  oldParseState <- readIORef ioRef
  let architecture :: Architecture
      architecture = parseStateArchitecture oldParseState
      
      framework :: Framework
      framework = parseStateFramework oldParseState
      
      chooseByBitSize :: (Maybe a)
                      -> (Maybe a)
                      -> (Maybe a)
      chooseByBitSize maybeDefault maybe64
        = case architecture of
            Architecture SixtyFourBit _ ->
              listToMaybe $ catMaybes [maybe64, maybeDefault]
            Architecture ThirtyTwoBit _ ->
              listToMaybe $ catMaybes [maybeDefault]
      
      chooseByBitSizeAndEndianness :: (Maybe a)
                                   -> (Maybe a)
                                   -> (Maybe a)
                                   -> (Maybe a)
                                   -> (Maybe a)
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
      
      typeByArchitecture :: Bool -> Maybe LinkageType
      typeByArchitecture isMethodSignature
        = case chooseByBitSize (lookup "type" attributes)
                               (lookup "type64" attributes) of
            Just theType ->
              parseLinkageType architecture isMethodSignature theType
            Nothing -> Nothing
      
      valueByArchitecture :: Maybe String
      valueByArchitecture
        = chooseByBitSizeAndEndianness (lookup "value" attributes)
                                       (lookup "value64" attributes)
                                       (lookup "le_value" attributes)
                                       (lookup "be_value" attributes)
      
      selectorTypeByArchitecture :: Maybe LinkageType
      selectorTypeByArchitecture
        = case chooseByBitSize (lookup "sel_of_type" attributes)
                               (lookup "sel_of_type64" attributes) of
            Just signature ->
              parseLinkageType architecture True signature
            Nothing -> Nothing
      
      semantics :: Maybe TypeSemantics
      semantics = let cArrayLengthInArgument
                        = fmap read $ lookup "c_array_length_in_arg" attributes
                      cArrayDelimitedByNull
                        = case lookup "c_array_delimited_by_null" attributes of
                            Just "true" -> True
                            _ -> False
                      cArrayFixedLength
                        = fmap read
                               $ lookup "c_array_of_fixed_length" attributes
                      cArrayVariableLength
                        = case lookup "c_array_of_variable_length" attributes of
                            Just "true" -> True
                            _ -> False
                      printfFormat
                        = case lookup "printf_format" attributes of
                            Just "true" -> True
                            _ -> False
                      nullAccepted
                        = case lookup "null_accepted" attributes of
                            Just "false" -> False
                            _ -> True
                  in case () of
                       () | printfFormat
                            -> Just PrintfFormat
                          | isJust cArrayLengthInArgument
                            -> Just $ CArrayLengthInArgument
                                       $ fromJust cArrayLengthInArgument
                          | cArrayDelimitedByNull
                            -> Just CArrayDelimitedByNull
                          | isJust cArrayFixedLength
                            -> Just $ CArrayFixedLength
                                       $ fromJust cArrayFixedLength
                          | cArrayVariableLength
                            -> Just CArrayVariableLength
                          | not nullAccepted
                            -> Just NotNull
                          | otherwise
                            -> Nothing
      
      computeUnusedAttributes :: [String] -> [(String, String)]
      computeUnusedAttributes usedAttributes =
        foldl (\results (key, value) ->
                 if not $ elem key usedAttributes
                   then results ++ [(key, value)]
                   else results)
              []
              attributes
      
      unusedAttributes :: [(String, String)]
      unusedAttributes =
        case elementName of
          "signatures" -> computeUnusedAttributes ["version"]
          "depends_on" -> computeUnusedAttributes ["path"]
          "opaque" -> computeUnusedAttributes ["name",
                                               "type",
                                               "type64",
                                               "magic_cookie"]
          "struct" -> computeUnusedAttributes ["name",
                                               "type",
                                               "type64",
                                               "opaque"]
          "cftype" -> computeUnusedAttributes ["name",
                                               "type",
                                               "type64",
                                               "tollfree",
                                               "gettypeid_func"]
          "constant" -> computeUnusedAttributes ["name",
                                                 "type",
                                                 "type64",
                                                 "magic_cookie",
                                                 "declared_type",
                                                 "const"]
          "enum" -> computeUnusedAttributes ["name",
                                             "value",
                                             "value64",
                                             "le_value",
                                             "be_value"]
          "string_constant" -> computeUnusedAttributes ["name",
                                                        "value",
                                                        "nsstring"]
          "function" -> computeUnusedAttributes ["name",
                                                 "variadic",
                                                 "inline"]
          "function_alias" -> computeUnusedAttributes ["name",
                                                       "original"]
          "class" -> computeUnusedAttributes ["name"]
          "informal_protocol" -> computeUnusedAttributes ["name"]
          "method" -> computeUnusedAttributes ["selector",
                                               "class_method",
                                               "type",
                                               "type64",
                                               "variadic",
                                               "ignore",
                                               "suggestion"]
          "retval" -> computeUnusedAttributes ["type",
                                               "type64",
                                               "declared_type",
                                               "already_retained",
                                               "const",
                                               "c_array_length_in_arg",
                                               "c_array_delimited_by_null",
                                               "c_array_of_fixed_length",
                                               "c_array_of_variable_length",
                                               "printf_format",
                                               "null_accepted"]
          "arg" -> computeUnusedAttributes ["name",
                                            "type",
                                            "type64",
                                            "type_modifier",
                                            "const",
                                            "declared_type",
                                            "index",
                                            "c_array_length_in_arg",
                                            "c_array_delimited_by_null",
                                            "c_array_of_fixed_length",
                                            "c_array_of_variable_length",
                                            "printf_format",
                                            "null_accepted",
                                            "sel_of_type",
                                            "sel_of_type64"]
          "field" -> computeUnusedAttributes ["name"]
          _ -> computeUnusedAttributes []
      
      newParseState :: ParseState
      newParseState
        = case elementName of
            "depends_on" ->
              let maybeDependency = lookup "path" attributes
              in case maybeDependency of
                   Just dependency ->
                     oldParseState {
                         parseStateFramework
                           = framework {
                                 frameworkDependencies
                                   = frameworkDependencies framework
                                     ++ [dependency]
                               }
                       }
                   _ -> oldParseState
            "opaque" ->
              let maybeName = lookup "name" attributes
                  maybeType = typeByArchitecture False
                  isMagic = case lookup "magic_cookie" attributes of
                              Just "true" -> True
                              _ -> False
              in case (maybeName, maybeType) of
                   (Just name, Just theType) ->
                     oldParseState {
                         parseStateFramework
                           = framework {
                                  frameworkTypes
                                    = frameworkTypes framework
                                      ++ [OpaqueType name theType isMagic]
                                }
                       }
                   _ -> oldParseState
            "struct" ->
              let maybeName = lookup "name" attributes
                  maybeType = typeByArchitecture False
                  opaque = case lookup "opaque" attributes of
                             Just "true" -> True
                             _ -> False
              in case (maybeName, maybeType) of
                   (Just name, Just theType) ->
                     oldParseState {
                         parseStateFramework
                           = framework {
                                 frameworkTypes
                                   = frameworkTypes framework
                                     ++ [StructureType name theType opaque]
                               }
                       }
                   _ -> oldParseState
            "cftype" ->
              let maybeName = lookup "name" attributes
                  maybeType = typeByArchitecture False
                  maybeTollfree = lookup "tollfree" attributes
                  maybeTypeIDFunctionName
                    = lookup "gettypeid_func" attributes
              in case (maybeName, maybeType) of
                   (Just name, Just theType) ->
                     oldParseState {
                         parseStateFramework
                           = framework {
                                 frameworkTypes
                                   = frameworkTypes framework
                                     ++ [CoreFoundationType
                                          name
                                          theType
                                          maybeTollfree
                                          maybeTypeIDFunctionName]
                               }
                       }
                   _ -> oldParseState
            "constant" ->
              let maybeName = lookup "name" attributes
                  maybeType = typeByArchitecture False
                  isMagic = case lookup "magic_cookie" attributes of
                              Just "true" -> True
                              _ -> False
                  declaredType =
                    fmap parseDeclaredType
                         $ lookup "declared_type" attributes
              in case (maybeName, maybeType) of
                   (Just name, Just theType) ->
                     oldParseState {
                         parseStateFramework
                           = framework {
                                 frameworkConstants
                                   = frameworkConstants framework
                                     ++ [Constant name
                                                  (Type theType
                                                        declaredType
                                                        Nothing)
                                                  isMagic]
                               }
                       }
                   _ -> oldParseState
            "enum" ->
              let maybeName = lookup "name" attributes
                  maybeValue = valueByArchitecture
              in case (maybeName, maybeValue) of
                   (Just name, Just value) ->
                     let enum = if elem '.' value
                                  then FloatEnum name (read value)
                                  else IntEnum name (read value)
                     in oldParseState {
                            parseStateFramework
                              = framework {
                                    frameworkEnums
                                      = frameworkEnums framework
                                        ++ [enum]
                                  }
                          }
                   _ -> oldParseState
            "string_constant" ->
              let maybeName = lookup "name" attributes
                  maybeValue = lookup "value" attributes
                  isNSString = case lookup "nsstring" attributes of
                                 Just "true" -> True
                                 _ -> False
              in case (maybeName, maybeValue) of
                   (Just name, Just value) ->
                     oldParseState {
                         parseStateFramework
                           = framework {
                                 frameworkStringConstants
                                   = frameworkStringConstants framework
                                     ++ [StringConstant name value isNSString]
                               }
                       }
                   _ -> oldParseState
            "function" ->
              let maybeName = lookup "name" attributes
                  isVariadic = case lookup "variadic" attributes of
                                 Just "true" -> True
                                 _ -> False
                  isInline = case lookup "inline" attributes of
                               Just "inline" -> True
                               _ -> False
              in case maybeName of
                   Just name ->
                     oldParseState {
                         parseStateCurrentFunction
                           = Just $ Function name Void [] isVariadic isInline
                       }
                   _ -> oldParseState
            "function_alias" ->
              let maybeName = lookup "name" attributes
                  maybeOriginal = lookup "original" attributes
              in case (maybeName, maybeOriginal) of
                   (Just name, Just original) ->
                     oldParseState {
                         parseStateFramework
                           = framework {
                                 frameworkFunctionAliases
                                   = frameworkFunctionAliases framework
                                     ++ [FunctionAlias name original]
                               }
                       }
                   _ -> oldParseState
            "class" ->
              let maybeName = lookup "name" attributes
              in case maybeName of
                   Just name ->
                     oldParseState {
                         parseStateCurrentClass
                           = Just $ Class {
                               className = name,
                               classMethods = [],
                               classSuperclass = undefined,
                               classSubclasses = undefined
                             },
                         parseStateCurrentMethodCollection
                           = Just []
                       }
                   _ -> oldParseState
            "informal_protocol" ->
              let maybeName = lookup "name" attributes
              in case maybeName of
                   Just name ->
                     oldParseState {
                         parseStateCurrentInformalProtocol
                           = Just $ InformalProtocol {
                               informalProtocolName = name,
                               informalProtocolMethods = []
                             },
                         parseStateCurrentMethodCollection
                           = Just []
                       }
                   _ -> oldParseState
            "method" ->
              let maybeSelector = fmap parseSelector
                                       $ lookup "selector" attributes
                  classMethod = case lookup "class_method" attributes of
                                  Just "true" -> True
                                  _ -> False
                  maybeType = typeByArchitecture True
                  isVariadic = case lookup "variadic" attributes of
                                 Just "true" -> True
                                 _ -> False
              in case (maybeSelector, maybeType) of
                   (Just selector, Just theType) ->
                     oldParseState {
                         parseStateCurrentMethod
                           = Just
                             $ if classMethod
                                 then ClassMethod selector
                                                  theType
                                                  Void
                                                  []
                                                  isVariadic
                                 else InstanceMethod selector
                                                     theType
                                                     Void
                                                     []
                                                     isVariadic
                       }
                   _ -> oldParseState
            "retval" -> 
              let maybeType = typeByArchitecture False
                  maybeDeclaredType = fmap parseDeclaredType
                                           $ lookup "declared_type" attributes
                  isAlreadyRetained =
                    case lookup "already_retained" attributes of
                      Just "true" -> True
                      _ -> False
                  maybeQualifiedType =
                    if isAlreadyRetained
                      then fmap (qualifyLinkageType RetainedQualifier)
                                maybeType
                      else maybeType
                  isConst = case lookup "const" attributes of
                              Just "true" -> True
                              _ -> False
                  maybePrettyType =
                    if isConst
                      then fmap (qualifyLinkageType ConstQualifier)
                                maybeQualifiedType
                      else maybeQualifiedType
                  maybeSemantics = semantics
              in case maybePrettyType of
                   Just theType ->
                     oldParseState {
                         parseStateCurrentReturnValue
                           = Just $ Type theType
                                         maybeDeclaredType
                                         maybeSemantics
                       }
                   _ -> oldParseState
            "arg" ->
              let maybeName = lookup "name" attributes
                  maybeSelectorType = selectorTypeByArchitecture
                  maybeType = typeByArchitecture False
                  maybeTypeWithSelectorType
                    = case (maybeType, maybeSelectorType) of
                        (Just (SelectorLinkageType Nothing), Just selectorType)
                          -> Just $ SelectorLinkageType $ Just selectorType
                        _ -> maybeType
                  maybeTypeQualifier = case lookup "type_modifier" attributes of
                                         Nothing -> Nothing
                                         Just "n" -> Just InQualifier
                                         Just "o" -> Just OutQualifier
                                         Just "N" -> Just InOutQualifier
                  maybeQualifiedType =
                    case maybeTypeQualifier of
                      Nothing -> maybeTypeWithSelectorType
                      Just typeQualifier ->
                        fmap (qualifyLinkageType typeQualifier)
                             maybeTypeWithSelectorType
                  isConst = case lookup "const" attributes of
                              Just "true" -> True
                              _ -> False
                  maybePrettyType =
                    if isConst
                      then fmap (qualifyLinkageType ConstQualifier)
                                maybeQualifiedType
                      else maybeQualifiedType
                  maybeDeclaredType
                    = fmap parseDeclaredType
                           $ lookup "declared_type" attributes
                  maybeSemantics = semantics
              in case maybePrettyType of
                   Just theType ->
                     let argument = (maybeName,
                                     Type theType
                                          maybeDeclaredType
                                          maybeSemantics)
                     in oldParseState {
                            parseStateCurrentArguments
                              = case parseStateCurrentArguments oldParseState of
                                  Nothing -> Just [argument]
                                  Just arguments ->
                                    Just $ arguments ++ [argument]
                          }
                   _ -> oldParseState
            _ -> oldParseState
  if not $ null unusedAttributes
    then putStrLn $ "Unused attributes in " ++ elementName ++ ": "
                    ++ show unusedAttributes
    else return ()
  writeIORef ioRef newParseState
  return True


gotEndElement :: IORef ParseState -> Name-> IO Bool
gotEndElement ioRef elementName' = do
  let elementName = TL.unpack $ nameLocalName $ elementName'
  oldParseState <- readIORef ioRef
  let framework :: Framework
      framework = parseStateFramework oldParseState
      
      newParseState :: ParseState
      newParseState
        = case elementName of
            "function" ->
              case parseStateCurrentFunction oldParseState of
                Just (Function name _ _ isVariadic isInline) ->
                  let function = Function name
                                          returnValue
                                          arguments
                                          isVariadic
                                          isInline
                      returnValue =
                        case parseStateCurrentReturnValue oldParseState of
                          Nothing -> Void
                          Just returnValue -> returnValue
                      arguments =
                        case parseStateCurrentArguments oldParseState of
                          Nothing -> []
                          Just arguments -> arguments
                  in oldParseState {
                         parseStateFramework
                           = framework {
                                 frameworkFunctions
                                   = frameworkFunctions framework
                                     ++ [function]
                               },
                         parseStateCurrentFunction
                           = Nothing
                       }
                _ -> oldParseState
            "class" ->
              case (parseStateCurrentClass oldParseState,
                    parseStateCurrentMethodCollection oldParseState) of
                (Just currentClass, Just methodCollection) ->
                  let theClass = currentClass {
                                     classMethods = methodCollection
                                   }
                  in oldParseState {
                        parseStateFramework
                          = framework {
                                frameworkClasses
                                  = frameworkClasses framework
                                    ++ [theClass]
                              },
                        parseStateCurrentClass
                          = Nothing,
                        parseStateCurrentMethodCollection
                          = Nothing
                      }
                _ -> oldParseState
            "informal_protocol" ->
              case (parseStateCurrentInformalProtocol oldParseState,
                    parseStateCurrentMethodCollection oldParseState) of
                (Just currentInformalProtocol, Just methodCollection) ->
                  let informalProtocol
                        = currentInformalProtocol {
                              informalProtocolMethods = methodCollection
                            }
                  in oldParseState {
                        parseStateFramework
                          = framework {
                                frameworkInformalProtocols
                                  = frameworkInformalProtocols framework
                                    ++ [informalProtocol]
                              },
                        parseStateCurrentInformalProtocol
                          = Nothing,
                        parseStateCurrentMethodCollection
                          = Nothing
                      }
                _ -> oldParseState
            "method" ->
              case parseStateCurrentMethod oldParseState of
                Just currentMethod ->
                  let method =
                        case currentMethod of
                          ClassMethod selector linkageType _ _ isVariadic ->
                            ClassMethod selector
                                        linkageType
                                        returnValue
                                        arguments
                                        isVariadic
                          InstanceMethod selector linkageType _ _ isVariadic ->
                            InstanceMethod selector
                                           linkageType
                                           returnValue
                                           arguments
                                           isVariadic
                      returnValue =
                        case parseStateCurrentReturnValue oldParseState of
                          Nothing -> Void
                          Just returnValue -> returnValue
                      arguments =
                        case parseStateCurrentArguments oldParseState of
                          Nothing -> []
                          Just arguments -> arguments
                  in case parseStateCurrentMethodCollection oldParseState of
                       Just methods ->
                         oldParseState {
                             parseStateCurrentMethodCollection
                               = Just $ methods ++ [method],
                             parseStateCurrentMethod
                               = Nothing
                           }
                       _ -> oldParseState {
                                parseStateCurrentMethod
                                  = Nothing
                              }
                _ -> oldParseState
            _ -> oldParseState
  writeIORef ioRef newParseState
  return True


parseLinkageType :: Architecture -> Bool -> String -> Maybe LinkageType
parseLinkageType _ isMethodSignature topLevelString =
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
          ':' -> Just (SelectorLinkageType Nothing, tail string)
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
      
  in if not isMethodSignature
       then case takeLinkageType topLevelString of
              Just (theType, "") -> Just theType
              _ -> Nothing
       else let takeItem :: String -> Maybe ((LinkageType, Int), String)
                takeItem string =
                  case takeLinkageType string of
                    Just (linkageType, string') ->
                      let (intString, string'') = span isDigit string'
                      in case intString of
                           "" -> Nothing
                           _ -> let int = read intString
                                in Just ((linkageType, int), string'')
                    _ -> Nothing
                
                takeAllItems :: [(LinkageType, Int)]
                             -> String
                             -> Maybe [(LinkageType, Int)]
                takeAllItems items string =
                  case takeItem string of
                    Just (item, "") -> Just $ items ++ [item]
                    Just (item, string') ->
                      takeAllItems (items ++ [item]) string'
                    _ -> Nothing
            in case takeAllItems [] topLevelString of
                 Just allItems -> Just $ MethodLinkageType (fst $ head allItems)
                                                           (snd $ head allItems)
                                                           (tail allItems)
                 Nothing -> Nothing


qualifyLinkageType :: LinkageQualifier -> LinkageType -> LinkageType
qualifyLinkageType qualifier linkageType =
  case linkageType of
    QualifiedLinkageType otherQualifiers underlyingType ->
      QualifiedLinkageType (otherQualifiers ++ [qualifier]) underlyingType
    _ -> QualifiedLinkageType [qualifier] linkageType


parseDeclaredType :: String -> DeclaredType
parseDeclaredType string =
  DeclaredType string


parseSelector :: String -> Selector
parseSelector string =
  Selector string
