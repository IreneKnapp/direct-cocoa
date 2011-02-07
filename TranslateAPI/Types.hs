{-# LANGUAGE DeriveDataTypeable #-}
module Types (Architecture(..),
              BitSize(..),
              Endianness(..),
              Framework(..),
              LinkageType(..),
              LinkageQualifier(..),
              DeclaredType(..),
              TypeDefinition(..),
              TypeSemantics(..),
              TypeReference(..),
              ConstantDefinition(..),
              EnumDefinition(..),
              FunctionDefinition(..),
              FunctionAliasDefinition(..),
              StringConstantDefinition(..),
              ClassDefinition(..),
              InformalProtocolDefinition(..),
              MethodDefinition(..),
              Selector(..))
  where

import Data.Generics
import Data.Int


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
    frameworkClasses :: [ClassDefinition],
    frameworkInformalProtocols :: [InformalProtocolDefinition]
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
                 | SelectorLinkageType (Maybe LinkageType)
                 | ArrayLinkageType Int LinkageType
                 | StructureLinkageType String
                                        (Maybe [(Maybe String, LinkageType)])
                 | UnionLinkageType String
                                    [(Maybe String, LinkageType)]
                 | BitfieldLinkageType Int
                 | PointerLinkageType LinkageType
                 | UnknownLinkageType
                 | QualifiedLinkageType [LinkageQualifier] LinkageType
                 | MethodLinkageType LinkageType Int [(LinkageType, Int)]
                 deriving (Show, Data, Typeable)


data LinkageQualifier = ConstQualifier
                      | InQualifier
                      | InOutQualifier
                      | OutQualifier
                      | ByCopyQualifier
                      | ByReferenceQualifier
                      | OneWayQualifier
                      | RetainedQualifier
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


data TypeSemantics = PrintfFormat
                   | CArrayLengthInArgument Int
                   | CArrayDelimitedByNull
                   | CArrayFixedLength Int
                   | CArrayVariableLength
                   | NotNull
                   deriving (Show, Data, Typeable)


data TypeReference = Type LinkageType (Maybe DeclaredType) (Maybe TypeSemantics)
                   | Void
                   deriving (Show, Data, Typeable)


data ConstantDefinition = Constant String TypeReference Bool
                        deriving (Show, Data, Typeable)


data EnumDefinition = IntEnum String Int64
                    | FloatEnum String Double
                    deriving (Show, Data, Typeable)


data FunctionDefinition = Function String
                                   TypeReference
                                   [(Maybe String, TypeReference)]
                                   Bool
                                   Bool
                        deriving (Show, Data, Typeable)


data FunctionAliasDefinition = FunctionAlias String String
                             deriving (Show, Data, Typeable)


data StringConstantDefinition = StringConstant String String Bool
                              deriving (Show, Data, Typeable)


data ClassDefinition = Class {
    className :: String,
    classMethods :: [MethodDefinition]
  }
  deriving (Show, Data, Typeable)


data InformalProtocolDefinition = InformalProtocol {
    informalProtocolName :: String,
    informalProtocolMethods :: [MethodDefinition]
  }
  deriving (Show, Data, Typeable)


data MethodDefinition
  = ClassMethod Selector
                LinkageType
                TypeReference
                [(Maybe String, TypeReference)]
                Bool
  | InstanceMethod Selector
                   LinkageType
                   TypeReference
                   [(Maybe String, TypeReference)]
                   Bool
  deriving (Show, Data, Typeable)

data Selector = Selector String
              deriving (Show, Data, Typeable)


              