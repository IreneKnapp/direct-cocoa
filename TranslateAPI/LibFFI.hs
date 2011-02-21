{-# LANGUAGE ForeignFunctionInterface, CPP #-}
module LibFFI (
               ABI,
               defaultABI,
               Type,
               CIF,
               cif,
               --call,
               typeUChar,
               typeSChar,
               typeUShort,
               typeSShort,
               typeUInt,
               typeSInt,
               typeULong,
               typeSLong,
               typeVoid,
               typeUInt8,
               typeSInt8,
               typeUInt16,
               typeSInt16,
               typeUInt32,
               typeSInt32,
               typeUInt64,
               typeSInt64,
               typeStruct
              )
  where

import Control.Monad
import Foreign
import Foreign.C
import System.IO.Unsafe

foreign import ccall "ffi_type_void" ffi_type_void :: Ptr ()
foreign import ccall "ffi_type_uint8" ffi_type_uint8 :: Ptr ()
foreign import ccall "ffi_type_sint8" ffi_type_sint8 :: Ptr ()
foreign import ccall "ffi_type_uint16" ffi_type_uint16 :: Ptr ()
foreign import ccall "ffi_type_sint16" ffi_type_sint16 :: Ptr ()
foreign import ccall "ffi_type_uint32" ffi_type_uint32 :: Ptr ()
foreign import ccall "ffi_type_sint32" ffi_type_sint32 :: Ptr ()
foreign import ccall "ffi_type_uint64" ffi_type_uint64 :: Ptr ()
foreign import ccall "ffi_type_sint64" ffi_type_sint64 :: Ptr ()
foreign import ccall "ffi_type_float" ffi_type_float :: Ptr ()
foreign import ccall "ffi_type_double" ffi_type_double :: Ptr ()
foreign import ccall "ffi_type_longdouble" ffi_type_longdouble :: Ptr ()
foreign import ccall "ffi_type_pointer" ffi_type_pointer :: Ptr ()
foreign import ccall "ffi_prep_cif" ffi_prep_cif
  :: Ptr () -> CInt -> CUInt -> Ptr () -> Ptr (Ptr ()) -> IO CInt
foreign import ccall "ffi_call" ffi_call
  :: Ptr () -> Ptr () -> Ptr () -> Ptr (Ptr ()) -> IO ()


#ifdef x86_64_HOST_ARCH

typeUChar :: Type
typeUChar = typeUInt8

typeSChar :: Type
typeSChar = typeSInt8

typeUShort :: Type
typeUShort = typeUInt16

typeSShort :: Type
typeSShort = typeSInt16

typeUInt :: Type
typeUInt = typeUInt32

typeSInt :: Type
typeSInt = typeSInt32

typeULong :: Type
typeULong = typeUInt64

typeSLong :: Type
typeSLong = typeSInt64

data ABI = SysV
         | Unix64
         deriving (Eq, Show)
instance Enum ABI where
  fromEnum SysV = 0
  fromEnum Unix64 = 1
  toEnum 0 = SysV
  toEnum 1 = Unix64

defaultABI :: ABI
defaultABI = Unix64

#elif i386_HOST_ARCH

typeUChar :: Type
typeUChar = typeUInt8

typeSChar :: Type
typeSChar = typeSInt8

typeUShort :: Type
typeUShort = typeUInt16

typeSShort :: Type
typeSShort = typeSInt16

typeUInt :: Type
typeUInt = typeUInt32

typeSInt :: Type
typeSInt = typeSInt32

typeULong :: Type
typeULong = typeUInt32

typeSLong :: Type
typeSLong = typeSInt32

data ABI = SysV
         | Unix64
         deriving (Eq, Show)
instance Enum ABI where
  fromEnum SysV = 0
  fromEnum Unix64 = 1
  toEnum 0 = SysV
  toEnum 1 = Unix64

defaultABI :: ABI
defaultABI = SysV

#elif ppc_HOST_ARCH

typeUChar :: Type
typeUChar = typeUInt8

typeSChar :: Type
typeSChar = typeSInt8

typeUShort :: Type
typeUShort = typeUInt16

typeSShort :: Type
typeSShort = typeSInt16

typeUInt :: Type
typeUInt = typeUInt32

typeSInt :: Type
typeSInt = typeSInt32

typeULong :: Type
typeULong = typeUInt32

typeSLong :: Type
typeSLong = typeSInt32

data ABI = SysV
         | GCCSysV
         | Linux64
         deriving (Eq, Show)
instance Enum ABI where
  fromEnum SysV = 0
  fromEnum GCCSysV = 1
  fromEnum Linux64 = 2
  toEnum 0 = SysV
  toEnum 1 = GCCSysV
  toEnum 2 = Linux64

defaultABI :: ABI
defaultABI = GCCSysV

#elif ppc64_HOST_ARCH

#error I wasn't able to find the appropriate definitions for this architecture.

#elif arm_HOST_ARCH

typeUChar :: Type
typeUChar = typeUInt8

typeSChar :: Type
typeSChar = typeSInt8

typeUShort :: Type
typeUShort = typeUInt16

typeSShort :: Type
typeSShort = typeSInt16

typeUInt :: Type
typeUInt = typeUInt32

typeSInt :: Type
typeSInt = typeSInt32

typeULong :: Type
typeULong = typeUInt32

typeSLong :: Type
typeSLong = typeSInt32

#endif


data Status = OK
            | BadTypedef
            | BadABI
            deriving (Eq)
instance Enum Status where
  fromEnum OK = 0
  fromEnum BadTypedef = 1
  fromEnum BadABI = 2
  toEnum 0 = OK
  toEnum 1 = BadTypedef
  toEnum 2 = BadABI         


data Type = Type (ForeignPtr ()) deriving (Eq)
data CIF = CIF (ForeignPtr ())


typeVoid :: Type
typeVoid = Type $ unsafePerformIO $ newForeignPtr_ ffi_type_void


typeUInt8 :: Type
typeUInt8 = Type $ unsafePerformIO $ newForeignPtr_ ffi_type_uint8


typeSInt8 :: Type
typeSInt8 = Type $ unsafePerformIO $ newForeignPtr_ ffi_type_sint8


typeUInt16 :: Type
typeUInt16 = Type $ unsafePerformIO $ newForeignPtr_ ffi_type_uint16


typeSInt16 :: Type
typeSInt16 = Type $ unsafePerformIO $ newForeignPtr_ ffi_type_sint16


typeUInt32 :: Type
typeUInt32 = Type $ unsafePerformIO $ newForeignPtr_ ffi_type_uint32


typeSInt32 :: Type
typeSInt32 = Type $ unsafePerformIO $ newForeignPtr_ ffi_type_sint32


typeUInt64 :: Type
typeUInt64 = Type $ unsafePerformIO $ newForeignPtr_ ffi_type_uint64


typeSInt64 :: Type
typeSInt64 = Type $ unsafePerformIO $ newForeignPtr_ ffi_type_sint64


typeFloat :: Type
typeFloat = Type $ unsafePerformIO $ newForeignPtr_ ffi_type_float


typeDouble :: Type
typeDouble = Type $ unsafePerformIO $ newForeignPtr_ ffi_type_double


typeLongDouble :: Type
typeLongDouble = Type $ unsafePerformIO $ newForeignPtr_ ffi_type_longdouble


typePointer :: Type
typePointer = Type $ unsafePerformIO $ newForeignPtr_ ffi_type_pointer


typeStructSizeFieldSize :: Int
typeStructSizeFieldSize = sizeOf (undefined :: CUInt)


typeStructAlignmentFieldSize :: Int
typeStructAlignmentFieldSize = sizeOf (undefined :: CShort)


typeStructTypeFieldSize :: Int
typeStructTypeFieldSize = sizeOf (undefined :: CShort)


typeStructElementsFieldSize :: Int
typeStructElementsFieldSize = sizeOf (undefined :: Ptr (Ptr ()))


typeStructEachElementFieldSize :: Int
typeStructEachElementFieldSize = sizeOf (undefined :: Ptr ())


typeStructSizeFieldPtr :: Ptr () -> Ptr CUInt
typeStructSizeFieldPtr theType = castPtr theType


typeStructAlignmentFieldPtr :: Ptr () -> Ptr CShort
typeStructAlignmentFieldPtr theType =
  castPtr $ plusPtr theType typeStructSizeFieldSize


typeStructTypeFieldPtr :: Ptr () -> Ptr CShort
typeStructTypeFieldPtr theType =
  castPtr $ plusPtr theType
                    (typeStructSizeFieldSize
                     + typeStructAlignmentFieldSize)


typeStructElementsFieldPtr :: Ptr () -> Ptr (Ptr (Ptr ()))
typeStructElementsFieldPtr theType =
  castPtr $ plusPtr theType
                    (typeStructSizeFieldSize
                     + typeStructAlignmentFieldSize
                     + typeStructTypeFieldSize)


typeStructGivenElementFieldPtr :: Ptr () -> Int -> Ptr (Ptr ())
typeStructGivenElementFieldPtr theType index =
  castPtr $ plusPtr theType
                    (typeStructSizeFieldSize
                     + typeStructAlignmentFieldSize
                     + typeStructTypeFieldSize
                     + typeStructElementsFieldSize
                     + index * typeStructEachElementFieldSize)


typeStructIsStaticallyAllocated :: Ptr () -> Bool
typeStructIsStaticallyAllocated theType =
  case () of () | theType == typeStructFromType typeVoid -> True
                | theType == typeStructFromType typeUInt8 -> True
                | theType == typeStructFromType typeSInt8 -> True
                | theType == typeStructFromType typeUInt16 -> True
                | theType == typeStructFromType typeSInt16 -> True
                | theType == typeStructFromType typeUInt32 -> True
                | theType == typeStructFromType typeSInt32 -> True
                | theType == typeStructFromType typeUInt64 -> True
                | theType == typeStructFromType typeSInt64 -> True
                | otherwise -> False


typeStructFromType :: Type -> Ptr ()
typeStructFromType (Type foundType) = unsafeForeignPtrToPtr foundType


computeTypeStructSizeOneLevel :: [Ptr ()] -> Int
computeTypeStructSizeOneLevel typeStructs = typeStructSizeFieldSize
                                  + typeStructAlignmentFieldSize
                                  + typeStructTypeFieldSize
                                  + typeStructElementsFieldSize
                                  + (1 + length typeStructs)
                                    * typeStructEachElementFieldSize


getTypeStructSizeOneLevelAndDown :: [Ptr ()] -> IO Int
getTypeStructSizeOneLevelAndDown typeStructs = do
  let sizeOneLevel = computeTypeStructSizeOneLevel typeStructs
  foldM (\result typeStruct -> do
          if typeStructIsStaticallyAllocated typeStruct
            then return result
            else do
              childTypeStructs <- peekTypeStructOneLevel typeStruct
              sizeChild <- getTypeStructSizeOneLevelAndDown childTypeStructs
              return $ result + sizeChild)
        (computeTypeStructSizeOneLevel typeStructs)
        typeStructs


pokeTypeStructOneLevel :: Ptr () -> [Ptr ()] -> IO ()
pokeTypeStructOneLevel typeStruct fieldTypeStructs = do
  poke (typeStructSizeFieldPtr typeStruct) 0
  poke (typeStructAlignmentFieldPtr typeStruct) 0
  poke (typeStructTypeFieldPtr typeStruct) 13
  poke (typeStructElementsFieldPtr typeStruct)
       (typeStructGivenElementFieldPtr typeStruct 0)
  mapM_ (\(fieldTypeStruct, index) ->
           poke (typeStructGivenElementFieldPtr typeStruct index)
                fieldTypeStruct)
        $ zip fieldTypeStructs [0..]
  poke (typeStructGivenElementFieldPtr typeStruct
                                       (length fieldTypeStructs))
       nullPtr


pokeTypeStructOneLevelAndDown :: Ptr () -> [Ptr ()] -> IO ()
pokeTypeStructOneLevelAndDown typeStruct fieldTypeStructs = do
  (newFieldTypeStructs, _)
    <- foldM (\(newFieldTypeStructs, newFieldTypeStruct)
               oldFieldTypeStruct -> do
                 isStruct <- peekTypeStructIsStruct oldFieldTypeStruct
                 if isStruct
                   then do
                     immediateSubfields
                       <- peekTypeStructOneLevel oldFieldTypeStruct
                     pokeTypeStructOneLevelAndDown newFieldTypeStruct 
                                                   immediateSubfields
                     let nextNewFieldTypeStruct =
                           incrementTypeStructPtr newFieldTypeStruct
                                                  immediateSubfields
                     return (newFieldTypeStructs ++ [newFieldTypeStruct],
                             nextNewFieldTypeStruct)
                   else return (newFieldTypeStructs
                                ++ [oldFieldTypeStruct],
                                newFieldTypeStruct))
             ([], incrementTypeStructPtr typeStruct fieldTypeStructs)
             fieldTypeStructs
  pokeTypeStructOneLevel typeStruct newFieldTypeStructs


peekTypeStructIsStruct :: Ptr () -> IO Bool
peekTypeStructIsStruct typeStruct = do
  typeField <- peek $ typeStructTypeFieldPtr typeStruct
  return $ typeField == 13


peekTypeStructOneLevel :: Ptr () -> IO [Ptr ()]
peekTypeStructOneLevel typeStruct = do
  let loop results givenElementFieldPtr = do
        possibleResult <- peek givenElementFieldPtr
        if possibleResult == nullPtr
          then return results
          else loop (results ++ [possibleResult])
                    (plusPtr givenElementFieldPtr
                             (sizeOf (undefined :: Ptr ())))
  firstGivenElementFieldPtr
    <- peek $ typeStructElementsFieldPtr typeStruct
  loop [] firstGivenElementFieldPtr


incrementTypeStructPtr :: Ptr () -> [Ptr ()] -> Ptr ()
incrementTypeStructPtr typeStruct fieldTypeStructs =
  plusPtr typeStruct $ computeTypeStructSizeOneLevel fieldTypeStructs


typeShouldBeFlattenedInCIF :: Type -> Bool
typeShouldBeFlattenedInCIF=
  not . typeStructIsStaticallyAllocated . typeStructFromType


cifStructHeaderSize :: Int
cifStructHeaderSize = sizeOf (undefined :: CInt)
                      + sizeOf (undefined :: CUInt)
                      + sizeOf (undefined :: Ptr (Ptr ()))
                      + sizeOf (undefined :: Ptr ())
                      + sizeOf (undefined :: CUInt)
                      + sizeOf (undefined :: CUInt)


computeCIFStructArgumentTypeListSize :: [Type] -> Int
computeCIFStructArgumentTypeListSize argumentTypes =
  length argumentTypes * sizeOf (undefined :: Ptr ())


getCIFStructSize :: (Type, [Type]) -> IO Int
getCIFStructSize (returnType, argumentTypes) = do
  let typesToFlatten = filter typeShouldBeFlattenedInCIF
                              (returnType : argumentTypes)
  sizeForTypesToFlatten
    <- foldM (\result typeToFlatten -> do
                immediateSubfields
                  <- peekTypeStructOneLevel $ typeStructFromType typeToFlatten
                size <- getTypeStructSizeOneLevelAndDown immediateSubfields
                return $ result + size)
             0
             typesToFlatten
  return $ cifStructHeaderSize
           + computeCIFStructArgumentTypeListSize argumentTypes
           + sizeForTypesToFlatten


typeStruct :: [Type] -> Type
typeStruct topLevelFieldTypes = unsafePerformIO $ do
  let topLevelTypeStructs = map typeStructFromType topLevelFieldTypes
  totalSize <- getTypeStructSizeOneLevelAndDown topLevelTypeStructs
  typeStruct <- mallocBytes totalSize
  pokeTypeStructOneLevelAndDown typeStruct topLevelTypeStructs
  newForeignPtr finalizerFree typeStruct >>= return . Type


cif :: ABI -> Type -> [Type] -> IO CIF
cif abi returnType argumentTypes = do
  totalSize <- getCIFStructSize (returnType, argumentTypes)
  cifStruct <- mallocBytes totalSize
  let argumentTypeStructList =
        plusPtr cifStruct cifStructHeaderSize
      flattenedTypeStructArea =
        plusPtr argumentTypeStructList
                $ computeCIFStructArgumentTypeListSize argumentTypes
  ((returnTypeStruct : argumentTypeStructs), _)
    <- foldM (\(resultTypeStructs, oldTail) theType -> do
                 if typeShouldBeFlattenedInCIF theType
                   then do
                     let oldTypeStruct = typeStructFromType theType
                     immediateSubfields
                       <- peekTypeStructOneLevel oldTypeStruct
                     pokeTypeStructOneLevelAndDown oldTail
                                                   immediateSubfields
                     let newTail =
                           incrementTypeStructPtr oldTail
                                                  immediateSubfields
                     return (resultTypeStructs ++ [oldTail], newTail)
                   else return (resultTypeStructs
                                ++ [typeStructFromType theType],
                                oldTail))
             ([], flattenedTypeStructArea)
             (returnType : argumentTypes)
  mapM_ (\(argumentTypeStruct, index) ->
           poke (plusPtr argumentTypeStructList
                         $ index * sizeOf (undefined :: Ptr ()))
                argumentTypeStruct)
        $ zip argumentTypeStructs [0..]
  let abiCode = fromEnum abi
  statusCode <- ffi_prep_cif cifStruct
                             (fromIntegral abiCode)
                             (fromIntegral $ length argumentTypeStructs)
                             returnTypeStruct
                             argumentTypeStructList
  let status = toEnum $ fromIntegral statusCode
  if status == OK
    then newForeignPtr finalizerFree cifStruct >>= return . CIF
    else do
      free cifStruct
      error $ case status of
                BadTypedef -> "Bad type definition when constructing CIF."
                BadABI -> "Bad ABI when constructing CIF."
