{-# LANGUAGE ForeignFunctionInterface, CPP #-}
module LibFFI (
               Type,
               CIF,
               --cif,
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

import Foreign
import Foreign.C
import System.IO.Unsafe


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
instance Enum Status where
  fromEnum OK = 0
  fromEnum BadTypedef = 1
  fromEnum BadABI = 2
  toEnum 0 = OK
  toEnum 1 = BadTypedef
  toEnum 2 = BadABI         


data Type = Type (ForeignPtr ()) deriving (Eq)
data CIF = CIF (ForeignPtr ())


foreign import ccall "ffi_type_void" ffi_type_void :: Ptr ()
typeVoid :: Type
typeVoid = Type $ unsafePerformIO $ newForeignPtr_ ffi_type_void


foreign import ccall "ffi_type_uint8" ffi_type_uint8 :: Ptr ()
typeUInt8 :: Type
typeUInt8 = Type $ unsafePerformIO $ newForeignPtr_ ffi_type_uint8


foreign import ccall "ffi_type_sint8" ffi_type_sint8 :: Ptr ()
typeSInt8 :: Type
typeSInt8 = Type $ unsafePerformIO $ newForeignPtr_ ffi_type_sint8


foreign import ccall "ffi_type_uint16" ffi_type_uint16 :: Ptr ()
typeUInt16 :: Type
typeUInt16 = Type $ unsafePerformIO $ newForeignPtr_ ffi_type_uint16


foreign import ccall "ffi_type_sint16" ffi_type_sint16 :: Ptr ()
typeSInt16 :: Type
typeSInt16 = Type $ unsafePerformIO $ newForeignPtr_ ffi_type_sint16


foreign import ccall "ffi_type_uint32" ffi_type_uint32 :: Ptr ()
typeUInt32 :: Type
typeUInt32 = Type $ unsafePerformIO $ newForeignPtr_ ffi_type_uint32


foreign import ccall "ffi_type_sint32" ffi_type_sint32 :: Ptr ()
typeSInt32 :: Type
typeSInt32 = Type $ unsafePerformIO $ newForeignPtr_ ffi_type_sint32


foreign import ccall "ffi_type_uint64" ffi_type_uint64 :: Ptr ()
typeUInt64 :: Type
typeUInt64 = Type $ unsafePerformIO $ newForeignPtr_ ffi_type_uint64


foreign import ccall "ffi_type_sint64" ffi_type_sint64 :: Ptr ()
typeSInt64 :: Type
typeSInt64 = Type $ unsafePerformIO $ newForeignPtr_ ffi_type_sint64


foreign import ccall "ffi_type_float" ffi_type_float :: Ptr ()
typeFloat :: Type
typeFloat = Type $ unsafePerformIO $ newForeignPtr_ ffi_type_float


foreign import ccall "ffi_type_double" ffi_type_double :: Ptr ()
typeDouble :: Type
typeDouble = Type $ unsafePerformIO $ newForeignPtr_ ffi_type_double


foreign import ccall "ffi_type_longdouble" ffi_type_longdouble :: Ptr ()
typeLongDouble :: Type
typeLongDouble = Type $ unsafePerformIO $ newForeignPtr_ ffi_type_longdouble


foreign import ccall "ffi_type_pointer" ffi_type_pointer :: Ptr ()
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
  let fromType (Type foundType) = unsafeForeignPtrToPtr foundType
  in case () of () | theType == fromType typeVoid -> True
                   | theType == fromType typeUInt8 -> True
                   | theType == fromType typeSInt8 -> True
                   | theType == fromType typeUInt16 -> True
                   | theType == fromType typeSInt16 -> True
                   | theType == fromType typeUInt32 -> True
                   | theType == fromType typeSInt32 -> True
                   | theType == fromType typeUInt64 -> True
                   | theType == fromType typeSInt64 -> True
                   | otherwise -> False


typeStructGetTotalRecursiveSize :: Ptr () -> IO Int
typeStructGetTotalRecursiveSize theType = do
  if typeStructIsStaticallyAllocated theType
    then return 0
    else do
      return 0


typeStruct :: [Type] -> Type
typeStruct topLevelFieldTypes = unsafePerformIO $ do
  let fromType :: Type -> Ptr ()
      fromType (Type foundType) = unsafeForeignPtrToPtr foundType
      
      sizeOneLevel :: [Ptr ()] -> Int
      sizeOneLevel typeStructs = typeStructSizeFieldSize
                                 + typeStructAlignmentFieldSize
                                 + typeStructTypeFieldSize
                                 + typeStructElementsFieldSize
                                 + (1 + length typeStructs)
                                   * typeStructEachElementFieldSize
      
      sizeOneLevelAndDown :: [Ptr ()] -> Int
      sizeOneLevelAndDown typeStructs = unsafePerformIO $ do
        foldM (\result typeStruct -> do
                if typeStructIsStaticallyAllocated typeStruct
                  then result
                  else result + sizeOneLevel ...
                  )
              (sizeOneLevel fieldTypes)
              typeStructs
      
      pokeOneLevel :: Ptr () -> [Type] -> IO ()
      pokeOneLevel theType fieldTypes = do
        poke (typeStructSizeFieldPtr theType) 0
        poke (typeStructAlignmentFieldPtr theType) 0
        poke (typeStructTypeFieldPtr theType) 13
        poke (typeStructElementsFieldPtr theType)
             (typeStructGivenElementFieldPtr theType 0)
        mapM (\(Type fieldType, index) ->
                withForeignPtr
                 fieldType
                 (\fieldType ->
                   poke (typeStructGivenElementFieldPtr theType index)
                        fieldType))
             $ zip fieldTypes [0..]
        poke (typeStructGivenElementFieldPtr theType (length fieldTypes))
             nullPtr
      
      peekIsStruct :: Ptr () -> IO Bool
      peekIsStruct typeStruct = do
        typeField <- peek $ typeStructTypeFieldPtr typeStruct
        return typeField == 13
      
      peekOneLevel :: Ptr () -> IO [Ptr ()]
      peekOneLevel typeStruct = do
        let loop results givenElementFieldPtr = do
              possibleResult <- peek givenElementFieldPtr
              if possibleResult == nullPtr
                then return results
                else loop (results ++ [possibleResult])
                          (plusPtr givenElementFieldPtr
                                   (sizeof (undefined :: Ptr ())))
        firstGivenElementFieldPtr
          <- peek $ typeStructElementsFieldPtr typeStruct
        loop [] firstGivenElementFieldPtr
      
      incrementStructPtr :: Ptr () -> [Type] -> Ptr ()
      incrementStructPtr theType fieldTypes =
        plusPtr theType $ sizeOneLevel fieldTypes
      
      topLevelTypeStructs :: [Ptr ()]
      topLevelTypeStructs = map fromType topLevelFieldTypes
  
  theType <- mallocBytes $ sizeOneLevel topLevelTypeStructs
  pokeOneLevel theType topLevelTypeStructs
  newForeignPtr finalizerFree theType >>= return . Type


foreign import ccall "ffi_prep_cif" ffi_prep_cif
  :: Ptr () -> CInt -> CUInt -> Ptr () -> Ptr (Ptr ()) -> IO CInt

{-
cif :: ABI -> Type -> [Type] -> IO CIF
cif abi returnType argumentTypes = do
  cif <- mallocBytes ...
  status <- ffi_prep_cif cif 
  newForeignPtr finalizerFree cif
-}

foreign import ccall "ffi_call" ffi_call
  :: Ptr () -> Ptr () -> Ptr () -> Ptr (Ptr ()) -> IO ()
