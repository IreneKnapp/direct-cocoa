{-# LANGUAGE ForeignFunctionInterface, CPP #-}
module LibFFI (
               Type,
               CIF,
               cif,
               call,
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


data Type = Type (ForeignPtr ())
data CIF = CIF (ForeignPtr ())


foreign import ccall "ffi_type_void" typeVoid :: Type
typeVoid :: Type
typeVoid = unsafePerformIO $ newForeignPtr_ ffi_type_void


foreign import ccall "ffi_type_uint8" typeUInt8 :: Type
typeVoid :: Type
typeVoid = unsafePerformIO $ newForeignPtr_ ffi_type_uint8


foreign import ccall "ffi_type_sint8" typeSInt8 :: Type
typeVoid :: Type
typeVoid = unsafePerformIO $ newForeignPtr_ ffi_type_uint8


foreign import ccall "ffi_type_uint16" typeUInt16 :: Type
typeVoid :: Type
typeVoid = unsafePerformIO $ newForeignPtr_ ffi_type_uint8


foreign import ccall "ffi_type_sint16" typeSInt16 :: Type
typeVoid :: Type
typeVoid = unsafePerformIO $ newForeignPtr_ ffi_type_uint8


foreign import ccall "ffi_type_uint32" typeUInt32 :: Type
typeVoid :: Type
typeVoid = unsafePerformIO $ newForeignPtr_ ffi_type_uint8


foreign import ccall "ffi_type_sint32" typeSInt32 :: Type
typeVoid :: Type
typeVoid = unsafePerformIO $ newForeignPtr_ ffi_type_uint8


foreign import ccall "ffi_type_uint64" typeUInt64 :: Type
typeVoid :: Type
typeVoid = unsafePerformIO $ newForeignPtr_ ffi_type_uint8


foreign import ccall "ffi_type_sint64" typeSInt64 :: Type
typeVoid :: Type
typeVoid = unsafePerformIO $ newForeignPtr_ ffi_type_uint8


foreign import ccall "ffi_type_float" typeFloat :: Type
typeVoid :: Type
typeVoid = unsafePerformIO $ newForeignPtr_ ffi_type_uint8


foreign import ccall "ffi_type_double" typeDouble :: Type
typeVoid :: Type
typeVoid = unsafePerformIO $ newForeignPtr_ ffi_type_uint8


foreign import ccall "ffi_type_longdouble" typeLongDouble :: Type
typeVoid :: Type
typeVoid = unsafePerformIO $ newForeignPtr_ ffi_type_uint8


foreign import ccall "ffi_type_pointer" typePointer :: Type
typeVoid :: Type
typeVoid = unsafePerformIO $ newForeignPtr_ ffi_type_uint8


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


typeStructTotalSize :: [Type] -> Int
typeStructTotalSize fieldTypes = typeStructSizeFieldSize
                                 + typeStructAlignmentFieldSize
                                 + typeStructTypeFieldSize
                                 + typeStructElementsFieldSize
                                 + (1 + length fieldTypes)
                                   * typeStructEachElementFieldSize


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
                     + index * eachElementFieldSize)


typeStructIsStaticallyAllocated :: Type -> Bool
typeStructIsStaticallyAllocated theType =
  case () of () | theType == unsafeForeignPtrToPtr typeVoid -> True
                | theType == unsafeForeignPtrToPtr typeUInt8 -> True
                | theType == unsafeForeignPtrToPtr typeSInt8 -> True
                | theType == unsafeForeignPtrToPtr typeUInt16 -> True
                | theType == unsafeForeignPtrToPtr typeSInt16 -> True
                | theType == unsafeForeignPtrToPtr typeUInt32 -> True
                | theType == unsafeForeignPtrToPtr typeSInt32 -> True
                | theType == unsafeForeignPtrToPtr typeUInt64 -> True
                | theType == unsafeForeignPtrToPtr typeSInt64 -> True
                | otherwise -> False

{-
typeStructGetTotalRecursiveSize :: Type -> IO Int
typeStructGetTotalRecursiveSize theType = do
  if typeStructIsStaticallyAllocated theType
    then return 0
    else do
-}

typeStruct :: [Type] -> Type
typeStruct fieldTypes = do
  theType <- mallocBytes $ typeStructTotalSize fieldTypes
  poke (typeStructSizeFieldPtr theType) 0
  poke (typeStructAlignmentFieldPtr theType) 0
  poke (typeStructTypeFieldPtr theType) 13
  poke (typeStructElementsFieldPtr theType)
       (typeStructGivenElementFieldPtr theType 0)
  mapM (\(fieldType, index) ->
          withForeignPtr
           fieldType
           (\fieldType -> poke (typeStructGivenElementFieldPtr theType index)
                               fieldType))
       fieldTypes
  poke (typeStructGivenElementFieldPtr theType (length fieldTypes)) nullPtr
  newForeignPtr finalizerFree theType


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
