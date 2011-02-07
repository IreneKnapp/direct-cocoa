{-# LANGUAGE ForeignFunctionInterface, CPP #-}
module LibFFI ()
  where

import Foreign
import Foreign.C


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


newtype Type = Type (Ptr ())
newtype CIF = CIF (Ptr ())
newtype Closure = Closure (Ptr ())
newtype Raw = Raw (Ptr ())
newtype RawClosure = RawClosure (Ptr ())


foreign import ccall "ffi_type_void" typeVoid :: Type
foreign import ccall "ffi_type_uint8" typeUInt8 :: Type
foreign import ccall "ffi_type_sint8" typeSInt8 :: Type
foreign import ccall "ffi_type_uint16" typeUInt16 :: Type
foreign import ccall "ffi_type_sint16" typeSInt16 :: Type
foreign import ccall "ffi_type_uint32" typeUInt32 :: Type
foreign import ccall "ffi_type_sint32" typeSInt32 :: Type
foreign import ccall "ffi_type_uint64" typeUInt64 :: Type
foreign import ccall "ffi_type_sint64" typeSInt64 :: Type
foreign import ccall "ffi_type_float" typeFloat :: Type
foreign import ccall "ffi_type_double" typeDouble :: Type
foreign import ccall "ffi_type_longdouble" typeLongDouble :: Type
foreign import ccall "ffi_type_pointer" typePointer :: Type


foreign import ccall "ffi_prep_closure" ffi_prep_closure
  :: Closure
  -> CIF
  -> FunPtr (CIF -> Ptr () -> Ptr (Ptr ()) -> Ptr () -> IO  ())
  -> Ptr ()
  -> IO CInt
foreign import ccall "ffi_prep_cif" ffi_prep_cif
  :: CIF -> CInt -> CUInt -> Type -> Ptr (Type) -> IO CInt
foreign import ccall "ffi_call" ffi_call
  :: CIF -> Ptr () -> Ptr () -> Ptr (Ptr ()) -> IO ()
