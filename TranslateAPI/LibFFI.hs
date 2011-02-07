{-# LANGUAGE ForeignFunctionInterface, CPP #-}
module LibFFI ()
  where

-- We need to define the architecture symbols prior to our inclusion of
-- limits.h, or it produces an error.
#define MACOSX
#ifdef x86_64_HOST_ARCH
#define __x86_64__
#elif i386_HOST_ARCH
#define __i386__
#elif ppc_HOST_ARCH
#define __ppc__
#elif ppc64_HOST_ARCH
#define __ppc64__
#elif arm_HOST_ARCH
#define __arm__
#endif

-- We need to suppress inclusion of limits.h by ffi.h, because it makes
-- unhelpful definitions of all the values I enumerate below, putting them
-- in terms of gcc's predefined symbols, which we don't have access to.
-- To achieve the suppression, we can't simply #define _LIMITS_H_, because
-- of an arguable bug in gcc's cpp:  Macros that surround entire files are
-- treated specially rather than as normal symbol definitions, as part of
-- an optimization to avoid repeated lexing.  Anyway, since we can't do
-- that, we instead include limits.h ourselves so that it can set itself
-- up to not be included a second time.
#include <limits.h>
#undef SCHAR_MAX
#undef SHRT_MAX
#undef INT_MAX
#undef LONG_MAX
#undef LONG_LONG_MAX

-- We need to define these symbols; ffi.h uses them, and we can't get them
-- from system headers for reasons described above.
#ifdef x86_64_HOST_ARCH
#define SCHAR_MAX 127
#define SHRT_MAX 32767
#define INT_MAX 2147483647
#define LONG_MAX 9223372036854775807L
#define LONG_LONG_MAX 9223372036854775807LL
#elif i386_HOST_ARCH
#define SCHAR_MAX 127
#define SHRT_MAX 32767
#define INT_MAX 2147483647
#define LONG_MAX 2147483647L
#define LONG_LONG_MAX 0x7fffffffffffffffLL
#elif ppc_HOST_ARCH
#define SCHAR_MAX 127
#define SHRT_MAX 32767
#define INT_MAX 2147483647
#define LONG_MAX 2147483647L
#define LONG_LONG_MAX 0x7fffffffffffffffLL
#elif ppc64_HOST_ARCH
#error I wasn't able to find these constants for ppc64.
#elif arm_HOST_ARCH
#define SCHAR_MAX 127
#define SHRT_MAX 32767
#define INT_MAX 2147483647
#define LONG_MAX 2147483647L
#define LONG_LONG_MAX 9223372036854775807LL
#endif

-- Finally!
#include <ffi/ffi.h>

import Foreign
import Foreign.C


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


typeUChar :: Type
#if ffi_type_uchar == ffi_type_uint8
typeUChar = typeUInt8
#else
typeUChar = error "typeUChar undefined"
#endif


typeSChar :: Type
#if ffi_type_schar == ffi_type_sint8
typeSChar = typeSInt8
#else
typeSChar = error "typeSChar undefined"
#endif


typeUShort :: Type
#if ffi_type_ushort == ffi_type_uint16
typeUShort = typeUInt16
#elif ffi_type_ushort == ffi_type_uint32
typeUShort = typeUint32
#else
typeUShort = error "typeSShort undefined"
#endif


typeSShort :: Type
#if ffi_type_sshort == ffi_type_sint16
typeSShort = typeSInt16
#elif ffi_type_sshort == ffi_type_sint32
typeSShort = typeSint32
#else
typeSShort = error "typeSShort undefined"
#endif


typeUInt :: Type
#if ffi_type_uint == ffi_type_uint16
typeUInt = typeUInt16
#elif ffi_type_uint == ffi_type_uint32
typeUInt = typeUInt32
#elif ffi_type_uint == ffi_type_uint64
typeUInt = typeUint64
#else
typeUInt = error "typeUInt undefined"
#endif


typeSInt :: Type
#if ffi_type_sint == ffi_type_sint16
typeSInt = typeSInt16
#elif ffi_type_sint == ffi_type_sint32
typeSInt = typeSInt32
#elif ffi_type_sint == ffi_type_sint64
typeSInt = typeSint64
#else
typeSInt = error "typeSInt undefined"
#endif


typeULong :: Type
typeULong = typeUInt64


typeSLong :: Type
typeSLong = typeSInt64


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


foreign import ccall "ffi_raw_call" ffi_raw_call
  :: CIF -> FunPtr (IO ()) -> Ptr () -> Raw -> IO ()
foreign import ccall "ffi_ptrarray_to_raw" ffi_ptrarray_to_raw
  :: CIF -> Ptr (Ptr ()) -> Raw -> IO ()
foreign import ccall "ffi_raw_to_ptrarray" ffi_raw_to_ptrarray
  :: CIF -> Raw -> Ptr (Ptr ()) -> IO ()
foreign import ccall "ffi_raw_size" ffi_raw_size
  :: CIF -> IO CSize
foreign import ccall "ffi_java_raw_call" ffi_java_raw_call
  :: CIF -> FunPtr (IO ()) -> Ptr () -> Raw -> IO ()
foreign import ccall "ffi_java_ptrarray_to_raw" ffi_java_ptrarray_to_raw
  :: CIF -> Ptr (Ptr ()) -> Raw -> IO ()
foreign import ccall "ffi_java_raw_to_ptrarray" ffi_java_raw_to_ptrarray
  :: CIF ->  Raw -> Ptr (Ptr ()) -> IO ()
foreign import ccall "ffi_java_raw_size" ffi_java_raw_size
  :: CIF -> IO CSize
foreign import ccall "ffi_prep_closure" ffi_prep_closure
  :: Closure
  -> CIF
  -> FunPtr (CIF -> Ptr () -> Ptr (Ptr ()) -> Ptr () -> IO  ())
  -> Ptr ()
  -> IO CInt
foreign import ccall "ffi_prep_raw_closure" ffi_prep_raw_closure
  :: RawClosure
  -> CIF
  -> FunPtr (CIF -> Ptr () -> Raw -> Ptr () -> IO ())
  -> Ptr ()
  -> IO CInt
foreign import ccall "ffi_prep_java_raw_closure" ffi_prep_java_raw_closure
  :: RawClosure
  -> CIF
  -> FunPtr (CIF -> Ptr () -> Raw -> Ptr () -> IO ())
  -> Ptr ()
  -> IO CInt
{-
foreign import ccall "ffi_prep_cif" ffi_prep_cif
  :: CIF
  -> abi
-}