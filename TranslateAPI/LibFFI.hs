{-# LANGUAGE ForeignFunctionInterface #-}
module LibFFI ()
  where


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


data Type = TypeUChar
          | TypeSChar
          | TypeUShort
          | TypeSShort
          | TypeUInt
          | TypeSInt
          | TypeULong
          | TypeSLong


newtype CIF = Ptr ()
