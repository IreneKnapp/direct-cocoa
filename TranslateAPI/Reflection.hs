{-# LANGUAGE ForeignFunctionInterface #-}
module Reflection (loadReflectionData)
  where

import Control.Monad
import Data.Function
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import Foreign
import Foreign.C

import Types
import Util
import LibFFI


foreign import ccall "objc_getClassList" objc_getClassList
    :: Ptr (Ptr ()) -> CInt -> IO CInt
foreign import ccall "class_getName" class_getName
    :: Ptr () -> IO CString
foreign import ccall "class_getSuperclass" class_getSuperclass
    :: Ptr () -> IO (Ptr ())
foreign import ccall "class_copyMethodList" class_copyMethodList
    :: Ptr () -> Ptr (CUInt) -> IO (Ptr (Ptr ()))


loadReflectionData :: [Framework] -> IO [Framework]
loadReflectionData frameworks = do
  frameworks <- loadAllClasses frameworks
  return frameworks


loadAllClasses :: [Framework] -> IO [Framework]
loadAllClasses frameworks = do
  nClasses <- objc_getClassList nullPtr 0
  classAndSuperclassList
    <- allocaBytes (fromIntegral nClasses * sizeOf (undefined :: Ptr ()))
                   (\classListBuffer -> do
                      _ <- objc_getClassList classListBuffer nClasses
                      mapM (\classIndex -> do
                              classPtr <- peekElemOff classListBuffer classIndex
                              preLoadClass classPtr)
                           [0 .. fromIntegral nClasses - 1]
                      >>= return . catMaybes)
       >>= return . sortBy (\(nameA, maybeSuperclassNameA)
                             (nameB, maybeSuperclassNameB) ->
                             mconcat [case (maybeSuperclassNameA,
                                             maybeSuperclassNameB) of
                                         (Nothing, Nothing) -> EQ
                                         (Nothing, Just _) -> LT
                                         (Just _, Nothing) -> GT
                                         (Just superclassNameA,
                                          Just superclassNameB) ->
                                            compare superclassNameA 
                                                    superclassNameB,
                                       compare nameA nameB])
  let topLevelClassList =
        catMaybes $ map (\(a, maybeB) ->
                            case maybeB of
                              Nothing -> Just a
                              Just _ -> Nothing)
                         classAndSuperclassList
      superclassMap =
        Map.fromList
         $ catMaybes $ map (\(a, maybeB) ->
                               case maybeB of
                                 Nothing -> Nothing
                                 Just b -> Just (a, b))
                           classAndSuperclassList
      subclassMap =
        Map.fromList
         $ map (\classes@((_, forASuperclass):_) ->
                   (forASuperclass, map fst classes))
         $ groupBy ((==) `on` snd)
         $ sortBy (compare `on` snd)
         $ catMaybes $ map (\(a, maybeB) ->
                               case maybeB of
                                 Nothing -> Nothing
                                 Just b -> Just (a, b))
                           classAndSuperclassList
  let visitClassList frameworks depth classList = do
        foldM (\frameworks foundClass -> do
                  visitClass frameworks depth foundClass)
              frameworks
              classList
      visitClass frameworks depth theClass = do
        putStrLn $ replicate (depth * 4) ' ' ++ theClass
        (frameworks, found)
          <- foldM
              (\(frameworks, found) framework -> do
                  if found
                    then return (frameworks ++ [framework], True)
                    else do
                      (classes, found)
                        <- foldM
                            (\(classes, found) foundClass -> do
                                if found
                                  then return (classes ++ [foundClass], True)
                                  else if className foundClass == theClass
                                    then do
                                      return (classes, True)
                                      {- TODO: Once loadClass is implemented,
                                               delete the line before and
                                               uncomment this.
                                      foundClass
                                        <- loadClass (Just foundClass)
                                                     theClass
                                                     (Map.lookup theClass
                                                                 superclassMap)
                                                     (case Map.lookup
                                                            theClass
                                                            subclassMap
                                                      of
                                                        Nothing -> []
                                                        Just subclasses ->
                                                          subclasses)
                                      return (classes ++ [foundClass], True)
                                      -}
                                    else return (classes ++ [foundClass],
                                                 False))
                            ([], False)
                            $ frameworkClasses framework
                      return (frameworks
                              ++ [framework {
                                      frameworkClasses = classes
                                    }],
                              found))
              ([], False)
              frameworks
        frameworks
          <- if not found
               then do
                 putStrLn $ "### Class not found."
                 return frameworks
               else return frameworks
        case Map.lookup theClass subclassMap of
          Nothing -> return frameworks
          Just subclassList -> visitClassList frameworks (depth + 1)
                                              subclassList
  frameworks <- visitClassList frameworks 0 topLevelClassList
  return frameworks


preLoadClass :: Ptr () -> IO (Maybe (String, Maybe String))
preLoadClass classPtr = do
  nameCString <- class_getName classPtr
  name <- peekCString nameCString
  if classIsIgnored name
    then return Nothing
    else do
      superclassPtr <- class_getSuperclass classPtr
      maybeSuperclassName
        <- if superclassPtr == nullPtr
             then return Nothing
             else do
               superclassNameCString <- class_getName superclassPtr
               peekCString superclassNameCString >>= return . Just
      return $ Just (name, maybeSuperclassName)


classIsIgnored :: String -> Bool
classIsIgnored name =
  (name !! 0 == '_')
  || (name == "Object")
  || (isSuffixOf "_ivars" name)


{- TODO: This seems to be half-finished, so I'm commenting it out for now.
loadClass :: Maybe ClassDefinition
          -> String
          -> Maybe String
          -> [String]
          -> IO ClassDefinition
loadClass maybeClassDefinition name maybeSuperclassName subclassNames = do
  classDefinition <- case maybeClassDefinition of
                       Nothing ->
                         return $ Class {
                                      className = name,
                                      classMethods = [],
                                      classSuperclass = maybeSuperclassName,
                                      classSubclasses = subclassNames
                                    }
                       Just classDefinition ->
                         return $ classDefinition {
                                      classSuperclass = maybeSuperclassName,
                                      classSubclasses = subclassNames
                                    }
  (methodBuffer, methodCount)
    <- alloca (\countPtr -> do
                  methodBuffer <- class_copyMethodList classPtr countPtr
                  count <- peek countPtr
                  return (methodBuffer, count))
  methods <- if methodBuffer == nullPtr
               then return []
               else do
                 foldM (\(methods, found) index ->
                           ...
                 free methodBuffer
                 return ...
  return classDefinition {
             classMethods = methods
           }
-}
