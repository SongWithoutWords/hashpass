{-# language GeneralizedNewtypeDeriving #-}
{-# language OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import Data.ByteString(ByteString)
import Data.ByteString.Conversion.To
import Data.List(find)
import Data.Word

import Crypto.Hash.SHA512

someFunc :: IO ()
someFunc = putStrLn "someFunc"

newtype Master = Master ByteString
  deriving(Eq, Show)

newtype Service = Service ByteString
  deriving(Eq, Show)

newtype Iteration = Iteration Word
  deriving(Eq, Num, Show)

newtype Salt = Salt Word64
  deriving(Eq, Show)

newtype LowerCount = LowerCount Word
  deriving(Eq, Show)

newtype UpperCount = UpperCount Word
  deriving(Eq, Show)

newtype NumberCount = NumberCount Word
  deriving(Eq, Show)

newtype SymbolCount = SymbolCount Word
  deriving(Eq, Show)

newtype AnyCount = AnyCount Word
  deriving(Eq, Show)

data Requirements = Requirements
  { lowerCount :: LowerCount
  , upperCount :: UpperCount
  , numberCount :: NumberCount
  , symbolCount :: SymbolCount
  , anyCount :: AnyCount
  }
  deriving(Eq, Show)

data Params = Params
  { service :: Service
  , salt :: Salt
  , iteration :: Iteration
  , requirements :: Requirements
  }
  deriving(Eq, Show)

saltValue :: Params -> Word64
saltValue p = let (Salt s) = salt p in s

newtype AllParams = AllParams [Params]
  deriving(Eq, Show)

data Command
  = Add Service Params
  | Increment Service
  | DisplayAllParams
  | Query Service

isService :: Service -> Params -> Bool
isService s = (== s) . service

contains :: Service -> AllParams -> Bool
contains s (AllParams allParams) =
  any (isService s) allParams

add :: Params -> AllParams -> AllParams
add params existing =
  if contains (service params) existing
  then existing
  else
    let AllParams paramsList = existing
    in AllParams $ params : paramsList

increment :: Service -> AllParams -> AllParams
increment s (AllParams paramList) = AllParams $
  fmap
    (\params ->
       if isService s params
       then params {iteration = (iteration params) + 1}
       else params)
    paramList

query :: Master -> Service -> AllParams -> Maybe ByteString
query m s (AllParams paramList) =
  compute m <$> find (isService s) paramList

compute :: Master -> Params -> ByteString
compute (Master master) params = hash $ (hash $ master) <> (hash $ toByteString' $ saltValue params)

