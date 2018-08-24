{-# language GeneralizedNewtypeDeriving #-}
{-# language OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import Data.Bits
import Data.ByteString(ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B (c2w, w2c)
import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Conversion.To
import Data.ByteString.Conversion.From
import Data.List(find)
import Data.Word

import Debug.Trace

import Crypto.Hash.SHA512

someFunc :: IO ()
someFunc = print $ compute (Master "asdf") $ Params
  (Service "snarwalk")
  (Salt 0)
  (Iteration 0)
  (Requirements
   (LowerCount 2)
   (UpperCount 0)
   (NumberCount 4)
   (SymbolCount 4)
   (AnyCount 4)
  )

newtype Master = Master ByteString
  deriving(Eq, Show)

newtype Service = Service ByteString
  deriving(Eq, Show)

newtype Iteration = Iteration Word
  deriving(Eq, Num, Show)

newtype Salt = Salt Word64
  deriving(Eq, Show)

newtype LowerCount = LowerCount Word
  deriving(Enum, Eq, Num, Integral, Ord, Real, Show)

newtype UpperCount = UpperCount Word
  deriving(Enum, Eq, Num, Integral, Ord, Real, Show)

newtype NumberCount = NumberCount Word
  deriving(Enum, Eq, Num, Integral, Ord, Real, Show)

newtype SymbolCount = SymbolCount Word
  deriving(Enum, Eq, Num, Integral, Ord, Real, Show)

newtype AnyCount = AnyCount Word
  deriving(Enum, Eq, Num, Integral, Ord, Real, Show)

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
compute (Master master) (Params (Service service) (Salt salt) (Iteration iter) reqs) =
  let digest = hash $
        hash master <>
        hash service <>
        (hash $ toByteString' salt) <>
        (hash $ toByteString' iter)
  in passwordFromDigest reqs digest

integerFromBytes :: ByteString -> Integer
integerFromBytes = B.foldl f 0 where
  f a b = a `shiftL` 8 .|. fromIntegral b

passwordFromDigest :: Requirements -> ByteString -> ByteString
passwordFromDigest req = (passwordFromInteger req) . integerFromBytes

-- passwordFromInteger :: Requirements -> Integer -> ByteString
-- passwordFromInteger = B.Char8.pack . passwordFromInteger'

-- newtype FirstChar = FirstChar Char
newtype Count = Count Word8
newtype Mapping = Mapping (Word8 -> Char)
data Range = Range Count Mapping

-- type WeightedRanges = [(Integer, Range)]

lower = Range (Count 26) $ Mapping $ \w -> B.w2c $ B.c2w 'a' + w
upper = Range (Count 26) $ Mapping $ \w -> B.w2c $ B.c2w 'A' + w
number = Range (Count 10) $ Mapping $ \w -> B.w2c $ B.c2w '0' + w

chooseFromRange :: Range -> Integer -> (Integer, Char)
chooseFromRange (Range (Count n) (Mapping f)) i =
  ( i `div` (toInteger n)
  , f $ fromInteger $ i `mod` (toInteger n))

-- chooseFromWeightedRanges :: [(Integer, Range)] -> Integer -> (Integer, Char)
-- chooseFromWeightedRanges weightedRanges i =
  -- let 

passwordFromInteger :: Requirements -> Integer -> ByteString
passwordFromInteger reqs i =
  -- let sum = sum [toInteger lower, toInteger upper, toInteger number]
  let
    sum = toInteger (lowerCount reqs) + toInteger (upperCount reqs) + toInteger (numberCount reqs)
    index = i `mod` sum
    i' = i `div` sum
  in if sum <= 0 then ""
  else if index < toInteger (lowerCount reqs)
    then let (i'', char) = chooseFromRange lower i'
         in C8.cons char $ passwordFromInteger reqs{lowerCount = lowerCount reqs - LowerCount 1} i''
  else if index < toInteger (lowerCount reqs) + toInteger (upperCount reqs)
    then let (i'', char) = chooseFromRange upper i'
         in C8.cons char $ passwordFromInteger reqs{upperCount = upperCount reqs - UpperCount 1} i''
  else
    let (i'', char) = chooseFromRange number i'
         in C8.cons char $ passwordFromInteger reqs{numberCount = numberCount reqs - NumberCount 1} i''

  -- if lower > LowerCount 0
  -- then C8.cons
  --   (['a' .. 'z'] !! (fromInteger $ i `mod` 26))
  --   $ passwordFromInteger (Requirements (lower - LowerCount 1) upper number symbol any ) (i `div` 26)
  -- else ""

