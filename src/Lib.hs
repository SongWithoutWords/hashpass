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
import Data.Map as M
import Data.Word

import Debug.Trace

import Crypto.Hash.SHA512

someFunc :: IO ()
someFunc = B.putStrLn $ compute (Master "asdf") $ Params
  (Service "snarwalk")
  (Salt 0)
  (Iteration 3)
  (Requirements $ M.fromList
   [ (LowerCase, 0)
   , (UpperCase, 0)
   , (Number, 0)
   , (Symbol, 0)
   , (Any, 16)
   ]
  )

newtype Master = Master ByteString
  deriving(Eq, Show)

newtype Service = Service ByteString
  deriving(Eq, Show)

newtype Iteration = Iteration Word
  deriving(Eq, Num, Show)

newtype Salt = Salt Word64
  deriving(Eq, Show)

data Range
  = LowerCase
  | UpperCase
  | Number
  | Symbol
  | Any
  deriving(Enum, Eq, Ord, Show)

-- The number of characters within each range that are required
-- newtype Requirements = Requirements (TotalMap Range Word8)
newtype Requirements = Requirements (M.Map Range Word8)
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
  -- | DisplayAllParams
  | Query Service

data Result
  = UpdateFile AllParams
  | ProvidePassword AllParams

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

rangeValues :: Range -> [Char]
rangeValues r = case r of
  LowerCase -> ['a' .. 'z']
  UpperCase -> ['A' .. 'X']
  Number -> ['0' .. '9']
  Symbol -> ['!' .. '/'] ++ [':' .. '@'] ++ ['[' .. '`'] ++ ['{' .. '~']
  Any -> concatMap rangeValues [LowerCase .. Symbol]

rangeCount :: Range -> Int
rangeCount = length . rangeValues

rangeMapping :: Range -> Integer -> Char
rangeMapping r i = (rangeValues r) !! fromIntegral i

chooseFromRange :: Range -> Integer -> (Integer, Char)
chooseFromRange r i =
  ( i `div` (toInteger $ rangeCount r)
  , rangeMapping r $ i `mod` (toInteger $ rangeCount r))

chooseRange :: Map Range Word8 -> Integer -> Range
chooseRange ranges roll =
  chooseRange' (M.toList ranges) roll

chooseRange' :: [(Range, Word8)] -> Integer -> Range
chooseRange' [(range, _)] _ = range
chooseRange' ((range, weight) : rest) roll =
  if roll < toInteger weight
  then range
  else chooseRange' rest (roll - toInteger weight)

passwordFromInteger :: Requirements -> Integer -> ByteString
passwordFromInteger (Requirements rangeWeights) i =
  let
    weightSum = toInteger $ M.foldl' (+) 0 rangeWeights
    rangeChoiceIndex = i `mod` weightSum
    i' = i `div` weightSum
  in if weightSum <= 0 then ""
  else let
    range = chooseRange rangeWeights rangeChoiceIndex
    (i'', char) = chooseFromRange range i'
  in C8.cons char $ passwordFromInteger (Requirements $ M.adjust (subtract 1) range rangeWeights) i''
