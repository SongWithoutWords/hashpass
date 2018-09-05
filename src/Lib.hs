{-# language GeneralizedNewtypeDeriving #-}
{-# language OverloadedStrings #-}

module Lib where

import Data.Bits
import qualified Data.ByteArray as A
import Data.ByteString(ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B (c2w, w2c)
import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Conversion.To
import Data.ByteString.Conversion.From
import Data.Map as M
import Data.Word

import Math.NumberTheory.Logarithms

import Crypto.Error
import Crypto.Hash(Digest, hash)
import Crypto.Hash.Algorithms(SHA3_512)
import qualified Crypto.KDF.Argon2 as Argon2
import Crypto.Random(getRandomBytes)

newtype Master = Master ByteString
  deriving(Eq, Show)

newtype Service = Service ByteString
  deriving(Eq, Ord, Read, Show)

newtype Version = Version Word64
  deriving(Eq, Num, Read, Show)

newtype Salt = Salt Word64
  deriving(Eq, Read, Show)

data Range
  = LowerCase
  | UpperCase
  | Number
  | Symbol
  | Any
  deriving(Enum, Eq, Ord, Read, Show)

-- The number of characters within each range that are required
newtype Requirements = Requirements (M.Map Range Word64)
  deriving(Eq, Read, Show)

newtype Disallowed = Disallowed [Char]
  deriving(Eq, Read, Show)

data Recipe = Recipe
  { salt :: Salt
  , version :: Version
  , requirements :: Requirements
  , disallowed :: Disallowed
  }
  deriving(Eq, Read, Show)

newRecipe :: Requirements -> Disallowed -> IO Recipe
newRecipe reqs dis = do
  salt <- integralFromBytes <$> getRandomBytes 8
  pure $ Recipe (Salt salt) 0 reqs dis

newtype Config = Config (Map Service Recipe)
  deriving(Eq, Read, Show)

defaultConfig :: Config
defaultConfig = Config M.empty

data Result
  = UpdateFile Config
  | ProvidePassword Config

add :: Service -> Recipe -> Config -> Config
add service recipe (Config allRecipe) = Config $
  alter addIfNotFound service allRecipe
  where
    addIfNotFound Nothing = Just recipe
    addIfNotFound (Just existing) = Just existing

increment :: Service -> Config -> Config
increment s (Config paramList) = Config $
  adjust (\p -> p {version = (version p) + 1}) s paramList

factorial :: Integer -> Integer
factorial n = if n <= 1 then 1 else n * factorial (n - 1)

numBytesRequired :: Requirements -> Int
numBytesRequired (Requirements reqs) = (`div` 8) $ integerLog2 $
  (factorial $ toInteger $ sum reqs) *
  (product $ M.mapWithKey (\range count -> (toInteger $ rangeCount range) ^ count) reqs)

data QueryFailure
  = ServiceNotFound Service
  | CryptoFailure CryptoError
  deriving(Eq, Show)

query :: Master -> Service -> Config -> Either QueryFailure ByteString
query m s (Config paramList) =
  (case M.lookup s paramList of
    Just recipe -> Right recipe
    Nothing -> Left $ ServiceNotFound s) >>= compute m s

hashBytes :: ByteString -> ByteString
hashBytes bs = A.convert ((hash bs) :: Digest SHA3_512)

hashNum :: Word64 -> ByteString
hashNum = hashBytes . toByteString'

computeHash :: Master -> Service -> Recipe -> Either QueryFailure ByteString
computeHash (Master master) (Service service) (Recipe (Salt salt) (Version ver) reqs dis) =
  let
    combinedPwHash = hashBytes service <> hashNum ver <> hashBytes master
    saltHash = hashNum salt
    outputLength = numBytesRequired reqs
  in
    case Argon2.hash Argon2.defaultOptions combinedPwHash saltHash outputLength of
      CryptoPassed a -> Right $ a
      CryptoFailed e -> Left $ CryptoFailure e

compute :: Master -> Service -> Recipe -> Either QueryFailure ByteString
compute master service recipe =
  (passwordFromBytes (requirements recipe)) <$> (computeHash master service recipe)

integralFromBytes :: (Integral a, Bits a) => ByteString -> a
integralFromBytes = B.foldl f 0 where
  f x y = x `shiftL` 8 .|. fromIntegral y

passwordFromBytes :: Requirements -> ByteString -> ByteString
passwordFromBytes req = (passwordFromInteger req) . integralFromBytes

-- TODO: Add set of excluded symbols
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

chooseRange :: Map Range Word64 -> Integer -> Range
chooseRange ranges roll =
  chooseRange' (M.toList ranges) roll

chooseRange' :: [(Range, Word64)] -> Integer -> Range
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
