module Main where

import Data.Semigroup ((<>))
import Data.Text.IO
import System.Directory
import Text.Read

import Options.Applicative

import Lib

data Options
  = Add Service Params
  | Increment Service
  -- | DisplayAllParams
  | Query Service
  deriving(Eq, Show)

main :: IO ()
main = do
  opts <- parseOptions
  print opts
  case opts of
    Query service -> do
      allParams <- readParams
      master <- readMaster
      print $ query master service allParams
    Increment service -> do
      transformParams $ increment service

-- TODO: Consider using getAppConfigDirectory
filePath :: IO FilePath
filePath = (++ "/.config/hashpass/config.hs") <$> getHomeDirectory

readMaster :: IO Master
readMaster = undefined

readParams :: IO AllParams
readParams = do
  path <- filePath
  read <$> readFile path

writeParams :: AllParams -> IO ()
writeParams params = do
  path <- filePath
  writeFile path (show params)

transformParams :: (AllParams -> AllParams) -> IO ()
transformParams f = (f <$> readParams) >>= writeParams

parseOptions :: IO Options
parseOptions = execParser options

options :: ParserInfo Options
options = info (parser <**> helper) (fullDesc)

parser :: Parser Options
parser = parseAdd <|> parseIncrement <|> parseQuery -- <|> parseAdd

parseQuery :: Parser Options
parseQuery = (Query . Service) <$> argument str (metavar "SERVICE")

parseIncrement :: Parser Options
parseIncrement = (Increment . Service) <$>
  strOption (
    long "increment" <>
    short 'i' <>
    metavar "SERVICE" <>
    help "Name of the service to be incremented")

parseAdd :: Parser Options
parseAdd = -- Add <$>
  hsubparser $ command "--add" (info asdf (progDesc "asdf"))

asdf :: Parser Options
asdf = undefined --Service <$> strOption (long "service" <> metavar "SERVICE")
  -- (Service <$> strOption (
  --   long "add" <>
  --   short 'a' <>
  --   metavar "SERVICE" <>
  --   help "Name of the service to be incremented"))
  -- undefined
