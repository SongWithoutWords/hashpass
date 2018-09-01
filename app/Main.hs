{-# language ApplicativeDo #-}
{-# language TupleSections #-}

module Main where

import qualified Data.Map as M
import Data.Semigroup ((<>))
import Data.Word

-- import Data.Text.IO
import System.Directory
-- import Text.Read
-- import Text.Show

import Options.Applicative

import Lib

data Options
  = Add Service Requirements
  | Increment Service
  -- | DisplayConfig
  | Query Service
  deriving(Eq, Show)

main :: IO ()
main = do
  opts <- parseOptions
  print opts
  case opts of
    Query service -> do
      config <- readConfig
      master <- readMaster
      print $ query master service config
    Increment service ->
      transformConfig $ increment service
    Add service requirements -> do
      recipe <- newRecipe requirements
      transformConfig $ add service recipe

-- TODO: Consider using getAppConfigDirectory
configDirectory :: IO FilePath
configDirectory = (++ "/.config/hashpass") <$> getHomeDirectory

configPath :: IO FilePath
configPath = (++ "/config.hs") <$> configDirectory

readMaster :: IO Master
readMaster = undefined

readConfig :: IO Config
readConfig = do
  path <- configPath
  configExists <- doesFileExist path
  if configExists
    then read <$> readFile path
    else pure defaultConfig

writeConfig :: Config -> IO ()
writeConfig params = do
  configDirectory >>= (createDirectoryIfMissing True)
  path <- configPath
  writeFile path (show params)

transformConfig :: (Config -> Config) -> IO ()
transformConfig f = (f <$> readConfig) >>= writeConfig

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
  hsubparser $ command "add" (info addInfo (progDesc "asdf"))

addInfo :: Parser Options
addInfo = Add <$>
  (Service <$> argument str (metavar "SERVICE" <> help "The name of the service for which the password is generated")) <*>
  (Requirements . M.fromList <$> sequenceA
    [ characterCountOption LowerCase "lower" 'l' "LOWER_COUNT" "lowercase letters"
    , characterCountOption UpperCase "upper" 'u' "UPPER_COUNT" "uppercase letters"
    , characterCountOption Number "number" 'n' "NUMBER_COUNT" "numeric characters"
    , characterCountOption Symbol "symbol" 's' "SYMBOL_COUNT" "symbolic characters"
    , characterCountOption Any "any" 'a' "ANY_COUNT" "lowercase, uppercase, numeric, or symbolic characters"
    ]
  )
  where
    characterCountOption :: Range -> String -> Char -> String -> String -> Parser (Range, Word64)
    characterCountOption range longName shortName meta helpDesc =
      (range,) <$> option auto
        (  long longName
        <> short shortName
        <> metavar meta
        <> help ("The number of " ++ helpDesc ++ " in the generated password")
        <> value 0
        )
