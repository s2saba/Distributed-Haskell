module Distributed.Config where

import Data.ConfigFile
import Data.Either.Utils
import System.IO

getConfig :: FilePath -> IO (Maybe ConfigParser)
getConfig filePath = do
  var <- readfile emptyCP filePath
  case var of
    Left (_, errorStr) -> do
             putStrLn errorStr
             return Nothing
    Right cp ->
        return (Just cp)

getConfigOrFail :: FilePath -> IO (ConfigParser)
getConfigOrFail filePath = do
  config <- getConfig filePath
  case config of 
    Nothing -> error "Couldn't get config."
    Just conf -> return conf

getValue :: (Get_C a) => ConfigParser -> String -> String -> Either String a
getValue cp section key = let x = get cp section key in
                          case x of
                            Left (_, errorStr) ->
                                Left errorStr
                            Right a -> Right a
                            
configGetCrucial :: (Get_C a) => ConfigParser -> String -> String -> a
configGetCrucial cp section key = 
  let val = (getValue cp section key) in
  case val of
    Left err -> error $ "Failed to get value from config: " ++ err
    Right v -> v

configGetValue :: (Get_C a) => ConfigParser -> String -> String -> Maybe a
configGetValue cp section key = 
  let val = (getValue cp section key) in
  case val of
    Left err -> Nothing
    Right v -> Just v
