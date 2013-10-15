module Distributed.Config where

import Data.ConfigFile
import Data.Either.Utils
import System.IO

-- Read a config, or return the error associated with why we couldn't read the config.
getConfig :: FilePath -> IO (Maybe ConfigParser)
getConfig filePath = do
  var <- readfile emptyCP filePath
  case var of
    Left (_, errorStr) -> do
             putStrLn errorStr
             return Nothing
    Right cp ->
        return (Just cp)
        
-- We don't care why we cant read the config. Either give it up, or fail.
getConfigOrFail :: FilePath -> IO ConfigParser
getConfigOrFail filePath = do
  config <- getConfig filePath
  case config of 
    Nothing -> error "Couldn't get config."
    Just conf -> return conf

-- Get a value from the config, or an error (usually because the value doesn't exist)
getValue :: (Get_C a) => ConfigParser -> String -> String -> Either String a
getValue cp section key = let x = get cp section key in
                          case x of
                            Left (_, errorStr) ->
                                Left errorStr
                            Right a -> Right a

-- Get a "mission-critical" value from the config, without which, the protocol can't operate.
-- Fail if we can't get the value.
configGetCrucial :: (Get_C a) => ConfigParser -> String -> String -> a
configGetCrucial cp section key = 
  let val = getValue cp section key in
  case val of
    Left err -> error $ "Failed to get value from config: " ++ err
    Right v -> v

-- Similar to getValue, only we don't care about the reason we can't get the value.
-- Just return it, or return Nothing.
configGetValue :: (Get_C a) => ConfigParser -> String -> String -> Maybe a
configGetValue cp section key = 
  let val = getValue cp section key in
  case val of
    Left err -> Nothing
    Right v -> Just v
