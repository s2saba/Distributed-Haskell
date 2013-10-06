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

getValue :: (Get_C a) => ConfigParser -> String -> String -> Either String a
getValue cp section key = let x = get cp section key in
                          case x of
                            Left (_, errorStr) ->
                                Left errorStr
                            Right a -> Right a
                            
