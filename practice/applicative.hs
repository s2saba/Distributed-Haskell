import Control.Applicative

main = do
  (++) <$> getLine <*> getLine
  return 
