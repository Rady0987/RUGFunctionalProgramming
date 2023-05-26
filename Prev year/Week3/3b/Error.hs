module Error where
import System.Exit
import System.IO.Unsafe
import Debug.Trace

echo = flip trace

abortWithMessage :: String -> IO a
abortWithMessage msg = do
  putStrLn msg
  exitSuccess

myError :: String -> a
myError msg = unsafePerformIO (abortWithMessage msg)
  
lexError :: Int -> Char -> a
lexError linenr ch = myError errmsg
  where errmsg = "Lexical error in line " ++ show linenr ++ ": unexpected character '" ++ [ch] ++ "'."

expectedError :: Int -> String -> a
expectedError linenr expmsg = myError errmsg
  where errmsg = show linenr ++ ":Syntax error, expected " ++ expmsg ++ "."

eofError :: a
eofError = myError "Syntax error: Unexpected end of file."