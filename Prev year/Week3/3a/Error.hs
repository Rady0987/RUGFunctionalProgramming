module Error where

lexError :: Int -> Char -> a
lexError linenr ch = errorWithoutStackTrace  errmsg
  where errmsg = "Lexical error in line " ++ show linenr ++ ": unexpected character '" ++ [ch] ++ "'."

expectedError :: Int -> String -> a
expectedError linenr expmsg = errorWithoutStackTrace  errmsg
  where errmsg = show linenr ++ ":Syntax error, expected " ++ expmsg ++ "."

eofError :: a
eofError = errorWithoutStackTrace "Syntax error: Unexpected end of file."