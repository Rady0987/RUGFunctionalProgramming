module Lexer where
import Data.Char
import Error

data LexToken = DotTok   | CommaTok | FollowsTok
              | QueryTok | LparTok  | RparTok
              | IdentTok String | VarTok String
  deriving Eq

instance Show LexToken where
  show DotTok            = "."
  show CommaTok          = ","
  show FollowsTok        = ":-"
  show QueryTok          = "?-"
  show LparTok           = "("
  show RparTok           = ")"
  show (IdentTok name)   = "<id:" ++ name ++ ">"
  show (VarTok name)     = "<var:" ++ name ++ ">"

{-- This function calls the lexerHelper function with 
the input string and line nr 1 --}
lexer :: String -> [(LexToken,Int)]    
lexer input = lexerHelper input 1

{-- This function returns a coresponding token for an input char --}
tokenFind :: Char -> LexToken
tokenFind xs
  | xs == '.' = DotTok
  | xs == ',' = CommaTok
  | xs == '(' = LparTok
  | xs == ')' = RparTok
  | xs == '?' = QueryTok
  | xs == ':' = FollowsTok

{-- This function creates tuples of the line number and the token, 
by checking every character of the string --}
lexerHelper :: String -> Int -> [(LexToken, Int)]
lexerHelper [] _ = []
lexerHelper (x:xs) lineNumber
  | x == '\n'     = lexerHelper xs (lineNumber + 1)
  | elem x " \t"  = lexerHelper xs lineNumber
  | x == '%'      = lexerHelper (dropWhile (/= '\n') xs) lineNumber
  | elem x "?:" && (head xs) == '-' = (tokenFind x, lineNumber) : lexerHelper (tail xs) lineNumber
  | elem x ",.()" = (tokenFind x, lineNumber) : lexerHelper xs lineNumber
  | isAlpha x     = (identOrVar x (takeWhile digitOrAlpha(x : xs)), lineNumber) : lexerHelper (dropWhile digitOrAlpha xs) lineNumber
  | otherwise     = lexError lineNumber x
    where 
      identOrVar x
        | isUpper x = VarTok
        | otherwise = IdentTok
      digitOrAlpha x = isAlpha x || isDigit x

      
