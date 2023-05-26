module Lexer(LexToken(..),lexer) where
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

lexer :: String -> [(LexToken,Int)]    -- The Int is the line number in the source code
lexer s = lex s 1
  where
    lex [] _ = []
    lex ('\n':cs) linenr     = lex cs (linenr+1)                      -- skip new lines
    lex (' ':cs) linenr      = lex cs linenr                          -- skip spaces
    lex ('\t':cs) linenr     = lex cs linenr                          -- skip tabs
    lex ('%':cs) linenr      = lex (dropWhile (/= '\n') cs) linenr    -- skip comment line
    lex ('.':cs) linenr      = (DotTok,linenr):lex cs linenr          
    lex (',':cs) linenr      = (CommaTok,linenr):lex cs linenr          
    lex (':':'-':cs) linenr  = (FollowsTok,linenr):lex cs linenr
    lex ('?':'-':cs) linenr  = (QueryTok,linenr):lex cs linenr
    lex ('(':cs) linenr      = (LparTok,linenr):lex cs linenr
    lex (')':cs) linenr      = (RparTok,linenr):lex cs linenr
    lex (c:cs) linenr
      | elem c ['a'..'z']    = (IdentTok (c:takeWhile isAlphaNum cs),linenr):lex(dropWhile isAlphaNum cs) linenr
      | elem c ['A'..'Z']    = (VarTok (c:takeWhile isAlphaNum cs),linenr):lex(dropWhile isAlphaNum cs) linenr
      | otherwise            = lexError linenr c