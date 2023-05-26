import Data.Char

type Name = String

data Expr = Val Integer
  | Var Name
  | Expr :+: Expr
  | Expr :-: Expr
  | Expr :/: Expr
  | Expr :%: Expr
  | Expr :*: Expr


instance Show Expr where
  show (Val a) = show a
  show (Var x) = x
  show (p :%: q) = par(show p ++ "%" ++ show q)
  show (p :+: q) = par(show p ++ "+" ++ show q)
  show (p :-: q) = par(show p ++ "-" ++ show q)
  show (p :*: q) = par(show p ++ "*" ++ show q)
  show (p :/: q) = par(show p ++ "/" ++ show q)

par:: String -> String
par s = "(" ++ s ++ ")"

{--This function returns a list with the input tokenized--}
lexer :: String -> [String]
lexer [] = []
lexer (c:cs)
  | elem c " \t\n" = lexer cs
  | elem c "-+/*%()" = [c]:(lexer cs)
  | isAlpha c = (c:takeWhile isAlpha cs):lexer(dropWhile isAlpha cs)
  | isDigit c = (c:takeWhile isDigit cs):lexer(dropWhile isDigit cs)
  | otherwise = error "Syntax error: invalid character in input"

{--This function creates and expresion from a given string--}
toExpr:: String -> Expr
toExpr s = acc 
  where (acc,tokens) = parse (lexer s)

{--This function joins the results of both parsing functions--}
parse :: [String] -> (Expr,[String])
parse tokens = parseF acc rest 
  where (acc, rest) = parseS (parseAccepted tokens) (parseRest tokens)

{--This function parses the expressions with the following tokens: '-', '+'.--}
parseF :: Expr -> [String] -> (Expr,[String])
parseF str ("+":tokens) = parseF (str :+: acc) rest
  where (acc,rest) = parseS (parseAccepted tokens) (parseRest tokens)
parseF str ("-":tokens) = parseF (str :-: acc) rest
  where (acc,rest) = parseS (parseAccepted tokens) (parseRest tokens)
parseF str tokens = (str,tokens)

{--This function parses the expressions with the following tokens: '*', '/', '%' .--}
parseS :: Expr -> [String] -> (Expr,[String])
parseS str ("*":tokens) = parseS (str :*: acc) rest
  where (acc,rest) = parseVarInt tokens 
parseS str ("/":tokens) = parseS (str :/: acc) rest
  where (acc,rest) = parseVarInt tokens 
parseS str ("%":tokens) = parseS (str :%: acc) rest
  where (acc,rest) = parseVarInt tokens
parseS str tokens = (str,tokens)

{--This function parses the variables and integers of the expression--}
parseVarInt :: [String] -> (Expr,[String])
parseVarInt ("(":tokens) = (x, tail rest) 
  where (x,rest) = parse tokens
parseVarInt (tok:tokens)
  | isAlpha (head tok) = (Var tok,tokens)
  | isDigit (head tok) = (Val (read tok),tokens)
  | otherwise = error "Syntax error"

{--This function returns the rest string of the expresion--}
parseRest :: [String] -> [String]
parseRest tokens = rest 
 where (acc,rest) = parseVarInt tokens

{--This function returns the already parsed part of the expression--}
parseAccepted :: [String] -> Expr
parseAccepted tokens = acc 
 where (acc,rest) = parseVarInt tokens

