import Data.Char

lexer :: String -> [String]
lexer [] = []
lexer (c:cs)
   | elem c "\n\t " = lexer cs
   | elem c "*/+-()" = [c]:(lexer cs)   
   | isAlpha c = (c:takeWhile isAlpha cs): lexer(dropWhile isAlpha cs)
   | isDigit c = (c:takeWhile isDigit cs): lexer(dropWhile isDigit cs)
   | otherwise = error "Syntax Error: invalid character in input"

parser :: String -> (Bool, String, [String]) -- + Bool,
parser str = parseE "" (lexer str) 

parseE :: String -> [String] -> Integer -> (Bool, String, [String]) -- + Bool,
parseE accepted tokens p
   | valid = parseE' acc rest valid 
   | otherwise = (False, acc, rest)
   where (valid, acc, rest) = parseT accepted tokens p

parseE' :: String -> [String] -> Bool -> (Bool, String, [String]) -- + Bool,
parseE' accepted tokens False  = (False, accepted, tokens)
parseE' accepted ("+":tokens) val  = parseE' acc rest valid
   where (valid, acc, rest) = parseT (accepted ++ "+") tokens 
parseE' accepted ("-":tokens) val  = parseE' acc rest valid
   where (valid, acc, rest) = parseT (accepted ++ "-") tokens 
parseE' accepted tokens val  = (val, accepted, tokens) --TRUE !!!!!

parseT :: String -> [String] -> Integer -> (Bool, String, [String])
parseT accepted tokens p
   | valid = parseT' acc rest valid p
   | otherwise = (False, acc, rest)
   where (valid, acc, rest, p) = parseF accepted tokens

parseT' :: String -> [String] -> Bool -> Integer -> (Bool, String, [String]) -- + Bool,
parseT' accepted tokens False p = (False, accepted, tokens)
parseT' accepted ("*":tokens) val p = parseT' acc rest valid p
   where (valid, acc, rest) = parseT (accepted ++ "*") tokens
parseT' accepted ("/":tokens) val p = parseT' acc rest valid p
   where (valid, acc, rest) = parseT (accepted ++ "/") tokens p
parseT' accepted (")":tokens) val 1 = parseT' (accepted ++ ")") tokens val 1
parseT' accepted tokens val p = (val, accepted, tokens) --TRUE !!!!!

parseF :: String -> [String]-> (Bool, String, [String], Integer) -- + Bool,
parseF accepted []  = (False, accepted, [])
parseF accepted (tok:tokens) 
   | isAlpha (head tok) = (True, accepted ++ tok, tokens)
   | isDigit (head tok) = (True, accepted ++ tok, tokens)
   | (head tok) == '(' = parseE (accepted ++ tok) tokens 1
   -- | (head tok) == ')' = (False, accepted, (tok:tokens))
   | otherwise = (False, accepted, (tok:tokens), 0)

-- parser :: String -> (String, [String]) -- + Bool,
-- parser str = parseE "" (lexer str)

-- parseE :: String -> [String] -> (String, [String]) -- + Bool,
-- parseE accepted tokens = parseE' acc rest 
--    where (acc, rest) = parseT accepted tokens

-- parseE' :: String -> [String] -> (String, [String]) -- + Bool,
-- parseE' accepted ("+":tokens) = parseE' acc rest
--    where (acc, rest) = parseT (accepted ++ "+") tokens
-- parseE' accepted ("-":tokens) = parseE' acc rest
--    where (acc, rest) = parseT (accepted ++ "-") tokens
-- parseE' accepted tokens = (accepted, tokens) --TRUE !!!!!

-- parseT :: String -> [String] -> (String, [String])
-- parseT accepted tokens = parseT' acc rest 
--    where (acc, rest) = parseF accepted tokens

-- parseT' :: String -> [String] -> (String, [String]) -- + Bool,
-- parseT' accepted ("*":tokens) = parseT' acc rest
--    where (acc, rest) = parseT (accepted ++ "*") tokens
-- parseT' accepted ("/":tokens) = parseT' acc rest
--    where (acc, rest) = parseT (accepted ++ "/") tokens
-- parseT' accepted (")":tokens) = parseT' (accepted ++ ")") tokens 
-- parseT' accepted tokens = (accepted, tokens) --TRUE !!!!!

-- parseF :: String -> [String] -> (String, [String]) -- + Bool,
-- parseF accepted [] = error "Parse error...abort"
-- parseF accepted (tok:tokens)
--    | isAlpha (head tok) = (accepted ++ tok, tokens)
--    | isDigit (head tok) = (accepted ++ tok, tokens)
--    | (head tok) == '(' = parseE (accepted ++ tok) tokens
--    -- | (head tok) == ')' = (accepted ++ tok, tokens)
--    | otherwise = error ("Syntax Error: " ++ tok)

