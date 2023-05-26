
import Data.Char

{-- This program converts a string containing an integer expression in RPN into 
an equivalent Haskell expression in prefix notation. You may assume that the input
string consists of non-negative integers, and the operators + (addition), - (subtraction),
 * (multiplication), and / (integer division, i.e. div). Moreover, the program skips 
 (abundant) spaces.--}

helper :: String -> [String] -> [String]
helper [] stack = stack
helper (x:xs) stack 
   | elem x "\n\t " = helper xs stack
   | elem x "*+-" =  helper xs ([operator ++ operands] ++ (drop 2 stack))
   | x == '/' = helper xs (["((div) " ++ operands] ++ (drop 2 stack))
   | isDigit x = helper (dropWhile isDigit xs) ([(x:takeWhile isDigit xs)] ++ stack)
   | otherwise = error "Syntax Error: invalid character in input"
   where 
      operator = "((" ++ [x] ++ ")" ++ " " 
      operands = (head (tail stack)) ++ " " ++ (head stack) ++ ")"

rpn2prefix :: String -> String
rpn2prefix str = concat $ helper str []
