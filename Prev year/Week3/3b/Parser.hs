module Parser(parseProgram) where

import Types
import Lexer
import Error

parseProgram :: String -> Program
parseProgram input = Program (parseProlog [] (lexer input))

parseProlog :: [(Statement,Int)] -> [(LexToken,Int)] -> [(Statement,Int)]
parseProlog accepted []     = reverse accepted
parseProlog accepted tokens = parseProlog ((statement,snd(head tokens)):accepted) rest
  where (statement,rest) = parseStatement tokens

acceptDot :: [(LexToken,Int)] -> [(LexToken,Int)]
acceptDot ((DotTok,_):tokens)  = tokens
acceptDot ((_,linenr):tokens)  = expectedError linenr "a '.'"
acceptDot []                   = eofError


parseStatement :: [(LexToken,Int)] -> (Statement,[(LexToken,Int)])
parseStatement ((QueryTok,_):tokens) = (Query fa,acceptDot rest)
  where (fa,rest) = parseRelation tokens
parseStatement tokens = parseStatement' fa rest
  where (fa,rest) = parseRelation tokens

parseStatement' :: FuncApplication -> [(LexToken,Int)] -> (Statement,[(LexToken,Int)])
parseStatement' fa ((DotTok,_):tokens) = (Fact fa,tokens)
parseStatement' fa ((FollowsTok,_):tokens) = (Rule fa args,acceptDot rest)
  where
    (args,rest) = parseRelationList tokens

parseStatement' _ ((_,linenr):tokens)      = expectedError linenr "'.' or ':-'"
parseStatement' _ []                       = eofError


parseRelationList :: [(LexToken,Int)] -> ([FuncApplication],[(LexToken,Int)])
parseRelationList tokens = parseRelationList' [fa] rest
  where (fa,rest) = parseRelation tokens

parseRelationList' :: [FuncApplication] -> [(LexToken,Int)] -> ([FuncApplication],[(LexToken,Int)])
parseRelationList' fas ((CommaTok,_):tokens) = parseRelationList' (fa:fas) rest
  where (fa,rest) = parseRelation tokens
parseRelationList' fas tokens = (reverse fas, tokens)



parseRelation :: [(LexToken,Int)] -> (FuncApplication,[(LexToken,Int)])
parseRelation ((IdentTok name,_):tokens) = (FuncApp name args,rest)
  where (args,rest) = parseArgs tokens
parseRelation ((_,linenr):tokens)        = expectedError linenr "a relation name (i.e. an identifier that start with a lower case letter)"
parseRelation []                         = eofError


parseArgs  :: [(LexToken,Int)] -> ([Argument],[(LexToken,Int)])
parseArgs ((LparTok,_):tokens) = (args, acceptRpar rest)
  where
    (args,rest) = parseArgList tokens
    acceptRpar ((RparTok,_):tokens) = tokens
    acceptRpar ((_,linenr):tokens)  = expectedError linenr "a closing parenthesis"
    acceptRpar []                   = eofError
parseArgs ((_,linenr):tokens)       = expectedError linenr "an opening parenthesis"
parseArgs []                        = eofError

parseArgList :: [(LexToken,Int)] -> ([Argument],[(LexToken,Int)])
parseArgList tokens = parseArgList' [arg] rest
  where (arg,rest) = parseArgument tokens

parseArgList' :: [Argument] -> [(LexToken,Int)] -> ([Argument],[(LexToken,Int)])
parseArgList' args ((CommaTok,_):tokens) = parseArgList' (arg:args) rest
  where (arg,rest) = parseArgument tokens
parseArgList' args tokens = (reverse args, tokens)

parseArgument :: [(LexToken,Int)] -> (Argument,[(LexToken,Int)])
parseArgument ((VarTok name,_):tokens)   = (Arg name,tokens)
parseArgument ((IdentTok name,_):tokens) = (Const name,tokens)
parseArgument ((_,linenr):tokens)        = expectedError linenr "a literal"
parseArgument []                         = eofError
