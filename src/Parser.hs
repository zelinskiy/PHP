module Parser where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import AST

langDef = emptyDef { Token.commentStart = "/*"
                   , Token.commentEnd = "*/"
                   , Token.commentLine = "//"
                   , Token.identStart = letter <|> char '$'
                   , Token.identLetter = alphaNum
                   , Token.reservedNames = [ "if", "else", "elseif", "while", "break", "do", "for", "continue"
                                           , "true", "false", "null", "and", "or", "class", "function", "return"
                                           ]
                   , Token.reservedOpNames = [ "=", "+", "-", "*", "/", "%", "<", ">", "and", "or", "||", "&&", "!" ]
                   }

lexer = Token.makeTokenParser langDef

identifier = Token.identifier lexer
reserved = Token.reserved lexer
float = Token.float lexer
stringTok = Token.stringLiteral lexer
reservedOp = Token.reservedOp lexer
parens = Token.parens lexer
braces = Token.braces lexer
integer = Token.integer lexer
semi = Token.semi lexer
whiteSpace = Token.whiteSpace lexer

whileParser :: Parser PHPStmt
whileParser = whiteSpace >> statement

statement :: Parser PHPStmt
statement = parens statement <|> sequenceOfStmt

sequenceOfStmt = do
    list <- (sepBy1 statement' semi)
    return $ if length list == 1 then head list else SeqS list

statement' :: Parser PHPStmt
statement' =
  whileStmt <|>
  echoStmt <|>
  ifStmt <|>
  liftM ExpressionS phpExpression

ifStmt :: Parser PHPStmt
ifStmt = do
    reserved "if"
    cond <- parens phpExpression
    stmt1 <- braces statement
    reserved "else"
    stmt2 <- braces statement
    return $ IfS cond stmt1 stmt2

echoStmt :: Parser PHPStmt
echoStmt = do
  reserved "echo"
  what <- phpExpression
  return $ EchoS [what]

whileStmt :: Parser PHPStmt
whileStmt = do
    reserved "while"
    cond <- parens phpExpression
    stmt <- braces statement
    return $ WhileS cond stmt

assignExpr :: Parser PHPExpr
assignExpr = do
    var <- varName
    reservedOp "="
    expr <- phpExpression
    return $ AssignE (PHPVar var) expr

varName :: Parser String
varName = do
    dollar <- char '$'
    name <- identifier
    return $ dollar : name

variableExpr :: Parser PHPVar
variableExpr = do
    name <- varName
    return $ PHPVar name

phpExpression :: Parser PHPExpr
phpExpression = buildExpressionParser phpOperators phpTerm

phpOperators = [ [Prefix (reservedOp "-" >> return (NegE))]
               , [Infix (reservedOp "*" >> return (BinaryE PHPMul)) AssocLeft]
               , [Infix (reservedOp "/" >> return (BinaryE PHPDiv)) AssocLeft]
               , [Infix (reservedOp "+" >> return (BinaryE PHPAdd)) AssocLeft]
               , [Infix (reservedOp "-" >> return (BinaryE PHPSub)) AssocLeft]
               , [Prefix (reservedOp "!" >> return (NotE))]
               , [Infix (reservedOp "&&" >> return (BinaryE PHPAnd)) AssocLeft]
               , [Infix (reservedOp "||" >> return (BinaryE PHPOr)) AssocLeft]
               , [Infix (reservedOp "<" >> return (BinaryE PHPLt)) AssocLeft]
               , [Infix (reservedOp ">" >> return (BinaryE PHPGt)) AssocLeft]
               ]

phpTerm = parens phpExpression
       <|> try assignExpr
       <|> liftM VariableE variableExpr
       <|> liftM LiteralE phpValue

phpValue :: Parser PHPVal
phpValue = (reserved "true" >> return (PHPBool True))
        <|> (reserved "false" >> return (PHPBool False))
        <|> (reserved "null" >> return PHPNull)
        <|> (Token.naturalOrFloat lexer >>= return . either PHPInt PHPFloat)
        <|> (stringTok >>= return . PHPString)

parseString :: String -> PHPStmt
parseString str = case parse whileParser "" str of
                    Left e -> error $ show e
                    Right r -> r
