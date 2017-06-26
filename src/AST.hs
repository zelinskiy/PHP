module AST where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

data PHPVal =
    PHPString String
  | PHPInt Integer
  | PHPFloat Double
  | PHPBool Bool
  | PHPNull
  deriving (Eq, Show)

data PHPVar =
    PHPVar String
  | PHPVarVar String
  deriving (Eq, Show)


data PHPExpr =
    LiteralE PHPVal
  | VariableE PHPVar
  | AssignE PHPVar PHPExpr
  | NegE PHPExpr
  | NotE PHPExpr
  | BinaryE PHPBinOp PHPExpr PHPExpr
  deriving (Eq, Show)

data PHPUnOp = PHPInc | PHPDec
  deriving (Eq, Show)

data PHPBinOp =
    PHPAdd
  | PHPSub
  | PHPMul
  | PHPDiv
  | PHPMod
  | PHPAnd
  | PHPOr
  | PHPGt
  | PHPLt
  deriving (Eq, Show)

data PHPStmt =
    SeqS [PHPStmt]
  | ExpressionS PHPExpr
  | IfS PHPExpr PHPStmt PHPStmt
  | WhileS PHPExpr PHPStmt
  | EchoS [PHPExpr]
  deriving (Eq, Show)
                  
