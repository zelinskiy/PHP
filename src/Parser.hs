module PHP.Parser where

--stolen from https://github.com/jhartikainen/hs-language-php

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

data PHPVal =
    PHPString String
  | PHPInt Int
  | PHPBool Bool
  | PHPNull
  deriving (Eq, Show)

data PHPVar =
    PHPVar String
  | PHPVarVar String
  deriving (Eq, Show)

data PHPFuncCall =
    PHPFuncCall String
  | PHPFuncCallVar PHPVar
  deriving (Eq, Show)

data PHPFuncArg =
  PHPFuncArg String
  deriving (Eq, Show)

data PHPExpr =
    LiteralE PHPVal
  | VariableE PHPVar
  | AssignE PHPVar PHPExpr
  | NegE PHPExpr
  | BinaryE PHPBinOp PHPExpr PHPExpr
  | UnaryE PHPUnOp PHPVar
  | CallE PHPFuncCall [PHPExpr]
  | PrintE PHPExpr
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
  | PHPEq
  | PHPEqq
  deriving (Eq, Show)

data PHPElseExpr =
    PHPElse PHPStmt
  | PHPElif PHPExpr PHPStmt (Maybe PHPElseExpr)
  deriving (Eq, Show)

data PHPStaticVar = PHPStaticVar String (Maybe PHPVal) deriving (Eq, Show)


data PHPStmt =
    SeqS [PHPStmt]
  | ExpressionS PHPExpr
  | IfS PHPExpr PHPStmt (Maybe PHPElseExpr)
  | FunctionS String [PHPFuncArg] PHPStmt
  | ReturnS PHPExpr
  | WhileS PHPExpr PHPStmt
  | ForS [PHPExpr] [PHPExpr] [PHPExpr] PHPStmt
  | EchoS [PHPExpr]
  | GlobalS PHPVar
  | StaticVarS [PHPStaticVar]
  deriving (Eq, Show)
                  
