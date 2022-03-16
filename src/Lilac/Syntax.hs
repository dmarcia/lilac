module Lilac.Syntax where

import Text.Megaparsec (SourcePos)
import qualified Data.Text as T
import Lilac.Source

type Name = T.Text 
type XName = SourceAnn Name

type Params = [XName]
type XParams = SourceAnn Params 

data Stmt
  = SLetFun XName XParams XExpr
  | SLetVal XName XExpr 
  deriving Show

type XStmt = SourceAnn Stmt 

type Str = T.Text
type XStr = SourceAnn Str

data Expr 
  = EStr Str 
  | EVar Name 
  | EArr [XExpr]
  | EApp XExpr [XExpr]
  deriving Show 

type XExpr = SourceAnn Expr 
