module Lilac.Parser.Phase1 where

import Control.Monad
import Data.Void
import Data.Composition
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Lilac.Source
import Lilac.Parser.Error
import Lilac.Parser.Obj

type Phase1 = Parsec Void T.Text

lexeme ∷ Phase1 a → Phase1 a
lexeme = L.lexeme space

lstring ∷ T.Text → Phase1 T.Text
lstring = lexeme . string

name ∷ Phase1 Obj
name = fmap OName $ T.pack .: (:) <$> head <*> many tail where
  head = letterChar
  tail = head <|> numberChar 

str ∷ Phase1 Obj
str = do 
  char '"'
  xs ← manyTill anySingle $ char '"'
  pure . OStr $ T.pack xs 

list ∷ Phase1 Obj
list = choice $ f <$> kinds where
  kinds = 
    [ ("(", ")", ParenList)
    , ("[", "]", BrackList)
    , ("{", "}", CurlyList)
    ]
  f (l, r, k) = OList k <$> between (lstring l) (string r) (many xobj) 

xobj ∷ Phase1 XObj
xobj = liftM2 MkSourceAnn getSourcePos $ lexeme obj where
  obj = choice [str, name, list]

-- | First pass of the parser; attempts to parse any objects.
phase1 ∷ Source → Either Phase1Error [XObj] 
phase1 (MkSource path content) = runParser (many xobj) path content
