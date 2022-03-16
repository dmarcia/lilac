module Lilac.Parser.Obj where

import qualified Data.Text as T
import Lilac.Source

-- | Kind of list.
data ListKind
  = ParenList -- ^ `(x y z)`
  | BrackList -- ^ `[x y z]`
  | CurlyList -- ^ `{x y z}` 
  deriving (Show, Eq)

-- | Canonical Lisp object.
data Obj 
  = OStr T.Text
  | OName T.Text 
  | OList ListKind [XObj]
  deriving Show 

-- | Obj with a 'SourcePos' attached.
type XObj = SourceAnn Obj 
