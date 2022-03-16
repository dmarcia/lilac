module Lilac.Parser.Phase2 where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Lazy
import Control.Applicative.Combinators
import Data.List.NonEmpty
import qualified Data.Text as T
import Lilac.Utility
import Lilac.Source
import Lilac.Syntax
import Lilac.Parser.Error
import Lilac.Parser.Obj

type Phase2 = ExceptT Phase2Error (Reader XObj)

runPhase2 ∷ Phase2 a → XObj → Either Phase2Error a
runPhase2 p x = runReader (runExceptT p) x 

errorHere ∷ T.Text → Phase2 a
errorHere msg = do
  pos ← asks annPos 
  throwError $ MkPhase2Error pos msg

expected ∷ T.Text → Phase2 a
expected = errorHere . ("Expected " <>)

match ∷ (Obj → Phase2 a) → Phase2 a
match f = asks ann >>= f 

srcAnn ∷ Phase2 a → Phase2 (SourceAnn a)
srcAnn p = MkSourceAnn <$> asks annPos <*> p 

-- | Abstraction for procedurally consuming objects within a list.
type Chomp = StateT [XObj] Phase2

runChomp ∷ Chomp a → [XObj] → Phase2 a
runChomp = evalStateT

chomp ∷ Phase2 a → Chomp a
chomp p = get >>= \case
  x:xs → do 
    put xs
    liftEither $ runPhase2 p x
  [] → lift $ expected "object"

chomps ∷ Phase2 a → Chomp [a]
chomps p = untilM (chomp p) (gets null) 

matchList ∷ ListKind → Chomp a → Phase2 a
matchList kind p = match $ \case
  (OList kind' xs) | kind == kind' → runChomp p xs
  _ → expected $ "list of kind: " <> tshow kind

keyword ∷ T.Text → Phase2 T.Text
keyword kw = match $ \case
  (OName kw') | kw == kw' → pure kw'
  _ → expected $ "keyword: " <> kw

name ∷ Phase2 Name
name = match $ \case
  OName xs → pure xs 
  _ → expected "name"

xname ∷ Phase2 XName
xname = srcAnn name 

params ∷ Phase2 Params
params = matchList BrackList $ chomps xname 

xparams ∷ Phase2 XParams
xparams = srcAnn params

str ∷ Phase2 Expr 
str = match $ \case
  OStr xs → pure $ EStr xs
  _ → expected "string"

var ∷ Phase2 Expr
var = match $ \case
  OName xs → pure $ EVar xs
  _ → expected "variable"

arr ∷ Phase2 Expr 
arr = matchList BrackList $ EArr <$> chomps xexpr

app ∷ Phase2 Expr
app = matchList ParenList $ EApp <$> chomp xexpr <*> chomps xexpr

expr ∷ Phase2 Expr
expr = choice [str, var, arr, app]

xexpr ∷ Phase2 XExpr 
xexpr = srcAnn expr

letfun ∷ Phase2 Stmt
letfun = matchList ParenList $ do
  chomp $ keyword "let"
  name ← chomp xname
  params ← chomp xparams
  body ← chomp xexpr 
  pure $ SLetFun name params body

letval ∷ Phase2 Stmt
letval = matchList ParenList $ do 
  chomp $ keyword "let"
  name ← chomp xname 
  body ← chomp xexpr 
  pure $ SLetVal name body 

stmt ∷ Phase2 Stmt
stmt = choice [letfun, letval]

xstmt ∷ Phase2 XStmt
xstmt = srcAnn stmt 

phase2 ∷ [XObj] → Either Phase2Error [XStmt] 
phase2 xs = forM xs $ runPhase2 xstmt
