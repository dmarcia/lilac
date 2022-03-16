module Lilac.Parser.Error where

import Data.Void
import qualified Data.Text as T
import Text.Megaparsec (SourcePos, ParseErrorBundle)

type Phase1Error = ParseErrorBundle T.Text Void

data Phase2Error 
  = NoPhase2Error -- ^ Empty error; for 'Monoid'.
  | MkPhase2Error
    { p2Pos     ∷ SourcePos
    , p2Message ∷ T.Text 
    }
  deriving Show

instance Semigroup Phase2Error where
  NoPhase2Error <> r = r
  l <> _ = l

instance Monoid Phase2Error where
  mempty = NoPhase2Error
