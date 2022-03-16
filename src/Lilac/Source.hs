module Lilac.Source where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Megaparsec (SourcePos)

-- | Representation of a source file.
data Source = MkSource
  { srcPath    ∷ FilePath 
  , srcContent ∷ T.Text 
  }

-- | Reads a source file from the provided file path.
readSource ∷ FilePath → IO Source
readSource filePath = MkSource filePath <$> T.readFile filePath

-- | Wraps a 'SourcePos' with an extra annotation type.
data SourceAnn a = MkSourceAnn
  { annPos ∷ SourcePos
  , ann    ∷ a 
  }

deriving instance Show a ⇒ Show (SourceAnn a)
