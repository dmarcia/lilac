module Lilac.Utility where

import qualified Data.Text as T

tshow ∷ Show a ⇒ a → T.Text
tshow = T.pack . show 

untilM ∷ Monad m ⇒ m a → m Bool → m [a]
untilM m p = p >>= \case
  True → (:) <$> m <*> untilM m p
  False → pure []
