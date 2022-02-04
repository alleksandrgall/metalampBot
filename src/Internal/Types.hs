module Internal.Types (Token, Url, Protocol(..)) where

import           Data.Text

data Protocol = Http | Https deriving (Eq)
type Token = String
type Url = Text
