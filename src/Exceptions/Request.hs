module Exceptions.Request
  ( RequestException (..),
    ParseException (..),
    WebException (..),
  )
where

import Control.Exception (Exception (..))
import Exceptions.Request.Parse (ParseException (..))
import Exceptions.Request.Web (WebException (..))

data RequestException = RWebException WebException | RParseException ParseException
  deriving (Show)

instance Exception RequestException
