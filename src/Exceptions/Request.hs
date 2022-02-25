module Exceptions.Request
    (RequestException(..)
    ,BodyException(..)
    ,ParseException(..)
    ,WebException(..))
where

import           Control.Exception        (Exception (..))
import           Exceptions.Request.Body  (BodyException (..))
import           Exceptions.Request.Parse (ParseException (..))
import           Exceptions.Request.Web   (WebException (..))

data RequestException = RWebException WebException | RParseException ParseException | RBodyException BodyException
    deriving (Show)
instance Exception RequestException
