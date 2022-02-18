module Exceptions.Request.Parse
    (ParseException(..))
where
import           Control.Exception (Exception)
import           Data.Text         (Text)

newtype ParseException = WrongType Text
    deriving (Show)
