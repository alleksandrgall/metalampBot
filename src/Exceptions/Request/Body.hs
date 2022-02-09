module Exceptions.Request.Body where
import           Control.Monad.Catch (Exception)
import           Data.Text           (Text)



newtype BodyException = EmptyReponseBody Text deriving (Show)

instance Exception BodyException
