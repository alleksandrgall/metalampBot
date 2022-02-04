module Exceptions.Body where
import           Control.Monad.Catch (Exception)
import           Data.Text           (Text)



data BodyException = EmptyReponseBody Text deriving (Show)

instance Exception BodyException
