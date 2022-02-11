module Exceptions.Request.Body where
import           Control.Monad.Catch (Exception)
import           Data.Text           (Text)



data BodyException = EmptyReponseBody deriving (Show)

instance Exception BodyException
