{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Config
import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Monad.Reader.Class
import Logger
import Data.Text
import Data.Text.IO as T
import Prelude hiding (log)
import Control.Monad.Writer (Writer, tell, runWriter)



main = do
  config <- fetchConfig 
  print config
