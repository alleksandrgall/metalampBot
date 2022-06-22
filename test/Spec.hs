module Spec where

import Bot

main :: IO ()
main = do
  processCallback
  processCommand
  processMessage
  processUpdates
