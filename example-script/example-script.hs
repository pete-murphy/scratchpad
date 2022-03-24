#!/usr/bin/env stack
-- stack --resolver lts-18.18 script --package rio

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

import RIO
import System.IO (putStrLn)

main :: IO ()
main = do
  putStrLn "Text 1"
  putStrLn "Text 2"
  runSimpleApp $ do
    logInfo "inside rio"
