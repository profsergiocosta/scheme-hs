{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( someFunc,
  )
where

import Data.Text as T

someFunc :: IO ()
someFunc = putStrLn "someFunc"
