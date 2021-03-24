{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Lib
    ( someFunc
    ) where

import Data.Aeson
import Data.Text


someFunc :: IO ()
someFunc = putStrLn "someFunc"
