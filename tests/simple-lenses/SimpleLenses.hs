{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Control.Lens.TH

main :: IO ()
main = putStrLn "Hello, World!"

data Example
   = Example
   { _a :: Int
   , _b :: (String, Int)
   , _c :: Char
   }

makeLenses ''Example
