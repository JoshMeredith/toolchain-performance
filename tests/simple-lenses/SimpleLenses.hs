{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens.TH

main :: IO ()
main = putStrLn "Hello, World!"

data Example
   = Example
   { a :: Int
   , b :: (String, Int)
   , c :: Char
   }

makeLenses ''Example
