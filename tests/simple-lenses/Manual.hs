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

a :: Lens' Example Int
a f (Example x y z) = (\x' -> Example x' y z) <$> f x

b :: Lens' Example (String, Int)
b f (Example x y z) = (\y' -> Example x y' z) <$> f y

c :: Lens' Example Char
c f (Example x y z) = (\z' -> Example x y z') <$> f z