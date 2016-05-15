module Main where

import Baselang (hours)

main :: IO ()
main = hours >>= print
