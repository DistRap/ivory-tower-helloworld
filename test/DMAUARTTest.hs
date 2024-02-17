module Main where

import Hello.Tests.Platforms
import Hello.Tests.DMAUART (app)

main :: IO ()
main = buildHelloApp f4disco app
