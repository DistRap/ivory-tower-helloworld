module Main where

import Hello.Tests.Platforms
import Hello.Tests.Blink (app)

main :: IO ()
main = buildHelloApp f4disco app
