module Main where

import Hello.Tests.Platforms
import Hello.Tests.EXTI (app)

main :: IO ()
main = buildHelloApp f4disco app
