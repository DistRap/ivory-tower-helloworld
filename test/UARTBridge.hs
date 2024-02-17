module Main where

import Hello.Tests.Platforms
import Hello.Tests.UARTBridge (app)

main :: IO ()
main = buildHelloApp monstick app
