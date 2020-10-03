module Main where

import Hello.Tests.Platforms
import Hello.Tests.CAN2UART (app)

main :: IO ()
main = buildHelloApp f4disco app
