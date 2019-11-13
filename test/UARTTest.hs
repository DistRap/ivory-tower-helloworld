module Main where

import Hello.Tests.Platforms
import Hello.Tests.UART (app)

main :: IO ()
main = buildHelloApp iot01a app
