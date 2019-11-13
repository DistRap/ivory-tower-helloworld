
module Main where

import Hello.Tests.Platforms
import Hello.Tests.IOT01A (app)

main :: IO ()
main = buildHelloApp iot01a app
