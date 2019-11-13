module Main where

import Hello.Tests.Platforms
import Hello.Tests.SimpleBlink (app)

main :: IO ()
main = buildHelloApp iot01a app
