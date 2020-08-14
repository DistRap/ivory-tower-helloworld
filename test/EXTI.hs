module Main where

import Hello.Tests.Platforms
import Hello.Tests.EXTI (app)

main :: IO ()
main = buildHelloApp iot01a app
