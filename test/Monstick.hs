module Main where

import Hello.Tests.Platforms
import Hello.Tests.Monstick (app)

main :: IO ()
main = buildHelloApp monstick app
