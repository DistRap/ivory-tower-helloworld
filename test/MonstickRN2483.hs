module Main where

import Hello.Tests.Platforms
import Hello.Tests.Monstick.RN2483 (app)

main :: IO ()
main = buildHelloApp monstick app
