module Main where

import Hello.Tests.Platforms
import Hello.Tests.NucleoPLC (app)

main :: IO ()
main = buildHelloApp nucleo_g474 app
