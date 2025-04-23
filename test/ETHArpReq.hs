module Main where

import Hello.Tests.Platforms
import Hello.Tests.ETHArpReq (app)

main :: IO ()
main = buildHelloApp nucleo_f767 app
