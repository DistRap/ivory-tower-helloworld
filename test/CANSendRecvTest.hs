module Main where

import Hello.Tests.Platforms
import Hello.Tests.CANSendRecv (app)

main = buildHelloApp f4disco app
main :: IO ()
