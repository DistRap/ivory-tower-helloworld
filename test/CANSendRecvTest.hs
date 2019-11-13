module Main where

import Hello.Tests.Platforms
import Hello.Tests.CANSendRecv (app)

main = buildHelloApp iot01a app
main :: IO ()
