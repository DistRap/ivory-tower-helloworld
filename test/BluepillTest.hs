module Main where

import Hello.Tests.Platforms
import Hello.Tests.Bluepill (app)

import qualified Ivory.BSP.STM32F103 as F103
import qualified Hello.Tests.Platforms.IOT01A as IOT

bluepillWithDisplay :: Platform
bluepillWithDisplay = bluepill {
    platformSPIDevs  = [ IOT.max7219dev F103.spi1 F103.pinA4 ]
  }

main :: IO ()
main = buildHelloApp bluepillWithDisplay app
