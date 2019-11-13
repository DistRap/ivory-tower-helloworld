module Main where

import Hello.Tests.Platforms
import Hello.Tests.Bluepill (app)

import qualified Ivory.BSP.STM32F103 as F103

bluepillWithDisplay = bluepill {
    platformSPIDevs  = [ max7219dev F103.spi1 F103.pinA4 ]
  }

main :: IO ()
main = buildHelloApp bluepillWithDisplay app
