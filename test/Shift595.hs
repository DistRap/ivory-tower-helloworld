module Main where

import Hello.Tests.Platforms
import Hello.Tests.Shift595 (app)

import qualified Ivory.BSP.STM32F103 as F103
import Ivory.Tower.Drivers.SPIDevice (genericSPIDev)

bluepill595 :: Platform
bluepill595 = bluepill {
    platformSPIDevs  = [ genericSPIDev "ic595" F103.spi1 F103.pinA4 ]
  }

main :: IO ()
main = buildHelloApp bluepill595 app
