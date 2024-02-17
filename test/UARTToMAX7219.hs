module Main where

import Hello.Tests.Platforms
import Hello.Tests.UARTToMAX7219 (app)

import qualified Ivory.BSP.STM32F765 as F765
import qualified Hello.Tests.Platforms.IOT01A as IOT
import qualified Hello.Tests.Platforms.Stamp as Stamp

stampWithDisplay :: Platform
stampWithDisplay = stamp
  { platformSPI = F765.spi2
  , platformSPIPins = Stamp.spi2Pins
  , platformSPIDevs  = [ IOT.max7219dev F765.spi2 Stamp.spi2CSPin ]
  }

main :: IO ()
main = buildHelloApp stampWithDisplay app
