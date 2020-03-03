module Main where

import Hello.Tests.Platforms
import Hello.Tests.Platforms.Monstick
import Ivory.BSP.STM32L431
import Hello.Tests.ADS (app)

-- external I2C0
p = monstick {
    platformI2C = i2c3
  , platformI2CPins = i2c3Pins
  }
main :: IO ()
main = buildHelloApp p app
