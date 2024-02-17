-- IOT01A specific test
-- for PCF8591 ADC/DAC
module Main where

import Hello.Tests.Platforms
import Hello.Tests.Platforms.IOT01A (i2c1Pins)
import Hello.Tests.PCF8591 (app)

import Ivory.BSP.STM32L475

-- PCF8591 connected to external I2C
-- SCL D15
-- SDA D14
-- +3V3 & GND
iot01a' :: Platform
iot01a' = iot01a {
    platformI2C = i2c1
  , platformI2CPins = i2c1Pins
  }

main :: IO ()
main = buildHelloApp iot01a' app
