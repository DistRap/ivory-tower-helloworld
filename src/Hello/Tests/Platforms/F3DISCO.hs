{-# LANGUAGE NumericUnderscores #-}

module Hello.Tests.Platforms.F3DISCO where

import Hello.Tests.Platforms.Types

import Ivory.BSP.STM32.Peripheral.GPIO
import Ivory.BSP.STM32.ClockConfig

import Ivory.BSP.STM32F303
import Ivory.BSP.STM32F303.Clock

import Ivory.BSP.STM32.Peripheral.I2C
import Ivory.BSP.STM32.Peripheral.SPI
import Ivory.BSP.STM32.Peripheral.UART

import qualified Ivory.Tower.Base as Base

-- 72Mhz core using 8Mhz external crystal
f3disco_cc :: ClockConfig
f3disco_cc = ClockConfig
  { clockconfig_source = HSE 8_000_000
  , clockconfig_pll    = PLLFactorMulDiv
    { pll_mul = 9
    , pll_div = 1
    }
  , clockconfig_hclk_divider = 1
  , clockconfig_pclk1_divider = 2
  , clockconfig_pclk2_divider = 1
  }

i2c1Pins :: I2CPins
i2c1Pins = I2CPins {
    i2cpins_sda = pinB7
  , i2cpins_scl = pinB6
  }

usart1Pins :: UARTPins
usart1Pins = UARTPins
  { uartPinTx = pinC4
  , uartPinRx = pinC5
  }

spiPins :: SPIPins
spiPins = SPIPins
    { spiPinMiso = pinA6
    , spiPinMosi = pinA7
    , spiPinSck  = pinA5
    }

-- L3G20D CS pin
spiCSPin :: GPIOPin
spiCSPin = pinE3

-- leds
-- E8 blue
-- E9 red
-- E10 orange
-- E11 green
-- E12 blue
-- E13 red
-- E14 orange
-- E15 green

f3disco :: Platform
f3disco = Platform {
    platformClocks   = f3disco_cc
  , platformMCU      = Nothing
  , platformMCUName  = "STM32F303VCT6"
  , platformPin      = pinE10
  , platformPinIn    = pinA0 -- user button
  , platformRedLED   = Base.LED pinE9 Base.ActiveHigh
  , platformGreenLED = Base.LED pinE11 Base.ActiveHigh
  , platformSPI      = spi1
  , platformSPIPins  = spiPins
  , platformSPIDevs  = []
  , platformI2C      = i2c1
  , platformI2CPins  = i2c1Pins
  , platformUART     = usart1
  , platformUARTPins = usart1Pins
  , platformCAN      = undef "can"
  , platformIWDG     = iwdg
  , platformEXTI     = exti
  , platformRNG      = undef "rng"
  }
