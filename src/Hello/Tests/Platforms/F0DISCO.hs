{-# LANGUAGE NumericUnderscores #-}

module Hello.Tests.Platforms.F0DISCO where

import Hello.Tests.Platforms.Types

import Ivory.BSP.STM32.Peripheral.GPIO
import Ivory.BSP.STM32.ClockConfig

import Ivory.BSP.STM32F051

import Ivory.BSP.STM32.Peripheral.I2C
import Ivory.BSP.STM32.Peripheral.SPI
import Ivory.BSP.STM32.Peripheral.UART

import qualified Ivory.Tower.Base as Base

-- 48Mhz core and peripherals using 8Mhz external crystal
f0disco_cc:: ClockConfig
f0disco_cc = ClockConfig
  { clockconfig_source = HSE 8_000_000
  , clockconfig_pll    = PLLFactorMulDiv
    { pll_mul = 6
    , pll_div = 1
    }
  , clockconfig_hclk_divider = 1
  , clockconfig_pclk1_divider = 1
  , clockconfig_pclk2_divider = 1
  }

i2c1Pins :: I2CPins
i2c1Pins = I2CPins {
    i2cpins_sda = pinF7
  , i2cpins_scl = pinF6
  }

usart1Pins :: UARTPins
usart1Pins = UARTPins
  { uartPinTx = pinA9
  , uartPinRx = pinA10
  }

spiPins :: SPIPins
spiPins = SPIPins
    { spiPinMiso = pinB4
    , spiPinMosi = pinB5
    , spiPinSck  = pinB3
    }

spiCSPin :: GPIOPin
spiCSPin = pinD2

f0disco :: Platform
f0disco = Platform {
    platformClocks   = f0disco_cc
  , platformMCU      = Nothing
  , platformMCUName  = "STM32F051R8T6"
  , platformPin      = pinC9
  , platformPinIn    = pinA0 -- user button
  , platformRedLED   = Base.LED pinC9 Base.ActiveHigh
  , platformGreenLED = Base.LED pinC8 Base.ActiveHigh -- actually blue
  , platformSPI      = spi1
  , platformSPIPins  = spiPins
  , platformSPIDevs  = []
  , platformI2C      = i2c1
  , platformI2CPins  = i2c1Pins
  , platformUART     = usart1
  , platformUARTPins = usart1Pins
  , platformDMAUART  = undef "DMAUART"
  , platformCAN      = undef "CAN"
  , platformIWDG     = iwdg
  , platformEXTI     = exti
  , platformRNG      = undef "RNG"
  }
