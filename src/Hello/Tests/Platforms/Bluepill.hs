module Hello.Tests.Platforms.Bluepill where

import Hello.Tests.Platforms.Types

import Ivory.BSP.STM32.Peripheral.GPIO
import Ivory.BSP.STM32.ClockConfig

import Ivory.BSP.STM32F103

import Ivory.BSP.STM32.Peripheral.I2C

import Ivory.BSP.STM32.Peripheral.SPI
import Ivory.BSP.STM32.Peripheral.SPI.Pins

import Ivory.BSP.STM32.Peripheral.UART.Pins
import Ivory.BSP.STM32.Peripheral.UART

import qualified Ivory.Tower.Base as Base

bluepill = Platform {
    platformClocks   = bluepill_cc
  , platformMCU = "STM32F103C8T6"
  , platformPin = pinC13
  , platformRedLED = Base.LED pinC13 Base.ActiveHigh
  , platformSPI = spi1
  , platformSPIPins = SPIPins
    { spiPinMiso = pinA6
    , spiPinMosi = pinA7
    , spiPinSck  = pinA5
    , spiPinAF   = gpio_af0
    }
  , platformSPIDevs = []
  , platformUART = usart2
  , platformUARTPins = UARTPins
      { uartPinTx = pinA2
      , uartPinRx = pinA3
      , uartPinAF = gpio_af0
      }
  , platformCAN = undef "CAN"
  }

-- 72Mhz with 8Mhz external crystal
bluepill_cc = ClockConfig { clockconfig_source = HSE $ 8 * 1000 * 1000
                          , clockconfig_pll    = PLLFactorMulDiv
                            { pll_mul = 9
                            , pll_div = 1
                            }
                          , clockconfig_hclk_divider = 1
                          , clockconfig_pclk1_divider = 2
                          , clockconfig_pclk2_divider = 1
                          }
