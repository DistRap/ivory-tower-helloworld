
module Hello.Tests.Platforms.IOT01A where

import Hello.Tests.Platforms.Types

import Ivory.BSP.STM32.Peripheral.GPIO
import Ivory.BSP.STM32.ClockConfig

import Ivory.BSP.STM32L475

import Ivory.BSP.STM32.Peripheral.I2C

import Ivory.BSP.STM32.Peripheral.SPI
import Ivory.BSP.STM32.Peripheral.SPI.Pins

import Ivory.BSP.STM32.Peripheral.UART.Pins
import Ivory.BSP.STM32.Peripheral.UART

import qualified Ivory.Tower.Base as Base


i2c1Pins = I2CPins { i2cpins_sda = pinB9
                   , i2cpins_scl = pinB8 }

i2c2Pins = I2CPins { i2cpins_sda = pinB11
                   , i2cpins_scl = pinB10 }

spiPins = SPIPins
    { spiPinMiso = pinD3
    , spiPinMosi = pinD4
    , spiPinSck  = pinD1
    }

uartPins = UARTPins
  { uartPinTx = pinB6
  , uartPinRx = pinB7
  }

iot01a_cc = ClockConfig { clockconfig_source = MSI $ 16 * 1000 * 1000 -- HSI16
                        , clockconfig_pll    = PLLFactorMNR
                          { pll_mnr_m = 1
                          , pll_mnr_n = 10
                          , pll_mnr_p = 7
                          , pll_mnr_q = 2
                          , pll_mnr_r = 2
                          }
                        , clockconfig_hclk_divider = 1
                        , clockconfig_pclk1_divider = 1
                        , clockconfig_pclk2_divider = 1
                        }

pin = pinA5

iot01a = Platform {
    platformClocks   = iot01a_cc
  , platformMCU      = Nothing
  , platformMCUName  = "STM32L475VGT6"
  , platformPin      = pin
  , platformRedLED   = Base.LED pinB14 Base.ActiveHigh -- green as well
  , platformGreenLED = Base.LED pin Base.ActiveHigh
  , platformSPI      = spi2
  , platformSPIPins  = spiPins
  , platformSPIDevs  = iot01a_spidevs
  , platformI2C      = i2c2
  , platformI2CPins  = i2c2Pins
  , platformUART     = usart1
  , platformUARTPins = uartPins
  , platformCAN      = undef "CAN"
  , platformIWDG     = iwdg
  , platformEXTI     = exti
  , platformRNG      = rng
  }

-- max7219 display connected to PMOD connector
iot01a_spidevs = [ max7219dev spi2 pinD5 ]

max7219dev :: SPI -> GPIOPin -> SPIDevice
max7219dev spi csPin =  SPIDevice
    { spiDevPeripheral    = spi
    , spiDevCSPin         = csPin
    , spiDevClockHz       = 500000
    , spiDevCSActive      = ActiveLow
    , spiDevClockPolarity = ClockPolarityLow
    , spiDevClockPhase    = ClockPhase1
    , spiDevBitOrder      = MSBFirst
    , spiDevName          = "max7219"
    }


