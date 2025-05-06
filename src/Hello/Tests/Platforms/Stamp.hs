
module Hello.Tests.Platforms.Stamp where

import Hello.Tests.Platforms.Types

import Ivory.BSP.STM32.Peripheral.GPIO
import Ivory.BSP.STM32.ClockConfig

import Ivory.BSP.STM32F765
import Ivory.BSP.STM32F765.Clock

import Ivory.BSP.STM32.Peripheral.I2C
import Ivory.BSP.STM32.Peripheral.SPI
import Ivory.BSP.STM32.Peripheral.UART

import qualified Ivory.Tower.Base as Base

-- | 25Mhz External crystal (HSE) clock configuration
stamp_hse_cc :: ClockConfig
stamp_hse_cc =
  ClockConfig {
    clockconfig_source = HSE $ 25 * 1000 * 1000
  , clockconfig_pll    = PLLFactorMNP
    { pll_m = 25
    , pll_n = 432
    , pll_p = 2
    , pll_q = 2
    }
  , clockconfig_hclk_divider = 1
  , clockconfig_pclk1_divider = 4
  , clockconfig_pclk2_divider = 2
  }

-- | 16Mhz Internal oscillator (HSI) clock configuration
stamp_hsi_cc :: ClockConfig
stamp_hsi_cc =
  ClockConfig {
    clockconfig_source = clockHSI
  , clockconfig_pll    = PLLFactorMNP
    { pll_m = 8
    , pll_n = 216
    , pll_p = 2
    , pll_q = 2
    }
  , clockconfig_hclk_divider = 1
  , clockconfig_pclk1_divider = 4
  , clockconfig_pclk2_divider = 2
  }

i2c1Pins :: I2CPins
i2c1Pins = I2CPins {
    i2cpins_sda = pinB9
  , i2cpins_scl = pinB8
  }

i2c2Pins :: I2CPins
i2c2Pins = I2CPins {
    i2cpins_sda = pinB11
  , i2cpins_scl = pinB10
  }

-- part of devboard debug connector
usart1Pins :: UARTPins
usart1Pins = UARTPins
  { uartPinTx = pinA9
  , uartPinRx = pinA10
  }

-- only on stamp itself, not broken out to devboard
usart2Pins :: UARTPins
usart2Pins = UARTPins
  { uartPinTx = pinD5
  , uartPinRx = pinD6
  }

-- above debug connector on devboard
uart5Pins :: UARTPins
uart5Pins = UARTPins
  { uartPinTx = pinB13
  , uartPinRx = pinB12
  }

spi1Pins :: SPIPins
spi1Pins = SPIPins
    { spiPinMiso = pinD7
    , spiPinMosi = pinA6
    , spiPinSck  = pinA5
    }

spi1CSPin :: GPIOPin
spi1CSPin = pinE7

spi2Pins :: SPIPins
spi2Pins = SPIPins
    { spiPinMiso = pinC2
    , spiPinMosi = pinC1
    , spiPinSck  = pinD3
    }

spi2CSPin :: GPIOPin
spi2CSPin = pinC15

spi3Pins :: SPIPins
spi3Pins = SPIPins
    { spiPinMiso = pinC11
    , spiPinMosi = pinC12
    , spiPinSck  = pinB3
    }

spi3CSPin :: GPIOPin
spi3CSPin = pinD2

-- not tested, only doable on fixed rev2 board
stampCAN1 :: CANConfig
stampCAN1 = CANConfig {
    canPeriph = can1
  , canPeriphFilters = canFilters
  , canRxPin = pinA11
  , canTxPin = pinA12
  }

stampCAN3 :: CANConfig
stampCAN3 = CANConfig {
    canPeriph = can3
  , canPeriphFilters = can3Filters
  , canRxPin = pinA8
  , canTxPin = pinA15
  }

stamp :: Platform
stamp = Platform {
    platformClocks   = stamp_hse_cc
  , platformMCU      = Nothing
  , platformMCUName  = "STM32F765VGT6"
  , platformPin      = pinD9
  , platformPinIn    = pinE3
  , platformRedLED   = Base.LED pinD9 Base.ActiveHigh
  , platformGreenLED = Base.LED pinD10 Base.ActiveHigh
  , platformSPI      = spi1
  , platformSPIPins  = spi1Pins
  , platformSPIDevs  = []
  , platformI2C      = i2c1
  , platformI2CPins  = i2c1Pins
  , platformUART     = usart1
  , platformUARTPins = usart1Pins
  , platformDMAUART  = dmausart1
  , platformCAN      = stampCAN3
  , platformIWDG     = iwdg
  , platformETH      = undef "ETH"
  , platformEXTI     = exti
  , platformRNG      = rng
  }

as5047dev :: SPI -> GPIOPin -> SPIDevice
as5047dev spi csPin =  SPIDevice
    { spiDevPeripheral    = spi
    , spiDevCSPin         = csPin
    , spiDevClockHz       = 500000
    , spiDevCSActive      = ActiveLow
    , spiDevClockPolarity = ClockPolarityLow -- CPOL=0
    , spiDevClockPhase    = ClockPhase2      -- CPHA=1
    , spiDevBitOrder      = MSBFirst
    , spiDevName          = "as5407"
    }
