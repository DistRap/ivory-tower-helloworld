
module Hello.Tests.Platforms.F4DISCO where

import Hello.Tests.Platforms.Types

import Ivory.BSP.STM32.Peripheral.GPIO
import Ivory.BSP.STM32.ClockConfig

import Ivory.BSP.STM32F407

import Ivory.BSP.STM32.Peripheral.I2C
import Ivory.BSP.STM32.Peripheral.SPI
import Ivory.BSP.STM32.Peripheral.UART

import qualified Ivory.Tower.Base as Base


f4disco_cc :: ClockConfig
f4disco_cc = externalXtalF4 8 168

i2c2Pins :: I2CPins
i2c2Pins = I2CPins {
    i2cpins_sda = pinB11
  , i2cpins_scl = pinB10
  }

usart2Pins :: UARTPins
usart2Pins = UARTPins
  { uartPinTx = pinA2
  , uartPinRx = pinA3
  }

spiPins :: SPIPins
spiPins = SPIPins
    { spiPinMiso = pinC11
    , spiPinMosi = pinC12
    , spiPinSck  = pinC10
    }

spiCSPin :: GPIOPin
spiCSPin = pinA15

f4disco :: Platform
f4disco = Platform {
    platformClocks   = f4disco_cc
  , platformMCU      = Nothing
  , platformMCUName  = "STM32F407VGT6"
  , platformPin      = pinD14
  , platformPinIn    = pinC0
  , platformRedLED   = Base.LED pinD14 Base.ActiveHigh
  , platformGreenLED = Base.LED pinD15 Base.ActiveHigh
  , platformSPI      = spi1
  , platformSPIPins  = spiPins
  , platformSPIDevs  = []
  , platformI2C      = i2c2
  , platformI2CPins  = i2c2Pins
  , platformUART     = usart2
  , platformUARTPins = usart2Pins
  , platformDMAUART  = dmausart2
  , platformCAN      = f4discoCAN1
  , platformIWDG     = iwdg
  , platformEXTI     = exti
  , platformRNG      = rng
  }

f4discoCAN1 :: CANConfig
f4discoCAN1 = CANConfig {
    canPeriph = can1
  , canPeriphFilters = canFilters
  , canRxPin = pinB8
  , canTxPin = pinB9
  }

f4discoCAN2 :: CANConfig
f4discoCAN2 = CANConfig {
    canPeriph = can2
  , canPeriphFilters = canFilters
  , canRxPin = pinB5
  , canTxPin = pinB6
  }
