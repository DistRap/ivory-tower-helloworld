
module Hello.Tests.Platforms.CAN4DISCO where

import Hello.Tests.Platforms.Types

import Ivory.BSP.STM32.Peripheral.GPIO
import Ivory.BSP.STM32.ClockConfig

import Ivory.BSP.STM32F407

import Ivory.BSP.STM32.Peripheral.I2C
import Ivory.BSP.STM32.Peripheral.SPI
import Ivory.BSP.STM32.Peripheral.UART

import qualified Ivory.Tower.Base as Base


c4d_cc :: ClockConfig
c4d_cc = externalXtalF4 8 168

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

can4disco :: Platform
can4disco = Platform {
    platformClocks   = c4d_cc
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
  , platformCAN      = c4dCAN1
  , platformIWDG     = iwdg
  , platformEXTI     = exti
  , platformRNG      = rng
  }

c4dCAN1 :: CANConfig
c4dCAN1 = CANConfig {
    canPeriph = can1
  , canPeriphFilters = canFilters
  , canRxPin = pinB8
  , canTxPin = pinB9
  }

c4dCAN2 :: CANConfig
c4dCAN2 = CANConfig {
    canPeriph = can2
  , canPeriphFilters = canFilters
  , canRxPin = pinB5
  , canTxPin = pinB6
  }
