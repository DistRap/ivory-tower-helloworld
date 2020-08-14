
module Hello.Tests.Platforms.Types where

import Ivory.Tower.Base (LED)

import Ivory.BSP.STM32.ClockConfig
import Ivory.BSP.STM32.MCU
import Ivory.BSP.STM32.Peripheral.CAN
import Ivory.BSP.STM32.Peripheral.EXTI
import Ivory.BSP.STM32.Peripheral.GPIO
import Ivory.BSP.STM32.Peripheral.I2C
import Ivory.BSP.STM32.Peripheral.IWDG
import Ivory.BSP.STM32.Peripheral.RNG
import Ivory.BSP.STM32.Peripheral.SPI
import Ivory.BSP.STM32.Peripheral.UART

data Platform = Platform {
    platformMCU      :: Maybe NamedMCU
  , platformMCUName  :: String
  , platformClocks   :: ClockConfig
  , platformPin      :: GPIOPin
  , platformPinIn    :: GPIOPin
  , platformRedLED   :: LED
  , platformGreenLED :: LED

  -- peripherals
  , platformCAN      :: CANConfig
  , platformEXTI     :: EXTI
  , platformIWDG     :: IWDG
  , platformRNG      :: RNG

  , platformSPI      :: SPI
  , platformSPIPins  :: SPIPins
  , platformSPIDevs  :: [ SPIDevice ]

  , platformI2C      :: I2C
  , platformI2CPins  :: I2CPins

  , platformUART     :: UART
  , platformUARTPins :: UARTPins

  }

undef :: String -> a
undef part = error $ part ++ " is not available on this platform"

data CANConfig = CANConfig
  { canPeriph        :: CANPeriph
  , canPeriphFilters :: CANPeriphFilters
  , canRxPin         :: GPIOPin
  , canTxPin         :: GPIOPin
  }
