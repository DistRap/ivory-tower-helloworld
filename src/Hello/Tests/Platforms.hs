{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}

module Hello.Tests.Platforms where

import Ivory.Tower.Config
import Data.Char (toUpper)

import qualified Ivory.BSP.STM32F405.CAN         as F405
import qualified Ivory.BSP.STM32F405.UART        as F405
import qualified Ivory.BSP.STM32F405.GPIO        as F405
import qualified Ivory.BSP.STM32F405.GPIO.AF     as F405
import qualified Ivory.BSP.STM32F405.SPI         as F405
import qualified Ivory.BSP.STM32F405.I2C         as F405
import qualified Ivory.BSP.STM32F405.RNG         as F405

import Ivory.BSP.STM32.Peripheral.CAN
import Ivory.BSP.STM32.Peripheral.GPIOF4
import Ivory.BSP.STM32.Peripheral.UART
import Ivory.BSP.STM32.Peripheral.SPI hiding (ActiveHigh, ActiveLow)
import Ivory.BSP.STM32.Peripheral.I2C
import Ivory.BSP.STM32.Peripheral.RNG
import Ivory.BSP.STM32.Peripheral.UART.DMA
import Ivory.BSP.STM32.Peripheral.SPI as SPI
import Ivory.BSP.STM32.ClockConfig
import Ivory.BSP.STM32.Config

import qualified Ivory.Tower.Base as Base

testPlatformParser :: ConfigParser TestPlatform
testPlatformParser = do
  p <- subsection "args" $ subsection "platform" string
  case map toUpper p of
    "HELLO4DISCO"       -> result hello4disco
    "HELLO4L"           -> result hello4l
    _ -> fail ("no such platform " ++ p)

  where
  result platform = do
    conf <- stm32ConfigParser (testplatform_stm32 platform)
    return platform { testplatform_stm32 = conf }


data ColoredLEDs =
  ColoredLEDs
    { greenLED :: Base.LED
    , orangeLED :: Base.LED
    , redLED  :: Base.LED
    , blueLED :: Base.LED
    }

data TestUART =
  TestUART
    { testUARTPeriph :: UART
    , testUARTPins   :: UARTPins
    }

data TestSPI =
  TestSPI
    { testSPIPeriph :: SPIPeriph
    , testSPIPins   :: SPIPins
    -- TODO FIXME: move CS pins for test devices into TestSPI
    }

data TestI2C =
  TestI2C
    { testI2C     :: I2CPeriph
    , testI2CPins :: I2CPins
    }

data TestCAN =
  TestCAN
    { testCAN        :: CANPeriph
    , testCANRX      :: GPIOPin
    , testCANTX      :: GPIOPin
    , testCANFilters :: CANPeriphFilters
    }

data TestDMA =
  TestDMA
    { testDMAUARTPeriph :: DMAUART
    , testDMAUARTPins   :: UARTPins
    }

data TestPlatform =
  TestPlatform
    { testplatform_leds   :: ColoredLEDs
    , testplatform_uart   :: TestUART
    , testplatform_i2c    :: TestI2C
    , testplatform_spi    :: TestSPI
    , testplatform_can1   :: TestCAN
    , testplatform_rng    :: RNG
    , testplatform_stm32  :: STM32Config
    -- Used by SimpleBlink
    , testplatform_ledpin :: GPIOPin
    }

testplatform_clockconfig :: TestPlatform -> ClockConfig
testplatform_clockconfig = stm32config_clock . testplatform_stm32

spi1_pins :: SPIPins
spi1_pins = SPIPins
  { spiPinMiso = F405.pinA6
  , spiPinMosi = F405.pinA7
  , spiPinSck  = F405.pinA5
  , spiPinAF   = F405.gpio_af_spi1
  }

spi2_pins :: SPIPins
spi2_pins = SPIPins
  { spiPinMiso = F405.pinD4
  , spiPinMosi = F405.pinD3
  , spiPinSck  = F405.pinD1
  , spiPinAF   = F405.gpio_af_spi2
  }

max7219dev :: SPI.SPIPeriph -> SPI.SPIDevice
max7219dev spi =  SPI.SPIDevice
    { spiDevPeripheral    = spi
    , spiDevCSPin         = F405.pinA4 -- D7
    , spiDevClockHz       = 250000
    , spiDevCSActive      = SPI.ActiveLow
    , spiDevClockPolarity = ClockPolarityLow
    , spiDevClockPhase    = ClockPhase1
    , spiDevBitOrder      = MSBFirst
    , spiDevName          = "max7219"
    }

max7219dev2 :: SPI.SPIPeriph -> SPI.SPIDevice
max7219dev2 spi =  SPI.SPIDevice
    { spiDevPeripheral    = spi
    , spiDevCSPin         = F405.pinD6
    , spiDevClockHz       = 500000
    , spiDevCSActive      = SPI.ActiveLow
    , spiDevClockPolarity = ClockPolarityLow
    , spiDevClockPhase    = ClockPhase1
    , spiDevBitOrder      = MSBFirst
    , spiDevName          = "max7219_2"
    }


-- HELLO4DISCO
hello4disco :: TestPlatform
hello4disco = TestPlatform
  { testplatform_leds = ColoredLEDs
      { redLED  = Base.LED F405.pinD14 Base.ActiveHigh
      , blueLED = Base.LED F405.pinD15 Base.ActiveHigh
      , orangeLED = Base.LED F405.pinD15 Base.ActiveHigh
      , greenLED = Base.LED F405.pinD15 Base.ActiveHigh
      }
  , testplatform_uart = TestUART
    { testUARTPeriph = F405.uart2
    , testUARTPins = UARTPins
        { uartPinTx = F405.pinA2
        , uartPinRx = F405.pinA3
        , uartPinAF = F405.gpio_af_uart2
        }
    }
  , testplatform_spi = TestSPI
      { testSPIPeriph = F405.spi2
      , testSPIPins   = spi2_pins
      }
  , testplatform_i2c = TestI2C
      { testI2C = F405.i2c2
      , testI2CPins = I2CPins
        { i2cpins_sda = F405.pinB11
        , i2cpins_scl = F405.pinB10
        }
      }
  , testplatform_can1 = TestCAN
      { testCAN = F405.can1
      , testCANRX = F405.pinB8
      , testCANTX = F405.pinB9
      , testCANFilters = F405.canFilters
      }
  , testplatform_rng = F405.rng
  , testplatform_stm32 = stm32f405Defaults 8

  -- SimpleBlink uses this for simplicity
  , testplatform_ledpin = F405.pinD14
  }

-- HELLO4L
hello4l :: TestPlatform
hello4l = TestPlatform
  { testplatform_leds = ColoredLEDs
      { redLED  = Base.LED F405.pinC9 Base.ActiveHigh
      , blueLED = Base.LED F405.pinD15 Base.ActiveHigh
      , orangeLED = Base.LED F405.pinA5 Base.ActiveHigh
      , greenLED = Base.LED F405.pinB14 Base.ActiveHigh
      }
  , testplatform_uart = TestUART
    { testUARTPeriph = F405.uart1
    , testUARTPins = UARTPins
        { uartPinTx = F405.pinA9
        , uartPinRx = F405.pinA10
        , uartPinAF = F405.gpio_af_uart1
        }
    }
  , testplatform_i2c = TestI2C
      { testI2C = F405.i2c2
      , testI2CPins = I2CPins
        { i2cpins_sda = F405.pinB13
        , i2cpins_scl = F405.pinB14
        }
      }
  , testplatform_spi = TestSPI
      { testSPIPeriph = F405.spi2
      , testSPIPins   = spi2_pins
      }
  , testplatform_can1 = TestCAN
      { testCAN = F405.can1
      , testCANRX = F405.pinB8
      , testCANTX = F405.pinB9
      , testCANFilters = F405.canFilters
      }
  , testplatform_rng = F405.rng
  , testplatform_stm32 = stm32l431Defaults 8

  -- SimpleBlink uses this for simplicity
  , testplatform_ledpin = F405.pinB14
  }
