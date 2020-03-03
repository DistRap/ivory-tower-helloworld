{-# LANGUAGE RecordWildCards #-}
module Hello.Tests.Platforms (
    platformParser
  , getPlatform
  , buildHelloApp
  , module Hello.Tests.Platforms.Bluepill
  , module Hello.Tests.Platforms.CAN4DISCO
  , module Hello.Tests.Platforms.IOT01A
  , module Hello.Tests.Platforms.Monstick
  , module Hello.Tests.Platforms.Types
  ) where

import Hello.Tests.Platforms.Types
import Hello.Tests.Platforms.Bluepill (bluepill)
import Hello.Tests.Platforms.CAN4DISCO (can4disco)
import Hello.Tests.Platforms.IOT01A   (iot01a)
import Hello.Tests.Platforms.Monstick (monstick)

import Data.Char (toUpper)
import Ivory.Tower
import Ivory.Tower.Options
import Ivory.Tower.Config
import Ivory.OS.FreeRTOS.Tower.STM32
import Ivory.BSP.STM32.ClockConfig

platformParser :: Platform -> ConfigParser Platform
platformParser defPlatform = do
  p <- (subsection "args" $ subsection "platform" string) <|> pure "default"
  case map toUpper p of
    "BLUEPILL"       -> return bluepill
    "CAN4DISCO"      -> return can4disco
    "IOT01A"         -> return iot01a
    "MONSTICK"       -> return monstick
    "DEFAULT"        -> return defPlatform
    _ -> fail ("no such platform " ++ p)

-- allows overriding platform, default platforms MCU and few of its params
-- via default.conf
getPlatform :: Platform -> TOpts -> IO Platform
getPlatform defPlatform topts = do
    -- first platformParser
    p <- getConfig topts (platformParser defPlatform)
    -- then possible mcu override with default mcu from platform
    mcuName <- getConfig topts (mcuNameParser (platformMCUName p))

    -- lookup mcu and set in platformMCU
    nmcu <- matchMCU mcuName

    -- mcu options (ram, flash, splitmem..)
    nmcuCfg <- getConfig topts (mcuConfigParser nmcu)

    return $ p { platformMCU = Just nmcuCfg }

platformToConfig :: Platform -> STM32Config
platformToConfig Platform{platformMCU = (Just mcu), platformClocks = cc} = STM32Config mcu cc
platformToConfig _ = error "platformMCU not initialized"

-- for our helloworld applications
-- with type
-- app :: (e -> ClockConfig) -> (e -> Platform) -> Tower e ()
-- we can define this helper
buildHelloApp :: Platform
       -> ((Platform -> ClockConfig) -> (Platform -> Platform) -> Tower Platform ())
       -> IO ()
buildHelloApp def twrapp = compileTowerSTM32FreeRTOS
  platformToConfig (getPlatform def) $ twrapp platformClocks id



{-
 -- OLD platform(s)
 -- XXX: needs work
data ColoredLEDs =
  ColoredLEDs
    { redLED  :: LED
    , blueLED :: LED
    }

data UARTConfig =
  UARTConfig
    { uartPeriph :: UART
    , uartPins   :: UARTPins
    }

data SPIConfig =
  SPIConfig
    { spiPeriph :: SPI
    , spiPins   :: SPIPins
    }

data I2CConfig =
  I2CConfig
    { i2cPeriph :: I2C
    , i2cPins   :: I2CPins
    }

data CANConfig =
  CANConfig
    { canPeriph  :: CANPeriph
    , canRxPin   :: GPIOPin
    , canTxPin   :: GPIOPin
    , canFilters :: CANPeriphFilters
    }


patformParser :: ConfigParser Platform
platformParser = do
  p <- subsection "args" $ subsection "platform" string
  case map toUpper p of
--    "HELLO4DISCO"       -> result hello4disco
--    "HELLOF0"           -> result hellof0
--    "HELLO7DISCO"       -> result hello7disco
    "STMSTAMP"       -> result stmstamp
    _ -> fail ("no such platform " ++ p)

  where
  result platform = do
    --conf <- stm32ConfigParser (platform_stm32 platform)
    --return platform { testplatform_stm32 = conf }
    return platform

data Platform =
  Platform
    { platform_leds   :: ColoredLEDs
    , platform_uart   :: TestUART
    , platform_i2c    :: TestI2C
    , platform_can1   :: TestCAN
    , platform_rng    :: RNG
    , platform_mcu    :: MCU
    -- Used by SimpleBlink
    --, testplatform_ledpin :: GPIOPin
    }


stmstamp_uart1 = UARTConfig
    { uartPeriph = F765.usart1
    , uartPins = UARTPins
        { uartPinTx = F765.pinA9
        , uartPinRx = F765.pinA10
        , uartPinAF = F765.gpio_af_uart1
        }
    }



data TestDMA =
  TestDMA
    { testDMAUARTPeriph :: DMAUART
    , testDMAUARTPins   :: UARTPins
    }



module Hello.Tests.Platforms
  ( testPlatformParser
  , ColoredLEDs(..)
  , TestUART(..)
  , TestSPI(..)
  , TestI2C(..)
  , TestCAN(..)
  , TestDMA(..)
  , TestPlatform(..)
  , testplatform_clockconfig
  , hello4disco
  , hello7disco
  ) where

import Ivory.Tower.Config
import Data.Char (toUpper)

import Ivory.BSP.STM32.ClockConfig
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
import Ivory.BSP.STM32.Config

import Ivory.Tower.Base

testPlatformParser :: ConfigParser TestPlatform
testPlatformParser = do
  p <- subsection "args" $ subsection "platform" string
  case map toUpper p of
    "HELLO4DISCO"       -> result hello4disco
--    "HELLOF0"           -> result hellof0
    "HELLO7DISCO"       -> result hello7disco
    "STMSTAMP"       -> result stmstamp
    _ -> fail ("no such platform " ++ p)

  where
  result platform = do
    conf <- stm32ConfigParser (testplatform_stm32 platform)
    return platform { testplatform_stm32 = conf }

data TestPlatform =
  TestPlatform
    { testplatform_leds   :: ColoredLEDs
    , testplatform_uart   :: TestUART
    , testplatform_i2c    :: TestI2C
    , testplatform_can1   :: TestCAN
    , testplatform_rng    :: RNG
    , testplatform_stm32  :: STM32Config
    -- Used by SimpleBlink
    , testplatform_ledpin :: GPIOPin
    }

testplatform_clockconfig :: TestPlatform -> ClockConfig
testplatform_clockconfig = stm32config_clock . testplatform_stm32

-- HELLO4DISCO

hello4disco :: TestPlatform
hello4disco = TestPlatform
  { testplatform_leds = ColoredLEDs
      { redLED  = LED F405.pinD14 ActiveHigh
      , blueLED = LED F405.pinD15 ActiveHigh
      }
  , testplatform_uart = TestUART
    { testUARTPeriph = F405.uart2
    , testUARTPins = UARTPins
        { uartPinTx = F405.pinA2
        , uartPinRx = F405.pinA3
        , uartPinAF = F405.gpio_af_uart2
        }
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


-- HELLO7DISCO

hello7disco :: TestPlatform
hello7disco = TestPlatform
  { testplatform_leds = ColoredLEDs
      { redLED  = LED F405.pinA8 ActiveHigh
      , blueLED = LED F405.pinD15 ActiveHigh
      }
  , testplatform_uart = TestUART
    { testUARTPeriph = F405.uart2
    , testUARTPins = UARTPins
        { uartPinTx = F405.pinA2
        , uartPinRx = F405.pinA3
        , uartPinAF = F405.gpio_af_uart2
        }
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
  , testplatform_stm32 = f7Config 25

  -- SimpleBlink uses this for simplicity
  , testplatform_ledpin = F405.pinA8
  }

-- STMSTAMP

stmstamp_uart2 = TestUART
    { testUARTPeriph = F405.uart2
    , testUARTPins = UARTPins
        { uartPinTx = F405.pinD5
        , uartPinRx = F405.pinD6
        , uartPinAF = F405.gpio_af_uart2
        }
    }

stmstamp_uart4 = TestUART
    { testUARTPeriph = F405.uart4
    , testUARTPins = UARTPins
        { uartPinTx = F405.pinD0
        , uartPinRx = F405.pinD1
        , uartPinAF = F405.gpio_af_uart4
        }
    }

stmstamp_uart5 = TestUART
    { testUARTPeriph = F405.uart5
    , testUARTPins = UARTPins
        { uartPinTx = F405.pinB13
        , uartPinRx = F405.pinB12
        , uartPinAF = F405.gpio_af_uart5
        }
    }

stmstamp_spi1_pins = SPIPins
  { spiPinMiso = F405.pinD7 -- acually MOSI on STM32F7
  , spiPinMosi = F405.pinA6 -- acually MISO on STM32F7
  , spiPinSck  = F405.pinA5
  , spiPinAF   = F405.gpio_af_spi1
  }

stmstamp_spi1 = TestSPI
  { testSPIPeriph = F405.spi1
  , testSPIPins = stmstamp_spi1_pins
  }

stmstamp :: TestPlatform
stmstamp = TestPlatform
  { testplatform_leds = ColoredLEDs
      { redLED  = LED F405.pinD9 ActiveHigh
      , blueLED = LED F405.pinD10 ActiveHigh
      }
  , testplatform_uart = stmstamp_uart1
  , testplatform_i2c = TestI2C
      { testI2C = F405.i2c2
      , testI2CPins = I2CPins
        { i2cpins_sda = F405.pinB11
        , i2cpins_scl = F405.pinB10
        }
      }
--  , testplatform_can1 = TestCAN
--      { testCAN = F405.can3
--      , testCANRX = F405.pinA8
--      , testCANTX = F405.pinA15
--      , testCANFilters = F405.canFilters
--      }
  , testplatform_rng = F405.rng
  , testplatform_stm32 = f7Config 25

  -- SimpleBlink uses this for simplicity
  , testplatform_ledpin = F405.pinA8
  }

data Divs = Divs {
    div_hclk :: Integer
  , div_pclk1 :: Integer
  , div_pclk2 :: Integer
  }

externalXtalDivs :: Integer -> Integer -> Divs -> ClockConfig
externalXtalDivs xtal_mhz sysclk_mhz Divs{..} = ClockConfig
  { clockconfig_source = External (xtal_mhz * 1000 * 1000)
  , clockconfig_pll    = PLLFactor
      -- PLLM / HSE divider
      { pll_m = xtal_mhz
      -- PLL n - PLL multiplier
      , pll_n = sysclk_mhz * 2
      -- PLL p - PLL divider
      , pll_p = 2
      -- PLL q - PLL divider for PLLQCLK
      , pll_q = 7
      }
  -- ahb prescaler
  , clockconfig_hclk_divider = div_hclk
  -- apb1 prescaler
  , clockconfig_pclk1_divider = div_pclk1
  -- apb1 prescaler
  , clockconfig_pclk2_divider = div_pclk2
  }

-- STM32F746NG
f7Config :: Integer -> STM32Config
f7Config xtal_mhz = STM32Config
  { stm32config_processor  = STM32F405
  , stm32config_px4version = Nothing
  , stm32config_clock      = externalXtalDivs xtal_mhz 216 divs
  -- XXX: this is 192 in total (112+16+64)
  -- 64 is CCM (core coupled memory)
  -- + 4kb additional backup sram
--  , stm32config_sram       = 128 * 1024
  , stm32config_sram       = 220 * 1024
  }
  where
    divs = Divs
             { div_hclk = 1
             , div_pclk1 = 4
             , div_pclk2 = 2
             }

-}
