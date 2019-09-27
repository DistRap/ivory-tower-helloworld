
module Main where

import Ivory.Tower.Config
import Ivory.Tower.Options
import Ivory.OS.FreeRTOS.Tower.STM32

import Hello.Tests.Platforms
import Hello.Tests.IOT01A (app)

import Ivory.BSP.STM32.Peripheral.GPIO
import Ivory.BSP.STM32.ClockConfig

import Ivory.BSP.STM32L475.GPIO
import Ivory.BSP.STM32L475.SPI
import Ivory.BSP.STM32L475.I2C
import Ivory.BSP.STM32L475.UART
import Ivory.BSP.STM32.Peripheral.I2C

-- XXX
import Ivory.BSP.STM32.Peripheral.SPIv2.Peripheral
import Ivory.BSP.STM32.Peripheral.SPI
import Ivory.BSP.STM32.Peripheral.SPI.Pins

import Ivory.BSP.STM32.Peripheral.UART.Pins
import Ivory.BSP.STM32.Peripheral.UART

i2cPins = I2CPins { i2cpins_sda = pinB11
                  , i2cpins_scl = pinB10 }

spiPins = SPIPins
    { spiPinMiso = pinD3
    , spiPinMosi = pinD4
    , spiPinSck  = pinD1
    , spiPinAF   = gpio_af5 -- gpio_af_spi2 -- XXX
    }

devs = [ max7219dev spi2 ]

max7219dev :: SPI -> SPIDevice
max7219dev spi =  SPIDevice
    { spiDevPeripheral    = spi
    , spiDevCSPin         = pinD5
    , spiDevClockHz       = 500000
    , spiDevCSActive      = ActiveLow
    , spiDevClockPolarity = ClockPolarityLow
    , spiDevClockPhase    = ClockPhase1
    , spiDevBitOrder      = MSBFirst
    , spiDevName          = "max7219"
    }

uartPins = UARTPins
  { uartPinTx = pinB6
  , uartPinRx = pinB7
  , uartPinAF = gpio_af7
  }

pin = pinA5

cc = ClockConfig { clockconfig_source = MSI $ 16 * 1000 * 1000 -- HSI16
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

plat = Platform {
  --  platformConf :: STM32Config
    platformPin      = pin
  , platformSPI      = spi2
  , platformSPIPins  = spiPins
  , platformSPIDevs  = devs
  , platformI2C      = i2c2
  , platformI2CPins  = i2cPins
  , platformUART     = usart1
  , platformUARTPins = uartPins
  }


main :: IO ()
main = compileTowerSTM32FreeRTOS platformConf p $
  app (confClocks . platformConf)
      id
  where
  p :: TOpts -> IO Platform
  p topts = do
    nmcu@(name, mcu) <- matchMCU "STM32L475VGT6"
    -- XXX: add parsers
    --getDefaultCC mcu
    --getConfig topts (platformParser nmcu cc)
    putStrLn "YE"
    return $
      plat {
        platformConf = STM32Config {
            confMCU = nmcu
          , confClocks = cc
        }
      }
  --p :: TOpts -> IO TestPlatform
  --p topts = getConfig topts testPlatformParser

