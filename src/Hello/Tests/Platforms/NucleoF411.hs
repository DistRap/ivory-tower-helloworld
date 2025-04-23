
module Hello.Tests.Platforms.NucleoF411 where

import Hello.Tests.Platforms.Types

import Ivory.BSP.STM32.Peripheral.GPIO
import Ivory.BSP.STM32.ClockConfig

import Ivory.BSP.STM32F411
import Ivory.BSP.STM32F411.Clock

import Ivory.BSP.STM32.Peripheral.I2C
import Ivory.BSP.STM32.Peripheral.SPI
import Ivory.BSP.STM32.Peripheral.UART

import qualified Ivory.Tower.Base as Base

-- | 8Mhz External crystal (HSE) clock configuration
--
-- No crystal soldered by default
--
-- Recommended is 8 MHz, 16 pF, 20 ppm, DIP
-- 9SL8000016AFXHF0 manufactured by Hong Kong X'tals Limited
-- with 20pF caps
nucleo_f411_hse_cc :: ClockConfig
nucleo_f411_hse_cc =
  ClockConfig {
    clockconfig_source = HSE $ 8 * 1000 * 1000
  , clockconfig_pll    = PLLFactorMNP
    { pll_m = 4
    , pll_n = 96
    , pll_p = 2
    , pll_q = 4
    }
  , clockconfig_hclk_divider = 1
  , clockconfig_pclk1_divider = 2
  , clockconfig_pclk2_divider = 1
  }

-- | 16Mhz Internal oscillator (HSI) clock configuration
-- configured to 100Mhz SYSCLK
nucleo_f411_hsi_cc :: ClockConfig
nucleo_f411_hsi_cc =
  ClockConfig {
    clockconfig_source = clockHSI
  , clockconfig_pll    = PLLFactorMNP
    { pll_m = 8
    , pll_n = 100
    , pll_p = 2
    , pll_q = 4
    }
  , clockconfig_hclk_divider = 1
  , clockconfig_pclk1_divider = 2
  , clockconfig_pclk2_divider = 1
  }

i2c1Pins :: I2CPins
i2c1Pins = I2CPins {
    i2cpins_sda = pinB9
  , i2cpins_scl = pinB8
  }

usart2Pins :: UARTPins
usart2Pins = UARTPins
  { uartPinTx = pinA2
  , uartPinRx = pinA3
  }

spiPins :: SPIPins
spiPins = SPIPins
    { spiPinMiso = pinA6
    , spiPinMosi = pinA7
    , spiPinSck  = pinA5
    }

spiCSPin :: GPIOPin
spiCSPin = pinB6

-- Warning: pinA5 (User LED) is shared with SPI clock when
-- used via duino header, don't accidentaly use LED or platformPin with SPI

nucleo_f411 :: Platform
nucleo_f411 = Platform {
    platformClocks   = nucleo_f411_hsi_cc
  , platformMCU      = Nothing
  , platformMCUName  = "STM32F411RET6"
  , platformPin      = pinA5
  , platformPinIn    = pinC13 -- blue button
  -- only green led available, shared with SPI SCK
  , platformRedLED   = Base.LED pinA5 Base.ActiveHigh
  , platformGreenLED = Base.LED pinA5 Base.ActiveHigh
  , platformSPI      = spi1
  , platformSPIPins  = spiPins
  , platformSPIDevs  = []
  , platformI2C      = i2c1
  , platformI2CPins  = i2c1Pins
  , platformUART     = usart2
  , platformUARTPins = usart2Pins
  , platformDMAUART  = dmausart2
  , platformCAN      = undef "CAN"
  , platformIWDG     = iwdg
  , platformETH      = undef "ETH"
  , platformEXTI     = exti
  , platformRNG      = undef "RNG"
  }
