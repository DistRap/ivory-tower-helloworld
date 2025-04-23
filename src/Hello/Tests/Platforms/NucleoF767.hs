
module Hello.Tests.Platforms.NucleoF767 where

import Hello.Tests.Platforms.Types

import Ivory.BSP.STM32.Peripheral.GPIO
import Ivory.BSP.STM32.ClockConfig

import Ivory.BSP.STM32F767
import Ivory.BSP.STM32F767.Clock

import Ivory.BSP.STM32.Peripheral.ETH
import Ivory.BSP.STM32.Peripheral.I2C
import Ivory.BSP.STM32.Peripheral.SPI
import Ivory.BSP.STM32.Peripheral.UART

import qualified Ivory.Tower.Base as Base

-- | 8Mhz External crystal (HSE) clock configuration
--
-- No crystal soldered by default
--
-- Recommended is 8 MHz, 8 pF, 20 ppm
-- NX3225GD-8.000M-EXS00A-CG04874 manufactured by NIHON DEMPA KOGYO CO.
-- with 4.3 pF caps
nucleo_f767_hse_cc :: ClockConfig
nucleo_f767_hse_cc =
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
nucleo_f767_hsi_cc :: ClockConfig
nucleo_f767_hsi_cc =
  ClockConfig {
    clockconfig_source = clockHSI
  , clockconfig_pll    = PLLFactorMNP
    { pll_m = 8
    , pll_n = 192
    , pll_p = 2
    , pll_q = 8
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

usart3Pins :: UARTPins
usart3Pins = UARTPins
  { uartPinTx = pinD8
  , uartPinRx = pinD9
  }

spiPins :: SPIPins
spiPins = SPIPins
    { spiPinMiso = pinA6
    , spiPinMosi = pinA7
    , spiPinSck  = pinA5
    }

spiCSPin :: GPIOPin
spiCSPin = pinD14

ethPins :: ETHPins
ethPins = ETHPins
  { ethPins_mdc    = pinC1
  , ethPins_mdio   = pinA2
  , ethPins_refclk = pinA1
  , ethPins_crs    = pinA7
  , ethPins_txen   = pinG11
  , ethPins_txd0   = pinG13
  , ethPins_txd1   = pinB13
  , ethPins_rxd0   = pinC4
  , ethPins_rxd1   = pinC5
  }

ethConfig :: ETHConfig
ethConfig = ETHConfig
  { ethConfigPeriph = eth
  , ethConfigPins   = ethPins
  }

nucleo_f767 :: Platform
nucleo_f767 = Platform {
    platformClocks   = nucleo_f767_hsi_cc
  , platformMCU      = Nothing
  , platformMCUName  = "STM32F767ZIT6"
  , platformPin      = pinB14
  , platformPinIn    = pinC13
  , platformRedLED   = Base.LED pinB14 Base.ActiveHigh
  , platformGreenLED = Base.LED pinB0 Base.ActiveHigh
  , platformSPI      = spi1
  , platformSPIPins  = spiPins
  , platformSPIDevs  = []
  , platformI2C      = i2c1
  , platformI2CPins  = i2c1Pins
  , platformUART     = usart3
  , platformUARTPins = usart3Pins
  , platformDMAUART  = dmausart3
  , platformCAN      = undef "CAN"
  , platformIWDG     = iwdg
  , platformETH      = ethConfig
  , platformEXTI     = exti
  , platformRNG      = rng
  }
