
module Hello.Tests.Platforms.NucleoG474 where

import Hello.Tests.Platforms.Types

import Ivory.BSP.STM32.Peripheral.GPIO
import Ivory.BSP.STM32.ClockConfig

import Ivory.BSP.STM32G474
import Ivory.BSP.STM32G474.Clock

import Ivory.BSP.STM32.Peripheral.I2C
import Ivory.BSP.STM32.Peripheral.SPI
import Ivory.BSP.STM32.Peripheral.UART

import qualified Ivory.Tower.Base as Base
import Ivory.Tower.Drivers.SPIDevice (genericSPIDev)


nucleo_g474_hse_cc :: ClockConfig
nucleo_g474_hse_cc =
  ClockConfig {
    clockconfig_source = HSE $ 24 * 1000 * 1000
  , clockconfig_pll    = PLLFactorMNR
    { pll_mnr_m = 6
    , pll_mnr_n = 85
    , pll_mnr_p = 7
    , pll_mnr_q = 2
    , pll_mnr_r = 2
    }
  , clockconfig_hclk_divider = 1
  , clockconfig_pclk1_divider = 1
  , clockconfig_pclk2_divider = 1
  }

nucleo_g474_hsi_cc :: ClockConfig
nucleo_g474_hsi_cc =
  ClockConfig {
    clockconfig_source = clockHSI
  , clockconfig_pll    = PLLFactorMNR
    { pll_mnr_m = 4
    , pll_mnr_n = 85
    , pll_mnr_p = 2
    , pll_mnr_q = 2
    , pll_mnr_r = 2
    }
  , clockconfig_hclk_divider = 1
  , clockconfig_pclk1_divider = 1
  , clockconfig_pclk2_divider = 1
  }

i2c1Pins :: I2CPins
i2c1Pins = I2CPins {
    i2cpins_sda = pinB9
  , i2cpins_scl = pinB8
  }

usart1Pins :: UARTPins
usart1Pins = UARTPins
  { uartPinTx = pinC4
  , uartPinRx = pinC5
  }

lpuart1Pins :: UARTPins
lpuart1Pins = UARTPins
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

nucleo_g474 :: Platform
nucleo_g474 = Platform {
    platformClocks   = nucleo_g474_hse_cc
  , platformMCU      = Nothing
  , platformMCUName  = "STM32G474RET6"
  , platformPin      = pinA5
  , platformPinIn    = pinC13 -- blue button
  -- only green led available, shared with SPI SCK
  , platformRedLED   = Base.LED pinA5 Base.ActiveHigh
  , platformGreenLED = Base.LED pinA5 Base.ActiveHigh
  , platformSPI      = spi1
  , platformSPIPins  = spiPins
  , platformSPIDevs  = [
      clt
    , vni
    ]
  , platformI2C      = i2c1
  , platformI2CPins  = i2c1Pins
  , platformUART     = lpuart1
  , platformUARTPins = lpuart1Pins
  , platformDMAUART  = undef "DMAUART"
  , platformCAN      = undef "CAN"
  , platformIWDG     = iwdg
  , platformEXTI     = exti
  , platformRNG      = rng
  }

-- CS1 - D9 - C7
clt :: SPIDevice
clt = genericSPIDev "clt" spi1 pinC7

-- CS2 - D10 - B6
vni :: SPIDevice
vni = genericSPIDev "vni" spi1 pinB6

vniOutEnablePin :: GPIOPin
vniOutEnablePin = pinB10
