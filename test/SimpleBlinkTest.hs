
module Main where

import Ivory.Tower.Config
import Ivory.Tower.Options
import Ivory.OS.FreeRTOS.Tower.STM32

import Hello.Tests.Platforms
import Hello.Tests.SimpleBlink (app)

import Ivory.BSP.STM32.Peripheral.GPIO
import Ivory.BSP.STM32.ClockConfig

import Ivory.BSP.STM32L475.GPIO

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
  }

main :: IO ()
main = compileTowerSTM32FreeRTOS platformConf p $
  app (confClocks . platformConf)
      id
  where
  p :: TOpts -> IO Platform
  p topts = do
    nmcu@(name, mcu) <- matchMCU "STM32L475VGT6"
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

