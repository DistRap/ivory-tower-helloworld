
module Main where

import Ivory.Tower.Config
import Ivory.Tower.Options
import Ivory.OS.FreeRTOS.Tower.STM32

--import Hello.Tests.Platforms
import Hello.Tests.SimpleBlink (app)

import Port
import Pin
import Ivory.BSP.STM32.Peripheral.GPIO
import Ivory.BSP.STM32.ClockConfig

lol = go (matchMCU "STM32L432KCU6")
  where
  go (Just x) = \_ -> x
  go Nothing = fail "Unable to match MCU!"


pin = GPIOFX pinB3
cc = const $ ClockConfig { clockconfig_source = Internal
                         , clockconfig_pll    = PLLFactor
                           { pll_m = 16 -- arbitrary, not used
                           , pll_n = 16 * p -- arbitrary, not used
                           , pll_p = p
                           , pll_q = 7
                           }
                         , clockconfig_hclk_divider = 1
                         , clockconfig_pclk1_divider = 1
                         , clockconfig_pclk2_divider = 1
                         }
     where p = 2

main :: IO ()
main = compileTowerSTM32FreeRTOS lol cc p $
  app (\_ -> pin) (\_ -> error "needs ClockConfig")
  where
  p :: TOpts -> IO ()
  p x = return ()
  --p :: TOpts -> IO TestPlatform
  --p topts = getConfig topts testPlatformParser

