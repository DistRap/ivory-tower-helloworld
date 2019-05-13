
module Main where

import Ivory.Tower.Config
import Ivory.Tower.Options
import Ivory.OS.FreeRTOS.Tower.STM32

--import Hello.Tests.Platforms
import Hello.Tests.SimpleBlink (app)

import Port
import Pin
import Ivory.BSP.STM32.Peripheral.GPIO

lol = go (matchMCU "STM32L432KCU6")
  where
  go (Just x) = \_ -> x
  go Nothing = fail "Unable to match MCU!"


pin = GPIOFX pinB3

main :: IO ()
main = compileTowerSTM32FreeRTOS lol p $
  app (\_ -> pin)
  where
  p :: TOpts -> IO ()
  p x = return ()
  --p :: TOpts -> IO TestPlatform
  --p topts = getConfig topts testPlatformParser

