-- | ADS1x1x I2C test
--
-- Periodically dumps ADC sampled values
-- to UART

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Hello.Tests.ADS where

import Ivory.Language
import Ivory.Tower

import Ivory.Tower.HAL.Bus.Interface

import Ivory.BSP.STM32.ClockConfig (ClockConfig)
import Ivory.BSP.STM32.Driver.I2C

import Ivory.Tower.Base
import Ivory.Tower.Base.UART.Types
import Ivory.Tower.Drivers.ADC.ADS1x1x
import Hello.Tests.Platforms

app :: (a -> ClockConfig)
    -> (a -> Platform)
    -> Tower a ()
app tocc toPlatform = do
  Platform{..} <- fmap toPlatform getEnv

  (i2cTransmit, ready) <- i2cTower tocc platformI2C platformI2CPins
  togIn <- ledToggle [platformRedLED]

  (BackpressureTransmit adsRequest adsResult) <-
    adsTower
      i2cTransmit
      ready
      adsDefaultAddr
      configADS101xAllChannels

  per <- period (Milliseconds 1000)
  fwd per togIn

  ms100 <- period (Milliseconds 100)
  fwd ms100 adsRequest

  uartTowerDeps
  (ostream, _istream) <-
    bufferedUartTower
      tocc
      platformUART
      platformUARTPins
      115200
      (Proxy :: Proxy UARTBuffer)

  monitor "dumpADS" $ do
    handler adsResult "adsResult" $ do
      o <- emitter ostream 64
      callback $ \adsSample -> do
        arrayMap $ \i -> do
          c <- deref (adsSample ! i)
          (str :: Ref ('Stack s) UARTBuffer) <- floatingToString c 4
          putIvoryString o (constRef str)
          puts o " "
        puts o "\r"
