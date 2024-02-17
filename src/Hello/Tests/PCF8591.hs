{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE BinaryLiterals #-}

module Hello.Tests.PCF8591 where

import Ivory.Language
import Ivory.Tower

import Ivory.Tower.HAL.Bus.Interface

import Ivory.BSP.STM32.Driver.I2C
import Ivory.BSP.STM32.Driver.SPI (spiTower, SPIDeviceHandle(..))
import Ivory.BSP.STM32.ClockConfig (ClockConfig)

import Ivory.Tower.Base.UART
import Ivory.Tower.Base.UART.Types

import Ivory.Tower.Drivers.ADC.PCF8591
import Ivory.Tower.Drivers.Display.MAX7219

import Hello.Tests.Platforms

app :: (e -> ClockConfig) -> (e -> Platform) -> Tower e ()
app tocc toPlatform = do
  Platform{..} <- fmap toPlatform getEnv

  -- 100khz standard I2C
  (i2cTransmit, i2cReady) <- i2cTowerStandard tocc platformI2C platformI2CPins
  (BackpressureTransmit adcReq adcRes, dac) <- pcf8591Tower i2cTransmit i2cReady pcf8591DefaultAddr

  (sreq, _sready) <- spiTower tocc platformSPIDevs platformSPIPins

  (displayIn, _intensityIn) <- max7219 sreq (SPIDeviceHandle 0) (Proxy :: Proxy UARTBuffer)

  -- required since we use UARTBuffer for string formatting/display output
  uartTowerDeps

  p1 <- period (Milliseconds 1)
  p100 <- period (Milliseconds 100)

  monitor "th" $ do
    dbg <- state "dbg"
    c <- state "c"

    handler p1 "dac" $ do
      e <- emitter dac 1
      callback $ const $ emit e (constRef c)

    handler p100 "adcRequest" $ do
      e <- emitter adcReq 1
      callback $ emit e

    handler adcRes "adcResult" $ do
      callback $ \x -> do
        -- store channel 4 value
        v <- deref (x ! 3)
        store c v

    handler p100 "displayAdcVal" $ do
      disp <- emitter displayIn 1

      callback $ const $ do
        c' <- deref c
        (str :: Ref ('Stack s) UARTBuffer) <- uint32ToString $ safeCast c'
        refCopy dbg str
        emit disp (constRef str)
        return ()
