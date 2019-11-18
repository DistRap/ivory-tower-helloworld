{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Hello.Tests.Monstick where

import Data.Char (ord)
import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.HW.Module

import Ivory.Tower.HAL.Bus.Interface

import Ivory.BSP.STM32.Peripheral.GPIO
import Ivory.BSP.STM32.Driver.I2C

import Ivory.BSP.STM32.ClockConfig (ClockConfig)

import Ivory.Tower.Base.LED (ledToggle)
import Ivory.Tower.Base.UART
import Ivory.Tower.Base.UART.Types
import Ivory.Tower.Base.Util (fwd, sampler)
import Hello.Tests.Platforms

import Ivory.Tower.Drivers.Temperature.SI7006
import Ivory.Tower.Drivers.Temperature.Types

app :: (e -> ClockConfig)
    -> (e -> Platform)
    -> Tower e ()
app tocc toPlatform = do
  Platform{..} <- fmap toPlatform getEnv

  (i2cTransmit, i2cReady) <- i2cTower tocc platformI2C platformI2CPins

  uartTowerDeps
  (ostream, istream) <- bufferedUartTower tocc
    platformUART platformUARTPins
    115200 (Proxy :: Proxy UARTBuffer)

  togIn <- ledToggle [platformRedLED]
  (BackpressureTransmit trigIn thOut, heatIn) <- si7006TowerHeater i2cTransmit i2cReady si7006DefaultAddr
  fwd thOut togIn
  sampler "th" thOut

  per <- period (Milliseconds 1000)
  fwd per trigIn
  monitor "heaterControl" $ do
    handler per "hPer" $ do
      hE <- emitter heatIn 1
      callback $ const $ do
        emitV hE 0

  monitor "logger" $ do
    handler thOut "perSample" $ do
      o <- emitter ostream 32
      callback $ \sample -> do
        t <- sample ~>* sample_th_temperature
        h <- sample ~>* sample_th_humidity

        (strT :: Ref ('Stack s) UARTBuffer) <- floatingToString t 4
        (strH :: Ref ('Stack s) UARTBuffer) <- floatingToString h 4

        puts o "T "
        putIvoryString o strT
        puts o " H "
        putIvoryString o strH
        puts o "\n"

  return ()

