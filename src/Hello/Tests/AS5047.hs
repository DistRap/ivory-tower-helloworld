{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Hello.Tests.AS5047 where

import Ivory.Tower

import Ivory.BSP.STM32.Driver.SPI
import Ivory.BSP.STM32.ClockConfig (ClockConfig)

import Ivory.Tower.Base (blink)
import Ivory.Tower.Drivers.Encoder.AS5047

import Hello.Tests.Platforms

app :: (e -> ClockConfig) -> (e -> Platform) -> Tower e ()
app tocc toPlatform = do
  Platform{..} <- fmap toPlatform getEnv

  blink (Milliseconds 1000) [platformRedLED]

  (sreq, sready) <- spiTower tocc platformSPIDevs platformSPIPins

  as5047 sreq (SPIDeviceHandle 0) sready
