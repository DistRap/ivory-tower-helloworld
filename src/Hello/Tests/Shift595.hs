{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Hello.Tests.Shift595 where

import Ivory.Language
import Ivory.Tower

import Hello.Tests.Platforms
import Ivory.BSP.STM32.ClockConfig

import Ivory.Tower.Base

import Ivory.Tower.Drivers.IO.Shift (io595)
import Ivory.BSP.STM32.Driver.SPI

app :: (e -> ClockConfig) -> (e -> Platform) -> Tower e ()
app tocc toPlatform = do
  Platform{..} <- fmap toPlatform getEnv

  (sreq, _sready) <- spiTower tocc platformSPIDevs platformSPIPins
  ioIn <- io595 sreq (SPIDeviceHandle 0)
  cOut <- periodicCounter (Milliseconds 1000) (0 :: Uint8)
  fwd cOut ioIn
