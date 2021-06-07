{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Hello.Tests.NucleoPLC where

import Ivory.Language
import Ivory.Tower

import Hello.Tests.Platforms
import Hello.Tests.Platforms.NucleoG474
import Ivory.BSP.STM32.ClockConfig

import Ivory.Tower.Base

import Ivory.BSP.STM32.Driver.SPI
import Ivory.Tower.Drivers.IO.PLC01A1

app :: (e -> ClockConfig) -> (e -> Platform) -> Tower e ()
app tocc toPlatform = do
  Platform{..} <- fmap toPlatform getEnv

  (sreq, sready) <- spiTower tocc platformSPIDevs platformSPIPins
  (plcIn, plcOut) <- plcTower sreq sready vniOutEnablePin

  return ()
  --p <- period (Seconds 1)
  --monitor "plctest" $ do
  --  cnt <- stateInit "counter" (ival (0 :: Uint8))
  --  handler p "plcPeriod" $ do
  --    plcOutE <- emitter plcOut 1
  --    callback $ const $ do
  --      emit plcOutE (constRef cnt)

  --ioIn <- io595 sreq (SPIDeviceHandle 0)
  --cOut <- periodicCounter (Milliseconds 1000) (0 :: Uint8)
  --fwd cOut ioIn
