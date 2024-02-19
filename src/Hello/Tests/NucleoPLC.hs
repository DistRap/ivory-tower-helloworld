{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Hello.Tests.NucleoPLC where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower

import Hello.Tests.Platforms
import Hello.Tests.Platforms.NucleoG474
import Ivory.BSP.STM32.ClockConfig

import Ivory.Tower.Base.UART
import Ivory.Tower.Base.UART.Types

import Ivory.BSP.STM32.Driver.SPI
import Ivory.Tower.Drivers.IO.PLC01A1

app :: (e -> ClockConfig) -> (e -> Platform) -> Tower e ()
app tocc toPlatform = do
  Platform{..} <- fmap toPlatform getEnv

  (sreq, sready) <- spiTower tocc platformSPIDevs platformSPIPins
  (plcInputs, plcSetOutput, plcOutputStatus) <- plcTower sreq sready vniOutEnablePin

  uartTowerDeps
  (ostream, istream) <- bufferedUartTower tocc platformUART platformUARTPins 115200 (Proxy :: Proxy UARTBuffer)

  -- echo prompt doesn't control anything for now
  dummyToggle <- channel
  echoPrompt "hello plc world" ostream istream (fst dummyToggle)

  p <- period (Milliseconds 50)
  monitor "plctest" $ do
    cnt <- stateInit "counter" (ival (0 :: Uint8))
    outputs <- state "outputs"

    handler p "plcSetOutputsPeriod" $ do
      plcSetOutE <- emitter plcSetOutput 1
      callback $ const $ do
        pos <- deref cnt
        -- flip a bit at current position
        cur <- deref (outputs ! toIx pos)
        store (outputs ! toIx pos) (iNot cur)

        emit plcSetOutE (constRef outputs)
        ifte_
          (pos ==? 7)
          (store cnt 0)
          (cnt += 1)

    outputsStatus <- state "outputsStatus"

    handler plcOutputStatus "plcOutputStatus" $ do
      o <- emitter ostream 64
      callback $ \os -> do
        vniFault <- vniHasFaults os
        -- note that one fault is expected
        -- on power on, until DC/DC converter
        -- stabilizes
        when vniFault $
          puts o $ "VNI Fault\n"
        refCopy outputsStatus os

    inputs <- state "inputs"

    handler plcInputs "plcInputs" $ do
      o <- emitter ostream 64
      callback $ \ins -> do
        cltFault <- cltHasFaults ins
        when cltFault $
          puts o $ "CLT Fault\n"

        refCopy inputs ins
