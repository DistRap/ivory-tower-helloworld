{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Hello.Tests.UARTToMAX7219 where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower

import Ivory.BSP.STM32.Driver.SPI
import Ivory.BSP.STM32.ClockConfig (ClockConfig)

import Ivory.Tower.Base
import Ivory.Tower.Base.UART.Types
import Ivory.Tower.Drivers.Display.MAX7219

import Hello.Tests.Platforms

app
  :: (e -> ClockConfig)
  -> (e -> Platform)
  -> Tower e ()
app tocc toPlatform = do
  Platform{..} <- fmap toPlatform getEnv

  blink (Milliseconds 1000) [platformRedLED]

  (sreq, _sready) <- spiTower tocc platformSPIDevs platformSPIPins

  (displayIn, _intensityIn) <- max7219 sreq (SPIDeviceHandle 0) (Proxy :: Proxy UARTBuffer)

  uartTowerDeps

  (ostream, istream) <- bufferedUartTower tocc platformUART platformUARTPins 115200 (Proxy :: Proxy UARTBuffer)
  displayBuffer ostream istream displayIn

-- circular string buffer with length 8 for MAX7219 7 segment display
displayBuffer :: IvoryString buf
              => ChanInput  ('Stored Uint8)
              -> ChanOutput ('Stored Uint8)
              -> ChanInput  (buf)
              -> Tower p ()
displayBuffer ostream istream disp = do
  monitor "displayBuffer" $ do
    initialized <- stateInit "initialized" (ival false)

    buf <- state "buf"

    handler systemInit "init" $ do
      o <- emitter ostream 32
      callback $ const $ do
        i <- deref initialized
        unless i $ do
          store initialized true
          puts o "ready\n"

    handler istream "istream" $ do
      d <- emitter disp 1
      o <- emitter ostream 32

      callbackV $ \input -> do
        putc o input -- echo to terminal
        pos <- deref (buf ~> stringLengthL)
        npos <- assign $ (pos ==? 8) ? (0, pos)

        store (buf ~> stringDataL ! toIx npos) input
        store (buf ~> stringLengthL) (npos + 1)
        emit d (constRef buf)
