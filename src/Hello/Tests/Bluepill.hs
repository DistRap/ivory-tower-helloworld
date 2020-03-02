{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Hello.Tests.Bluepill where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.HW.Module

import Ivory.BSP.STM32.Peripheral.GPIO
import Ivory.BSP.STM32.Driver.SPI
import Ivory.BSP.STM32.ClockConfig (ClockConfig)

import Ivory.Tower.Base
import Ivory.Tower.Base.UART.Types

import Ivory.Tower.Drivers.Display.MAX7219

import Hello.Tests.Platforms

app :: (e -> ClockConfig) -> (e -> Platform) -> Tower e ()
app tocc toPlatform = do
  Platform{..} <- fmap toPlatform getEnv

  (sreq, _sready) <- spiTower tocc platformSPIDevs platformSPIPins

  (displayIn, intensityIn) <- max7219 sreq (SPIDeviceHandle 0) (Proxy :: Proxy UARTBuffer)

  uartTowerDeps

  (ostream, istream) <- bufferedUartTower tocc platformUART platformUARTPins 115200 (Proxy :: Proxy UARTBuffer)
  displayBufferN ostream istream displayIn

  triOut <- triangle (Milliseconds 50) 0 0xE
  fwd triOut intensityIn

-- acually triangle
triangle :: forall t n e . (Time t, IvoryEq n, IvoryStore n, IvoryZeroVal n, Num n)
    => t
    -> n
    -> n
    -> Tower e (ChanOutput ('Stored n))
triangle per bottom top = do
  p <- period per
  c <- channel

  monitor "saw" $ do
    val <- state "sawVal"
    dirUp <- stateInit "sawDir" (ival true)

    handler p "sawPer" $ do
      cE <- emitter (fst c) 1

      callback $ const $ do
        goingUp <- deref dirUp
        cval <- deref val
        store val $ goingUp ? (cval + 1, cval - 1)
        nval <- deref val
        cond_ [
            nval ==? top    ==> store dirUp false
          , nval ==? bottom ==> store dirUp true
          ]
        emit cE (constRef val)

  return (snd c)

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

-- buffer N characters then emit
displayBufferN :: IvoryString buf
              => ChanInput  ('Stored Uint8)
              -> ChanOutput ('Stored Uint8)
              -> ChanInput  (buf)
              -> Tower p ()
displayBufferN ostream istream disp = do
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
        ifte_ (input `isChar` '\r' .|| input `isChar` '\n')
          (do
            emit d (constRef buf)
            store (buf ~> stringLengthL) 0
          )
          (do
            pos <- deref (buf ~> stringLengthL)
            npos <- assign $ (pos ==? 8) ? (0, pos)

            store (buf ~> stringDataL ! toIx npos) input
            store (buf ~> stringLengthL) (npos + 1)
          )

