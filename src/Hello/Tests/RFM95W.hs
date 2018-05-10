{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hello.Tests.UART where

import Data.Char (ord)

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower

import Ivory.BSP.STM32.ClockConfig

import Hello.Tests.Platforms

import Ivory.Tower.Base hiding (putc, puts)
import Ivory.Tower.Base.UART.Types

import Ivory.Tower.Drivers.Net.RFM95

app :: (e -> ClockConfig)
    -> (e -> ColoredLEDs)
    -> (e -> TestUART)
    -> (e -> TestSPI)
    -> Tower e ()
app tocc toleds touart testspi = do
  uartTowerDeps

  leds <- fmap toleds getEnv
  uart <- fmap touart getEnv
  spi  <- fmap tospi getEnv

  (ostream, istream) <- bufferedUartTower tocc (testUARTPeriph uart) (testUARTPins uart) 115200 (Proxy :: Proxy UARTBuffer)

  (ledctl_input, ledctl_output) <- channel
  echoPrompt "hello Ivory Tower world" ostream istream ledctl_input
  monitor "settableLED" $ ledController [redLED leds] ledctl_output

--------------------------------------------------------------------------------
echoPrompt :: String
           -> ChanInput  ('Stored Uint8)
           -> ChanOutput ('Stored Uint8)
           -> ChanInput  ('Stored IBool)
           -> Tower p ()
echoPrompt greeting ostream istream ledctl = do
  p <- period (Milliseconds 1)
  per <- period (Milliseconds 1000)

  let puts :: (GetAlloc eff ~ 'Scope cs)
           => Emitter ('Stored Uint8) -> String -> Ivory eff ()
      puts e str = mapM_ (\c -> putc e (fromIntegral (ord c))) str

      putc :: (GetAlloc eff ~ 'Scope cs)
           => Emitter ('Stored Uint8) -> Uint8 -> Ivory eff ()
      putc = emitV

  monitor "echoprompt" $ do
    initialized <- stateInit "initialized" (ival false)
    handler p "init" $ do
      o <- emitter ostream 32
      callback $ const $ do
        i <- deref initialized
        unless i $ do
          store initialized true
          puts o (greeting ++ "\n")
          puts o prompt

    handler per "blinkPeriod" $ do
      o <- emitter ostream 32
      l <- emitter ledctl 1

      callbackV $ const $ do
        puts o "a"

-- build a DRV8301 register read message
drv_msg_read :: DrvAddr -> Drv8301
drv_msg_read addr = drv_msg drv_read addr (fromRep 0)

-- build a DRV8301 control message
drv_msg :: DrvRW -> DrvAddr -> Bits 11 -> Drv8301
drv_msg rw addr c = fromRep $ withBits 0 $ do
  setField drv_rw rw
  setField drv_addr addr
  setField drv_data c

-- create an SPI request from device and Drv8301 data
spi_req :: (GetAlloc eff ~ 'Scope s)
        => SPIDeviceHandle
        -> Drv8301
        -> Ivory eff (ConstRef ('Stack s) ('Struct "spi_transaction_request"))
spi_req dev msg = fmap constRef $ local $ istruct
              [ tx_device .= ival dev
              , tx_buf    .= iarray [h msg, l msg]
              , tx_len    .= ival 2
              ]
  where l x = ival $ bitCast $ toRep x
        h x = ival $ bitCast $ (toRep x) `iShiftR` 8
