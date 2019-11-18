{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hello.Tests.Monstick.RN2483 where

import Data.Char (ord)

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.HW.Module

import Ivory.BSP.STM32.ClockConfig
import Ivory.BSP.STM32.Peripheral.GPIO
import Ivory.BSP.STM32L431 (usart1, pinC12)


import Hello.Tests.Platforms
import qualified Hello.Tests.Platforms.Monstick as Monstick

import Ivory.Tower.Base hiding (putc, puts)
import Ivory.Tower.Base.UART
import Ivory.Tower.Base.UART.Types
import Ivory.Tower.Base.Util

import Ivory.Tower.Drivers.Net.RN2483

resetPin :: GPIOPin
resetPin = pinC12

app :: (e -> ClockConfig)
    -> (e -> Platform)
    -> Tower e ()
app tocc toPlatform = do
  uartTowerDeps

  Platform{..} <- fmap toPlatform getEnv

  -- debug
  (ostream, istream) <- bufferedUartTower tocc
    platformUART platformUARTPins
    115200 (Proxy :: Proxy UARTBuffer)

  -- RN2483
  (ostream', istream') <- bufferedUartTower tocc
    usart1 Monstick.usart1Pins
    57600 (Proxy :: Proxy UARTBuffer)


  -- pretty istream from RN2483
  istreamf' <- replaceChar '\r' '\n' istream' >>= dropEvery 2 (`isChar` '\n')
  uartBridge ostream istream ostream' istreamf'

  -- prepend RN2483 commands with >
  dbgstream <- dbgPrepend '\n' "> " ostream

  -- create merged input channel that feeds both
  -- ostream' and dbgstream
  merged <- ostream' `mergeInputs` dbgstream

  istreamCRLF <- crlfBuffer istream' (Proxy :: Proxy UARTBuffer)
  (rdy, acc, txdone, cmd) <- rn2483 merged istreamCRLF systemInit resetPin (Proxy :: Proxy UARTBuffer)

  monitor "rnleds" $ do
    monitorModuleDef $ hw_moduledef

    handler systemInit "rnInit" $ do
      callback $ const $ do
        ledSetup $ platformRedLED
        ledSetup $ platformGreenLED

    handler rdy "rnRdy" $ do
      callback $ const $ do
        ledOn $ platformRedLED

    handler acc "rnAccepted" $ do
      cmdE <- emitter cmd 1
      callbackV $ \v -> do
        when v $ do
          ledOn $ platformGreenLED
          x <- local $ stringInit "481337"
          emit cmdE (constRef x)

        unless v $ ledOff platformRedLED

    handler txdone "rnTXdone" $ do
      callback $ const $ do
        ledOff $ platformGreenLED
