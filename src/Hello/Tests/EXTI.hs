{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Hello.Tests.EXTI where

import Ivory.Language
import Ivory.Tower

import Hello.Tests.Platforms
import Ivory.BSP.STM32.ClockConfig

import Ivory.Tower.Base
import Ivory.Tower.Base.UART.Types

import Ivory.BSP.STM32.Peripheral.GPIO
import Ivory.BSP.STM32.Peripheral.EXTI
import Ivory.BSP.STM32.Driver.EXTI

import Control.Monad (forM_)

app :: (e -> ClockConfig) -> (e -> Platform) -> Tower e ()
app tocc toPlatform = do
  Platform{..} <- fmap toPlatform getEnv

  out@[chan0] <- extiTower platformEXTI [
      EXTIPin platformPinIn Both PullUp -- typically a button of a devboard
      -- we could add more pins like:
--    , EXTIPin pinB12 Rising PullUp
--    , EXTIPin pinB13 Falling PullUp
    ]

  togIn <- ledToggle [platformRedLED]
  fwd chan0 togIn

  uartTowerDeps
  (ostream, istream) <- bufferedUartTower tocc platformUART platformUARTPins 115200 (Proxy :: Proxy UARTBuffer)
  echoPrompt "hello exti world" ostream istream togIn

  debugChans ostream out

debugChans :: (IvoryArea a
             , IvoryZero a)
           => ChanInput ('Stored Uint8)
           -> [ChanOutput a]
           -> Tower e ()
debugChans ostream chans = do
  monitor "dbgChans" $ do
    forM_ (zip chans [(0 :: Int)..]) $ \(c, i) -> do
      handler c "dbgChansHandler" $ do
        o <- emitter ostream 64
        callback $ const $ do
          puts o $ "Triggered " ++ show i
