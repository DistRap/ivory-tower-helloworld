{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hello.Tests.UARTBridge where

import Ivory.Language
import Ivory.Tower

import Ivory.BSP.STM32.ClockConfig
import Ivory.BSP.STM32L431 (usart1)

import Hello.Tests.Platforms
import qualified Hello.Tests.Platforms.Monstick as Monstick

import Ivory.Tower.Base.UART
import Ivory.Tower.Base.UART.Types

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

  uartBridge ostream istream ostream' istream'
