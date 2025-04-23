{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Hello.Tests.ETHArpReq where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.HAL.Bus.Interface (BackpressureTransmit(..))

import Hello.Tests.Platforms

import Ivory.BSP.STM32.ClockConfig

import Ivory.Tower.Base (blink, fwd, ledToggle)

import Ivory.BSP.STM32.Driver.ETH
import Ivory.BSP.STM32.Driver.ETH.FrameBuffer

app :: (e -> ClockConfig) -> (e -> Platform) -> Tower e ()
app tocc toPlatform = do
  Platform{..} <- fmap toPlatform getEnv

  blink (Milliseconds 1000) [platformRedLED]

  ethTowerDeps

  (ready, BackpressureTransmit txReq _txDone, rxDone) <-
    ethTower
      tocc
      platformETH
      (Proxy @FrameBuffer)

  togIn <- ledToggle [platformGreenLED]
  fwd ready togIn

  txPer <- period (Seconds 1)
  monitor "eth" $ do
    canSend <- state "canSend"
    txCounter <- stateInit "txCounter" (ival (0 :: Uint8))

    lastRx <- state "lastRx"
    rxCounter <- stateInit "rxCounter" (ival (0 :: Uint8))

    handler ready "senderReady" $ do
      callback $ const $ store canSend true

    handler txPer "txPeriod" $ do
      txE <- emitter txReq 1
      callback $ const $ do
        ok <- deref canSend
        when ok $ do
          arpReq <- local $ izero
          arpPacket arpReq
          txCounter += 1
          emit txE (constRef arpReq)

    handler rxDone "rx" $ do
      callback $ \rx -> do
        refCopy lastRx rx
        rxCounter += 1

-- | Craft arp request packet
-- for 192.168.192.1 target IP
arpPacket
  :: IvoryString frameBuffer
  => Ref s frameBuffer
  -> Ivory eff ()
arpPacket tframe = do
  -- 0-6 dest mac
  -- 7-12 src mac
  -- 13 14 eth type
  -- 15+ payload
  -- DST FF:FF:FF:FF:FF:FF
  -- SRC 32:54:11:7e:5e:20
  store (tframe ~> stringDataL !  0) 0xFF
  store (tframe ~> stringDataL !  1) 0xFF
  store (tframe ~> stringDataL !  2) 0xFF
  store (tframe ~> stringDataL !  3) 0xFF
  store (tframe ~> stringDataL !  4) 0xFF
  store (tframe ~> stringDataL !  5) 0xFF

  store (tframe ~> stringDataL !  6) 0x32
  store (tframe ~> stringDataL !  7) 0x54
  store (tframe ~> stringDataL !  8) 0x11
  store (tframe ~> stringDataL !  9) 0x7e
  store (tframe ~> stringDataL ! 10) 0x5e
  store (tframe ~> stringDataL ! 11) 0x23
  -- https://en.wikipedia.org/wiki/EtherType#Values
  -- arp
  store (tframe ~> stringDataL ! 12) 0x08
  store (tframe ~> stringDataL ! 13) 0x06

  -- arp payload
  -- hw type ethernet 0x0001
  store (tframe ~> stringDataL ! 14) 0x00
  store (tframe ~> stringDataL ! 15) 0x01
  -- pro(tocol) type IP 0x0800
  store (tframe ~> stringDataL ! 16) 0x08
  store (tframe ~> stringDataL ! 17) 0x00
  -- mac length
  store (tframe ~> stringDataL ! 18) 0x06
  -- ip length
  store (tframe ~> stringDataL ! 19) 0x04
  -- oper(ation) arp request 0x0001
  store (tframe ~> stringDataL ! 20) 0x00
  store (tframe ~> stringDataL ! 21) 0x01
  -- src mac
  store (tframe ~> stringDataL ! 22) 0x32
  store (tframe ~> stringDataL ! 23) 0x54
  store (tframe ~> stringDataL ! 24) 0x11
  store (tframe ~> stringDataL ! 25) 0x7e
  store (tframe ~> stringDataL ! 26) 0x5e
  store (tframe ~> stringDataL ! 27) 0x23
  -- sender IP 192.168.192.2
  store (tframe ~> stringDataL ! 28) 192
  store (tframe ~> stringDataL ! 29) 168
  store (tframe ~> stringDataL ! 30) 192
  store (tframe ~> stringDataL ! 31) 2
  -- target mac
  store (tframe ~> stringDataL ! 32) 0x00
  store (tframe ~> stringDataL ! 33) 0x00
  store (tframe ~> stringDataL ! 34) 0x00
  store (tframe ~> stringDataL ! 35) 0x00
  store (tframe ~> stringDataL ! 36) 0x00
  store (tframe ~> stringDataL ! 37) 0x00
  -- target IP 192.168.192.1
  store (tframe ~> stringDataL ! 38) 192
  store (tframe ~> stringDataL ! 39) 168
  store (tframe ~> stringDataL ! 40) 192
  store (tframe ~> stringDataL ! 41) 1

  store (tframe ~> stringLengthL) 42
