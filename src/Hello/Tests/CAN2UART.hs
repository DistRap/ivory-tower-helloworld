{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hello.Tests.CAN2UART where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.HAL.Bus.CAN

import Ivory.BSP.STM32.ClockConfig
import Ivory.BSP.STM32.Driver.CAN
import Ivory.BSP.STM32.Peripheral.CAN.Filter

import Hello.Tests.Platforms
import Ivory.Tower.Base
import Ivory.Tower.Base.UART.Types

app :: (e -> ClockConfig)
    -> (e -> Platform)
    -> Tower e ()
app tocc toPlatform = do
  Platform{..} <- fmap toPlatform getEnv

  uartTowerDeps

  (canctl_input, canctl_output) <- channel

  (ostream, istream) <- bufferedUartTower tocc platformUART platformUARTPins 115200 (Proxy :: Proxy UARTBuffer)

  echoPrompt "hello world" ostream istream canctl_input

  (res, req, _, _) <- canTower tocc (canPeriph platformCAN) 1000000
    (canRxPin $ platformCAN) (canTxPin $ platformCAN)

  canSendTower req canctl_output

  monitor "simplecontroller" $ do
    handler systemInit "init" $ do
      callback $ const $ do
        let emptyID = CANFilterID32 (fromRep 0) (fromRep 0) False False
        canFilterInit (canPeriphFilters $ platformCAN)
                      [CANFilterBank CANFIFO0 CANFilterMask $ CANFilter32 emptyID emptyID]
                      [CANFilterBank CANFIFO1 CANFilterMask $ CANFilter32 emptyID emptyID]
        ledSetup $ platformRedLED

    received <- stateInit "can_received_count" (ival (0 :: Uint32))

    handler res "result" $ do
      o <- emitter ostream 64
      callback $ \msg -> do
        count <- deref received
        store received (count + 1)

        puts o "\n\rrcv\n\r"

        arrayMap $ \ix -> do
              val <- deref ((msg ~> can_message_buf) ! ix)
              putc o (48 + (castWith 0 $ fromIx $ ix))
              putc o val

        puts o "\n\r/rcv\n\r"

        ifte_ (count .& 1 ==? 1)
          (ledOff platformRedLED)
          (ledOn platformRedLED)

echoPrompt :: String
           -> ChanInput  ('Stored Uint8)
           -> ChanOutput ('Stored Uint8)
           -> ChanInput  ('Struct "can_message")
           -> Tower p ()
echoPrompt greeting ostream istream canctl = do
  towerDepends canDriverTypes
  towerModule  canDriverTypes
  p <- period (Milliseconds 1)


  monitor "echoprompt" $ do
    (incoming :: Ref 'Global UARTBuffer) <- state "incoming"
    initialized <- stateInit "initialized" (ival false)

    let push :: Uint8 -> Ivory eff ()
        push byte = do
          pos <- deref (incoming ~> stringLengthL)
          when (pos <? arrayLen (incoming ~> stringDataL)) $ do
            store (incoming ~> stringDataL ! toIx pos) byte
            store (incoming ~> stringLengthL) (pos + 1)

    handler p "init" $ do
      o <- emitter ostream 32
      callback $ const $ do
        i <- deref initialized
        unless i $ do
          store initialized true
          puts o (greeting ++ "\n")
          puts o prompt

    handler istream "istream" $ do
      c <- emitter canctl 1
      o <- emitter ostream 32
      callbackV $ \input -> do
        putc o input -- echo to terminal
        push input
        pos <- deref (incoming ~> stringLengthL)
        when (pos ==? 8) $ do
          let msgid = standardCANID (fromRep 0x7FF) (boolToBit false)
          r <- local $ istruct
            [ can_message_id  .= ival msgid
            , can_message_len .= ival 8
            ]

          -- arrayCopy: to from offset len
          arrayCopy (r ~> can_message_buf) (constRef (incoming ~> stringDataL)) 0 8
          emit c (constRef r)
          store (incoming ~> stringLengthL) 0

  where prompt = "tower> "
