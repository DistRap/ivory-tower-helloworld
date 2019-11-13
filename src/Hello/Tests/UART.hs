{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
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

app :: (e -> ClockConfig)
    -> (e -> Platform)
    -> Tower e ()
app tocc toPlatform = do
  uartTowerDeps

  Platform{..} <- fmap toPlatform getEnv

  (ostream, istream) <- bufferedUartTower tocc platformUART platformUARTPins 115200 (Proxy :: Proxy UARTBuffer)

  (ledctl_input, ledctl_output) <- channel
  echoPrompt "hello Ivory Tower world" ostream istream ledctl_input
  monitor "settableLED" $ ledController [platformRedLED, platformGreenLED] ledctl_output

--------------------------------------------------------------------------------
echoPrompt :: String
           -> ChanInput  ('Stored Uint8)
           -> ChanOutput ('Stored Uint8)
           -> ChanInput  ('Stored IBool)
           -> Tower p ()
echoPrompt greeting ostream istream ledctl = do
  p <- period (Milliseconds 1)

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

    handler istream "istream" $ do
      l <- emitter ledctl 1
      o <- emitter ostream 32
      callbackV $ \input -> do
        putc o input -- echo to terminal
        let testChar = (input `isChar`)
        cond_
          [ testChar '1'  ==> puts o "\r\noutput on\r\n"  >> emitV l true
          , testChar '2'  ==> puts o "\r\noutput off\r\n" >> emitV l false
          , testChar '\n' ==> puts o prompt
          ]
  where prompt = "tower> "
