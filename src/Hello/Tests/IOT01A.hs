{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Hello.Tests.IOT01A where

import Data.Char (ord)
import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.HW.Module

import Ivory.Tower.HAL.Bus.Interface

import Ivory.BSP.STM32.Peripheral.GPIO
import Ivory.BSP.STM32.Driver.I2C
import Ivory.BSP.STM32.Driver.SPI
import Ivory.BSP.STM32.Driver.UART
import Ivory.BSP.STM32.ClockConfig (ClockConfig)

import Ivory.Tower.Base.UART
import Ivory.Tower.Base.UART.Types
import Ivory.Tower.HAL.Bus.Sched

import Ivory.Tower.Drivers.Temperature.HTS221
import Ivory.Tower.Drivers.Temperature.Types
import Ivory.Tower.Drivers.Display.MAX7219

import Hello.Tests.Platforms

-- XXX: move somewhere more appropriate?
-- 0xBE / 2
hts221Addr = I2CDeviceAddr 0x5F

app :: (e -> ClockConfig) -> (e -> Platform) -> Tower e ()
app tocc toPlatform = do
  Platform{..} <- fmap toPlatform getEnv
  let ledpin = platformPin

  (i2cTransmit, _ready) <- i2cTower tocc platformI2C platformI2CPins
  htsResult <- hts221Tower i2cTransmit systemInit hts221Addr

  (sreq, sready) <- spiTower tocc platformSPIDevs platformSPIPins

  (displayIn, intensityIn) <- max7219 sreq (SPIDeviceHandle 0) (Proxy :: Proxy UARTBuffer)

  uartTowerDeps

  togIn <- ledToggle ledpin
  (ostream, istream) <- bufferedUartTower tocc platformUART platformUARTPins 115200 (Proxy :: Proxy UARTBuffer)
  echoPrompt "hello world" ostream istream togIn

  per <- period (Milliseconds 100)
  iper <- period (Milliseconds 10)

  -- toggle value on display (temperature or humidity)
  toggleDisplay <- period (Milliseconds 10000)

  monitor "th" $ do
    lastSample <- state "lastSample"
    showHumi <- state "showHumi"
    dbg <- state "dbg"

    handler htsResult "hts" $ do
      disp <- emitter displayIn 1

      callback $ \s -> do
        refCopy lastSample s
        t <- lastSample ~>* sample_th_temperature
        h <- lastSample ~>* sample_th_humidity

        humi <- deref showHumi
        displayed <- assign $ humi ? (h, t)

        str <- floatingToString (displayed) 3
        refCopy dbg str
        emit disp (constRef str)
        return ()

    handler toggleDisplay "toggleDisplay" $ do
      callback $ const $ do
        old <- deref showHumi
        store showHumi (iNot old)

ledToggle :: GPIOPin -> Tower e (ChanInput ('Stored IBool))
ledToggle ledPin = do
  (cIn, cOut) <- channel

  monitor "ledToggle" $ do
    monitorModuleDef $ hw_moduledef

    handler systemInit "initLED" $ do
      callback $ const $ do
        pinEnable ledPin
        pinSetMode ledPin gpio_mode_output

    -- LED state
    ledOn <- stateInit "ledOn" (ival false)

    -- handler for channel output
    handler cOut "toggleLED" $ do
      callbackV $ \isOn -> do
        ifte_ isOn
          (do
            pinClear ledPin
            store ledOn false
          )
          (do
            pinSet ledPin
            store ledOn true
          )

  return (cIn)

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
