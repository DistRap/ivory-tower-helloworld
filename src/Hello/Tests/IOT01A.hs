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

import Ivory.BSP.STM32.ClockConfig (ClockConfig)
import Ivory.BSP.STM32.Peripheral.GPIO
import Ivory.BSP.STM32.Driver.I2C
import Ivory.BSP.STM32.Driver.SPI

import Ivory.Tower.Base.UART
import Ivory.Tower.Base.UART.Types

import Ivory.Tower.Drivers.Temperature.HTS221
import Ivory.Tower.Drivers.Temperature.Types
import Ivory.Tower.Drivers.Display.MAX7219

import Hello.Tests.Platforms

app :: (e -> ClockConfig) -> (e -> Platform) -> Tower e ()
app tocc toPlatform = do
  Platform{..} <- fmap toPlatform getEnv
  let ledpin = platformPin

  (i2cTransmit, _ready) <- i2cTower tocc platformI2C platformI2CPins
  htsResult <- hts221Tower i2cTransmit systemInit hts221DefaultAddr

  (sreq, _sready) <- spiTower tocc platformSPIDevs platformSPIPins

  (displayIn, _intensityIn) <- max7219 sreq (SPIDeviceHandle 0) (Proxy :: Proxy UARTBuffer)

  uartTowerDeps

  togIn <- ledToggle ledpin
  (ostream, istream) <- bufferedUartTower tocc platformUART platformUARTPins 115200 (Proxy :: Proxy UARTBuffer)
  echoPrompt "hello world" ostream istream togIn

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

        str <- floatingToString (displayed) 4
        prefix <- assign $ humi ? (fromIntegral $ ord 'H', fromIntegral $ ord 'T')
        store ((str ~> stringDataL) ! 0) prefix
        refCopy dbg str
        emit disp (constRef str)

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
