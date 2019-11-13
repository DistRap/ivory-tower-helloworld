{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Hello.Tests.SimpleBlink where

import Ivory.Language
import Ivory.Tower
import Ivory.HW.Module

import Ivory.BSP.STM32.Peripheral.GPIO
import Ivory.BSP.STM32.ClockConfig (ClockConfig)

import Hello.Tests.Platforms

-- This artificial Tower program toggles LED when
-- message arrives on its channel
ledToggle :: GPIOPin -> Tower e (ChanInput ('Stored ITime))
ledToggle ledPin = do
  -- Create a channel for communicating with this tower
  (cIn, cOut) <- channel

  monitor "ledToggle" $ do
    -- declare dependency on Ivory.HW.Module.hw_moduledef
    monitorModuleDef $ hw_moduledef

    -- handler called during system initialization
    handler systemInit "initLED" $ do
      callback $ const $ do
        pinEnable ledPin
        pinSetMode ledPin gpio_mode_output

    -- LED state
    ledOn <- stateInit "ledOn" (ival false)

    -- handler for channel output
    handler cOut "toggleLED" $ do
      callback $ const $ do
        -- get current state
        isOn <- deref ledOn

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

-- main Tower of our application
app :: (e -> ClockConfig) -> (e -> Platform) -> Tower e ()
app _tocc toPlatform = do
  Platform{..} <- fmap toPlatform getEnv

  -- creates a period that fires every 500ms
  -- `per` is a ChanOutput, specifically ChanOutput ('Stored ITime)
  per <- period (Milliseconds 1000)

  -- create our ledToggle tower
  togIn <- ledToggle platformPin

  -- this monitor simply forwards `per` messages to `togIn` ChanInput
  monitor "blink" $ do
    handler per "blinkPeriod" $ do
      -- message to `togIn` channel are sent via `togInEmitter` - FIFO with capacity 1
      togInEmitter <- emitter togIn 1

      -- callback for period
      callback $ \x ->
        emit togInEmitter x
