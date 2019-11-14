{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Hello.Tests.I2CWhoAmI where

import Data.Char (ord)
import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.HW.Module

import Ivory.Tower.HAL.Bus.Interface

import Ivory.BSP.STM32.Peripheral.GPIO
import Ivory.BSP.STM32.Driver.I2C

import Ivory.BSP.STM32.ClockConfig (ClockConfig)

import Ivory.Tower.Base.LED (ledToggle)
import Hello.Tests.Platforms

-- HTS221 default address
addr = I2CDeviceAddr 0x5F

app tocc toPlatform = do
  Platform{..} <- fmap toPlatform getEnv

  ((BackpressureTransmit req res), _ready) <- i2cTower tocc platformI2C platformI2CPins
  togIn <- ledToggle [platformRedLED]

  per <- period (Milliseconds 1000)

  monitor "i2c" $ do
    handler per "i2cPer" $ do
      reqE <- emitter req 1
      callback $ const $ do
        -- whoami
        r <- local $ istruct
                     [ tx_addr   .= ival addr
                     , tx_buf    .= iarray [ival 0x0F]
                     , tx_len    .= ival 1
                     , rx_len    .= ival 1
                     ]
        emit reqE (constRef r)

    dbg <- state "whoami_result"

    handler res "i2cResult" $ do
      e <- emitter togIn 1
      callback $ \x -> do
        refCopy dbg x
        emit e x
