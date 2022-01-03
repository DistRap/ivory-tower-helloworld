{-# LANGUAGE RecordWildCards #-}
module Hello.Tests.Platforms (
    platformParser
  , getPlatform
  , buildHelloApp
  , buildWrappedApp
  , module Hello.Tests.Platforms.Bluepill
  , module Hello.Tests.Platforms.F0DISCO
  , module Hello.Tests.Platforms.F3DISCO
  , module Hello.Tests.Platforms.F4DISCO
  , module Hello.Tests.Platforms.IOT01A
  , module Hello.Tests.Platforms.Monstick
  , module Hello.Tests.Platforms.NucleoG474
  , module Hello.Tests.Platforms.NucleoF767
  , module Hello.Tests.Platforms.Types
  ) where

import Hello.Tests.Platforms.Types
import Hello.Tests.Platforms.Bluepill (bluepill)
import Hello.Tests.Platforms.F0DISCO (f0disco)
import Hello.Tests.Platforms.F3DISCO (f3disco)
import Hello.Tests.Platforms.F4DISCO (f4disco)
import Hello.Tests.Platforms.IOT01A   (iot01a)
import Hello.Tests.Platforms.Monstick (monstick)
import Hello.Tests.Platforms.NucleoG474 (nucleo_g474)
import Hello.Tests.Platforms.NucleoF767 (nucleo_f767)

import Data.Char (toUpper)
import Ivory.Tower
import Ivory.Tower.Options
import Ivory.Tower.Config
import Ivory.OS.FreeRTOS.Tower.STM32
import Ivory.BSP.STM32.ClockConfig

platformParser :: Platform -> ConfigParser Platform
platformParser defPlatform = do
  p <- (subsection "args" $ subsection "platform" string) <|> pure "default"
  case map toUpper p of
    "BLUEPILL"       -> return bluepill
    "F0DISCO"        -> return f0disco
    "F3DISCO"        -> return f3disco
    "F4DISCO"        -> return f4disco
    "IOT01A"         -> return iot01a
    "MONSTICK"       -> return monstick
    "NUCLEO_G474"    -> return nucleo_g474
    "NUCLEO_F767"    -> return nucleo_f767
    "DEFAULT"        -> return defPlatform
    _ -> fail ("no such platform " ++ p)

-- allows overriding platform, default platforms MCU and few of its params
-- via default.conf
getPlatform :: Platform -> TOpts -> IO Platform
getPlatform defPlatform topts = do
    -- first platformParser
    p <- getConfig topts (platformParser defPlatform)
    -- then possible mcu override with default mcu from platform
    mcuName <- getConfig topts (mcuNameParser (platformMCUName p))

    -- lookup mcu and set in platformMCU
    nmcu <- matchMCU mcuName

    -- mcu options (ram, flash, splitmem..)
    nmcuCfg <- getConfig topts (mcuConfigParser nmcu)

    return $ p { platformMCU = Just nmcuCfg }

platformToConfig :: Platform -> STM32Config
platformToConfig Platform{platformMCU = (Just mcu), platformClocks = cc} = STM32Config mcu cc
platformToConfig _ = error "platformMCU not initialized"

-- for our helloworld applications
-- with type
-- app :: (e -> ClockConfig) -> (e -> Platform) -> Tower e ()
-- we can define this helper
buildHelloApp :: Platform
       -> ((Platform -> ClockConfig) -> (Platform -> Platform) -> Tower Platform ())
       -> IO ()
buildHelloApp def twrapp = compileTowerSTM32FreeRTOS
  platformToConfig (getPlatform def) $ twrapp platformClocks id

-- Wrapper which can be used to build applications
-- using wrapped Platform by providing `unWrap` and `wrap`
-- converting between your wrapper `a` and `Platform`
buildWrappedApp :: (a -> Platform)
                -> (Platform -> a)
                -> a
                -> ((a -> ClockConfig) -> (a -> a) -> Tower a ())
                -> IO ()
buildWrappedApp unWrap wrap def towerApp = compileTowerSTM32FreeRTOS
  (platformToConfig . unWrap)
  ((wrap <$>) . getPlatform (unWrap def))
  (towerApp (platformClocks . unWrap) id)
