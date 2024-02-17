module Main where


import Ivory.BSP.STM32.ClockConfig (ClockConfig)
import Ivory.Tower (Tower)
import Hello.Tests.Platforms (Platform, buildHelloApp, iot01a)

import qualified Data.Foldable
import qualified Hello.Tests.UART
import qualified Hello.Tests.SimpleBlink

main :: IO ()
main =
  buildHelloApp
    iot01a
    composedApp

-- | Compose UART and SimpleBlink applications.
--
-- Note that when led is disabled via UART app,
-- its ledController switches it to HiZ, so
-- blinking stops as its controller only toggles
-- Hi/Lo states.
composedApp
  :: (e -> ClockConfig)
  -> (e -> Platform)
  -> Tower e ()
composedApp c p =
  Data.Foldable.traverse_
    (\x -> x c p)
    [ Hello.Tests.UART.app
    , Hello.Tests.SimpleBlink.app
    ]

-- | Equivavelent with @composedApp@
composedApp'
  :: (e -> ClockConfig)
  -> (e -> Platform)
  -> Tower e ()
composedApp' c p = do
  Hello.Tests.UART.app c p
  Hello.Tests.SimpleBlink.app c p
