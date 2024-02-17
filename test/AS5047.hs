module Main where

import Hello.Tests.Platforms
import Hello.Tests.AS5047 (app)

import qualified Hello.Tests.Platforms.Stamp as Stamp

stampWithEncoder :: Platform
stampWithEncoder = stamp
  { platformSPIDevs  = [ Stamp.as5047dev (platformSPI stamp) Stamp.spi1CSPin ]
  }

main :: IO ()
main = buildHelloApp stampWithEncoder app
