{-# LANGUAGE TemplateHaskell #-}
module Pin where

import Port
import Ivory.BSP.STM32.Peripheral.GPIO2.Regs
import Ivory.BSP.STM32.Peripheral.GPIO2.TH

mkGPIOPins 'gpioA "pinA"
mkGPIOPins 'gpioB "pinB"
mkGPIOPins 'gpioC "pinC"
