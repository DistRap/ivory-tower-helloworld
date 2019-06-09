module Port where

import Ivory.Language
import Ivory.HW



import Ivory.BSP.STM32.Peripheral.GPIO2.Peripheral

import Ivory.BSP.STM32L432.MemoryMap
import Ivory.BSP.STM32L432.RCC


gpioA :: GPIOPort
gpioA = mkGPIOPort gpioa_periph_base
          (rccEnable rcc_ahb2enr_gpioaen)
          (rccDisable rcc_ahb2enr_gpioaen)
          0

gpioB :: GPIOPort
gpioB = mkGPIOPort gpiob_periph_base
          (rccEnable rcc_ahb2enr_gpioben)
          (rccDisable rcc_ahb2enr_gpioben)
          1

gpioC :: GPIOPort
gpioC = mkGPIOPort gpioc_periph_base
          (rccEnable rcc_ahb2enr_gpiocen)
          (rccDisable rcc_ahb2enr_gpiocen)
          2

rccEnable :: BitDataField RCC_AHB2ENR Bit -> Ivory eff ()
rccEnable f = modifyReg rcc_reg_ahb2enr $ setBit f
rccDisable :: BitDataField RCC_AHB2ENR Bit -> Ivory eff ()
rccDisable f = modifyReg rcc_reg_ahb2enr $ clearBit f
