module Port where

import Ivory.Language
import Ivory.HW



import Ivory.BSP.STM32.Peripheral.GPIO1.Peripheral

import Ivory.BSP.STM32F103.MemoryMap
import Ivory.BSP.STM32F103.RCC


gpioA :: GPIOPort
gpioA = mkGPIOPort gpioa_periph_base
          (rccEnable rcc_apb2enr_iopaen)
          (rccDisable rcc_apb2enr_iopaen)
          0

gpioB :: GPIOPort
gpioB = mkGPIOPort gpiob_periph_base
          (rccEnable rcc_apb2enr_iopben)
          (rccDisable rcc_apb2enr_iopben)
          0

gpioC :: GPIOPort
gpioC = mkGPIOPort gpioc_periph_base
          (rccEnable rcc_apb2enr_iopcen)
          (rccDisable rcc_apb2enr_iopcen)
          0

rccEnable :: BitDataField RCC_APB2ENR Bit -> Ivory eff ()
rccEnable f = modifyReg rcc_reg_apb2enr $ setBit f
rccDisable :: BitDataField RCC_APB2ENR Bit -> Ivory eff ()
rccDisable f = modifyReg rcc_reg_apb2enr $ clearBit f
