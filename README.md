# helloworld firmware

Experimental version of Hello world firmware.
This repository contains some examples to get you started
and test basic functionality of your board.

Ideal targets are F4 Discovery or Nucleo F767 boards,
with their programming side flashed with BlackMagic Probe
(but it is possible to use `stlink` and `OpenOCD` as well).

Written in http://ivorylang.org/

## Development shell

To enter development shell simply run `nix-shell`

## Test applications

### Blink

Blinks red and blue LEDs on `GPIOD14` and `GPIOD15` (depending on your platform).

### SimpleBlink

Simplified blink application toggling red LED.

### CANSendRecv

Test application sending packets from CAN1, blinks on received packets.

### CAN2UART

Test application for receiving and sending CAN packets controlled by UART.

### UART

Simple UART console, allows toggling LED with `1` `2` characters.

## Usage

Run `make` to build all test applications.
Specific application can be built with `make APP`
loaded with `make APP-load` and `make APP-run`.

To load Blink test application run

```shell
make blink-test-load
```

to also issue run and start application after loading use

```shell
make blink-test-run
```

to just run gdb with new binary without loading

```shell
make blink-test-gdb
# issuing 'load' in gdb         == blink-test-load
# running both 'load' and 'run' == blink-test-run
```


### Flashing

Manually with BlackMagic Probe

```shell
arm-none-eabi-gdb \
  --ex 'target extended-remote /dev/ttyACM0' \
  --ex 'monitor swdp_scan' \
  --ex 'attach 1' \
  --ex 'load' \
  build/can2uart-test/image
```
