Zscale core Generator (Using Rocket Chip Generator)
=====================

This repository contains the Rocket chip generator necessary to instantiate
the RISC-V Zscale Core. 

## Table of Contents

+ [Quick instructions](#quick) for those who want to dive directly into the details without knowing exactly what's in the repository.
+ [How should I use the Rocket chip generator?](#how)
    + [Using the cycle-accurate Verilator simulation](#emulator)
    + [Mapping a Rocket core down to an FPGA](#fpga)
    + [Pushing a Rocket core through the VLSI tools](#vlsi)
+ [How can I parameterize my Rocket chip?](#param)

## <a name="quick"></a> Quick Instructions

### Checkout The Code

    $ git clone https://github.com/onchipuis/zscale.git
    $ cd zscale
    $ git submodule update --init
    $ cd rocket-chip
    $ git submodule update --init

### Setting up the RISCV environment variable

To build the rocket-chip repository, you must point the RISCV
environment variable to your riscv-tools installation directory. 

    $ export RISCV=/path/to/riscv/toolchain/installation
    
The riscv-tools repository is already included in 
rocket-chip as a git submodule. You **must** build this version 
of riscv-tools:

    $ cd zscale/rocket-chip/riscv-tools
    $ git submodule update --init --recursive
    $ export RISCV=/path/to/install/riscv/toolchain
    $ export MAKEFLAGS="$MAKEFLAGS -jN" # Assuming you have N cores on your host system
    $ ./build.sh
    $ ./build-rv32ima.sh (if you are using RV32).

For more information (or if you run into any issues), please consult the
[riscv-tools/README](https://github.com/riscv/riscv-tools/blob/master/README.md).

### Install Necessary Dependencies

You may need to install some additional packages to use this repository.
Rather than list all dependencies here, please see the appropriate section of the READMEs for each of the subprojects:

* [riscv-tools "Ubuntu Packages Needed"](https://github.com/riscv/riscv-tools/blob/priv-1.10/README.md#quickstart)
* [chisel3 "Installation"](https://github.com/ucb-bar/chisel3#installation)

### Building The Project (Verilog)

To build the core in verilog:

    $ make

## <a name="how"></a> How should I use the Zscale core generator?

TODO: Write this

### <a name="emulator"></a> 1) Using the high-performance cycle-accurate Verilator

TODO: Write this

### <a name="fpga"></a> 2) Mapping a Rocket core to an FPGA

You can generate synthesizable Verilog with the following commands:

    $ make CONFIG=DefaultConfig

You can generate synthesizable Verilog for minimum:

    $ make CONFIG=TinyConfig
    
TODO: Write more

### <a name="vlsi"></a> 3) Pushing a Rocket core through the VLSI tools

You can generate Verilog for your VLSI flow with the following commands:

    $ make verilog

TODO: Write more

## <a name="param"></a> How can I parameterize my Rocket chip?

TODO: Write this

## <a name="contributors"></a> Contributors

Can be found [here](https://github.com/onchipuis/zscale/graphs/contributors).

## <a name="attribution"></a> Attribution

TODO: Write this
