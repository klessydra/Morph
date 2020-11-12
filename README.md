<img src="/pics/Klessydra_Logo.png" width="400">

# KLESSYDRA-OoO "BETA VERSION!!!!!" Out-of-Order Execution Processor

This is a non-functioning beta version that can only run a helloworld atm. Updates will follow shortly along with diagrams and a manual explaining the functionality of the core

Intro: The Klessydra processing core family is a set of processors featuring full compliance with RISC-V, and pin-to-pin compatible with the PULPino Riscy cores. Klessydra-OoO is a bare-metal 32-bit Out-of-Order Executing processor fully supporting the RV32IM from the RISC-V ISA, and one instruction from the Atomic "A" extension. 'OoO' further extends the instruction set with a set of custom vector instructions.

Architecture: Klessydra-OoO is a six pipeline stage processor Fetch-Decode, Rename, Issue, Registerfile, Execute/Ld-Str, Writeback. The Renamed register file has a parametrizable number of registers. The registerfile can support up-to twelve read ports and three write ports. A commit counter handles the out-of-order commit of the registers, and an ROB handles the recovery in case of traps or branch misses.

The OoO furhter supports the vector accelerator present in the T2M, T13, and S1.

The Coprocessor is a highly parametrizable accelerator, with up to 256-bit SIMD+MIMD execution capabilities. It comprises the Multipurpose Functional Unit, and the Scratchpad Memory Interface. The custom instruction set supported are listed in the Technincal manuals in the Docs folder. In addition to SIMD execution, the coprocessor supports subword-SIMD to further accelerate 8-bit and 16-bit integer data types.

The coprocessor features a parametrizable set of Scratchpad memories 'SPMs' (parametrizable being their size and number, and their bank numbers will automatically expand to match the SIMD configuration). 

The coprocessor can be configured to run in three different modes:

1) Shared Coprocessor: Where the coprocessor is shared by all the harts (SIMD Coprocessor).
2) Fully Symmetrical Coprocessor: Where each hart has its dedicated MFU and SPMI. (SIMD+MIMD Coprocessor ver.1).
3) Heterogeneous coprocessor: Where the harts share the functional units in the MFU, but each hart maintains it own dedicated SPMI (SIMD+MIMD coprocessor ver.2).

Parameters:
- N = Number of SPMs in the SPMI.
- M = Number of SPMIs, as well as control logic for every hart.
- D = Number of Functional Units per MFU, and banks per SPM (i.e. determines the SIMD width).
- F = Number of Functional Units per hart (i.e. determines the MIMD width).

<p align="center">
<img src="/pics/Vector Coprocessor.png" width="500">
</p> 
