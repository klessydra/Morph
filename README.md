<img src="/pics/Klessydra_Logo.png" width="400">

# KLESSYDRA-M MORPHING PROCESSOR

Intro: Klessydra is a family of processing cores and coprescessing units with a fully integratted C/C++ software test suite and libraries. The Klessydra Cores are RISC-V based, and pin-to-pin compatible with the PULPino Riscy cores. Klessydra-M is a bare-metal 32-bit morphing processor fully supporting the RV32IM from the RISC-V ISA, and one instruction from the Atomic "A" extension. 'M' further extends the instruction set with a set of custom vector instructions.

Architecture: Morph as its predecssors T13x and T0x vesions is also an interleaved multithreaded processor (Aka, barrel processor). It interleaves three hardware threads (harts). Each hart has it's own registerfile, CSR-unit, and program counter, and the harts communicate with each other via software interrupts. However as opposed to its predecessors Klessydra-M has a dynamic architecture that can morph from one architecture to the other when needed in order to accelerate the execution of the underlying application.

Morphing ability of the Klessydra-M: This processor is capable of dynamically changing the execution paradigm of its architecture morphing itself from being an interleaved multithreaded prcoessor when all the harts are active to an In-order execution processor when one hart is acive by enabling a data dependency checker, registerfile bypass logic, a branch predictor, and pipeline flushing logic. Klessydra-M can be considred as a merge between Klessydra-S1, and Klessydra-T13 processing cores assimilating both their advantages.

Fencing role of the harts: The harts in our IMT archtiecture play an essential fencing role to avoid pipeline stalls. One role is to fence between registerfile read and write accesses, by interleaving threads to sit between the read and write stages thus never having data-dependency pipeline stalls. The other is to fence between the execution and fetch stage, thus avoiding the need to perform any pipeline flushing. Once the number of harts becomes less then the required baseline required to create a fence, in that case the data dependency checker and the branch-predictor turn on in order to avoid execution hazards.

Klessydra-M furhter maintains the interface to connect to the vector accelerator present in both the T13 and S1.

The Coprocessor is a highly parametrizable accelerator, with up to 256-bit SIMD+MIMD execution capabilities. It comprises the Vector Coprocessing Unit, and the Scratchpad Memory Interface. The custom instruction set supported are listed in the Technincal manuals in the Docs folder. In addition to SIMD/MIMD execution, the coprocessor supports subword-SIMD to further accelerate 8-bit and 16-bit integer data types.

The coprocessor features a parametrizable set of Scratchpad memories 'SPMs' (parametrizable being their size and number, while the bank count per SPM will automatically expand to match the bandwidth of the SIMD configuration set). 

The coprocessor can be configured to run in three different modes:

1) Shared Coprocessor: Where the coprocessor is shared by all the harts (SIMD Coprocessor).
2) Fully Symmetrical Coprocessor: Where each hart has its dedicated VCU and SPMI. (SIMD+MIMD Coprocessor ver.1).
3) Heterogeneous coprocessor: Where the harts share the functional units in the VCU, but each hart maintains it own dedicated SPMI (SIMD+MIMD coprocessor ver.2).

Parameters:
- N = Number of SPMs in the SPMI.
- M = Number of SPMIs, as well as control logic for every hart.
- D = Number of Functional Units per MFU, and banks per SPM (i.e. determines the SIMD width).
- F = Number of Functional Units per hart (i.e. determines the MIMD width).

<p align="center">
<img src="/pics/Vector Coprocessor.png" width="500">
</p> 

# Using Klessydra-M

This guide explains how one can download Pulpino-Klessydra that has all the Klessydra Cores integrated inside of it, and build PULP's version of the riscv-gnu-toolchain. It also demonstrates how to patch the offcial riscv-gnu-toolchain in order to add the Klessydra custom vector extensions.

###########################################################################################
- Prerequisites as indicated by the PULP group
	- Mentor ModelSim
	- CMake >= 2.8.0, versions greater than 3.1.0 recommended due to support for ninja
	- riscv-toolchain, there are two choices for getting the toolchain: 

  		1) RECOMENDED OPTION: Use the custom version of the RISC-V toolchain from ETH. 
  		The ETH versions supports all the ISA extensions that were incorporated 
	  	into the RI5CY core as well as the reduced base instruction set for zero-riscy.
	        " https://github.com/pulp-platform/ri5cy_gnu_toolchain.git "

		2) Or download the official RISC-V toolchain supported by Berkeley.
 	       	" https://github.com/riscv/riscv-gnu-toolchain "


	  	Please make sure you are using the newlib version of the toolchain.
	- python2 >= 2.6
	
###########################################################################################

PROCEDURE:
1.	Install the following packeges:
		
		sudo apt-get install git cmake python-yaml tcsh autoconf automake autotools-dev curl libmpc-dev libmpfr-dev libgmp-dev gawk build-essential bison flex texinfo gperf libtool patchutils bc zlib1g-dev libexpat-dev

2.	Download and build the "ri5cy_gnu_toolchain"

		a) git clone https://github.com/pulp-platform/ri5cy_gnu_toolchain.git
		
		b) cd ri5cy_gnu_toolchain
		
		c) make ZERORISCY=1

		d) in case you need to build for RISCY cores, then just do "make" instead, and then add the symbolic links as shown from step 4.
		
	When the build is done, add the path **_<path_to_toolchain>/ri5cy_gnu_toolchain/install/bin_**, to the $PATH variable

3.	To run the klessydra tests, you have to download and patch the official riscv-gnu-toolchain, and then build it. Instructions for doing so are included in the README.md file inside the folder called "toolchain_files".

4.	Download PULPino-Klessydra:

		a) git clone https://github.com/klessydra/pulpino-klessydra
		
		b) cd pulpino-klessydra
		
		c) ./update-ips.py	

5.	OPTIONAL: After the update scipt is done, then you will be able to test Klessydra-m.
		-Navigate to "sw" folder inside pulpino and execute the following commands

		a) mkdir build
		
		b) cp cmake_configure.klessydra-m.gcc.sh build/
		
		c) cd build
		
		d) ./cmake_configure.klessydra-m.gcc.sh
		   
		e) make vcompile

		For running Klessydra tests; make sure the variable "USE_KLESSYDRA_TEST" in the above bash script is set to '1' by default. You only need to build and run your test
		f) (e.g.  make KDOTP_test.vsimc)
		General tests for all "Txx" versions of Klessydra are also available
		g) (e.g.  make barrier_test.vsimc)
		
		h) You can run one of the PULPino native tests,  (e.g. make testALU.vsimc)
			
	IT"S DONE!!!!!!

Supplimentary Information:

6.	In order to run tests under Modelsim in gui mode, navigate again to the build folder and do the following:
		make nameofthetest.vsim (while .vsimc runs the test under Modelsim in background)

9. Klessydra-Morph libraries are available, and their functions are described in the software runtime manual found in the Docs folder

# Morph Extensions illustration

The following illustrates briefly the parameters of the Klessydra-M, and their usage settings.

- For more details about the Klessydra processing cores, please refer to the technincal manual in Docs
- For more details about the Klessydra runtime libraries, please refer to the software runtime manual in Docs

Extensions of Morph core:

The Morph can be configed in many ways in the from the "cmake_configure.klessydra-t2-m.gcc.sh" found in the sw forlder:

You will find the following generics that will be passed to the RTL. **_Read the comments next to the variables before modifying_**:
1)  "THREAD_POOL_SIZE" sets the number of hardware threads.
2)	"LUTRAM_RF" this variable creates a LUTRAM based registerfile instead of a flip-flop based one, it is good for FPGA synthesis as LUTRAMs based regfiles are more efficient than FF based ones.
3)	"RV32E" this enables the embedded extension of the RISCV ISA, and makes the regfile to be half its original size (16 regs only).
4)	"RV32M" this enable the M-extension of the RISCV ISA. The mul instruction is a single cycle instructions, and the mulh/hu/hsu instructions need 3 cycles. divisions are slow, and can be up to 32 cycles, however fast single cycle divisions are availabe for special cases (div by 0, numerator < denominator, numerator is 0, and numerator equals the denominator).
5)	"superscalar_exec_en=1"  Enables superscalar execution when set to 1, else the stall of the pipeline will depend on tha latency of the instruction executing. This more than doubles the speed of the core in many applications, however if in the exceptional case the RTL is not simulating correctly, disable this and see whether the RTL will work again.
6)	"accl_en"  Enables the generation of the hardware accelerator.
7)	"replicate_accl_en" Once set, it replicates the accelerator for every thread, this increases the parallelism of the Morph by allocating a dedicated accelerator for each hart in the Morph.
8)	"multithreaded_accl_en" Set this to 1 to let the replicated accelerator have shared functional units, but maintain dedicated SPM memories for each hardware thread (note: replicate_accl_en must be set to '1').
9)	"SPM_NUM" The number of scratchpads available "Minimum allowed is 2". When the acclerator is replicated, each hardware thread will have scratchpads equal to SPM_NUM, so in a THREAD_POOL_SIZE of 3 we will have 3*SPM_NUM scratchpads in totals
10)	Addr_Width" This address is for scratchpads. Setting this will make the size of the spm to be: "2^Addr_Width -1"
11)	"SIMD" Changing the SIMD, would increase the DLP by increasing the number of the functional units in the accelerator, and the number of banks in the spms acordingly (can be power of 2 only e.g. 1,2,4,8) no more than SIMD 8 is allowed. Setting this to '8' with replicate_accl_en and superscalar_exec_en being set to '1' as well will make the accelerator run at peak performance.
12)	"MCYCLE_EN" Can be set to 1 or 0 only. Setting to zero will disable MCYCLE and MCYCLEH
13)	"MINSTRET_EN" Can be set to 1 or 0 only. Setting to zero will disable MINSTRET and MINSTRETH
14)	"MHPMCOUNTER_EN" Can be set to 1 or 0 only. Setting to zero will disable all performance counters except "MCYCLE/H" and "MINSTRET/H"
15)	"count_all" Perfomance counters count for all the harts instead of there own hart
16)	"debug_en" Generates the debug unit, the debug unit is elimentary and might need some further evaluation and testing
17)	"tracer_en" Generates an instruction tracer, used for debugging


Hope you like it :D
