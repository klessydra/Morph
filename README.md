<img src="/pics/Klessydra_Logo.png" width="400">

# KLESSYDRA-M MORPHING PROCESSOR

Klessydra-Morph as its predecssors T13x and T0x vesions is also an interleaved multithreaded processor (aka barrel processor). It interleaves three hardware threads (harts). Each hart has it's own registerfile, CSR-unit, and program counter, and the harts communicate with each other via software interrupts. However as opposed to its predecessors Klessydra-M has a dynamic architecture that can morph from one architecture to the other when needed in order to accelerate the execution of the underlying application.

Morphing ability of the Klessydra-M: This processor is capable of dynamically changing the execution paradigm of its architecture morphing itself from being an interleaved multithreaded prcoessor when all the harts are active to an In-order execution processor when one hart is acive by enabling a data dependency checker, registerfile bypass logic, a branch predictor, and pipeline flushing logic. When two harts are active, we say that the processor is in hybrid mode still enabling the aforementioned logic, as well as we continue to interleave two harts in the pipeline. Klessydra-M can be considred as a merge between Klessydra-S1, and Klessydra-T13 processing cores assimilating both their advantages, plus the advantage of running in hybrid mode.

Generating other cores: Klessydra-M can be configured to work in many modes, (see Klessydra-M parameters section below for more details) for example setting "KLESS_morph_en" to '0' disables the generation of all the morphing logic, makeing the core become like the Klessydra-T1, and further setting "KLESS_accl_en" to '0' turns of the accelerator making the core execute like the Klessydra-T0. Setting the "KLESS_THREAD_POOL_SIZE" to '1' make the core execute like Klessydra-S1.

Klessydra-HetC: There is one particular generic "KLESS_CONTEXT_SWITCH", that if you set to '1', will generate a cluster of two context switching cores, the first being Klessydra-T1, while the second is the Klessydra-S1. Setting this generic will provide a degree of inter-processor heterogeneity that allows a porcessor to make a context switch towards the Klessydra-S1 when the active number of harts in Klessydra is equal to '1', and then switch back to Klesydra-T1 when we need to wake up the harts. This is opposed to the intra-processor heterogeneity present in the architecture of the Klessydra-M. 

Fencing role of the harts: The harts in the IMT archtiectures of Klessydra play an essential fencing role to avoid pipeline stalls. One role is to fence between registerfile read and write accesses, by interleaving threads to sit between the read and write stages thus never having data-dependency related pipeline stalls. The other is to fence between the execution stage where branch instructions and jumps are handled and the fetch stage, thus avoiding the need to perform any pipeline flushing. Once the number of harts becomes less then the required baseline required to create a fence, in that case the data dependency checker and the branch-predictor turn on in order to avoid data and control hazards.

PROCEDURE:

- For more details about the Klessydra processing cores, please refer to the technincal manual in Docs
- For more details about the Klessydra runtime libraries, please refer to the software runtime manual in Docs

Hope you like it :D
