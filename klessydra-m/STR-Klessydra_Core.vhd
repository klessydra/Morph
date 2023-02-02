---------------------------------------------------------------------------------------------------------------------
--                                                                                                                 --
--  Author(s): Abdallah Cheikh abdallah.cheikh@uniroma1.it (abdallah93.as@gmail.com)                               --
--                                                                                                                 --
--  Date Modified: 07-04-2020                                                                                      --
---------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------
--  Klessydra-M core v.4.0:                                                                                        --
--  RISCY core pinout, RISC-V core, RV32IMA support plus the RISC-V Embedded E-extension and custom                --
--  K-extension. T13 has 4 pipeline stages F/RD/E/W, in order execution. With the execute stage being superscalar  --
--  Supports interleaved multithreading (IMT), with maximum configurable thread pool size = 16 threads.            --
--  Pure RISCV exception and interrupt handling. Only thread 0 can be interrupted extenranlly. inter-thread ints   --
--  are allowed, and used for thread synchronization. Pulpino irq/exception table fully supported by SW            --
--  runtime system.                                                                                                --
--  Contributors : Abdallah Cheikh.                                                                                --
--  last update: 27-08-2021                                                                                        --
---------------------------------------------------------------------------------------------------------------------

-- package riscv_kless is new work.riscv_klessydra
--   generic map (RV32E => 0);

-- ieee packages ------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_misc.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
use std.textio.all;

-- local packages ------------
use work.riscv_klessydra.all;
--  use work.riscv_kless.all;

------------------------------------------------------------------------------------------------
--  ██╗  ██╗██╗     ███████╗███████╗███████╗██╗   ██╗██████╗ ██████╗  █████╗     ███╗   ███╗  --
--  ██║ ██╔╝██║     ██╔════╝██╔════╝██╔════╝╚██╗ ██╔╝██╔══██╗██╔══██╗██╔══██╗    ████╗ ████║  --
--  █████╔╝ ██║     █████╗  ███████╗███████╗ ╚████╔╝ ██║  ██║██████╔╝███████║    ██╔████╔██║  --
--  ██╔═██╗ ██║     ██╔══╝  ╚════██║╚════██║  ╚██╔╝  ██║  ██║██╔══██╗██╔══██║    ██║╚██╔╝██║  --
--  ██║  ██╗███████╗███████╗███████║███████║   ██║   ██████╔╝██║  ██║██║  ██║    ██║ ╚═╝ ██║  --
--  ╚═╝  ╚═╝╚══════╝╚══════╝╚══════╝╚══════╝   ╚═╝   ╚═════╝ ╚═╝  ╚═╝╚═╝  ╚═╝    ╚═╝     ╚═╝  --
------------------------------------------------------------------------------------------------

-- core entity declaration --
entity klessydra_m_core is
  generic (
    THREAD_POOL_SIZE_GLOBAL : natural := 4;   -- Indicates the total number of harts on the chip, and not only the ones in the cores
    THREAD_POOL_SIZE        : natural := 1;   -- Changing the TPS to less than "number of pipeline stages-1" is not allowed. And making it bigger than "pipeline stages-1" is okay but not recommended
    cluster_size_ceil       : natural := 1;   -- The cieling bits of that identify the size of the cluster
    lutram_rf               : natural := 1;   -- Changes the regfile from flip-flop type into LUTRAM type
    latch_rf                : natural := 0;   -- Changes the regfile from flip-flop type into Latch type (only works if lutram_rf is set to 0)
    RV32E                   : natural := 0;   -- Regfile size, Can be set to 32 for RV32E being 0 else 16 for RV32E being set to 1
    RV32M                   : natural := 1;   -- Enables the M-extension of the risc-v instruction set
    context_switch          : natural := 0;   -- Enables the context switching between cores
    morph_en                : natural := 1;   -- Enables the generation of the logic that allows processor to morph from an IMT to a single core processor
    fetch_stage_en          : natural := 0;   -- Enables the generation of a fetch stage buffer, else the incoming instrution will go directly to the decode stage.
    branch_predict_en       : natural := 1;   -- This enables the branch predictor
    btb_en                  : natural := 0;   -- Enables the BTB instead of the single bit predictor
    btb_len                 : natural := 6;   -- Indicates the number of entries in the btb which is 2^btb_len
    superscalar_exec_en     : natural := 1;   -- Enables superscalar execution when set to 1, else the stall of the pipeline will depend on tha latency of the instruction
    accl_en                 : natural := 1;   -- Enables the generation of the general purpose accelerator
    replicate_accl_en       : natural := 0;   -- Set to 1 to replicate the accelerator for every thread
    multithreaded_accl_en   : natural := 0;   -- Set to 1 to let the replicated accelerator share the functional units (note: replicate_accl_en must be set to '1')
    SPM_NUM                 : natural := 3;   -- The number of scratchpads available "Minimum allowed is two"
    Addr_Width              : natural := 13;  -- This address is for scratchpads. Setting this will make the size of the spm to be: "2^Addr_Width -1"
    SPM_STRT_ADDR           : std_logic_vector(31 downto 0) := x"1000_0000";  -- This is starting address of the spms, it shouldn't overlap any sections in the memory map
    SIMD                    : natural := 8;   -- Changing the SIMD, would change the number of the functional units in the dsp, and the number of banks in the spms (can be power of 2 only e.g. 1,2,4,8)
    MCYCLE_EN               : natural := 0;   -- Can be set to 1 or 0 only. Setting to zero will disable MCYCLE and MCYCLEH
    MINSTRET_EN             : natural := 0;   -- Can be set to 1 or 0 only. Setting to zero will disable MINSTRET and MINSTRETH
    MHPMCOUNTER_EN          : natural := 0;   -- Can be set to 1 or 0 only. Setting to zero will disable all performance counters except "MCYCLE/H" and "MINSTRET/H"
    count_all               : natural := 1;   -- Perfomance counters count for all the harts instead of there own hart
    debug_en                : natural := 0;   -- Generates the debug unit
    tracer_en               : natural := 0;   -- Enables the generation of the instruction tracer disable in extremely long simulations in order to save storage space
    ----------------------------------------------------------------------------------------
    Data_Width              : natural;
    SPM_ADDR_WID            : natural;
    SIMD_BITS               : natural;
    ACCL_NUM                : natural;
     ----------------------------------------------------------------------------------------
    N_EXT_PERF_COUNTERS     : integer := 0;   -- ignored in Klessydra
    INSTR_RDATA_WIDTH       : integer := 32;  -- ignored in Klessydra
    N_HWLP                  : integer := 2;   -- ignored in Klessydra
    N_HWLP_BITS             : integer := 4    -- ignored in Klessydra
    );
  port (
    -- clock, reset active low, test enable
    clk_i                   : in  std_logic;
    clock_en_i              : in  std_logic;
    rst_ni                  : in  std_logic;
    test_en_i               : in  std_logic;
    -- initialization signals 
    boot_addr_i             : in  std_logic_vector(31 downto 0);
    core_id_i               : in  std_logic_vector(3 downto 0);
    cluster_id_i            : in  std_logic_vector(5 downto 0);
    -- program memory interface
    instr_req_o             : out std_logic;
    instr_gnt_i             : in  std_logic;
    instr_rvalid_i          : in  std_logic;
    instr_addr_o            : out std_logic_vector(31 downto 0);
    instr_rdata_i           : in  std_logic_vector(31 downto 0);
    -- data memory interface
    data_req_o              : out std_logic;
    data_gnt_i              : in  std_logic;
    data_rvalid_i           : in  std_logic;
    data_we_o               : out std_logic;
    data_be_o               : out std_logic_vector(3 downto 0);
    data_addr_o             : out std_logic_vector(31 downto 0);
    data_wdata_o            : out std_logic_vector(31 downto 0);
    data_rdata_i            : in  std_logic_vector(31 downto 0);
    data_err_i              : in  std_logic;
    -- interrupt request interface
    irq_i                   : in  std_logic;
    irq_id_i                : in  std_logic_vector(4 downto 0);
    irq_ack_o               : out std_logic;
    irq_id_o                : out std_logic_vector(4 downto 0);
    irq_sec_i               : in  std_logic;  -- unused in Pulpino
    sec_lvl_o               : out std_logic;  -- unused in Pulpino
    -- debug interface
    debug_req_i             : in  std_logic;
    debug_gnt_o             : out std_logic;
    debug_rvalid_o          : out std_logic;
    debug_addr_i            : in  std_logic_vector(14 downto 0);
    debug_we_i              : in  std_logic;
    debug_wdata_i           : in  std_logic_vector(31 downto 0);
    debug_rdata_o           : out std_logic_vector(31 downto 0);
    debug_halted_o          : out std_logic;
    debug_halt_i            : in  std_logic;
    debug_resume_i          : in  std_logic;
    -- miscellanous control signals
    fetch_enable_i          : in  std_logic;
    core_busy_o             : out std_logic;
    ext_perf_counters_i     : in  std_logic_vector(N_EXT_PERF_COUNTERS to 1);
    -- klessydra-specific signals
    core_select             : in  natural range 1 downto 0;
    source_hartid_i         : in  natural range THREAD_POOL_SIZE_GLOBAL-1 downto 0; -- used to overwrite the mhartID of the core doing the context switch
    source_hartid_o         : out natural range THREAD_POOL_SIZE_GLOBAL-1 downto 0;
    sw_irq_i                : in  std_logic_vector(THREAD_POOL_SIZE_GLOBAL-1 downto 0);
    sw_irq_o                : out std_logic_vector(THREAD_POOL_SIZE_GLOBAL-1 downto 0);
    sw_irq_served_i         : in  std_logic_vector(THREAD_POOL_SIZE-1 downto 0);
    sw_irq_served_o         : out std_logic_vector(THREAD_POOL_SIZE_GLOBAL-1 downto 0);
    -- VCU Signals
    harc_EXEC               : out natural range THREAD_POOL_SIZE-1 downto 0;
    MVSIZE                  : out array_2d(THREAD_POOL_SIZE-1 downto 0)(Addr_Width downto 0);
    MVTYPE                  : out array_2d(THREAD_POOL_SIZE-1 downto 0)(3 downto 0);
    MPSCLFAC                : out array_2d(THREAD_POOL_SIZE-1 downto 0)(4 downto 0);
    pc_IE                   : out std_logic_vector(31 downto 0);
    rs1_to_sc               : out std_logic_vector(SPM_ADDR_WID-1 downto 0);
    rs2_to_sc               : out std_logic_vector(SPM_ADDR_WID-1 downto 0);
    rd_to_sc                : out std_logic_vector(SPM_ADDR_WID-1 downto 0);
    decoded_instruction_DSP : out std_logic_vector(DSP_UNIT_INSTR_SET_SIZE-1 downto 0);
    RS1_Data_IE             : out std_logic_vector(31 downto 0);
    RS2_Data_IE             : out std_logic_vector(31 downto 0);
    RD_Data_IE              : out std_logic_vector(31 downto 0);  -- unused
    dsp_instr_req           : out std_logic_vector(ACCL_NUM-1 downto 0);
    spm_rs1                 : out std_logic;
    spm_rs2                 : out std_logic;
    vec_read_rs1_ID         : out std_logic;
    vec_read_rs2_ID         : out std_logic;
    vec_write_rd_ID         : out std_logic;
    busy_DSP                : in  std_logic_vector(ACCL_NUM-1 downto 0);
    state_LS                : out fsm_LS_states;
    sc_word_count_wire      : out integer;
    spm_bcast               : out std_logic;
    harc_LS_wire            : out integer range ACCL_NUM-1 downto 0;
    ls_sc_data_write_wire   : out std_logic_vector(Data_Width-1 downto 0);
    ls_sc_read_addr         : out std_logic_vector(Addr_Width-(SIMD_BITS+3) downto 0);
    ls_sc_write_addr        : out std_logic_vector(Addr_Width-(SIMD_BITS+3) downto 0);
    ls_sci_req              : out std_logic_vector(SPM_NUM-1 downto 0);
    ls_sci_we               : out std_logic_vector(SPM_NUM-1 downto 0);
    kmemld_inflight         : out std_logic_vector(SPM_NUM-1 downto 0);
    kmemstr_inflight        : out std_logic_vector(SPM_NUM-1 downto 0);
    ls_sc_data_read_wire    : in  std_logic_vector(Data_Width-1 downto 0);
    ls_sci_wr_gnt           : in  std_logic;
    ls_data_gnt_i           : in  std_logic_vector(SPM_NUM-1 downto 0);
    dsp_taken_branch        : in  std_logic_vector(ACCL_NUM-1 downto 0);
    dsp_except_condition    : in  std_logic_vector(ACCL_NUM-1 downto 0)
    );

end entity klessydra_m_core;

architecture Klessydra_M of klessydra_m_core is

  constant RF_SIZE             : natural := 32-16*RV32E;
  constant RF_CEIL             : natural := integer(ceil(log2(real(RF_SIZE))));
  constant TPS_CEIL            : natural := integer(ceil(log2(real(THREAD_POOL_SIZE))));
  constant TPS_BUF_CEIL        : natural := integer(ceil(log2(real(THREAD_POOL_SIZE)))); -- AAA equal as the signal aboe remove it 
  constant TPS_GLBL_CEIL       : natural := integer(ceil(log2(real(THREAD_POOL_SIZE_GLOBAL))));
  constant SIMD_Width          : natural := SIMD*Data_Width;

  subtype harc_range is natural range THREAD_POOL_SIZE-1 downto 0;  -- will be used replicated units in the core

  constant FU_NUM   : natural := (ACCL_NUM-(ACCL_NUM-1)*(multithreaded_accl_en));

  subtype accl_range is integer range ACCL_NUM - 1 downto 0;  -- will be used replicated accelerators in the core 
  subtype fu_range   is integer range FU_NUM - 1 downto 0; -- will be used replicated accelerators in the core 

  -- Control Status Register (CSR) signals
  signal MHARTID     : array_2d(harc_range)(9  downto 0);
  signal MSTATUS     : array_2d(harc_range)(1 downto 0);
  signal MEPC        : array_2d(harc_range)(31 downto 0);
  signal MCAUSE      : array_2d(harc_range)(31 downto 0);
  signal MIP         : array_2d(harc_range)(31 downto 0);
  signal MPIP        : array_2d(harc_range)(THREAD_POOL_SIZE_GLOBAL-1 downto 0);
  signal MTVEC       : array_2d(harc_range)(31 downto 0);
  signal PCER        : array_2d(harc_range)(31 downto 0);

  signal sw_irq          : std_logic_vector(THREAD_POOL_SIZE_GLOBAL-1 downto 0);
  signal sw_irq_pending  : std_logic_vector(THREAD_POOL_SIZE_GLOBAL-1 downto 0);
  signal irq_pending     : std_logic_vector(harc_range);
  signal WFI_Instr       : std_logic;
  signal except_pc_vec_o : std_logic_vector(31 downto 0);

  -- Memory fault signals
  signal load_err, store_err : std_logic;

  -- Interface signals from EXEC unit to CSR management unit
  signal csr_instr_req       : std_logic;
  signal csr_instr_done      : std_logic;
  signal csr_access_denied_o : std_logic;
  signal csr_wdata_i         : std_logic_vector(31 downto 0);
  signal csr_op_i            : std_logic_vector(2 downto 0);
  signal csr_rdata_o         : std_logic_vector(31 downto 0);
  signal csr_addr_i          : std_logic_vector(11 downto 0);

  -- program counters --
  signal pc_IF     : std_logic_vector(31 downto 0);  -- pc_IF is the actual pc
  signal pc_ID     : std_logic_vector(31 downto 0);  -- pc_ID is the orogram counter of the Decode stage

  -- instruction register and instr. propagation registers --
  signal instr_word_IE    : std_logic_vector(31 downto 0);
  signal instr_rvalid_IE  : std_logic;  -- validity bit at IE input

  -- pc updater signals
  signal served_ie_except_condition      : std_logic_vector(harc_range);
  signal served_ls_except_condition      : std_logic_vector(harc_range);
  signal served_dsp_except_condition     : std_logic_vector(harc_range);
  signal served_except_condition         : std_logic_vector(harc_range);
  signal served_mret_condition           : std_logic_vector(harc_range);
  signal served_irq                      : std_logic_vector(harc_range);
  signal served_pending_irq              : std_logic_vector(harc_range);
  signal taken_branch_pending            : std_logic_vector(harc_range);
  signal ie_except_data                  : std_logic_vector(31 downto 0);
  signal ls_except_data                  : std_logic_vector(31 downto 0);
  signal dsp_except_data                 : array_2d(accl_range)(31 downto 0);
  signal taken_branch                    : std_logic;
  signal ie_taken_branch                 : std_logic;
  signal ls_taken_branch                 : std_logic;
  signal set_branch_condition            : std_logic;
  signal ie_except_condition             : std_logic;
  signal ls_except_condition             : std_logic;
  signal set_except_condition            : std_logic;
  signal set_mret_condition              : std_logic;
  signal absolute_address                : std_logic_vector(31 downto 0);
  signal PC_offset                       : std_logic_vector(31 downto 0);
  signal pc_except_value                 : array_2d(harc_range)(31 downto 0);
  signal pc_except_value_wire            : array_2d(harc_range)(31 downto 0);
  signal incremented_pc                  : array_2d(harc_range)(31 downto 0);
  signal relative_to_PC                  : array_2d(harc_range)(31 downto 0);
  signal absolute_jump                   : std_logic_vector(harc_range);
  signal data_we_o_lat                   : std_logic;
  signal misaligned_err                  : std_logic;
  signal PC_offset_ID                    : std_logic_vector(31 downto 0);
  signal set_branch_condition_ID         : std_logic;
  signal branch_addr_FETCH               : std_logic_vector(31 downto 0);
  signal jump_addr_FETCH                 : std_logic_vector(31 downto 0);
  signal jalr_addr_FETCH                 : std_logic_vector(31 downto 0);
  signal branch_FETCH                    : std_logic;
  signal jump_FETCH                      : std_logic;
  signal jalr_FETCH                      : std_logic;
  signal harc_sleep_wire                 : std_logic_vector(harc_range);
  signal harc_sleep                      : std_logic_vector(harc_range);
  signal wfi_hart_wire                   : std_logic_vector(harc_range);
  signal wfi_hart                        : std_logic_vector(harc_range);
  signal ext_sw_irq_het_core             : std_logic_vector(harc_range);
  signal latch_count_sw_irq              : std_logic_vector(harc_range); -- AAA check the minimum number of latch cycles we should add
  signal CORE_INACTIVE                   : std_logic;
  signal core_enable_i                   : std_logic;

  -- AAA check if we need these signals
  -- signals for counting intructions
  --signal clock_cycle         : std_logic_vector(63 downto 0);  -- RDCYCLE
  --signal external_counter    : std_logic_vector(63 downto 0);  -- RDTIME
  --signal instruction_counter : std_logic_vector(63 downto 0);  -- RDINSTRET

  -- regfile replicated array
  signal regfile            : array_3d(harc_range)(RF_SIZE-1 downto 0)(31 downto 0);

  --signal used by counters
  signal set_wfi_condition          : std_logic;
  signal harc_to_csr                : natural range THREAD_POOL_SIZE_GLOBAL-1 downto 0;
  signal jump_instr                 : std_logic;
  signal jump_instr_lat             : std_logic;
  signal branch_instr               : std_logic;
  signal branch_instr_lat           : std_logic;

  -- auxiliary data memory interface signals
  signal data_addr_internal     : std_logic_vector(31 downto 0);
  signal data_be_internal       : std_logic_vector(3 downto 0);

  --Debug Unit signal and state
  signal ebreak_instr    : std_logic;

  -- hardware context id at fetch, and propagated hardware context ids
  --signal harc_count            : harc_min_range;
  signal harc_IF         : harc_range;
  signal harc_FETCH      : harc_range;
  signal harc_ID         : harc_range;

  signal halt_update     : std_logic_vector(harc_range);

  signal hart_sleep_count        : std_logic_vector(TPS_CEIL-1 downto 0); -- number of sleeping harts
  signal hart_sleep_count_wire   : std_logic_vector(TPS_CEIL-1 downto 0);
  signal wfi_count               : std_logic_vector(TPS_CEIL-1 downto 0); -- number of wfi harts
  signal wfi_count_wire          : std_logic_vector(TPS_CEIL-1 downto 0);
  signal CORE_STATE              : std_logic_vector(THREAD_POOL_BASELINE downto 0);
  signal IMT_ACTIVE_HARTS        : natural;
  signal MORPH_ACTIVE_HARTS      : natural;

  function and_const(a: natural; b: natural) return natural is
    variable c : natural;
  begin
    if a = b then
      c := 1;
    else
      c := 0;
    end if;
    return c;
  end function and_const;

  constant HET_CLUSTER_S1_CORE   : natural := and_const(THREAD_POOL_SIZE, context_switch);
  signal block_input_inst_wire   : std_logic;
  signal block_input_inst        : std_logic;

  signal source_hartid_lat_wire  : natural range THREAD_POOL_SIZE_GLOBAL-1 downto 0;
  signal source_hartid_lat       : natural range THREAD_POOL_SIZE_GLOBAL-1 downto 0;
  signal served_irq_lat          : std_logic_vector(harc_range);

  -- This function increments all the bits in a std_logic_vector
  function add_vect_bits(v: std_logic_vector) return natural is
    variable h: natural;
  begin
    h := 0;
    for i in v'range loop
      if v(i) = '1' then
        h := h + 1;
      end if;
    end loop;
    return h;
  end function add_vect_bits;

  component Program_Counter
  generic (
    THREAD_POOL_SIZE_GLOBAL           : natural;
    THREAD_POOL_SIZE                  : natural;
    HET_CLUSTER_S1_CORE               : natural;
    ACCL_NUM                          : natural;
    morph_en                          : natural
  );
  port (
    absolute_jump                     : in  std_logic_vector(harc_range);
    data_we_o_lat                     : in  std_logic;
    absolute_address                  : in  std_logic_vector(31 downto 0);
    PC_offset                         : in  std_logic_vector(31 downto 0);
    taken_branch                      : in  std_logic;
    ie_taken_branch                   : in  std_logic;
    ls_taken_branch                   : in  std_logic;
    dsp_taken_branch                  : in  std_logic_vector(accl_range);
    set_branch_condition              : in  std_logic;
    ie_except_condition               : in  std_logic;
    ls_except_condition               : in  std_logic;
    dsp_except_condition              : in  std_logic_vector(accl_range);
    set_except_condition              : in  std_logic;
    set_mret_condition                : in  std_logic;
    set_wfi_condition                 : in  std_logic;
    harc_FETCH                        : in  harc_range;
    harc_ID                           : in  harc_range;
    harc_EXEC                         : in  natural range THREAD_POOL_SIZE-1 downto 0;
    instr_rvalid_IE                   : in  std_logic;
    pc_ID                             : in  std_logic_vector(31 downto 0);
    pc_IE                             : in  std_logic_vector(31 downto 0);
    MSTATUS                           : in  array_2d(harc_range)(1 downto 0);
    MIP, MEPC, MCAUSE, MTVEC          : in  array_2d(harc_range)(31 downto 0);
    instr_word_IE                     : in  std_logic_vector(31 downto 0);
    pc_IF                             : out std_logic_vector(31 downto 0);
    harc_IF                           : out harc_range;
    served_ie_except_condition        : out std_logic_vector(harc_range);
    served_ls_except_condition        : out std_logic_vector(harc_range);
    served_dsp_except_condition       : out std_logic_vector(harc_range);
    served_except_condition           : out std_logic_vector(harc_range);
    served_mret_condition             : out std_logic_vector(harc_range);
    served_irq                        : in  std_logic_vector(harc_range);
    taken_branch_pending              : out std_logic_vector(harc_range);
    incremented_pc                    : out array_2d(harc_range)(31 downto 0);
    irq_pending                       : out std_logic_vector(harc_range);
    harc_sleep_wire                   : out std_logic_vector(harc_range);
    harc_sleep                        : out std_logic_vector(harc_range);
    wfi_hart_wire                     : out std_logic_vector(harc_range);
    wfi_hart                          : out std_logic_vector(harc_range);
    ext_sw_irq_het_core               : in  std_logic_vector(harc_range);
    CORE_STATE                        : in  std_logic_vector(THREAD_POOL_BASELINE downto 0);
    CORE_INACTIVE                     : in  std_logic;
    halt_update                       : in  std_logic_vector(harc_range);
    PC_offset_ID                      : in  std_logic_vector(31 downto 0);
    set_branch_condition_ID           : in  std_logic;
    branch_addr_FETCH                 : in  std_logic_vector(31 downto 0);
    jump_addr_FETCH                   : in  std_logic_vector(31 downto 0);
    jalr_addr_FETCH                   : in  std_logic_vector(31 downto 0);
    branch_FETCH                      : in  std_logic;
    jump_FETCH                        : in  std_logic;
    jalr_FETCH                        : in  std_logic;
    clk_i                             : in  std_logic;
    rst_ni                            : in  std_logic;
    source_hartid_i                   : in  natural range THREAD_POOL_SIZE_GLOBAL-1 downto 0; -- used to overwrite the mhartID of the core doing the context switch
    irq_i                             : in  std_logic;
    fetch_enable_i                    : in  std_logic;
    boot_addr_i                       : in  std_logic_vector(31 downto 0);
    instr_gnt_i                       : in  std_logic
    );
  end component;

  component CSR_Unit
  generic (
    THREAD_POOL_SIZE_GLOBAL     : natural;
    THREAD_POOL_SIZE            : natural;
    HET_CLUSTER_S1_CORE         : natural;
    ACCL_NUM                    : natural;
    Addr_Width                  : natural;
    replicate_accl_en           : natural;
    accl_en                     : natural;
    MCYCLE_EN                   : natural;
    MINSTRET_EN                 : natural;
    MHPMCOUNTER_EN              : natural;
    RF_CEIL                     : natural;
    TPS_GLBL_CEIL               : natural;
    count_all                   : natural
  );
  port (
    pc_IE                       : in  std_logic_vector(31 downto 0);
    ie_except_data              : in  std_logic_vector(31 downto 0);
    ls_except_data              : in  std_logic_vector(31 downto 0);
    dsp_except_data             : in  array_2d(accl_range)(31 downto 0);
    served_ie_except_condition  : in  std_logic_vector(harc_range);
    served_ls_except_condition  : in  std_logic_vector(harc_range);
    served_dsp_except_condition : in  std_logic_vector(harc_range);
    harc_sleep                  : in  std_logic_vector(harc_range);
    harc_EXEC                   : in  natural range THREAD_POOL_SIZE-1 downto 0;
    harc_to_csr                 : in  natural range THREAD_POOL_SIZE_GLOBAL-1 downto 0;
    instr_word_IE               : in  std_logic_vector(31 downto 0);
    served_except_condition     : in  std_logic_vector(harc_range);
    served_mret_condition       : in  std_logic_vector(harc_range);
    served_irq                  : in  std_logic_vector(harc_range);
    served_pending_irq          : in  std_logic_vector(harc_range);
    pc_except_value_wire        : in  array_2d(harc_range)(31 downto 0);
    data_addr_internal          : in  std_logic_vector(31 downto 0);
    jump_instr                  : in  std_logic;
    branch_instr                : in  std_logic;
    set_branch_condition        : in  std_logic;
    csr_instr_req               : in  std_logic;
    misaligned_err              : in  std_logic;
    WFI_Instr                   : in  std_logic;
    csr_wdata_i                 : in  std_logic_vector(31 downto 0);
    csr_op_i                    : in  std_logic_vector(2  downto 0);
    csr_addr_i                  : in  std_logic_vector(11 downto 0);
    csr_instr_done              : out std_logic;
    csr_access_denied_o         : out std_logic;
    csr_rdata_o                 : out std_logic_vector (31 downto 0);
    MVSIZE                      : out array_2d(harc_range)(Addr_Width downto 0);
    MVTYPE                      : out array_2d(harc_range)(3 downto 0);
    MPSCLFAC                    : out array_2d(harc_range)(4 downto 0);
    MHARTID                     : out array_2d(harc_range)(9 downto 0);
    MSTATUS                     : out array_2d(harc_range)(1 downto 0);
    MEPC                        : out array_2d(harc_range)(31 downto 0);
    MCAUSE                      : out array_2d(harc_range)(31 downto 0);
    MIP                         : out array_2d(harc_range)(31 downto 0);
    MPIP                        : out array_2d(harc_range)(THREAD_POOL_SIZE_GLOBAL-1 downto 0);
    MTVEC                       : out array_2d(harc_range)(31 downto 0);
    PCER                        : out array_2d(harc_range)(31 downto 0);
    fetch_enable_i              : in  std_logic;
    clk_i                       : in  std_logic;
    rst_ni                      : in  std_logic;
    core_id_i                   : in  std_logic_vector(3 downto 0);
    cluster_id_i                : in  std_logic_vector(5 downto 0);
    instr_rvalid_i              : in  std_logic;
    instr_rvalid_IE             : in  std_logic;
    data_we_o                   : in  std_logic;
    data_req_o                  : in  std_logic;
    data_gnt_i                  : in  std_logic;
    irq_i                       : in  std_logic;
    irq_id_i                    : in  std_logic_vector(4 downto 0);
    irq_id_o                    : out std_logic_vector(4 downto 0);
    irq_ack_o                   : out std_logic;
    ext_sw_irq_het_core         : in  std_logic_vector(THREAD_POOL_SIZE-1 downto 0);
    sw_irq                      : in  std_logic_vector(THREAD_POOL_SIZE_GLOBAL-1 downto 0);
    sw_irq_i                    : in  std_logic_vector(THREAD_POOL_SIZE_GLOBAL-1 downto 0);
    sw_irq_pending              : in  std_logic_vector(THREAD_POOL_SIZE_GLOBAL-1 downto 0);
    source_hartid_i             : in  natural range THREAD_POOL_SIZE_GLOBAL-1 downto 0
    );
  end component;

  component Pipeline
  generic(
    THREAD_POOL_SIZE_GLOBAL    : natural;
    THREAD_POOL_SIZE           : natural;
    HET_CLUSTER_S1_CORE        : natural;
    lutram_rf                  : natural;
    latch_rf                   : natural;
    RV32E                      : natural;
    RV32M                      : natural;
    context_switch             : natural;
    morph_en                   : natural;
    fetch_stage_en             : natural;
    branch_predict_en          : natural;
    btb_en                     : natural;
    btb_len                    : natural;
    superscalar_exec_en        : natural;
    accl_en                    : natural;
    replicate_accl_en          : natural;
    multithreaded_accl_en      : natural;
    SPM_NUM                    : natural;  
    Addr_Width                 : natural;
    SPM_STRT_ADDR              : std_logic_vector(31 downto 0);
    SIMD                       : natural;
    MCYCLE_EN                  : natural;
    MINSTRET_EN                : natural;
    MHPMCOUNTER_EN             : natural;
    count_all                  : natural;
    debug_en                   : natural;
    tracer_en                  : natural;
    -------------------------------------
    ACCL_NUM                   : natural;
    FU_NUM                     : natural;
    RF_SIZE                    : natural;
    RF_CEIL                    : natural;
    TPS_CEIL                   : natural;
    TPS_BUF_CEIL               : natural;
    SPM_ADDR_WID               : natural;
    SIMD_BITS                  : natural;
    Data_Width                 : natural;
    SIMD_Width                 : natural
    );
  port (
    pc_IF                      : in  std_logic_vector(31 downto 0);
    harc_IF                    : in  harc_range;
    irq_pending                : in  std_logic_vector(harc_range);
    csr_instr_done             : in  std_logic;
    csr_access_denied_o        : in  std_logic;
    csr_rdata_o                : in  std_logic_vector (31 downto 0);
    MVSIZE                     : in  array_2d(harc_range)(Addr_Width downto 0);
    MVTYPE                     : in  array_2d(harc_range)(3 downto 0);
    MPSCLFAC                   : in  array_2d(harc_range)(4 downto 0);
    MHARTID                    : in  array_2d(harc_range)(9  downto 0);
    MSTATUS                    : in  array_2d(harc_range)(1 downto 0);
    MPIP                       : in  array_2d(THREAD_POOL_SIZE-1 downto 0)(THREAD_POOL_SIZE_GLOBAL-1 downto 0);
    PCER                       : in  array_2d(harc_range)(31 downto 0);
    block_input_inst_wire      : in  std_logic;
    block_input_inst           : in  std_logic;
    served_irq                 : out std_logic_vector(harc_range);
    served_pending_irq         : out std_logic_vector(harc_range);
    WFI_Instr                  : out std_logic;
    misaligned_err             : out std_logic;
    pc_ID                      : out std_logic_vector(31 downto 0);
    pc_IE                      : out std_logic_vector(31 downto 0);
    ie_except_data             : out std_logic_vector(31 downto 0);
    ls_except_data             : out std_logic_vector(31 downto 0);
    dsp_except_data            : out array_2d(accl_range)(31 downto 0);
    taken_branch               : out std_logic;
    ie_taken_branch            : out std_logic;
    ls_taken_branch            : out std_logic;
    dsp_taken_branch           : in  std_logic_vector(accl_range);
    set_branch_condition       : out std_logic;
    set_except_condition       : out std_logic;        
    ie_except_condition        : out std_logic;
    ls_except_condition        : out std_logic;
    dsp_except_condition       : in  std_logic_vector(accl_range);
    set_mret_condition         : out std_logic;
    set_wfi_condition          : out std_logic;
    csr_instr_req              : out std_logic;
    instr_rvalid_IE            : out std_logic;  -- validity bit at IE input
    csr_addr_i                 : out std_logic_vector (11 downto 0);
    csr_wdata_i                : out std_logic_vector (31 downto 0);
    csr_op_i                   : out std_logic_vector (2 downto 0);
    jump_instr                 : out std_logic;
    jump_instr_lat             : out std_logic;
    branch_instr               : out std_logic;
    branch_instr_lat           : out std_logic;
    harc_FETCH                 : out harc_range;
    harc_ID                    : out harc_range;
    harc_EXEC                  : out natural range THREAD_POOL_SIZE-1 downto 0;
    harc_to_csr                : out natural range THREAD_POOL_SIZE_GLOBAL-1 downto 0;
    instr_word_IE              : out std_logic_vector(31 downto 0);
    PC_offset                  : out std_logic_vector(31 downto 0);
    absolute_address           : out std_logic_vector(31 downto 0);
    ebreak_instr               : out std_logic;
    data_addr_internal         : out std_logic_vector(31 downto 0);
    absolute_jump              : out std_logic_vector(harc_range);
    regfile                    : out array_3d(harc_range)(RF_SIZE-1 downto 0)(31 downto 0);
    PC_offset_ID               : out std_logic_vector(31 downto 0);
    set_branch_condition_ID    : out std_logic;
    branch_FETCH               : out std_logic;
    jump_FETCH                 : out std_logic;
    jalr_FETCH                 : out std_logic;
    branch_addr_FETCH          : out std_logic_vector(31 downto 0);
    jump_addr_FETCH            : out std_logic_vector(31 downto 0);
    jalr_addr_FETCH            : out std_logic_vector(31 downto 0);
    harc_sleep_wire            : in  std_logic_vector(harc_range);
    harc_sleep                 : in  std_logic_vector(harc_range);
    wfi_hart_wire              : in  std_logic_vector(harc_range);
    CORE_STATE                 : in  std_logic_vector(THREAD_POOL_BASELINE downto 0);
    IMT_ACTIVE_HARTS           : in  natural;
    halt_update                : out std_logic_vector(harc_range);

    -- clock, reset active low, test enable
    clk_i                      : in  std_logic;
    rst_ni                     : in  std_logic;
    -- program memory interface
    instr_req_o                : out std_logic;
    instr_gnt_i                : in  std_logic;
    instr_rvalid_i             : in  std_logic;
    instr_rdata_i              : in  std_logic_vector(31 downto 0);
    -- data memory interface
    data_req_o                 : out std_logic;
    data_gnt_i                 : in  std_logic;
    data_rvalid_i              : in  std_logic;
    data_we_o                  : out std_logic;
    data_be_o                  : out std_logic_vector(3 downto 0);
    data_addr_o                : out std_logic_vector(31 downto 0);
    data_wdata_o               : out std_logic_vector(31 downto 0);
    data_rdata_i               : in  std_logic_vector(31 downto 0);
    data_err_i                 : in  std_logic;
    -- interrupt request interface
    irq_i                      : in  std_logic;
    -- miscellanous control signals
    fetch_enable_i             : in  std_logic;
    core_busy_o                : out std_logic;
    -- klessydra-specific signals
    core_enable_i              : in  std_logic;
    source_hartid_o            : out natural range THREAD_POOL_SIZE_GLOBAL-1 downto 0;
    sw_irq                     : out std_logic_vector(THREAD_POOL_SIZE_GLOBAL-1 downto 0);
    sw_irq_served_i            : in  std_logic_vector(THREAD_POOL_SIZE-1 downto 0);
    sw_irq_served_o            : out std_logic_vector(THREAD_POOL_SIZE_GLOBAL-1 downto 0);
    sw_irq_pending             : out std_logic_vector(THREAD_POOL_SIZE_GLOBAL-1 downto 0);
    -- VCU Signals
    rs1_to_sc                  : out std_logic_vector(SPM_ADDR_WID-1 downto 0);
    rs2_to_sc                  : out std_logic_vector(SPM_ADDR_WID-1 downto 0);
    rd_to_sc                   : out std_logic_vector(SPM_ADDR_WID-1 downto 0);
    decoded_instruction_DSP    : out std_logic_vector(DSP_UNIT_INSTR_SET_SIZE-1 downto 0);
    RS1_Data_IE                : out std_logic_vector(31 downto 0);
    RS2_Data_IE                : out std_logic_vector(31 downto 0);
    RD_Data_IE                 : out std_logic_vector(31 downto 0);  -- unused
    dsp_instr_req              : out std_logic_vector(ACCL_NUM-1 downto 0);
    spm_rs1                    : out std_logic;
    spm_rs2                    : out std_logic;
    vec_read_rs1_ID            : out std_logic;
    vec_read_rs2_ID            : out std_logic;
    vec_write_rd_ID            : out std_logic;
    busy_DSP                   : in  std_logic_vector(ACCL_NUM-1 downto 0);
    state_LS                   : out fsm_LS_states;
    sc_word_count_wire         : out integer;
    spm_bcast                  : out std_logic;
    harc_LS_wire               : out integer range ACCL_NUM-1 downto 0;
    ls_sc_data_write_wire      : out std_logic_vector(Data_Width-1 downto 0);
    ls_sc_read_addr            : out std_logic_vector(Addr_Width-(SIMD_BITS+3) downto 0);
    ls_sc_write_addr           : out std_logic_vector(Addr_Width-(SIMD_BITS+3) downto 0);
    ls_sci_req                 : out std_logic_vector(SPM_NUM-1 downto 0);
    ls_sci_we                  : out std_logic_vector(SPM_NUM-1 downto 0);
    kmemld_inflight            : out std_logic_vector(SPM_NUM-1 downto 0);
    kmemstr_inflight           : out std_logic_vector(SPM_NUM-1 downto 0);
    ls_sc_data_read_wire       : in  std_logic_vector(Data_Width-1 downto 0);
    ls_sci_wr_gnt              : in  std_logic;
    ls_data_gnt_i              : in  std_logic_vector(SPM_NUM-1 downto 0)
  );
  end component;

--------------------------------------------------------------------------------------------------
----------------------- ARCHITECTURE BEGIN -------------------------------------------------------              
begin

  core_enable_i <= '1' when (core_select = 1 and HET_CLUSTER_S1_CORE = 1) or    -- S1 is enabled
                            (core_select = 0 and HET_CLUSTER_S1_CORE = 0) else  -- T13 is enabled
                   '0';                                                         -- either S1 or T13 is disabled

  sw_irq_o <= sw_irq;

  hart_sleep_count_wire <= std_logic_vector(to_unsigned(add_vect_bits(harc_sleep_wire),TPS_CEIL));
  wfi_count_wire        <= std_logic_vector(to_unsigned(add_vect_bits(wfi_hart_wire),TPS_CEIL));
  MORPH_ACTIVE_HARTS    <= THREAD_POOL_SIZE-to_integer(unsigned(hart_sleep_count_wire));
  IMT_ACTIVE_HARTS      <= THREAD_POOL_SIZE-to_integer(unsigned(wfi_count_wire));
  CORE_INACTIVE         <= '1' when ((IMT_ACTIVE_HARTS = 0 and morph_en = 0) or -- AAA S1 core probably should be added here
                                    (MORPH_ACTIVE_HARTS = 0 and morph_en = 1)) and
                                    context_switch = 1 else '0';

  process(all)
  begin
    source_hartid_lat_wire <= source_hartid_lat;
    for i in 0 to THREAD_POOL_SIZE_GLOBAL-1 loop
      if (context_switch = 1 and sw_irq_i(i) = '1') then
        source_hartid_lat_wire <= source_hartid_i;
      end if;
    end loop;
    for i in 0 to THREAD_POOL_SIZE-1 loop
      if (context_switch = 1 and (sw_irq_i(i) = '1' or latch_count_sw_irq(i) = '1')  and i = source_hartid_lat_wire) then  -- only the source_hartid can go to the interrupt routine
        ext_sw_irq_het_core(i) <= '1';
      else
        ext_sw_irq_het_core(i) <= '0';
      end if;
    end loop;
    if HET_CLUSTER_S1_CORE = 1 then
      if sw_irq_i(THREAD_POOL_SIZE_GLOBAL-1) = '1' or latch_count_sw_irq(0) = '1' then
        ext_sw_irq_het_core(0) <= '1';
      else
        ext_sw_irq_het_core(0) <= '0';
      end if;
    end if;
    block_input_inst_wire <= '0';
    for i in 0 to THREAD_POOL_SIZE-1 loop
      if (context_switch = 1 and sw_irq_i(i) = '1') then
        block_input_inst_wire <= '1'; -- blocks the input instruction for one cycle as it comes from another hart
      end if;
    end loop;
  end process;

  process(clk_i, rst_ni)
  begin
    if rst_ni = '0' then
      latch_count_sw_irq <= (others => '0');
      block_input_inst   <= '0';
      source_hartid_lat  <= 0;
      served_irq_lat     <= (others => '0');
    elsif rising_edge(clk_i) then
      served_irq_lat     <= served_irq;
      block_input_inst   <= block_input_inst_wire;
      source_hartid_lat  <= source_hartid_lat_wire;
      for i in 0 to THREAD_POOL_SIZE-1 loop
        if (context_switch = 1 and sw_irq_i(i) = '1'  and i = source_hartid_lat_wire) then
          latch_count_sw_irq(i) <= latch_count_sw_irq(i) xor '1'; -- like a 1-bit adder without a carry_out
        elsif served_irq_lat(i) = '1' then
          latch_count_sw_irq(i) <= '0';
        end if;
      end loop;
      if HET_CLUSTER_S1_CORE = 1 then
        if sw_irq_i(3) = '1' then
          latch_count_sw_irq(0) <= latch_count_sw_irq(0) xor '1'; -- like a 1-bit adder without a carry_out
        elsif served_irq_lat(0) = '1' then
          latch_count_sw_irq(0) <= '0';
        end if;
      end if;
    end if;
  end process;

  process(clk_i, rst_ni)
  begin
    if rst_ni = '0' then
      hart_sleep_count <= (others => '0');
      wfi_count        <= (others => '0');
      CORE_STATE       <= '1' & (0 to THREAD_POOL_BASELINE-1 => '0');
    elsif rising_edge(clk_i) then
      hart_sleep_count <= hart_sleep_count_wire;
      wfi_count        <= wfi_count_wire;
      CORE_STATE       <= (others => '0');
      if MORPH_ACTIVE_HARTS >= THREAD_POOL_BASELINE then
        CORE_STATE(IMT_MODE) <= '1';
      else
        CORE_STATE(MORPH_ACTIVE_HARTS) <= '1';
      end if;
    end if;
  end process;

  assert (lutram_rf /= debug_en and lutram_rf /= 1) report "Debug-Unit cannot read from a LUTRAM regfile." severity WARNING;

  instr_addr_o <= pc_IF;

  process(all)
  begin
  pc_except_value_wire <= pc_except_value;
    if set_except_condition  = '1' then
      pc_except_value_wire(harc_EXEC) <=  pc_IE;    
    end if;
  end process;

  process(clk_i, rst_ni)
  begin
    if rst_ni = '0' then
    elsif rising_edge(clk_i) then
      pc_except_value <= pc_except_value_wire;
    end if;
  end process;

  Prg_Ctr : Program_Counter
    generic map (
      THREAD_POOL_SIZE_GLOBAL     => THREAD_POOL_SIZE_GLOBAL,
      THREAD_POOL_SIZE            => THREAD_POOL_SIZE,
      HET_CLUSTER_S1_CORE         => HET_CLUSTER_S1_CORE,
      ACCL_NUM                    => ACCL_NUM,
      morph_en                    => morph_en
      )
    port map(
      absolute_jump               => absolute_jump,
      data_we_o_lat               => data_we_o_lat,
      absolute_address            => absolute_address,       
      PC_offset                   => PC_offset,
      taken_branch                => taken_branch,
      ie_taken_branch             => ie_taken_branch,
      ls_taken_branch             => ls_taken_branch,
      dsp_taken_branch            => dsp_taken_branch,
      set_branch_condition        => set_branch_condition,
      ie_except_condition         => ie_except_condition,
      ls_except_condition         => ls_except_condition,
      dsp_except_condition        => dsp_except_condition, 
      set_except_condition        => set_except_condition,
      set_mret_condition          => set_mret_condition,
      set_wfi_condition           => set_wfi_condition,
      harc_FETCH                  => harc_FETCH,
      harc_ID                     => harc_ID,
      harc_EXEC                   => harc_EXEC,
      instr_rvalid_IE             => instr_rvalid_IE,
      pc_ID                       => pc_ID,
      pc_IE                       => pc_IE,
      MIP                         => MIP,
      MEPC                        => MEPC,
      MSTATUS                     => MSTATUS,
      MCAUSE                      => MCAUSE,
      MTVEC                       => MTVEC,
      instr_word_IE               => instr_word_IE,
      pc_IF                       => pc_IF,
      harc_IF                     => harc_IF,
      served_ie_except_condition  => served_ie_except_condition,
      served_ls_except_condition  => served_ls_except_condition,
      served_dsp_except_condition => served_dsp_except_condition,
      served_except_condition     => served_except_condition,
      served_mret_condition       => served_mret_condition,
      served_irq                  => served_irq,
      taken_branch_pending        => taken_branch_pending,
      incremented_pc              => incremented_pc,
      irq_pending                 => irq_pending,
      harc_sleep_wire             => harc_sleep_wire,
      harc_sleep                  => harc_sleep,
      wfi_hart_wire               => wfi_hart_wire,
      wfi_hart                    => wfi_hart,
      ext_sw_irq_het_core         => ext_sw_irq_het_core,
      CORE_STATE                  => CORE_STATE,
      CORE_INACTIVE               => CORE_INACTIVE,
      halt_update                 => halt_update,
      PC_offset_ID                => PC_offset_ID,
      set_branch_condition_ID     => set_branch_condition_ID,
      branch_addr_FETCH           => branch_addr_FETCH,
      jump_addr_FETCH             => jump_addr_FETCH, 
      jalr_addr_FETCH             => jalr_addr_FETCH,
      branch_FETCH                => branch_FETCH,
      jump_FETCH                  => jump_FETCH,
      jalr_FETCH                  => jalr_FETCH,
      clk_i                       => clk_i,
      rst_ni                      => rst_ni,
      irq_i                       => irq_i,
      source_hartid_i             => source_hartid_i, 
      fetch_enable_i              => fetch_enable_i,
      boot_addr_i                 => boot_addr_i,
      instr_gnt_i                 => instr_gnt_i
      );

  CSR : CSR_Unit
    generic map (
      THREAD_POOL_SIZE_GLOBAL     => THREAD_POOL_SIZE_GLOBAL,
      THREAD_POOL_SIZE            => THREAD_POOL_SIZE,
      HET_CLUSTER_S1_CORE         => HET_CLUSTER_S1_CORE,
      ACCL_NUM                    => ACCL_NUM,
      Addr_Width                  => Addr_Width,
      replicate_accl_en           => replicate_accl_en,
      accl_en                     => accl_en,
      MCYCLE_EN                   => MCYCLE_EN,
      MINSTRET_EN                 => MINSTRET_EN,
      MHPMCOUNTER_EN              => MHPMCOUNTER_EN,
      RF_CEIL                     => RF_CEIL,
      TPS_GLBL_CEIL               => TPS_GLBL_CEIL,
      count_all                   => count_all
    )
    port map(
      pc_IE                       => pc_IE,
      ie_except_data              => ie_except_data,
      ls_except_data              => ls_except_data,
      dsp_except_data             => dsp_except_data,
      served_ie_except_condition  => served_ie_except_condition,
      served_ls_except_condition  => served_ls_except_condition,
      served_dsp_except_condition => served_dsp_except_condition,
      harc_sleep                  => harc_sleep,
      harc_EXEC                   => harc_EXEC,
      harc_to_csr                 => harc_to_csr,
      instr_word_IE               => instr_word_IE,
      served_except_condition     => served_except_condition,
      served_mret_condition       => served_mret_condition,
      served_irq                  => served_irq,
      served_pending_irq          => served_pending_irq,
      pc_except_value_wire        => pc_except_value_wire,
      data_addr_internal          => data_addr_internal,
      jump_instr                  => jump_instr,
      branch_instr                => branch_instr,
      set_branch_condition        => set_branch_condition,
      csr_instr_req               => csr_instr_req,
      misaligned_err              => misaligned_err,
      WFI_Instr                   => WFI_Instr,
      csr_wdata_i                 => csr_wdata_i,
      csr_op_i                    => csr_op_i,
      csr_addr_i                  => csr_addr_i,
      csr_instr_done              => csr_instr_done,
      csr_access_denied_o         => csr_access_denied_o,
      csr_rdata_o                 => csr_rdata_o,
      MVSIZE                      => MVSIZE,
      MVTYPE                      => MVTYPE,
      MPSCLFAC                    => MPSCLFAC,
      MHARTID                     => MHARTID,
      MSTATUS                     => MSTATUS,
      MEPC                        => MEPC,
      MCAUSE                      => MCAUSE,
      MIP                         => MIP,
      MPIP                        => MPIP,
      MTVEC                       => MTVEC,
      PCER                        => PCER,
      fetch_enable_i              => fetch_enable_i,
      clk_i                       => clk_i,
      rst_ni                      => rst_ni,
      core_id_i                   => core_id_i,
      cluster_id_i                => cluster_id_i,
      instr_rvalid_i              => instr_rvalid_i,
      instr_rvalid_IE             => instr_rvalid_IE,
      data_we_o                   => data_we_o,
      data_req_o                  => data_req_o,
      data_gnt_i                  => data_gnt_i,
      irq_i                       => irq_i,
      irq_id_i                    => irq_id_i,
      irq_id_o                    => irq_id_o,
      irq_ack_o                   => irq_ack_o,
      ext_sw_irq_het_core         => ext_sw_irq_het_core,
      sw_irq                      => sw_irq,
      sw_irq_i                    => sw_irq_i,
      sw_irq_pending              => sw_irq_pending,
      source_hartid_i             => source_hartid_i
      );

  Pipe : Pipeline
    generic map(
      THREAD_POOL_SIZE_GLOBAL => THREAD_POOL_SIZE_GLOBAL,
      THREAD_POOL_SIZE        => THREAD_POOL_SIZE,
      HET_CLUSTER_S1_CORE     => HET_CLUSTER_S1_CORE,
      lutram_rf               => lutram_rf,
      latch_rf                => latch_rf,
      RV32E                   => RV32E,
      RV32M                   => RV32M,
      context_switch          => context_switch,
      morph_en                => morph_en,
      fetch_stage_en          => fetch_stage_en,
      branch_predict_en       => branch_predict_en,
      btb_en                  => btb_en,
      btb_len                 => btb_len,
      superscalar_exec_en     => superscalar_exec_en,
      accl_en                 => accl_en,
      replicate_accl_en       => replicate_accl_en,
      multithreaded_accl_en   => multithreaded_accl_en,
      SPM_NUM                 => SPM_NUM,  
      Addr_Width              => Addr_Width,
      SPM_STRT_ADDR           => SPM_STRT_ADDR,
      SIMD                    => SIMD,
      MCYCLE_EN               => MCYCLE_EN,
      MINSTRET_EN             => MINSTRET_EN,
      MHPMCOUNTER_EN          => MHPMCOUNTER_EN,
      count_all               => count_all,
      debug_en                => debug_en,
      tracer_en               => tracer_en,
      -----------------------------------
      ACCL_NUM                => ACCL_NUM,
      FU_NUM                  => FU_NUM,
      RF_SIZE                 => RF_SIZE,
      RF_CEIL                 => RF_CEIL,
      TPS_CEIL                => TPS_CEIL,
      TPS_BUF_CEIL            => TPS_BUF_CEIL,
      SPM_ADDR_WID            => SPM_ADDR_WID,
      SIMD_BITS               => SIMD_BITS,
      Data_Width              => Data_Width,
      SIMD_Width              => SIMD_Width
      )
    port map(
      pc_IF                      => pc_IF,
      harc_IF                    => harc_IF,
      irq_pending                => irq_pending,
      csr_instr_done             => csr_instr_done,
      csr_access_denied_o        => csr_access_denied_o,
      csr_rdata_o                => csr_rdata_o,
      pc_ID                      => pc_ID,
      pc_IE                      => pc_IE,
      ie_except_data             => ie_except_data,
      ls_except_data             => ls_except_data,
      dsp_except_data            => dsp_except_data,
      MVSIZE                     => MVSIZE,
      MVTYPE                     => MVTYPE,
      MPSCLFAC                   => MPSCLFAC,
      MHARTID                    => MHARTID,
      MSTATUS                    => MSTATUS,
      MPIP                       => MPIP,
      PCER                       => PCER,
      block_input_inst_wire      => block_input_inst_wire,
      block_input_inst           => block_input_inst,
      served_irq                 => served_irq,
      served_pending_irq         => served_pending_irq,
      WFI_Instr                  => WFI_Instr,
      misaligned_err             => misaligned_err,
      taken_branch               => taken_branch,
      ie_taken_branch            => ie_taken_branch,
      ls_taken_branch            => ls_taken_branch,
      dsp_taken_branch           => dsp_taken_branch,
      set_branch_condition       => set_branch_condition,
      set_except_condition       => set_except_condition,
      ie_except_condition        => ie_except_condition,
      ls_except_condition        => ls_except_condition,
      dsp_except_condition       => dsp_except_condition,
      set_mret_condition         => set_mret_condition,
      set_wfi_condition          => set_wfi_condition,
      csr_instr_req              => csr_instr_req,
      instr_rvalid_IE            => instr_rvalid_IE,
      csr_addr_i                 => csr_addr_i,
      csr_wdata_i                => csr_wdata_i,
      csr_op_i                   => csr_op_i,
      jump_instr                 => jump_instr,
      jump_instr_lat             => jump_instr_lat,
      branch_instr               => branch_instr,
      branch_instr_lat           => branch_instr_lat,
      harc_FETCH                 => harc_FETCH,
      harc_ID                    => harc_ID,
      harc_EXEC                  => harc_EXEC,
      harc_to_csr                => harc_to_csr,
      instr_word_IE              => instr_word_IE,
      PC_offset                  => PC_offset,
      absolute_address           => absolute_address,
      ebreak_instr               => ebreak_instr,
      data_addr_internal         => data_addr_internal,
      absolute_jump              => absolute_jump,
      regfile                    => regfile,
      PC_offset_ID               => PC_offset_ID,
      set_branch_condition_ID    => set_branch_condition_ID,
      branch_FETCH               => branch_FETCH,
      jump_FETCH                 => jump_FETCH,
      jalr_FETCH                 => jalr_FETCH,
      branch_addr_FETCH          => branch_addr_FETCH,
      jump_addr_FETCH            => jump_addr_FETCH,
      jalr_addr_FETCH            => jalr_addr_FETCH,
      harc_sleep_wire            => harc_sleep_wire,
      harc_sleep                 => harc_sleep,
      wfi_hart_wire              => wfi_hart_wire,
      CORE_STATE                 => CORE_STATE,
      IMT_ACTIVE_HARTS           => IMT_ACTIVE_HARTS,
      halt_update                => halt_update,
      clk_i                      => clk_i,
      rst_ni                     => rst_ni,
      instr_req_o                => instr_req_o,
      instr_gnt_i                => instr_gnt_i,
      instr_rvalid_i             => instr_rvalid_i,
      instr_rdata_i              => instr_rdata_i,
      data_req_o                 => data_req_o,
      data_gnt_i                 => data_gnt_i,
      data_rvalid_i              => data_rvalid_i,
      data_we_o                  => data_we_o,
      data_be_o                  => data_be_o,
      data_addr_o                => data_addr_o,
      data_wdata_o               => data_wdata_o,
      data_rdata_i               => data_rdata_i,
      data_err_i                 => data_err_i,
      irq_i                      => irq_i,
      fetch_enable_i             => fetch_enable_i,
      core_busy_o                => core_busy_o,
      core_enable_i              => core_enable_i,
      source_hartid_o            => source_hartid_o,
      sw_irq                     => sw_irq,
      sw_irq_served_i            => sw_irq_served_i, 
      sw_irq_served_o            => sw_irq_served_o,
      sw_irq_pending             => sw_irq_pending,
      rs1_to_sc                  => rs1_to_sc,
      rs2_to_sc                  => rs2_to_sc,
      rd_to_sc                   => rd_to_sc,
      decoded_instruction_DSP    => decoded_instruction_DSP,
      RS1_Data_IE                => RS1_Data_IE,
      RS2_Data_IE                => RS2_Data_IE,
      RD_Data_IE                 => RD_Data_IE,
      dsp_instr_req              => dsp_instr_req,
      spm_rs1                    => spm_rs1,
      spm_rs2                    => spm_rs2,
      vec_read_rs1_ID            => vec_read_rs1_ID,
      vec_read_rs2_ID            => vec_read_rs2_ID,
      vec_write_rd_ID            => vec_write_rd_ID,
      busy_DSP                   => busy_DSP,
      state_LS                   => state_LS,
      sc_word_count_wire         => sc_word_count_wire,
      spm_bcast                  => spm_bcast,
      harc_LS_wire               => harc_LS_wire,
      ls_sc_data_write_wire      => ls_sc_data_write_wire,
      ls_sc_read_addr            => ls_sc_read_addr,
      ls_sc_write_addr           => ls_sc_write_addr,
      ls_sci_req                 => ls_sci_req,
      ls_sci_we                  => ls_sci_we,
      kmemld_inflight            => kmemld_inflight,
      kmemstr_inflight           => kmemstr_inflight,
      ls_sc_data_read_wire       => ls_sc_data_read_wire,
      ls_sci_wr_gnt              => ls_sci_wr_gnt,
      ls_data_gnt_i              => ls_data_gnt_i
      );

end Klessydra_M;
--------------------------------------------------------------------------------------------------
-- END of Klessydra M core architecture ----------------------------------------------------------
--------------------------------------------------------------------------------------------------