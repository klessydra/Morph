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
entity klessydra_top is
  generic (
    THREAD_POOL_SIZE        : natural := 3;   -- Changing the TPS to less than "number of pipeline stages-1" is not allowed. And making it bigger than "pipeline stages-1" is okay but not recommended
    THREAD_POOL_SIZE_GLOBAL : natural := THREAD_POOL_SIZE+1;   -- Indicates the total number of harts on the chip, and not only the ones in the cores
    cluster_size_ceil       : natural := 1;   -- The cieling bits of that identify the size of the cluster
    lutram_rf               : natural := 1;   -- Changes the regfile from flip-flop type into LUTRAM type
    latch_rf                : natural := 0;   -- Changes the regfile from flip-flop type into Latch type (only works if lutram_rf is set to 0)
    RV32E                   : natural := 0;   -- Regfile size, Can be set to 32 for RV32E being 0 else 16 for RV32E being set to 1
    RV32M                   : natural := 1;   -- Enables the M-extension of the risc-v instruction set
    RV32F                   : natural := 1;   -- Enables the F-extension of the risc-v instruction set
    context_switch          : natural := 0;   -- Enables the context switching between cores
    morph_en                : natural := 1;   -- Enables the generation of the logic that allows processor to morph from an IMT to a single core processor
    fetch_stage_en          : natural := 1;   -- Enables the generation of a fetch stage buffer, else the incoming instrution will go directly to the decode stage.
    branch_predict_en       : natural := 1;   -- This enables the branch predictor
    btb_en                  : natural := 1;   -- Enables the BTB instead of the single bit predictor
    btb_len                 : natural := 6;   -- Indicates the number of entries in the btb which is 2^btb_len
    superscalar_exec_en     : natural := 1;   -- Enables superscalar execution when set to 1, else the stall of the pipeline will depend on tha latency of the instruction
    accl_en                 : natural := 0;   -- Enables the generation of the general purpose accelerator
    replicate_accl_en       : natural := 0;   -- Set to 1 to replicate the accelerator for every thread
    multithreaded_accl_en   : natural := 0;   -- Set to 1 to let the replicated accelerator share the functional units (note: replicate_accl_en must be set to '1')
    SPM_NUM                 : natural := 3;   -- The number of scratchpads available "Minimum allowed is two"
    Addr_Width              : natural := 13;  -- This address is for scratchpads. Setting this will make the size of the spm to be: "2^Addr_Width -1"
    SPM_STRT_ADDR           : std_logic_vector(31 downto 0) := x"1000_0000";  -- This is starting address of the spms, it shouldn't overlap any sections in the memory map
    SIMD                    : natural := 1;   -- Changing the SIMD, would change the number of the functional units in the dsp, and the number of banks in the spms (can be power of 2 only e.g. 1,2,4,8)
    MCYCLE_EN               : natural := 0;   -- Can be set to 1 or 0 only. Setting to zero will disable MCYCLE and MCYCLEH
    MINSTRET_EN             : natural := 0;   -- Can be set to 1 or 0 only. Setting to zero will disable MINSTRET and MINSTRETH
    MHPMCOUNTER_EN          : natural := 0;   -- Can be set to 1 or 0 only. Setting to zero will disable all performance counters except "MCYCLE/H" and "MINSTRET/H"
    count_all               : natural := 0;   -- Perfomance counters count for all the harts instead of there own hart
    debug_en                : natural := 0;   -- Generates the debug unit
    tracer_en               : natural := 0;   -- Enables the generation of the instruction tracer disable in extremely long simulations in order to save storage space
     ----------------------------------------------------------------------------------------
    N_EXT_PERF_COUNTERS     : integer := 0;   -- ignored in Klessydra
    INSTR_RDATA_WIDTH       : integer := 32;  -- ignored in Klessydra
    N_HWLP                  : integer := 2;   -- ignored in Klessydra
    N_HWLP_BITS             : integer := 4    -- ignored in Klessydra
    );
  port (
    -- clock, reset active low, test enable
    clk_i               : in  std_logic;
    clock_en_i          : in  std_logic;
    rst_ni              : in  std_logic;
    test_en_i           : in  std_logic;
    -- initialization signals 
    boot_addr_i         : in  std_logic_vector(31 downto 0);
    core_id_i           : in  std_logic_vector(3 downto 0);
    cluster_id_i        : in  std_logic_vector(5 downto 0);
    -- program memory interface
    instr_req_o         : out std_logic;
    instr_gnt_i         : in  std_logic;
    instr_rvalid_i      : in  std_logic;
    instr_addr_o        : out std_logic_vector(31 downto 0);
    instr_rdata_i       : in  std_logic_vector(31 downto 0);
    -- data memory interface
    data_req_o          : out std_logic;
    data_gnt_i          : in  std_logic;
    data_rvalid_i       : in  std_logic;
    data_we_o           : out std_logic;
    data_be_o           : out std_logic_vector(3 downto 0);
    data_addr_o         : out std_logic_vector(31 downto 0);
    data_wdata_o        : out std_logic_vector(31 downto 0);
    data_rdata_i        : in  std_logic_vector(31 downto 0);
    data_err_i          : in  std_logic;
    -- interrupt request interface
    irq_i               : in  std_logic;
    irq_id_i            : in  std_logic_vector(4 downto 0);
    irq_ack_o           : out std_logic;
    irq_id_o            : out std_logic_vector(4 downto 0);
    irq_sec_i           : in  std_logic;  -- unused in Pulpino
    sec_lvl_o           : out std_logic;  -- unused in Pulpino
    -- debug interface
    debug_req_i         : in  std_logic;
    debug_gnt_o         : out std_logic;
    debug_rvalid_o      : out std_logic;
    debug_addr_i        : in  std_logic_vector(14 downto 0);
    debug_we_i          : in  std_logic;
    debug_wdata_i       : in  std_logic_vector(31 downto 0);
    debug_rdata_o       : out std_logic_vector(31 downto 0);
    debug_halted_o      : out std_logic;
    debug_halt_i        : in  std_logic;
    debug_resume_i      : in  std_logic;
    -- miscellanous control signals
    fetch_enable_i      : in  std_logic;
    core_busy_o         : out std_logic;
    ext_perf_counters_i : in  std_logic_vector(N_EXT_PERF_COUNTERS to 1);
    -- klessydra-specific signals
    core_select         : in  natural range 1 downto 0;
    source_hartid_i     : in  natural range THREAD_POOL_SIZE_GLOBAL-1 downto 0; -- used to overwrite the mhartID of the core doing the context switch
    source_hartid_o     : out natural range THREAD_POOL_SIZE_GLOBAL-1 downto 0;
    sw_irq_o            : out std_logic_vector(THREAD_POOL_SIZE_GLOBAL-1 downto 0);
    sw_irq_served_i     : in  std_logic_vector(THREAD_POOL_SIZE-1 downto 0);
    sw_irq_served_o     : out std_logic_vector(THREAD_POOL_SIZE_GLOBAL-1 downto 0)
    );
end entity klessydra_top;

architecture Klessydra of klessydra_top is

  constant TPS_CEIL            : natural := integer(ceil(log2(real(THREAD_POOL_SIZE))));
  constant TPS_BUF_CEIL        : natural := integer(ceil(log2(real(THREAD_POOL_SIZE)))); -- AAA equal as the signal above remove it 
  constant SPM_ADDR_WID        : natural := integer(ceil(log2(real(SPM_NUM+1)))); 
  constant SIMD_BITS           : natural := integer(ceil(log2(real(SIMD))));
  constant Data_Width          : natural := 32;
  constant SIMD_Width          : natural := SIMD*Data_Width;

  subtype harc_range is natural range THREAD_POOL_SIZE-1 downto 0;  -- will be used replicated units in the core

  constant ACCL_NUM : natural := (THREAD_POOL_SIZE-(THREAD_POOL_SIZE-1)*(1-replicate_accl_en));
  constant FU_NUM   : natural := (ACCL_NUM-(ACCL_NUM-1)*(multithreaded_accl_en));

  subtype accl_range is integer range ACCL_NUM - 1 downto 0;  -- will be used replicated accelerators in the core 

  signal sw_irq_i                : std_logic_vector(THREAD_POOL_SIZE_GLOBAL-1 downto 0);

  -- VCU Signals
  signal rs1_to_sc               : std_logic_vector(SPM_ADDR_WID-1 downto 0);
  signal rs2_to_sc               : std_logic_vector(SPM_ADDR_WID-1 downto 0);
  signal rd_to_sc                : std_logic_vector(SPM_ADDR_WID-1 downto 0);
  signal MVSIZE                  : array_2d(harc_range)(Addr_Width downto 0);
  signal MVTYPE                  : array_2d(harc_range)(3 downto 0);
  signal MPSCLFAC                : array_2d(harc_range)(4 downto 0);
  signal dsp_except_data         : array_2d(accl_range)(31 downto 0);
  signal decoded_instruction_DSP : std_logic_vector(DSP_UNIT_INSTR_SET_SIZE-1 downto 0);
  signal harc_EXEC               : harc_range;
  signal pc_IE                   : std_logic_vector(31 downto 0);  -- pc_IE is pc entering stage IE
  signal RS1_Data_IE             : std_logic_vector(31 downto 0);
  signal RS2_Data_IE             : std_logic_vector(31 downto 0);
  signal RD_Data_IE              : std_logic_vector(31 downto 0);
  signal dsp_instr_req           : std_logic_vector(accl_range);
  signal spm_rs1                 : std_logic;
  signal spm_rs2                 : std_logic;
  signal vec_read_rs1_ID         : std_logic;
  signal vec_read_rs2_ID         : std_logic;
  signal vec_write_rd_ID         : std_logic;
  signal busy_DSP                : std_logic_vector(accl_range);
  signal state_DSP               : array_2d(ACCL_NUM-1 downto 0)(1 downto 0);
  signal state_LS                : fsm_LS_states;
  signal sc_word_count_wire      : integer;
  signal spm_bcast               : std_logic;
  signal harc_LS_wire            : integer range ACCL_NUM-1 downto 0;
  signal ls_sc_data_write_wire   : std_logic_vector(Data_Width-1 downto 0);
  signal ls_sc_read_addr         : std_logic_vector(Addr_Width-(SIMD_BITS+3) downto 0);
  signal ls_sc_write_addr        : std_logic_vector(Addr_Width-(SIMD_BITS+3) downto 0);
  signal ls_sci_req              : std_logic_vector(SPM_NUM-1 downto 0);
  signal ls_sci_we               : std_logic_vector(SPM_NUM-1 downto 0);
  signal kmemld_inflight         : std_logic_vector(SPM_NUM-1 downto 0);
  signal kmemstr_inflight        : std_logic_vector(SPM_NUM-1 downto 0);
  signal ls_sc_data_read_wire    : std_logic_vector(Data_Width-1 downto 0);
  signal ls_sci_wr_gnt           : std_logic;
  signal ls_data_gnt_i           : std_logic_vector(SPM_NUM-1 downto 0);
  signal dsp_taken_branch        : std_logic_vector(accl_range);
  signal dsp_except_condition    : std_logic_vector(accl_range);

  component klessydra_m_core
  generic(
    THREAD_POOL_SIZE_GLOBAL : natural;
    THREAD_POOL_SIZE        : natural;
    cluster_size_ceil       : natural;
    lutram_rf               : natural;
    latch_rf                : natural;
    RV32E                   : natural;
    RV32M                   : natural;
    RV32F                   : natural;
    context_switch          : natural;
    morph_en                : natural;
    fetch_stage_en          : natural;
    branch_predict_en       : natural;
    btb_en                  : natural;
    btb_len                 : natural;
    superscalar_exec_en     : natural;
    accl_en                 : natural;
    replicate_accl_en       : natural;
    multithreaded_accl_en   : natural;
    SPM_NUM                 : natural;
    Addr_Width              : natural;
    SPM_STRT_ADDR           : std_logic_vector(31 downto 0);
    SIMD                    : natural;
    MCYCLE_EN               : natural;
    MINSTRET_EN             : natural;
    MHPMCOUNTER_EN          : natural;
    count_all               : natural;
    debug_en                : natural;
    tracer_en               : natural;
    Data_Width              : natural;
    SPM_ADDR_WID            : natural;
    SIMD_BITS               : natural;
    ACCL_NUM                : natural;
    N_EXT_PERF_COUNTERS     : integer; 
    INSTR_RDATA_WIDTH       : integer;
    N_HWLP                  : integer; 
    N_HWLP_BITS             : integer
  );
  port (
    clk_i               : in  std_logic;
    clock_en_i          : in  std_logic;
    rst_ni              : in  std_logic;
    test_en_i           : in  std_logic;
    boot_addr_i         : in  std_logic_vector(31 downto 0);
    core_id_i           : in  std_logic_vector(3 downto 0);
    cluster_id_i        : in  std_logic_vector(5 downto 0);
    instr_req_o         : out std_logic;
    instr_gnt_i         : in  std_logic;
    instr_rvalid_i      : in  std_logic;
    instr_addr_o        : out std_logic_vector(31 downto 0);
    instr_rdata_i       : in  std_logic_vector(31 downto 0);
    -- data memory interface
    data_req_o          : out std_logic;
    data_gnt_i          : in  std_logic;
    data_rvalid_i       : in  std_logic;
    data_we_o           : out std_logic;
    data_be_o           : out std_logic_vector(3 downto 0);
    data_addr_o         : out std_logic_vector(31 downto 0);
    data_wdata_o        : out std_logic_vector(31 downto 0);
    data_rdata_i        : in  std_logic_vector(31 downto 0);
    data_err_i          : in  std_logic;
    -- interrupt request interface
    irq_i               : in  std_logic;
    irq_id_i            : in  std_logic_vector(4 downto 0);
    irq_ack_o           : out std_logic;
    irq_id_o            : out std_logic_vector(4 downto 0);
    irq_sec_i           : in  std_logic;  -- unused in Pulpino
    sec_lvl_o           : out std_logic;  -- unused in Pulpino
    -- debug interface
    debug_req_i         : in  std_logic;
    debug_gnt_o         : out std_logic;
    debug_rvalid_o      : out std_logic;
    debug_addr_i        : in  std_logic_vector(14 downto 0);
    debug_we_i          : in  std_logic;
    debug_wdata_i       : in  std_logic_vector(31 downto 0);
    debug_rdata_o       : out std_logic_vector(31 downto 0);
    debug_halted_o      : out std_logic;
    debug_halt_i        : in  std_logic;
    debug_resume_i      : in  std_logic;
    -- miscellanous control signals
    fetch_enable_i      : in  std_logic;
    core_busy_o         : out std_logic;
    ext_perf_counters_i : in  std_logic_vector(N_EXT_PERF_COUNTERS to 1);
    -- klessydra-specific signals
    core_select         : in  natural range 1 downto 0;
    source_hartid_i     : in  natural range THREAD_POOL_SIZE_GLOBAL-1 downto 0;
    source_hartid_o     : out natural range THREAD_POOL_SIZE_GLOBAL-1 downto 0;
    sw_irq_i            : in  std_logic_vector(THREAD_POOL_SIZE_GLOBAL-1 downto 0);
    sw_irq_o            : out std_logic_vector(THREAD_POOL_SIZE_GLOBAL-1 downto 0);
    sw_irq_served_i     : in  std_logic_vector(THREAD_POOL_SIZE-1 downto 0);
    sw_irq_served_o     : out std_logic_vector(THREAD_POOL_SIZE_GLOBAL-1 downto 0);
    -- VCU Signals
    harc_EXEC               : out natural range THREAD_POOL_SIZE-1 downto 0;
    MVSIZE                  : out array_2d(harc_range)(Addr_Width downto 0);
    MVTYPE                  : out array_2d(harc_range)(3 downto 0);
    MPSCLFAC                : out array_2d(harc_range)(4 downto 0);
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
  end component;

  component VCU is
  generic(
    THREAD_POOL_SIZE      : natural;
    accl_en               : natural;
    replicate_accl_en     : natural;
    multithreaded_accl_en : natural;
    SPM_NUM               : natural;
    Addr_Width            : natural;
    SIMD                  : natural;
    --------------------------------
    ACCL_NUM              : natural;
    FU_NUM                : natural;
    TPS_CEIL              : natural;
    TPS_BUF_CEIL          : natural;
    SPM_ADDR_WID          : natural;
    SIMD_BITS             : natural;
    Data_Width            : natural;
    SIMD_Width            : natural
  );
  port (
  -- Core Signals
    clk_i, rst_ni              : in std_logic;
    -- Processing Pipeline Signals
    rs1_to_sc                  : in  std_logic_vector(SPM_ADDR_WID-1 downto 0);
    rs2_to_sc                  : in  std_logic_vector(SPM_ADDR_WID-1 downto 0);
    rd_to_sc                   : in  std_logic_vector(SPM_ADDR_WID-1 downto 0);
  -- CSR Signals
    MVSIZE                     : in  array_2d(harc_range)(Addr_Width downto 0);
    MVTYPE                     : in  array_2d(harc_range)(3 downto 0);
    MPSCLFAC                   : in  array_2d(harc_range)(4 downto 0);
    dsp_except_data            : out array_2d(accl_range)(31 downto 0);
  -- Program Counter Signals
    dsp_taken_branch           : out std_logic_vector(accl_range);
    dsp_except_condition       : out std_logic_vector(accl_range);
    -- ID_Stage Signals
    decoded_instruction_DSP    : in  std_logic_vector(DSP_UNIT_INSTR_SET_SIZE-1 downto 0);
    harc_EXEC                  : in  natural range THREAD_POOL_SIZE-1 downto 0;
    pc_IE                      : in  std_logic_vector(31 downto 0);
    RS1_Data_IE                : in  std_logic_vector(31 downto 0);
    RS2_Data_IE                : in  std_logic_vector(31 downto 0);
    RD_Data_IE                 : in  std_logic_vector(Addr_Width -1 downto 0);
    dsp_instr_req              : in  std_logic_vector(accl_range);
    spm_rs1                    : in  std_logic;
    spm_rs2                    : in  std_logic;
    vec_read_rs1_ID            : in  std_logic;
    vec_read_rs2_ID            : in  std_logic;
    vec_write_rd_ID            : in  std_logic;
    busy_dsp                   : out std_logic_vector(ACCL_NUM-1 downto 0);
    -- tracer signals
    state_DSP                  : out array_2d(ACCL_NUM-1 downto 0)(1 downto 0);
    -- SPMI specific
    data_rvalid_i              : in  std_logic;
    state_LS                   : in  fsm_LS_states;
    sc_word_count_wire         : in  integer;
    spm_bcast                  : in  std_logic;
    harc_LS_wire               : in  integer range ACCL_NUM-1 downto 0;
    ls_sc_data_write_wire      : in  std_logic_vector(Data_Width-1 downto 0);
    ls_sc_read_addr            : in  std_logic_vector(Addr_Width-(SIMD_BITS+3) downto 0);
    ls_sc_write_addr           : in  std_logic_vector(Addr_Width-(SIMD_BITS+3) downto 0);
    ls_sci_req                 : in  std_logic_vector(SPM_NUM-1 downto 0);
    ls_sci_we                  : in  std_logic_vector(SPM_NUM-1 downto 0);
    kmemld_inflight            : in  std_logic_vector(SPM_NUM-1 downto 0);
    kmemstr_inflight           : in  std_logic_vector(SPM_NUM-1 downto 0);
    ls_sc_data_read_wire       : out std_logic_vector(Data_Width-1 downto 0);
    ls_sci_wr_gnt              : out std_logic;
    ls_data_gnt_i              : out std_logic_vector(SPM_NUM-1 downto 0)
  );
  end component;

--------------------------------------------------------------------------------------------------
----------------------- ARCHITECTURE BEGIN -------------------------------------------------------              
begin

  dsp_except_off : if accl_en = 0 generate
    dsp_taken_branch     <= (others => '0');
    dsp_except_condition <= (others => '0');
  end generate;

  MORPH_inst : klessydra_m_core
  generic map (
    THREAD_POOL_SIZE_GLOBAL => THREAD_POOL_SIZE_GLOBAL,
    THREAD_POOL_SIZE        => THREAD_POOL_SIZE,
    cluster_size_ceil       => cluster_size_ceil,
    lutram_rf               => lutram_rf,
    latch_rf                => latch_rf,
    RV32E                   => RV32E,
    RV32M                   => RV32M,
    RV32F                   => RV32F,
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
    Data_Width              => Data_Width,
    SIMD_BITS               => SIMD_BITS,
    SPM_ADDR_WID            => SPM_ADDR_WID,
    ACCL_NUM                => ACCL_NUM,
    N_EXT_PERF_COUNTERS     => N_EXT_PERF_COUNTERS,
    INSTR_RDATA_WIDTH       => INSTR_RDATA_WIDTH,
    N_HWLP                  => N_HWLP,
    N_HWLP_BITS             => N_HWLP_BITS
  )
  port map (
    clk_i                   => clk_i,
    clock_en_i              => clock_en_i,
    rst_ni                  => rst_ni,
    test_en_i               => test_en_i,
    boot_addr_i             => boot_addr_i,
    core_id_i               => core_id_i,
    cluster_id_i            => cluster_id_i,
    instr_req_o             => instr_req_o,
    instr_gnt_i             => instr_gnt_i,
    instr_rvalid_i          => instr_rvalid_i,
    instr_addr_o            => instr_addr_o,
    instr_rdata_i           => instr_rdata_i,
    data_req_o              => data_req_o,
    data_gnt_i              => data_gnt_i,
    data_rvalid_i           => data_rvalid_i,
    data_we_o               => data_we_o,
    data_be_o               => data_be_o,
    data_addr_o             => data_addr_o,
    data_wdata_o            => data_wdata_o,
    data_rdata_i            => data_rdata_i,
    data_err_i              => data_err_i,
    irq_i                   => irq_i,
    irq_id_i                => irq_id_i,
    irq_ack_o               => irq_ack_o,
    irq_id_o                => irq_id_o,
    irq_sec_i               => irq_sec_i,
    sec_lvl_o               => sec_lvl_o,
    debug_req_i             => debug_req_i,
    debug_gnt_o             => debug_gnt_o,
    debug_rvalid_o          => debug_rvalid_o,
    debug_addr_i            => debug_addr_i,
    debug_we_i              => debug_we_i,
    debug_wdata_i           => debug_wdata_i,
    debug_rdata_o           => debug_rdata_o,
    debug_halted_o          => debug_halted_o,
    debug_halt_i            => debug_halt_i,
    debug_resume_i          => debug_resume_i,
    fetch_enable_i          => fetch_enable_i,
    core_busy_o             => core_busy_o,
    ext_perf_counters_i     => ext_perf_counters_i,
    core_select             => core_select,
    source_hartid_i         => source_hartid_i,
    source_hartid_o         => source_hartid_o,
    sw_irq_i                => sw_irq_i,
    sw_irq_o                => sw_irq_o,
    sw_irq_served_i         => sw_irq_served_i,
    sw_irq_served_o         => sw_irq_served_o,
    harc_EXEC               => harc_EXEC,
    MVSIZE                  => MVSIZE,
    MVTYPE                  => MVTYPE,
    MPSCLFAC                => MPSCLFAC,
    pc_IE                   => pc_IE,
    rs1_to_sc               => rs1_to_sc,
    rs2_to_sc               => rs2_to_sc,
    rd_to_sc                => rd_to_sc,
    decoded_instruction_DSP => decoded_instruction_DSP,
    RS1_Data_IE             => RS1_Data_IE,
    RS2_Data_IE             => RS2_Data_IE,
    RD_Data_IE              => RD_Data_IE,
    dsp_instr_req           => dsp_instr_req,
    spm_rs1                 => spm_rs1,
    spm_rs2                 => spm_rs2,
    vec_read_rs1_ID         => vec_read_rs1_ID,
    vec_read_rs2_ID         => vec_read_rs2_ID,
    vec_write_rd_ID         => vec_write_rd_ID,
    busy_DSP                => busy_DSP,
    state_LS                => state_LS,
    sc_word_count_wire      => sc_word_count_wire,
    spm_bcast               => spm_bcast,
    harc_LS_wire            => harc_LS_wire,
    ls_sc_data_write_wire   => ls_sc_data_write_wire,
    ls_sc_read_addr         => ls_sc_read_addr,
    ls_sc_write_addr        => ls_sc_write_addr,
    ls_sci_req              => ls_sci_req,
    ls_sci_we               => ls_sci_we,
    kmemld_inflight         => kmemld_inflight,
    kmemstr_inflight        => kmemstr_inflight,
    ls_sc_data_read_wire    => ls_sc_data_read_wire,
    ls_sci_wr_gnt           => ls_sci_wr_gnt,
    ls_data_gnt_i           => ls_data_gnt_i,
    dsp_taken_branch        => dsp_taken_branch,
    dsp_except_condition    => dsp_except_condition
  );

  ACCL_generate : if accl_en = 1 generate

  VCU_inst : VCU
  generic map(
    THREAD_POOL_SIZE           => THREAD_POOL_SIZE, 
    accl_en                    => accl_en, 
    replicate_accl_en          => replicate_accl_en, 
    multithreaded_accl_en      => multithreaded_accl_en, 
    SPM_NUM                    => SPM_NUM,  
    Addr_Width                 => Addr_Width, 
    SIMD                       => SIMD, 
    --------------------------------
    ACCL_NUM                   => ACCL_NUM, 
    FU_NUM                     => FU_NUM, 
    TPS_CEIL                   => TPS_CEIL, 
    TPS_BUF_CEIL               => TPS_BUF_CEIL, 
    SPM_ADDR_WID               => SPM_ADDR_WID, 
    SIMD_BITS                  => SIMD_BITS, 
    Data_Width                 => Data_Width, 
    SIMD_Width                 => SIMD_Width
  )
  port map(
    clk_i                      => clk_i,
    rst_ni                     => rst_ni,
    rs1_to_sc                  => rs1_to_sc,
    rs2_to_sc                  => rs2_to_sc,
    rd_to_sc                   => rd_to_sc,
    MVSIZE                     => MVSIZE,
    MVTYPE                     => MVTYPE,
    MPSCLFAC                   => MPSCLFAC,
    dsp_except_data            => dsp_except_data,
    dsp_taken_branch           => dsp_taken_branch,
    dsp_except_condition       => dsp_except_condition,
    decoded_instruction_DSP    => decoded_instruction_DSP,
    harc_EXEC                  => harc_EXEC,
    pc_IE                      => pc_IE,
    RS1_Data_IE                => RS1_Data_IE,
    RS2_Data_IE                => RS2_Data_IE,
    RD_Data_IE                 => RD_Data_IE(Addr_Width -1 downto 0),
    dsp_instr_req              => dsp_instr_req,
    spm_rs1                    => spm_rs1,
    spm_rs2                    => spm_rs2,
    vec_read_rs1_ID            => vec_read_rs1_ID,
    vec_read_rs2_ID            => vec_read_rs2_ID,
    vec_write_rd_ID            => vec_write_rd_ID,
    busy_DSP                   => busy_DSP,
    data_rvalid_i              => data_rvalid_i,
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

  end generate;

end Klessydra;
--------------------------------------------------------------------------------------------------
-- END of Klessydra M core architecture ----------------------------------------------------------
--------------------------------------------------------------------------------------------------