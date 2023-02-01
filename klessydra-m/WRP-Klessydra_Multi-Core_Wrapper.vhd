
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

-- core entity declaration --
entity klessydra_heterogeneous_cluster is
  generic (
    THREAD_POOL_SIZE      : natural := 3;   -- Changing the TPS to less than "number of pipeline stages-1" is not allowed. And making it bigger than "pipeline stages-1" is okay but not recommended
    lutram_rf             : natural := 1;   -- Changes the regfile from flip-flop type into BRAM type
    latch_rf              : natural := 0;   -- Changes the regfile from flip-flop type into Latch type (only works if lutram_rf is set to 0)
    RV32E                 : natural := 0;   -- Regfile size, Can be set to 32 for RV32E being 0 else 16 for RV32E being set to 1
    RV32M                 : natural := 1;   -- Enables the M-extension of the risc-v instruction set
    context_switch        : natural := 1;   -- Enables the context switching between cores
    morph_en              : natural := 1;   -- Enables the generation of the logic that allows processor to morph from an IMT to a single core processor
    fetch_stage_en        : natural := 0;   -- Enables the generation of a fetch stage buffer, else the incoming instrution will go directly to the decode stage.
    branch_predict_en     : natural := 1;   -- This enables the branch predictor
    btb_en                : natural := 0;   -- Enables the BTB instead of the single bit predictor
    btb_len               : natural := 6;   -- Indicates the number of entries in the btb which is 2^btb_len
    superscalar_exec_en   : natural := 1;   -- Enables superscalar execution when set to 1, else the stall of the pipeline will depend on tha latency of the instruction
    accl_en               : natural := 1;   -- Enables the generation of the general purpose accelerator
    replicate_accl_en     : natural := 1;   -- Set to 1 to replicate the accelerator for every thread
    multithreaded_accl_en : natural := 1;   -- Set to 1 to let the replicated accelerator share the functional units (note: replicate_accl_en must be set to '1')
    SPM_NUM               : natural := 4;   -- The number of scratchpads available "Minimum allowed is two"
    Addr_Width            : natural := 13;  -- This address is for scratchpads. Setting this will make the size of the spm to be: "2^Addr_Width -1"
    SPM_STRT_ADDR         : std_logic_vector(31 downto 0) := x"1000_0000";  -- This is starting address of the spms, it shouldn't overlap any sections in the memory map
    SIMD                  : natural := 2;   -- Changing the SIMD, would change the number of the functional units in the dsp, and the number of banks in the spms (can be power of 2 only e.g. 1,2,4,8)
    MCYCLE_EN             : natural := 0;   -- Can be set to 1 or 0 only. Setting to zero will disable MCYCLE and MCYCLEH
    MINSTRET_EN           : natural := 0;   -- Can be set to 1 or 0 only. Setting to zero will disable MINSTRET and MINSTRETH
    MHPMCOUNTER_EN        : natural := 0;   -- Can be set to 1 or 0 only. Setting to zero will disable all performance counters except "MCYCLE/H" and "MINSTRET/H"
    count_all             : natural := 1;   -- Perfomance counters count for all the harts instead of there own hart
    debug_en              : natural := 0;   -- Generates the debug unit
    tracer_en             : natural := 0;   -- Enables the generation of the instruction tracer disable in extremely long simulations in order to save storage space
     ----------------------------------------------------------------------------------------
    N_EXT_PERF_COUNTERS   : integer := 0;   -- ignored in Klessydra
    INSTR_RDATA_WIDTH     : integer := 32;  -- ignored in Klessydra
    N_HWLP                : integer := 2;   -- ignored in Klessydra
    N_HWLP_BITS           : integer := 4    -- ignored in Klessydra
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
    ext_perf_counters_i : in  std_logic_vector(N_EXT_PERF_COUNTERS to 1)
    );

end entity klessydra_heterogeneous_cluster;

architecture Het_Cluster of klessydra_heterogeneous_cluster is

constant TPS_CEIL            : natural := integer(ceil(log2(real(THREAD_POOL_SIZE))));
constant TPS_BUF_CEIL        : natural := integer(ceil(log2(real(THREAD_POOL_SIZE)))); -- AAA equal as the signal above remove it 
constant SPM_ADDR_WID        : natural := integer(ceil(log2(real(SPM_NUM+1)))); 
constant SIMD_BITS           : natural := integer(ceil(log2(real(SIMD))));
constant Data_Width          : natural := 32;
constant SIMD_Width          : natural := SIMD*Data_Width;

subtype harc_range is natural range THREAD_POOL_SIZE-1 downto 0;  -- will be used replicated units in the core

constant ACCL_NUM : natural := (THREAD_POOL_SIZE-(THREAD_POOL_SIZE-1)*(1-replicate_accl_en));
constant FU_NUM   : natural := (ACCL_NUM-(ACCL_NUM-1)*(multithreaded_accl_en));

subtype accl_range is integer range ACCL_NUM-1 downto 0;  -- will be used replicated accelerators in the core 

type array_2d_harc is array (integer range<>) of harc_range;
type array_2d_accl is array (integer range<>) of accl_range;
type array_2d_states_LS is array (integer range<>) of fsm_LS_states;

constant cluster_size            : natural := 2;
constant cluster_size_ceil       : natural := integer(ceil(log2(real(cluster_size))));
constant THREAD_POOL_SIZE_GLOBAL : natural := 4;

signal core_select               : natural range cluster_size-1 downto 0;
signal VCU_hartID                : accl_range;
signal source_hartid             : array_2d_nat(cluster_size-1 downto 0);

-- Signals Mapped to Output
signal instr_req_int             : std_logic_vector(cluster_size-1 downto 0);
signal instr_addr_int            : array_2d(cluster_size-1 downto 0)(31 downto 0);
signal data_req_int              : std_logic_vector(cluster_size-1 downto 0);
signal data_we_int               : std_logic_vector(cluster_size-1 downto 0);
signal data_be_int               : array_2d(cluster_size-1 downto 0)(3 downto 0);
signal data_addr_int             : array_2d(cluster_size-1 downto 0)(31 downto 0);
signal data_wdata_int            : array_2d(cluster_size-1 downto 0)(31 downto 0);
signal irq_ack_int               : std_logic_vector(cluster_size-1 downto 0);
signal sec_lvl_int               : std_logic_vector(cluster_size-1 downto 0);  -- unused in Pulpino
signal debug_gnt_int             : std_logic_vector(cluster_size-1 downto 0);
signal debug_rvalid_int          : std_logic_vector(cluster_size-1 downto 0);
signal debug_rdata_int           : array_2d(cluster_size-1 downto 0)(31 downto 0);
signal debug_halted_int          : std_logic_vector(cluster_size-1 downto 0);
signal core_busy_int             : std_logic_vector(cluster_size-1 downto 0);
signal source_hartid_o           : array_2d_nat(cluster_size-1 downto 0);
signal sw_irq_o                  : array_2d(cluster_size-1 downto 0)(THREAD_POOL_SIZE_GLOBAL-1 downto 0);
signal sw_irq_served_o           : array_2d(cluster_size-1 downto 0)(THREAD_POOL_SIZE_GLOBAL-1 downto 0);

-- Signals Mapped to Input
signal instr_gnt_int             : std_logic_vector(cluster_size-1 downto 0);
signal instr_rvalid_int          : std_logic_vector(cluster_size-1 downto 0);
signal instr_rdata_int           : array_2d(cluster_size-1 downto 0)(31 downto 0);
signal data_gnt_int              : std_logic_vector(cluster_size-1 downto 0);
signal data_rvalid_int           : std_logic_vector(cluster_size-1 downto 0);
signal data_rdata_int            : array_2d(cluster_size-1 downto 0)(31 downto 0);
signal data_err_int              : std_logic_vector(cluster_size-1 downto 0);
signal irq_int                   : std_logic_vector(cluster_size-1 downto 0);
signal irq_id_int                : array_2d(cluster_size-1 downto 0)(4 downto 0);
signal irq_sec_int               : std_logic_vector(cluster_size-1 downto 0);
signal debug_req_int             : std_logic_vector(cluster_size-1 downto 0);
signal debug_addr_int            : array_2d(cluster_size-1 downto 0)(14 downto 0);
signal debug_we_int              : std_logic_vector(cluster_size-1 downto 0);
signal debug_wdata_int           : array_2d(cluster_size-1 downto 0)(31 downto 0);
signal debug_halt_int            : std_logic_vector(cluster_size-1 downto 0);
signal debug_resume_int          : std_logic_vector(cluster_size-1 downto 0);
signal fetch_enable_int          : std_logic_vector(cluster_size-1 downto 0);
signal ext_perf_counters_int     : array_2d(cluster_size-1 downto 0)(N_EXT_PERF_COUNTERS to 1);
--signal source_hartid_i           : natural range THREAD_POOL_SIZE_GLOBAL-1 downto 0;
signal sw_irq                    : std_logic_vector(THREAD_POOL_SIZE_GLOBAL-1 downto 0);
signal sw_irq_i                  : std_logic_vector(THREAD_POOL_SIZE_GLOBAL-1 downto 0);
signal sw_irq_served_i           : array_2d(cluster_size-1 downto 0)(THREAD_POOL_SIZE_GLOBAL-1 downto 0);

-- VCU Signals
signal rs1_to_sc                 : array_2d(cluster_size-1 downto 0)(SPM_ADDR_WID-1 downto 0);
signal rs2_to_sc                 : array_2d(cluster_size-1 downto 0)(SPM_ADDR_WID-1 downto 0);
signal rd_to_sc                  : array_2d(cluster_size-1 downto 0)(SPM_ADDR_WID-1 downto 0);
signal MVSIZE                    : array_3d(cluster_size-1 downto 0)(harc_range)(Addr_Width downto 0);
signal MVSIZE_int                : array_3d(cluster_size-1 downto 0)(harc_range)(Addr_Width downto 0);
signal MVTYPE                    : array_3d(cluster_size-1 downto 0)(harc_range)(3 downto 0);
signal MVTYPE_int                : array_3d(cluster_size-1 downto 0)(harc_range)(3 downto 0);
signal MPSCLFAC                  : array_3d(cluster_size-1 downto 0)(harc_range)(4 downto 0);
signal MPSCLFAC_int              : array_3d(cluster_size-1 downto 0)(harc_range)(4 downto 0);
signal dsp_except_data           : array_3d(cluster_size-1 downto 0)(accl_range)(31 downto 0);
signal decoded_instruction_DSP   : array_2d(cluster_size-1 downto 0)(DSP_UNIT_INSTR_SET_SIZE-1 downto 0);
signal harc_EXEC_int             : array_2d_harc(cluster_size-1 downto 0);
signal harc_EXEC                 : array_2d_harc(cluster_size-1 downto 0);
signal pc_IE                     : array_2d(cluster_size-1 downto 0)(31 downto 0);  -- pc_IE is pc entering stage IE
signal RS1_Data_IE               : array_2d(cluster_size-1 downto 0)(31 downto 0);
signal RS2_Data_IE               : array_2d(cluster_size-1 downto 0)(31 downto 0);
signal RD_Data_IE                : array_2d(cluster_size-1 downto 0)(31 downto 0);
signal dsp_instr_req             : array_2d(cluster_size-1 downto 0)(accl_range);
signal dsp_instr_req_int         : array_2d(cluster_size-1 downto 0)(accl_range);
signal spm_rs1                   : std_logic_vector(cluster_size-1 downto 0);
signal spm_rs2                   : std_logic_vector(cluster_size-1 downto 0);
signal vec_read_rs1_ID           : std_logic_vector(cluster_size-1 downto 0);
signal vec_read_rs2_ID           : std_logic_vector(cluster_size-1 downto 0);
signal vec_write_rd_ID           : std_logic_vector(cluster_size-1 downto 0);
signal busy_DSP                  : array_2d(cluster_size-1 downto 0)(accl_range);
signal busy_DSP_int              : array_2d(cluster_size-1 downto 0)(accl_range);
signal state_DSP                 : array_3d(cluster_size-1 downto 0)(ACCL_NUM-1 downto 0)(1 downto 0);
signal state_LS                  : array_2d_states_LS(cluster_size-1 downto 0);
signal sc_word_count_wire        : array_2d_int(cluster_size-1 downto 0);
signal spm_bcast                 : std_logic_vector(cluster_size-1 downto 0);
signal harc_LS_wire              : array_2d_accl(cluster_size-1 downto 0);
signal harc_LS_wire_int          : array_2d_accl(cluster_size-1 downto 0);
signal ls_sc_data_write_wire     : array_2d(cluster_size-1 downto 0)(Data_Width-1 downto 0);
signal ls_sc_read_addr           : array_2d(cluster_size-1 downto 0)(Addr_Width-(SIMD_BITS+3) downto 0);
signal ls_sc_write_addr          : array_2d(cluster_size-1 downto 0)(Addr_Width-(SIMD_BITS+3) downto 0);
signal ls_sci_req                : array_2d(cluster_size-1 downto 0)(SPM_NUM-1 downto 0);
signal ls_sci_we                 : array_2d(cluster_size-1 downto 0)(SPM_NUM-1 downto 0);
signal kmemld_inflight           : array_2d(cluster_size-1 downto 0)(SPM_NUM-1 downto 0);
signal kmemstr_inflight          : array_2d(cluster_size-1 downto 0)(SPM_NUM-1 downto 0);
signal ls_sc_data_read_wire      : array_2d(cluster_size-1 downto 0)(Data_Width-1 downto 0);
signal ls_sci_wr_gnt             : std_logic_vector(cluster_size-1 downto 0);
signal ls_data_gnt_i             : array_2d(cluster_size-1 downto 0)(SPM_NUM-1 downto 0);
signal dsp_taken_branch          : array_2d(cluster_size-1 downto 0)(accl_range);
signal dsp_taken_branch_int      : array_2d(cluster_size-1 downto 0)(accl_range);
signal dsp_except_condition      : array_2d(cluster_size-1 downto 0)(accl_range);
signal dsp_except_condition_int  : array_2d(cluster_size-1 downto 0)(accl_range);

-- VCU Signals
signal rs1_to_sc_int               : std_logic_vector(SPM_ADDR_WID-1 downto 0);
signal rs2_to_sc_int               : std_logic_vector(SPM_ADDR_WID-1 downto 0);
signal rd_to_sc_int                : std_logic_vector(SPM_ADDR_WID-1 downto 0);
signal MVSIZE_int2                 : array_2d(harc_range)(Addr_Width downto 0);
signal MVTYPE_int2                 : array_2d(harc_range)(3 downto 0);
signal MPSCLFAC_int2               : array_2d(harc_range)(4 downto 0);
signal dsp_except_data_int         : array_2d(accl_range)(31 downto 0);
signal decoded_instruction_DSP_int : std_logic_vector(DSP_UNIT_INSTR_SET_SIZE-1 downto 0);
signal harc_EXEC_int2              : harc_range;
signal pc_IE_int                   : std_logic_vector(31 downto 0);  -- pc_IE is pc entering stage IE
signal RS1_Data_IE_int             : std_logic_vector(31 downto 0);
signal RS2_Data_IE_int             : std_logic_vector(31 downto 0);
signal RD_Data_IE_int              : std_logic_vector(31 downto 0);
signal dsp_instr_req_int2          : std_logic_vector(accl_range);
signal spm_rs1_int                 : std_logic;
signal spm_rs2_int                 : std_logic;
signal vec_read_rs1_ID_int         : std_logic;
signal vec_read_rs2_ID_int         : std_logic;
signal vec_write_rd_ID_int         : std_logic;
signal busy_DSP_int2               : std_logic_vector(accl_range);
signal state_DSP_int               : array_2d(ACCL_NUM-1 downto 0)(1 downto 0);
signal state_LS_int                : fsm_LS_states;
signal sc_word_count_wire_int      : integer;
signal spm_bcast_int               : std_logic;
signal harc_LS_wire_int2           : accl_range;
signal ls_sc_data_write_wire_int   : std_logic_vector(Data_Width-1 downto 0);
signal ls_sc_read_addr_int         : std_logic_vector(Addr_Width-(SIMD_BITS+3) downto 0);
signal ls_sc_write_addr_int        : std_logic_vector(Addr_Width-(SIMD_BITS+3) downto 0);
signal ls_sci_req_int              : std_logic_vector(SPM_NUM-1 downto 0);
signal ls_sci_we_int               : std_logic_vector(SPM_NUM-1 downto 0);
signal kmemld_inflight_int         : std_logic_vector(SPM_NUM-1 downto 0);
signal kmemstr_inflight_int        : std_logic_vector(SPM_NUM-1 downto 0);
signal ls_sc_data_read_wire_int    : std_logic_vector(Data_Width-1 downto 0);
signal ls_sci_wr_gnt_int           : std_logic;
signal ls_data_gnt_i_int           : std_logic_vector(SPM_NUM-1 downto 0);
signal dsp_taken_branch_int2       : std_logic_vector(accl_range);
signal dsp_except_condition_int2   : std_logic_vector(accl_range);

component klessydra_m_core
generic(
    THREAD_POOL_SIZE_GLOBAL : natural := 4;
    THREAD_POOL_SIZE        : natural;
    cluster_size_ceil       : natural;
    lutram_rf               : natural;
    latch_rf                : natural;
    RV32E                   : natural;
    RV32M                   : natural;
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
    clk_i                   : in  std_logic;
    clock_en_i              : in  std_logic;
    rst_ni                  : in  std_logic;
    test_en_i               : in  std_logic;
    boot_addr_i             : in  std_logic_vector(31 downto 0);
    core_id_i               : in  std_logic_vector(3 downto 0);
    cluster_id_i            : in  std_logic_vector(5 downto 0);
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
    source_hartid_i         : in  natural range THREAD_POOL_SIZE_GLOBAL-1 downto 0;
    source_hartid_o         : out natural range THREAD_POOL_SIZE_GLOBAL-1 downto 0;
    sw_irq_i                : in  std_logic_vector(THREAD_POOL_SIZE_GLOBAL-1 downto 0);
    sw_irq_o                : out std_logic_vector(THREAD_POOL_SIZE_GLOBAL-1 downto 0);
    sw_irq_served_i         : in  std_logic_vector(THREAD_POOL_SIZE-1 downto 0);
    sw_irq_served_o         : out std_logic_vector(THREAD_POOL_SIZE_GLOBAL-1 downto 0);
    -- VCU Signals
    harc_EXEC               : out natural range THREAD_POOL_SIZE-1 downto 0;
    MVSIZE                  : out array_2d(THREAD_POOL_SIZE-1 downto 0)(Addr_Width downto 0); -- AAA change this signal to ACCL_RANGE
    MVTYPE                  : out array_2d(THREAD_POOL_SIZE-1 downto 0)(3 downto 0);          -- AAA change this signal to ACCL_RANGE
    MPSCLFAC                : out array_2d(THREAD_POOL_SIZE-1 downto 0)(4 downto 0);          -- AAA change this signal to ACCL_RANGE
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
    THREAD_POOL_SIZE           : natural;
    accl_en                    : natural;
    replicate_accl_en          : natural;
    multithreaded_accl_en      : natural;
    SPM_NUM                    : natural;
    Addr_Width                 : natural;
    SIMD                       : natural;
    -------------------------------------
    ACCL_NUM                   : natural;
    FU_NUM                     : natural;
    TPS_CEIL                   : natural;
    TPS_BUF_CEIL               : natural;
    SPM_ADDR_WID               : natural;
    SIMD_BITS                  : natural;
    Data_Width                 : natural;
    SIMD_Width                 : natural
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

begin

T13_inst : klessydra_m_core
  generic map (
    THREAD_POOL_SIZE_GLOBAL => 4,
    THREAD_POOL_SIZE        => 3,
    cluster_size_ceil       => cluster_size_ceil,
    lutram_rf               => lutram_rf,
    latch_rf                => latch_rf,
    RV32E                   => RV32E,
    RV32M                   => RV32M,
    context_switch          => 1,
    morph_en                => 0,
    fetch_stage_en          => 0,
    branch_predict_en       => 0,
    btb_en                  => 0,
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
    SPM_ADDR_WID            => SPM_ADDR_WID, 
    SIMD_BITS               => SIMD_BITS, 
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
    core_id_i               => (others => '0'),
    cluster_id_i            => cluster_id_i,
    instr_req_o             => instr_req_int(0),
    instr_gnt_i             => instr_gnt_int(0),
    instr_rvalid_i          => instr_rvalid_int(0),
    instr_addr_o            => instr_addr_int(0),
    instr_rdata_i           => instr_rdata_int(0),
    data_req_o              => data_req_int(0),
    data_gnt_i              => data_gnt_int(0),
    data_rvalid_i           => data_rvalid_int(0),
    data_we_o               => data_we_int(0),
    data_be_o               => data_be_int(0),
    data_addr_o             => data_addr_int(0),
    data_wdata_o            => data_wdata_int(0),
    data_rdata_i            => data_rdata_int(0),
    data_err_i              => data_err_int(0),
    irq_i                   => irq_int(0),
    irq_id_i                => irq_id_int(0),
    irq_ack_o               => irq_ack_int(0),
    irq_id_o                => open,
    irq_sec_i               => irq_sec_int(0),
    sec_lvl_o               => sec_lvl_int(0),
    debug_req_i             => debug_req_int(0),
    debug_gnt_o             => debug_gnt_int(0),
    debug_rvalid_o          => debug_rvalid_int(0),
    debug_addr_i            => debug_addr_int(0),
    debug_we_i              => debug_we_int(0),
    debug_wdata_i           => debug_wdata_int(0),
    debug_rdata_o           => debug_rdata_int(0),
    debug_halted_o          => debug_halted_int(0),
    debug_halt_i            => debug_halt_int(0),
    debug_resume_i          => debug_resume_int(0),
    fetch_enable_i          => fetch_enable_int(0),
    core_busy_o             => core_busy_int(0),
    ext_perf_counters_i     => ext_perf_counters_int(0),
    core_select             => core_select,
    source_hartid_i         => source_hartid_o(1),
    source_hartid_o         => source_hartid_o(0), -- the output gets mapped to source_hartid_o(0) which goes as an input to the source_hartid_i of the S0 core
    sw_irq_i                => sw_irq_o(1),
    sw_irq_o                => sw_irq_o(0),
    sw_irq_served_i         => sw_irq_served_i(0)(THREAD_POOL_SIZE-1 downto 0),
    sw_irq_served_o         => sw_irq_served_o(0),
    harc_EXEC               => harc_EXEC_int(0),
    MVSIZE                  => MVSIZE_int(0),
    MVTYPE                  => MVTYPE_int(0),
    MPSCLFAC                => MPSCLFAC_int(0),
    pc_IE                   => pc_IE(0),
    rs1_to_sc               => rs1_to_sc(0),
    rs2_to_sc               => rs2_to_sc(0),
    rd_to_sc                => rd_to_sc(0),
    decoded_instruction_DSP => decoded_instruction_DSP(0),
    RS1_Data_IE             => RS1_Data_IE(0),
    RS2_Data_IE             => RS2_Data_IE(0),
    RD_Data_IE              => RD_Data_IE(0),
    dsp_instr_req           => dsp_instr_req_int(0),
    spm_rs1                 => spm_rs1(0),
    spm_rs2                 => spm_rs2(0),
    vec_read_rs1_ID         => vec_read_rs1_ID(0),
    vec_read_rs2_ID         => vec_read_rs2_ID(0),
    vec_write_rd_ID         => vec_write_rd_ID(0),
    busy_DSP                => busy_DSP_int(0),
    state_LS                => state_LS(0),
    sc_word_count_wire      => sc_word_count_wire(0),
    spm_bcast               => spm_bcast(0),
    harc_LS_wire            => harc_LS_wire_int(0),
    ls_sc_data_write_wire   => ls_sc_data_write_wire(0),
    ls_sc_read_addr         => ls_sc_read_addr(0),
    ls_sc_write_addr        => ls_sc_write_addr(0),
    ls_sci_req              => ls_sci_req(0),
    ls_sci_we               => ls_sci_we(0),
    kmemld_inflight         => kmemld_inflight(0),
    kmemstr_inflight        => kmemstr_inflight(0),
    ls_sc_data_read_wire    => ls_sc_data_read_wire(0),
    ls_sci_wr_gnt           => ls_sci_wr_gnt(0),
    ls_data_gnt_i           => ls_data_gnt_i(0),
    dsp_taken_branch        => dsp_taken_branch_int(0),
    dsp_except_condition    => dsp_except_condition_int(0)
  );

S1_inst : klessydra_m_core
  generic map (
    THREAD_POOL_SIZE_GLOBAL => 4,
    THREAD_POOL_SIZE        => 1,
    cluster_size_ceil       => cluster_size_ceil,
    lutram_rf               => lutram_rf,
    latch_rf                => latch_rf,
    RV32E                   => RV32E,
    RV32M                   => RV32M,
    context_switch          => 1,
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
    SPM_ADDR_WID            => SPM_ADDR_WID,
    SIMD_BITS               => SIMD_BITS,
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
    core_id_i               => (1 to 3 => '0') & '1',
    cluster_id_i            => cluster_id_i,
    instr_req_o             => instr_req_int(1),
    instr_gnt_i             => instr_gnt_int(1),
    instr_rvalid_i          => instr_rvalid_int(1),
    instr_addr_o            => instr_addr_int(1),
    instr_rdata_i           => instr_rdata_int(1),
    data_req_o              => data_req_int(1),
    data_gnt_i              => data_gnt_int(1),
    data_rvalid_i           => data_rvalid_int(1),
    data_we_o               => data_we_int(1),
    data_be_o               => data_be_int(1),
    data_addr_o             => data_addr_int(1),
    data_wdata_o            => data_wdata_int(1),
    data_rdata_i            => data_rdata_int(1),
    data_err_i              => data_err_int(1),
    irq_i                   => irq_int(1),
    irq_id_i                => irq_id_int(1),
    irq_ack_o               => irq_ack_int(1),
    irq_id_o                => open,
    irq_sec_i               => irq_sec_int(1),
    sec_lvl_o               => sec_lvl_int(1),
    debug_req_i             => debug_req_int(1),
    debug_gnt_o             => debug_gnt_int(1),
    debug_rvalid_o          => debug_rvalid_int(1),
    debug_addr_i            => debug_addr_int(1),
    debug_we_i              => debug_we_int(1),
    debug_wdata_i           => debug_wdata_int(1),
    debug_rdata_o           => debug_rdata_int(1),
    debug_halted_o          => debug_halted_int(1),
    debug_halt_i            => debug_halt_int(1),
    debug_resume_i          => debug_resume_int(1),
    fetch_enable_i          => fetch_enable_int(1),
    core_busy_o             => core_busy_int(1),
    ext_perf_counters_i     => ext_perf_counters_int(1),
    core_select             => core_select,
    source_hartid_i         => source_hartid_o(0),
    source_hartid_o         => source_hartid_o(1), -- the output gets mapped to source_hartid_o(1) which goes as an input to the source_hartid_i of the T13 core
    sw_irq_i                => sw_irq_o(0),
    sw_irq_o                => sw_irq_o(1),
    sw_irq_served_i         => sw_irq_served_i(1)(THREAD_POOL_SIZE_GLOBAL-1 downto THREAD_POOL_SIZE_GLOBAL-1),
    sw_irq_served_o         => sw_irq_served_o(1),
    harc_EXEC               => harc_EXEC_int(1),
    MVSIZE(0)               => MVSIZE_int(1)(0),   -- only mapping is for thread 0
    MVTYPE(0)               => MVTYPE_int(1)(0),   -- only mapping is for thread 0
    MPSCLFAC(0)             => MPSCLFAC_int(1)(0), -- only mapping is for thread 0
    pc_IE                   => pc_IE(1),
    rs1_to_sc               => rs1_to_sc(1),
    rs2_to_sc               => rs2_to_sc(1),
    rd_to_sc                => rd_to_sc(1),
    decoded_instruction_DSP => decoded_instruction_DSP(1),
    RS1_Data_IE             => RS1_Data_IE(1),
    RS2_Data_IE             => RS2_Data_IE(1),
    RD_Data_IE              => RD_Data_IE(1),
    dsp_instr_req           => dsp_instr_req_int(1),
    spm_rs1                 => spm_rs1(1),
    spm_rs2                 => spm_rs2(1),
    vec_read_rs1_ID         => vec_read_rs1_ID(1),
    vec_read_rs2_ID         => vec_read_rs2_ID(1),
    vec_write_rd_ID         => vec_write_rd_ID(1),
    busy_DSP                => busy_DSP_int(1),
    state_LS                => state_LS(1),
    sc_word_count_wire      => sc_word_count_wire(1),
    spm_bcast               => spm_bcast(1),
    harc_LS_wire            => harc_LS_wire_int(1),
    ls_sc_data_write_wire   => ls_sc_data_write_wire(1),
    ls_sc_read_addr         => ls_sc_read_addr(1),
    ls_sc_write_addr        => ls_sc_write_addr(1),
    ls_sci_req              => ls_sci_req(1),
    ls_sci_we               => ls_sci_we(1),
    kmemld_inflight         => kmemld_inflight(1),
    kmemstr_inflight        => kmemstr_inflight(1),
    ls_sc_data_read_wire    => ls_sc_data_read_wire(1),
    ls_sci_wr_gnt           => ls_sci_wr_gnt(1),
    ls_data_gnt_i           => ls_data_gnt_i(1),
    dsp_taken_branch        => dsp_taken_branch_int(1),
    dsp_except_condition    => dsp_except_condition_int(1)
  );

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
    rs1_to_sc                  => rs1_to_sc_int,
    rs2_to_sc                  => rs2_to_sc_int,
    rd_to_sc                   => rd_to_sc_int,
    MVSIZE                     => MVSIZE_int2,
    MVTYPE                     => MVTYPE_int2,
    MPSCLFAC                   => MPSCLFAC_int2,
    dsp_except_data            => dsp_except_data_int,
    dsp_taken_branch           => dsp_taken_branch_int2,
    dsp_except_condition       => dsp_except_condition_int2,
    decoded_instruction_DSP    => decoded_instruction_DSP_int,
    harc_EXEC                  => harc_EXEC_int2,
    pc_IE                      => pc_IE_int,
    RS1_Data_IE                => RS1_Data_IE_int,
    RS2_Data_IE                => RS2_Data_IE_int,
    RD_Data_IE                 => RD_Data_IE_int(Addr_Width -1 downto 0),
    dsp_instr_req              => dsp_instr_req_int2,
    spm_rs1                    => spm_rs1_int,
    spm_rs2                    => spm_rs2_int,
    vec_read_rs1_ID            => vec_read_rs1_ID_int,
    vec_read_rs2_ID            => vec_read_rs2_ID_int,
    vec_write_rd_ID            => vec_write_rd_ID_int,
    busy_DSP                   => busy_DSP_int2,
    data_rvalid_i              => data_rvalid_i,
    state_LS                   => state_LS_int,
    sc_word_count_wire         => sc_word_count_wire_int,
    spm_bcast                  => spm_bcast_int,
    harc_LS_wire               => harc_LS_wire_int2,
    ls_sc_data_write_wire      => ls_sc_data_write_wire_int,
    ls_sc_read_addr            => ls_sc_read_addr_int,
    ls_sc_write_addr           => ls_sc_write_addr_int,
    ls_sci_req                 => ls_sci_req_int,
    ls_sci_we                  => ls_sci_we_int,
    kmemld_inflight            => kmemld_inflight_int,
    kmemstr_inflight           => kmemstr_inflight_int,
    ls_sc_data_read_wire       => ls_sc_data_read_wire_int,
    ls_sci_wr_gnt              => ls_sci_wr_gnt_int,
    ls_data_gnt_i              => ls_data_gnt_i_int
  );

--   ██████╗ ██████╗ ██████╗ ███████╗    ███╗   ███╗██╗   ██╗██╗  ████████╗██╗██████╗ ██╗     ███████╗██╗  ██╗███████╗██████╗ 
--  ██╔════╝██╔═══██╗██╔══██╗██╔════╝    ████╗ ████║██║   ██║██║  ╚══██╔══╝██║██╔══██╗██║     ██╔════╝╚██╗██╔╝██╔════╝██╔══██╗
--  ██║     ██║   ██║██████╔╝█████╗      ██╔████╔██║██║   ██║██║     ██║   ██║██████╔╝██║     █████╗   ╚███╔╝ █████╗  ██████╔╝
--  ██║     ██║   ██║██╔══██╗██╔══╝      ██║╚██╔╝██║██║   ██║██║     ██║   ██║██╔═══╝ ██║     ██╔══╝   ██╔██╗ ██╔══╝  ██╔══██╗
--  ╚██████╗╚██████╔╝██║  ██║███████╗    ██║ ╚═╝ ██║╚██████╔╝███████╗██║   ██║██║     ███████╗███████╗██╔╝ ██╗███████╗██║  ██║
--   ╚═════╝ ╚═════╝ ╚═╝  ╚═╝╚══════╝    ╚═╝     ╚═╝ ╚═════╝ ╚══════╝╚═╝   ╚═╝╚═╝     ╚══════╝╚══════╝╚═╝  ╚═╝╚══════╝╚═╝  ╚═╝
--                                                                                                                            

  process(all)
  begin
    for i in 0 to cluster_size-1 loop
      source_hartid(i) <= source_hartid_o(i)+THREAD_POOL_SIZE*i; -- This makes the hart_0 in core_1 have a hartID of 0+THREAD_POOL_SIZE*1
    end loop;
  end process;

  rs1_to_sc_int               <= rs1_to_sc(core_select);
  rs2_to_sc_int               <= rs2_to_sc(core_select);
  rd_to_sc_int                <= rd_to_sc(core_select);
  MVSIZE_int2                 <= MVSIZE(core_select);
  MVTYPE_int2                 <= MVTYPE(core_select);
  MPSCLFAC_int2               <= MPSCLFAC(core_select);
  dsp_except_data_int         <= dsp_except_data(core_select);
  decoded_instruction_DSP_int <= decoded_instruction_DSP(core_select);
  harc_EXEC_int2              <= harc_EXEC(core_select);
  pc_IE_int                   <= pc_IE(core_select);
  RS1_Data_IE_int             <= RS1_Data_IE(core_select);
  RS2_Data_IE_int             <= RS2_Data_IE(core_select);
  RD_Data_IE_int(Addr_Width-1 downto 0) <= RD_Data_IE(core_select)(Addr_Width -1 downto 0);
  dsp_instr_req_int2          <= dsp_instr_req(core_select);
  spm_rs1_int                 <= spm_rs1(core_select);
  spm_rs2_int                 <= spm_rs2(core_select);
  vec_read_rs1_ID_int         <= vec_read_rs1_ID(core_select);
  vec_read_rs2_ID_int         <= vec_read_rs2_ID(core_select);
  vec_write_rd_ID_int         <= vec_write_rd_ID(core_select);
  state_LS_int                <= state_LS(core_select);
  sc_word_count_wire_int      <= sc_word_count_wire(core_select);
  spm_bcast_int               <= spm_bcast(core_select);
  harc_LS_wire_int2           <= harc_LS_wire(core_select);
  ls_sc_data_write_wire_int   <= ls_sc_data_write_wire(core_select);
  ls_sc_read_addr_int         <= ls_sc_read_addr(core_select);
  ls_sc_write_addr_int        <= ls_sc_write_addr(core_select);
  ls_sci_req_int              <= ls_sci_req(core_select);
  ls_sci_we_int               <= ls_sci_we(core_select);
  kmemld_inflight_int         <= kmemld_inflight(core_select);
  kmemstr_inflight_int        <= kmemstr_inflight(core_select);

  sw_irq <= sw_irq_o(0) or sw_irq_o(1);

  -- Output Multiplexing
  instr_req_o                         <= instr_req_int(core_select);
  instr_addr_o                        <= instr_addr_int(core_select);
  data_req_o                          <= data_req_int(core_select);
  data_we_o                           <= data_we_int(core_select);
  data_be_o                           <= data_be_int(core_select);
  data_addr_o                         <= data_addr_int(core_select);
  data_wdata_o                        <= data_wdata_int(core_select);
  irq_ack_o                           <= irq_ack_int(core_select);
  irq_id_o                            <= irq_id_int(core_select);
  sec_lvl_o                           <= sec_lvl_int(core_select);
  debug_gnt_o                         <= debug_gnt_int(core_select);
  debug_rvalid_o                      <= debug_rvalid_int(core_select);
  debug_rdata_o                       <= debug_rdata_int(core_select);
  debug_halted_o                      <= debug_halted_int(core_select);
  core_busy_o                         <= core_busy_int(core_select);
  -- Input Multiplexing
  process(all)
  begin
    instr_gnt_int                        <= (others => '0');
    instr_rvalid_int                     <= (others => '0');
    instr_rdata_int                      <= (others => (others =>'0'));
    data_gnt_int                         <= (others => '0');
    data_rvalid_int                      <= (others => '0');
    data_rdata_int                       <= (others => (others => '0'));
    data_err_int                         <= (others => '0');
    irq_int                              <= (others => '0');
    irq_id_int                           <= (others => (others => '0'));
    irq_sec_int                          <= (others => '0');
    debug_req_int                        <= (others => '0');
    debug_addr_int                       <= (others => (others => '0'));
    debug_we_int                         <= (others => '0');
    debug_wdata_int                      <= (others => (others => '0'));
    debug_halt_int                       <= (others => '0');
    debug_resume_int                     <= (others => '0');
    fetch_enable_int                     <= (others => '0');
    ext_perf_counters_int                <= (others => (others => '0'));
    busy_DSP                             <= (others => (others => '0'));
    ls_sc_data_read_wire                 <= (others => (others => '0'));
    ls_sci_wr_gnt                        <= (others => '0');
    ls_data_gnt_i                        <= (others => (others => '0'));
    dsp_taken_branch                     <= (others => (others => '0'));
    dsp_except_condition                 <= (others => (others => '0'));
    instr_gnt_int(core_select)           <= instr_gnt_i;
    instr_rvalid_int(core_select)        <= instr_rvalid_i;
    instr_rdata_int(core_select)         <= instr_rdata_i;
    data_gnt_int(core_select)            <= data_gnt_i;
    data_rvalid_int(core_select)         <= data_rvalid_i;
    data_rdata_int(core_select)          <= data_rdata_i;
    data_err_int(core_select)            <= data_err_i;
    irq_int(core_select)                 <= irq_i;
    irq_id_int(core_select)              <= irq_id_i;
    irq_sec_int(core_select)             <= irq_sec_i;
    debug_req_int(core_select)           <= debug_req_i;
    debug_addr_int(core_select)          <= debug_addr_i;
    debug_we_int(core_select)            <= debug_we_i;
    debug_wdata_int(core_select)         <= debug_wdata_i;
    debug_halt_int(core_select)          <= debug_halt_i;
    debug_resume_int(core_select)        <= debug_resume_i;
    fetch_enable_int(core_select)        <= fetch_enable_i;
    ext_perf_counters_int(core_select)   <= ext_perf_counters_i;
    busy_DSP(core_select)                <= busy_DSP_int2;
    ls_sc_data_read_wire(core_select)    <= ls_sc_data_read_wire_int;
    ls_sci_wr_gnt(core_select)           <= ls_sci_wr_gnt_int;
    ls_data_gnt_i(core_select)           <= ls_data_gnt_i_int;
    dsp_taken_branch(core_select)        <= dsp_taken_branch_int2;
    dsp_except_condition(core_select)    <= dsp_except_condition_int2;
  end process;

  process(clk_i, rst_ni)
  begin
    if rst_ni = '0' then
      core_select <= 0;  -- T13 is selected at reset
      VCU_hartID  <= 0;
    elsif rising_edge(clk_i) then
      if unsigned(sw_irq(THREAD_POOL_SIZE_GLOBAL-2 downto 0)) /= 0 then
        core_select <= 0;
      elsif sw_irq(THREAD_POOL_SIZE_GLOBAL-1) /= '0' then
        core_select <= 1;
        if accl_en = 1 and replicate_accl_en = 1 then
          VCU_hartID  <= source_hartid_o(0);
        end if;
      end if;
    end if;
  end process;

  process(all)
  begin
    harc_EXEC                   <= harc_EXEC_int;
    harc_LS_wire                <= harc_LS_wire_int;
    MVSIZE(0)                   <= MVSIZE_int(0);
    MVSIZE(1)                   <= (others => (others => '0'));
    MVTYPE(0)                   <= MVTYPE_int(0);
    MVTYPE(1)                   <= (others => (others => '0'));
    MPSCLFAC(0)                 <= MPSCLFAC_int(0);
    MPSCLFAC(1)                 <= (others => (others => '0'));
    dsp_instr_req(0)            <= dsp_instr_req_int(0);
    dsp_instr_req(1)            <= (others => '0');
    busy_DSP_int(0)             <= busy_DSP(0);
    busy_DSP_int(1)             <= (others => '0');
    dsp_taken_branch_int(0)     <= dsp_taken_branch(0);
    dsp_taken_branch_int(1)     <= (others => '0');
    dsp_except_condition_int(0) <= dsp_except_condition(0);
    dsp_except_condition_int(1) <= (others => '0');
    if core_select = 1 then
      -- in case we do a context switch, we make sure we take the hartID from the IMT core 
      harc_EXEC(1)                 <= VCU_hartID;
      harc_LS_wire(1)              <= VCU_hartID;
      MVSIZE(1)(VCU_hartID)        <= MVSIZE_int(1)(0);
      MVTYPE(1)(VCU_hartID)        <= MVTYPE_int(1)(0);
      MPSCLFAC(1)(VCU_hartID)      <= MPSCLFAC_int(1)(0);
      dsp_instr_req(1)(VCU_hartID) <= dsp_instr_req_int(1)(0);
      busy_DSP_int(1)(0)           <= busy_DSP(1)(VCU_hartID);
      dsp_taken_branch(1)(0)       <= dsp_taken_branch_int(1)(VCU_hartID);
      dsp_except_condition(1)(0)   <= dsp_except_condition_int(1)(VCU_hartID);
    end if;
  end process;

end Het_Cluster;