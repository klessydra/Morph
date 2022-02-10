
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
    LUTRAM_RF             : natural := 1;   -- Changes the regfile from flip-flop type into BRAM type
    RV32E                 : natural := 0;   -- Regfile size, Can be set to 32 for RV32E being 0 else 16 for RV32E being set to 1
    RV32M                 : natural := 1;   -- Enables the M-extension of the risc-v instruction set
    context_switch        : natural := 1;   -- Enables the context switching between cores
    morph_en              : natural := 1;   -- Enables the generation of the logic that allows processor to morph from an IMT to a single core processor
    fetch_stage_en        : natural := 1;   -- Enables the generation of a fetch stage buffer, else the incoming instrution will go directly to the decode stage.
    branch_predict_en     : natural := 1;   -- This enables the branch predictor
    btb_en                : natural := 1;   -- Enables the BTB instead of the single bit predictor
    btb_len               : natural := 6;   -- Indicates the number of entries in the btb which is 2^btb_len
    superscalar_exec_en   : natural := 1;   -- Enables superscalar execution when set to 1, else the stall of the pipeline will depend on tha latency of the instruction
    accl_en               : natural := 1;   -- Enables the generation of the general purpose accelerator
    replicate_accl_en     : natural := 0;   -- Set to 1 to replicate the accelerator for every thread
    multithreaded_accl_en : natural := 0;   -- Set to 1 to let the replicated accelerator share the functional units (note: replicate_accl_en must be set to '1')
    SPM_NUM               : natural := 3;   -- The number of scratchpads available "Minimum allowed is two"
    Addr_Width            : natural := 13;  -- This address is for scratchpads. Setting this will make the size of the spm to be: "2^Addr_Width -1"
    SPM_STRT_ADDR         : std_logic_vector(31 downto 0) := x"1000_0000";  -- This is starting address of the spms, it shouldn't overlap any sections in the memory map
    SIMD                  : natural := 4;   -- Changing the SIMD, would change the number of the functional units in the dsp, and the number of banks in the spms (can be power of 2 only e.g. 1,2,4,8)
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

constant cluster_size            : natural := 2;
constant cluster_size_ceil       : natural := integer(ceil(log2(real(cluster_size))));
constant THREAD_POOL_SIZE_GLOBAL : natural := 4;

signal core_select               : natural range cluster_size-1 downto 0;
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
signal sw_irq_i                  : std_logic_vector(THREAD_POOL_SIZE_GLOBAL-1 downto 0);
signal sw_irq_served_i           : array_2d(cluster_size-1 downto 0)(THREAD_POOL_SIZE_GLOBAL-1 downto 0);

component klessydra_m_core
generic(
    THREAD_POOL_SIZE_GLOBAL : natural := 4;
    THREAD_POOL_SIZE        : natural := 3;
    cluster_size_ceil       : natural := 1;
    LUTRAM_RF               : natural := 1;
    RV32E                   : natural := 0;
    RV32M                   : natural := 1;
    context_switch          : natural := 1;
    morph_en                : natural := 1;
    fetch_stage_en          : natural := 1;
    branch_predict_en       : natural := 1;
    btb_en                  : natural := 1;
    btb_len                 : natural := 6;
    superscalar_exec_en     : natural := 1;
    accl_en                 : natural := 0;
    replicate_accl_en       : natural := 0;
    multithreaded_accl_en   : natural := 0;
    SPM_NUM                 : natural := 3;
    Addr_Width              : natural := 13;
    SPM_STRT_ADDR           : std_logic_vector(31 downto 0) := x"1000_0000";
    SIMD                    : natural := 8;
    MCYCLE_EN               : natural := 0;   
    MINSTRET_EN             : natural := 0;   
    MHPMCOUNTER_EN          : natural := 0;   
    count_all               : natural := 1;   
    debug_en                : natural := 0;   
    tracer_en               : natural := 0;   
    N_EXT_PERF_COUNTERS     : integer := 0; 
    INSTR_RDATA_WIDTH       : integer := 32;
    N_HWLP                  : integer := 2; 
    N_HWLP_BITS             : integer := 4  
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
    sw_irq_i            : in  std_logic_vector(THREAD_POOL_SIZE-1 downto 0);
    sw_irq_o            : out std_logic_vector(THREAD_POOL_SIZE_GLOBAL-1 downto 0);
    sw_irq_served_i     : in  std_logic_vector(THREAD_POOL_SIZE-1 downto 0);
    sw_irq_served_o     : out std_logic_vector(THREAD_POOL_SIZE_GLOBAL-1 downto 0)
);
end component;

begin

T13_inst : klessydra_m_core
  generic map (
    THREAD_POOL_SIZE_GLOBAL => 4,
    THREAD_POOL_SIZE        => 3,
    cluster_size_ceil       => cluster_size_ceil,
    LUTRAM_RF               => LUTRAM_RF,
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
    N_EXT_PERF_COUNTERS     => N_EXT_PERF_COUNTERS,
    INSTR_RDATA_WIDTH       => INSTR_RDATA_WIDTH,
    N_HWLP                  => N_HWLP,
    N_HWLP_BITS             => N_HWLP_BITS
  )
  port map (
    clk_i                 => clk_i,
    clock_en_i            => clock_en_i,
    rst_ni                => rst_ni,
    test_en_i             => test_en_i,
    boot_addr_i           => boot_addr_i,
    core_id_i             => (others => '0'),
    cluster_id_i          => cluster_id_i,
    instr_req_o           => instr_req_int(0),
    instr_gnt_i           => instr_gnt_int(0),
    instr_rvalid_i        => instr_rvalid_int(0),
    instr_addr_o          => instr_addr_int(0),
    instr_rdata_i         => instr_rdata_int(0),
    data_req_o            => data_req_int(0),
    data_gnt_i            => data_gnt_int(0),
    data_rvalid_i         => data_rvalid_int(0),
    data_we_o             => data_we_int(0),
    data_be_o             => data_be_int(0),
    data_addr_o           => data_addr_int(0),
    data_wdata_o          => data_wdata_int(0),
    data_rdata_i          => data_rdata_int(0),
    data_err_i            => data_err_int(0),
    irq_i                 => irq_int(0),
    irq_id_i              => irq_id_int(0),
    irq_ack_o             => irq_ack_int(0),
    irq_id_o              => open,
    irq_sec_i             => irq_sec_int(0),
    sec_lvl_o             => sec_lvl_int(0),
    debug_req_i           => debug_req_int(0),
    debug_gnt_o           => debug_gnt_int(0),
    debug_rvalid_o        => debug_rvalid_int(0),
    debug_addr_i          => debug_addr_int(0),
    debug_we_i            => debug_we_int(0),
    debug_wdata_i         => debug_wdata_int(0),
    debug_rdata_o         => debug_rdata_int(0),
    debug_halted_o        => debug_halted_int(0),
    debug_halt_i          => debug_halt_int(0),
    debug_resume_i        => debug_resume_int(0),
    fetch_enable_i        => fetch_enable_int(0),
    core_busy_o           => core_busy_int(0),
    ext_perf_counters_i   => ext_perf_counters_int(0),
    core_select           => core_select,
    source_hartid_i       => source_hartid_o(1),
    source_hartid_o       => source_hartid_o(0), -- the output gets mapped to source_hartid_o(0) which goes as an input to the source_hartid_i of the S0 core
    sw_irq_i              => sw_irq_i(THREAD_POOL_SIZE-1 downto 0),
    sw_irq_o              => sw_irq_o(0),
    sw_irq_served_i       => sw_irq_served_i(0)(THREAD_POOL_SIZE-1 downto 0),
    sw_irq_served_o       => sw_irq_served_o(0)
  );

S1_inst : klessydra_m_core
  generic map (
    THREAD_POOL_SIZE_GLOBAL => 4,
    THREAD_POOL_SIZE        => 1,
    cluster_size_ceil       => cluster_size_ceil,
    LUTRAM_RF               => LUTRAM_RF,
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
    N_EXT_PERF_COUNTERS     => N_EXT_PERF_COUNTERS,
    INSTR_RDATA_WIDTH       => INSTR_RDATA_WIDTH,
    N_HWLP                  => N_HWLP,
    N_HWLP_BITS             => N_HWLP_BITS
  )
  port map (
    clk_i                 => clk_i,
    clock_en_i            => clock_en_i,
    rst_ni                => rst_ni,
    test_en_i             => test_en_i,
    boot_addr_i           => boot_addr_i,
    core_id_i             => (1 to 3 => '0') & '1',
    cluster_id_i          => cluster_id_i,
    instr_req_o           => instr_req_int(1),
    instr_gnt_i           => instr_gnt_int(1),
    instr_rvalid_i        => instr_rvalid_int(1),
    instr_addr_o          => instr_addr_int(1),
    instr_rdata_i         => instr_rdata_int(1),
    data_req_o            => data_req_int(1),
    data_gnt_i            => data_gnt_int(1),
    data_rvalid_i         => data_rvalid_int(1),
    data_we_o             => data_we_int(1),
    data_be_o             => data_be_int(1),
    data_addr_o           => data_addr_int(1),
    data_wdata_o          => data_wdata_int(1),
    data_rdata_i          => data_rdata_int(1),
    data_err_i            => data_err_int(1),
    irq_i                 => irq_int(1),
    irq_id_i              => irq_id_int(1),
    irq_ack_o             => irq_ack_int(1),
    irq_id_o              => open,
    irq_sec_i             => irq_sec_int(1),
    sec_lvl_o             => sec_lvl_int(1),
    debug_req_i           => debug_req_int(1),
    debug_gnt_o           => debug_gnt_int(1),
    debug_rvalid_o        => debug_rvalid_int(1),
    debug_addr_i          => debug_addr_int(1),
    debug_we_i            => debug_we_int(1),
    debug_wdata_i         => debug_wdata_int(1),
    debug_rdata_o         => debug_rdata_int(1),
    debug_halted_o        => debug_halted_int(1),
    debug_halt_i          => debug_halt_int(1),
    debug_resume_i        => debug_resume_int(1),
    fetch_enable_i        => fetch_enable_int(1),
    core_busy_o           => core_busy_int(1),
    ext_perf_counters_i   => ext_perf_counters_int(1),
    core_select           => core_select,
    source_hartid_i       => source_hartid_o(0),
    source_hartid_o       => source_hartid_o(1), -- the output gets mapped to source_hartid_o(1) which goes as an input to the source_hartid_i of the T13 core
    sw_irq_i              => sw_irq_i(THREAD_POOL_SIZE_GLOBAL-1 downto THREAD_POOL_SIZE_GLOBAL-1),
    sw_irq_o              => sw_irq_o(1),
    sw_irq_served_i       => sw_irq_served_i(1)(THREAD_POOL_SIZE_GLOBAL-1 downto THREAD_POOL_SIZE_GLOBAL-1),
    sw_irq_served_o       => sw_irq_served_o(1)
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

  sw_irq_i <= sw_irq_o(0) or sw_irq_o(1);

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
    instr_gnt_int                       <= (others => '0');
    instr_rvalid_int                    <= (others => '0');
    instr_rdata_int                     <= (others => (others =>'0'));
    data_gnt_int                        <= (others => '0');
    data_rvalid_int                     <= (others => '0');
    data_rdata_int                      <= (others => (others => '0'));
    data_err_int                        <= (others => '0');
    irq_int                             <= (others => '0');
    irq_id_int                          <= (others => (others => '0'));
    irq_sec_int                         <= (others => '0');
    debug_req_int                       <= (others => '0');
    debug_addr_int                      <= (others => (others => '0'));
    debug_we_int                        <= (others => '0');
    debug_wdata_int                     <= (others => (others => '0'));
    debug_halt_int                      <= (others => '0');
    debug_resume_int                    <= (others => '0');
    fetch_enable_int                    <= (others => '0');
    ext_perf_counters_int               <= (others => (others => '0'));
    instr_gnt_int(core_select)          <= instr_gnt_i;
    instr_rvalid_int(core_select)       <= instr_rvalid_i;
    instr_rdata_int(core_select)        <= instr_rdata_i;
    data_gnt_int(core_select)           <= data_gnt_i;
    data_rvalid_int(core_select)        <= data_rvalid_i;
    data_rdata_int(core_select)         <= data_rdata_i;
    data_err_int(core_select)           <= data_err_i;
    irq_int(core_select)                <= irq_i;
    irq_id_int(core_select)             <= irq_id_i;
    irq_sec_int(core_select)            <= irq_sec_i;
    debug_req_int(core_select)          <= debug_req_i;
    debug_addr_int(core_select)         <= debug_addr_i;
    debug_we_int(core_select)           <= debug_we_i;
    debug_wdata_int(core_select)        <= debug_wdata_i;
    debug_halt_int(core_select)         <= debug_halt_i;
    debug_resume_int(core_select)       <= debug_resume_i;
    fetch_enable_int(core_select)       <= fetch_enable_i;
    ext_perf_counters_int(core_select)  <= ext_perf_counters_i;
  end process;

  process(clk_i, rst_ni)
  begin
    if rst_ni = '0' then
      core_select <= 0;  -- T13 is selected at reset
    elsif rising_edge(clk_i) then
      if unsigned(sw_irq_i(THREAD_POOL_SIZE_GLOBAL-2 downto 0)) /= 0 then
        core_select <= 0;
      elsif sw_irq_i(THREAD_POOL_SIZE_GLOBAL-1) /= '0' then
        core_select <= 1;
      end if;
    end if;
  end process;

end Het_Cluster;