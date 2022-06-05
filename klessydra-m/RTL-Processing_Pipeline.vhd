--------------------------------------------------------------------------------------------------------------
--  Processing Pipeline --                                                                                  --
--  Author(s): Abdallah Cheikh abdallah.cheikh@uniroma1.it (abdallah93.as@gmail.com)                        --
--                                                                                                          --
--  Date Modified: 07-04-2020                                                                               --
--------------------------------------------------------------------------------------------------------------
--  The processing pipeline encapsulates all the componenets containing the datapath of the instruction     --
--  Also in this entity there is a non-synthesizable instruction tracer that displays the trace of all the  --
--  the instructions entering the pipe.                                                                     --
--------------------------------------------------------------------------------------------------------------

-- ieee packages ------------
library ieee;
use ieee.math_real.all;
use ieee.std_logic_1164.all;
use ieee.std_logic_misc.all;
use ieee.numeric_std.all;
use std.textio.all;

-- local packages ------------
use work.riscv_klessydra.all;
--use work.klessydra_parameters.all;

-- pipeline  pinout --------------------
entity Pipeline is
  generic(
    THREAD_POOL_SIZE_GLOBAL    : natural;
    THREAD_POOL_SIZE           : natural;
    LUTRAM_RF                  : natural;
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
    --------------------------------
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
    harc_IF                    : in  natural range THREAD_POOL_SIZE-1 downto 0;
    irq_pending                : in  std_logic_vector(THREAD_POOL_SIZE-1 downto 0);
    csr_instr_done             : in  std_logic;
    csr_access_denied_o        : in  std_logic;
    csr_rdata_o                : in  std_logic_vector (31 downto 0);
    MVSIZE                     : in  array_2d(THREAD_POOL_SIZE-1 downto 0)(Addr_Width downto 0);
    MVTYPE                     : in  array_2d(THREAD_POOL_SIZE-1 downto 0)(3 downto 0);
    MPSCLFAC                   : in  array_2d(THREAD_POOL_SIZE-1 downto 0)(4 downto 0);
    MHARTID                    : in  array_2d(THREAD_POOL_SIZE-1 downto 0)(9  downto 0);
    MSTATUS                    : in  array_2d(THREAD_POOL_SIZE-1 downto 0)(1 downto 0);
    MPIP                       : in  array_2d(THREAD_POOL_SIZE-1 downto 0)(THREAD_POOL_SIZE_GLOBAL-1 downto 0);
    PCER                       : in  array_2d(THREAD_POOL_SIZE-1 downto 0)(31 downto 0);
    block_input_inst_wire      : in  std_logic;
    block_input_inst           : in  std_logic;
    served_irq                 : out std_logic_vector(THREAD_POOL_SIZE-1 downto 0);
    served_pending_irq         : out std_logic_vector(THREAD_POOL_SIZE-1 downto 0);
    WFI_Instr                  : out std_logic;
    misaligned_err             : out std_logic;
    pc_ID                      : out std_logic_vector(31 downto 0);
    pc_IE                      : out std_logic_vector(31 downto 0);
    ie_except_data             : out std_logic_vector(31 downto 0);
    ls_except_data             : out std_logic_vector(31 downto 0);
    dsp_except_data            : out array_2d(ACCL_NUM-1 downto 0)(31 downto 0);
    taken_branch               : out std_logic;
    ie_taken_branch            : out std_logic;
    ls_taken_branch            : out std_logic;
    dsp_taken_branch           : in  std_logic_vector(ACCL_NUM-1 downto 0);
    set_branch_condition       : out std_logic;
    set_except_condition       : out std_logic;
    ie_except_condition        : out std_logic;
    ls_except_condition        : out std_logic;
    dsp_except_condition       : in  std_logic_vector(ACCL_NUM-1 downto 0);
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
    harc_FETCH                 : out natural range THREAD_POOL_SIZE-1 downto 0;
    harc_ID                    : out natural range THREAD_POOL_SIZE-1 downto 0;
    harc_EXEC                  : out natural range THREAD_POOL_SIZE-1 downto 0;
    harc_to_csr                : out natural range THREAD_POOL_SIZE_GLOBAL-1 downto 0;
    instr_word_IE              : out std_logic_vector(31 downto 0);
    PC_offset                  : out std_logic_vector(31 downto 0);
    absolute_address           : out std_logic_vector(31 downto 0);
    ebreak_instr               : out std_logic;
    data_addr_internal         : out std_logic_vector(31 downto 0);
    absolute_jump              : out std_logic_vector(THREAD_POOL_SIZE-1 downto 0);
    regfile                    : out array_3d(THREAD_POOL_SIZE-1 downto 0)(RF_SIZE-1 downto 0)(31 downto 0);
    PC_offset_ID               : out std_logic_vector(31 downto 0);
    set_branch_condition_ID    : out std_logic;
    branch_FETCH               : out std_logic;
    jump_FETCH                 : out std_logic;
    jalr_FETCH                 : out std_logic;
    branch_addr_FETCH          : out std_logic_vector(31 downto 0);
    jump_addr_FETCH            : out std_logic_vector(31 downto 0);
    jalr_addr_FETCH            : out std_logic_vector(31 downto 0);
    harc_sleep_wire            : in  std_logic_vector(THREAD_POOL_SIZE-1 downto 0);
    harc_sleep                 : in  std_logic_vector(THREAD_POOL_SIZE-1 downto 0);
    wfi_hart_wire              : in  std_logic_vector(THREAD_POOL_SIZE-1 downto 0);
    CORE_STATE                 : in  std_logic_vector(THREAD_POOL_BASELINE downto 0);
    IMT_ACTIVE_HARTS           : in  natural;
    HET_CLUSTER_S1_CORE        : in  std_logic;
    halt_update                : out std_logic_vector(THREAD_POOL_SIZE-1 downto 0);

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
    -- VCU  Signals
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
  end entity;  ------------------------------------------


architecture Pipe of Pipeline is

  subtype harc_range is natural range THREAD_POOL_SIZE - 1 downto 0;
  subtype accl_range is integer range ACCL_NUM - 1 downto 0;
  subtype fu_range   is integer range FU_NUM - 1 downto 0;

  signal rs1_valid_ID           : std_logic;
  signal rs2_valid_ID           : std_logic;
  signal rd_valid_ID            : std_logic;
  signal rd_read_valid_ID       : std_logic;

  signal state_IE               : fsm_IE_states;
  signal state_DSP              : array_2d(accl_range)(1 downto 0);
  signal instr_rvalid_state     : std_logic;
  signal busy_ID                : std_logic;
  signal core_busy_IE           : std_logic;
  signal data_dependency        : std_logic;
  signal bypass_rs1             : std_logic;
  signal bypass_rs2             : std_logic;
  signal bypass_rd_read         : std_logic;
  signal jalr_stall             : std_logic;
  signal branch_stall           : std_logic;

  signal sleep_state            : std_logic;
  signal ls_instr_done          : std_logic;
  signal ie_to_csr              : std_logic_vector(31 downto 0);
  signal ie_csr_wdata_i         : std_logic_vector(31 downto 0);
  signal ie_instr_req           : std_logic;
  signal ls_instr_req           : std_logic;
  signal core_busy_LS           : std_logic;
  signal busy_LS                : std_logic;
  signal LS_WB_EN               : std_logic;
  signal LS_WB_EN_wire          : std_logic;
  signal LS_WB                  : std_logic_vector(31 downto 0);
  signal data_addr_internal_IE  : std_logic_vector(31 downto 0);
  signal data_be_ID             : std_logic_vector(3 downto 0);
  signal data_width_ID          : std_logic_vector(1 downto 0);
  signal IE_WB_EN               : std_logic;
  signal IE_WB_EN_wire          : std_logic;
  signal IE_WB                  : std_logic_vector(31 downto 0);
  signal MUL_WB_EN              : std_logic;
  signal MUL_WB_EN_wire         : std_logic;
  signal MUL_WB                 : std_logic_vector(31 downto 0);

  signal comparator_en : std_logic;

  signal flush_decode  : std_logic_vector(harc_range);
  signal jalr_flush    : std_logic_vector(harc_range);

  signal halt_IE       : std_logic;
  signal halt_LSU      : std_logic;
  signal WB_EN_next_ID : std_logic;

  -- program counters --
  signal pc_WB     : std_logic_vector(31 downto 0);  -- pc_WB is pc entering stage WB

  -- instruction register and instr. propagation registers --
  signal instr_word_FETCH        : std_logic_vector(31 downto 0);
  signal instr_word_ID           : std_logic_vector(31 downto 0);  -- latch needed for long-latency program memory
  signal instr_rvalid_ID         : std_logic;  -- validity bit at ID input
  signal instr_word_LS_WB        : std_logic_vector(31 downto 0);
  signal instr_word_IE_WB        : std_logic_vector(31 downto 0);
  signal decoded_branching_instr : std_logic_vector(BRANCHING_INSTR_SET_SIZE-1 downto 0);
  signal decoded_instruction_IE  : std_logic_vector(EXEC_UNIT_INSTR_SET_SIZE-1 downto 0);
  signal decoded_instruction_LS  : std_logic_vector(LS_UNIT_INSTR_SET_SIZE-1 downto 0);

  signal zero_rd                : std_logic;
  signal amo_load_skip          : std_logic;
  signal amo_load               : std_logic;
  signal amo_store              : std_logic;
  signal load_op                : std_logic;
  signal store_op               : std_logic;
  --signal sw_mip                 : std_logic;
  signal signed_op              : std_logic;

  -- hardware context id at fetch, and propagated hardware context ids
  signal harc_LS_WB             : natural range THREAD_POOL_SIZE-1 downto 0;
  signal harc_IE_WB             : natural range THREAD_POOL_SIZE-1 downto 0;
  signal harc_WB                : natural range THREAD_POOL_SIZE-1 downto 0;

  signal CORE_STATE_FETCH       : std_logic_vector(THREAD_POOL_BASELINE downto 0);
  signal CORE_STATE_ID          : std_logic_vector(THREAD_POOL_BASELINE downto 0);

  -- DSP Unit Signals
  signal dsp_sc_data_read       : array_3d(accl_range)(1 downto 0)(SIMD_Width-1 downto 0);
  signal dsp_sc_read_addr       : array_3d(accl_range)(1 downto 0)(Addr_Width-1 downto 0);
  signal dsp_to_sc              : array_3d(accl_range)(SPM_NUM-1 downto 0)(1 downto 0);
  signal dsp_sc_write_addr      : array_2d(accl_range)(Addr_Width-1 downto 0);
  signal dsp_sc_data_write_wire : array_2d(accl_range)(SIMD_Width - 1 downto 0);
  signal dsp_sci_req            : array_2d(accl_range)(SPM_NUM-1 downto 0);
  signal dsp_sci_we             : array_2d(accl_range)(SPM_NUM-1 downto 0);

  -- instruction operands
  signal CSR_ADDR_IE        : std_logic_vector(11 downto 0);  -- unused
  signal RS1_Addr_IE        : std_logic_vector(4 downto 0);   -- unused
  signal RS2_Addr_IE        : std_logic_vector(4 downto 0);   -- unused
  signal return_address     : array_2d(THREAD_POOL_SIZE-1 downto 0)(31 downto 0);

  signal ls_parallel_exec   : std_logic;
  signal dsp_parallel_exec  : std_logic;
  
  signal dsp_to_jump_wire   : std_logic;
  signal dsp_to_jump        : std_logic_vector(harc_range);

  signal instr_rvalid_ID_int : std_logic;

  constant buf_size                  : integer := 10;

  signal harc_EXEC_lat               : harc_range;
  signal core_busy_IE_lat            : std_logic;
  signal rs1_chk_en                  : std_logic;
  signal rs2_chk_en                  : std_logic;
  signal rd_read_only_chk_en         : std_logic;
  signal IE_instr                    : std_logic;
  signal LSU_instr                   : std_logic;
  signal DSP_instr                   : std_logic;
  signal EXEC_instr                  : std_logic;
  signal EXEC_Instr_lat              : std_logic;
  signal rs1_valid                   : std_logic;
  signal rs2_valid                   : std_logic;
  signal rd_read_only_valid          : std_logic;
  signal rd_valid                    : std_logic;
  signal rs1_valid_buf               : std_logic_vector(buf_size-1 downto 0);
  signal rs2_valid_buf               : std_logic_vector(buf_size-1 downto 0);
  signal rd_read_only_valid_buf      : std_logic_vector(buf_size-1 downto 0);
  signal rd_valid_buf                : std_logic_vector(buf_size-1 downto 0);
  signal Instr_word_buf              : array_2d(buf_size-1 downto 0)(31 downto 0);
  signal pc_buf                      : array_2d(buf_size-1 downto 0)(31 downto 0);
  signal rs1_valid_buf_wire          : std_logic_vector(buf_size-1 downto 0);
  signal rs2_valid_buf_wire          : std_logic_vector(buf_size-1 downto 0);
  signal rd_valid_buf_wire           : std_logic_vector(buf_size-1 downto 0);
  signal rd_read_only_valid_buf_wire : std_logic_vector(buf_size-1 downto 0);
  signal Instr_word_buf_wire         : array_2d(buf_size-1 downto 0)(31 downto 0);
  signal pc_buf_wire                 : array_2d(buf_size-1 downto 0)(31 downto 0);
  signal buf_wr_ptr                  : integer := 0;
  signal buf_wr_ptr_lat              : integer := 0;

  signal zero_rs1                    : std_logic;
  signal zero_rs2                    : std_logic;
  signal pass_BEQ                    : std_logic;
  signal pass_BNE                    : std_logic;
  signal pass_BLT                    : std_logic;
  signal pass_BLTU                   : std_logic;
  signal pass_BGE                    : std_logic;
  signal pass_BGEU                   : std_logic;

  signal IE_flush_hart_ID            : std_logic_vector(harc_range);
  signal LSU_flush_hart_ID           : std_logic_vector(harc_range);
  signal flush_hart_ID               : std_logic_vector(harc_range);
  signal IE_flush_hart_FETCH         : std_logic_vector(harc_range);
  signal LSU_flush_hart_FETCH        : std_logic_vector(harc_range);
  signal flush_hart_FETCH            : std_logic_vector(harc_range);

  signal branch_taken                : std_logic;
  signal branch_predict_taken_ID     : std_logic; 
  signal branch_predict_taken_IE     : std_logic; 

  signal halt_update_FETCH           : std_logic_vector(harc_range);
  signal halt_update_IE              : std_logic_vector(harc_range);

  signal RAW_wire                    : array_2d_int(buf_size-1 downto 0);
  signal RAW                         : array_2d_int(buf_size-1 downto 0);
  signal tracer_result               : std_logic_vector(31 downto 0);
  signal tracer_mul_result           : std_logic_vector(63 downto 0);   

  function rs1 (signal instr : in std_logic_vector(31 downto 0)) return integer is
  begin
    return to_integer(unsigned(instr(15+(RF_CEIL-1) downto 15)));
  end;

  function rs2 (signal instr : in std_logic_vector(31 downto 0)) return integer is
  begin
    return to_integer(unsigned(instr(20+(RF_CEIL-1) downto 20)));
  end;

  function rd (signal instr : in std_logic_vector(31 downto 0)) return integer is
  begin
    return to_integer(unsigned(instr(7+(RF_CEIL-1) downto 7)));
  end;

  component IF_STAGE is
  generic(
    THREAD_POOL_SIZE           : natural;
    morph_en                   : natural;
    fetch_stage_en             : natural;
    branch_predict_en          : natural;
    btb_en                     : natural;
    btb_len                    : natural;
    TPS_CEIL                   : natural;
    RF_CEIL                    : natural
    );
  port (
    pc_IF                      : in  std_logic_vector(31 downto 0);
    busy_ID                    : in  std_logic;  
    instr_rvalid_i             : in  std_logic;
    harc_sleep_wire            : in  std_logic_vector(harc_range);
    harc_sleep                 : in  std_logic_vector(harc_range);
    CORE_STATE                 : in  std_logic_vector(THREAD_POOL_BASELINE downto 0);
    CORE_STATE_FETCH           : out std_logic_vector(THREAD_POOL_BASELINE downto 0);
    block_input_inst_wire      : in  std_logic;
    block_input_inst           : in  std_logic;
    served_irq                 : in  std_logic_vector(harc_range);
    flush_decode               : in  std_logic_vector(harc_range);
    harc_IF                    : in  natural range THREAD_POOL_SIZE-1 downto 0;
    harc_FETCH                 : out natural range THREAD_POOL_SIZE-1 downto 0;
    harc_ID                    : out natural range THREAD_POOL_SIZE-1 downto 0;
    pc_ID                      : out std_logic_vector(31 downto 0);  -- pc_ID is PC entering ID stage
    instr_rvalid_ID            : out std_logic;
    instr_word_ID              : out std_logic_vector(31 downto 0);
    instr_word_FETCH           : out std_logic_vector(31 downto 0);
    rs1_valid_ID               : out std_logic;
    rs2_valid_ID               : out std_logic;
    rd_valid_ID                : out std_logic;
    rd_read_valid_ID           : out std_logic;
    instr_rvalid_IE            : in  std_logic;
    pc_IE                      : in  std_logic_vector(31 downto 0);
    return_address             : in  array_2d(THREAD_POOL_SIZE-1 downto 0)(31 downto 0);
    -- branch related signals
    absolute_jump              : in  std_logic_vector(harc_range);
    branch_instr               : in  std_logic;
    flush_hart_FETCH           : in  std_logic_vector(harc_range);
    branch_taken               : in  std_logic;
    decoded_branching_instr    : out std_logic_vector(BRANCHING_INSTR_SET_SIZE-1 downto 0);
    branch_predict_taken_ID    : out std_logic;
    branch_FETCH               : out std_logic;
    jump_FETCH                 : out std_logic;
    jalr_FETCH                 : out std_logic;
    halt_update_FETCH          : out std_logic_vector(harc_range);
    branch_addr_FETCH          : out std_logic_vector(31 downto 0);
    jump_addr_FETCH            : out std_logic_vector(31 downto 0);
    jalr_addr_FETCH            : out std_logic_vector(31 downto 0);
    -- clock, reset active low
    clk_i                      : in  std_logic;
    rst_ni                     : in  std_logic;
    -- program memory interface
    instr_req_o                : out std_logic;
    instr_gnt_i                : in  std_logic;
    instr_rdata_i              : in  std_logic_vector(31 downto 0);
    core_enable_i              : in  std_logic
  );
  end component; --------------------------------------------------

  component ID_STAGE is
  generic(
    THREAD_POOL_SIZE           : natural;
    RV32M                      : natural;
    morph_en                   : natural;
    fetch_stage_en             : natural;
    branch_predict_en          : natural;
    btb_en                     : natural;
    superscalar_exec_en        : natural;
    accl_en                    : natural;
    replicate_accl_en          : natural;
    SPM_NUM                    : natural;  
    Addr_Width                 : natural;
    SPM_STRT_ADDR              : std_logic_vector(31 downto 0);
    ACCL_NUM                   : natural;
    TPS_CEIL                   : natural;
    RF_CEIL                    : natural;
    RF_SIZE                    : natural;
    SPM_ADDR_WID               : natural
    );
  port (
  -- Branch Control Signals
    comparator_en              : out std_logic;
    ls_instr_req               : out std_logic;
    ie_instr_req               : out std_logic;
    dsp_instr_req              : out std_logic_vector(ACCL_NUM-1 downto 0);
    decoded_branching_instr    : in  std_logic_vector(BRANCHING_INSTR_SET_SIZE-1 downto 0);
    decoded_instruction_IE     : out std_logic_vector(EXEC_UNIT_INSTR_SET_SIZE-1 downto 0);
    decoded_instruction_LS     : out std_logic_vector(LS_UNIT_INSTR_SET_SIZE-1 downto 0);
    decoded_instruction_DSP    : out std_logic_vector(DSP_UNIT_INSTR_SET_SIZE-1 downto 0);
    data_be_ID                 : out std_logic_vector(3 downto 0);
    data_width_ID              : out std_logic_vector(1 downto 0);
    amo_store                  : in  std_logic;
    amo_load                   : out std_logic;
    amo_load_skip              : out std_logic;
    load_op                    : out std_logic;
    store_op                   : out std_logic;
    instr_word_IE              : out std_logic_vector(31 downto 0);
    MSTATUS                    : in  array_2D(THREAD_POOL_SIZE-1 downto 0)(1 downto 0);
    harc_FETCH                 : in  natural range THREAD_POOL_SIZE-1 downto 0;
    harc_ID                    : in  natural range THREAD_POOL_SIZE-1 downto 0;
    pc_ID                      : in  std_logic_vector(31 downto 0);  -- pc_ID is PC entering ID stage
    rs1_valid_ID               : in  std_logic;
    rs2_valid_ID               : in  std_logic;
    rd_valid_ID                : in  std_logic;
    rd_read_valid_ID           : in  std_logic;
    data_dependency            : out std_logic;
    bypass_rs1                 : out std_logic;
    bypass_rs2                 : out std_logic;
    bypass_rd_read             : out std_logic;
    jalr_stall                 : out std_logic;
    branch_stall               : out std_logic;
    core_busy_IE               : in  std_logic;
    core_busy_LS               : in  std_logic;
    busy_LS                    : in  std_logic;
    busy_DSP                   : in  std_logic_vector(ACCL_NUM-1 downto 0);
    busy_ID                    : out std_logic;
    ls_parallel_exec           : out std_logic;
    dsp_parallel_exec          : out std_logic;
    dsp_to_jump_wire           : out std_logic;
    dsp_to_jump                : out std_logic_vector(harc_range);
    pc_IE                      : out std_logic_vector(31 downto 0);  -- pc_IE is pc entering stage IE ***
    WB_EN_next_ID              : out std_logic;
    instr_rvalid_ID            : in  std_logic; 
    instr_rvalid_IE            : out std_logic;  -- validity bit at IE input
    instr_rvalid_ID_int        : out std_logic;
    halt_IE                    : out std_logic;
    halt_LSU                   : out std_logic;
    instr_word_ID              : in  std_logic_vector(31 downto 0);
    instr_word_FETCH           : in  std_logic_vector(31 downto 0);
    branch_instr               : in  std_logic;
    absolute_jump              : in  std_logic_vector(harc_range);
    harc_LS_WB                 : in  natural range THREAD_POOL_SIZE-1 downto 0;
    harc_IE_WB                 : in  natural range THREAD_POOL_SIZE-1 downto 0;
    LS_WB_EN                   : in  std_logic;
    IE_WB_EN                   : in  std_logic;
    MUL_WB_EN                  : in  std_logic;
    LS_WB_EN_wire              : in  std_logic;
    IE_WB_EN_wire              : in  std_logic;
    MUL_WB_EN_wire             : in  std_logic;
    instr_word_LS_WB           : in  std_logic_vector(31 downto 0);
    instr_word_IE_WB           : in  std_logic_vector(31 downto 0);
    spm_rs1                    : out std_logic;
    spm_rs2                    : out std_logic;
    harc_sleep_wire            : in  std_logic_vector(harc_range);
    harc_sleep                 : in  std_logic_vector(harc_range);
    CORE_STATE                 : in  std_logic_vector(THREAD_POOL_BASELINE downto 0);
    CORE_STATE_FETCH           : in  std_logic_vector(THREAD_POOL_BASELINE downto 0);
    CORE_STATE_ID              : out std_logic_vector(THREAD_POOL_BASELINE downto 0);
    set_except_condition       : in  std_logic;
    served_irq                 : in  std_logic_vector(harc_range);
    flush_decode               : out std_logic_vector(harc_range);
    jalr_flush                 : out std_logic_vector(harc_range);
    --sw_mip                     : out std_logic;
    signed_op                  : out std_logic;
    harc_EXEC                  : out natural range THREAD_POOL_SIZE-1 downto 0;
    harc_WB                    : in  natural range THREAD_POOL_SIZE-1 downto 0;
    vec_read_rs1_ID            : out std_logic;
    vec_read_rs2_ID            : out std_logic;
    vec_write_rd_ID            : out std_logic;
    vec_width_ID               : out std_logic_vector(1 downto 0);
    PC_offset_ID               : out std_logic_vector(31 downto 0);
    set_branch_condition_ID    : out std_logic;
    zero_rd                    : out std_logic;
    flush_hart_ID              : in  std_logic_vector(THREAD_POOL_SIZE-1 downto 0);
    branch_predict_taken_ID    : in  std_logic; 
    branch_predict_taken_IE    : out std_logic; 

    -- clock, reset active low
    clk_i                      : in  std_logic;
    rst_ni                     : in  std_logic
    );
  end component; ------------------------------------------

  component Load_Store_Unit is
  generic(
    THREAD_POOL_SIZE           : natural;
    fetch_stage_en             : natural;
    accl_en                    : natural;
    replicate_accl_en          : natural;
    SIMD                       : natural;
    SPM_NUM                    : natural;  
    Addr_Width                 : natural;
    Data_Width                 : natural;
    SIMD_BITS                  : natural;
    ACCL_NUM                   : natural;
    SPM_ADDR_WID               : natural
    );
  port (
    -- clock, and reset active low
    clk_i, rst_ni              : in std_logic;
    -- Program Counter Signals
    irq_pending                : in std_logic_vector(harc_range);
    -- ID_Stage Signals
    RS1_Data_IE                : in  std_logic_vector(31 downto 0);
    RS2_Data_IE                : in  std_logic_vector(31 downto 0);
    RD_Data_IE                 : in  std_logic_vector(31 downto 0);
    instr_word_IE              : in  std_logic_vector(31 downto 0);
    pc_IE                      : in  std_logic_vector(31 downto 0);
    decoded_instruction_LS     : in  std_logic_vector(LS_UNIT_INSTR_SET_SIZE-1 downto 0);
    data_be_ID                 : in  std_logic_vector(3 downto 0);
    data_width_ID              : in  std_logic_vector(1 downto 0);
    harc_EXEC                  : in  natural range THREAD_POOL_SIZE-1 downto 0;
    LS_instr_req               : in  std_logic;
    load_op                    : in  std_logic;
    store_op                   : in  std_logic;
    --sw_mip                     : in  std_logic;
    core_busy_LS               : out std_logic;
    busy_LS                    : out std_logic;
    -- Processing Pipeline Signals
    rs1_to_sc                  : in  std_logic_vector(SPM_ADDR_WID-1 downto 0);
    rs2_to_sc                  : in  std_logic_vector(SPM_ADDR_WID-1 downto 0);
    rd_to_sc                   : in  std_logic_vector(SPM_ADDR_WID-1 downto 0);
    halt_LSU                   : in  std_logic;
    data_addr_internal         : out std_logic_vector(31 downto 0);
    ls_except_data             : out std_logic_vector(31 downto 0);
    ls_except_condition        : out std_logic;
    ls_taken_branch            : out std_logic;
    LSU_flush_hart_FETCH       : out std_logic_vector(THREAD_POOL_SIZE-1 downto 0);
    LSU_flush_hart_ID          : out std_logic_vector(THREAD_POOL_SIZE-1 downto 0);
    amo_load                   : in  std_logic;
    amo_load_skip              : in  std_logic;
    amo_store                  : out std_logic;
    -- CSR Signals
    misaligned_err             : out std_logic;
    -- Scratchpad Interface Signals
    ls_data_gnt_i              : in  std_logic_vector(SPM_NUM-1 downto 0);
    ls_sci_wr_gnt              : in  std_logic;
    ls_sc_data_read_wire       : in  std_logic_vector(Data_Width-1 downto 0);
    state_LS                   : out fsm_LS_states;
    harc_LS_wire               : out integer range ACCL_NUM-1 downto 0;
    sc_word_count_wire         : out integer;
    spm_bcast                  : out std_logic;
    kmemld_inflight            : out std_logic_vector(SPM_NUM-1 downto 0);
    kmemstr_inflight           : out std_logic_vector(SPM_NUM-1 downto 0);
    ls_sci_req                 : out std_logic_vector(SPM_NUM-1 downto 0);
    ls_sci_we                  : out std_logic_vector(SPM_NUM-1 downto 0);
    ls_sc_read_addr            : out std_logic_vector(Addr_Width-(SIMD_BITS+3) downto 0);
    ls_sc_write_addr           : out std_logic_vector(Addr_Width-(SIMD_BITS+3)downto 0);
    ls_sc_data_write_wire      : out std_logic_vector(Data_Width-1 downto 0);
    -- WB_Stage Signals
    LS_WB_EN                   : out std_logic;
    LS_WB_EN_wire              : out std_logic;
    harc_LS_WB                 : out harc_range;
    instr_word_LS_WB           : out std_logic_vector(31 downto 0);
    LS_WB                      : out std_logic_vector(31 downto 0);
    -- Data memory interface
    data_req_o                 : out std_logic;
    data_gnt_i                 : in  std_logic;
    data_rvalid_i              : in  std_logic;
    data_we_o                  : out std_logic;
    data_be_o                  : out std_logic_vector(3 downto 0);
    data_addr_o                : out std_logic_vector(31 downto 0);
    data_wdata_o               : out std_logic_vector(31 downto 0);
    data_rdata_i               : in  std_logic_vector(31 downto 0);
    data_err_i                 : in  std_logic
  );
  end component;  ------------------------------------------  

  component IE_STAGE is
  generic(
    THREAD_POOL_SIZE_GLOBAL   : natural;
    THREAD_POOL_SIZE          : natural;
    context_switch            : natural;
    morph_en                  : natural;
    fetch_stage_en            : natural; 
    branch_predict_en         : natural;
    RV32M                     : natural;
    TPS_CEIL                  : natural;
    RF_CEIL                   : natural
  );
  port (
     -- clock, and reset active low
    clk_i, rst_ni             : in  std_logic;
    instr_gnt_i               : in  std_logic;
    irq_i                     : in  std_logic;
    source_hartid_o           : out natural range THREAD_POOL_SIZE_GLOBAL-1 downto 0;
    sw_irq                    : out std_logic_vector(THREAD_POOL_SIZE_GLOBAL-1 downto 0);
    sw_irq_served_i           : in  std_logic_vector(THREAD_POOL_SIZE-1 downto 0);
    sw_irq_served_o           : out std_logic_vector(THREAD_POOL_SIZE_GLOBAL-1 downto 0);
    sw_irq_pending            : out std_logic_vector(THREAD_POOL_SIZE_GLOBAL-1 downto 0);
    RS1_Data_IE               : in  std_logic_vector(31 downto 0);
    RS2_Data_IE               : in  std_logic_vector(31 downto 0);
    irq_pending               : in  std_logic_vector(harc_range);
    fetch_enable_i            : in  std_logic;
    csr_instr_done            : in  std_logic;
    csr_access_denied_o       : in  std_logic;
    csr_rdata_o               : in  std_logic_vector(31 downto 0);
    pc_IE                     : in  std_logic_vector(31 downto 0);
    instr_word_IE             : in  std_logic_vector(31 downto 0);
    data_addr_internal_IE     : in  std_logic_vector(31 downto 0);
    comparator_en             : in  std_logic;
    --sw_mip                 : in std_logic;
    zero_rd                   : in  std_logic;
    signed_op                 : in  std_logic;
    zero_rs1                  : in  std_logic;
    zero_rs2                  : in  std_logic;
    pass_BEQ                  : in  std_logic;
    pass_BNE                  : in  std_logic;
    pass_BLT                  : in  std_logic;
    pass_BLTU                 : in  std_logic;
    pass_BGE                  : in  std_logic;
    pass_BGEU                 : in  std_logic;
    ie_instr_req              : in  std_logic;
    MHARTID                   : in  array_2d(harc_range)(9 downto 0);
    MSTATUS                   : in  array_2d(harc_range)(1 downto 0);
    MPIP                      : in  array_2d(harc_range)(THREAD_POOL_SIZE_GLOBAL-1 downto 0);
    harc_EXEC                 : in  natural range THREAD_POOL_SIZE-1 downto 0;
    instr_rvalid_IE           : in  std_logic;  -- validity bit at IE input
    WB_EN_next_ID             : in  std_logic;
    taken_branch              : in  std_logic;
    halt_IE                   : in  std_logic;
    decoded_instruction_IE    : in  std_logic_vector(EXEC_UNIT_INSTR_SET_SIZE-1 downto 0);
    harc_sleep_wire           : in  std_logic_vector(harc_range);
    harc_sleep                : in  std_logic_vector(harc_range);
    wfi_hart_wire             : in  std_logic_vector(harc_range);
    core_enable_i             : in  std_logic;
    CORE_STATE                : in  std_logic_vector(THREAD_POOL_BASELINE downto 0);
    CORE_STATE_ID             : in  std_logic_vector(THREAD_POOL_BASELINE downto 0);
    IMT_ACTIVE_HARTS          : in  natural;
    HET_CLUSTER_S1_CORE       : in  std_logic;
    csr_addr_i                : out std_logic_vector(11 downto 0);
    ie_except_data            : out std_logic_vector(31 downto 0);
    ie_csr_wdata_i            : out std_logic_vector(31 downto 0);
    csr_op_i                  : out std_logic_vector(2 downto 0);
    harc_to_csr               : out natural range THREAD_POOL_SIZE_GLOBAL-1 downto 0;
    csr_instr_req             : out std_logic;
    core_busy_IE              : out std_logic;
    jump_instr                : out std_logic;
    jump_instr_lat            : out std_logic;
    WFI_Instr                 : out std_logic;
    sleep_state               : out std_logic;
    set_branch_condition      : out std_logic;
    IE_except_condition       : out std_logic;
    set_mret_condition        : out std_logic;
    set_wfi_condition         : out std_logic;
    ie_taken_branch           : out std_logic;
    branch_instr              : out std_logic;
    branch_instr_lat          : out std_logic;
    PC_offset                 : out std_logic_vector(31 downto 0);
    absolute_address          : out std_logic_vector(31 downto 0);
    served_irq                : out std_logic_vector(harc_range);
    served_pending_irq        : out std_logic_vector(harc_range);
    ebreak_instr              : out std_logic;
    absolute_jump             : out std_logic_vector(harc_range);
    instr_word_IE_WB          : out std_logic_vector (31 downto 0);
    IE_WB_EN                  : out std_logic;
    IE_WB_EN_wire             : out std_logic;
    IE_WB                     : out std_logic_vector(31 downto 0);
    MUL_WB_EN                 : out std_logic;
    MUL_WB_EN_wire            : out std_logic;
    MUL_WB                    : out std_logic_vector(31 downto 0);
    harc_IE_WB                : out natural range THREAD_POOL_SIZE-1 downto 0;
    pc_WB                     : out std_logic_vector(31 downto 0);
    state_IE                  : out fsm_IE_states;
    dsp_to_jump               : in  std_logic_vector(THREAD_POOL_SIZE-1 downto 0);
    jalr_flush                : in  std_logic_vector(THREAD_POOL_SIZE-1 downto 0);
    IE_flush_hart_FETCH       : out std_logic_vector(THREAD_POOL_SIZE-1 downto 0);
    IE_flush_hart_ID          : out std_logic_vector(THREAD_POOL_SIZE-1 downto 0);
    halt_update_IE            : out std_logic_vector(THREAD_POOL_SIZE-1 downto 0);
    branch_taken              : out std_logic;
    branch_predict_taken_IE   : in  std_logic
  );
  end component;  ------------------------------------------

  component REGISTERFILE is
  generic(
    THREAD_POOL_SIZE           : natural;
    LUTRAM_RF                  : natural;
    morph_en                   : natural;
    fetch_stage_en             : natural;
    accl_en                    : natural;
    SPM_NUM                    : natural;
    Addr_Width                 : natural;
    SPM_STRT_ADDR              : std_logic_vector(31 downto 0);
    RF_SIZE                    : natural;
    RF_CEIL                    : natural;
    SPM_ADDR_WID               : natural
  );
  port (
    -- clock, reset active low
    clk_i                      : in  std_logic;
    rst_ni                     : in  std_logic;
    -- Branch Control Signals
    harc_ID                    : in  natural range THREAD_POOL_SIZE-1 downto 0;
    pc_ID                      : in  std_logic_vector(31 downto 0);  -- pc_ID is PC entering ID stage
    data_dependency            : in  std_logic;
    bypass_rs1                 : in  std_logic;
    bypass_rs2                 : in  std_logic;
    bypass_rd_read             : in  std_logic;
    jalr_stall                 : in  std_logic;
    branch_stall               : in  std_logic;
    core_busy_IE               : in  std_logic;
    core_busy_LS               : in  std_logic;
    ls_parallel_exec           : in  std_logic;
    dsp_parallel_exec          : in  std_logic;
    dsp_to_jump_wire           : in  std_logic;
    instr_rvalid_ID            : in  std_logic;
    instr_rvalid_ID_int        : in  std_logic;
    instr_word_ID              : in  std_logic_vector(31 downto 0);
    LS_WB_EN                   : in  std_logic;
    IE_WB_EN                   : in  std_logic;
    MUL_WB_EN                  : in  std_logic;
    IE_WB                      : in  std_logic_vector(31 downto 0);
    MUL_WB                     : in  std_logic_vector(31 downto 0);
    LS_WB                      : in  std_logic_vector(31 downto 0);
    instr_word_LS_WB           : in  std_logic_vector(31 downto 0);
    instr_word_IE_WB           : in  std_logic_vector(31 downto 0);
    harc_LS_WB                 : in  natural range THREAD_POOL_SIZE-1 downto 0;
    harc_IE_WB                 : in  natural range THREAD_POOL_SIZE-1 downto 0;
    zero_rs1                   : out std_logic;
    zero_rs2                   : out std_logic;
    pass_BEQ                   : out std_logic;
    pass_BNE                   : out std_logic;
    pass_BLT                   : out std_logic;
    pass_BLTU                  : out std_logic;
    pass_BGE                   : out std_logic;
    pass_BGEU                  : out std_logic;
    RS1_Data_IE                : out std_logic_vector(31 downto 0);
    RS2_Data_IE                : out std_logic_vector(31 downto 0);
    RD_Data_IE                 : out std_logic_vector(31 downto 0);
    return_address             : out array_2d(THREAD_POOL_SIZE-1 downto 0)(31 downto 0);
    rs1_to_sc                  : out std_logic_vector(SPM_ADDR_WID-1 downto 0);
    rs2_to_sc                  : out std_logic_vector(SPM_ADDR_WID-1 downto 0);
    rd_to_sc                   : out std_logic_vector(SPM_ADDR_WID-1 downto 0);
    data_addr_internal_IE      : out std_logic_vector(31 downto 0);
    regfile                    : out array_3d(harc_range)(RF_SIZE-1 downto 0)(31 downto 0)
  );
  end component;

--------------------------------------------------------------------------------------------------
----------------------- ARCHITECTURE BEGIN -------------------------------------------------------
begin

  -- Klessydra T13 (4 stages) pipeline implementation -----------------------

  -- check for microarchitecture configuration limit, up to 16 thread support.
  assert THREAD_POOL_SIZE < 2**THREAD_ID_SIZE
    report "Threading configuration not supported"
  severity error;

  set_except_condition <= '1' when (IE_except_condition = '1' or LS_except_condition = '1' or DSP_except_condition /= (accl_range => '0')) else '0';

  flush_hart_ID    <= IE_flush_hart_ID    or LSU_flush_hart_ID;
  flush_hart_FETCH <= IE_flush_hart_FETCH or LSU_flush_hart_FETCH;

  taken_branch <= '1' when (ie_taken_branch = '1' or ls_taken_branch = '1' or unsigned(dsp_taken_branch) /= 0) else '0';
          
  csr_wdata_i <= ie_csr_wdata_i;

  harc_WB <= harc_LS_WB when LS_WB_EN = '1' else harc_IE_WB;
------------------------------------------------------------------------------------------------------------------------------------
-- Core_busy_o
------------------------------------------------------------------------------------------------------------------------------------

  core_busy_o <= '1' when (instr_rvalid_i or instr_rvalid_ID or instr_rvalid_IE) = '1' and rst_ni = '1' else '0';
------------------------------------------------------------------------------------------------------------------------------------

  halt_update <= halt_update_FETCH or halt_update_IE;

------------------------------------------------------------------------------------------------------------------------------------
-- Mapping of the pipeline stages
------------------------------------------------------------------------------------------------------------------------------------

  ----------------------------------------------------------------
  --  ██████╗ ██╗██████╗ ███████╗██╗     ██╗███╗   ██╗███████╗  --
  --  ██╔══██╗██║██╔══██╗██╔════╝██║     ██║████╗  ██║██╔════╝  --
  --  ██████╔╝██║██████╔╝█████╗  ██║     ██║██╔██╗ ██║█████╗    --
  --  ██╔═══╝ ██║██╔═══╝ ██╔══╝  ██║     ██║██║╚██╗██║██╔══╝    --
  --  ██║     ██║██║     ███████╗███████╗██║██║ ╚████║███████╗  --
  --  ╚═╝     ╚═╝╚═╝     ╚══════╝╚══════╝╚═╝╚═╝  ╚═══╝╚══════╝  --
  ----------------------------------------------------------------                                                      


  FETCH : IF_STAGE
  generic map(
    THREAD_POOL_SIZE           => THREAD_POOL_SIZE,
    morph_en                   => morph_en,
    fetch_stage_en             => fetch_stage_en,
    branch_predict_en          => branch_predict_en,
    btb_en                     => btb_en,
    btb_len                    => btb_len,
    TPS_CEIL                   => TPS_CEIL,
    RF_CEIL                    => RF_CEIL
    )
  port map(
    pc_IF                      => pc_IF,
    harc_IF                    => harc_IF,
    busy_ID                    => busy_ID,   
    instr_rvalid_i             => instr_rvalid_i,
    harc_sleep_wire            => harc_sleep_wire,
    harc_sleep                 => harc_sleep,
    CORE_STATE                 => CORE_STATE,
    CORE_STATE_FETCH           => CORE_STATE_FETCH,
    block_input_inst_wire      => block_input_inst_wire,
    block_input_inst           => block_input_inst,
    served_irq                 => served_irq,
    flush_decode               => flush_decode,
    harc_FETCH                 => harc_FETCH,
    harc_ID                    => harc_ID,
    pc_ID                      => pc_ID,
    instr_rvalid_ID            => instr_rvalid_ID,
    instr_word_ID              => instr_word_ID,
    instr_word_FETCH           => instr_word_FETCH,
    rs1_valid_ID               => rs1_valid_ID, 
    rs2_valid_ID               => rs2_valid_ID, 
    rd_valid_ID                => rd_valid_ID, 
    rd_read_valid_ID           => rd_read_valid_ID,
    instr_rvalid_IE            => instr_rvalid_IE,
    pc_IE                      => pc_IE,
    return_address             => return_address,
    absolute_jump              => absolute_jump, 
    branch_instr               => branch_instr, 
    flush_hart_FETCH           => flush_hart_FETCH, 
    branch_taken               => branch_taken,
    decoded_branching_instr    => decoded_branching_instr,
    branch_predict_taken_ID    => branch_predict_taken_ID,
    branch_FETCH               => branch_FETCH,
    jump_FETCH                 => jump_FETCH,
    jalr_FETCH                 => jalr_FETCH,
    halt_update_FETCH          => halt_update_FETCH,
    branch_addr_FETCH          => branch_addr_FETCH,
    jump_addr_FETCH            => jump_addr_FETCH,
    jalr_addr_FETCH            => jalr_addr_FETCH,
    clk_i                      => clk_i,
    rst_ni                     => rst_ni,
    instr_req_o                => instr_req_o,      
    instr_gnt_i                => instr_gnt_i,
    instr_rdata_i              => instr_rdata_i,
    core_enable_i              => core_enable_i
    );

  DECODE : ID_STAGE
  generic map(
    THREAD_POOL_SIZE           => THREAD_POOL_SIZE,
    RV32M                      => RV32M,
    morph_en                   => morph_en,
    fetch_stage_en             => fetch_stage_en,
    branch_predict_en          => branch_predict_en,
    btb_en                     => btb_en,
    superscalar_exec_en        => superscalar_exec_en,
    accl_en                    => accl_en,
    replicate_accl_en          => replicate_accl_en,
    SPM_NUM                    => SPM_NUM,  
    Addr_Width                 => Addr_Width,
    SPM_STRT_ADDR              => SPM_STRT_ADDR,
    ACCL_NUM                   => ACCL_NUM,
    TPS_CEIL                   => TPS_CEIL,
    RF_CEIL                    => RF_CEIL,
    RF_SIZE                    => RF_SIZE,
    SPM_ADDR_WID               => SPM_ADDR_WID
    )
  port map(            
    comparator_en              => comparator_en,
    ie_instr_req               => ie_instr_req,        
    ls_instr_req               => ls_instr_req,        
    dsp_instr_req              => dsp_instr_req,
    decoded_branching_instr    => decoded_branching_instr,
    decoded_instruction_IE     => decoded_instruction_IE, 
    decoded_instruction_LS     => decoded_instruction_LS, 
    decoded_instruction_DSP    => decoded_instruction_DSP,
    data_be_ID                 => data_be_ID,
    data_width_ID              => data_width_ID,
    amo_store                  => amo_store,
    amo_load                   => amo_load,
    amo_load_skip              => amo_load_skip,
    load_op                    => load_op,
    store_op                   => store_op,      
    instr_word_IE              => instr_word_IE,
    MSTATUS                    => MSTATUS,
    harc_ID                    => harc_ID,
    pc_ID                      => pc_ID,
    rs1_valid_ID               => rs1_valid_ID,
    rs2_valid_ID               => rs2_valid_ID,
    rd_valid_ID                => rd_valid_ID,
    rd_read_valid_ID           => rd_read_valid_ID,
    data_dependency            => data_dependency,
    bypass_rs1                 => bypass_rs1,
    bypass_rs2                 => bypass_rs2,
    bypass_rd_read             => bypass_rd_read,
    jalr_stall                 => jalr_stall,
    branch_stall               => branch_stall,
    core_busy_IE               => core_busy_IE,
    core_busy_LS               => core_busy_LS,
    busy_LS                    => busy_LS,
    busy_DSP                   => busy_DSP,
    busy_ID                    => busy_ID,
    ls_parallel_exec           => ls_parallel_exec,
    dsp_parallel_exec          => dsp_parallel_exec,
    dsp_to_jump_wire           => dsp_to_jump_wire,
    dsp_to_jump                => dsp_to_jump,
    pc_IE                      => pc_IE,
    WB_EN_next_ID              => WB_EN_next_ID,
    instr_rvalid_ID            => instr_rvalid_ID,
    instr_rvalid_IE            => instr_rvalid_IE,
    instr_rvalid_ID_int        => instr_rvalid_ID_int,
    halt_IE                    => halt_IE,
    halt_LSU                   => halt_LSU,
    instr_word_ID              => instr_word_ID,
    instr_word_FETCH           => instr_word_FETCH,
    harc_FETCH                 => harc_FETCH,              
    branch_instr               => branch_instr,
    absolute_jump              => absolute_jump,
    harc_LS_WB                 => harc_LS_WB,
    harc_IE_WB                 => harc_IE_WB,
    LS_WB_EN                   => LS_WB_EN,
    IE_WB_EN                   => IE_WB_EN,
    MUL_WB_EN                  => MUL_WB_EN,
    LS_WB_EN_wire              => LS_WB_EN_wire,
    IE_WB_EN_wire              => IE_WB_EN_wire,
    MUL_WB_EN_wire             => MUL_WB_EN_wire,
    instr_word_LS_WB           => instr_word_LS_WB,
    instr_word_IE_WB           => instr_word_IE_WB,
    spm_rs1                    => spm_rs1,
    spm_rs2                    => spm_rs2,
    flush_hart_ID              => flush_hart_ID,
    branch_predict_taken_ID    => branch_predict_taken_ID,
    branch_predict_taken_IE    => branch_predict_taken_IE,
    harc_sleep_wire            => harc_sleep_wire,
    harc_sleep                 => harc_sleep,
    CORE_STATE                 => CORE_STATE,
    CORE_STATE_FETCH           => CORE_STATE_FETCH,
    CORE_STATE_ID              => CORE_STATE_ID,
    set_except_condition       => set_except_condition,
    served_irq                 => served_irq,
    flush_decode               => flush_decode,
    jalr_flush                 => jalr_flush,
    --sw_mip                     => sw_mip,
    signed_op                  => signed_op,
    harc_EXEC                  => harc_EXEC,
    harc_WB                    => harc_WB,
    vec_read_rs1_ID            => vec_read_rs1_ID,
    vec_read_rs2_ID            => vec_read_rs2_ID,
    vec_write_rd_ID            => vec_write_rd_ID,
    PC_offset_ID               => PC_offset_ID,
    set_branch_condition_ID    => set_branch_condition_ID,
    zero_rd                    => zero_rd,
    clk_i                      => clk_i,
    rst_ni                     => rst_ni                
  );

  LSU : Load_Store_Unit
  generic map(
    THREAD_POOL_SIZE           => THREAD_POOL_SIZE,
    fetch_stage_en             => fetch_stage_en,
    accl_en                    => accl_en,
    replicate_accl_en          => replicate_accl_en,
    SIMD                       => SIMD,
    SPM_NUM                    => SPM_NUM,  
    Addr_Width                 => Addr_Width,
    Data_Width                 => Data_Width,
    SIMD_BITS                  => SIMD_BITS,
    ACCL_NUM                   => ACCL_NUM,
    SPM_ADDR_WID               => SPM_ADDR_WID
    )
  port map(
    clk_i                      => clk_i,
    rst_ni                     => rst_ni,    
    irq_pending                => irq_pending,
    instr_word_IE              => instr_word_IE,                  
    pc_IE                      => pc_IE,   
    RS1_Data_IE                => RS1_Data_IE,           
    RS2_Data_IE                => RS2_Data_IE,           
    RD_Data_IE                 => RD_Data_IE,
    decoded_instruction_LS     => decoded_instruction_LS,
    data_be_ID                 => data_be_ID,
    data_width_ID              => data_width_ID,
    harc_EXEC                  => harc_EXEC,
    LS_instr_req               => LS_instr_req,
    load_op                    => load_op,
    store_op                   => store_op,
    --sw_mip                     => sw_mip,
    core_busy_LS               => core_busy_LS,               
    busy_LS                    => busy_LS,
    rs1_to_sc                  => rs1_to_sc,
    rs2_to_sc                  => rs2_to_sc,
    rd_to_sc                   => rd_to_sc,
    halt_LSU                   => halt_LSU,
    data_addr_internal         => data_addr_internal, 
    ls_except_data             => ls_except_data,  
    ls_except_condition        => ls_except_condition,        
    ls_taken_branch            => ls_taken_branch,
    LSU_flush_hart_FETCH       => LSU_flush_hart_FETCH,
    LSU_flush_hart_ID          => LSU_flush_hart_ID,
    amo_load                   => amo_load,              
    amo_load_skip              => amo_load_skip,         
    amo_store                  => amo_store,    
    misaligned_err             => misaligned_err,
    ls_data_gnt_i              => ls_data_gnt_i,
    ls_sci_wr_gnt              => ls_sci_wr_gnt,
    ls_sc_data_read_wire       => ls_sc_data_read_wire,
    state_LS                   => state_LS,
    harc_LS_wire               => harc_LS_wire,
    sc_word_count_wire         => sc_word_count_wire,
    spm_bcast                  => spm_bcast,
    kmemld_inflight            => kmemld_inflight,
    kmemstr_inflight           => kmemstr_inflight,
    ls_sci_req                 => ls_sci_req,
    ls_sci_we                  => ls_sci_we,
    ls_sc_read_addr            => ls_sc_read_addr,
    ls_sc_write_addr           => ls_sc_write_addr,
    ls_sc_data_write_wire      => ls_sc_data_write_wire,
    LS_WB_EN                   => LS_WB_EN,
    LS_WB_EN_wire              => LS_WB_EN_wire,
    harc_LS_WB                 => harc_LS_WB,
    instr_word_LS_WB           => instr_word_LS_WB,
    LS_WB                      => LS_WB,
    data_req_o                 => data_req_o,   
    data_gnt_i                 => data_gnt_i,            
    data_rvalid_i              => data_rvalid_i,         
    data_we_o                  => data_we_o,             
    data_be_o                  => data_be_o,             
    data_addr_o                => data_addr_o,           
    data_wdata_o               => data_wdata_o,          
    data_rdata_i               => data_rdata_i,          
    data_err_i                 => data_err_i    
  );

  EXECUTE : IE_STAGE
  generic map(
    THREAD_POOL_SIZE_GLOBAL    => THREAD_POOL_SIZE_GLOBAL,
    THREAD_POOL_SIZE           => THREAD_POOL_SIZE,
    context_switch             => context_switch,
    morph_en                   => morph_en,
    fetch_stage_en             => fetch_stage_en,
    branch_predict_en          => branch_predict_en, 
    RV32M                      => RV32M,
    TPS_CEIL                   => TPS_CEIL,
    RF_CEIL                    => RF_CEIL
  )
  port map(
    clk_i                      => clk_i,
    rst_ni                     => rst_ni,
    instr_gnt_i                => instr_gnt_i,
    irq_i                      => irq_i,
    source_hartid_o            => source_hartid_o,
    sw_irq                     => sw_irq,
    sw_irq_served_i            => sw_irq_served_i, 
    sw_irq_served_o            => sw_irq_served_o,
    sw_irq_pending             => sw_irq_pending,
    RS1_Data_IE                => RS1_Data_IE,
    RS2_Data_IE                => RS2_Data_IE,
    irq_pending                => irq_pending,
    fetch_enable_i             => fetch_enable_i,
    csr_instr_done             => csr_instr_done,
    csr_access_denied_o        => csr_access_denied_o,
    csr_rdata_o                => csr_rdata_o,
    pc_IE                      => pc_IE,
    instr_word_IE              => instr_word_IE,
    data_addr_internal_IE      => data_addr_internal_IE,
    comparator_en              => comparator_en,
    --sw_mip                     => sw_mip,
    zero_rd                    => zero_rd,
    signed_op                  => signed_op,
    zero_rs1                   => zero_rs1,
    zero_rs2                   => zero_rs2,
    pass_BEQ                   => pass_BEQ,
    pass_BNE                   => pass_BNE,
    pass_BLT                   => pass_BLT,
    pass_BLTU                  => pass_BLTU,
    pass_BGE                   => pass_BGE,
    pass_BGEU                  => pass_BGEU,
    ie_instr_req               => ie_instr_req,
    MHARTID                    => MHARTID,
    MSTATUS                    => MSTATUS,
    MPIP                       => MPIP,
    harc_EXEC                  => harc_EXEC,
    instr_rvalid_IE            => instr_rvalid_IE,
    WB_EN_next_ID              => WB_EN_next_ID,
    taken_branch               => taken_branch,
    halt_IE                    => halt_IE,
    decoded_instruction_IE     => decoded_instruction_IE,
    harc_sleep_wire            => harc_sleep_wire,
    harc_sleep                 => harc_sleep,
    wfi_hart_wire              => wfi_hart_wire,
    core_enable_i              => core_enable_i,
    CORE_STATE                 => CORE_STATE,
    CORE_STATE_ID              => CORE_STATE_ID,
    IMT_ACTIVE_HARTS           => IMT_ACTIVE_HARTS,
    HET_CLUSTER_S1_CORE        => HET_CLUSTER_S1_CORE,
    csr_addr_i                 => csr_addr_i,
    ie_except_data             => ie_except_data,
    ie_csr_wdata_i             => ie_csr_wdata_i,
    csr_op_i                   => csr_op_i,
    harc_to_csr                => harc_to_csr,
    csr_instr_req              => csr_instr_req,
    core_busy_IE               => core_busy_IE,
    jump_instr                 => jump_instr,
    jump_instr_lat             => jump_instr_lat,
    WFI_Instr                  => WFI_Instr,
    sleep_state                => sleep_state,
    set_branch_condition       => set_branch_condition,
    IE_except_condition        => IE_except_condition,
    set_mret_condition         => set_mret_condition,
    set_wfi_condition          => set_wfi_condition,
    ie_taken_branch            => ie_taken_branch,
    branch_instr               => branch_instr,
    branch_instr_lat           => branch_instr_lat,
    PC_offset                  => PC_offset,
    absolute_address           => absolute_address,
    served_irq                 => served_irq,
    served_pending_irq         => served_pending_irq,
    ebreak_instr               => ebreak_instr,
    absolute_jump              => absolute_jump,
    instr_word_IE_WB           => instr_word_IE_WB,
    IE_WB_EN                   => IE_WB_EN,
    IE_WB_EN_wire              => IE_WB_EN_wire,
    IE_WB                      => IE_WB,
    MUL_WB_EN                  => MUL_WB_EN,
    MUL_WB_EN_wire             => MUL_WB_EN_wire,
    MUL_WB                     => MUL_WB,
    harc_IE_WB                 => harc_IE_WB,
    pc_WB                      => pc_WB,
    state_IE                   => state_IE,
    dsp_to_jump                => dsp_to_jump,
    jalr_flush                 => jalr_flush,
    IE_flush_hart_FETCH        => IE_flush_hart_FETCH,
    IE_flush_hart_ID           => IE_flush_hart_ID,
    halt_update_IE             => halt_update_IE,
    branch_taken               => branch_taken,
    branch_predict_taken_IE    => branch_predict_taken_IE
  );

  RF : REGISTERFILE
  generic map(
    THREAD_POOL_SIZE           => THREAD_POOL_SIZE,
    LUTRAM_RF                  => LUTRAM_RF,
    morph_en                   => morph_en,
    fetch_stage_en             => fetch_stage_en,
    accl_en                    => accl_en,
    SPM_NUM                    => SPM_NUM,
    Addr_Width                 => Addr_Width,
    SPM_STRT_ADDR              => SPM_STRT_ADDR,
    RF_SIZE                    => RF_SIZE,
    RF_CEIL                    => RF_CEIL,
    SPM_ADDR_WID               => SPM_ADDR_WID
  )
  port map(
    -- clock, reset active low
    clk_i                      => clk_i,
    rst_ni                     => rst_ni,
    -- Branch Control Signals
    harc_ID                    => harc_ID,
    pc_ID                      => pc_ID,
    data_dependency            => data_dependency,
    bypass_rs1                 => bypass_rs1,
    bypass_rs2                 => bypass_rs2,        
    bypass_rd_read             => bypass_rd_read,        
    jalr_stall                 => jalr_stall,
    branch_stall               => branch_stall, 
    core_busy_IE               => core_busy_IE,
    core_busy_LS               => core_busy_LS,
    ls_parallel_exec           => ls_parallel_exec,
    dsp_parallel_exec          => dsp_parallel_exec,
    dsp_to_jump_wire           => dsp_to_jump_wire,
    instr_rvalid_ID            => instr_rvalid_ID,
    instr_rvalid_ID_int        => instr_rvalid_ID_int,
    instr_word_ID              => instr_word_ID,
    LS_WB_EN                   => LS_WB_EN,
    IE_WB_EN                   => IE_WB_EN,
    MUL_WB_EN                  => MUL_WB_EN,
    IE_WB                      => IE_WB,
    MUL_WB                     => MUL_WB,
    LS_WB                      => LS_WB,
    instr_word_LS_WB           => instr_word_LS_WB,
    instr_word_IE_WB           => instr_word_IE_WB,
    harc_LS_WB                 => harc_LS_WB,
    harc_IE_WB                 => harc_IE_WB,
    zero_rs1                   => zero_rs1,
    zero_rs2                   => zero_rs2,
    pass_BEQ                   => pass_BEQ,
    pass_BNE                   => pass_BNE,
    pass_BLT                   => pass_BLT,
    pass_BLTU                  => pass_BLTU,
    pass_BGE                   => pass_BGE,
    pass_BGEU                  => pass_BGEU,
    RS1_Data_IE                => RS1_Data_IE,
    RS2_Data_IE                => RS2_Data_IE,
    RD_Data_IE                 => RD_Data_IE,
    return_address             => return_address,
    rs1_to_sc                  => rs1_to_sc,
    rs2_to_sc                  => rs2_to_sc,
    rd_to_sc                   => rd_to_sc,
    data_addr_internal_IE      => data_addr_internal_IE,
    regfile                    => regfile
  );
  
  ---------------------------------------------------------
  --  ████████╗██████╗  █████╗  ██████╗███████╗██████╗   --
  --  ╚══██╔══╝██╔══██╗██╔══██╗██╔════╝██╔════╝██╔══██╗  --
  --     ██║   ██████╔╝███████║██║     █████╗  ██████╔╝  --
  --     ██║   ██╔══██╗██╔══██║██║     ██╔══╝  ██╔══██╗  --
  --     ██║   ██║  ██║██║  ██║╚██████╗███████╗██║  ██║  --
  --     ╚═╝   ╚═╝  ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝╚═╝  ╚═╝  --
  ---------------------------------------------------------

  -- pragma translate_off

  Tracer_generate : if tracer_en = 1 generate
  Tracer_sync : process(clk_i, rst_ni) -- also implements the delay slot counters and some aux signals
    variable row  : line;
    variable row0 : line;
  begin
    if rst_ni = '0' then
    elsif rising_edge(clk_i) then


      ----------------------------------------------------------------
      --  ██╗███████╗    ████████╗██████╗  █████╗  ██████╗███████╗  --
      --  ██║██╔════╝    ╚══██╔══╝██╔══██╗██╔══██╗██╔════╝██╔════╝  --
      --  ██║█████╗         ██║   ██████╔╝███████║██║     █████╗    --
      --  ██║██╔══╝         ██║   ██╔══██╗██╔══██║██║     ██╔══╝    --
      --  ██║███████╗       ██║   ██║  ██║██║  ██║╚██████╗███████╗  --
      --  ╚═╝╚══════╝       ╚═╝   ╚═╝  ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝  --
      ----------------------------------------------------------------

      case state_IE is -- stage state
        when normal =>
          if  ie_instr_req = '0' and core_busy_IE_lat = '0' then
          elsif irq_pending(harc_EXEC) = '1' then
          else
            -- EXECUTE OF INSTRUCTION -------------------------------------------
            if (decoded_instruction_IE(SW_MIP_bit_position) = '1') then
              if (data_addr_internal_IE(31 downto 8) = x"0000FF") then -- checks that the instruction is not a store
                write(row0, "   " & to_string(now, ns) & "  ");  --Add a timestamp to line
                write(row0, ht);
                hwrite(row0, pc_IE);
                write(row0, string'("_"));
                hwrite(row0, instr_word_IE); 
              end if;
            else
              write(row0, "   " & to_string(now, ns) & "  ");  --Add a timestamp to line
              write(row0, ht);
              hwrite(row0, pc_IE);
              write(row0, string'("_"));
              hwrite(row0, instr_word_IE); 
            end if;

            if decoded_instruction_IE(ADDI_bit_position) = '1' then
              write(row0, string'("    addi x"));
            end if;

            if decoded_instruction_IE(SLTI_bit_position) = '1' then
              write(row0, string'("    slti x"));
            end if;

            if decoded_instruction_IE(SLTIU_bit_position) = '1' then
              write(row0, string'("    sltiu x"));
            end if;

            if decoded_instruction_IE(ANDI_bit_position) = '1' then
              write(row0, string'("    andi x"));
            end if;

            if decoded_instruction_IE(ORI_bit_position) = '1' then
              write(row0, string'("    ori x"));
            end if;

            if decoded_instruction_IE(XORI_bit_position) = '1' then
              write(row0, string'("    xori x"));
            end if;

            if decoded_instruction_IE(SLLI_bit_position) = '1' then
              write(row0, string'("    slli x"));
            end if;

            if decoded_instruction_IE(SRLI7_bit_position) = '1' then
              write(row0, string'("    srli x"));
            end if;

            if decoded_instruction_IE(SRAI7_bit_position) = '1' then
              write(row0, string'("    srai x"));
            end if;

            if   decoded_instruction_IE(ADDI_bit_position)  = '1' or decoded_instruction_IE(SLTI_bit_position)  = '1'
              or decoded_instruction_IE(SLTIU_bit_position) = '1' or decoded_instruction_IE(ANDI_bit_position)  = '1'
              or decoded_instruction_IE(ORI_bit_position)   = '1' or decoded_instruction_IE(XORI_bit_position)  = '1'
              or decoded_instruction_IE(SLLI_bit_position)  = '1' or decoded_instruction_IE(SRLI7_bit_position) = '1'
              or decoded_instruction_IE(SRAI7_bit_position) = '1' then
              write(row0, rd(instr_word_IE));
              write(row0, string'(",x"));
              write(row0, rs1(instr_word_IE));
              write(row0, string'(",0x"));
              hwrite(row0, I_imm(instr_word_IE));
              write(row0, ht);
              write(row0, ht);
              write(row0, string'("rs1=0x"));
              hwrite(row0, RS1_Data_IE);
              --if accl_en = 1 then
              --  write(row0, string'("      old_rd=0x"));
              --  hwrite(row0, RD_Data_IE);
              --end if;
              write(row0, string'("      new_rd=0x"));
              hwrite(row0, tracer_result);
            end if;

            if decoded_instruction_IE(LUI_bit_position) = '1' then
              write(row0, string'("    lui x"));
              write(row0, rd(instr_word_IE));
              write(row0, string'(",0x"));
              hwrite(row0, U_imm(instr_word_IE));
              write(row0, ht);
              write(row0, ht);
              --if accl_en = 1 then
              --  write(row0, string'("old_rd=0x"));
              --  hwrite(row0, RD_Data_IE);
              --end if;
              write(row0, string'("      new_rd=0x"));
              hwrite(row0, tracer_result);
            end if;

            if decoded_instruction_IE(AUIPC_bit_position) = '1' then
              write(row0, string'("    auipc x"));
              write(row0, rd(instr_word_IE));
              write(row0, string'(",0x"));
              hwrite(row0, U_imm(instr_word_IE));
              write(row0, ht);
              write(row0, ht);
              --if accl_en = 1 then
              --  write(row0, string'("old_rd=0x"));
              --  hwrite(row0, RD_Data_IE);
              --nd if;
              write(row0, string'("      new_rd=0x"));
              hwrite(row0, tracer_result);
            end if;

            if decoded_instruction_IE(ADD7_bit_position) = '1' then
              write(row0, string'("    add x"));
            end if;

            if decoded_instruction_IE(SUB7_bit_position) = '1' then
              write(row0, string'("    sub x"));
            end if;

            if decoded_instruction_IE(SLT_bit_position) = '1' then
              write(row0, string'("    slt x"));
            end if;

            if decoded_instruction_IE(SLTU_bit_position) = '1' then
              write(row0, string'("    sltu x"));
            end if;

            if decoded_instruction_IE(ANDD_bit_position) = '1' then
              write(row0, string'("    and x"));
            end if;

            if decoded_instruction_IE(ORR_bit_position) = '1' then
              write(row0, string'("    or x"));
            end if;

            if decoded_instruction_IE(XORR_bit_position) = '1' then
              write(row0, string'("    xor x"));
            end if;

            if decoded_instruction_IE(SLLL_bit_position) = '1' then
              write(row0, string'("    sll x"));
            end if;

            if decoded_instruction_IE(SRLL7_bit_position) = '1' then
              write(row0, string'("    srl x"));
            end if;

            if decoded_instruction_IE(SRAA7_bit_position) = '1' then
              write(row0, string'("    sra x"));
            end if;

            if   decoded_instruction_IE(ADD7_bit_position)  = '1' or decoded_instruction_IE(SUB7_bit_position)  = '1'
              or decoded_instruction_IE(SLT_bit_position)   = '1' or decoded_instruction_IE(SLTU_bit_position)  = '1'
              or decoded_instruction_IE(ANDD_bit_position)  = '1' or decoded_instruction_IE(ORR_bit_position)   = '1'
              or decoded_instruction_IE(XORR_bit_position)  = '1' or decoded_instruction_IE(SLLL_bit_position)  = '1'
              or decoded_instruction_IE(SRLL7_bit_position) = '1' or decoded_instruction_IE(SRAA7_bit_position) = '1' then
              write(row0, rd(instr_word_IE));
              write(row0, string'(",x"));
              write(row0, rs1(instr_word_IE));
              write(row0, string'(",x"));
              write(row0, rs2(instr_word_IE));
              write(row0, ht);
              write(row0, ht);
              write(row0, string'("rs1=0x"));
              hwrite(row0, RS1_Data_IE);
              write(row0, string'("      rs2=0x"));
              hwrite(row0, RS2_Data_IE);
              --if accl_en = 1 then
              --  write(row0, string'("      old_rd=0x"));
              --  hwrite(row0, RD_Data_IE);
              --end if;
              write(row0, string'("      new_rd=0x"));
              hwrite(row0, tracer_result);            end if;

            if decoded_instruction_IE(JAL_bit_position) = '1' then
              write(row0, string'("    jal x"));
              write(row0, rd(instr_word_IE));
              write(row0, string'(",0x"));
              hwrite(row0, UJ_imm(instr_word_IE));
              write(row0, ht);
              write(row0, ht);
              write(row0, string'("next_pc="));
              hwrite(row0, tracer_result);
            end if;

            if decoded_instruction_IE(JALR_bit_position) = '1' then
              write(row0, string'("    jalr x"));
              write(row0, rd(instr_word_IE));
              write(row0, string'(",x"));
              write(row0, rs1(instr_word_IE));
              write(row0, string'(",0x"));
              hwrite(row0, I_imm(instr_word_IE));
              write(row0, ht);
              write(row0, ht);
              write(row0, string'("next_pc="));
              hwrite(row0, tracer_result);
            end if;

            if decoded_instruction_IE(BEQ_bit_position) = '1' then
              write(row0, string'("    beq x"));
            end if;

            if decoded_instruction_IE(BNE_bit_position) = '1' then
              write(row0, string'("    bne x"));
            end if;

            if decoded_instruction_IE(BLT_bit_position) = '1' then
              write(row0, string'("    blt x"));
            end if;

            if decoded_instruction_IE(BLTU_bit_position) = '1' then
              write(row0, string'("    bltu x"));
            end if;

            if decoded_instruction_IE(BGE_bit_position) = '1' then
              write(row0, string'("    bge x"));
            end if;

            if decoded_instruction_IE(BGEU_bit_position) = '1' then
              write(row0, string'("    bgeu x"));
            end if;

            if decoded_instruction_IE(BEQ_bit_position)  = '1' or
               decoded_instruction_IE(BNE_bit_position)  = '1' or
               decoded_instruction_IE(BLT_bit_position)  = '1' or
               decoded_instruction_IE(BLTU_bit_position) = '1' or
               decoded_instruction_IE(BGE_bit_position)  = '1' or
               decoded_instruction_IE(BGEU_bit_position) = '1' then
              write(row0, rs1(instr_word_IE));
              write(row0, string'(",x"));
              write(row0, rs2(instr_word_IE));
              write(row0, string'(",0x"));
              hwrite(row0, B_imm(instr_word_IE));
              write(row0, ht);
              write(row0, ht);
              write(row0, string'("rs1=0x"));
              hwrite(row0, RS1_Data_IE);
              write(row0, string'("      rs2=0x"));
              hwrite(row0, RS2_Data_IE);
              write(row0, string'("      next_pc=0x"));
              hwrite(row0, tracer_result);
            end if;

            if decoded_instruction_IE(SW_MIP_bit_position) = '1' then
            end if;

            if decoded_instruction_IE(FENCE_bit_position) = '1' then
              write(row0, string'("    fence"));
            end if;

            if decoded_instruction_IE(FENCEI_bit_position) = '1' then
              write(row0, string'("    fencei"));
            end if;

            if decoded_instruction_IE(ECALL_bit_position) = '1' then
              write(row0, string'("    ecall"));
            end if;

            if decoded_instruction_IE(EBREAK_bit_position) = '1' then
              write(row0, string'("    ebreak"));
            end if;

            if decoded_instruction_IE(MRET_bit_position) = '1' then
              write(row0, string'("    mret"));
            end if;

            if decoded_instruction_IE(WFI_bit_position) = '1' then
              write(row0, string'("    wfi"));
            end if;

            if decoded_instruction_IE(CSRRW_bit_position) = '1' then
              write(row0, string'("    csrw x"));
            end if;

            if decoded_instruction_IE(CSRRC_bit_position) = '1' then
              write(row0, string'("    csrc x"));
            end if;

            if decoded_instruction_IE(CSRRS_bit_position) = '1' then
              write(row0, string'("    csrs x"));
            end if;

            if decoded_instruction_IE(CSRRWI_bit_position) = '1' then
              write(row0, string'("    csrwi x"));
            end if;

            if decoded_instruction_IE(CSRRSI_bit_position) = '1' then
              write(row0, string'("    csrsi x"));
            end if;

            if decoded_instruction_IE(CSRRCI_bit_position) = '1' then
              write(row0, string'("    csrci x"));
            end if;

            if decoded_instruction_IE(CSRRW_bit_position)  = '1' or
               decoded_instruction_IE(CSRRC_bit_position)  = '1' or 
               decoded_instruction_IE(CSRRS_bit_position)  = '1' or
               decoded_instruction_IE(CSRRWI_bit_position) = '1' or
               decoded_instruction_IE(CSRRSI_bit_position) = '1' or
               decoded_instruction_IE(CSRRCI_bit_position) = '1' then
                write(row0, rd(instr_word_IE));
                write(row0, string'(",x"));
                write(row0, rs1(instr_word_IE));
                case CSR_ADDR(instr_word_IE) is
                  when MVSIZE_addr =>
                    write(row0, string'(",mvsize"));
                  when MPSCLFAC_addr =>
                    write(row0, string'(",mpsclfac"));
                  when MSTATUS_addr =>
                    write(row0, string'(",mstatus"));
                  when MIP_addr =>
                    write(row0, string'(",mip"));
                  when MEPC_addr =>
                    write(row0, string'(",mepc"));
                  when MTVEC_addr =>
                    write(row0, string'(",mtvec"));
                  when MCAUSE_addr =>
                    write(row0, string'(",mcause"));
                  when MESTATUS_addr =>
                    write(row0, string'(",mestatus"));
                  when MCPUID_addr =>
                    write(row0, string'(",mcpuid"));
                  when MIMPID_addr =>
                    write(row0, string'(",mimpid"));
                  when MHARTID_addr =>
                    write(row0, string'(",mhartid"));
                  when MIRQ_addr =>
                    write(row0, string'(",mirq"));
                  when BADADDR_addr =>
                    write(row0, string'(",badaddr"));
                  when MCYCLE_addr =>
                    write(row0, string'(",mcycle"));
                  when MCYCLEH_addr =>
                    write(row0, string'(",mcycleh"));
                  when MINSTRET_addr =>
                    write(row0, string'(",minstret"));
                  when MINSTRETH_addr =>
                    write(row0, string'(",minstreth"));
                  when MHPMCOUNTER3_addr =>
                    write(row0, string'(",mhpcounter3"));
                  when MHPMCOUNTER6_addr =>
                    write(row0, string'(",mhpcounter6"));
                  when MHPMCOUNTER7_addr =>
                    write(row0, string'(",mhpcounter7"));
                  when MHPMCOUNTER8_addr =>
                    write(row0, string'(",mhpcounter8"));
                  when MHPMCOUNTER9_addr =>
                    write(row0, string'(",mhpcounter10"));
                  when MHPMCOUNTER10_addr =>
                    write(row0, string'(",mhpcounter10"));
                  when PCER_addr =>
                    write(row0, string'(",pcer"));
                  when MHPMEVENT3_addr =>
                    write(row0, string'(",mhpevent3"));
                  when MHPMEVENT6_addr =>
                    write(row0, string'(",mhpevent6"));
                  when MHPMEVENT7_addr =>
                    write(row0, string'(",mhpevent7"));
                  when MHPMEVENT8_addr =>
                    write(row0, string'(",mhpevent8"));
                  when MHPMEVENT9_addr =>
                    write(row0, string'(",mhpevent9"));
                  when MHPMEVENT10_addr =>
                    write(row0, string'(",mhpevent10"));
                  when others =>
                    write(row0, string'(",0x"));
                    hwrite(row0, instr_word_IE(31 downto 20));
                end case;
                write(row0, ht);
                write(row0, ht);
                write(row0, string'("rs1=0x"));
                hwrite(row0, RS1_Data_IE);
                --if accl_en = 1 then
                --  write(row0, string'("      old_rd=0x"));
                --  hwrite(row0, RD_Data_IE);
                --end if;
            end if;

            if decoded_instruction_IE(ILL_bit_position) = '1' then
              write(row0, string'("    ill"));
            end if;

            if decoded_instruction_IE(NOP_bit_position) = '1' then
              write(row0, string'("    nop"));
            end if;

            if decoded_instruction_IE(MUL_bit_position) = '1' then
              write(row0, string'("    mul x"));
            end if;

            if decoded_instruction_IE(MULH_bit_position)   = '1' then
              write(row0, string'("    mulh x"));
            end if;

            if decoded_instruction_IE(MULHU_bit_position)  = '1' then
              write(row0, string'("    mulhu x"));
            end if;

            if decoded_instruction_IE(MULHSU_bit_position) = '1' then
              write(row0, string'("    mulhsu x"));
            end if;

            if decoded_instruction_IE(DIVU_bit_position) = '1' then
              write(row0, string'("    divu x"));
            end if;

            if decoded_instruction_IE(DIV_bit_position) = '1' then
              write(row0, string'("    div x"));
            end if;

            if decoded_instruction_IE(REMU_bit_position) = '1' then
              write(row0, string'("    remu x"));
            end if;

            if decoded_instruction_IE(REM_bit_position) = '1' then
              write(row0, string'("    rem x"));
            end if;

            if decoded_instruction_IE(MUL_bit_position)    = '1' or
               decoded_instruction_IE(MULH_bit_position)   = '1' or
               decoded_instruction_IE(MULHU_bit_position)  = '1' or
               decoded_instruction_IE(MULHSU_bit_position) = '1' or
               decoded_instruction_IE(DIV_bit_position)    = '1' or
               decoded_instruction_IE(DIVU_bit_position)   = '1' or
               decoded_instruction_IE(REMU_bit_position)   = '1' or
               decoded_instruction_IE(REM_bit_position)    = '1' then
              write(row0, rd(instr_word_IE));
              write(row0, string'(",x"));
              write(row0, rs1(instr_word_IE));
              write(row0, string'(",x"));
              write(row0, rs2(instr_word_IE));
              write(row0, ht);
              write(row0, ht);
              write(row0, string'("rs1=0x"));
              hwrite(row0, RS1_Data_IE);
              write(row0, string'("      rs2=0x"));
              hwrite(row0, RS2_Data_IE);
              --if accl_en = 1 then
              --  write(row0, string'("      old_rd=0x"));
              --  hwrite(row0, RD_Data_IE);
              --end if;
              write(row0, string'("      new_rd=0x"));
              if decoded_instruction_IE(MUL_bit_position) = '1' then
                hwrite(row0, tracer_mul_result(31 downto 0));
              elsif decoded_instruction_IE(MULH_bit_position)   = '1' or
                    decoded_instruction_IE(MULHU_bit_position)  = '1' or
                    decoded_instruction_IE(MULHSU_bit_position) = '1' then
                hwrite(row0, tracer_mul_result(63 downto 32));
              end if;
            end if;


          -- EXECUTE OF INSTRUCTION (END) --------------------------
          end if;  -- instr_rvalid_IE values
        when others =>
      end case;  -- fsm_IE state cases


      ----------------------------------------------------------------------------
      --  ██╗     ███████╗██╗   ██╗    ████████╗██████╗  █████╗  ██████╗███████╗  --
      --  ██║     ██╔════╝██║   ██║    ╚══██╔══╝██╔══██╗██╔══██╗██╔════╝██╔════╝  --
      --  ██║     ███████╗██║   ██║       ██║   ██████╔╝███████║██║     █████╗    --
      --  ██║     ╚════██║██║   ██║       ██║   ██╔══██╗██╔══██║██║     ██╔══╝    --
      --  ███████╗███████║╚██████╔╝       ██║   ██║  ██║██║  ██║╚██████╗███████╗  --
      --  ╚══════╝╚══════╝ ╚═════╝        ╚═╝   ╚═╝  ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝  --
      ----------------------------------------------------------------------------

      if LS_instr_req = '1' and halt_LSU = '0' then
        case state_LS is  
          when normal =>

            if not(decoded_instruction_IE(SW_MIP_bit_position) = '1' and data_addr_internal_IE(31 downto 8) = x"0000FF") then -- checks that the instruction is not a sw_mip
              write(row0, "   " & to_string(now, ns) & "  ");  --Add a timestamp to line
              write(row0, ht);
              hwrite(row0, pc_IE);
              write(row0, '_');
              hwrite(row0, instr_word_IE); 
            end if;

            if decoded_instruction_LS(LW_bit_position) = '1' then
              write(row0, string'("    lw x"));
            end if;

            if decoded_instruction_LS(LH_bit_position) = '1' then
              write(row0, string'("    lh x"));
            end if;

            if decoded_instruction_LS(LHU_bit_position) = '1' then
              write(row0, string'("    lhu x"));
            end if;

            if decoded_instruction_LS(LB_bit_position) = '1' then
              write(row0, string'("    lb x"));
            end if;

            if decoded_instruction_LS(LBU_bit_position) = '1' then
              write(row0, string'("    lbu x"));
            end if;

            if decoded_instruction_LS(LW_bit_position) = '1' or
               decoded_instruction_LS(LH_bit_position) = '1' or decoded_instruction_LS(LHU_bit_position) = '1' or 
               decoded_instruction_LS(LB_bit_position) = '1' or decoded_instruction_LS(LBU_bit_position) = '1' then
              write(row0, rd(instr_word_IE));
              write(row0, string'(","));
              write(row0, string'(",0x"));
              hwrite(row0, I_imm(instr_word_IE));
              write(row0, string'("(x"));
              write(row0, rs1(instr_word_IE));
              write(row0, string'(")"));
              write(row0, ht);
              write(row0, ht);
              write(row0, ht);
              write(row0, string'("rs1=0x"));
              hwrite(row0, RS1_Data_IE);
              --write(row0, string'("      rd=0x"));
              --hwrite(row0, RD_Data_IE);
              write(row0, string'("      addr=0x"));
              hwrite(row0, tracer_result);
            end if;

            if decoded_instruction_LS(SW_bit_position) = '1' then
              write(row0, string'("    sw x"));
            end if;

            if decoded_instruction_LS(SH_bit_position) = '1' then
              write(row0, string'("    sh x"));
            end if;

            if decoded_instruction_LS(SB_bit_position) = '1' then
              write(row0, string'("    sb x"));
            end if;

           if decoded_instruction_LS(SW_bit_position) = '1' or
              decoded_instruction_LS(SH_bit_position) = '1' or
              decoded_instruction_LS(SB_bit_position) = '1' then
             write(row0, rs2(instr_word_IE));
             write(row0, string'(","));
             write(row0, string'(",0x"));
             hwrite(row0, S_imm(instr_word_IE));
             write(row0, string'("(x"));
             write(row0, rs1(instr_word_IE));
             write(row0, string'(")"));
             write(row0, ht);
             write(row0, ht);
             write(row0, ht);
             write(row0, string'("rs1=0x"));
             hwrite(row0, RS1_Data_IE);
             write(row0, string'("      rs2=0x"));
             hwrite(row0, RS2_Data_IE);
             write(row0, string'("      addr=0x"));
             hwrite(row0, tracer_result);
           end if;

            if decoded_instruction_LS(AMOSWAP_bit_position) = '1' then
              write(row0, string'("    amoswap x"));
              write(row0, rd(instr_word_IE));
              write(row0, string'(",x"));
              write(row0, rs2(instr_word_IE));
              write(row0, string'(",(x"));
              write(row0, rs1(instr_word_IE));
              write(row0, string'(")"));
              write(row0, string'("      rs1=0x"));
              hwrite(row0, RS1_Data_IE);
              write(row0, string'("      rs2=0x"));
              hwrite(row0, RS2_Data_IE);
              write(row0, string'("      rd=0x"));
              hwrite(row0, RD_Data_IE);
              write(row0, string'("      addr=0x"));
              hwrite(row0, tracer_result);
            end if;

            if decoded_instruction_LS(KMEMLD_bit_position)   = '1' then
              write(row0, string'("    kmemld x"));
            end if;

           if decoded_instruction_LS(KBCASTLD_bit_position) = '1' then
             write(row0, string'("    kbcastld x"));
            end if;

            if decoded_instruction_LS(KMEMSTR_bit_position) = '1' then
              write(row0, string'("    kmemstr x"));
            end if;

            if decoded_instruction_LS(KMEMLD_bit_position)   = '1' or
               decoded_instruction_LS(KBCASTLD_bit_position) = '1' or 
               decoded_instruction_LS(KMEMSTR_bit_position)  = '1' then
              write(row0, rd(instr_word_IE));
              write(row0, string'(",x"));
              write(row0, rs1(instr_word_IE));
              write(row0, string'(",x"));
              write(row0, rs2(instr_word_IE));
              write(row0, string'("      rs1=0x"));
              hwrite(row0, RS1_Data_IE);
              write(row0, string'("      rs2=0x"));
              hwrite(row0, RS2_Data_IE);
              write(row0, string'("      rd=0x"));
              hwrite(row0, RD_Data_IE);
              write(row0, string'("      addr=0x"));
              hwrite(row0, tracer_result);
            end if;

          when data_valid_waiting =>
        end case;
      end if;


      -----------------------------------------------------------------------------
      --  ██████╗ ███████╗██████╗     ████████╗██████╗  █████╗  ██████╗███████╗  --
      --  ██╔══██╗██╔════╝██╔══██╗    ╚══██╔══╝██╔══██╗██╔══██╗██╔════╝██╔════╝  --
      --  ██║  ██║███████╗██████╔╝       ██║   ██████╔╝███████║██║     █████╗    --
      --  ██║  ██║╚════██║██╔═══╝        ██║   ██╔══██╗██╔══██║██║     ██╔══╝    --
      --  ██████╔╝███████║██║            ██║   ██║  ██║██║  ██║╚██████╗███████╗  --
      --  ╚═════╝ ╚══════╝╚═╝            ╚═╝   ╚═╝  ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝  --
      -----------------------------------------------------------------------------
      if accl_en = 1 then
          for h in accl_range loop
            if dsp_instr_req(h) = '1' and instr_word_IE /= x"0000_006F" then -- checks that the valid dsp instruction did not to execute as an infinite jump
              write(row0, "   " & to_string(now, ns) & "  ");  --Add a timestamp to line
              write(row0, ht);
              hwrite(row0, pc_IE);
              write(row0, '_');
              hwrite(row0, instr_word_IE);
              -- Set signals to enable correct virtual parallelism operation
              if decoded_instruction_DSP(KADDV_bit_position)       = '1' then
                write(row0, string'("    kaddv x"));
              elsif decoded_instruction_DSP(KSVADDRF_bit_position) = '1' then
                write(row0, string'("    ksvaddrf x"));
              elsif decoded_instruction_DSP(KSVADDSC_bit_position) = '1' then
                write(row0, string'("    ksvaddsc x"));
              elsif decoded_instruction_DSP(KSUBV_bit_position)    = '1' then
                write(row0, string'("    ksubv x"));
              elsif decoded_instruction_DSP(KVMUL_bit_position)    = '1' then
                write(row0, string'("    kvmul x"));
              elsif decoded_instruction_DSP(KSVMULRF_bit_position) = '1' then
                write(row0, string'("    ksvmulrf x"));
              elsif decoded_instruction_DSP(KSVMULSC_bit_position) = '1' then
                write(row0, string'("    ksvmulsc x"));
              elsif decoded_instruction_DSP(KSRLV_bit_position)    = '1' then
                write(row0, string'("    ksrlv x"));
              elsif decoded_instruction_DSP(KSRAV_bit_position)    = '1' then
                write(row0, string'("    ksrav x"));
              elsif decoded_instruction_DSP(KVRED_bit_position)    = '1' then
                write(row0, string'("    kvred x"));
              elsif decoded_instruction_DSP(KDOTP_bit_position)    = '1' then
                write(row0, string'("    kdotp x"));
              elsif decoded_instruction_DSP(KDOTPPS_bit_position)  = '1' then
                write(row0, string'("    kdotpps x"));
              elsif decoded_instruction_DSP(KRELU_bit_position)    = '1' then
                write(row0, string'("    krelu x"));
              elsif decoded_instruction_DSP(KVSLT_bit_position)    = '1' then
                write(row0, string'("    kvslt x"));
              elsif decoded_instruction_DSP(KSVSLT_bit_position)   = '1' then
                write(row0, string'("    ksvslt x"));
              elsif decoded_instruction_DSP(KBCAST_bit_position)   = '1' then
                write(row0, string'("    kbcast x"));
              elsif decoded_instruction_DSP(KVCP_bit_position)     = '1' then
                write(row0, string'("    kvcp x"));
              end if;

              if decoded_instruction_DSP(KVRED_bit_position)  = '1'  or
                 decoded_instruction_DSP(KRELU_bit_position)  = '1'  or
                 decoded_instruction_DSP(KBCAST_bit_position) = '1'  or
                 decoded_instruction_DSP(KVCP_bit_position)   = '1'  or
                 FUNCT7(instr_word_IE) = KBCAST then
                write(row0, rd(instr_word_IE));
                write(row0, string'(",x"));
                write(row0, rs1(instr_word_IE));
              else
                write(row0, rd(instr_word_IE));
                write(row0, string'(",x"));
                write(row0, rs1(instr_word_IE));
                write(row0, string'(",x"));
                write(row0, rs2(instr_word_IE));
              end if;
              write(row0, string'("      SPM_rd("));
              write(row0, to_integer(unsigned(rd_to_sc)));
              write(row0, string'(")=0x"));
              hwrite(row0, RD_Data_IE);
              if spm_rs1 = '1' then
                write(row0, string'("      SPM_rs1("));
                write(row0, to_integer(unsigned(rs1_to_sc)));
                write(row0, string'(")=0x"));
                hwrite(row0, RS1_Data_IE);
              else
                write(row0, string'("      RF_rs1=0x"));
                hwrite(row0, RS1_Data_IE);
              end if;
              if spm_rs2 = '1' then
                write(row0, string'("      SPM_rs2("));
                write(row0, to_integer(unsigned(rs2_to_sc)));
                write(row0, string'(")=0x"));
                hwrite(row0, RS2_Data_IE);
              else
                write(row0, string'("      RF_rs2=0x"));
                hwrite(row0, RS2_Data_IE);
              end if;
              write(row0, string'("      MVSIZE=0x"));
              hwrite(row0, MVSIZE(h));
              write(row0, string'("(0d'"));
              write(row0, to_integer(unsigned(MVSIZE(h))));
              write(row0, string'(")"));
           end if;
         end loop;
     end if;
     ----------------------------------------------------------------------- Write Line -------------------------------------------------------------
     for i in 0 to THREAD_POOL_SIZE-1 loop
       if harc_EXEC=i then  
         if (instr_rvalid_IE = '1') then
           if i = 0 then
             writeline(file_handler0, row0);  -- Writes line to instr. trace file
           elsif i = 1 then
             writeline(file_handler1, row0);  -- Writes line to instr. trace file
           elsif i = 2 then
             writeline(file_handler2, row0);  -- Writes line to instr. trace file
           end if;
         end if;
       end if;
     end loop;
     ------------------------------------------------------------------------------------------------------------------------------------------------

    end if;  -- reset, clk_i
  end process;
  
  

  ----------------------------------------------------------------------------------------------------
  --  ████████╗██████╗  █████╗ ███████╗██████╗     ██████╗ ███████╗███████╗██╗   ██╗██╗  ████████╗  --
  --  ╚══██╔══╝██╔══██╗██╔══██╗██╔════╝██╔══██╗    ██╔══██╗██╔════╝██╔════╝██║   ██║██║  ╚══██╔══╝  --
  --     ██║   ██████╔╝███████║█████╗  ██████╔╝    ██████╔╝█████╗  ███████╗██║   ██║██║     ██║     --
  --     ██║   ██╔══██╗██╔══██║██╔══╝  ██╔══██╗    ██╔══██╗██╔══╝  ╚════██║██║   ██║██║     ██║     --
  --     ██║   ██║  ██║██║  ██║███████╗██║  ██║    ██║  ██║███████╗███████║╚██████╔╝███████╗██║     --
  --     ╚═╝   ╚═╝  ╚═╝╚═╝  ╚═╝╚══════╝╚═╝  ╚═╝    ╚═╝  ╚═╝╚══════╝╚══════╝ ╚═════╝ ╚══════╝╚═╝     --
  ----------------------------------------------------------------------------------------------------


  Tracer_Comb : process(all) -- also implements the delay slot counters and some aux signals
  begin

    IE_instr                        <= '0';
    LSU_instr                       <= '0';
    DSP_instr                       <= '0';
    rs1_valid                       <= '0';
    rs2_valid                       <= '0';
    rd_read_only_valid              <= '0';
    rd_valid                        <= '0';

    case state_IE is                  -- stage status
      when normal =>
        if ie_instr_req = '0' and core_busy_IE_lat = '0' then
        elsif irq_pending(harc_EXEC) = '1' then
        else-- process the instruction
          IE_instr <= '1';
          -- TRACE IF EXECUTE INSTRUCTIONS ---------------------

          if decoded_instruction_IE(ADDI_bit_position)  = '1'  then
            rs1_valid <= '1' when rs1(instr_word_IE) /= 0;
            rd_valid  <= '1' when rd(instr_word_IE)  /= 0;
            tracer_result <= std_logic_vector(signed(RS1_Data_IE)+signed(I_immediate(instr_word_IE)));
          end if;

          if decoded_instruction_IE(SLTI_bit_position)  = '1'  then
            rs1_valid <= '1' when rs1(instr_word_IE) /= 0;
            rd_valid  <= '1' when rd(instr_word_IE)  /= 0;
            if (signed(RS1_Data_IE) < signed (I_immediate(instr_word_IE))) then
              tracer_result <= std_logic_vector(to_unsigned(1, 32));
            else
              tracer_result <= std_logic_vector(to_unsigned(0, 32));
            end if;
          end if;

          if decoded_instruction_IE(SLTIU_bit_position)  = '1'  then
            rs1_valid <= '1' when rs1(instr_word_IE) /= 0;
            rd_valid  <= '1' when rd(instr_word_IE)  /= 0;
            if (unsigned(RS1_Data_IE) < unsigned (I_immediate(instr_word_IE))) then
              tracer_result <= std_logic_vector(to_unsigned(1, 32));
            else
              tracer_result <= std_logic_vector(to_unsigned(0, 32));
            end if;
          end if;

          if decoded_instruction_IE(ANDI_bit_position)  = '1'  then
            rs1_valid <= '1' when rs1(instr_word_IE) /= 0;
            rd_valid  <= '1' when rd(instr_word_IE)  /= 0;
            tracer_result <= RS1_Data_IE and I_immediate(instr_word_IE);
          end if;

          if decoded_instruction_IE(ORI_bit_position)  = '1'  then
            rs1_valid <= '1' when rs1(instr_word_IE) /= 0;
            rd_valid  <= '1' when rd(instr_word_IE)  /= 0;
            tracer_result <= RS1_Data_IE or I_immediate(instr_word_IE);
          end if;

          if decoded_instruction_IE(XORI_bit_position)  = '1'  then
            rs1_valid <= '1' when rs1(instr_word_IE) /= 0;
            rd_valid  <= '1' when rd(instr_word_IE)  /= 0;
            tracer_result <= RS1_Data_IE xor I_immediate(instr_word_IE);
          end if;

          if decoded_instruction_IE(SLLI_bit_position)  = '1'  then
            rs1_valid <= '1' when rs1(instr_word_IE) /= 0;
            rd_valid  <= '1' when rd(instr_word_IE)  /= 0;
            tracer_result <= to_stdlogicvector(to_bitvector(RS1_Data_IE) sll to_integer(unsigned(SHAMT(instr_word_IE))));
          end if;

          if decoded_instruction_IE(SRLI7_bit_position)  = '1'  then
            rs1_valid <= '1' when rs1(instr_word_IE) /= 0;
            rd_valid  <= '1' when rd(instr_word_IE)  /= 0;
            tracer_result <= to_stdlogicvector(to_bitvector(RS1_Data_IE) srl to_integer(unsigned(SHAMT(instr_word_IE))));
          end if;

          if decoded_instruction_IE(SRAI7_bit_position)  = '1'  then
            rs1_valid <= '1' when rs1(instr_word_IE) /= 0;
            rd_valid  <= '1' when rd(instr_word_IE)  /= 0;
            tracer_result <= to_stdlogicvector(to_bitvector(RS1_Data_IE) sra to_integer(unsigned(SHAMT(instr_word_IE))));
          end if;

          if decoded_instruction_IE(LUI_bit_position) = '1' then
            rd_valid  <= '1' when rd(instr_word_IE)  /= 0;
            tracer_result <= std_logic_vector(unsigned(U_immediate(instr_word_IE)));
          end if;

          if decoded_instruction_IE(AUIPC_bit_position) = '1' then
            rd_valid  <= '1' when rd(instr_word_IE)  /= 0;
            tracer_result <= std_logic_vector(unsigned(U_immediate(instr_word_IE))+unsigned(pc_IE));
          end if;

          if decoded_instruction_IE(ADD7_bit_position) = '1' then
            rs1_valid <= '1' when rs1(instr_word_IE) /= 0;
            rs2_valid <= '1' when rs2(instr_word_IE) /= 0;
            rd_valid  <= '1' when rd(instr_word_IE)  /= 0;
            tracer_result <= std_logic_vector(signed(RS1_Data_IE)+signed(RS2_Data_IE));
          end if;

          if decoded_instruction_IE(SUB7_bit_position) = '1' then
            rs1_valid <= '1' when rs1(instr_word_IE) /= 0;
            rs2_valid <= '1' when rs2(instr_word_IE) /= 0;
            rd_valid  <= '1' when rd(instr_word_IE)  /= 0;
            tracer_result <= std_logic_vector(signed(RS1_Data_IE)-signed(RS2_Data_IE));
          end if;

          if decoded_instruction_IE(SLT_bit_position) = '1' then
            rs1_valid <= '1' when rs1(instr_word_IE) /= 0;
            rs2_valid <= '1' when rs2(instr_word_IE) /= 0;
            rd_valid  <= '1' when rd(instr_word_IE)  /= 0;
            if (signed(RS1_Data_IE) < signed(RS2_Data_IE)) then
              tracer_result <= std_logic_vector(to_unsigned(1, 32));
            else
              tracer_result <= std_logic_vector(to_unsigned(0, 32));
            end if;
          end if;

          if decoded_instruction_IE(SLTU_bit_position) = '1' then
            rs1_valid <= '1' when rs1(instr_word_IE) /= 0;
            rs2_valid <= '1' when rs2(instr_word_IE) /= 0;
            rd_valid  <= '1' when rd(instr_word_IE)  /= 0;
            if (unsigned(RS1_Data_IE) < unsigned(RS2_Data_IE)) then
              tracer_result <= std_logic_vector(to_unsigned(1, 32));
            else
              tracer_result <= std_logic_vector(to_unsigned(0, 32));
            end if;
          end if;

          if decoded_instruction_IE(ANDD_bit_position) = '1' then
            rs1_valid <= '1' when rs1(instr_word_IE) /= 0;
            rs2_valid <= '1' when rs2(instr_word_IE) /= 0;
            rd_valid  <= '1' when rd(instr_word_IE)  /= 0;
            tracer_result <= RS1_Data_IE and RS2_Data_IE;
          end if;

          if decoded_instruction_IE(ORR_bit_position) = '1' then
            rs1_valid <= '1' when rs1(instr_word_IE) /= 0;
            rs2_valid <= '1' when rs2(instr_word_IE) /= 0;
            rd_valid  <= '1' when rd(instr_word_IE)  /= 0;
            tracer_result <= RS1_Data_IE or RS2_Data_IE;
          end if;

          if decoded_instruction_IE(XORR_bit_position) = '1' then
            rs1_valid <= '1' when rs1(instr_word_IE) /= 0;
            rs2_valid <= '1' when rs2(instr_word_IE) /= 0;
            rd_valid  <= '1' when rd(instr_word_IE)  /= 0;
            tracer_result <= RS1_Data_IE xor RS2_Data_IE;
          end if;

          if decoded_instruction_IE(SLLL_bit_position) = '1' then
            rs1_valid <= '1' when rs1(instr_word_IE) /= 0;
            rs2_valid <= '1' when rs2(instr_word_IE) /= 0;
            rd_valid  <= '1' when rd(instr_word_IE)  /= 0;
            tracer_result <= to_stdlogicvector(to_bitvector(RS1_Data_IE) sll to_integer(unsigned(RS2_Data_IE(4 downto 0))));
          end if;

          if decoded_instruction_IE(SRLL7_bit_position) = '1' then
            rs1_valid <= '1' when rs1(instr_word_IE) /= 0;
            rs2_valid <= '1' when rs2(instr_word_IE) /= 0;
            rd_valid  <= '1' when rd(instr_word_IE)  /= 0;
            tracer_result <= to_stdlogicvector(to_bitvector(RS1_Data_IE) srl to_integer(unsigned(RS2_Data_IE(4 downto 0))));
          end if;

          if decoded_instruction_IE(SRAA7_bit_position) = '1' then
            rs1_valid <= '1' when rs1(instr_word_IE) /= 0;
            rs2_valid <= '1' when rs2(instr_word_IE) /= 0;
            rd_valid  <= '1' when rd(instr_word_IE)  /= 0;
            tracer_result <= to_stdlogicvector(to_bitvector(RS1_Data_IE) sra to_integer(unsigned(RS2_Data_IE(4 downto 0))));
          end if;

          if decoded_instruction_IE(FENCE_bit_position) = '1' or decoded_instruction_IE(FENCEI_bit_position) = '1' then
          end if;

          if decoded_instruction_IE(JAL_bit_position) = '1' then  -- JAL instruction
            rd_valid  <= '1' when rd(instr_word_IE)  /= 0;
            tracer_result <= std_logic_vector(signed(pc_IE)+signed(UJ_immediate(instr_word_IE)));
          end if;

          if decoded_instruction_IE(JALR_bit_position) = '1' then  --JALR instruction
            rs1_valid <= '1' when rs1(instr_word_IE) /= 0;
            rd_valid  <= '1' when rd(instr_word_IE)  /= 0;
            tracer_result <= std_logic_vector(signed(RS1_Data_IE)+signed(I_immediate(instr_word_IE)));
          end if;

          if decoded_instruction_IE(BEQ_bit_position) = '1' then
            rs1_valid <= '1' when rs1(instr_word_IE) /= 0;
            rs2_valid <= '1' when rs2(instr_word_IE) /= 0;
            if (signed(RS1_Data_IE) = signed(RS2_Data_IE)) then
              tracer_result <= std_logic_vector(signed(pc_IE)+signed(B_immediate(instr_word_IE)));
            else
              tracer_result <= std_logic_vector(signed(pc_IE)+4);                
            end if;
          end if;

          if decoded_instruction_IE(BNE_bit_position) = '1' then
            rs1_valid <= '1' when rs1(instr_word_IE) /= 0;
            rs2_valid <= '1' when rs2(instr_word_IE) /= 0;
            if (signed(RS1_Data_IE) /= signed(RS2_Data_IE)) then
              tracer_result <= std_logic_vector(signed(pc_IE)+signed(B_immediate(instr_word_IE)));
            else
              tracer_result <= std_logic_vector(signed(pc_IE)+4);  
            end if;
          end if;

          if decoded_instruction_IE(BLT_bit_position) = '1' then
            rs1_valid <= '1' when rs1(instr_word_IE) /= 0;
            rs2_valid <= '1' when rs2(instr_word_IE) /= 0;
            if (signed(RS1_Data_IE) < signed(RS2_Data_IE)) then
              tracer_result <= std_logic_vector(signed(pc_IE)+signed(B_immediate(instr_word_IE)));
            else
              tracer_result <= std_logic_vector(signed(pc_IE)+4);  
            end if;
          end if;

          if decoded_instruction_IE(BLTU_bit_position) = '1' then
            rs1_valid <= '1' when rs1(instr_word_IE) /= 0;
            rs2_valid <= '1' when rs2(instr_word_IE) /= 0;
            if (unsigned(RS1_Data_IE) < unsigned(RS2_Data_IE)) then
              tracer_result <= std_logic_vector(signed(pc_IE)+signed(B_immediate(instr_word_IE)));
            else
              tracer_result <= std_logic_vector(signed(pc_IE)+4);  
            end if;
          end if;

          if decoded_instruction_IE(BGE_bit_position) = '1' then
            rs1_valid <= '1' when rs1(instr_word_IE) /= 0;
            rs2_valid <= '1' when rs2(instr_word_IE) /= 0;
            if (signed(RS1_Data_IE) >= signed(RS2_Data_IE)) then
              tracer_result <= std_logic_vector(signed(pc_IE)+signed(B_immediate(instr_word_IE)));
            else
              tracer_result <= std_logic_vector(signed(pc_IE)+4);  
            end if;
          end if;

          if decoded_instruction_IE(BGEU_bit_position) = '1' then
            rs1_valid <= '1' when rs1(instr_word_IE) /= 0;
            rs2_valid <= '1' when rs2(instr_word_IE) /= 0;
            if (signed(RS1_Data_IE) >= signed(RS2_Data_IE)) then
              tracer_result <= std_logic_vector(signed(pc_IE)+signed(B_immediate(instr_word_IE)));
            else
              tracer_result <= std_logic_vector(signed(pc_IE)+4);  
            end if;
          end if;

          if decoded_instruction_IE(SW_MIP_bit_position) = '1' then
            rs1_valid <= '1' when rs1(instr_word_IE) /= 0;
            rs2_valid <= '1' when rs2(instr_word_IE) /= 0;
          end if;

          if decoded_instruction_IE(CSRRW_bit_position) = '1' or decoded_instruction_IE(CSRRWI_bit_position) = '1' or
             decoded_instruction_IE(CSRRC_bit_position) = '1' or decoded_instruction_IE(CSRRCI_bit_position) = '1' or
             decoded_instruction_IE(CSRRS_bit_position) = '1' or decoded_instruction_IE(CSRRSI_bit_position) = '1' then
            rs1_valid <= '1' when rs1(instr_word_IE) /= 0;
            rd_valid  <= '1' when rd(instr_word_IE)  /= 0;
          end if;

          if decoded_instruction_IE(ECALL_bit_position) = '1' then
          end if;

          if decoded_instruction_IE(EBREAK_bit_position) = '1' then
          end if;

          if decoded_instruction_IE(MRET_bit_position) = '1' then
          end if;

          if decoded_instruction_IE(WFI_bit_position) = '1' then
          end if;

          if decoded_instruction_IE(ILL_bit_position) = '1' then  -- ILLEGAL_INSTRUCTION
          end if;

          if decoded_instruction_IE(NOP_bit_position) = '1' then
          end if;

          if decoded_instruction_IE(MUL_bit_position)    = '1' or 
             decoded_instruction_IE(MULH_bit_position)   = '1' or
             decoded_instruction_IE(MULHU_bit_position)  = '1' or
             decoded_instruction_IE(MULHSU_bit_position) = '1' then
            rs1_valid <= '1' when rs1(instr_word_IE) /= 0;
            rs2_valid <= '1' when rs2(instr_word_IE) /= 0;
            rd_valid  <= '1' when rd(instr_word_IE)  /= 0;
          end if;

          if decoded_instruction_IE(MUL_bit_position) = '1' or 
             decoded_instruction_IE(MULH_bit_position) = '1' then
            tracer_mul_result <= std_logic_vector(signed(RS1_Data_IE)*signed(RS2_Data_IE));
          end if;

          if decoded_instruction_IE(MULHU_bit_position) = '1' then
            tracer_mul_result <= std_logic_vector(unsigned(RS1_Data_IE)*unsigned(RS2_Data_IE));
          end if;

          if decoded_instruction_IE(MULHSU_bit_position) = '1' then
            tracer_mul_result <= x"FFFFFFFF_00000000";
          end if;

          if decoded_instruction_IE(DIV_bit_position)  = '1' or 
             decoded_instruction_IE(REM_bit_position)  = '1' or
             decoded_instruction_IE(DIVU_bit_position) = '1' or 
             decoded_instruction_IE(REMU_bit_position) = '1' then
            rs1_valid <= '1' when rs1(instr_word_IE) /= 0;
            rs2_valid <= '1' when rs2(instr_word_IE) /= 0;
            rd_valid  <= '1' when rd(instr_word_IE)  /= 0;
          end if;

        -- EXECUTE OF INSTRUCTION (END)
        end if;  -- instr_rvalid_IE values 

      when others =>

    end case;  -- fsm_IE state cases

    if LS_instr_req = '1' then
      LSU_instr <= '1';
      case state_LS is  
        when normal =>
          if decoded_instruction_LS(LW_bit_position) = '1' or
             decoded_instruction_LS(LH_bit_position) = '1' or decoded_instruction_LS(LHU_bit_position) = '1' or 
             decoded_instruction_LS(LB_bit_position) = '1' or decoded_instruction_LS(LBU_bit_position) = '1' then
            rs1_valid           <= '1' when rs1(instr_word_IE) /= 0;
            rd_valid            <= '1' when rd(instr_word_IE)  /= 0;
            tracer_result <= std_logic_vector(signed(RS1_Data_IE)+signed(I_immediate(instr_word_IE)));
          end if;

          if decoded_instruction_LS(SW_bit_position) = '1' or
             decoded_instruction_LS(SH_bit_position) = '1' or 
             decoded_instruction_LS(SB_bit_position) = '1' then
            rs1_valid           <= '1' when rs1(instr_word_IE) /= 0;
            rs2_valid           <= '1' when rs2(instr_word_IE) /= 0;
            tracer_result <= std_logic_vector(signed(RS1_Data_IE)+signed(S_immediate(instr_word_IE)));
          end if;

          if decoded_instruction_LS(AMOSWAP_bit_position) = '1' then
            rs1_valid           <= '1' when rs1(instr_word_IE) /= 0;
            rs2_valid           <= '1' when rs2(instr_word_IE) /= 0;
            rd_valid            <= '1' when rd(instr_word_IE)  /= 0;
            tracer_result <= RS1_Data_IE;
          end if;

          if decoded_instruction_LS(KMEMLD_bit_position)   = '1' or
             decoded_instruction_LS(KBCASTLD_bit_position) = '1' then
            rs1_valid           <= '1' when rs1(instr_word_IE) /= 0;
            rs2_valid           <= '1' when rs2(instr_word_IE) /= 0;
            rd_read_only_valid  <= '1' when rd(instr_word_IE)  /= 0;
            tracer_result <= RS1_Data_IE;
          end if;

          if decoded_instruction_LS(KMEMSTR_bit_position) = '1' then
            rs1_valid           <= '1' when rs1(instr_word_IE) /= 0;
            rs2_valid           <= '1' when rs2(instr_word_IE) /= 0;
            rd_read_only_valid  <= '1' when rd(instr_word_IE)  /= 0;
            tracer_result <= RD_Data_IE;
          end if;

        when data_valid_waiting =>
      end case;
    end if;

    for h in accl_range loop
    if dsp_instr_req(h) = '1' then  
      DSP_instr <= '1';
      case state_DSP(h) is
        when dsp_init =>
          if    decoded_instruction_DSP(KADDV_bit_position)    = '1' then
            rs1_valid           <= '1' when rs1(instr_word_IE) /= 0;
            rs2_valid           <= '1' when rs2(instr_word_IE) /= 0;
            rd_read_only_valid  <= '1' when rd(instr_word_IE)  /= 0;
          elsif decoded_instruction_DSP(KSVADDSC_bit_position) = '1' then
            rs1_valid           <= '1' when rs1(instr_word_IE) /= 0;
            rs2_valid           <= '1' when rs2(instr_word_IE) /= 0;
            rd_read_only_valid  <= '1' when rd(instr_word_IE)  /= 0;
          elsif decoded_instruction_DSP(KSVADDRF_bit_position) = '1' then
            rs1_valid           <= '1' when rs1(instr_word_IE) /= 0;
            rs2_valid           <= '1' when rs2(instr_word_IE) /= 0;
            rd_read_only_valid  <= '1' when rd(instr_word_IE)  /= 0;
          elsif decoded_instruction_DSP(KSUBV_bit_position)    = '1' then
            rs1_valid           <= '1' when rs1(instr_word_IE) /= 0;
            rs2_valid           <= '1' when rs2(instr_word_IE) /= 0;
            rd_read_only_valid  <= '1' when rd(instr_word_IE)  /= 0;
          elsif decoded_instruction_DSP(KVMUL_bit_position)    = '1' then
            rs1_valid           <= '1' when rs1(instr_word_IE) /= 0;
            rs2_valid           <= '1' when rs2(instr_word_IE) /= 0;
            rd_read_only_valid  <= '1' when rd(instr_word_IE)  /= 0;
          elsif decoded_instruction_DSP(KSVMULSC_bit_position) = '1' then
            rs1_valid           <= '1' when rs1(instr_word_IE) /= 0;
            rs2_valid           <= '1' when rs2(instr_word_IE) /= 0;
            rd_read_only_valid  <= '1' when rd(instr_word_IE)  /= 0;
          elsif decoded_instruction_DSP(KSVMULRF_bit_position) = '1' then
            rs1_valid           <= '1' when rs1(instr_word_IE) /= 0;
            rs2_valid           <= '1' when rs2(instr_word_IE) /= 0;
            rd_read_only_valid  <= '1' when rd(instr_word_IE)  /= 0;
          elsif decoded_instruction_DSP(KSRAV_bit_position)    = '1' then
            rs1_valid           <= '1' when rs1(instr_word_IE) /= 0;
            rs2_valid           <= '1' when rs2(instr_word_IE) /= 0;
            rd_read_only_valid  <= '1' when rd(instr_word_IE)  /= 0;
          elsif decoded_instruction_DSP(KSRLV_bit_position)    = '1' then
            rs1_valid           <= '1' when rs1(instr_word_IE) /= 0;
            rs2_valid           <= '1' when rs2(instr_word_IE) /= 0;
            rd_read_only_valid  <= '1' when rd(instr_word_IE)  /= 0;
          elsif decoded_instruction_DSP(KVRED_bit_position)    = '1' then
            rs1_valid           <= '1' when rs1(instr_word_IE) /= 0;
            rd_read_only_valid  <= '1' when rd(instr_word_IE)  /= 0;
          elsif decoded_instruction_DSP(KVSLT_bit_position)    = '1' then
            rs1_valid           <= '1' when rs1(instr_word_IE) /= 0;
            rs2_valid           <= '1' when rs2(instr_word_IE) /= 0;
            rd_read_only_valid  <= '1' when rd(instr_word_IE)  /= 0;
          elsif decoded_instruction_DSP(KSVSLT_bit_position)   = '1' then
            rs1_valid           <= '1' when rs1(instr_word_IE) /= 0;
            rs2_valid           <= '1' when rs2(instr_word_IE) /= 0;
            rd_read_only_valid  <= '1' when rd(instr_word_IE)  /= 0;
          elsif decoded_instruction_DSP(KRELU_bit_position)    = '1' then
            rs1_valid           <= '1' when rs1(instr_word_IE) /= 0;
            rd_read_only_valid  <= '1' when rd(instr_word_IE)  /= 0;
          elsif decoded_instruction_DSP(KDOTP_bit_position)    = '1' then
            rs1_valid           <= '1' when rs1(instr_word_IE) /= 0;
            rs2_valid           <= '1' when rs2(instr_word_IE) /= 0;
            rd_read_only_valid  <= '1' when rd(instr_word_IE)  /= 0;
          elsif decoded_instruction_DSP(KDOTPPS_bit_position)  = '1' then
            rs1_valid           <= '1' when rs1(instr_word_IE) /= 0;
            rs2_valid           <= '1' when rs2(instr_word_IE) /= 0;
            rd_read_only_valid  <= '1' when rd(instr_word_IE)  /= 0;
          elsif decoded_instruction_DSP(KBCAST_bit_position)   = '1' then
            rs1_valid           <= '1' when rs1(instr_word_IE) /= 0;
            rd_read_only_valid  <= '1' when rd(instr_word_IE)  /= 0;
          elsif decoded_instruction_DSP(KVCP_bit_position)     = '1' then
            rs1_valid           <= '1' when rs1(instr_word_IE) /= 0;
            rd_read_only_valid  <= '1' when rd(instr_word_IE)  /= 0;
          end if;
        when others =>
      end case;
    end if;
    end loop;
  end process;
  end generate Tracer_generate;

  EXEC_instr <= IE_instr or LSU_instr or DSP_instr;

  Dependency_checker : process(all) -- also implements the delay slot counters and some aux signals
  begin
    rs1_chk_en                  <= '0';
    rs2_chk_en                  <= '0';
    rd_read_only_chk_en         <= '0';
    RAW_wire                    <= RAW;
    Instr_word_buf_wire         <= Instr_word_buf; 
    pc_buf_wire                 <= pc_buf; 
    rs1_valid_buf_wire          <= rs1_valid_buf; 
    rs2_valid_buf_wire          <= rs2_valid_buf; 
    rd_read_only_valid_buf_wire <= rd_read_only_valid_buf;
    rd_valid_buf_wire           <= rd_valid_buf;
    if unsigned(Instr_word_buf(buf_wr_ptr_lat)) /= 0 then  -- check if there is a valid instruction (after first instruction this is always valid)
      if unsigned(PCER(0)) /= 0 then  -- PCER when a perf counter is enabled the RAW dependency checker os also enabled.
        if harc_EXEC_lat = 0 and EXEC_Instr_lat = '1' then  -- the checking only happens for harc_EXEC_lat = 0
        -----------------------------------------------------------------------------------------------
          if rs1_valid_buf(buf_wr_ptr_lat) = '1' then -- if rs1 is valid, then enable rs1_chk
            rs1_chk_en <= '1';  -- enable rs1_chk
          end if;
        -----------------------------------------------------------------------------------------------
          if rs2_valid_buf(buf_wr_ptr_lat) = '1' then -- if rs2(current_instr) is valid
            if rs1_valid_buf(buf_wr_ptr_lat) = '1' then -- if rs2(current_instr) is valid
              if Instr_word_buf(buf_wr_ptr_lat)(24 downto 20) /= Instr_word_buf(buf_wr_ptr_lat)(19 downto 15) then -- rs2 and rs1 operands are different
                rs2_chk_en <= '1'; -- enable if rs1 is valid and not equal to rs2
              end if;
            else  -- if rs2 is valid, and rs1 is not then enable rs2
              rs2_chk_en <= '1';  -- enable rs2_chk if rs1 is not valid
            end if;
          end if;
        -----------------------------------------------------------------------------------------------
          if rd_read_only_valid_buf(buf_wr_ptr_lat) = '1' then -- if rd_read_only(current_instr) is valid
            if rs1_valid_buf(buf_wr_ptr_lat) = '1' then -- if rs2(current_instr) is valid
              if Instr_word_buf(buf_wr_ptr_lat)(11 downto 7) /= Instr_word_buf(buf_wr_ptr_lat)(19 downto 15) then -- rd and rs1 operands are different
                if rs2_valid_buf(buf_wr_ptr_lat) = '1' then
                  if Instr_word_buf(buf_wr_ptr_lat)(11 downto 7) /= Instr_word_buf(buf_wr_ptr_lat)(24 downto 20) then -- rd and rs2 operands are different
                    rd_read_only_chk_en <= '1'; -- enable if rs1, rs2 are valid and they are not equal to rd_read_only
                  end if;
                else
                  rd_read_only_chk_en <= '1'; -- enable if rs1 is valid and it is not equal to rd_read_only
                end if;
              end if;
            elsif rs2_valid_buf(buf_wr_ptr_lat) = '1' then  -- if rs2 is valid, and rs1 is not then enable rs2
              if Instr_word_buf(buf_wr_ptr_lat)(11 downto 7) /= Instr_word_buf(buf_wr_ptr_lat)(24 downto 20) then -- rd and rs2 operands are different
                rd_read_only_chk_en <= '1'; -- enable if rs2 is valid and it is not equal to rd_read_only
              end if;
            else
              rd_read_only_chk_en <= '1'; -- enable rd_read_only_chk if rs1 and rs2 are npt valid
            end if;
          end if;
        -----------------------------------------------------------------------------------------------
        end if;
      end if;
      -----------------------------------------------------------------------------------------------
      if rs1_chk_en = '1' then -- check if the current instruction has rs1 as a valid read operand
        for i in 1 to buf_size-1 loop -- loop through the buffer (starts from i=1, since i=0 is the position of the current instruction)
          if buf_wr_ptr_lat-i >= 0 then -- buffer loop underflow enters here
            if unsigned(Instr_word_buf(buf_wr_ptr_lat-i)) /= 0 then -- if the previous instruction is valid
              if rd_valid_buf(buf_wr_ptr_lat-i) = '1' then  -- if the previous instruction has a valid destination rd
                if  (Instr_word_buf(buf_wr_ptr_lat)(19 downto 15) = Instr_word_buf(buf_wr_ptr_lat-i)(11 downto 7)) then -- if there is a RAW dependency between rs1(current_instr) and rd(previous_instr)
                  Instr_word_buf_wire(buf_wr_ptr_lat-i)         <= (others => '0');
                  pc_buf_wire(buf_wr_ptr_lat-i)                 <= (others => '0');
                  rs1_valid_buf_wire(buf_wr_ptr_lat-i)          <= '0';
                  rs2_valid_buf_wire(buf_wr_ptr_lat-i)          <= '0';
                  rd_read_only_valid_buf_wire(buf_wr_ptr_lat-i) <= '0';
                  rd_valid_buf_wire(buf_wr_ptr_lat-i)           <= '0';
                  RAW_wire(i) <= RAW(i)+1;
                  exit;
                end if;
              end if;
            end if;
          else  -- buffer loop overflow are here
            if unsigned(Instr_word_buf(buf_wr_ptr_lat-i+buf_size)) /= 0 then -- if the previous instruction is valid
              if rd_valid_buf(buf_wr_ptr_lat-i+buf_size) = '1' then  -- if the previous instruction has a valid destination register
                if (Instr_word_buf(buf_wr_ptr_lat)(19 downto 15) = Instr_word_buf(buf_wr_ptr_lat-i+buf_size)(11 downto 7)) then -- if there is a RAW dependency between rs1(current_instr) and rd(previous_instr)
                  Instr_word_buf_wire(buf_wr_ptr_lat-i+buf_size)         <= (others => '0');
                  pc_buf_wire(buf_wr_ptr_lat-i+buf_size)                 <= (others => '0');
                  rs1_valid_buf_wire(buf_wr_ptr_lat-i+buf_size)          <= '0';
                  rs2_valid_buf_wire(buf_wr_ptr_lat-i+buf_size)          <= '0';
                  rd_read_only_valid_buf_wire(buf_wr_ptr_lat-i+buf_size) <= '0';
                  rd_valid_buf_wire(buf_wr_ptr_lat-i+buf_size)           <= '0';
                  RAW_wire(i) <= RAW(i)+1;
                  exit;
                end if;
              end if;
            end if;
          end if;
        end loop;
      end if;
      if rs2_chk_en = '1' then -- check if the current instruction has rs2 as a valid read operand
        for i in 1 to buf_size-1 loop -- loop through the buffer (starts from i=1, since i=0 is the position of the current instruction)
          if buf_wr_ptr_lat-i >= 0 then -- buffer loop underflow enters here
            if unsigned(Instr_word_buf(buf_wr_ptr_lat-i)) /= 0 then -- if the previous instruction is valid
              if rd_valid_buf(buf_wr_ptr_lat-i) = '1' then  -- if the previous instruction has a valid destination rd
                if  (Instr_word_buf(buf_wr_ptr_lat)(24 downto 20) = Instr_word_buf(buf_wr_ptr_lat-i)(11 downto 7))  then   -- if there is a RAW dependency between rs2(current_instr) and rd(previous_instr)
                  Instr_word_buf_wire(buf_wr_ptr_lat-i)         <= (others => '0');
                  pc_buf_wire(buf_wr_ptr_lat-i)                 <= (others => '0');
                  rs1_valid_buf_wire(buf_wr_ptr_lat-i)          <= '0';
                  rs2_valid_buf_wire(buf_wr_ptr_lat-i)          <= '0';
                  rd_read_only_valid_buf_wire(buf_wr_ptr_lat-i) <= '0';
                  rd_valid_buf_wire(buf_wr_ptr_lat-i)           <= '0';
                  RAW_wire(i) <= RAW(i)+1;
                  exit;
                end if;
              end if;
            end if;
          else  -- buffer loop overflow are here
            if unsigned(Instr_word_buf(buf_wr_ptr_lat-i+buf_size)) /= 0 then -- if the previous instruction is valid
              if rd_valid_buf(buf_wr_ptr_lat-i+buf_size) = '1' then  -- if the previous instruction has a valid destination register
                if (Instr_word_buf(buf_wr_ptr_lat)(24 downto 20) = Instr_word_buf(buf_wr_ptr_lat-i+buf_size)(11 downto 7)) then   -- if there is a RAW dependency between rs2(current_instr) and rd(previous_instr)
                  Instr_word_buf_wire(buf_wr_ptr_lat-i+buf_size)         <= (others => '0');
                  pc_buf_wire(buf_wr_ptr_lat-i+buf_size)                 <= (others => '0');
                  rs1_valid_buf_wire(buf_wr_ptr_lat-i+buf_size)          <= '0';
                  rs2_valid_buf_wire(buf_wr_ptr_lat-i+buf_size)          <= '0';
                  rd_read_only_valid_buf_wire(buf_wr_ptr_lat-i+buf_size) <= '0';
                  rd_valid_buf_wire(buf_wr_ptr_lat-i+buf_size)           <= '0';
                  RAW_wire(i) <= RAW(i)+1;
                  exit;
                end if;
              end if;
            end if;
          end if;
        end loop;
      end if;
      if rd_read_only_chk_en = '1' then -- check if the current instruction has rs1 as a valid read operand
        for i in 1 to buf_size-1 loop -- loop through the buffer (starts from i=1, since i=0 is the position of the current instruction)
          if buf_wr_ptr_lat-i >= 0 then -- buffer loop underflow enters here
            if unsigned(Instr_word_buf(buf_wr_ptr_lat-i)) /= 0 then -- if the previous instruction is valid
              if rd_valid_buf(buf_wr_ptr_lat-i) = '1' then  -- if the previous instruction has a valid destination rd
                if  (Instr_word_buf(buf_wr_ptr_lat)(11 downto 7) = Instr_word_buf(buf_wr_ptr_lat-i)(11 downto 7))  then   -- if there is a RAW dependency between rd_read_only(current_instr) and rd(previous_instr)
                  Instr_word_buf_wire(buf_wr_ptr_lat-i)         <= (others => '0');
                  pc_buf_wire(buf_wr_ptr_lat-i)                 <= (others => '0');
                  rs1_valid_buf_wire(buf_wr_ptr_lat-i)          <= '0';
                  rs2_valid_buf_wire(buf_wr_ptr_lat-i)          <= '0';
                  rd_read_only_valid_buf_wire(buf_wr_ptr_lat-i) <= '0';
                  rd_valid_buf_wire(buf_wr_ptr_lat-i)           <= '0';
                  RAW_wire(i) <= RAW(i)+1;
                  exit;
                end if;
              end if;
            end if;
          else  -- buffer loop overflow are here
            if unsigned(Instr_word_buf(buf_wr_ptr_lat-i+buf_size)) /= 0 then -- if the previous instruction is valid
              if rd_valid_buf(buf_wr_ptr_lat-i+buf_size) = '1' then  -- if the previous instruction has a valid destination register
                if (Instr_word_buf(buf_wr_ptr_lat)(11 downto 7) = Instr_word_buf(buf_wr_ptr_lat-i+buf_size)(11 downto 7)) then   -- if there is a RAW dependency between rd_read_only(current_instr) and rd(previous_instr)
                  Instr_word_buf_wire(buf_wr_ptr_lat-i+buf_size)         <= (others => '0');
                  pc_buf_wire(buf_wr_ptr_lat-i+buf_size)                 <= (others => '0');
                  rs1_valid_buf_wire(buf_wr_ptr_lat-i+buf_size)          <= '0';
                  rs2_valid_buf_wire(buf_wr_ptr_lat-i+buf_size)          <= '0';
                  rd_read_only_valid_buf_wire(buf_wr_ptr_lat-i+buf_size) <= '0';
                  rd_valid_buf_wire(buf_wr_ptr_lat-i+buf_size)           <= '0';
                  RAW_wire(i) <= RAW(i)+1;
                  exit;
                end if;
              end if;
            end if;
          end if;
        end loop;
      end if;
      -----------------------------------------------------------------------------------------------
    end if;
  end process;

  Dependency_buffer : process(clk_i, rst_ni) -- also implements the delay slot counters and some aux signals
  begin
    if rst_ni = '0' then
      Instr_word_buf         <= (others => (others => '0'));
      pc_buf                 <= (others => (others => '0'));
      rs1_valid_buf          <= (others => '0');
      rs2_valid_buf          <= (others => '0');
      rd_read_only_valid_buf <= (others => '0');
      rd_valid_buf           <= (others => '0');
      RAW                    <= (others =>  0);
      core_busy_IE_lat       <= '0';
      EXEC_Instr_lat         <= '0';
      buf_wr_ptr             <=  0;
      buf_wr_ptr_lat         <=  0;
      harc_EXEC_lat          <=  0;
    elsif rising_edge(clk_i) then
      core_busy_IE_lat       <= core_busy_IE;
      Instr_word_buf         <= Instr_word_buf_wire;
      pc_buf                 <= pc_buf_wire;
      rs1_valid_buf          <= rs1_valid_buf_wire;
      rs2_valid_buf          <= rs2_valid_buf_wire;
      rd_read_only_valid_buf <= rd_read_only_valid_buf_wire;
      rd_valid_buf           <= rd_valid_buf_wire;
      RAW                    <= RAW_wire;
      buf_wr_ptr_lat         <= buf_wr_ptr;
      EXEC_Instr_lat         <= EXEC_Instr;
      harc_EXEC_lat          <= harc_EXEC;
      if EXEC_instr = '1' then
        if harc_EXEC = 0 then
          rs1_valid_buf(buf_wr_ptr)          <= rs1_valid;
          rs2_valid_buf(buf_wr_ptr)          <= rs2_valid;
          rd_read_only_valid_buf(buf_wr_ptr) <= rd_read_only_valid;
          rd_valid_buf(buf_wr_ptr)           <= rd_valid;
          Instr_word_buf(buf_wr_ptr)         <= Instr_word_IE;
          pc_buf(buf_wr_ptr)                 <= pc_IE;
          if buf_wr_ptr < buf_size -1 then
            buf_wr_ptr  <= buf_wr_ptr+1;
          else
            buf_wr_ptr  <= 0;
          end if; 
        end if;
      end if;
    end if;
  end process;
  -- pragma translate_on

--------------------------------------------------------------------- end of PIPE -----------------
---------------------------------------------------------------------------------------------------

end Pipe;
--------------------------------------------------------------------------------------------------
-- END of Processing-Pipeline architecture -------------------------------------------------------
--------------------------------------------------------------------------------------------------