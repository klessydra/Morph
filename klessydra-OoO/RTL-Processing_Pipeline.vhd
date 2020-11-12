--------------------------------------------------------------------------------------------------------------
--  Processing Pipeline --                                                                                  --
--  Author(s): Abdallah Cheikh abdallah.cheikh@uniroma1.it (abdallah93.as@gmail.com)                        --
--                                                                                                          --
--  Date Modified: 07-04-2020                                                                                --
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
    THREAD_POOL_SIZE           : integer;
    LUTRAM_RF                  : natural;
    RV32E                      : natural;
    RV32M                      : natural;
    superscalar_exec_en        : natural;
    accl_en                    : natural;
    replicate_accl_en          : natural;
    multithreaded_accl_en      : natural;
    SPM_NUM	                   : natural;  
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
    Logical_RF_SIZE            : natural;
    Logical_RF_CEIL            : natural;
    TPS_CEIL                   : natural;
    TPS_BUF_CEIL               : natural;
    SPM_ADDR_WID               : natural;
    SIMD_BITS                  : natural;
    Data_Width                 : natural;
    SIMD_Width                 : natural;
    XLEN                       : natural;
    MUL_DIV_BUF_SIZE           : natural;
    IE_BUF_SIZE                : natural;
    LSU_BUF_SIZE               : natural;
    DSP_BUF_SIZE               : natural
    );
  port (
    pc_IF                      : in  std_logic_vector(31 downto 0);
    harc_IF                    : in  integer range THREAD_POOL_SIZE-1 downto 0;
    irq_pending                : in  std_logic;
    csr_instr_done             : in  std_logic;
    csr_access_denied_o        : in  std_logic;
    csr_rdata_o                : in  std_logic_vector (31 downto 0);
    dbg_req_o                  : in  std_logic;
    MVSIZE                     : in  array_2d(THREAD_POOL_SIZE-1 downto 0)(Addr_Width downto 0);
    MVTYPE                     : in  array_2d(THREAD_POOL_SIZE-1 downto 0)(3 downto 0);
    MPSCLFAC                   : in  array_2d(THREAD_POOL_SIZE-1 downto 0)(4 downto 0);
    MSTATUS                    : in  array_2d(THREAD_POOL_SIZE-1 downto 0)(1 downto 0);
    served_irq     	           : out std_logic;
    WFI_Instr		               : out std_logic;
    misaligned_err             : out std_logic;
    pc_ID                      : out std_logic_vector(31 downto 0);
    pc_IE                      : out std_logic_vector(31 downto 0);
    ie_except_data             : out std_logic_vector(31 downto 0);
    ls_except_data             : out std_logic_vector(31 downto 0);
    dsp_except_data            : out array_2d(ACCL_NUM-1 downto 0)(31 downto 0);
    id_taken_branch            : out std_logic;
    taken_branch               : out std_logic;
    ie_taken_branch            : out std_logic;
    ls_taken_branch            : out std_logic;
    set_branch_condition_ID    : out std_logic;
    dsp_taken_branch           : out std_logic_vector(ACCL_NUM-1 downto 0);
    set_branch_condition       : out std_logic;
    ie_except_condition        : out std_logic;
    ls_except_condition        : out std_logic;
    dsp_except_condition       : out std_logic_vector(ACCL_NUM-1 downto 0);
    set_mret_condition         : out std_logic;
    set_wfi_condition          : out std_logic;
    csr_instr_req              : out std_logic;
    instr_rvalid_IE            : out std_logic;  -- validity bit at IE input
    PC_offset_ID               : out std_logic_vector(31 downto 0);
    csr_addr_i                 : out std_logic_vector(11 downto 0);
    csr_wdata_i                : out std_logic_vector(31 downto 0);
    csr_op_i                   : out std_logic_vector(2 downto 0);
    jump_instr                 : out std_logic;
    jump_instr_lat             : out std_logic;
    branch_instr               : out std_logic;
    branch_instr_lat           : out std_logic;
    instr_word_IE              : out std_logic_vector(31 downto 0);
    PC_offset                  : out std_logic_vector(31 downto 0);
    dbg_ack_i                  : out std_logic;
    ebreak_instr               : out std_logic;
    data_addr_internal         : out std_logic_vector(31 downto 0);
    absolute_jump              : out std_logic;
    --regfile                    : out array_3d(THREAD_POOL_SIZE-1 downto 0)(RF_SIZE-1 downto 0)(31 downto 0);
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
    core_busy_o                : out std_logic
    );
end entity;  ------------------------------------------


architecture Pipe of Pipeline is

  constant MUL_DIV_buf_width : natural := 1+1+1+MUL_DIV_UNIT_INSTR_SET_SIZE+3+3*RF_CEIL;
  constant IE_buf_width      : natural := 1+1+1+2*XLEN+EXEC_UNIT_INSTR_SET_SIZE+3+3*RF_CEIL;
  constant LSU_buf_width     : natural := 1+1+8+XLEN+LS_UNIT_INSTR_SET_SIZE+4+4*RF_CEIL;
  constant DSP_buf_width     : natural := 1+1+7+DSP_UNIT_INSTR_SET_SIZE+3+3*RF_CEIL;

  subtype harc_range is integer range THREAD_POOL_SIZE - 1 downto 0;
  subtype accl_range is integer range ACCL_NUM - 1 downto 0; 
  subtype fu_range   is integer range FU_NUM - 1 downto 0;

  signal state_IE               : fsm_IE_states;
  signal state_LS               : fsm_LS_states;
  signal state_DSP              : array_2d(accl_range)(1 downto 0);
  
  signal sleep_state            : std_logic;
  signal ls_sci_wr_gnt          : std_logic;
  signal dsp_sci_wr_gnt         : std_logic_vector(accl_range);
  signal ls_data_gnt_i          : std_logic_vector(SPM_NUM-1 downto 0);
  signal dsp_data_gnt_i         : std_logic_vector(accl_range);
  signal ls_instr_done          : std_logic;
  signal csr_wdata_en           : std_logic;
  signal ie_to_csr              : std_logic_vector(31 downto 0);
  signal ie_csr_wdata_i         : std_logic_vector(31 downto 0);
  signal data_addr_internal_IE  : std_logic_vector(31 downto 0);
  signal data_width_ID          : std_logic_vector(1 downto 0);


  signal halt_IE       : std_logic;
  signal halt_LSU      : std_logic;

  signal kmemld_inflight      : std_logic_vector(SPM_NUM-1 downto 0);
  signal kmemstr_inflight     : std_logic_vector(SPM_NUM-1 downto 0);
  signal sc_word_count_wire   : integer;

  -- instruction register and instr. propagation registers --
  signal instr_word_ID_lat       : std_logic_vector(31 downto 0);  -- latch needed for long-latency program memory
  signal instr_rvalid_ID         : std_logic;  -- validity bit at ID input

  signal amo_load_skip : std_logic;
  signal amo_load      : std_logic;
  signal amo_store     : std_logic;
  --signal sw_mip        : std_logic;

  -- DSP Unit Signals
  signal ls_sc_data_write_wire  : std_logic_vector(Data_Width-1 downto 0);
  signal ls_sc_data_read_wire   : std_logic_vector(Data_Width-1 downto 0);
  signal dsp_sc_data_read       : array_3d(accl_range)(1 downto 0)(SIMD_Width-1 downto 0);
  signal dsp_sc_read_addr       : array_3d(accl_range)(1 downto 0)(Addr_Width-1 downto 0);
  signal dsp_to_sc              : array_3d(accl_range)(SPM_NUM-1 downto 0)(1 downto 0);
  signal dsp_sc_write_addr      : array_2d(accl_range)(Addr_Width-1 downto 0);
  signal dsp_sc_data_write_wire : array_2d(accl_range)(SIMD_Width - 1 downto 0);
  signal dsp_instr_req          : std_logic_vector(accl_range);
  signal ls_sc_read_addr        : std_logic_vector(Addr_Width-(SIMD_BITS+3) downto 0);
  signal ls_sc_write_addr       : std_logic_vector(Addr_Width-(SIMD_BITS+3) downto 0);
  signal ls_sci_req             : std_logic_vector(SPM_NUM-1 downto 0);
  signal ls_sci_we              : std_logic_vector(SPM_NUM-1 downto 0);
  signal dsp_sci_req            : array_2d(accl_range)(SPM_NUM-1 downto 0);
  signal dsp_sci_we             : array_2d(accl_range)(SPM_NUM-1 downto 0);
  signal spm_rs1                : std_logic;
  signal spm_rs2                : std_logic;
  signal vec_read_rs1_ID        : std_logic;
  signal vec_read_rs2_ID        : std_logic;
  signal vec_write_rd_ID        : std_logic;
  signal dsp_we_word            : array_2d(accl_range)(SIMD-1 downto 0);

  signal busy_mul_div           : std_logic;
  signal busy_ie                : std_logic;
  signal busy_lsu               : std_logic;
  signal busy_dsp               : std_logic;


  signal halt_IF                      : std_logic;
  signal instr_word_RENAME            : std_logic_vector(31 downto 0);
  signal pc_RENAME                    : std_logic_vector(31 downto 0);
  signal decoded_instr_RENAME_MUL_DIV : std_logic_vector(MUL_DIV_UNIT_INSTR_SET_SIZE-1 downto 0);
  signal decoded_instr_RENAME_IE      : std_logic_vector(EXEC_UNIT_INSTR_SET_SIZE-1 downto 0);
  signal decoded_instr_RENAME_LSU     : std_logic_vector(LS_UNIT_INSTR_SET_SIZE-1 downto 0);
  signal decoded_instr_RENAME_DSP     : std_logic_vector(DSP_UNIT_INSTR_SET_SIZE-1 downto 0);
  signal instr_valid_RENAME           : std_logic;
  signal MUL_DIV_instr_RENAME         : std_logic;
  signal IE_instr_RENAME              : std_logic;
  signal LSU_instr_RENAME             : std_logic;
  signal DSP_instr_RENAME             : std_logic;
  signal rs1_valid_RENAME             : std_logic;
  signal rs2_valid_RENAME             : std_logic;
  signal rd_valid_RENAME              : std_logic;
  signal rd_read_only_RENAME          : std_logic;
  signal halt_ID                      : std_logic;
  signal signed_op_RENAME             : std_logic;
  signal comparator_en_RENAME         : std_logic;
  signal load_op_RENAME               : std_logic;
  signal store_op_RENAME              : std_logic;
  signal data_width_RENAME            : std_logic_vector(1 downto 0);
  signal data_be_RENAME               : std_logic_vector(3 downto 0);
  signal spm_rs1_RENAME               : std_logic;
  signal spm_rs2_RENAME               : std_logic;
  signal vec_read_rs1_RENAME          : std_logic;
  signal vec_read_rs2_RENAME          : std_logic;
  signal vec_write_rd_RENAME          : std_logic;
  signal vec_width_RENAME             : std_logic_vector(1 downto 0);
  signal signed_op_ISSUE              : std_logic;
  signal comparator_en_ISSUE          : std_logic;
  signal load_op_ISSUE                : std_logic;
  signal store_op_ISSUE               : std_logic;
  signal data_width_ISSUE             : std_logic_vector(1 downto 0);
  signal data_be_ISSUE                : std_logic_vector(3 downto 0);
  signal spm_rs1_ISSUE                : std_logic;
  signal spm_rs2_ISSUE                : std_logic;
  signal vec_read_rs1_ISSUE           : std_logic;
  signal vec_read_rs2_ISSUE           : std_logic;
  signal vec_write_rd_ISSUE           : std_logic;
  signal vec_width_ISSUE              : std_logic_vector(1 downto 0);
  signal instr_valid_ISSUE            : std_logic;
  signal MUL_DIV_instr_ISSUE          : std_logic;
  signal IE_instr_ISSUE               : std_logic;
  signal LSU_instr_ISSUE              : std_logic;
  signal DSP_instr_ISSUE              : std_logic;
  signal rs1_valid_ISSUE              : std_logic;
  signal rs2_valid_ISSUE              : std_logic;
  signal old_rd_valid_ISSUE           : std_logic;
  signal new_rd_valid_ISSUE           : std_logic;
  signal rd_read_only_valid_ISSUE     : std_logic;
  signal rs1_ISSUE                    : std_logic_vector(RF_CEIL-1 downto 0);
  signal rs2_ISSUE                    : std_logic_vector(RF_CEIL-1 downto 0);
  signal new_rd_ISSUE                 : std_logic_vector(RF_CEIL-1 downto 0);
  signal rd_read_only_ISSUE           : std_logic_vector(RF_CEIL-1 downto 0);
  signal decoded_instr_ISSUE_MUL_DIV  : std_logic_vector(MUL_DIV_UNIT_INSTR_SET_SIZE-1 downto 0);
  signal decoded_instr_ISSUE_IE       : std_logic_vector(EXEC_UNIT_INSTR_SET_SIZE-1 downto 0);
  signal decoded_instr_ISSUE_LSU      : std_logic_vector(LS_UNIT_INSTR_SET_SIZE-1 downto 0);
  signal decoded_instr_ISSUE_DSP      : std_logic_vector(DSP_UNIT_INSTR_SET_SIZE-1 downto 0);
  signal pc_ISSUE                     : std_logic_vector(31 downto 0);
  signal instr_word_ISSUE             : std_logic_vector(31 downto 0);
  signal MUL_DIV_buff_full            : std_logic;
  signal IE_buff_full                 : std_logic;
  signal LSU_buff_full                : std_logic;
  signal DSP_buff_full                : std_logic;
  signal FRL_commit_addr              : array_2D(11 downto 0)(RF_CEIL-1 downto 0);
  signal FRL_commit_en                : std_logic_vector(11 downto 0);
  signal commit_count                 : std_logic_vector(3 downto 0);
  signal old_rd_count                 : std_logic_vector(1 downto 0);
  signal instr_valid_RENAME_CC        : std_logic;
  signal rs1_rename_valid_CC          : std_logic;
  signal rs2_rename_valid_CC          : std_logic;
  signal rd_read_only_rename_valid_CC : std_logic;
  signal old_rd_rename_valid_CC       : std_logic;
  signal new_rd_rename_valid_CC       : std_logic;

  signal decoded_instr_RF_MUL_DIV    : std_logic_vector(MUL_DIV_UNIT_INSTR_SET_SIZE-1 downto 0);
  signal decoded_instr_RF_IE         : std_logic_vector(EXEC_UNIT_INSTR_SET_SIZE-1    downto 0);
  signal decoded_instr_RF_LSU        : std_logic_vector(LS_UNIT_INSTR_SET_SIZE-1      downto 0);
  signal decoded_instr_RF_DSP        : std_logic_vector(DSP_UNIT_INSTR_SET_SIZE-1     downto 0);
  signal instr_valid_RF              : std_logic;
  signal MUL_DIV_instr_RF            : std_logic;
  signal IE_instr_RF                 : std_logic;
  signal LSU_instr_RF                : std_logic;
  signal DSP_instr_RF                : std_logic;
  signal mul_div_instr_word_RF       : std_logic_vector(MUL_DIV_buf_width-1 downto 0); 
  signal ie_instr_word_RF            : std_logic_vector(IE_buf_width -1 downto 0); 
  signal lsu_instr_word_RF           : std_logic_vector(LSU_buf_width-1 downto 0); 
  signal dsp_instr_word_RF           : std_logic_vector(DSP_buf_width-1 downto 0); 
  signal MUL_DIV_WB_EN               : std_logic;
  signal MUL_DIV_WB_RD_ADDR          : std_logic_vector(RF_CEIL-1 downto 0);
  signal IE_WB_EN                    : std_logic;
  signal LSU_WB_EN                   : std_logic;
  signal IE_WB_RD_ADDR               : std_logic_vector(RF_CEIL-1 downto 0);
  signal LSU_WB_RD_ADDR              : std_logic_vector(RF_CEIL-1 downto 0);
  signal speculation_ISSUE           : std_logic;
  signal branch_hit                  : std_logic;
  signal branch_miss                 : std_logic;
  signal mul_div_ready_RF            : std_logic;
  signal ie_ready_RF                 : std_logic;
  signal lsu_ready_RF                : std_logic;
  signal dsp_ready_RF                : std_logic;
  signal decoded_instruction_MUL_DIV : std_logic_vector(MUL_DIV_UNIT_INSTR_SET_SIZE-1 downto 0);
  signal decoded_instruction_IE      : std_logic_vector(EXEC_UNIT_INSTR_SET_SIZE -1   downto 0);
  signal decoded_instruction_LSU     : std_logic_vector(LS_UNIT_INSTR_SET_SIZE -1     downto 0);
  signal decoded_instruction_DSP     : std_logic_vector(DSP_UNIT_INSTR_SET_SIZE -1    downto 0);
  signal MUL_DIV_instr               : std_logic;
  signal IE_instr                    : std_logic;
  signal LSU_instr                   : std_logic;
  signal DSP_instr                   : std_logic;
  signal mul_div_data_rs1            : std_logic_vector(31 downto 0);
  signal mul_div_data_rs2            : std_logic_vector(31 downto 0);
  signal mul_div_data_old_rd         : std_logic_vector(31 downto 0);
  signal ie_data_rs1                 : std_logic_vector(31 downto 0);
  signal ie_data_rs2                 : std_logic_vector(31 downto 0);
  signal ie_data_old_rd              : std_logic_vector(31 downto 0);
  signal lsu_data_rs1                : std_logic_vector(31 downto 0);
  signal lsu_data_rs2                : std_logic_vector(31 downto 0);
  signal lsu_data_old_rd             : std_logic_vector(31 downto 0);
  signal dsp_data_rs1                : std_logic_vector(31 downto 0);
  signal dsp_data_rs2                : std_logic_vector(31 downto 0);
  signal dsp_data_rd                 : std_logic_vector(31 downto 0);
  signal mul_div_rs1_valid           : std_logic;
  signal mul_div_rs2_valid           : std_logic;
  signal mul_div_rd_valid            : std_logic;
  signal ie_rs1_valid                : std_logic;
  signal ie_rs2_valid                : std_logic;
  signal ie_rd_valid                 : std_logic;
  signal lsu_rs1_valid               : std_logic;
  signal lsu_rs2_valid               : std_logic;
  signal lsu_rd_valid                : std_logic;
  signal dsp_rs1_valid               : std_logic;
  signal dsp_rs2_valid               : std_logic;
  signal dsp_rd_valid                : std_logic;
  signal mul_div_addr_rs1            : std_logic_vector(RF_CEIL-1 downto 0);
  signal mul_div_addr_rs2            : std_logic_vector(RF_CEIL-1 downto 0);
  signal mul_div_addr_new_rd         : std_logic_vector(RF_CEIL-1 downto 0);
  signal ie_addr_rs1                 : std_logic_vector(RF_CEIL-1 downto 0);
  signal ie_addr_rs2                 : std_logic_vector(RF_CEIL-1 downto 0);
  signal ie_addr_new_rd              : std_logic_vector(RF_CEIL-1 downto 0);
  signal lsu_addr_rs1                : std_logic_vector(RF_CEIL-1 downto 0);
  signal lsu_addr_rs2                : std_logic_vector(RF_CEIL-1 downto 0);
  signal lsu_addr_new_rd             : std_logic_vector(RF_CEIL-1 downto 0);
  signal dsp_addr_rs1                : std_logic_vector(RF_CEIL-1 downto 0);
  signal dsp_addr_rs2                : std_logic_vector(RF_CEIL-1 downto 0);
  signal dsp_addr_rd                 : std_logic_vector(RF_CEIL-1 downto 0);
  signal signed_op                   : std_logic;
  signal comparator_en               : std_logic;
  signal load_op                     : std_logic;
  signal store_op                    : std_logic;
  signal data_be                     : std_logic_vector(3 downto 0);
  signal rs1_to_sc                   : std_logic_vector(SPM_ADDR_WID-1 downto 0);
  signal rs2_to_sc                   : std_logic_vector(SPM_ADDR_WID-1 downto 0);
  signal rd_to_sc                    : std_logic_vector(SPM_ADDR_WID-1 downto 0);
  signal MUL_DIV_WB                  : std_logic_vector(31 downto 0);
  signal IE_WB                       : std_logic_vector(31 downto 0);
  signal LSU_WB                      : std_logic_vector(31 downto 0);
  signal instr_valid_RF_CC           : std_logic_vector(3 downto 0);
  signal mul_div_valid_RF_CC         : std_logic_vector(2 downto 0);
  signal ie_valid_RF_CC              : std_logic_vector(2 downto 0);
  signal lsu_valid_RF_CC             : std_logic_vector(2 downto 0);
  signal dsp_valid_RF_CC             : std_logic_vector(2 downto 0);
  signal old_rd_valid_CC             : std_logic_vector(2 downto 0);-- contains the valids of the old_rds
  signal old_rd_addr_CC              : array_2D(2 downto 0)(RF_CEIL-1 downto 0);
  signal mul_div_ptr_RF_CC           : array_2D(2 downto 0)(RF_CEIL-1 downto 0);
  signal ie_ptr_RF_CC                : array_2D(2 downto 0)(RF_CEIL-1 downto 0);
  signal lsu_ptr_RF_CC               : array_2D(2 downto 0)(RF_CEIL-1 downto 0);
  signal dsp_ptr_RF_CC               : array_2D(2 downto 0)(RF_CEIL-1 downto 0);
  signal pc_mul_div                  : std_logic_vector(31 downto 0); 
  signal mul_div_old_rd              : std_logic_vector(31 downto 0);
  signal mul_div_old_rd_valid        : std_logic;
  signal mul_div_old_rd_valid_CC     : std_logic;
  signal pc_mul_div_WB               : std_logic_vector(31 downto 0);
  signal mul_div_old_rd_CC           : std_logic_vector(31 downto 0);
  signal pc_WB                       : std_logic_vector(31 downto 0);
  signal instr_word_LSU              : std_logic_vector(31 downto 0);
  signal harc_LS_wire                : integer;
  signal spm_bcast                   : std_logic;
  signal ie_old_rd_valid_CC          : std_logic;
  signal lsu_old_rd_valid_CC         : std_logic;
  signal ie_old_rd_CC                : std_logic_vector(31 downto 0);
  signal lsu_old_rd_CC               : std_logic_vector(31 downto 0);
  signal mul_div_addr_CC             : std_logic_vector(RF_CEIL-1 downto 0);
  signal ie_rob_addr_CC              : std_logic_vector(RF_CEIL-1 downto 0);
  signal lsu_rob_addr_CC             : std_logic_vector(RF_CEIL-1 downto 0);
  signal rs1_rename_addr_CC          : std_logic_vector(RF_CEIL-1 downto 0);
  signal rs2_rename_addr_CC          : std_logic_vector(RF_CEIL-1 downto 0);
  signal old_rd_rename_addr_CC       : std_logic_vector(RF_CEIL-1 downto 0);
  signal new_rd_rename_addr_CC       : std_logic_vector(RF_CEIL-1 downto 0);

  -- instruction operands
  signal RS1_Data_IE        : std_logic_vector(31 downto 0);
  signal RS2_Data_IE        : std_logic_vector(31 downto 0);
  signal RD_Data_IE         : std_logic_vector(31 downto 0);  -- unused

  signal tracer_result      : std_logic_vector(31 downto 0);

  function rs1 (signal instr : in std_logic_vector(31 downto 0)) return integer is
  begin
    return to_integer(unsigned(instr(15+(Logical_RF_CEIL-1) downto 15)));
  end;

  function rs2 (signal instr : in std_logic_vector(31 downto 0)) return integer is
  begin
    return to_integer(unsigned(instr(20+(Logical_RF_CEIL-1) downto 20)));
  end;

  function rd (signal instr : in std_logic_vector(31 downto 0)) return integer is
  begin
    return to_integer(unsigned(instr(7+(Logical_RF_CEIL-1) downto 7)));
  end;

  component IF_STAGE is
  port(
    pc_IF                      : in  std_logic_vector(31 downto 0);
    instr_rvalid_i             : in  std_logic;  
    pc_ID                      : out std_logic_vector(31 downto 0);  -- pc_ID is PC entering ID stage
    instr_rvalid_ID            : out std_logic; 
    instr_word_ID_lat          : out std_logic_vector(31 downto 0);
    --halt signals
    halt_IF                    : in  std_logic;
    -- clock, reset active low
    clk_i                      : in  std_logic;
    rst_ni                     : in  std_logic;
    -- program memory interface
    fetch_enable_i             : in  std_logic;
    instr_req_o                : out std_logic;
    instr_gnt_i                : in  std_logic;
    instr_rdata_i              : in  std_logic_vector(31 downto 0)
    );
  end component; ------------------------------------------

  component ID_STAGE is
  generic(
    RV32M                        : natural;
    accl_en                      : natural;
    RF_CEIL                      : natural;
    Logical_RF_CEIL              : natural
    );
  port (
    -- clock, reset active low
    clk_i                        : in  std_logic;
    rst_ni                       : in  std_logic;
    -- OD Signals
    pc_ID                        : in  std_logic_vector(31 downto 0);  -- pc_ID is PC entering ID stage
    instr_rvalid_ID              : in  std_logic;
    -- Instruction Related Signals
    instr_word_ID_lat            : in  std_logic_vector(31 downto 0);
    instr_word_RENAME            : out std_logic_vector(31 downto 0);
    pc_RENAME                    : out std_logic_vector(31 downto 0);
    decoded_instr_RENAME_MUL_DIV : out std_logic_vector(MUL_DIV_UNIT_INSTR_SET_SIZE-1 downto 0);
    decoded_instr_RENAME_IE      : out std_logic_vector(EXEC_UNIT_INSTR_SET_SIZE-1 downto 0);
    decoded_instr_RENAME_LSU     : out std_logic_vector(LS_UNIT_INSTR_SET_SIZE-1 downto 0);
    decoded_instr_RENAME_DSP     : out std_logic_vector(DSP_UNIT_INSTR_SET_SIZE-1 downto 0);
    instr_valid_RENAME           : out std_logic;
    MUL_DIV_instr_RENAME         : out std_logic;
    IE_instr_RENAME              : out std_logic;
    LSU_instr_RENAME             : out std_logic;
    DSP_instr_RENAME             : out std_logic;
    rs1_valid_RENAME             : out std_logic;
    rs2_valid_RENAME             : out std_logic;
    rd_valid_RENAME              : out std_logic;
    rd_read_only_RENAME          : out std_logic;
    -- halt signals
    halt_ID                      : in  std_logic;
    halt_IF                      : out std_logic;
    -- Jump Signals
    absolute_jump                : in  std_logic;
    branch_instr                 : in  std_logic;
    set_branch_condition_ID      : out std_logic;
    id_taken_branch              : out std_logic;
    PC_offset_ID                 : out std_logic_vector(31 downto 0);
    -- MUL_DIV Signals
    signed_op_RENAME             : out std_logic;
    -- IE Signals 
    comparator_en_RENAME         : out std_logic;
    -- LSU Signals
    load_op_RENAME               : out std_logic;
    store_op_RENAME              : out std_logic;
    data_width_RENAME            : out std_logic_vector(1 downto 0);
    data_be_RENAME               : out std_logic_vector(3 downto 0);
    -- DSP Unit Signals
    spm_rs1_RENAME               : out std_logic;
    spm_rs2_RENAME               : out std_logic;
    vec_read_rs1_RENAME          : out std_logic;
    vec_read_rs2_RENAME          : out std_logic;
    vec_write_rd_RENAME          : out std_logic;
    vec_width_RENAME             : out std_logic_vector(1 downto 0)
    );
  end component; ------------------------------------------

  component REG_RENAMING is
  generic(
    RF_SIZE                      : natural;
    RF_CEIL                      : natural;
    Logical_RF_SIZE              : natural;
    Logical_RF_CEIL              : natural
    );
  port (
    clk_i                        : in  std_logic;
    rst_ni                       : in  std_logic;
    -- Input from the Decoder
    pc_RENAME                    : in  std_logic_vector(31 downto 0);
    instr_word_RENAME            : in  std_logic_vector(31 downto 0); -- contains the instruction word pushed from the previous stage --AAA truncate bits 6 downto 0 inorder to have a smaller area footprint for synthesis
    decoded_instr_RENAME_MUL_DIV : in  std_logic_vector(MUL_DIV_UNIT_INSTR_SET_SIZE-1 downto 0); -- contains the valid bit for the instruction encoded in a one-hot decodeing format
    decoded_instr_RENAME_IE      : in  std_logic_vector(EXEC_UNIT_INSTR_SET_SIZE-1 downto 0); -- contains the valid bit for the instruction encoded in a one-hot decodeing format
    decoded_instr_RENAME_LSU     : in  std_logic_vector(LS_UNIT_INSTR_SET_SIZE-1 downto 0); -- contains the valid bit for the instruction encoded in a one-hot decodeing format
    decoded_instr_RENAME_DSP     : in  std_logic_vector(DSP_UNIT_INSTR_SET_SIZE-1 downto 0); -- contains the valid bit for the instruction encoded in a one-hot decodeing format
    MUL_DIV_instr_RENAME         : in  std_logic; -- indicates which functional unit the Instruction will go to
    IE_instr_RENAME              : in  std_logic; -- indicates which functional unit the Instruction will go to
    LSU_instr_RENAME             : in  std_logic; -- indicates which functional unit the Instruction will go to
    DSP_instr_RENAME             : in  std_logic; -- indicates which functional unit the Instruction will go to
    instr_valid_RENAME           : in  std_logic; -- indicates that there is a valid input instruction
    rs1_valid_RENAME             : in  std_logic; -- indicates that the instruction to the Rename-Stage has this operand valid
    rs2_valid_RENAME             : in  std_logic; -- indicates that the instruction to the Rename-Stage has this operand valid
    rd_valid_RENAME              : in  std_logic; -- indicates that the instruction to the Rename-Stage has this operand valid
    rd_read_only_RENAME          : in  std_logic; -- indicates that the instruction to the Rename-Stage has this operand valid
    -- halt previous stage
    halt_ID                      : out std_logic;  -- sends a halt to the Decode-Stage
    -- MUL_DIV Signals
    signed_op_RENAME             : in  std_logic;
    signed_op_ISSUE              : out std_logic;
    -- IE Signals 
    comparator_en_RENAME         : in  std_logic;
    comparator_en_ISSUE          : out std_logic;
    -- LSU Signals
    load_op_RENAME               : in  std_logic;
    store_op_RENAME              : in  std_logic;
    data_width_RENAME            : in  std_logic_vector(1 downto 0);
    data_be_RENAME               : in  std_logic_vector(3 downto 0);
    load_op_ISSUE                : out std_logic;
    store_op_ISSUE               : out std_logic;
    data_width_ISSUE             : out std_logic_vector(1 downto 0);
    data_be_ISSUE                : out std_logic_vector(3 downto 0);
    -- DSP Unit Signals
    spm_rs1_RENAME               : in  std_logic;
    spm_rs2_RENAME               : in  std_logic;
    vec_read_rs1_RENAME          : in  std_logic;
    vec_read_rs2_RENAME          : in  std_logic;
    vec_write_rd_RENAME          : in  std_logic;
    vec_width_RENAME             : in  std_logic_vector(1 downto 0);
    spm_rs1_ISSUE                : out std_logic;
    spm_rs2_ISSUE                : out std_logic;
    vec_read_rs1_ISSUE           : out std_logic;
    vec_read_rs2_ISSUE           : out std_logic;
    vec_write_rd_ISSUE           : out std_logic;
    vec_width_ISSUE              : out std_logic_vector(1 downto 0);
    -- Output Instruction Signals to the next stage
    instr_valid_ISSUE            : out std_logic;  -- indicates that there is a valid output instruction
    MUL_DIV_instr_ISSUE          : out std_logic;  -- indicates which functional unit the Instruction will go to
    IE_instr_ISSUE               : out std_logic;  -- indicates which functional unit the Instruction will go to
    LSU_instr_ISSUE              : out std_logic;  -- indicates which functional unit the Instruction will go to
    DSP_instr_ISSUE              : out std_logic;  -- indicates which functional unit the Instruction will go to
    rs1_valid_ISSUE              : out std_logic;  -- indicates that the instruction to the Issue-Stage has this operand valid
    rs2_valid_ISSUE              : out std_logic;  -- indicates that the instruction to the Issue-Stage has this operand valid
    old_rd_valid_ISSUE           : out std_logic;  -- indicates that the instruction to the Issue-Stage has this operand valid
    new_rd_valid_ISSUE           : out std_logic;  -- indicates that the instruction to the Issue-Stage has this operand valid
    rd_read_only_valid_ISSUE     : out std_logic;  -- indicates that the instruction to the Issue-Stage has this operand valid
    rs1_ISSUE                    : out std_logic_vector(RF_CEIL-1 downto 0); -- contains the address of the renamed operand
    rs2_ISSUE                    : out std_logic_vector(RF_CEIL-1 downto 0); -- contains the address of the renamed operand
    new_rd_ISSUE                 : out std_logic_vector(RF_CEIL-1 downto 0); -- contains the address of the renamed operand
    rd_read_only_ISSUE           : out std_logic_vector(RF_CEIL-1 downto 0); -- contains the address of the renamed operand
    decoded_instr_ISSUE_MUL_DIV  : out std_logic_vector(MUL_DIV_UNIT_INSTR_SET_SIZE-1 downto 0);  -- contains the valid bit for the instruction encoded in a one-hot decodeing fashion
    decoded_instr_ISSUE_IE       : out std_logic_vector(EXEC_UNIT_INSTR_SET_SIZE-1 downto 0);  -- contains the valid bit for the instruction encoded in a one-hot decodeing fashion
    decoded_instr_ISSUE_LSU      : out std_logic_vector(LS_UNIT_INSTR_SET_SIZE-1 downto 0);  -- contains the valid bit for the instruction encoded in a one-hot decodeing fashion
    decoded_instr_ISSUE_DSP      : out std_logic_vector(DSP_UNIT_INSTR_SET_SIZE-1 downto 0);  -- contains the valid bit for the instruction encoded in a one-hot decodeing fashion
    pc_ISSUE                     : out std_logic_vector(31 downto 0);
    instr_word_ISSUE             : out std_logic_vector(31 downto 0);
    -- Buffer Full Signals from Issue stage
    MUL_DIV_buff_full            : in  std_logic; -- indicate the the ISSUE stage has this buffer full
    IE_buff_full                 : in  std_logic; -- indicate the the ISSUE stage has this buffer full
    LSU_buff_full                : in  std_logic; -- indicate the the ISSUE stage has this buffer full
    DSP_buff_full                : in  std_logic; -- indicate the the ISSUE stage has this buffer full
    -- Commit Counter signals
    FRL_commit_addr              : in  array_2D(11 downto 0)(RF_CEIL-1 downto 0); -- four pointers for rs1, rs2, rd_read only, and old_rd -- AAA rd_read_only should be configurable when accl is disabled
    FRL_commit_en                : in  std_logic_vector(11 downto 0);
    commit_count                 : in  std_logic_vector(3 downto 0); -- four bits to store the count of the regs to be commited for mul_div, ie, and lsu ops and old_rd in rename
    old_rd_count                 : in  std_logic_vector(1 downto 0); -- Two bits since only three resutls can be valid at once
    instr_valid_RENAME_CC        : out std_logic;
    rs1_rename_valid_CC          : out std_logic; -- indicates that the instruction to the Commit-Stage has this operand valid
    rs2_rename_valid_CC          : out std_logic; -- indicates that the instruction to the Commit-Stage has this operand valid
    rd_read_only_rename_valid_CC : out std_logic; -- indicates that the instruction to the Commit-Stage has this operand valid
    old_rd_rename_valid_CC       : out std_logic; -- indicates that the instruction to the Commit-Stage has this operand valid
    new_rd_rename_valid_CC       : out std_logic; -- indicates that the instruction to the Commit-Stage has this operand valid
    rs1_rename_addr_CC           : out std_logic_vector(RF_CEIL-1 downto 0); -- contains the address of the renamed operand
    rs2_rename_addr_CC           : out std_logic_vector(RF_CEIL-1 downto 0); -- contains the address of the renamed operand
    old_rd_rename_addr_CC        : out std_logic_vector(RF_CEIL-1 downto 0); -- contains the address of the renamed operand
    new_rd_rename_addr_CC        : out std_logic_vector(RF_CEIL-1 downto 0)  -- contains the address of the renamed operand
    );
  end component; ------------------------------------------

  component INSTR_ISSUE is
  generic (
    RF_SIZE           : natural;
    RF_CEIL           : natural;
    XLEN              : natural;
    MUL_DIV_BUF_SIZE  : natural;
    IE_BUF_SIZE       : natural;
    LSU_BUF_SIZE      : natural;
    DSP_BUF_SIZE      : natural;
    MUL_DIV_buf_width : natural;
    IE_buf_width      : natural;
    LSU_buf_width     : natural;
    DSP_buf_width     : natural
    );
  port (
    -- core signals
    clk_i                        : in  std_logic;
    rst_ni                       : in  std_logic;
    -- Input operation signals
    pc_ISSUE                     : in  std_logic_vector(31 downto 0);
    instr_word_ISSUE             : in  std_logic_vector(31 downto 0);
    instr_valid_ISSUE            : in  std_logic; -- indicates that there is a valid input instruction
    MUL_DIV_instr_ISSUE          : in  std_logic; -- indicates which functional unit the Instruction will go to
    IE_instr_ISSUE               : in  std_logic; -- indicates which functional unit the Instruction will go to
    LSU_instr_ISSUE              : in  std_logic; -- indicates which functional unit the Instruction will go to
    DSP_instr_ISSUE              : in  std_logic; -- indicates which functional unit the Instruction will go to
    rs1_valid_ISSUE              : in  std_logic; -- indicates that the instruction to the Rename-Stage has this operand valid
    rs2_valid_ISSUE              : in  std_logic; -- indicates that the instruction to the Rename-Stage has this operand valid
    old_rd_valid_ISSUE           : in  std_logic;
    new_rd_valid_ISSUE           : in  std_logic; -- indicates that the instruction to the Rename-Stage has this operand valid
    rd_read_only_valid_ISSUE     : in  std_logic; -- indicates that the instruction to the Rename-Stage has this operand valid
    rs1_ISSUE                    : in  std_logic_vector(RF_CEIL-1 downto 0); -- contains the address of the renamed operand
    rs2_ISSUE                    : in  std_logic_vector(RF_CEIL-1 downto 0); -- contains the address of the renamed operand
    new_rd_ISSUE                 : in  std_logic_vector(RF_CEIL-1 downto 0); -- contains the address of the renamed operand
    rd_read_only_ISSUE           : in  std_logic_vector(RF_CEIL-1 downto 0); -- contains the address of the renamed operand
    decoded_instr_ISSUE_MUL_DIV  : in  std_logic_vector(MUL_DIV_UNIT_INSTR_SET_SIZE-1 downto 0); -- one-hot encoded instructions
    decoded_instr_ISSUE_IE       : in  std_logic_vector(EXEC_UNIT_INSTR_SET_SIZE-1    downto 0); -- one-hot encoded instructions
    decoded_instr_ISSUE_LSU      : in  std_logic_vector(LS_UNIT_INSTR_SET_SIZE-1      downto 0); -- one-hot encoded instructions
    decoded_instr_ISSUE_DSP      : in  std_logic_vector(DSP_UNIT_INSTR_SET_SIZE-1     downto 0); -- one-hot encoded instructions
    decoded_instr_RF_MUL_DIV     : out std_logic_vector(MUL_DIV_UNIT_INSTR_SET_SIZE-1 downto 0); -- one-hot encoded instructions
    decoded_instr_RF_IE          : out std_logic_vector(EXEC_UNIT_INSTR_SET_SIZE-1    downto 0); -- one-hot encoded instructions
    decoded_instr_RF_LSU         : out std_logic_vector(LS_UNIT_INSTR_SET_SIZE-1      downto 0); -- one-hot encoded instructions
    decoded_instr_RF_DSP         : out std_logic_vector(DSP_UNIT_INSTR_SET_SIZE-1     downto 0); -- one-hot encoded instructions
    -- MUL_DIV Signals
    signed_op_ISSUE              : in  std_logic;
    -- IE Signals 
    comparator_en_ISSUE          : in  std_logic;
    -- LSU Signals
    load_op_ISSUE                : in  std_logic;
    store_op_ISSUE               : in  std_logic;
    data_width_ISSUE             : in  std_logic_vector(1 downto 0);
    data_be_ISSUE                : in  std_logic_vector(3 downto 0);
    -- DSP Unit Signals
    spm_rs1_ISSUE                : in  std_logic;
    spm_rs2_ISSUE                : in  std_logic;
    vec_read_rs1_ISSUE           : in  std_logic;
    vec_read_rs2_ISSUE           : in  std_logic;
    vec_write_rd_ISSUE           : in  std_logic;
    vec_width_ISSUE              : in  std_logic_vector(1 downto 0);
    -- Output operation signals
    instr_valid_RF               : out std_logic; -- indicates that there is a valid input instruction
    MUL_DIV_instr_RF             : out std_logic; -- indicates which functional unit the Instruction will go to
    IE_instr_RF                  : out std_logic; -- indicates which functional unit the Instruction will go to
    LSU_instr_RF                 : out std_logic; -- indicates which functional unit the Instruction will go to
    DSP_instr_RF                 : out std_logic; -- indicates which functional unit the Instruction will go to
    mul_div_instr_word_RF        : out std_logic_vector(MUL_DIV_buf_width-1 downto 0); -- contains the restructured instruction word
    ie_instr_word_RF             : out std_logic_vector(IE_buf_width -1 downto 0); -- contains the restructured instruction word
    lsu_instr_word_RF            : out std_logic_vector(LSU_buf_width-1 downto 0); -- contains the restructured instruction word
    dsp_instr_word_RF            : out std_logic_vector(DSP_buf_width-1 downto 0); -- contains the restructured instruction word
    -- Buffer Full signals
    MUL_DIV_buff_full            : out std_logic; -- indicate the the ISSUE stage has this buffer full used for halt the Renaming unit
    IE_buff_full                 : out std_logic; -- indicate the the ISSUE stage has this buffer full used for halt the Renaming unit
    LSU_buff_full                : out std_logic; -- indicate the the ISSUE stage has this buffer full used for halt the Renaming unit
    DSP_buff_full                : out std_logic; -- indicate the the ISSUE stage has this buffer full used for halt the Renaming unit
    -- writeback signals
    MUL_DIV_WB_EN                : in  std_logic;
    IE_WB_EN                     : in  std_logic;
    LSU_WB_EN                    : in  std_logic;
    MUL_DIV_WB_RD_ADDR           : in  std_logic_vector(RF_CEIL-1 downto 0); -- contains the address of the register that has the new valid result
    IE_WB_RD_ADDR                : in  std_logic_vector(RF_CEIL-1 downto 0); -- contains the address of the register that has the new valid result
    LSU_WB_RD_ADDR               : in  std_logic_vector(RF_CEIL-1 downto 0); -- contains the address of the register that has the new valid result
    -- Speculation signals
    speculation_ISSUE            : in  std_logic;
    branch_hit                   : in  std_logic;
    branch_miss                  : in  std_logic;
    -- FU Ready signals
    mul_div_ready_RF             : in  std_logic;  -- indicate that the the RF does not have a pending instruction and it is ready
    ie_ready_RF                  : in  std_logic;  -- indicate that the the RF does not have a pending instruction and it is ready
    lsu_ready_RF                 : in  std_logic;  -- indicate that the the RF does not have a pending instruction and it is ready
    dsp_ready_RF                 : in  std_logic   -- indicate that the the RF does not have a pending instruction and it is ready
    );
  end component; ------------------------------------------

  component Registerfile is
    generic (
      accl_en                      : natural;
      Addr_Width                   : natural;
      SPM_ADDR_WID                 : natural;
      RF_SIZE                      : natural;
      RF_CEIL                      : natural;
      XLEN                         : natural;
      MUL_DIV_buf_width            : natural;
      IE_buf_width                 : natural;
      LSU_buf_width                : natural;
      DSP_buf_width                : natural
    );
    port (
      clk_i                        : in  std_logic;
      rst_ni                       : in  std_logic;
      -- FU Ready signals
      decoded_instr_RF_MUL_DIV     : in  std_logic_vector(MUL_DIV_UNIT_INSTR_SET_SIZE-1 downto 0); -- One-Hot encoded instructions
      decoded_instr_RF_IE          : in  std_logic_vector(EXEC_UNIT_INSTR_SET_SIZE -1   downto 0); -- One-Hot encoded instructions
      decoded_instr_RF_LSU         : in  std_logic_vector(LS_UNIT_INSTR_SET_SIZE -1     downto 0); -- One-Hot encoded instructions
      decoded_instr_RF_DSP         : in  std_logic_vector(DSP_UNIT_INSTR_SET_SIZE -1    downto 0); -- One-Hot encoded instructions
      decoded_instruction_MUL_DIV  : out std_logic_vector(MUL_DIV_UNIT_INSTR_SET_SIZE-1 downto 0); -- One-Hot encoded instructions
      decoded_instruction_IE       : out std_logic_vector(EXEC_UNIT_INSTR_SET_SIZE -1   downto 0); -- One-Hot encoded instructions
      decoded_instruction_LSU      : out std_logic_vector(LS_UNIT_INSTR_SET_SIZE -1     downto 0); -- One-Hot encoded instructions
      decoded_instruction_DSP      : out std_logic_vector(DSP_UNIT_INSTR_SET_SIZE -1    downto 0); -- One-Hot encoded instructions
      -- Input operation signals
      instr_valid_RF               : in  std_logic; -- indicates that there is a valid input instruction
      MUL_DIV_instr_RF             : in  std_logic; -- indicates which functional unit the Instruction will go to
      IE_instr_RF                  : in  std_logic; -- indicates which functional unit the Instruction will go to
      LSU_instr_RF                 : in  std_logic; -- indicates which functional unit the Instruction will go to
      DSP_instr_RF                 : in  std_logic; -- indicates which functional unit the Instruction will go to
      MUL_DIV_instr                : out std_logic; -- indicates which functional unit the Instruction will go to
      IE_instr                     : out std_logic; -- indicates which functional unit the Instruction will go to
      LSU_instr                    : out std_logic; -- indicates which functional unit the Instruction will go to
      DSP_instr                    : out std_logic; -- indicates which functional unit the Instruction will go to
      mul_div_instr_word_RF        : in  std_logic_vector(MUL_DIV_buf_width-1 downto 0); -- contains the restructured instruction word for the RF stage
      ie_instr_word_RF             : in  std_logic_vector(IE_buf_width -1 downto 0); -- contains the restructured instruction word for the RF stage
      lsu_instr_word_RF            : in  std_logic_vector(LSU_buf_width-1 downto 0); -- contains the restructured instruction word for the RF stage
      dsp_instr_word_RF            : in  std_logic_vector(DSP_buf_width-1 downto 0); -- contains the restructured instruction word for the RF stage
      -- FU input ports
      mul_div_data_rs1             : out std_logic_vector(31 downto 0);
      mul_div_data_rs2             : out std_logic_vector(31 downto 0);
      mul_div_data_old_rd          : out std_logic_vector(31 downto 0);
      ie_data_rs1                  : out std_logic_vector(31 downto 0);
      ie_data_rs2                  : out std_logic_vector(31 downto 0);
      ie_data_old_rd               : out std_logic_vector(31 downto 0);
      lsu_data_rs1                 : out std_logic_vector(31 downto 0);
      lsu_data_rs2                 : out std_logic_vector(31 downto 0);
      lsu_data_old_rd              : out std_logic_vector(31 downto 0);
      dsp_data_rs1                 : out std_logic_vector(31 downto 0);
      dsp_data_rs2                 : out std_logic_vector(31 downto 0);
      dsp_data_rd                  : out std_logic_vector(31 downto 0);
      -- FU SIGNAL OPERAND Valid
      mul_div_rs1_valid            : out std_logic;
      mul_div_rs2_valid            : out std_logic;
      mul_div_rd_valid             : out std_logic;
      ie_rs1_valid                 : out std_logic;
      ie_rs2_valid                 : out std_logic;
      ie_rd_valid                  : out std_logic;
      lsu_rs1_valid                : out std_logic;
      lsu_rs2_valid                : out std_logic;
      lsu_rd_valid                 : out std_logic;
      dsp_rs1_valid                : out std_logic;
      dsp_rs2_valid                : out std_logic;
      dsp_rd_valid                 : out std_logic;
      -- FU SIGNAL OPERAND ADDR
      mul_div_addr_rs1             : out std_logic_vector(RF_CEIL-1 downto 0);
      mul_div_addr_rs2             : out std_logic_vector(RF_CEIL-1 downto 0);
      mul_div_addr_new_rd          : out std_logic_vector(RF_CEIL-1 downto 0);
      ie_addr_rs1                  : out std_logic_vector(RF_CEIL-1 downto 0);
      ie_addr_rs2                  : out std_logic_vector(RF_CEIL-1 downto 0);
      ie_addr_new_rd               : out std_logic_vector(RF_CEIL-1 downto 0);
      lsu_addr_rs1                 : out std_logic_vector(RF_CEIL-1 downto 0);
      lsu_addr_rs2                 : out std_logic_vector(RF_CEIL-1 downto 0);
      lsu_addr_new_rd              : out std_logic_vector(RF_CEIL-1 downto 0);
      dsp_addr_rs1                 : out std_logic_vector(RF_CEIL-1 downto 0);
      dsp_addr_rs2                 : out std_logic_vector(RF_CEIL-1 downto 0);
      dsp_addr_rd                  : out std_logic_vector(RF_CEIL-1 downto 0);
      -- MUL_DIV INSTR INFO
      signed_op                    : out std_logic;
      -- IE INSTR INFO
      comparator_en                : out std_logic;
      pc_IE                        : out std_logic_vector(31 downto 0);
      instr_word_IE                : out std_logic_vector(31 downto 0);
      -- LSU INSTR INFO
      load_op                      : out std_logic;
      store_op                     : out std_logic;
      data_width_ID                : out std_logic_vector(1 downto 0);
      data_be                      : out std_logic_vector(3 downto 0);
      instr_word_LSU               : out std_logic_vector(31 downto 0);
      -- DSP INSTR INFO
      spm_rs1_ISSUE                : out std_logic;
      spm_rs2_ISSUE                : out std_logic;
      vec_read_rs1_ISSUE           : out std_logic;
      vec_read_rs2_ISSUE           : out std_logic;
      vec_write_rd_ISSUE           : out std_logic;
      vec_width_ISSUE              : out std_logic_vector(1 downto 0);
      -- FU busy signals
      busy_mul_div                 : in  std_logic;
      busy_ie                      : in  std_logic;
      busy_lsu                     : in  std_logic;
      busy_dsp                     : in  std_logic;
      -- RF ready signals
      mul_div_ready_RF             : out std_logic;
      ie_ready_RF                  : out std_logic;
      lsu_ready_RF                 : out std_logic;
      dsp_ready_RF                 : out std_logic;
      -- SPM operand mappers
      rs1_to_sc                    : out std_logic_vector(SPM_ADDR_WID-1 downto 0);
      rs2_to_sc                    : out std_logic_vector(SPM_ADDR_WID-1 downto 0);
      rd_to_sc                     : out std_logic_vector(SPM_ADDR_WID-1 downto 0);
      -- writeback signals
      MUL_DIV_WB_EN                : in  std_logic;
      IE_WB_EN                     : in  std_logic;
      LSU_WB_EN                    : in  std_logic;
      MUL_DIV_WB                   : in  std_logic_vector(31 downto 0);
      IE_WB                        : in  std_logic_vector(31 downto 0);
      LSU_WB                       : in  std_logic_vector(31 downto 0);
      MUL_DIV_WB_RD_ADDR           : in  std_logic_vector(RF_CEIL-1 downto 0); 
      IE_WB_RD_ADDR                : in  std_logic_vector(RF_CEIL-1 downto 0); 
      LSU_WB_RD_ADDR               : in  std_logic_vector(RF_CEIL-1 downto 0);
      -- CC signals
      instr_valid_RF_CC            : out std_logic_vector(3 downto 0);
      mul_div_valid_RF_CC          : out std_logic_vector(2 downto 0);
      ie_valid_RF_CC               : out std_logic_vector(2 downto 0);
      lsu_valid_RF_CC              : out std_logic_vector(2 downto 0);
      dsp_valid_RF_CC              : out std_logic_vector(2 downto 0);
      old_rd_valid_CC              : out std_logic_vector(2 downto 0);-- contains the valids of the old_rds
      old_rd_addr_CC               : out array_2D(2 downto 0)(RF_CEIL-1 downto 0);
      mul_div_ptr_RF_CC            : out array_2D(2 downto 0)(RF_CEIL-1 downto 0);
      ie_ptr_RF_CC                 : out array_2D(2 downto 0)(RF_CEIL-1 downto 0);
      lsu_ptr_RF_CC                : out array_2D(2 downto 0)(RF_CEIL-1 downto 0);
      dsp_ptr_RF_CC                : out array_2D(2 downto 0)(RF_CEIL-1 downto 0)
    );
  end component; ------------------------------------------

  component MULTIPLIER_DIVIDER is
  generic (
    RF_CEIL                     : natural
    );
  port (
    clk_i                       : in  std_logic;
    rst_ni                      : in  std_logic;
    fetch_enable_i              : in  std_logic;

    irq_pending                 : in  std_logic;
    dbg_req_o                   : in  std_logic;
    signed_op                   : in  std_logic;
    MUL_DIV_instr               : in  std_logic;
    mul_div_addr_new_rd         : in  std_logic_vector(RF_CEIL-1 downto 0);
    decoded_instruction_MUL_DIV : in  std_logic_vector(MUL_DIV_UNIT_INSTR_SET_SIZE-1 downto 0);
    pc_mul_div                  : in  std_logic_vector(31 downto 0); 
    mul_div_data_rs1            : in  std_logic_vector(31 downto 0);
    mul_div_data_rs2            : in  std_logic_vector(31 downto 0);
    mul_div_old_rd              : in  std_logic_vector(31 downto 0);
    mul_div_old_rd_valid        : in  std_logic;
    mul_div_old_rd_valid_CC     : out std_logic;
    pc_mul_div_WB               : out std_logic_vector(31 downto 0);
    mul_div_old_rd_CC           : out std_logic_vector(31 downto 0);
    MUL_DIV_WB                  : out std_logic_vector(31 downto 0);
    MUL_DIV_WB_EN               : out std_logic;
    MUL_DIV_WB_RD_ADDR          : out std_logic_vector(RF_CEIL-1 downto 0);
    busy_mul_div                : out std_logic
    );
  end component; ------------------------------------------

  component IE_STAGE is
  generic(
    RF_CEIL                : natural
  );
  port (
  -- clock, and reset active low
    clk_i, rst_ni          : in  std_logic;
    fetch_enable_i         : in  std_logic;
    irq_i                  : in  std_logic;
    IE_instr               : in  std_logic;
    pc_IE                  : in  std_logic_vector(31 downto 0);
    instr_word_IE          : in  std_logic_vector(31 downto 0);
    ie_data_rs1            : in  std_logic_vector(31 downto 0);
    ie_data_rs2            : in  std_logic_vector(31 downto 0);
    ie_addr_rs1            : in  std_logic_vector(RF_CEIL-1 downto 0);
    ie_addr_rs2            : in  std_logic_vector(RF_CEIL-1 downto 0);
    ie_addr_new_rd         : in  std_logic_vector(RF_CEIL-1 downto 0);
    irq_pending            : in  std_logic;
    csr_instr_done         : in  std_logic;
    csr_access_denied_o    : in  std_logic;
    csr_rdata_o            : in  std_logic_vector(31 downto 0);
    data_addr_internal_IE  : in  std_logic_vector(31 downto 0);
    dbg_req_o              : in  std_logic;
    MSTATUS                : in  std_logic_vector(1 downto 0);
    instr_rvalid_IE        : in  std_logic;  -- validity bit at IE input
    taken_branch           : in  std_logic;
    comparator_en          : in  std_logic;
    decoded_instruction_IE : in  std_logic_vector(EXEC_UNIT_INSTR_SET_SIZE-1 downto 0);
    csr_addr_i             : out std_logic_vector(11 downto 0);
    ie_except_data         : out std_logic_vector(31 downto 0);
    ie_csr_wdata_i         : out std_logic_vector(31 downto 0);
    csr_op_i               : out std_logic_vector(2 downto 0);
    csr_wdata_en           : out std_logic;
    csr_instr_req          : out std_logic;
    busy_ie                : out std_logic;
    jump_instr             : out std_logic;
    jump_instr_lat         : out std_logic;
    WFI_Instr              : out std_logic;
    sleep_state            : out std_logic;
    set_branch_condition   : out std_logic;
    IE_except_condition    : out std_logic;
    set_mret_condition     : out std_logic;
    set_wfi_condition      : out std_logic;
    ie_taken_branch        : out std_logic;
    branch_instr           : out std_logic;
    branch_instr_lat       : out std_logic;
    PC_offset              : out std_logic_vector(31 downto 0);
    served_irq             : out std_logic;
    dbg_ack_i              : out std_logic;
    ebreak_instr           : out std_logic;
    absolute_jump          : out std_logic;
    IE_WB_EN               : out std_logic;
    IE_WB                  : out std_logic_vector(31 downto 0);
    IE_WB_RD_ADDR          : out std_logic_vector(RF_CEIL-1 downto 0);
    pc_WB                  : out std_logic_vector(31 downto 0);
    state_IE               : out fsm_IE_states
    );
  end component;  -----------------------------------------

  component Load_Store_Unit is
  generic(
    accl_en                    : natural;
    SIMD                       : natural;
    SPM_NUM                    : natural;  
    Addr_Width                 : natural;
    Data_Width                 : natural;
    SIMD_BITS                  : natural;
    SPM_ADDR_WID               : natural;
    RF_CEIL                    : natural
    );
  port (
    -- clock, and reset active low
    clk_i, rst_ni              : in std_logic;
  -- Program Counter Signals
    irq_pending                : in std_logic;
    -- ID_Stage Signals
    lsu_data_rs1               : in  std_logic_vector(31 downto 0);
    lsu_data_rs2               : in  std_logic_vector(31 downto 0);
    lsu_data_old_rd            : in  std_logic_vector(31 downto 0);
    instr_word_LSU             : in  std_logic_vector(31 downto 0);
    pc_IE                      : in  std_logic_vector(31 downto 0);
    decoded_instruction_LSU    : in  std_logic_vector(LS_UNIT_INSTR_SET_SIZE-1 downto 0);
    data_be                    : in  std_logic_vector(3 downto 0);
    data_width_ID              : in  std_logic_vector(1 downto 0);
    LSU_instr                  : in  std_logic;
    load_op                    : in  std_logic;
    store_op                   : in  std_logic;
    --sw_mip                     : in  std_logic;
    busy_lsu                   : out std_logic;
    -- Processing Pipeline Signals
    rs1_to_sc                  : in  std_logic_vector(SPM_ADDR_WID-1 downto 0);
    rs2_to_sc                  : in  std_logic_vector(SPM_ADDR_WID-1 downto 0);
    rd_to_sc                   : in  std_logic_vector(SPM_ADDR_WID-1 downto 0);
    halt_LSU                   : in  std_logic;
    data_addr_internal         : out std_logic_vector(31 downto 0);
    ls_except_data             : out std_logic_vector(31 downto 0);
    ls_except_condition        : out std_logic;
    ls_taken_branch            : out std_logic;
    amo_load                   : in  std_logic;
    amo_load_skip              : in  std_logic;
    amo_store                  : out std_logic;
    -- CSR Signals
    misaligned_err             : out std_logic;
    -- Scratchpad Interface Signals
    lsu_addr_rs1               : in  std_logic_vector(RF_CEIL-1 downto 0);
    lsu_addr_rs2               : in  std_logic_vector(RF_CEIL-1 downto 0);
    lsu_addr_new_rd            : in  std_logic_vector(RF_CEIL-1 downto 0);
    ls_data_gnt_i              : in  std_logic_vector(SPM_NUM-1 downto 0);
    ls_sci_wr_gnt              : in  std_logic;
    ls_sc_data_read_wire       : in  std_logic_vector(Data_Width-1 downto 0);
    state_LS                   : out fsm_LS_states;
    harc_LS_wire               : out integer;
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
    LSU_WB_EN                  : out std_logic;
    LSU_WB                     : out std_logic_vector(31 downto 0);
    LSU_WB_RD_ADDR             : out std_logic_vector(RF_CEIL-1 downto 0);
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
  end component;  -----------------------------------------  

  component COMMIT_COUNTER is
  generic (
    RF_SIZE                      : natural;
    RF_CEIL                      : natural
    );
  port (
    clk_i                        : in  std_logic;
    rst_ni                       : in  std_logic;
    -- Recovery Buffer Signals
    mul_div_old_rd_valid_CC      : in  std_logic;
    ie_old_rd_valid_CC           : in  std_logic;
    lsu_old_rd_valid_CC          : in  std_logic;
    mul_div_old_rd_CC            : in  std_logic_vector(31 downto 0);
    ie_old_rd_CC                 : in  std_logic_vector(31 downto 0);
    lsu_old_rd_CC                : in  std_logic_vector(31 downto 0);
    mul_div_addr_CC              : in  std_logic_vector(RF_CEIL-1 downto 0);
    ie_rob_addr_CC               : in  std_logic_vector(RF_CEIL-1 downto 0);
    lsu_rob_addr_CC              : in  std_logic_vector(RF_CEIL-1 downto 0);
    -- frl signal
    instr_valid_RENAME_CC        : in  std_logic;
    rs1_rename_valid_CC          : in  std_logic; -- indicates that the instruction to the Commit-Stage has this operand valid
    rs2_rename_valid_CC          : in  std_logic; -- indicates that the instruction to the Commit-Stage has this operand valid
    rd_read_only_rename_valid_CC : in  std_logic; -- indicates that the instruction to the Commit-Stage has this operand valid
    old_rd_rename_valid_CC       : in  std_logic; -- indicates that the instruction to the Commit-Stage has this operand valid
    new_rd_rename_valid_CC       : in  std_logic; -- indicates that the instruction to the Commit-Stage has this operand valid
    rs1_rename_addr_CC           : in  std_logic_vector(RF_CEIL-1 downto 0); -- contains the address of the renamed operand
    rs2_rename_addr_CC           : in  std_logic_vector(RF_CEIL-1 downto 0); -- contains the address of the renamed operand
    old_rd_rename_addr_CC        : in  std_logic_vector(RF_CEIL-1 downto 0); -- contains the address of the renamed operand
    new_rd_rename_addr_CC        : in  std_logic_vector(RF_CEIL-1 downto 0); -- contains the address of the renamed operand
    FRL_commit_addr              : out array_2D(11 downto 0)(RF_CEIL-1 downto 0); -- four pointers for rs1, rs2, rd_read only, and old_rd -- AAA rd_read_only should be configurable when accl is disabled
    FRL_commit_en                : out std_logic_vector(11 downto 0); -- AAA should be configurable just as the above
    commit_count                 : out std_logic_vector(3 downto 0); -- four bits to store the count of the regs to be commited for mul_div, ie, and lsu ops and old_rd in rename
    old_rd_count                 : out std_logic_vector(1 downto 0); -- Two bits since only three resutls can be valid at once
    -- CC signals
    instr_valid_RF_CC            : in  std_logic_vector(3 downto 0);
    mul_div_valid_RF_CC          : in  std_logic_vector(2 downto 0);
    ie_valid_RF_CC               : in  std_logic_vector(2 downto 0);
    lsu_valid_RF_CC              : in  std_logic_vector(2 downto 0);
    dsp_valid_RF_CC              : in  std_logic_vector(2 downto 0);
    old_rd_valid_CC              : in  std_logic_vector(2 downto 0);-- contains the valids of the old_rds
    old_rd_addr_CC               : in  array_2D(2 downto 0)(RF_CEIL-1 downto 0);
    mul_div_ptr_RF_CC            : in  array_2D(2 downto 0)(RF_CEIL-1 downto 0);
    ie_ptr_RF_CC                 : in  array_2D(2 downto 0)(RF_CEIL-1 downto 0);
    lsu_ptr_RF_CC                : in  array_2D(2 downto 0)(RF_CEIL-1 downto 0);
    dsp_ptr_RF_CC                : in  array_2D(2 downto 0)(RF_CEIL-1 downto 0)
  );
  end component;  -----------------------------------------

  component DSP_Unit is
  generic(
    THREAD_POOL_SIZE      : integer;
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
    harc_EXEC                  : in  harc_range;
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
    busy_dsp                   : out std_logic_vector(accl_range);
	-- Scratchpad Interface Signals
    dsp_data_gnt_i             : in  std_logic_vector(accl_range);
    dsp_sci_wr_gnt             : in  std_logic_vector(accl_range);
    dsp_sc_data_read           : in  array_3d(accl_range)(1 downto 0)(SIMD_Width-1 downto 0);
    dsp_we_word                : out array_2d(accl_range)(SIMD-1 downto 0);
    dsp_sc_read_addr           : out array_3d(accl_range)(1 downto 0)(Addr_Width-1 downto 0);
    dsp_to_sc                  : out array_3d(accl_range)(SPM_NUM-1 downto 0)(1 downto 0);
    dsp_sc_data_write_wire     : out array_2d(accl_range)(SIMD_Width-1 downto 0);
    dsp_sc_write_addr          : out array_2d(accl_range)(Addr_Width-1 downto 0);
    dsp_sci_we                 : out array_2d(accl_range)(SPM_NUM-1 downto 0);
    dsp_sci_req                : out array_2d(accl_range)(SPM_NUM-1 downto 0);
    -- tracer signals
    state_DSP                  : out array_2d(accl_range)(1 downto 0)
	);
  end component;  ------------------------------------------

  component Scratchpad_memory_interface is
  generic(
    accl_en                    : natural;
    SPM_NUM		                 : natural; 
    Addr_Width                 : natural;
    SIMD                       : natural;
    -------------------------------------
    ACCL_NUM                   : natural;
    SIMD_BITS                  : natural;
    Data_Width                 : natural;
    SIMD_Width                 : natural
  );
  port (
    clk_i, rst_ni              : in  std_logic;
	  data_rvalid_i              : in  std_logic;
    state_LS                   : in  fsm_LS_states;
    sc_word_count_wire         : in  integer;
    spm_bcast                  : in  std_logic;
    harc_LS_wire               : in  accl_range;
    dsp_we_word                : in  array_2d(accl_range)(SIMD-1 downto 0);
    ls_sc_data_write_wire      : in  std_logic_vector(Data_Width-1 downto 0);
    dsp_sc_data_write_wire     : in  array_2d(accl_range)(SIMD_Width-1 downto 0);
    ls_sc_read_addr            : in  std_logic_vector(Addr_Width-(SIMD_BITS+3) downto 0);
    ls_sc_write_addr           : in  std_logic_vector(Addr_Width-(SIMD_BITS+3) downto 0);
    dsp_sc_write_addr          : in  array_2d(accl_range)(Addr_Width-1 downto 0);
    ls_sci_req                 : in  std_logic_vector(SPM_NUM-1 downto 0);
    ls_sci_we                  : in  std_logic_vector(SPM_NUM-1 downto 0);
    dsp_sci_req                : in  array_2d(accl_range)(SPM_NUM-1 downto 0);
    dsp_sci_we                 : in  array_2d(accl_range)(SPM_NUM-1 downto 0);
    kmemld_inflight            : in  std_logic_vector(SPM_NUM-1 downto 0);
    kmemstr_inflight           : in  std_logic_vector(SPM_NUM-1 downto 0);
	  dsp_to_sc                  : in  array_3d(accl_range)(SPM_NUM-1 downto 0)(1 downto 0);
    dsp_sc_read_addr           : in  array_3d(accl_range)(1 downto 0)(Addr_Width-1 downto 0);
    dsp_sc_data_read           : out array_3d(accl_range)(1 downto 0)(SIMD_Width-1 downto 0);
    ls_sc_data_read_wire       : out std_logic_vector(Data_Width-1 downto 0);
    ls_sci_wr_gnt              : out std_logic;
    dsp_sci_wr_gnt             : out std_logic_vector(accl_range);
    ls_data_gnt_i              : out std_logic_vector(SPM_NUM-1 downto 0);
    dsp_data_gnt_i             : out std_logic_vector(accl_range)
	);
  end component;  ------------------------------------------

--------------------------------------------------------------------------------------------------
----------------------- ARCHITECTURE BEGIN -------------------------------------------------------
begin

  -- Klessydra T13 (4 stages) pipeline implementation -----------------------

  -- check for microarchitecture configuration limit, up to 16 thread support.
  assert THREAD_POOL_SIZE < 2**THREAD_ID_SIZE
    report "Threading configuration not supported"
  severity error;

	  
  taken_branch <= '1' when (ie_taken_branch = '1' or ls_taken_branch = '1' or unsigned(dsp_taken_branch) /= 0) else '0';
					
  csr_wdata_i <= ie_csr_wdata_i;

------------------------------------------------------------------------------------------------------------------------------------
-- Core_busy_o
------------------------------------------------------------------------------------------------------------------------------------

  core_busy_o <= '1' when (instr_rvalid_i or instr_rvalid_ID or instr_rvalid_IE) = '1' and rst_ni = '1' else '0';
------------------------------------------------------------------------------------------------------------------------------------

  dsp_except_off : if accl_en = 0 generate
    dsp_except_condition <= (others => '0');
    dsp_taken_branch     <= (others => '0');
  end generate;

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
  port map(
    pc_IF             => pc_IF, 
    instr_rvalid_i    => instr_rvalid_i, 
    pc_ID             => pc_ID, 
    instr_rvalid_ID   => instr_rvalid_ID, 
    instr_word_ID_lat => instr_word_ID_lat, 
    halt_IF           => halt_IF,
    clk_i             => clk_i,
    rst_ni            => rst_ni,
    fetch_enable_i    => fetch_enable_i,
    instr_req_o       => instr_req_o,
    instr_gnt_i       => instr_gnt_i,
    instr_rdata_i     => instr_rdata_i
    );

  DECODE : ID_STAGE
  generic map(
    RV32M                        => RV32M,
    accl_en                      => accl_en,
    RF_CEIL                      => RF_CEIL,
    Logical_RF_CEIL              => Logical_RF_CEIL
    )
  port map(            
    clk_i                        => clk_i,
    rst_ni                       => rst_ni,
    pc_ID                        => pc_ID,
    instr_rvalid_ID              => instr_rvalid_ID,
    instr_word_ID_lat            => instr_word_ID_lat,
    instr_word_RENAME            => instr_word_RENAME,
    pc_RENAME                    => pc_RENAME,
    decoded_instr_RENAME_MUL_DIV => decoded_instr_RENAME_MUL_DIV,
    decoded_instr_RENAME_IE      => decoded_instr_RENAME_IE,
    decoded_instr_RENAME_LSU     => decoded_instr_RENAME_LSU,
    decoded_instr_RENAME_DSP     => decoded_instr_RENAME_DSP,
    instr_valid_RENAME           => instr_valid_RENAME,
    MUL_DIV_instr_RENAME         => MUL_DIV_instr_RENAME,
    IE_instr_RENAME              => IE_instr_RENAME,
    LSU_instr_RENAME             => LSU_instr_RENAME,
    DSP_instr_RENAME             => DSP_instr_RENAME,
    rs1_valid_RENAME             => rs1_valid_RENAME,
    rs2_valid_RENAME             => rs2_valid_RENAME,
    rd_valid_RENAME              => rd_valid_RENAME,
    rd_read_only_RENAME          => rd_read_only_RENAME,
    halt_ID                      => halt_ID,
    halt_IF                      => halt_IF,
    absolute_jump                => absolute_jump,
    branch_instr                 => branch_instr,
    set_branch_condition_ID      => set_branch_condition_ID,
    id_taken_branch              => id_taken_branch,
    PC_offset_ID                 => PC_offset_ID,
    signed_op_RENAME             => signed_op_RENAME,
    comparator_en_RENAME         => comparator_en_RENAME,
    load_op_RENAME               => load_op_RENAME,
    store_op_RENAME              => store_op_RENAME,
    data_width_RENAME            => data_width_RENAME,
    data_be_RENAME               => data_be_RENAME,
    spm_rs1_RENAME               => spm_rs1_RENAME,
    spm_rs2_RENAME               => spm_rs2_RENAME,
    vec_read_rs1_RENAME          => vec_read_rs1_RENAME,
    vec_read_rs2_RENAME          => vec_read_rs2_RENAME,
    vec_write_rd_RENAME          => vec_write_rd_RENAME,
    vec_width_RENAME             => vec_width_RENAME
  );

  RENAME : REG_RENAMING
  generic map(
    RF_SIZE                       => RF_SIZE,
    RF_CEIL                       => RF_CEIL,
    Logical_RF_SIZE               => Logical_RF_SIZE,
    Logical_RF_CEIL               => Logical_RF_CEIL
  )
  port map(
    clk_i                         => clk_i,
    rst_ni                        => rst_ni,
    pc_RENAME                     => pc_RENAME,
    instr_word_RENAME             => instr_word_RENAME,
    decoded_instr_RENAME_MUL_DIV  => decoded_instr_RENAME_MUL_DIV,
    decoded_instr_RENAME_IE       => decoded_instr_RENAME_IE,
    decoded_instr_RENAME_LSU      => decoded_instr_RENAME_LSU,
    decoded_instr_RENAME_DSP      => decoded_instr_RENAME_DSP,
    MUL_DIV_instr_RENAME          => MUL_DIV_instr_RENAME,
    IE_instr_RENAME               => IE_instr_RENAME,
    LSU_instr_RENAME              => LSU_instr_RENAME,
    DSP_instr_RENAME              => DSP_instr_RENAME,
    instr_valid_RENAME            => instr_valid_RENAME,
    rs1_valid_RENAME              => rs1_valid_RENAME,
    rs2_valid_RENAME              => rs2_valid_RENAME,
    rd_valid_RENAME               => rd_valid_RENAME,
    rd_read_only_RENAME           => rd_read_only_RENAME,
    halt_ID                       => halt_ID,
    signed_op_RENAME              => signed_op_RENAME,
    signed_op_ISSUE               => signed_op_ISSUE,
    comparator_en_RENAME          => comparator_en_RENAME,
    comparator_en_ISSUE           => comparator_en_ISSUE,
    load_op_RENAME                => load_op_RENAME,
    store_op_RENAME               => store_op_RENAME,
    data_width_RENAME             => data_width_RENAME,
    data_be_RENAME                => data_be_RENAME,      
    load_op_ISSUE                 => load_op_ISSUE,
    store_op_ISSUE                => store_op_ISSUE,
    data_width_ISSUE              => data_width_ISSUE,
    data_be_ISSUE                 => data_be_ISSUE,
    spm_rs1_RENAME                => spm_rs1_RENAME,
    spm_rs2_RENAME                => spm_rs2_RENAME,
    vec_read_rs1_RENAME           => vec_read_rs1_RENAME,
    vec_read_rs2_RENAME           => vec_read_rs2_RENAME,
    vec_write_rd_RENAME           => vec_write_rd_RENAME,
    vec_width_RENAME              => vec_width_RENAME,
    spm_rs1_ISSUE                 => spm_rs1_ISSUE,
    spm_rs2_ISSUE                 => spm_rs2_ISSUE,
    vec_read_rs1_ISSUE            => vec_read_rs1_ISSUE,
    vec_read_rs2_ISSUE            => vec_read_rs2_ISSUE,
    vec_write_rd_ISSUE            => vec_write_rd_ISSUE,
    vec_width_ISSUE               => vec_width_ISSUE,          
    instr_valid_ISSUE             => instr_valid_ISSUE,
    MUL_DIV_instr_ISSUE           => MUL_DIV_instr_ISSUE,
    IE_instr_ISSUE                => IE_instr_ISSUE,
    LSU_instr_ISSUE               => LSU_instr_ISSUE,
    DSP_instr_ISSUE               => DSP_instr_ISSUE,
    rs1_valid_ISSUE               => rs1_valid_ISSUE,
    rs2_valid_ISSUE               => rs2_valid_ISSUE,
    old_rd_valid_ISSUE            => old_rd_valid_ISSUE,
    new_rd_valid_ISSUE            => new_rd_valid_ISSUE,
    rd_read_only_valid_ISSUE      => rd_read_only_valid_ISSUE,
    rs1_ISSUE                     => rs1_ISSUE,
    rs2_ISSUE                     => rs2_ISSUE,
    new_rd_ISSUE                  => new_rd_ISSUE,
    rd_read_only_ISSUE            => rd_read_only_ISSUE,
    decoded_instr_ISSUE_MUL_DIV   => decoded_instr_ISSUE_MUL_DIV,
    decoded_instr_ISSUE_IE        => decoded_instr_ISSUE_IE,
    decoded_instr_ISSUE_LSU       => decoded_instr_ISSUE_LSU,
    decoded_instr_ISSUE_DSP       => decoded_instr_ISSUE_DSP,
    pc_ISSUE                      => pc_ISSUE,
    instr_word_ISSUE              => instr_word_ISSUE,
    MUL_DIV_buff_full             => MUL_DIV_buff_full,
    IE_buff_full                  => IE_buff_full,
    LSU_buff_full                 => LSU_buff_full,
    DSP_buff_full                 => DSP_buff_full,
    FRL_commit_addr               => FRL_commit_addr,
    FRL_commit_en                 => FRL_commit_en,
    commit_count                  => commit_count,
    old_rd_count                  => old_rd_count,
    instr_valid_RENAME_CC         => instr_valid_RENAME_CC,
    rs1_rename_valid_CC           => rs1_rename_valid_CC,
    rs2_rename_valid_CC           => rs2_rename_valid_CC,
    rd_read_only_rename_valid_CC  => rd_read_only_rename_valid_CC,
    old_rd_rename_valid_CC        => old_rd_rename_valid_CC,
    new_rd_rename_valid_CC        => new_rd_rename_valid_CC,
    rs1_rename_addr_CC            => rs1_rename_addr_CC,
    rs2_rename_addr_CC            => rs2_rename_addr_CC,
    old_rd_rename_addr_CC         => old_rd_rename_addr_CC,
    new_rd_rename_addr_CC         => new_rd_rename_addr_CC
  );


  ISSUE : INSTR_ISSUE
  generic map(
    RF_SIZE                      => RF_SIZE,
    RF_CEIL                      => RF_CEIL,
    XLEN                         => XLEN,
    MUL_DIV_BUF_SIZE             => MUL_DIV_BUF_SIZE,
    IE_BUF_SIZE                  => IE_BUF_SIZE,
    LSU_BUF_SIZE                 => LSU_BUF_SIZE,
    DSP_BUF_SIZE                 => DSP_BUF_SIZE,
    MUL_DIV_buf_width            => MUL_DIV_buf_width,
    IE_buf_width                 => IE_buf_width, 
    LSU_buf_width                => LSU_buf_width,
    DSP_buf_width                => DSP_buf_width
  )
  port map(
    clk_i                        => clk_i,
    rst_ni                       => rst_ni,
    pc_ISSUE                     => pc_ISSUE,
    instr_word_ISSUE             => instr_word_ISSUE,
    instr_valid_ISSUE            => instr_valid_ISSUE,
    MUL_DIV_instr_ISSUE          => MUL_DIV_instr_ISSUE,
    IE_instr_ISSUE               => IE_instr_ISSUE,
    LSU_instr_ISSUE              => LSU_instr_ISSUE,
    DSP_instr_ISSUE              => DSP_instr_ISSUE,
    rs1_valid_ISSUE              => rs1_valid_ISSUE,
    rs2_valid_ISSUE              => rs2_valid_ISSUE,
    old_rd_valid_ISSUE           => old_rd_valid_ISSUE,
    new_rd_valid_ISSUE           => new_rd_valid_ISSUE,
    rd_read_only_valid_ISSUE     => rd_read_only_valid_ISSUE,
    rs1_ISSUE                    => rs1_ISSUE,
    rs2_ISSUE                    => rs2_ISSUE,
    new_rd_ISSUE                 => new_rd_ISSUE,
    rd_read_only_ISSUE           => rd_read_only_ISSUE,
    decoded_instr_ISSUE_MUL_DIV  => decoded_instr_ISSUE_MUL_DIV,
    decoded_instr_ISSUE_IE       => decoded_instr_ISSUE_IE,
    decoded_instr_ISSUE_LSU      => decoded_instr_ISSUE_LSU,
    decoded_instr_ISSUE_DSP      => decoded_instr_ISSUE_DSP,
    decoded_instr_RF_MUL_DIV     => decoded_instr_RF_MUL_DIV,
    decoded_instr_RF_IE          => decoded_instr_RF_IE,
    decoded_instr_RF_LSU         => decoded_instr_RF_LSU,
    decoded_instr_RF_DSP         => decoded_instr_RF_DSP,
    signed_op_ISSUE              => signed_op_ISSUE,
    comparator_en_ISSUE          => comparator_en_ISSUE,
    load_op_ISSUE                => load_op_ISSUE,
    store_op_ISSUE               => store_op_ISSUE,
    data_width_ISSUE             => data_width_ISSUE,
    data_be_ISSUE                => data_be_ISSUE,
    spm_rs1_ISSUE                => spm_rs1_ISSUE,
    spm_rs2_ISSUE                => spm_rs2_ISSUE,
    vec_read_rs1_ISSUE           => vec_read_rs1_ISSUE,
    vec_read_rs2_ISSUE           => vec_read_rs2_ISSUE,
    vec_write_rd_ISSUE           => vec_write_rd_ISSUE,
    vec_width_ISSUE              => vec_width_ISSUE,
    instr_valid_RF               => instr_valid_RF,
    MUL_DIV_instr_RF             => MUL_DIV_instr_RF,
    IE_instr_RF                  => IE_instr_RF,
    LSU_instr_RF                 => LSU_instr_RF,
    DSP_instr_RF                 => DSP_instr_RF,
    mul_div_instr_word_RF        => mul_div_instr_word_RF,
    ie_instr_word_RF             => ie_instr_word_RF,
    lsu_instr_word_RF            => lsu_instr_word_RF,
    dsp_instr_word_RF            => dsp_instr_word_RF,
    MUL_DIV_buff_full            => MUL_DIV_buff_full,
    IE_buff_full                 => IE_buff_full,
    LSU_buff_full                => LSU_buff_full,
    DSP_buff_full                => DSP_buff_full,
    MUL_DIV_WB_EN                => MUL_DIV_WB_EN,
    IE_WB_EN                     => IE_WB_EN,
    LSU_WB_EN                    => LSU_WB_EN,
    MUL_DIV_WB_RD_ADDR           => MUL_DIV_WB_RD_ADDR,
    IE_WB_RD_ADDR                => IE_WB_RD_ADDR,
    LSU_WB_RD_ADDR               => LSU_WB_RD_ADDR,
    speculation_ISSUE            => speculation_ISSUE,
    branch_hit                   => branch_hit,
    branch_miss                  => branch_miss,
    mul_div_ready_RF             => mul_div_ready_RF,
    ie_ready_RF                  => ie_ready_RF,
    lsu_ready_RF                 => lsu_ready_RF,
    dsp_ready_RF                 => dsp_ready_RF
  );

  RF : Registerfile
  generic map(
      accl_en                      => accl_en,
      Addr_Width                   => Addr_Width,
      SPM_ADDR_WID                 => SPM_ADDR_WID,
      RF_SIZE                      => RF_SIZE,
      RF_CEIL                      => RF_CEIL,
      XLEN                         => XLEN,
      MUL_DIV_buf_width            => MUL_DIV_buf_width,
      IE_buf_width                 => IE_buf_width,
      LSU_buf_width                => LSU_buf_width,
      DSP_buf_width                => DSP_buf_width
  )
  port map(
      clk_i                        => clk_i,
      rst_ni                       => rst_ni,
      decoded_instr_RF_MUL_DIV     => decoded_instr_RF_MUL_DIV,
      decoded_instr_RF_IE          => decoded_instr_RF_IE,
      decoded_instr_RF_LSU         => decoded_instr_RF_LSU,
      decoded_instr_RF_DSP         => decoded_instr_RF_DSP,
      decoded_instruction_MUL_DIV  => decoded_instruction_MUL_DIV,
      decoded_instruction_IE       => decoded_instruction_IE,
      decoded_instruction_LSU      => decoded_instruction_LSU,
      decoded_instruction_DSP      => decoded_instruction_DSP,
      instr_valid_RF               => instr_valid_RF,
      MUL_DIV_instr_RF             => MUL_DIV_instr_RF,
      IE_instr_RF                  => IE_instr_RF,
      LSU_instr_RF                 => LSU_instr_RF,
      DSP_instr_RF                 => DSP_instr_RF,
      MUL_DIV_instr                => MUL_DIV_instr,
      IE_instr                     => IE_instr,
      LSU_instr                    => LSU_instr,
      DSP_instr                    => DSP_instr,
      mul_div_instr_word_RF        => mul_div_instr_word_RF,
      ie_instr_word_RF             => ie_instr_word_RF,
      lsu_instr_word_RF            => lsu_instr_word_RF,
      dsp_instr_word_RF            => dsp_instr_word_RF,
      mul_div_data_rs1             => mul_div_data_rs1,
      mul_div_data_rs2             => mul_div_data_rs2,
      mul_div_data_old_rd          => mul_div_data_old_rd,
      ie_data_rs1                  => ie_data_rs1,
      ie_data_rs2                  => ie_data_rs2,
      ie_data_old_rd               => ie_data_old_rd,
      lsu_data_rs1                 => lsu_data_rs1,
      lsu_data_rs2                 => lsu_data_rs2,
      lsu_data_old_rd              => lsu_data_old_rd,
      dsp_data_rs1                 => dsp_data_rs1,
      dsp_data_rs2                 => dsp_data_rs2,
      dsp_data_rd                  => dsp_data_rd,
      mul_div_rs1_valid            => mul_div_rs1_valid,
      mul_div_rs2_valid            => mul_div_rs2_valid,
      mul_div_rd_valid             => mul_div_rd_valid,
      ie_rs1_valid                 => ie_rs1_valid,
      ie_rs2_valid                 => ie_rs2_valid,
      ie_rd_valid                  => ie_rd_valid,
      lsu_rs1_valid                => lsu_rs1_valid,
      lsu_rs2_valid                => lsu_rs2_valid,
      lsu_rd_valid                 => lsu_rd_valid,
      dsp_rs1_valid                => dsp_rs1_valid,
      dsp_rs2_valid                => dsp_rs2_valid,
      dsp_rd_valid                 => dsp_rd_valid,
      mul_div_addr_rs1             => mul_div_addr_rs1,
      mul_div_addr_rs2             => mul_div_addr_rs2,
      mul_div_addr_new_rd          => mul_div_addr_new_rd,
      ie_addr_rs1                  => ie_addr_rs1,
      ie_addr_rs2                  => ie_addr_rs2,
      ie_addr_new_rd               => ie_addr_new_rd,
      lsu_addr_rs1                 => lsu_addr_rs1,
      lsu_addr_rs2                 => lsu_addr_rs2,
      lsu_addr_new_rd              => lsu_addr_new_rd,
      dsp_addr_rs1                 => dsp_addr_rs1,
      dsp_addr_rs2                 => dsp_addr_rs2,
      dsp_addr_rd                  => dsp_addr_rd,
      signed_op                    => signed_op,
      comparator_en                => comparator_en,
      pc_IE                        => pc_IE,
      instr_word_IE                => instr_word_IE,
      load_op                      => load_op,
      store_op                     => store_op,
      data_width_ID                => data_width_ID,
      data_be                      => data_be,
      instr_word_LSU               => instr_word_LSU,
      spm_rs1_ISSUE                => spm_rs1_ISSUE,
      spm_rs2_ISSUE                => spm_rs2_ISSUE,
      vec_read_rs1_ISSUE           => vec_read_rs1_ISSUE,
      vec_read_rs2_ISSUE           => vec_read_rs2_ISSUE,
      vec_write_rd_ISSUE           => vec_write_rd_ISSUE,
      vec_width_ISSUE              => vec_width_ISSUE,
      busy_mul_div                 => busy_mul_div,
      busy_ie                      => busy_ie,
      busy_lsu                     => busy_lsu,
      busy_dsp                     => busy_dsp,
      mul_div_ready_RF             => mul_div_ready_RF,
      ie_ready_RF                  => ie_ready_RF,
      lsu_ready_RF                 => lsu_ready_RF,
      dsp_ready_RF                 => dsp_ready_RF,
      rs1_to_sc                    => rs1_to_sc,
      rs2_to_sc                    => rs2_to_sc,
      rd_to_sc                     => rd_to_sc,
      MUL_DIV_WB_EN                => MUL_DIV_WB_EN,
      IE_WB_EN                     => IE_WB_EN,
      LSU_WB_EN                    => LSU_WB_EN,
      MUL_DIV_WB                   => MUL_DIV_WB,
      IE_WB                        => IE_WB,
      LSU_WB                       => LSU_WB,
      MUL_DIV_WB_RD_ADDR           => MUL_DIV_WB_RD_ADDR,
      IE_WB_RD_ADDR                => IE_WB_RD_ADDR,
      LSU_WB_RD_ADDR               => LSU_WB_RD_ADDR,
      instr_valid_RF_CC            => instr_valid_RF_CC,
      mul_div_valid_RF_CC          => mul_div_valid_RF_CC,
      ie_valid_RF_CC               => ie_valid_RF_CC,
      lsu_valid_RF_CC              => lsu_valid_RF_CC,
      dsp_valid_RF_CC              => dsp_valid_RF_CC,
      old_rd_valid_CC              => old_rd_valid_CC,
      old_rd_addr_CC               => old_rd_addr_CC,
      mul_div_ptr_RF_CC            => mul_div_ptr_RF_CC,
      ie_ptr_RF_CC                 => ie_ptr_RF_CC,
      lsu_ptr_RF_CC                => lsu_ptr_RF_CC,
      dsp_ptr_RF_CC                => dsp_ptr_RF_CC
  );

  MUL_DIV_generate : if RV32M = 1 generate
    MUL_DIV : MULTIPLIER_DIVIDER
    generic map(
      RF_CEIL                     => RF_CEIL
    )
    port map(
      clk_i                       => clk_i,
      rst_ni                      => rst_ni,
      fetch_enable_i              => fetch_enable_i,
      irq_pending                 => irq_pending,
      dbg_req_o                   => dbg_req_o,
      signed_op                   => signed_op,
      MUL_DIV_instr               => MUL_DIV_instr,
      mul_div_addr_new_rd         => mul_div_addr_new_rd,
      decoded_instruction_MUL_DIV => decoded_instruction_MUL_DIV,
      pc_mul_div                  => pc_mul_div,
      mul_div_data_rs1            => mul_div_data_rs1,
      mul_div_data_rs2            => mul_div_data_rs2,
      mul_div_old_rd              => mul_div_old_rd,
      mul_div_old_rd_valid        => mul_div_old_rd_valid,
      mul_div_old_rd_valid_CC     => mul_div_old_rd_valid_CC,
      pc_mul_div_WB               => pc_mul_div_WB,
      mul_div_old_rd_CC           => mul_div_old_rd_CC,
      MUL_DIV_WB                  => MUL_DIV_WB,
      MUL_DIV_WB_EN               => MUL_DIV_WB_EN,
      MUL_DIV_WB_RD_ADDR          => MUL_DIV_WB_RD_ADDR,
      busy_mul_div                => busy_mul_div
    );
  end generate;


  EXECUTE : IE_STAGE
  generic map(
    RF_CEIL                 => RF_CEIL
  )
  port map(
    clk_i, rst_ni          => rst_ni,
    fetch_enable_i         => fetch_enable_i,
    irq_i                  => irq_i,
    IE_instr               => IE_instr,
    pc_IE                  => pc_IE,
    instr_word_IE          => instr_word_IE,
    ie_data_rs1            => ie_data_rs1,
    ie_data_rs2            => ie_data_rs2,
    ie_addr_rs1            => ie_addr_rs1,
    ie_addr_rs2            => ie_addr_rs2,
    ie_addr_new_rd         => ie_addr_new_rd,
    irq_pending            => irq_pending,
    csr_instr_done         => csr_instr_done,
    csr_access_denied_o    => csr_access_denied_o,
    csr_rdata_o            => csr_rdata_o,
    comparator_en          => comparator_en,
    data_addr_internal_IE  => data_addr_internal_IE,
    dbg_req_o              => dbg_req_o,
    MSTATUS                => MSTATUS(0),
    instr_rvalid_IE        => instr_rvalid_IE,
    taken_branch           => taken_branch,
    decoded_instruction_IE => decoded_instruction_IE,
    csr_addr_i             => csr_addr_i,
    ie_except_data         => ie_except_data,
    ie_csr_wdata_i         => ie_csr_wdata_i,
    csr_op_i               => csr_op_i,
    csr_wdata_en           => csr_wdata_en,
    csr_instr_req          => csr_instr_req,
    busy_ie                => busy_ie,
    jump_instr             => jump_instr,
    jump_instr_lat         => jump_instr_lat,
    WFI_Instr              => WFI_Instr,
    sleep_state            => sleep_state,
    set_branch_condition   => set_branch_condition,
    IE_except_condition    => IE_except_condition,
    set_mret_condition     => set_mret_condition,
    set_wfi_condition      => set_wfi_condition,
    ie_taken_branch        => ie_taken_branch,
    branch_instr           => branch_instr,
    branch_instr_lat       => branch_instr_lat,
    PC_offset              => PC_offset,
    served_irq             => served_irq,
    dbg_ack_i              => dbg_ack_i,
    ebreak_instr           => ebreak_instr,
    absolute_jump          => absolute_jump,
    IE_WB_EN               => IE_WB_EN,
    IE_WB                  => IE_WB,
    IE_WB_RD_ADDR          => IE_WB_RD_ADDR,
    pc_WB                  => pc_WB,
    state_IE               => state_IE
  );

  LSU : Load_Store_Unit
  generic map(
    accl_en                 => accl_en,
    SIMD                    => SIMD,
    SPM_NUM                 => SPM_NUM,
    Addr_Width              => Addr_Width,
    Data_Width              => Data_Width,
    SIMD_BITS               => SIMD_BITS,
    SPM_ADDR_WID            => SPM_ADDR_WID,
    RF_CEIL                 => RF_CEIL
    )
  port map(
    clk_i                   => clk_i,
    rst_ni                  => rst_ni,
    irq_pending             => irq_pending,
    lsu_data_rs1            => lsu_data_rs1,
    lsu_data_rs2            => lsu_data_rs2,
    lsu_data_old_rd         => lsu_data_old_rd,
    instr_word_LSU          => instr_word_LSU,
    pc_IE                   => pc_IE,
    decoded_instruction_LSU => decoded_instruction_LSU,
    data_be                 => data_be,
    data_width_ID           => data_width_ID,
    LSU_instr               => LSU_instr,
    load_op                 => load_op,
    store_op                => store_op,
    busy_lsu                => busy_lsu,
    rs1_to_sc               => rs1_to_sc,
    rs2_to_sc               => rs2_to_sc,
    rd_to_sc                => rd_to_sc,
    halt_LSU                => halt_LSU,
    data_addr_internal      => data_addr_internal,
    ls_except_data          => ls_except_data,
    ls_except_condition     => ls_except_condition,
    ls_taken_branch         => ls_taken_branch,
    amo_load                => amo_load,
    amo_load_skip           => amo_load_skip,
    amo_store               => amo_store,
    misaligned_err          => misaligned_err,
    lsu_addr_rs1            => lsu_addr_rs1,
    lsu_addr_rs2            => lsu_addr_rs2,
    lsu_addr_new_rd         => lsu_addr_new_rd,
    ls_data_gnt_i           => ls_data_gnt_i,
    ls_sci_wr_gnt           => ls_sci_wr_gnt,
    ls_sc_data_read_wire    => ls_sc_data_read_wire,
    state_LS                => state_LS,
    harc_LS_wire            => harc_LS_wire,
    sc_word_count_wire      => sc_word_count_wire,
    spm_bcast               => spm_bcast,
    kmemld_inflight         => kmemld_inflight,
    kmemstr_inflight        => kmemstr_inflight,
    ls_sci_req              => ls_sci_req,
    ls_sci_we               => ls_sci_we,
    ls_sc_read_addr         => ls_sc_read_addr,
    ls_sc_write_addr        => ls_sc_write_addr,
    ls_sc_data_write_wire   => ls_sc_data_write_wire,
    LSU_WB_EN               => LSU_WB_EN,
    LSU_WB                  => LSU_WB,
    LSU_WB_RD_ADDR          => LSU_WB_RD_ADDR,
    data_req_o              => data_req_o,
    data_gnt_i              => data_gnt_i,
    data_rvalid_i           => data_rvalid_i,
    data_we_o               => data_we_o,
    data_be_o               => data_be_o,
    data_addr_o             => data_addr_o,
    data_wdata_o            => data_wdata_o,
    data_rdata_i            => data_rdata_i,
    data_err_i              => data_err_i
  );

  CC : COMMIT_COUNTER
  generic map(
    RF_SIZE                      => RF_SIZE,
    RF_CEIL                      => RF_CEIL
  )
  port map(
    clk_i                        => clk_i,
    rst_ni                       => rst_ni,
    mul_div_old_rd_valid_CC      => mul_div_old_rd_valid_CC,
    ie_old_rd_valid_CC           => ie_old_rd_valid_CC,
    lsu_old_rd_valid_CC          => lsu_old_rd_valid_CC,
    mul_div_old_rd_CC            => mul_div_old_rd_CC,
    ie_old_rd_CC                 => ie_old_rd_CC,
    lsu_old_rd_CC                => lsu_old_rd_CC,
    mul_div_addr_CC              => mul_div_addr_CC,
    ie_rob_addr_CC               => ie_rob_addr_CC,
    lsu_rob_addr_CC              => lsu_rob_addr_CC,
    instr_valid_RENAME_CC        => instr_valid_RENAME_CC,
    rs1_rename_valid_CC          => rs1_rename_valid_CC,
    rs2_rename_valid_CC          => rs2_rename_valid_CC,
    rd_read_only_rename_valid_CC => rd_read_only_rename_valid_CC,
    new_rd_rename_valid_CC       => new_rd_rename_valid_CC,
    old_rd_rename_valid_CC       => old_rd_rename_valid_CC,
    rs1_rename_addr_CC           => rs1_rename_addr_CC,
    rs2_rename_addr_CC           => rs2_rename_addr_CC,
    old_rd_rename_addr_CC        => old_rd_rename_addr_CC,
    new_rd_rename_addr_CC        => new_rd_rename_addr_CC,
    FRL_commit_addr              => FRL_commit_addr,
    FRL_commit_en                => FRL_commit_en,
    commit_count                 => commit_count,
    old_rd_count                 => old_rd_count,
    instr_valid_RF_CC            => instr_valid_RF_CC,
    mul_div_valid_RF_CC          => mul_div_valid_RF_CC,
    ie_valid_RF_CC               => ie_valid_RF_CC,
    lsu_valid_RF_CC              => lsu_valid_RF_CC,
    dsp_valid_RF_CC              => dsp_valid_RF_CC,
    old_rd_valid_CC              => old_rd_valid_CC,
    old_rd_addr_CC               => old_rd_addr_CC,
    mul_div_ptr_RF_CC            => mul_div_ptr_RF_CC,
    ie_ptr_RF_CC                 => ie_ptr_RF_CC,
    lsu_ptr_RF_CC                => lsu_ptr_RF_CC,
    dsp_ptr_RF_CC                => dsp_ptr_RF_CC
  );

  ACCL_generate : if accl_en = 1 generate

  DSP : DSP_Unit
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
    harc_EXEC                  => 0,
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
    busy_dsp(0)                => busy_dsp,
    dsp_data_gnt_i             => dsp_data_gnt_i,
    dsp_sci_wr_gnt             => dsp_sci_wr_gnt,
    dsp_sc_data_read           => dsp_sc_data_read,
    dsp_sc_read_addr           => dsp_sc_read_addr,
    dsp_to_sc                  => dsp_to_sc,
    dsp_sc_data_write_wire     => dsp_sc_data_write_wire,
    dsp_we_word                => dsp_we_word,
    dsp_sc_write_addr          => dsp_sc_write_addr,
    dsp_sci_we                 => dsp_sci_we,
    dsp_sci_req                => dsp_sci_req
	);

  SCI : Scratchpad_memory_interface
  generic map(
    accl_en                       => accl_en, 
    SPM_NUM                       => SPM_NUM,  
    Addr_Width                    => Addr_Width, 
    SIMD                          => SIMD, 
    ----------------------------------------------
    ACCL_NUM                      => ACCL_NUM, 
    SIMD_BITS                     => SIMD_BITS, 
    Data_Width                    => Data_Width, 
    SIMD_Width                    => SIMD_Width
  )
  port map(  
    clk_i                         => clk_i,
    rst_ni                        => rst_ni,
    data_rvalid_i                 => data_rvalid_i,
    state_LS                      => state_LS,
    sc_word_count_wire            => sc_word_count_wire,
    spm_bcast                     => spm_bcast,
    harc_LS_wire                  => harc_LS_wire,
    dsp_we_word                   => dsp_we_word,
    ls_sc_data_write_wire         => ls_sc_data_write_wire,
    dsp_sc_data_write_wire        => dsp_sc_data_write_wire,
    ls_sc_read_addr               => ls_sc_read_addr,
    ls_sc_write_addr              => ls_sc_write_addr,
    dsp_sc_write_addr             => dsp_sc_write_addr,
    ls_sci_req                    => ls_sci_req,
    ls_sci_we                     => ls_sci_we,
    dsp_sci_req                   => dsp_sci_req,
    dsp_sci_we                    => dsp_sci_we,
    kmemld_inflight               => kmemld_inflight,
    kmemstr_inflight              => kmemstr_inflight,
    dsp_to_sc                     => dsp_to_sc,
    dsp_sc_read_addr              => dsp_sc_read_addr,	  
    dsp_sc_data_read              => dsp_sc_data_read,
    ls_sc_data_read_wire          => ls_sc_data_read_wire,
    ls_sci_wr_gnt                 => ls_sci_wr_gnt,
    dsp_sci_wr_gnt                => dsp_sci_wr_gnt,
    ls_data_gnt_i                 => ls_data_gnt_i,
    dsp_data_gnt_i                => dsp_data_gnt_i
	);

  end generate;

--------------------------------------------------------------------- end of PIPE -----------------
---------------------------------------------------------------------------------------------------

end Pipe;
--------------------------------------------------------------------------------------------------
-- END of Processing-Pipeline architecture -------------------------------------------------------
--------------------------------------------------------------------------------------------------