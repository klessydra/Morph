--------------------------------------------------------------------------------------------------------------
--  VCU Unit --                                                                                             --
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

entity VCU is
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
    MVSIZE                     : in  array_2d(THREAD_POOL_SIZE-1 downto 0)(Addr_Width downto 0);
    MVTYPE                     : in  array_2d(THREAD_POOL_SIZE-1 downto 0)(3 downto 0);
    MPSCLFAC                   : in  array_2d(THREAD_POOL_SIZE-1 downto 0)(4 downto 0);
    dsp_except_data            : out array_2d(ACCL_NUM-1 downto 0)(31 downto 0);
  -- Program Counter Signals
    dsp_taken_branch           : out std_logic_vector(ACCL_NUM-1 downto 0);
    dsp_except_condition       : out std_logic_vector(ACCL_NUM-1 downto 0);
    -- ID_Stage Signals
    decoded_instruction_DSP    : in  std_logic_vector(DSP_UNIT_INSTR_SET_SIZE-1 downto 0);
    harc_EXEC                  : in  natural range THREAD_POOL_SIZE-1 downto 0;
    pc_IE                      : in  std_logic_vector(31 downto 0);
    RS1_Data_IE                : in  std_logic_vector(31 downto 0);
    RS2_Data_IE                : in  std_logic_vector(31 downto 0);
    RD_Data_IE                 : in  std_logic_vector(Addr_Width -1 downto 0);
    dsp_instr_req              : in  std_logic_vector(ACCL_NUM-1 downto 0);
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
  end entity;

  architecture Coprocesso of VCU is

  subtype harc_range is natural range THREAD_POOL_SIZE - 1 downto 0;
  subtype accl_range is integer range ACCL_NUM-1 downto 0;
  subtype fu_range   is integer range FU_NUM - 1 downto 0;

  signal dsp_data_gnt_i         : std_logic_vector(accl_range);
  signal dsp_sci_wr_gnt         : std_logic_vector(accl_range);
  signal dsp_sc_data_read       : array_3d(accl_range)(1 downto 0)(SIMD_Width-1 downto 0);
  signal dsp_sc_read_addr       : array_3d(accl_range)(1 downto 0)(Addr_Width-1 downto 0);
  signal dsp_to_sc              : array_3d(accl_range)(SPM_NUM-1 downto 0)(1 downto 0);
  signal dsp_sc_write_addr      : array_2d(accl_range)(Addr_Width-1 downto 0);
  signal dsp_sc_data_write_wire : array_2d(accl_range)(SIMD_Width - 1 downto 0);
  signal dsp_sci_req            : array_2d(accl_range)(SPM_NUM-1 downto 0);
  signal dsp_sci_we             : array_2d(accl_range)(SPM_NUM-1 downto 0);
  signal dsp_we_word            : array_2d(accl_range)(SIMD-1 downto 0);

  component DSP_Unit is
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
    SPM_NUM                    : natural; 
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
    harc_LS_wire               : in  integer range ACCL_NUM-1 downto 0;
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

  begin


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


  end architecture;