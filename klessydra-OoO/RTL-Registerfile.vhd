-- ieee packages ------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_misc.all;
use ieee.numeric_std.all;
use std.textio.all;

-- local packages ------------
use work.riscv_klessydra.all;

entity Registerfile is
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
      instr_valid_RF               : in  std_logic;  -- indicates that there is a valid input instruction
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
      mul_div_data_new_rd          : out std_logic_vector(31 downto 0);
      ie_data_rs1                  : out std_logic_vector(31 downto 0);
      ie_data_rs2                  : out std_logic_vector(31 downto 0);
      ie_data_new_rd               : out std_logic_vector(31 downto 0);
      lsu_data_rs1                 : out std_logic_vector(31 downto 0);
      lsu_data_rs2                 : out std_logic_vector(31 downto 0);
      lsu_data_new_rd              : out std_logic_vector(31 downto 0);
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
      new_rd_valid_CC              : out std_logic_vector(2 downto 0);-- contains the valids of the old_rds
      new_rd_addr_CC               : out array_2D(2 downto 0)(RF_CEIL-1 downto 0);
      mul_div_ptr_RF_CC            : out array_2D(2 downto 0)(RF_CEIL-1 downto 0);
      ie_ptr_RF_CC                 : out array_2D(2 downto 0)(RF_CEIL-1 downto 0);
      lsu_ptr_RF_CC                : out array_2D(2 downto 0)(RF_CEIL-1 downto 0);
      dsp_ptr_RF_CC                : out array_2D(2 downto 0)(RF_CEIL-1 downto 0);
      -- Branch predictor Signals
      branch_miss                  : in  std_logic;
      speculative_mul_div          : out std_logic;
      speculative_lsu              : out std_logic;
      -- Recovery Buffer Signals
      regfile_recovery             : in  std_logic;
      rf_addr_rec                  : in  array_2D(2 downto 0)(RF_CEIL-1 downto 0);
      rf_data_rec                  : in  array_2D(2 downto 0)(XLEN-1 downto 0)
    );
end entity; --------------------------------------

architecture Regfile of Registerfile is

constant RF_READ_PORTS          : natural := 12;
constant RF_WRITE_PORTS         : natural := 3;
constant RF_BANK_NUM            : natural := RF_READ_PORTS*RF_WRITE_PORTS;

signal MUL_DIV_instr_RF_int     : std_logic;
signal MUL_DIV_instr_RF_int_lat : std_logic;
signal IE_instr_RF_int          : std_logic;
signal IE_instr_RF_int_lat      : std_logic;
signal LSU_instr_RF_int         : std_logic;
signal LSU_instr_RF_int_lat     : std_logic;
signal DSP_instr_RF_int         : std_logic;
signal DSP_instr_RF_int_lat     : std_logic;

signal RF_READ_EN               : std_logic_vector(RF_BANK_NUM-1 downto 0);
signal RF_READ_EN_LAT           : std_logic_vector(RF_BANK_NUM-1 downto 0);
signal RF_WRITE_EN              : std_logic_vector(RF_BANK_NUM-1 downto 0);
signal RF_READ_DATA             : array_2D(RF_BANK_NUM-1 downto 0)(31 downto 0);
signal RF_WRITE_DATA            : array_2D(RF_BANK_NUM-1 downto 0)(31 downto 0);
signal rf_read_addr             : array_2D(RF_BANK_NUM-1 downto 0)(RF_CEIL-1 downto 0);
signal rf_read_addr_lat         : array_2D(RF_BANK_NUM-1 downto 0)(RF_CEIL-1 downto 0);
signal rf_write_addr            : array_2D(RF_BANK_NUM-1 downto 0)(RF_CEIL-1 downto 0);
signal regfile                  : array_3D(RF_BANK_NUM-1 downto 0)(RF_SIZE-1 downto 0)(31 downto 0); -- MULTI PORT REGISTERFILE
signal LVT                      : array_2D(RF_SIZE-1 downto 0)(1 downto 0); -- LIVE VALUE TABLE

signal RF_rs1_to_sc             : std_logic_vector(SPM_ADDR_WID-1 downto 0);
signal RF_rs2_to_sc             : std_logic_vector(SPM_ADDR_WID-1 downto 0);
signal RF_rd_to_sc              : std_logic_vector(SPM_ADDR_WID-1 downto 0);
-- FU operand RF addresses
signal mul_div_addr_rs1_wire    : std_logic_vector(RF_CEIL-1 downto 0); -- address to index the register to read
signal mul_div_addr_rs2_wire    : std_logic_vector(RF_CEIL-1 downto 0); -- address to index the register to read
--signal mul_div_addr_old_rd_wire : std_logic_vector(RF_CEIL-1 downto 0);
signal mul_div_addr_new_rd_wire : std_logic_vector(RF_CEIL-1 downto 0);
signal ie_addr_rs1_wire         : std_logic_vector(RF_CEIL-1 downto 0); -- address to index the register to read
signal ie_addr_rs2_wire         : std_logic_vector(RF_CEIL-1 downto 0); -- address to index the register to read
--signal ie_addr_old_rd_wire      : std_logic_vector(RF_CEIL-1 downto 0);
signal ie_addr_new_rd_wire      : std_logic_vector(RF_CEIL-1 downto 0);
signal lsu_addr_rs1_wire        : std_logic_vector(RF_CEIL-1 downto 0); -- address to index the register to read
signal lsu_addr_rs2_wire        : std_logic_vector(RF_CEIL-1 downto 0); -- address to index the register to read
--signal lsu_addr_old_rd_wire     : std_logic_vector(RF_CEIL-1 downto 0); -- address to index the register to read
signal lsu_addr_new_rd_wire     : std_logic_vector(RF_CEIL-1 downto 0); -- address to index the register to read
signal dsp_addr_rs1_wire        : std_logic_vector(RF_CEIL-1 downto 0); -- address to index the register to read
signal dsp_addr_rs2_wire        : std_logic_vector(RF_CEIL-1 downto 0); -- address to index the register to read
signal dsp_addr_rd_wire         : std_logic_vector(RF_CEIL-1 downto 0); -- address to index the register to read

signal mul_div_valid            : std_logic_vector(2 downto 0); -- contaim the rs1, rs2, and rd valids of the instructions
signal ie_valid                 : std_logic_vector(2 downto 0); -- contaim the rs1, rs2, and rd valids of the instructions
signal lsu_valid                : std_logic_vector(2 downto 0); -- contaim the rs1, rs2, and rd valids of the instructions
signal dsp_valid                : std_logic_vector(2 downto 0); -- contaim the rs1, rs2, and rd valids of the instructions

signal flush_mul_div            : std_logic;
signal flush_ie                 : std_logic;
signal flush_lsu                : std_logic;
signal flush_dsp                : std_logic;

attribute ram_style : string;
attribute ram_style of regfile : signal is "block";

begin

  -- logic used to latch the input instruction
  MUL_DIV_instr_RF_int <= MUL_DIV_instr_RF or MUL_DIV_instr_RF_int_lat when MUL_DIV_instr  = '0' else MUL_DIV_instr_RF;
  IE_instr_RF_int      <= IE_instr_RF      or IE_instr_RF_int_lat      when IE_instr       = '0' else IE_instr_RF;
  LSU_instr_RF_int     <= LSU_instr_RF     or LSU_instr_RF_int_lat     when LSU_instr      = '0' else LSU_instr_RF;
  DSP_instr_RF_int     <= DSP_instr_RF     or DSP_instr_RF_int_lat     when DSP_instr      = '0' else DSP_instr_RF;

  process(clk_i, rst_ni)
  begin
    if rst_ni = '0' then
      MUL_DIV_instr_RF_int_lat <= '0';
      IE_instr_RF_int_lat      <= '0';
      LSU_instr_RF_int_lat     <= '0';
      DSP_instr_RF_int_lat     <= '0';
    elsif rising_edge(clk_i) then
      if MUL_DIV_instr_RF = '1' then
        MUL_DIV_instr_RF_int_lat <= '1';
      elsif MUL_DIV_instr = '1' and MUL_DIV_instr_RF = '0' then
        MUL_DIV_instr_RF_int_lat <= '0';
      end if;
      if IE_instr_RF = '1' then
        IE_instr_RF_int_lat <= '1';
      elsif IE_instr = '1' and IE_instr_RF = '0' then
        IE_instr_RF_int_lat <= '0';
      end if;
      if LSU_instr_RF = '1' then
        LSU_instr_RF_int_lat <= '1';
      elsif LSU_instr = '1' and LSU_instr_RF = '0' then
        LSU_instr_RF_int_lat <= '0';
      end if;
      if DSP_instr_RF = '1' then
        DSP_instr_RF_int_lat <= '1';
      elsif DSP_instr = '1' and DSP_instr_RF = '0' then
        DSP_instr_RF_int_lat <= '0';
      end if;
    end if;
  end process;  


  mul_div_addr_rs1_wire     <= mul_div_instr_word_RF(   RF_CEIL -1 downto 0);
  mul_div_addr_rs2_wire     <= mul_div_instr_word_RF((2*RF_CEIL)-1 downto   RF_CEIL);
  --mul_div_addr_old_rd_wire  <= mul_div_instr_word_RF((3*RF_CEIL)-1 downto 2*RF_CEIL);
  mul_div_addr_new_rd_wire  <= mul_div_instr_word_RF((3*RF_CEIL)-1 downto 2*RF_CEIL);
  ie_addr_rs1_wire          <= ie_instr_word_RF(        RF_CEIL -1 downto 0);
  ie_addr_rs2_wire          <= ie_instr_word_RF(     (2*RF_CEIL)-1 downto   RF_CEIL);
  --ie_addr_old_rd_wire       <= ie_instr_word_RF(     (3*RF_CEIL)-1 downto 2*RF_CEIL);
  ie_addr_new_rd_wire       <= ie_instr_word_RF(     (3*RF_CEIL)-1 downto 2*RF_CEIL);
  lsu_addr_rs1_wire         <= lsu_instr_word_RF(       RF_CEIL -1 downto 0);
  lsu_addr_rs2_wire         <= lsu_instr_word_RF(    (2*RF_CEIL)-1 downto RF_CEIL);
  --lsu_addr_old_rd_wire      <= lsu_instr_word_RF(    (3*RF_CEIL)-1 downto 2*RF_CEIL);
  lsu_addr_new_rd_wire      <= lsu_instr_word_RF(    (3*RF_CEIL)-1 downto 2*RF_CEIL);
  dsp_addr_rs1_wire         <= dsp_instr_word_RF(       RF_CEIL -1 downto 0);
  dsp_addr_rs2_wire         <= dsp_instr_word_RF(    (2*RF_CEIL)-1 downto RF_CEIL);
  dsp_addr_rd_wire          <= dsp_instr_word_RF(    (3*RF_CEIL)-1 downto 2*RF_CEIL); -- old_rd is mapped differently for these instructions

  mul_div_valid             <= mul_div_instr_word_RF((4*RF_CEIL)+2 downto 4*RF_CEIL);
  ie_valid                  <= ie_instr_word_RF(     (4*RF_CEIL)+2 downto 4*RF_CEIL);
  lsu_valid                 <= (lsu_instr_word_RF(   (4*RF_CEIL)+4) & "00") or (lsu_instr_word_RF((4*RF_CEIL)+2 downto 4*RF_CEIL)); -- The 'or' is to select rd_read_only or old_rd valids
  dsp_valid                 <= dsp_instr_word_RF(    (3*RF_CEIL)+2 downto 3*RF_CEIL);

  RF_SIGNAL_MAPPING : for i in 0 to RF_WRITE_PORTS-1 generate 
  RF_SIGNAL_MAPPING : for j in 0 to 2 generate -- loops through the ports of each execution unit "rs1, rs2, old_rd"

    -- Enable the appropriate memory bank
    RF_READ_EN((RF_READ_PORTS*i)+j+0) <= '1' when mul_div_valid(j) = '1' and busy_mul_div = '0' and (LVT(to_integer(unsigned(rf_read_addr(j+0)))) = std_logic_vector(to_unsigned(i,2))) else '0';
    RF_READ_EN((RF_READ_PORTS*i)+j+3) <= '1' when ie_valid(j)      = '1' and busy_ie      = '0' and (LVT(to_integer(unsigned(rf_read_addr(j+3)))) = std_logic_vector(to_unsigned(i,2))) else '0';
    RF_READ_EN((RF_READ_PORTS*i)+j+6) <= '1' when lsu_valid(j)     = '1' and busy_lsu     = '0' and (LVT(to_integer(unsigned(rf_read_addr(j+6)))) = std_logic_vector(to_unsigned(i,2))) else '0';
    RF_READ_EN((RF_READ_PORTS*i)+j+9) <= '1' when dsp_valid(j)     = '1' and busy_dsp     = '0' and (LVT(to_integer(unsigned(rf_read_addr(j+9)))) = std_logic_vector(to_unsigned(i,2))) else '0';

  end generate;
  end generate;

  RF_WR_PORT_LOOP : for i in 0 to RF_WRITE_PORTS-1 generate 

    rf_read_addr((RF_READ_PORTS*i)+0)  <= mul_div_addr_rs1_wire;
    rf_read_addr((RF_READ_PORTS*i)+1)  <= mul_div_addr_rs2_wire;
    rf_read_addr((RF_READ_PORTS*i)+2)  <= mul_div_addr_new_rd_wire;
    rf_read_addr((RF_READ_PORTS*i)+3)  <= ie_addr_rs1_wire;
    rf_read_addr((RF_READ_PORTS*i)+4)  <= ie_addr_rs2_wire;
    rf_read_addr((RF_READ_PORTS*i)+5)  <= ie_addr_new_rd_wire;
    rf_read_addr((RF_READ_PORTS*i)+6)  <= lsu_addr_rs1_wire;
    rf_read_addr((RF_READ_PORTS*i)+7)  <= lsu_addr_rs2_wire;
    rf_read_addr((RF_READ_PORTS*i)+8)  <= lsu_addr_new_rd_wire;
    rf_read_addr((RF_READ_PORTS*i)+9)  <= dsp_addr_rs1_wire;
    rf_read_addr((RF_READ_PORTS*i)+10) <= dsp_addr_rs2_wire;
    rf_read_addr((RF_READ_PORTS*i)+11) <= dsp_addr_rd_wire;

    -- Based on the value written in rf_read_addr(i) of the Live Value Table, we choose the write port
    -- the below might create a latch if, and putting an others condition might ruin the outcome of the next loop index
    --mul_div_data_rs1    <= RF_READ_DATA((i*RF_READ_PORTS)+0)  when RF_READ_EN((i*RF_READ_PORTS)+0)  = '1' and unsigned(LVT(to_integer(unsigned(mul_div_addr_rs1_wire)))) = i;
    --mul_div_data_rs2    <= RF_READ_DATA((i*RF_READ_PORTS)+1)  when RF_READ_EN((i*RF_READ_PORTS)+1)  = '1' and unsigned(LVT(to_integer(unsigned(mul_div_addr_rs2_wire)))) = i;
    --mul_div_data_old_rd <= RF_READ_DATA((i*RF_READ_PORTS)+2)  when RF_READ_EN((i*RF_READ_PORTS)+2)  = '1' and unsigned(LVT(to_integer(unsigned(mul_div_addr_old_rd_wire)))) = i;
    --ie_data_rs1         <= RF_READ_DATA((i*RF_READ_PORTS)+3)  when RF_READ_EN((i*RF_READ_PORTS)+3)  = '1' and unsigned(LVT(to_integer(unsigned(ie_addr_rs1_wire)))) = i;
    --ie_data_rs2         <= RF_READ_DATA((i*RF_READ_PORTS)+4)  when RF_READ_EN((i*RF_READ_PORTS)+4)  = '1' and unsigned(LVT(to_integer(unsigned(ie_addr_rs2_wire)))) = i;
    --ie_data_old_rd      <= RF_READ_DATA((i*RF_READ_PORTS)+5)  when RF_READ_EN((i*RF_READ_PORTS)+5)  = '1' and unsigned(LVT(to_integer(unsigned(ie_addr_old_rd_wire)))) = i;
    --lsu_data_rs1        <= RF_READ_DATA((i*RF_READ_PORTS)+6)  when RF_READ_EN((i*RF_READ_PORTS)+6)  = '1' and unsigned(LVT(to_integer(unsigned(lsu_addr_rs1_wire)))) = i;
    --lsu_data_rs2        <= RF_READ_DATA((i*RF_READ_PORTS)+7)  when RF_READ_EN((i*RF_READ_PORTS)+7)  = '1' and unsigned(LVT(to_integer(unsigned(lsu_addr_rs2_wire)))) = i;
    --lsu_data_old_rd     <= RF_READ_DATA((i*RF_READ_PORTS)+8)  when RF_READ_EN((i*RF_READ_PORTS)+8)  = '1' and unsigned(LVT(to_integer(unsigned(lsu_addr_old_rd_wire)))) = i;
    --dsp_data_rs1        <= RF_READ_DATA((i*RF_READ_PORTS)+9)  when RF_READ_EN((i*RF_READ_PORTS)+9)  = '1' and unsigned(LVT(to_integer(unsigned(dsp_addr_rs1_wire)))) = i;
    --dsp_data_rs2        <= RF_READ_DATA((i*RF_READ_PORTS)+10) when RF_READ_EN((i*RF_READ_PORTS)+10) = '1' and unsigned(LVT(to_integer(unsigned(dsp_addr_rs2_wire)))) = i;
    --dsp_data_rd         <= RF_READ_DATA((i*RF_READ_PORTS)+11) when RF_READ_EN((i*RF_READ_PORTS)+11) = '1' and unsigned(LVT(to_integer(unsigned(dsp_addr_rd_wire)))) = i;

  end generate;

  mul_div_data_rs1    <= RF_READ_DATA((0*RF_READ_PORTS)+0)   when RF_READ_EN_LAT((0*RF_READ_PORTS)+0)  = '1' and unsigned(LVT(to_integer(unsigned(rf_read_addr_lat((RF_READ_PORTS*0)+0))))) = 0 else
                         RF_READ_DATA((1*RF_READ_PORTS)+0)   when RF_READ_EN_LAT((1*RF_READ_PORTS)+0)  = '1' and unsigned(LVT(to_integer(unsigned(rf_read_addr_lat((RF_READ_PORTS*1)+0))))) = 1 else
                         RF_READ_DATA((2*RF_READ_PORTS)+0)   when RF_READ_EN_LAT((2*RF_READ_PORTS)+0)  = '1' and unsigned(LVT(to_integer(unsigned(rf_read_addr_lat((RF_READ_PORTS*2)+0))))) = 2 else
                         (others => '0');

  mul_div_data_rs2    <= RF_READ_DATA((0*RF_READ_PORTS)+1)   when RF_READ_EN_LAT((0*RF_READ_PORTS)+1)  = '1' and unsigned(LVT(to_integer(unsigned(rf_read_addr_lat((RF_READ_PORTS*0)+1))))) = 0 else
                         RF_READ_DATA((1*RF_READ_PORTS)+1)   when RF_READ_EN_LAT((1*RF_READ_PORTS)+1)  = '1' and unsigned(LVT(to_integer(unsigned(rf_read_addr_lat((RF_READ_PORTS*1)+1))))) = 1 else
                         RF_READ_DATA((2*RF_READ_PORTS)+1)   when RF_READ_EN_LAT((2*RF_READ_PORTS)+1)  = '1' and unsigned(LVT(to_integer(unsigned(rf_read_addr_lat((RF_READ_PORTS*2)+1))))) = 2 else
                         (others => '0');

  mul_div_data_new_rd <= RF_READ_DATA((0*RF_READ_PORTS)+2)   when RF_READ_EN_LAT((0*RF_READ_PORTS)+2)  = '1' and unsigned(LVT(to_integer(unsigned(rf_read_addr_lat((RF_READ_PORTS*0)+2))))) = 0 else
                         RF_READ_DATA((1*RF_READ_PORTS)+2)   when RF_READ_EN_LAT((1*RF_READ_PORTS)+2)  = '1' and unsigned(LVT(to_integer(unsigned(rf_read_addr_lat((RF_READ_PORTS*1)+2))))) = 1 else
                         RF_READ_DATA((2*RF_READ_PORTS)+2)   when RF_READ_EN_LAT((2*RF_READ_PORTS)+2)  = '1' and unsigned(LVT(to_integer(unsigned(rf_read_addr_lat((RF_READ_PORTS*2)+2))))) = 2 else
                         (others => '0');

  ie_data_rs1         <= RF_READ_DATA((0*RF_READ_PORTS)+3)   when RF_READ_EN_LAT((0*RF_READ_PORTS)+3)  = '1' and unsigned(LVT(to_integer(unsigned(rf_read_addr_lat((RF_READ_PORTS*0)+3))))) = 0 else
                         RF_READ_DATA((1*RF_READ_PORTS)+3)   when RF_READ_EN_LAT((1*RF_READ_PORTS)+3)  = '1' and unsigned(LVT(to_integer(unsigned(rf_read_addr_lat((RF_READ_PORTS*1)+3))))) = 1 else
                         RF_READ_DATA((2*RF_READ_PORTS)+3)   when RF_READ_EN_LAT((2*RF_READ_PORTS)+3)  = '1' and unsigned(LVT(to_integer(unsigned(rf_read_addr_lat((RF_READ_PORTS*2)+3))))) = 2 else
                         (others => '0');

  ie_data_rs2         <= RF_READ_DATA((0*RF_READ_PORTS)+4)   when RF_READ_EN_LAT((0*RF_READ_PORTS)+4)  = '1' and unsigned(LVT(to_integer(unsigned(rf_read_addr_lat((RF_READ_PORTS*0)+4))))) = 0 else
                         RF_READ_DATA((1*RF_READ_PORTS)+4)   when RF_READ_EN_LAT((1*RF_READ_PORTS)+4)  = '1' and unsigned(LVT(to_integer(unsigned(rf_read_addr_lat((RF_READ_PORTS*1)+4))))) = 1 else
                         RF_READ_DATA((2*RF_READ_PORTS)+4)   when RF_READ_EN_LAT((2*RF_READ_PORTS)+4)  = '1' and unsigned(LVT(to_integer(unsigned(rf_read_addr_lat((RF_READ_PORTS*2)+4))))) = 2 else
                         (others => '0');

  ie_data_new_rd      <= RF_READ_DATA((0*RF_READ_PORTS)+5)   when RF_READ_EN_LAT((0*RF_READ_PORTS)+5)  = '1' and unsigned(LVT(to_integer(unsigned(rf_read_addr_lat((RF_READ_PORTS*0)+5))))) = 0 else
                         RF_READ_DATA((1*RF_READ_PORTS)+5)   when RF_READ_EN_LAT((1*RF_READ_PORTS)+5)  = '1' and unsigned(LVT(to_integer(unsigned(rf_read_addr_lat((RF_READ_PORTS*1)+5))))) = 1 else
                         RF_READ_DATA((2*RF_READ_PORTS)+5)   when RF_READ_EN_LAT((2*RF_READ_PORTS)+5)  = '1' and unsigned(LVT(to_integer(unsigned(rf_read_addr_lat((RF_READ_PORTS*2)+5))))) = 2 else
                         (others => '0');

  lsu_data_rs1        <= RF_READ_DATA((0*RF_READ_PORTS)+6)   when RF_READ_EN_LAT((0*RF_READ_PORTS)+6)  = '1' and unsigned(LVT(to_integer(unsigned(rf_read_addr_lat((RF_READ_PORTS*0)+6))))) = 0 else
                         RF_READ_DATA((1*RF_READ_PORTS)+6)   when RF_READ_EN_LAT((1*RF_READ_PORTS)+6)  = '1' and unsigned(LVT(to_integer(unsigned(rf_read_addr_lat((RF_READ_PORTS*1)+6))))) = 1 else
                         RF_READ_DATA((2*RF_READ_PORTS)+6)   when RF_READ_EN_LAT((2*RF_READ_PORTS)+6)  = '1' and unsigned(LVT(to_integer(unsigned(rf_read_addr_lat((RF_READ_PORTS*2)+6))))) = 2 else
                         (others => '0');

  lsu_data_rs2        <= RF_READ_DATA((0*RF_READ_PORTS)+7)   when RF_READ_EN_LAT((0*RF_READ_PORTS)+7)  = '1' and unsigned(LVT(to_integer(unsigned(rf_read_addr_lat((RF_READ_PORTS*0)+7))))) = 0 else
                         RF_READ_DATA((1*RF_READ_PORTS)+7)   when RF_READ_EN_LAT((1*RF_READ_PORTS)+7)  = '1' and unsigned(LVT(to_integer(unsigned(rf_read_addr_lat((RF_READ_PORTS*1)+7))))) = 1 else
                         RF_READ_DATA((2*RF_READ_PORTS)+7)   when RF_READ_EN_LAT((2*RF_READ_PORTS)+7)  = '1' and unsigned(LVT(to_integer(unsigned(rf_read_addr_lat((RF_READ_PORTS*2)+7))))) = 2 else
                         (others => '0');

  lsu_data_new_rd     <= RF_READ_DATA((0*RF_READ_PORTS)+8)   when RF_READ_EN_LAT((0*RF_READ_PORTS)+8)  = '1' and unsigned(LVT(to_integer(unsigned(rf_read_addr_lat((RF_READ_PORTS*0)+8))))) = 0 else
                         RF_READ_DATA((1*RF_READ_PORTS)+8)   when RF_READ_EN_LAT((1*RF_READ_PORTS)+8)  = '1' and unsigned(LVT(to_integer(unsigned(rf_read_addr_lat((RF_READ_PORTS*1)+8))))) = 1 else
                         RF_READ_DATA((2*RF_READ_PORTS)+8)   when RF_READ_EN_LAT((2*RF_READ_PORTS)+8)  = '1' and unsigned(LVT(to_integer(unsigned(rf_read_addr_lat((RF_READ_PORTS*2)+8))))) = 2 else
                         (others => '0');

  dsp_data_rs1        <= RF_READ_DATA((0*RF_READ_PORTS)+9)   when RF_READ_EN_LAT((0*RF_READ_PORTS)+9)  = '1' and unsigned(LVT(to_integer(unsigned(rf_read_addr_lat((RF_READ_PORTS*0)+9))))) = 0 else
                         RF_READ_DATA((1*RF_READ_PORTS)+9)   when RF_READ_EN_LAT((1*RF_READ_PORTS)+9)  = '1' and unsigned(LVT(to_integer(unsigned(rf_read_addr_lat((RF_READ_PORTS*1)+9))))) = 1 else
                         RF_READ_DATA((2*RF_READ_PORTS)+9)   when RF_READ_EN_LAT((2*RF_READ_PORTS)+9)  = '1' and unsigned(LVT(to_integer(unsigned(rf_read_addr_lat((RF_READ_PORTS*2)+9))))) = 2 else
                         (others => '0');

  dsp_data_rs2        <= RF_READ_DATA((0*RF_READ_PORTS)+10)  when RF_READ_EN_LAT((0*RF_READ_PORTS)+10)  = '1' and unsigned(LVT(to_integer(unsigned(rf_read_addr_lat((RF_READ_PORTS*0)+10))))) = 0 else
                         RF_READ_DATA((1*RF_READ_PORTS)+10)  when RF_READ_EN_LAT((1*RF_READ_PORTS)+10)  = '1' and unsigned(LVT(to_integer(unsigned(rf_read_addr_lat((RF_READ_PORTS*1)+10))))) = 1 else
                         RF_READ_DATA((2*RF_READ_PORTS)+10)  when RF_READ_EN_LAT((2*RF_READ_PORTS)+10)  = '1' and unsigned(LVT(to_integer(unsigned(rf_read_addr_lat((RF_READ_PORTS*2)+10))))) = 2 else
                         (others => '0');

  dsp_data_rd         <= RF_READ_DATA((0*RF_READ_PORTS)+11)  when RF_READ_EN_LAT((0*RF_READ_PORTS)+11)  = '1' and unsigned(LVT(to_integer(unsigned(rf_read_addr_lat((RF_READ_PORTS*0)+11))))) = 0 else
                         RF_READ_DATA((1*RF_READ_PORTS)+11)  when RF_READ_EN_LAT((1*RF_READ_PORTS)+11)  = '1' and unsigned(LVT(to_integer(unsigned(rf_read_addr_lat((RF_READ_PORTS*1)+11))))) = 1 else
                         RF_READ_DATA((2*RF_READ_PORTS)+11)  when RF_READ_EN_LAT((2*RF_READ_PORTS)+11)  = '1' and unsigned(LVT(to_integer(unsigned(rf_read_addr_lat((RF_READ_PORTS*2)+11))))) = 2 else
                         (others => '0');

  RF_RD_PORT_LOOP : for i in 0 to RF_READ_PORTS-1 generate 

  RF_WRITE_EN(i+(0*RF_READ_PORTS)) <= MUL_DIV_WB_EN or regfile_recovery;
  RF_WRITE_EN(i+(1*RF_READ_PORTS)) <= IE_WB_EN or regfile_recovery;
  RF_WRITE_EN(i+(2*RF_READ_PORTS)) <= LSU_WB_EN or regfile_recovery;

  rf_write_addr(i+(0*RF_READ_PORTS)) <= rf_addr_rec(0) when regfile_recovery else MUL_DIV_WB_RD_ADDR;
  rf_write_addr(i+(1*RF_READ_PORTS)) <= rf_addr_rec(1) when regfile_recovery else IE_WB_RD_ADDR;
  rf_write_addr(i+(2*RF_READ_PORTS)) <= rf_addr_rec(2) when regfile_recovery else LSU_WB_RD_ADDR;

  RF_WRITE_DATA(i+(0*RF_READ_PORTS)) <= rf_data_rec(0) when regfile_recovery else MUL_DIV_WB;
  RF_WRITE_DATA(i+(1*RF_READ_PORTS)) <= rf_data_rec(1) when regfile_recovery else IE_WB;
  RF_WRITE_DATA(i+(2*RF_READ_PORTS)) <= rf_data_rec(2) when regfile_recovery else LSU_WB;

  end generate;

  process(all)
  begin
    flush_mul_div <= '0';
    flush_ie      <= '0';
    flush_lsu     <= '0';
    flush_dsp     <= '0';
    if branch_miss = '1' then -- branch miss
      if MUL_DIV_instr_RF_int = '1' then -- valid mul_div instr
        if mul_div_instr_word_RF(MUL_DIV_buf_width-2) = '1' then -- executing in speculation
          flush_mul_div <= '1';
        end if;
      end if;
      if IE_instr_RF_int = '1' then -- valid ie instr
        if ie_instr_word_RF(IE_buf_width-2) = '1' then -- executing in speculation
          flush_ie <= '1';
        end if;
      end if;
      if LSU_instr_RF_int = '1' then -- valid lsu instr
        if lsu_instr_word_RF(LSU_buf_width-2) = '1' then -- executing in speculation
          flush_lsu <= '1';
        end if;
      end if;
      if DSP_instr_RF_int = '1' then -- valid dsp instr
        if dsp_instr_word_RF(DSP_buf_width-2) = '1' then -- executing in speculation
          flush_dsp <= '1';
        end if;
      end if;
    end if;
  end process;

  ------------------------------------------------------------------------------------------------
  --  ██████╗ ███████╗ ██████╗ ██╗███████╗████████╗███████╗██████╗ ███████╗██╗██╗     ███████╗  --
  --  ██╔══██╗██╔════╝██╔════╝ ██║██╔════╝╚══██╔══╝██╔════╝██╔══██╗██╔════╝██║██║     ██╔════╝  --
  --  ██████╔╝█████╗  ██║  ███╗██║███████╗   ██║   █████╗  ██████╔╝█████╗  ██║██║     █████╗    --
  --  ██╔══██╗██╔══╝  ██║   ██║██║╚════██║   ██║   ██╔══╝  ██╔══██╗██╔══╝  ██║██║     ██╔══╝    --
  --  ██║  ██║███████╗╚██████╔╝██║███████║   ██║   ███████╗██║  ██║██║     ██║███████╗███████╗  --
  --  ╚═╝  ╚═╝╚══════╝ ╚═════╝ ╚═╝╚══════╝   ╚═╝   ╚══════╝╚═╝  ╚═╝╚═╝     ╚═╝╚══════╝╚══════╝  --
  ------------------------------------------------------------------------------------------------

  RF_BANK_NUM_LOOP : for i in 0 to RF_BANK_NUM-1 generate

  MULTI_PORT_RF : process(clk_i, rst_ni)
  begin
    if rst_ni = '0' then
      regfile(i)(0) <= (others => '0');
    elsif rising_edge(clk_i) then
      if RF_READ_EN(i) = '1' then
        RF_READ_DATA(i) <= regfile(i)(to_integer(unsigned(rf_read_addr(i))));
      end if;
      if RF_WRITE_EN(i) = '1' then
        regfile(i)(to_integer(unsigned(rf_write_addr(i)))) <= RF_WRITE_DATA(i);
      end if;
    end if;
  end process;

  end generate;

  ---------------------------------
  --  ██╗    ██╗   ██╗████████╗  --
  --  ██║    ██║   ██║╚══██╔══╝  --
  --  ██║    ██║   ██║   ██║     --
  --  ██║    ╚██╗ ██╔╝   ██║     --
  --  ███████╗╚████╔╝    ██║     --
  --  ╚══════╝ ╚═══╝     ╚═╝     --
  ---------------------------------

  LVT_UPDATE : process(clk_i, rst_ni)
  begin
    if rst_ni = '0' then
      LVT <= (others => (others => '0'));
    elsif rising_edge(clk_i) then
      ------------------------------------------------------------------- LVT PORTS -------------------------------------------------------------------------------
      if MUL_DIV_WB_EN = '1' then  -- write port 1
        LVT(to_integer(unsigned(MUL_DIV_WB_RD_ADDR))) <= "00"; -- live value table will direct the reading of the address written to cluster "00"
      end if;
      if IE_WB_EN = '1' then  -- write port 2
        LVT(to_integer(unsigned(IE_WB_RD_ADDR))) <= "01";
      end if;
      if LSU_WB_EN = '1' then -- write port 3
        LVT(to_integer(unsigned(LSU_WB_RD_ADDR))) <= "10";
      end if;
      ------------------------------------------------------------------------------------------------------------------------------------------------------------
    end if;
  end process;

  LATCH_SIGNALS : process(clk_i, rst_ni)
  begin
    if rst_ni = '0' then 
      RF_READ_EN_LAT   <= (others => '0');
      rf_read_addr_lat <= (others => (others => '0'));
    elsif rising_edge(clk_i) then
      RF_READ_EN_LAT   <= RF_READ_EN;
      rf_read_addr_lat <= rf_read_addr;
    end if;
  end process;

  FU_SIGNALS : process(clk_i, rst_ni)
  begin
    if rst_ni = '0' then
      speculative_mul_div <= '0';
      speculative_lsu     <= '0';
      MUL_DIV_instr       <= '0';
      IE_instr            <= '0';
      LSU_instr           <= '0';
      DSP_instr           <= '0';
    elsif rising_edge(clk_i) then
      speculative_mul_div <= '0';
      speculative_lsu     <= '0';
      MUL_DIV_instr       <= '0';
      IE_instr            <= '0';
      LSU_instr           <= '0';
      DSP_instr           <= '0';
      if MUL_DIV_instr_RF_int = '1' and busy_mul_div = '0' and not flush_mul_div = '1' then
        MUL_DIV_instr <= '1';
        speculative_mul_div <= mul_div_instr_word_RF(MUL_DIV_buf_width-2);
        decoded_instruction_MUL_DIV <= decoded_instr_RF_MUL_DIV;
        signed_op           <= mul_div_instr_word_RF(MUL_DIV_UNIT_INSTR_SET_SIZE+4+4*RF_CEIL);
        mul_div_rs1_valid   <= mul_div_valid(0);
        mul_div_rs2_valid   <= mul_div_valid(1);
        mul_div_rd_valid    <= mul_div_valid(2);
        mul_div_addr_rs1    <= mul_div_addr_rs1_wire;
        mul_div_addr_rs2    <= mul_div_addr_rs2_wire;
        mul_div_addr_new_rd <= mul_div_addr_new_rd_wire;
      end if;
      if IE_instr_RF_int = '1' and busy_ie = '0' and not flush_ie = '1' then
        IE_instr <= '1';
        decoded_instruction_IE <= decoded_instr_RF_IE;
        pc_IE          <= ie_instr_word_RF((  XLEN+EXEC_UNIT_INSTR_SET_SIZE+4+4*RF_CEIL)-1 downto      EXEC_UNIT_INSTR_SET_SIZE+4+4*RF_CEIL);
        instr_word_IE  <= ie_instr_word_RF((2*XLEN+EXEC_UNIT_INSTR_SET_SIZE+4+4*RF_CEIL)-1 downto XLEN+EXEC_UNIT_INSTR_SET_SIZE+4+4*RF_CEIL);
        comparator_en  <= ie_instr_word_RF( 2*XLEN+EXEC_UNIT_INSTR_SET_SIZE+4+4*RF_CEIL);
        ie_rs1_valid   <= ie_valid(0);
        ie_rs2_valid   <= ie_valid(1);
        ie_rd_valid    <= ie_valid(2);
        ie_addr_rs1    <= ie_addr_rs1_wire;
        ie_addr_rs2    <= ie_addr_rs2_wire;
        ie_addr_new_rd <= ie_addr_new_rd_wire;
      end if;
      if LSU_instr_RF_int = '1' and busy_lsu = '0' and not flush_lsu = '1'then
        LSU_instr <= '1';
        decoded_instruction_LSU <= decoded_instr_RF_LSU;
        speculative_lsu  <= lsu_instr_word_RF(LSU_buf_width-2);
        load_op          <= lsu_instr_word_RF(7+XLEN+LS_UNIT_INSTR_SET_SIZE+5+4*RF_CEIL);
        store_op         <= lsu_instr_word_RF(6+XLEN+LS_UNIT_INSTR_SET_SIZE+5+4*RF_CEIL);
        data_width_ID    <= lsu_instr_word_RF(5+XLEN+LS_UNIT_INSTR_SET_SIZE+5+4*RF_CEIL    downto 4+XLEN+LS_UNIT_INSTR_SET_SIZE+5+4*RF_CEIL);
        data_be          <= lsu_instr_word_RF(3+XLEN+LS_UNIT_INSTR_SET_SIZE+5+4*RF_CEIL    downto 0+XLEN+LS_UNIT_INSTR_SET_SIZE+5+4*RF_CEIL);
        instr_word_LSU   <= lsu_instr_word_RF(( XLEN+LS_UNIT_INSTR_SET_SIZE+5+4*RF_CEIL)-1 downto        LS_UNIT_INSTR_SET_SIZE+5+4*RF_CEIL);
        lsu_rs1_valid    <= lsu_valid(0);
        lsu_rs2_valid    <= lsu_valid(1);
        lsu_rd_valid     <= lsu_valid(2);
        lsu_addr_rs1     <= lsu_addr_rs1_wire;
        lsu_addr_rs2     <= lsu_addr_rs2_wire;
        lsu_addr_new_rd  <= lsu_addr_new_rd_wire;
      end if;
      if DSP_instr_RF_int = '1' and busy_dsp = '0' and not flush_dsp = '1' then
        DSP_instr <= '1';
        DSP_instr <= '1';
        decoded_instruction_DSP <= decoded_instr_RF_DSP;
        spm_rs1_ISSUE      <= dsp_instr_word_RF(6+DSP_UNIT_INSTR_SET_SIZE+3+3*RF_CEIL);
        spm_rs2_ISSUE      <= dsp_instr_word_RF(5+DSP_UNIT_INSTR_SET_SIZE+3+3*RF_CEIL);
        vec_read_rs1_ISSUE <= dsp_instr_word_RF(4+DSP_UNIT_INSTR_SET_SIZE+3+3*RF_CEIL);
        vec_read_rs2_ISSUE <= dsp_instr_word_RF(3+DSP_UNIT_INSTR_SET_SIZE+3+3*RF_CEIL);
        vec_write_rd_ISSUE <= dsp_instr_word_RF(2+DSP_UNIT_INSTR_SET_SIZE+3+3*RF_CEIL);
        vec_width_ISSUE    <= dsp_instr_word_RF(1+DSP_UNIT_INSTR_SET_SIZE+3+3*RF_CEIL downto 0+DSP_UNIT_INSTR_SET_SIZE+3+3*RF_CEIL);
        dsp_rs1_valid      <= dsp_valid(0);
        dsp_rs2_valid      <= dsp_valid(1);
        dsp_rd_valid       <= dsp_valid(2);
        dsp_addr_rs1       <= dsp_addr_rs1_wire;
        dsp_addr_rs2       <= dsp_addr_rs2_wire;
        dsp_addr_rd        <= dsp_addr_rd_wire;
      end if;
    end if;
  end process;

  READY_SIGNALS : process(all)
  begin
    mul_div_ready_RF  <= '0';
    ie_ready_RF       <= '0';
    lsu_ready_RF      <= '0';
    dsp_ready_RF      <= '0';
    if busy_mul_div = '0' then
      mul_div_ready_RF <= '1';
    elsif MUL_DIV_instr_RF_int = '0' or flush_mul_div = '1' then
      mul_div_ready_RF <= '1';
    end if;
    if busy_ie = '0' then
      ie_ready_RF <= '1';
    elsif IE_instr_RF_int = '0' or flush_ie = '1' then
      ie_ready_RF <= '1';
    end if;
    if busy_lsu = '0' then
      lsu_ready_RF <= '1';
    elsif LSU_instr_RF_int = '0' or flush_lsu = '1' then
      lsu_ready_RF <= '1';
    end if;
    if busy_dsp = '0' then
      dsp_ready_RF <= '1';
    elsif DSP_instr_RF_int = '0' or flush_dsp = '1' then
      dsp_ready_RF <= '1';
    end if;
  end process;

  CC_SIGNALS : process(clk_i, rst_ni)
  begin
    if rst_ni = '0' then
    elsif rising_edge(clk_i) then
      instr_valid_RF_CC   <= (others => '0');
      mul_div_valid_RF_CC <= (others => '0');
      ie_valid_RF_CC      <= (others => '0');
      lsu_valid_RF_CC     <= (others => '0');
      dsp_valid_RF_CC     <= (others => '0');
      for i in 0 to 2 loop -- loop through rs1, rs2, and rd valids
        if MUL_DIV_instr_RF_int = '1' and busy_mul_div = '0' and not flush_mul_div = '1' then
          if mul_div_valid(i) = '1' then
            instr_valid_RF_CC(0)   <= '1';
            mul_div_valid_RF_CC(i) <= '1';  -- used to update the CC
            if    i = 0 then
              mul_div_ptr_RF_CC(0) <= mul_div_addr_rs1_wire;
            elsif i = 1 then
              mul_div_ptr_RF_CC(1) <= mul_div_addr_rs2_wire;
            elsif i = 2 then
              mul_div_ptr_RF_CC(2) <= mul_div_addr_new_rd_wire;
              new_rd_addr_CC(0)    <= mul_div_addr_new_rd_wire;
              new_rd_valid_CC(0)   <= mul_div_instr_word_RF((3*RF_CEIL)+2); -- these will be sent to CC for correct count calculation 
            end if;
          end if;
        end if;
        if IE_instr_RF_int = '1' and busy_ie = '0' and not flush_ie = '1' then
          if ie_valid(i) = '1' then
            instr_valid_RF_CC(1) <= '1';
            ie_valid_RF_CC(i)    <= '1';  -- used to update the CC
            if    i = 0 then
              ie_ptr_RF_CC(0)    <= ie_addr_rs1_wire;
            elsif i = 1 then
              ie_ptr_RF_CC(1)    <= ie_addr_rs2_wire;
            elsif i = 2 then
              ie_ptr_RF_CC(2)    <= ie_addr_new_rd_wire;
              new_rd_addr_CC(1)  <= ie_addr_new_rd_wire;
              new_rd_valid_CC(1) <= ie_instr_word_RF((3*RF_CEIL)+2); -- these will be sent to CC for correct count calculation 
            end if;
          end if;
        end if;
        if LSU_instr_RF_int = '1' and busy_lsu = '0' and not flush_lsu = '1' then
          if lsu_valid(i) = '1' then
            instr_valid_RF_CC(2) <= '1';
            lsu_valid_RF_CC(i)   <= '1';  -- used to update the CC
            if    i = 0 then
              lsu_ptr_RF_CC(0)   <= lsu_addr_rs1_wire;
            elsif i = 1 then
              lsu_ptr_RF_CC(1)   <= lsu_addr_rs2_wire;
            elsif i = 2 then
              lsu_ptr_RF_CC(2)   <= lsu_addr_new_rd_wire;
              new_rd_addr_CC(2)  <= lsu_addr_new_rd_wire;
              new_rd_valid_CC(2) <= lsu_instr_word_RF((4*RF_CEIL)+2); -- these will be sent to CC for correct count calculation 
            end if;
          end if;
        end if;
        if DSP_instr_RF_int = '1' and busy_dsp = '0' and not flush_dsp = '1' then
          if dsp_valid(i) = '1' then
            instr_valid_RF_CC(3)  <= '1';
            dsp_valid_RF_CC(i)    <= '1';  -- used to update the CC
            if    i = 0 then
              dsp_ptr_RF_CC(0)    <= dsp_addr_rs1_wire;
            elsif i = 1 then
              dsp_ptr_RF_CC(1)    <= dsp_addr_rs2_wire;
            elsif i = 2 then
              dsp_ptr_RF_CC(2)    <= dsp_addr_rd_wire;
            end if;
          end if;
        end if;
      end loop;
    end if;
  end process;


  --MULTI_PORT_RF : process(clk_i, rst_ni)
  --begin
    --if rst_ni = '0' then
      --for i in 0 to REGFILE_SIZE-1 loop
      --  regfile(i) <= std_logic_vector(to_unsigned(0, 32));
      --end loop;
    --elsif rising_edge(clk_i) then

      ------------------------------------------------------------------------------------------------
      --  ██████╗ ███████╗ ██████╗ ██╗███████╗████████╗███████╗██████╗ ███████╗██╗██╗     ███████╗  --
      --  ██╔══██╗██╔════╝██╔════╝ ██║██╔════╝╚══██╔══╝██╔════╝██╔══██╗██╔════╝██║██║     ██╔════╝  --
      --  ██████╔╝█████╗  ██║  ███╗██║███████╗   ██║   █████╗  ██████╔╝█████╗  ██║██║     █████╗    --
      --  ██╔══██╗██╔══╝  ██║   ██║██║╚════██║   ██║   ██╔══╝  ██╔══██╗██╔══╝  ██║██║     ██╔══╝    --
      --  ██║  ██║███████╗╚██████╔╝██║███████║   ██║   ███████╗██║  ██║██║     ██║███████╗███████╗  --
      --  ╚═╝  ╚═╝╚══════╝ ╚═════╝ ╚═╝╚══════╝   ╚═╝   ╚══════╝╚═╝  ╚═╝╚═╝     ╚═╝╚══════╝╚══════╝  --
      ------------------------------------------------------------------------------------------------

      ----------------------------------------------------------------- READ PORTS --------------------------------------------------------------------------------
      --if RF_READ_EN(i) = '1' then
      --  RF_READ_DATA(i) <= regfile(to_integer(unsigned(LVT(to_integer(unsigned(rf_read_addr_rs1(i))))))*RF_READ_PORTS + i)(to_integer(unsigned(rf_read_addr_rs1(i))));
      --end if;
      --if MUL_DIV_instr_RF = '1' then -- Read Ports 1 and 2 for multiplier and divider when there is a valid MUL_DIV instruction and the FU is not busy
      --  if busy_mul_div = '0' then
      --    mul_div_data_rs1 <= regfile(to_integer(unsigned(LVT(to_integer(unsigned(mul_div_addr_rs1)))))*RF_READ_PORTS + 0)(to_integer(unsigned(mul_div_addr_rs1)));
      --    mul_div_data_rs2 <= regfile(to_integer(unsigned(LVT(to_integer(unsigned(mul_div_addr_rs2)))))*RF_READ_PORTS + 1)(to_integer(unsigned(mul_div_addr_rs2)));
      --    mul_div_data_rd  <= regfile(to_integer(unsigned(LVT(to_integer(unsigned(mul_div_addr_rd)))))*RF_READ_PORTS  + 2)(to_integer(unsigned(mul_div_addr_rs2)));
      --  end if;
      --end if;
      --if IE_instr_RF = '1' then -- Read Ports 3 and 4 for IE Instructions when there is a valid IE instruction and the FU is not busy
      --  if busy_ie = '0' then
      --    ie_data_rs1 <= regfile(to_integer(unsigned(LVT(to_integer(unsigned(ie_addr_rs1)))))*RF_READ_PORTS + 3)(to_integer(unsigned(ie_addr_rs1)));
      --    ie_data_rs2 <= regfile(to_integer(unsigned(LVT(to_integer(unsigned(ie_addr_rs2)))))*RF_READ_PORTS + 4)(to_integer(unsigned(ie_addr_rs2)));
      --    ie_data_rd  <= regfile(to_integer(unsigned(LVT(to_integer(unsigned(ie_addr_rs2)))))*RF_READ_PORTS + 5)(to_integer(unsigned(ie_addr_rs2)));
      --  end if;
      --end if;
      --if LSU_instr_RF = '1' then -- Read ports 5, 6, and 7 for the Load-Store Instructions when there is a valid LSU instruction and the FU is not busy
      --  if busy_lsu = '0' then
      --    lsu_data_rs1 <= regfile(to_integer(unsigned(LVT(to_integer(unsigned(lsu_addr_rs1)))))*RF_READ_PORTS + 6)(to_integer(unsigned(lsu_addr_rs1)));
      --    lsu_data_rs2 <= regfile(to_integer(unsigned(LVT(to_integer(unsigned(lsu_addr_rs2)))))*RF_READ_PORTS + 7)(to_integer(unsigned(lsu_addr_rs2)));
      --    lsu_data_rd  <= regfile(to_integer(unsigned(LVT(to_integer(unsigned(lsu_addr_rd)))))*RF_READ_PORTS  + 8)(to_integer(unsigned(lsu_addr_rd)));
      --  end if;
      --end if;
      --if DSP_instr_RF = '1' then  -- Read ports 8, 9, and 10 for the DSP instructions when there is a valid DSP instruction and the FU is not busy
      --  if busy_dsp = '1' then  -- Read ports 8, 9, and 10 for the DSP instructions
      --    if accl_en = 1 then
      --      dsp_data_rs1 <= regfile(to_integer(unsigned(LVT(to_integer(unsigned(dsp_addr_rs1)))))*RF_READ_PORTS + 9)(to_integer(unsigned(dsp_addr_rs1)));
      --      dsp_data_rs2 <= regfile(to_integer(unsigned(LVT(to_integer(unsigned(dsp_addr_rs2)))))*RF_READ_PORTS + 10)(to_integer(unsigned(dsp_addr_rs2)));
      --      dsp_data_rd  <= regfile(to_integer(unsigned(LVT(to_integer(unsigned(dsp_addr_rd)))))*RF_READ_PORTS  + 11)(to_integer(unsigned(dsp_addr_rd)));
      --    end if;
      --  end if;
      --end if;
      -------------------------------------------------------------------------------------------------------------------------------------------------------------
      ----------------------------------------------------------------- WRITE PORTS -------------------------------------------------------------------------------
      --if MUL_DIV_WB_EN = '1' then  -- write port 1
      --  for i in 0 to RF_READ_PORTS loop
      --    regfile(i+0*RF_READ_PORTS)(to_integer(unsigned(MUL_DIV_WB_RD_ADDR))) <= MUL_DIV_WB;
      --  end loop;
      --  LVT(to_integer(unsigned(MUL_DIV_WB_RD_ADDR))) <= "00"; -- live value table will direct the reading of the address written to cluster "00"
      --end if;
      --if IE_WB_EN = '1' then  -- write port 2
      --  for i in 0 to RF_READ_PORTS loop
      --    regfile(i+1*RF_READ_PORTS)(to_integer(unsigned(IE_WB_RD_ADDR))) <= IE_WB; -- live value table will direct the reading of the address written to cluster "01"
      --  end loop;
      --  LVT(to_integer(unsigned(IE_WB_RD_ADDR))) <= "01";
      --end if;
      --if LSU_WB_EN = '1' then -- write port 3
      --  for i in 0 to RF_READ_PORTS loop
      --    regfile(i+2*RF_READ_PORTS)(to_integer(unsigned(LSU_WB_RD_ADDR))) <= LSU_WB; -- live value table will direct the reading of the address written to cluster "10"
      --  end loop;
      --  LVT(to_integer(unsigned(LSU_WB_RD_ADDR))) <= "10";
      --end if;
      ------------------------------------------------------------------------------------------------------------------------------------------------------------
    --end if;
  --end process;


  ------------------------------------------------------------------------------------------
  --  ███████╗██████╗ ███╗   ███╗    ███╗   ███╗ █████╗ ██████╗ ██████╗ ███████╗██████╗   --
  --  ██╔════╝██╔══██╗████╗ ████║    ████╗ ████║██╔══██╗██╔══██╗██╔══██╗██╔════╝██╔══██╗  --
  --  ███████╗██████╔╝██╔████╔██║    ██╔████╔██║███████║██████╔╝██████╔╝█████╗  ██████╔╝  --
  --  ╚════██║██╔═══╝ ██║╚██╔╝██║    ██║╚██╔╝██║██╔══██║██╔═══╝ ██╔═══╝ ██╔══╝  ██╔══██╗  --
  --  ███████║██║     ██║ ╚═╝ ██║    ██║ ╚═╝ ██║██║  ██║██║     ██║     ███████╗██║  ██║  --
  --  ╚══════╝╚═╝     ╚═╝     ╚═╝    ╚═╝     ╚═╝╚═╝  ╚═╝╚═╝     ╚═╝     ╚══════╝╚═╝  ╚═╝  --
  ------------------------------------------------------------------------------------------                                                           

  spm_mapper : if accl_en = 1 generate 

  --Spm_Addr_Mapping : process(all)
  --begin
  --RF_rs1_to_sc <= std_logic_vector(to_unsigned(SPM_NUM, SPM_ADDR_WID));  -- we assign SPM_NUM to rs1_to_sc as a default case which is out of range (0 to SPM_NUM-1)
  --RF_rs2_to_sc <= std_logic_vector(to_unsigned(SPM_NUM, SPM_ADDR_WID));  -- we assign SPM_NUM to rs2_to_sc as a default case which is out of range (0 to SPM_NUM-1)
  --RF_rd_to_sc  <= std_logic_vector(to_unsigned(SPM_NUM, SPM_ADDR_WID));  -- we assign SPM_NUM to rd_to_sc  as a default case which is out of range (0 to SPM_NUM-1)
  --  for i in 0 to SPM_NUM-1 loop -- Decode the address and assign and set the scratchpad number (0 to SPM_NUM-1) to the operand
  --    if regfile(to_integer(unsigned(LVT(to_integer(unsigned(dsp_addr_rs1)))))*RF_READ_PORTS + 7)(to_integer(unsigned(dsp_addr_rs1)))(31 downto Addr_Width) >= 
  --       std_logic_vector(unsigned(SPM_STRT_ADDR(31 downto Addr_Width)) + (i)) and
  --       regfile(to_integer(unsigned(LVT(to_integer(unsigned(dsp_addr_rs1)))))*RF_READ_PORTS + 7)(to_integer(unsigned(dsp_addr_rs1)))(31 downto Addr_Width) <
  --       std_logic_vector(unsigned(SPM_STRT_ADDR(31 downto Addr_Width)) + (i+1)) then
  --      RF_rs1_to_sc <= std_logic_vector(to_unsigned(i, SPM_ADDR_WID));
  --    end if;
  --    if regfile(to_integer(unsigned(LVT(to_integer(unsigned(dsp_addr_rs2)))))*RF_READ_PORTS + 8)(to_integer(unsigned(dsp_addr_rs2)))(31 downto Addr_Width) >= 
  --       std_logic_vector(unsigned(SPM_STRT_ADDR(31 downto Addr_Width)) + (i)) and
  --       regfile(to_integer(unsigned(LVT(to_integer(unsigned(dsp_addr_rs2)))))*RF_READ_PORTS + 8)(to_integer(unsigned(dsp_addr_rs2)))(31 downto Addr_Width) <
  --       std_logic_vector(unsigned(SPM_STRT_ADDR(31 downto Addr_Width)) + (i+1)) then
  --      RF_rs2_to_sc <= std_logic_vector(to_unsigned(i, SPM_ADDR_WID));
  --    end if;
  --    if regfile(to_integer(unsigned(LVT(to_integer(unsigned(dsp_addr_rd)))))*RF_READ_PORTS  + 9)(to_integer(unsigned(dsp_addr_rd)))(31 downto Addr_Width) >=
  --       std_logic_vector(unsigned(SPM_STRT_ADDR(31 downto Addr_Width)) + (i)) and
  --       regfile(to_integer(unsigned(LVT(to_integer(unsigned(dsp_addr_rd)))))*RF_READ_PORTS  + 9)(to_integer(unsigned(dsp_addr_rd)))(31 downto Addr_Width) <
  --       std_logic_vector(unsigned(SPM_STRT_ADDR(31 downto Addr_Width)) + (i+1)) then
  --      RF_rd_to_sc <= std_logic_vector(to_unsigned(i, SPM_ADDR_WID));
  --    end if;
  --  end loop;
  --end process;

  Spm_Addr_Mapping_Synch : process(clk_i, rst_ni)
  begin
    if rst_ni = '0' then
    elsif rising_edge(clk_i) then
      rs1_to_sc <= RF_rs1_to_sc;
      rs2_to_sc <= RF_rs2_to_sc;
      rd_to_sc  <= RF_rd_to_sc;
    end if;
  end process;

  end generate;

end Regfile;