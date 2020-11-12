-- ieee packages ------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_misc.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
use std.textio.all;

-- local packages ------------
use work.riscv_klessydra.all;

entity INSTR_ISSUE is
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
    DSP_buf_width     : natural;
    MUL_DIV_BUF_CEIL  : natural := integer(ceil(log2(real(MUL_DIV_BUF_SIZE))));
    IE_BUF_CEIL       : natural := integer(ceil(log2(real(IE_BUF_SIZE))));
    LSU_BUF_CEIL      : natural := integer(ceil(log2(real(LSU_BUF_SIZE))));
    DSP_BUF_CEIL      : natural := integer(ceil(log2(real(DSP_BUF_SIZE))))
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
    --old_rd_valid_ISSUE           : in  std_logic;
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
    dsp_ready_RF                 : in  std_logic;  -- indicate that the the RF does not have a pending instruction and it is ready
    -- Recovery Buffer Signals
    issue_recovery               : in  std_logic;
    mul_div_issue_rec            : in  std_logic;
    ie_issue_rec                 : in  std_logic;
    lsu_issue_rec                : in  std_logic;
    dsp_issue_rec                : in  std_logic;
    mul_div_issue_ptr_rec        : in  std_logic_vector(MUL_DIV_BUF_CEIL-1 downto 0);
    ie_issue_ptr_rec             : in  std_logic_vector(MUL_DIV_BUF_CEIL-1 downto 0);
    lsu_issue_ptr_rec            : in  std_logic_vector(MUL_DIV_BUF_CEIL-1 downto 0);
    dsp_issue_ptr_rec            : in  std_logic_vector(MUL_DIV_BUF_CEIL-1 downto 0);
    mul_div_issue_rob            : out std_logic;
    ie_issue_rob                 : out std_logic;
    lsu_issue_rob                : out std_logic;
    dsp_issue_rob                : out std_logic;
    mul_div_issue_ptr_rob        : out std_logic_vector(MUL_DIV_BUF_CEIL-1 downto 0);
    ie_issue_ptr_rob             : out std_logic_vector(MUL_DIV_BUF_CEIL-1 downto 0);
    lsu_issue_ptr_rob            : out std_logic_vector(MUL_DIV_BUF_CEIL-1 downto 0);
    dsp_issue_ptr_rob            : out std_logic_vector(MUL_DIV_BUF_CEIL-1 downto 0)
    );
end entity; --------------------------------------

architecture ISSUE of INSTR_ISSUE is

-- constant MUL_DIV_BUF_CEIL         : natural := integer(ceil(log2(real(MUL_DIV_BUF_SIZE))));
-- constant IE_BUF_CEIL              : natural := integer(ceil(log2(real(IE_BUF_SIZE))));
-- constant LSU_BUF_CEIL             : natural := integer(ceil(log2(real(LSU_BUF_SIZE))));
-- constant DSP_BUF_CEIL             : natural := integer(ceil(log2(real(DSP_BUF_SIZE))));

signal MUL_DIV_RF_wire            : std_logic;
signal IE_RF_wire                 : std_logic;
signal LSU_RF_wire                : std_logic;
signal DSP_RF_wire                : std_logic;

signal MUL_DIV_buffer_wire        : array_2d(MUL_DIV_BUF_SIZE-1 downto 0)(MUL_DIV_buf_width-1 downto 0);
signal IE_buffer_wire             : array_2d(IE_BUF_SIZE-1 downto 0)(IE_buf_width -1 downto 0); 
signal LSU_buffer_wire            : array_2d(LSU_BUF_SIZE-1 downto 0)(LSU_buf_width-1 downto 0);
signal DSP_buffer_wire            : array_2d(DSP_BUF_SIZE-1 downto 0)(DSP_buf_width-1 downto 0);

signal MUL_DIV_buffer             : array_2d(MUL_DIV_BUF_SIZE-1 downto 0)(MUL_DIV_buf_width-1 downto 0);
signal IE_buffer                  : array_2d(IE_BUF_SIZE-1 downto 0)(IE_buf_width -1 downto 0);
signal LSU_buffer                 : array_2d(LSU_BUF_SIZE-1 downto 0)(LSU_buf_width-1 downto 0);
signal DSP_buffer                 : array_2d(DSP_BUF_SIZE-1 downto 0)(DSP_buf_width-1 downto 0);

signal mul_div_instr_word_RF_wire : std_logic_vector(MUL_DIV_buf_width-1 downto 0);
signal ie_instr_word_RF_wire      : std_logic_vector(IE_buf_width -1 downto 0);
signal lsu_instr_word_RF_wire     : std_logic_vector(LSU_buf_width-1 downto 0);
signal dsp_instr_word_RF_wire     : std_logic_vector(DSP_buf_width-1 downto 0);

signal rs1_ready                  : std_logic;
signal rs2_ready                  : std_logic;
signal rd_ready                   : std_logic;
signal operands_ready             : std_logic; -- AAA this is a combinational signal, and it creates a glitch, see if there is a possible fix

signal mul_div_gnt_in_op          : std_logic;
signal ie_gnt_in_op               : std_logic;
signal lsu_gnt_in_op              : std_logic;
signal dsp_gnt_in_op              : std_logic;

signal mul_div_buf_req            : std_logic;
signal ie_buf_req                 : std_logic;
signal lsu_buf_req                : std_logic;
signal dsp_buf_req                : std_logic;

signal mul_div_buf_release        : std_logic;
signal ie_buf_release             : std_logic;
signal lsu_buf_release            : std_logic;
signal dsp_buf_release            : std_logic;


signal mul_div_issue_ptr          : std_logic_vector(MUL_DIV_BUF_CEIL-1 downto 0);
signal ie_issue_ptr               : std_logic_vector(IE_BUF_CEIL-1 downto 0);
signal lsu_issue_ptr              : std_logic_vector(LSU_BUF_CEIL-1 downto 0);
signal dsp_issue_ptr              : std_logic_vector(DSP_BUF_CEIL-1 downto 0);

signal mul_div_wr_ptr             : std_logic_vector(MUL_DIV_BUF_CEIL-1 downto 0);
signal ie_wr_ptr                  : std_logic_vector(IE_BUF_CEIL-1 downto 0);
signal lsu_wr_ptr                 : std_logic_vector(LSU_BUF_CEIL-1 downto 0);
signal dsp_wr_ptr                 : std_logic_vector(DSP_BUF_CEIL-1 downto 0);

signal mul_div_wr_ptr_inc         : std_logic;
signal ie_wr_ptr_inc              : std_logic;
signal lsu_wr_ptr_inc             : std_logic;
signal dsp_wr_ptr_inc             : std_logic;

signal mul_div_wr_ptr_inc_wire    : std_logic;
signal ie_wr_ptr_inc_wire         : std_logic;
signal lsu_wr_ptr_inc_wire        : std_logic;
signal dsp_wr_ptr_inc_wire        : std_logic;

signal mul_div_release_index      : natural;       
signal ie_release_index           : natural;  
signal lsu_release_index          : natural;   
signal dsp_release_index          : natural;   

signal mul_div_rs1_buf_ready      : std_logic;
signal mul_div_rs2_buf_ready      : std_logic;
signal mul_div_rd_buf_ready       : std_logic;
signal ie_rs1_buf_ready           : std_logic;
signal ie_rs2_buf_ready           : std_logic;
signal ie_rd_buf_ready            : std_logic;
signal lsu_rs1_buf_ready          : std_logic;
signal lsu_rs2_buf_ready          : std_logic;
signal lsu_rd_buf_ready           : std_logic;
signal dsp_rs1_buf_ready          : std_logic;
signal dsp_rs2_buf_ready          : std_logic;
signal dsp_rd_buf_ready           : std_logic;

signal valid_bits                 : std_logic_vector(RF_SIZE-1 downto 0);

-- recovery signals
signal mul_div_issue_rob_wire     : std_logic;
signal ie_issue_rob_wire          : std_logic;
signal lsu_issue_rob_wire         : std_logic;
signal dsp_issue_rob_wire         : std_logic;
signal mul_div_issue_ptr_rob_wire : std_logic_vector(MUL_DIV_BUF_CEIL-1 downto 0);
signal ie_issue_ptr_rob_wire      : std_logic_vector(MUL_DIV_BUF_CEIL-1 downto 0);
signal lsu_issue_ptr_rob_wire     : std_logic_vector(MUL_DIV_BUF_CEIL-1 downto 0);
signal dsp_issue_ptr_rob_wire     : std_logic_vector(MUL_DIV_BUF_CEIL-1 downto 0);

 -- pragma translate_off
signal pc_RF_wire                 : array_2d(3 downto 0)(31 downto 0);
signal instr_word_RF_wire         : array_2d(3 downto 0)(31 downto 0);
signal pc_RF                      : array_2d(3 downto 0)(31 downto 0);
signal instr_word_RF              : array_2d(3 downto 0)(31 downto 0);
signal pc_mul_div_buf             : array_2d(MUL_DIV_BUF_SIZE-1 downto 0)(31 downto 0);
signal pc_ie_buf                  : array_2d(IE_BUF_SIZE-1      downto 0)(31 downto 0);
signal pc_lsu_buf                 : array_2d(LSU_BUF_SIZE-1     downto 0)(31 downto 0);
signal pc_dsp_buf                 : array_2d(DSP_BUF_SIZE-1     downto 0)(31 downto 0);
signal instr_word_mul_div_buf     : array_2d(MUL_DIV_BUF_SIZE-1 downto 0)(31 downto 0);
signal instr_word_ie_buf          : array_2d(IE_BUF_SIZE-1      downto 0)(31 downto 0);
signal instr_word_lsu_buf         : array_2d(LSU_BUF_SIZE-1     downto 0)(31 downto 0);
signal instr_word_dsp_buf         : array_2d(DSP_BUF_SIZE-1     downto 0)(31 downto 0);
 -- pragma translate_on

begin

 process(clk_i, rst_ni)
 begin
   if rst_ni = '0' then
   elsif rising_edge(clk_i) then
    if MUL_DIV_RF_wire = '1' then 
      if mul_div_buf_release then
        decoded_instr_RF_MUL_DIV <= mul_div_instr_word_RF_wire((MUL_DIV_UNIT_INSTR_SET_SIZE+4+4*RF_CEIL)-1 downto 4+4*RF_CEIL);

      else
        decoded_instr_RF_MUL_DIV <= decoded_instr_ISSUE_MUL_DIV;
      end if;
    end if;
    if IE_RF_wire = '1' then
      if ie_buf_release then
        decoded_instr_RF_IE   <= ie_instr_word_RF_wire((EXEC_UNIT_INSTR_SET_SIZE+4+4*RF_CEIL)-1 downto 4+4*RF_CEIL);

      else
        decoded_instr_RF_IE   <= decoded_instr_ISSUE_IE;
      end if;
    end if;
    if LSU_RF_wire = '1' then
      if lsu_buf_release then
        decoded_instr_RF_LSU   <= lsu_instr_word_RF_wire((LS_UNIT_INSTR_SET_SIZE+5+4*RF_CEIL) -1 downto 5+4*RF_CEIL);
      else
        decoded_instr_RF_LSU   <= decoded_instr_ISSUE_LSU;
      end if;
    end if;
    if DSP_RF_wire = '1' then
      if dsp_buf_release then
        decoded_instr_RF_DSP   <= dsp_instr_word_RF_wire((DSP_UNIT_INSTR_SET_SIZE+3+3*RF_CEIL)-1 downto 3+3*RF_CEIL);

      else
        decoded_instr_RF_DSP   <= decoded_instr_ISSUE_DSP;
      end if;
    end if;
   end if;
 end process;

  Valid_RF_bits : process(clk_i, rst_ni)
  begin
    if rst_ni = '0' then
      valid_bits <= (others => '1'); -- Initially all register are valid, and considered to have a value of 0
    elsif rising_edge(clk_i) then
      if instr_valid_ISSUE = '1' then
        if new_rd_valid_ISSUE = '1' and rd_read_only_valid_ISSUE = '0' then
          valid_bits(to_integer(unsigned(new_rd_ISSUE))) <= '0'; -- invalidate the data in the new destiantion register given by the FRL 
        end if;
      end if;
      if MUL_DIV_WB_EN = '1' then
        valid_bits(to_integer(unsigned(MUL_DIV_WB_RD_ADDR))) <= '1'; -- Validate the data in the register in order for the instructions waiting to read it 
      end if;
      if IE_WB_EN = '1' then
        valid_bits(to_integer(unsigned(IE_WB_RD_ADDR))) <= '1'; -- Validate the data in the register in order for the instructions waiting to read it 
      end if;
      if LSU_WB_EN = '1' then
        valid_bits(to_integer(unsigned(LSU_WB_RD_ADDR))) <= '1'; -- Validate the data in the register in order for the instructions waiting to read it 
      end if;
    end if;
  end process;

  rd_ready   <= '1' when ((valid_bits(to_integer(unsigned(new_rd_ISSUE))) = '1' and new_rd_valid_ISSUE  = '1' and rd_read_only_valid_ISSUE = '1') or -- this is for instructions that read rd_ops
                                                                                   (new_rd_valid_ISSUE  = '1' and rd_read_only_valid_ISSUE = '0')) or -- instructions that write rd are always ready
                                                                                    new_rd_valid_ISSUE  = '0' else '0';  -- instructipons with no rd operands are always ready
  rs1_ready  <= '1' when ((valid_bits(to_integer(unsigned(rs1_ISSUE)))    = '1' and rs1_valid_ISSUE = '1') or rs1_valid_ISSUE = '0') else '0';
  rs2_ready  <= '1' when ((valid_bits(to_integer(unsigned(rs2_ISSUE)))    = '1' and rs2_valid_ISSUE = '1') or rs2_valid_ISSUE = '0') else '0';

  operands_ready <= '1' when rd_ready = '1' and rs1_ready = '1' and rs2_ready = '1' else '0';

  Issue_logic_comb : process(all)
  begin
    -- contention hanlder always gives a grant to the inputs unless a buffered instruction is ready, input grant will be halted
    mul_div_gnt_in_op     <= '1';
    ie_gnt_in_op          <= '1'; 
    lsu_gnt_in_op         <= '1'; 
    dsp_gnt_in_op         <= '1';
    -- reset cases for the following control signals is '0'
    MUL_DIV_RF_wire       <= '0';
    IE_RF_wire            <= '0';
    LSU_RF_wire           <= '0';
    DSP_RF_wire           <= '0';
    mul_div_buf_req       <= '0';
    ie_buf_req            <= '0';
    lsu_buf_req           <= '0';
    dsp_buf_req           <= '0';
    mul_div_buf_release   <= '0';
    ie_buf_release        <= '0';
    lsu_buf_release       <= '0';
    dsp_buf_release       <= '0';
    mul_div_rs1_buf_ready <= '0';
    mul_div_rs2_buf_ready <= '0';
    mul_div_rd_buf_ready  <= '0';
    ie_rs1_buf_ready      <= '0';
    ie_rs2_buf_ready      <= '0';
    ie_rd_buf_ready       <= '0';
    lsu_rs1_buf_ready     <= '0';
    lsu_rs2_buf_ready     <= '0';
    lsu_rd_buf_ready      <= '0';
    dsp_rs1_buf_ready     <= '0';
    dsp_rs2_buf_ready     <= '0';
    dsp_rd_buf_ready      <= '0';
    mul_div_release_index <=  0;
    ie_release_index      <=  0;
    lsu_release_index     <=  0;
    dsp_release_index     <=  0;
    -- defult cases of the output wires of the buffers are to latch the last value in the reg  
    mul_div_instr_word_RF_wire  <= mul_div_instr_word_RF;
    ie_instr_word_RF_wire       <= ie_instr_word_RF;
    lsu_instr_word_RF_wire      <= lsu_instr_word_RF;
    dsp_instr_word_RF_wire      <= dsp_instr_word_RF;
    if instr_valid_ISSUE = '1' and issue_recovery = '0' then
      if MUL_DIV_instr_ISSUE = '1' and MUL_DIV_buff_full = '0' then
        -- if the MUL_DIV unit, and input instruction operands are ready, and there is no contention on using the mul_div unit, then issue the input instruction
        if mul_div_ready_RF = '1' and operands_ready = '1' and mul_div_gnt_in_op = '1' then 
          MUL_DIV_RF_wire  <= '1';
          mul_div_instr_word_RF_wire <= MUL_DIV_instr_ISSUE & 
                                        speculation_ISSUE &
                                        signed_op_ISSUE &
                                        decoded_instr_ISSUE_MUL_DIV & 
                                        --new_rd_valid_ISSUE & old_rd_valid_ISSUE & rs2_valid_ISSUE & rs1_valid_ISSUE &
                                        new_rd_valid_ISSUE & rs2_valid_ISSUE & rs1_valid_ISSUE &
                                        --new_rd_ISSUE & rd_read_only_ISSUE & rs2_ISSUE & rs1_ISSUE;
                                        new_rd_ISSUE & rs2_ISSUE & rs1_ISSUE;
        -- buffer the instruction if the conditions above are not satisfied
        else
          mul_div_buf_req <= '1';
        end if;
      end if;
      if IE_instr_ISSUE = '1' and IE_buff_full = '0' then
        -- if the IE unit, and input instruction operands are ready, and there is no contention on using the mul_div unit, then issue the input instruction
        if ie_ready_RF = '1' and operands_ready = '1' and ie_gnt_in_op = '1' then 
          IE_RF_wire <= '1';
          ie_instr_word_RF_wire <= IE_instr_ISSUE & 
                                   speculation_ISSUE &
                                   comparator_en_ISSUE &
                                   instr_word_ISSUE & pc_ISSUE &
                                   decoded_instr_ISSUE_IE &
                                   --new_rd_valid_ISSUE & old_rd_valid_ISSUE & rs2_valid_ISSUE & rs1_valid_ISSUE &
                                   new_rd_valid_ISSUE & rs2_valid_ISSUE & rs1_valid_ISSUE &
                                   --new_rd_ISSUE & rd_read_only_ISSUE & rs2_ISSUE & rs1_ISSUE;
                                   new_rd_ISSUE & rs2_ISSUE & rs1_ISSUE;
        -- buffer the instruction if the conditions above are not satisfied
        else
          ie_buf_req <= '1';
        end if;
      end if;
      if LSU_instr_ISSUE = '1' and LSU_buff_full = '0' then
        -- if the LSU unit, and input instruction operands are ready, and there is no contention on using the mul_div unit, then issue the input instruction
        if lsu_ready_RF = '1' and operands_ready = '1' and lsu_gnt_in_op = '1' then 
          LSU_RF_wire <= '1';
          lsu_instr_word_RF_wire <= LSU_instr_ISSUE & 
                                    speculation_ISSUE &
                                    load_op_ISSUE & store_op_ISSUE & data_width_ISSUE & data_be_ISSUE &
                                    instr_word_ISSUE &
                                    decoded_instr_ISSUE_LSU &
                                    --rd_read_only_valid_ISSUE & new_rd_valid_ISSUE & old_rd_valid_ISSUE & rs2_valid_ISSUE & rs1_valid_ISSUE &
                                    new_rd_valid_ISSUE & rd_read_only_valid_ISSUE & rs2_valid_ISSUE & rs1_valid_ISSUE &
                                    new_rd_ISSUE & rd_read_only_ISSUE & rs2_ISSUE & rs1_ISSUE; --rd_read_only_ISSUE is used both old_rd or rd_read_only
        -- buffer the instruction if the conditions above are not satisfied
        else
          lsu_buf_req <= '1';
        end if;
      end if;
      if DSP_instr_ISSUE = '1' and DSP_buff_full = '0' then
        -- if the mul_div unit, and input instruction operands are ready, and there is no contention on using the mul_div unit, then issue the input instruction
        if dsp_ready_RF = '1' and operands_ready = '1' and dsp_gnt_in_op = '1' then 
          DSP_RF_wire <= '1';
          dsp_instr_word_RF_wire <= DSP_instr_ISSUE & 
                                    speculation_ISSUE &
                                    spm_rs1_ISSUE & spm_rs2_ISSUE & vec_read_rs1_ISSUE & vec_read_rs2_ISSUE & vec_write_rd_ISSUE & vec_width_ISSUE &
                                    decoded_instr_ISSUE_DSP &
                                    rd_read_only_valid_ISSUE & rs2_valid_ISSUE & rs1_valid_ISSUE &
                                    rd_read_only_ISSUE & rs2_ISSUE & rs1_ISSUE; --rd_read_only_ISSUE is used for rd_read_only here
        -- buffer the instruction if the conditions above are not satisfied
        else
          dsp_buf_req <= '1';
        end if;
      end if;
    end if;

    if mul_div_ready_RF = '1' then -- multiplier/divider ports in the RF are not already servicing a pending instruction
      --if MUL_DIV_buffer(to_integer(unsigned(mul_div_issue_ptr)))(MUL_DIV_buf_width-1) = '1' then -- the buffer has atleast one valid instruction
        for i in 0 to MUL_DIV_BUF_SIZE-1 loop -- loop throughout the buffer to check for ready instructions
          if (to_integer(unsigned(mul_div_issue_ptr)) + i) <= MUL_DIV_BUF_SIZE-1 then -- checks if the loop overflows the buffer
            if MUL_DIV_buffer(to_integer(unsigned(mul_div_issue_ptr))+i)(MUL_DIV_buf_width-1) = '1' then
              if      ((valid_bits(to_integer(unsigned(MUL_DIV_buffer(to_integer(unsigned(mul_div_issue_ptr))+i)((1*RF_CEIL)-1 downto 0*RF_CEIL)))) = '1' and -- rs1 ready
                    MUL_DIV_buffer(to_integer(unsigned(mul_div_issue_ptr))+i)((4*RF_CEIL)+0) = '1') or 
                    MUL_DIV_buffer(to_integer(unsigned(mul_div_issue_ptr))+i)((4*RF_CEIL)+0) = '0') and 
                      ((valid_bits(to_integer(unsigned(MUL_DIV_buffer(to_integer(unsigned(mul_div_issue_ptr))+i)((2*RF_CEIL)-1 downto 1*RF_CEIL)))) = '1' and -- rs2 ready
                    MUL_DIV_buffer(to_integer(unsigned(mul_div_issue_ptr))+i)((4*RF_CEIL)+1) = '1') or 
                    MUL_DIV_buffer(to_integer(unsigned(mul_div_issue_ptr))+i)((4*RF_CEIL)+1) = '0') and 
                      ((valid_bits(to_integer(unsigned(MUL_DIV_buffer(to_integer(unsigned(mul_div_issue_ptr))+i)((3*RF_CEIL)-1 downto 2*RF_CEIL)))) = '1' and -- rd ready
                    MUL_DIV_buffer(to_integer(unsigned(mul_div_issue_ptr))+i)((4*RF_CEIL)+2) = '1') or 
                    MUL_DIV_buffer(to_integer(unsigned(mul_div_issue_ptr))+i)((4*RF_CEIL)+2) = '0') then
                MUL_DIV_RF_wire <= '1'; -- enable the issuing of the instruction
                mul_div_instr_word_RF_wire <= MUL_DIV_buffer(to_integer(unsigned(mul_div_issue_ptr))+i); -- drive the buffer to the output
                mul_div_gnt_in_op <= '0'; -- block the input mul/div instructions from going to the output
                mul_div_buf_release <= '1';
                mul_div_release_index <= i;
                 -- pragma translate_off
                 pc_RF_wire(0)         <= pc_mul_div_buf(to_integer(unsigned(mul_div_issue_ptr))+i);
                 instr_word_RF_wire(0) <= instr_word_mul_div_buf(to_integer(unsigned(mul_div_issue_ptr))+i);
                 -- pragma translate_on
                exit; -- send to the output the first instruction ready, and then break from the loop giving priority to the first instruction
              end if;
            end if;
          else
            if MUL_DIV_buffer(to_integer(unsigned(mul_div_issue_ptr))+i-MUL_DIV_BUF_SIZE)(MUL_DIV_buf_width-1) = '1' then
              if     ((valid_bits(to_integer(unsigned(MUL_DIV_buffer(to_integer(unsigned(ie_issue_ptr))+i-MUL_DIV_BUF_SIZE)((1*RF_CEIL)-1 downto 0*RF_CEIL)))) = '1' and -- rs1 ready
                   MUL_DIV_buffer(to_integer(unsigned(mul_div_issue_ptr))+i-MUL_DIV_BUF_SIZE)((4*RF_CEIL)+0) = '1') or 
                   MUL_DIV_buffer(to_integer(unsigned(mul_div_issue_ptr))+i-MUL_DIV_BUF_SIZE)((4*RF_CEIL)+0) = '0') and 
                     ((valid_bits(to_integer(unsigned(MUL_DIV_buffer(to_integer(unsigned(ie_issue_ptr))+i-MUL_DIV_BUF_SIZE)((2*RF_CEIL)-1 downto 1*RF_CEIL)))) = '1' and -- rs2 ready
                   MUL_DIV_buffer(to_integer(unsigned(mul_div_issue_ptr))+i-MUL_DIV_BUF_SIZE)((4*RF_CEIL)+1) = '1') or 
                   MUL_DIV_buffer(to_integer(unsigned(mul_div_issue_ptr))+i-MUL_DIV_BUF_SIZE)((4*RF_CEIL)+1) = '0') and 
                     ((valid_bits(to_integer(unsigned(MUL_DIV_buffer(to_integer(unsigned(ie_issue_ptr))+i-MUL_DIV_BUF_SIZE)((3*RF_CEIL)-1 downto 2*RF_CEIL)))) = '1' and -- rd ready
                   MUL_DIV_buffer(to_integer(unsigned(mul_div_issue_ptr))+i-MUL_DIV_BUF_SIZE)((4*RF_CEIL)+2) = '1') or 
                   MUL_DIV_buffer(to_integer(unsigned(mul_div_issue_ptr))+i-MUL_DIV_BUF_SIZE)((4*RF_CEIL)+2) = '0') then
                MUL_DIV_RF_wire <= '1';
                mul_div_instr_word_RF_wire <= MUL_DIV_buffer(to_integer(unsigned(mul_div_issue_ptr))+i-MUL_DIV_BUF_SIZE);
                mul_div_gnt_in_op <= '0';
                mul_div_buf_release <= '1';
                mul_div_release_index <= i;
                 -- pragma translate_off
                 pc_RF_wire(0)         <= pc_mul_div_buf(to_integer(unsigned(mul_div_issue_ptr))+i-MUL_DIV_BUF_SIZE);
                 instr_word_RF_wire(0) <= instr_word_mul_div_buf(to_integer(unsigned(mul_div_issue_ptr))+i-MUL_DIV_BUF_SIZE);
                 -- pragma translate_on
                exit;  -- send to the output the first instruction ready, and then break from the loop giving priority to the first instruction
              end if;
            end if;
          end if;
        end loop;
      --end if;
    end if;
    if ie_ready_RF = '1' then -- IE unit ports in the RF are not already servicing a pending instruction
      --if IE_buffer(to_integer(unsigned(ie_issue_ptr)))(IE_buf_width-1) = '1' then -- the buffer has atleast one valid instruction
        for i in 0 to IE_BUF_SIZE-1 loop -- loop throughout the buffer to check for ready instructions
          if (to_integer(unsigned(ie_issue_ptr)) + i) <= IE_BUF_SIZE-1 then -- checks if the loop overflows the buffer
            if IE_buffer(to_integer(unsigned(ie_issue_ptr))+i)(IE_buf_width-1) = '1' then
              if ((valid_bits(to_integer(unsigned(IE_buffer(to_integer(unsigned(ie_issue_ptr))+i)((1*RF_CEIL)-1 downto 0*RF_CEIL)))) = '1' and -- rs1 ready
                    IE_buffer(to_integer(unsigned(ie_issue_ptr))+i)((4*RF_CEIL)+0) = '1') or 
                    IE_buffer(to_integer(unsigned(ie_issue_ptr))+i)((4*RF_CEIL)+0) = '0') and 
                 ((valid_bits(to_integer(unsigned(IE_buffer(to_integer(unsigned(ie_issue_ptr))+i)((2*RF_CEIL)-1 downto 1*RF_CEIL)))) = '1' and -- rs2 ready
                    IE_buffer(to_integer(unsigned(ie_issue_ptr))+i)((4*RF_CEIL)+1) = '1') or 
                    IE_buffer(to_integer(unsigned(ie_issue_ptr))+i)((4*RF_CEIL)+1) = '0') and 
                 ((valid_bits(to_integer(unsigned(IE_buffer(to_integer(unsigned(ie_issue_ptr))+i)((3*RF_CEIL)-1 downto 2*RF_CEIL)))) = '1' and -- rd ready
                    IE_buffer(to_integer(unsigned(ie_issue_ptr))+i)((4*RF_CEIL)+2) = '1') or 
                    IE_buffer(to_integer(unsigned(ie_issue_ptr))+i)((4*RF_CEIL)+2) = '0') then 
                IE_RF_wire <= '1'; -- enable the issuing of the instruction
                ie_instr_word_RF_wire <= IE_buffer(to_integer(unsigned(ie_issue_ptr))+i); -- drive the buffer to the output
                ie_gnt_in_op <= '0'; -- block the input mul/div instructions from going to the output
                ie_buf_release <= '1';
                ie_release_index <= i;
                -- pragma translate_off
                pc_RF_wire(1)         <= pc_ie_buf(to_integer(unsigned(ie_issue_ptr))+i);
                instr_word_RF_wire(1) <= instr_word_ie_buf(to_integer(unsigned(ie_issue_ptr))+i);
                -- pragma translate_on
                exit; -- send to the output the first instruction ready, and then break from the loop giving priority to the first instruction
              end if;
            end if;
          else
            if IE_buffer(to_integer(unsigned(ie_issue_ptr))+i-IE_BUF_SIZE)(IE_buf_width-1) = '1' then
              if ((valid_bits(to_integer(unsigned(IE_buffer(to_integer(unsigned(ie_issue_ptr))+i-IE_BUF_SIZE)((3*RF_CEIL)-1 downto 2*RF_CEIL)))) = '1' and -- rs1 ready
                    IE_buffer(to_integer(unsigned(ie_issue_ptr))+i-IE_BUF_SIZE)((4*RF_CEIL)+2) = '1') or 
                    IE_buffer(to_integer(unsigned(ie_issue_ptr))+i-IE_BUF_SIZE)((4*RF_CEIL)+2) = '0') and 
                 ((valid_bits(to_integer(unsigned(IE_buffer(to_integer(unsigned(ie_issue_ptr))+i-IE_BUF_SIZE)((2*RF_CEIL)-1 downto 1*RF_CEIL)))) = '1' and -- rs2 ready
                    IE_buffer(to_integer(unsigned(ie_issue_ptr))+i-IE_BUF_SIZE)((4*RF_CEIL)+1) = '1') or 
                    IE_buffer(to_integer(unsigned(ie_issue_ptr))+i-IE_BUF_SIZE)((4*RF_CEIL)+1) = '0') and 
                 ((valid_bits(to_integer(unsigned(IE_buffer(to_integer(unsigned(ie_issue_ptr))+i-IE_BUF_SIZE)((1*RF_CEIL)-1 downto 0*RF_CEIL)))) = '1' and -- rd ready
                    IE_buffer(to_integer(unsigned(ie_issue_ptr))+i-IE_BUF_SIZE)((4*RF_CEIL)+0) = '1') or 
                    IE_buffer(to_integer(unsigned(ie_issue_ptr))+i-IE_BUF_SIZE)((4*RF_CEIL)+0) = '0') then
                IE_RF_wire <= '1';
                ie_instr_word_RF_wire <= IE_buffer(to_integer(unsigned(ie_issue_ptr))+i-IE_BUF_SIZE);
                ie_gnt_in_op <= '0';
                ie_buf_release <= '1';
                ie_release_index <= i;
                -- pragma translate_off
                pc_RF_wire(1)         <= pc_ie_buf(to_integer(unsigned(ie_issue_ptr))+i-IE_BUF_SIZE);
                instr_word_RF_wire(1) <= instr_word_ie_buf(to_integer(unsigned(ie_issue_ptr))+i-IE_BUF_SIZE);
                -- pragma translate_on
                exit;  -- send to the output the first instruction ready, and then break from the loop giving priority to the first instruction
              end if;
            end if;
          end if;
        end loop;
      --end if;
    end if;
    if lsu_ready_RF = '1' then -- load-store unit ports in the RF are not already servicing a pending instruction
  --    if LSU_buffer(to_integer(unsigned(lsu_issue_ptr)))(LSU_buf_width-1) = '1' then -- the buffer has atleast one valid instruction
        for i in 0 to LSU_BUF_SIZE-1 loop -- loop throughout the buffer to check for ready instructions
          if (to_integer(unsigned(lsu_issue_ptr)) + i) <= LSU_BUF_SIZE-1 then -- checks if the loop overflows the buffer
            if LSU_buffer(to_integer(unsigned(lsu_issue_ptr))+i)(LSU_buf_width-1) = '1' then
              if ((valid_bits(to_integer(unsigned(LSU_buffer(to_integer(unsigned(lsu_issue_ptr))+i)((1*RF_CEIL)-1 downto 0*RF_CEIL)))) = '1' and -- rs1 ready
                   LSU_buffer(to_integer(unsigned(lsu_issue_ptr))+i)((4*RF_CEIL)+0) = '1') or 
                   LSU_buffer(to_integer(unsigned(lsu_issue_ptr))+i)((4*RF_CEIL)+0) = '0') and 
                 ((valid_bits(to_integer(unsigned(LSU_buffer(to_integer(unsigned(lsu_issue_ptr))+i)((2*RF_CEIL)-1 downto 1*RF_CEIL)))) = '1' and -- rs2 ready
                   LSU_buffer(to_integer(unsigned(lsu_issue_ptr))+i)((4*RF_CEIL)+1) = '1') or 
                   LSU_buffer(to_integer(unsigned(lsu_issue_ptr))+i)((4*RF_CEIL)+1) = '0') and 
                 ((valid_bits(to_integer(unsigned(LSU_buffer(to_integer(unsigned(lsu_issue_ptr))+i)((3*RF_CEIL)-1 downto 2*RF_CEIL)))) = '1' and -- rd ready
                   LSU_buffer(to_integer(unsigned(lsu_issue_ptr))+i)((4*RF_CEIL)+2) = '1') or 
                   LSU_buffer(to_integer(unsigned(lsu_issue_ptr))+i)((4*RF_CEIL)+2) = '0') then
                LSU_RF_wire <= '1'; -- enable the issuing of the instruction
                lsu_instr_word_RF_wire <= LSU_buffer(to_integer(unsigned(lsu_issue_ptr))+i); -- drive the buffer to the output
                lsu_gnt_in_op <= '0'; -- block the input mul/div instructions from going to the output
                lsu_buf_release <= '1';
                lsu_release_index <= i;
                -- pragma translate_off
                pc_RF_wire(2)         <= pc_lsu_buf(to_integer(unsigned(lsu_issue_ptr))+i);
                instr_word_RF_wire(2) <= instr_word_lsu_buf(to_integer(unsigned(lsu_issue_ptr))+i);
                -- pragma translate_on
                exit; -- send to the output the first instruction ready, and then break from the loop giving priority to the first instruction
              end if;
            end if;
          else
            if LSU_buffer(to_integer(unsigned(lsu_issue_ptr))+i-LSU_BUF_SIZE)(LSU_buf_width-1) = '1' then
              if  ((valid_bits(to_integer(unsigned(LSU_buffer(to_integer(unsigned(lsu_issue_ptr))+i-LSU_BUF_SIZE)((1*RF_CEIL)-1 downto 0*RF_CEIL)))) = '1' and -- rs1 ready
                    LSU_buffer(to_integer(unsigned(lsu_issue_ptr))+i-LSU_BUF_SIZE)((4*RF_CEIL)+0) = '1') or 
                    LSU_buffer(to_integer(unsigned(lsu_issue_ptr))+i-LSU_BUF_SIZE)((4*RF_CEIL)+0) = '0') and 
                  ((valid_bits(to_integer(unsigned(LSU_buffer(to_integer(unsigned(lsu_issue_ptr))+i-LSU_BUF_SIZE)((2*RF_CEIL)-1 downto 1*RF_CEIL)))) = '1' and -- rs2 ready
                    LSU_buffer(to_integer(unsigned(lsu_issue_ptr))+i-LSU_BUF_SIZE)((4*RF_CEIL)+1) = '1') or 
                    LSU_buffer(to_integer(unsigned(lsu_issue_ptr))+i-LSU_BUF_SIZE)((4*RF_CEIL)+1) = '0') and 
                  ((valid_bits(to_integer(unsigned(LSU_buffer(to_integer(unsigned(lsu_issue_ptr))+i-LSU_BUF_SIZE)((3*RF_CEIL)-1 downto 2*RF_CEIL)))) = '1' and -- rd ready
                    LSU_buffer(to_integer(unsigned(lsu_issue_ptr))+i-LSU_BUF_SIZE)((4*RF_CEIL)+2) = '1') or 
                    LSU_buffer(to_integer(unsigned(lsu_issue_ptr))+i-LSU_BUF_SIZE)((4*RF_CEIL)+2) = '0') then
                LSU_RF_wire <= '1';
                lsu_instr_word_RF_wire <= LSU_buffer(to_integer(unsigned(lsu_issue_ptr))+i-LSU_BUF_SIZE);
                lsu_gnt_in_op <= '0';
                lsu_buf_release <= '1';
                lsu_release_index <= i;
                -- pragma translate_off
                pc_RF_wire(2)         <= pc_lsu_buf(to_integer(unsigned(lsu_issue_ptr))+i-LSU_BUF_SIZE);
                instr_word_RF_wire(2) <= instr_word_lsu_buf(to_integer(unsigned(lsu_issue_ptr))+i-LSU_BUF_SIZE);
                -- pragma translate_on
                exit;  -- send to the output the first instruction ready, and then break from the loop giving priority to the first instruction
              end if;
            end if;
          end if;
        end loop;
      --end if;
    end if;
    if dsp_ready_RF = '1' then -- dsp unit ports in the RF are not already servicing a pending instruction
      --if DSP_buffer(to_integer(unsigned(dsp_issue_ptr)))(DSP_buf_width-1) = '1' then -- the buffer has atleast one valid instruction
        for i in 0 to DSP_BUF_SIZE-1 loop -- loop throughout the buffer to check for ready instructions
          if (to_integer(unsigned(dsp_issue_ptr)) + i) <= DSP_BUF_SIZE-1 then -- checks if the loop overflows the buffer
            if DSP_buffer(to_integer(unsigned(dsp_issue_ptr))+i)(DSP_buf_width-1) = '1' then
              if  ((valid_bits(to_integer(unsigned(DSP_buffer(to_integer(unsigned(dsp_issue_ptr))+i)((1*RF_CEIL)-1 downto 0*RF_CEIL)))) = '1' and -- rs1 ready
                    DSP_buffer(to_integer(unsigned(dsp_issue_ptr))+i)((3*RF_CEIL)+0) = '1') or 
                    DSP_buffer(to_integer(unsigned(dsp_issue_ptr))+i)((3*RF_CEIL)+0) = '0') and 
                  ((valid_bits(to_integer(unsigned(DSP_buffer(to_integer(unsigned(dsp_issue_ptr))+i)((2*RF_CEIL)-1 downto 1*RF_CEIL)))) = '1' and -- rs2 ready
                    DSP_buffer(to_integer(unsigned(dsp_issue_ptr))+i)((3*RF_CEIL)+1) = '1') or 
                    DSP_buffer(to_integer(unsigned(dsp_issue_ptr))+i)((3*RF_CEIL)+1) = '0') and 
                  ((valid_bits(to_integer(unsigned(DSP_buffer(to_integer(unsigned(dsp_issue_ptr))+i)((3*RF_CEIL)-1 downto 2*RF_CEIL)))) = '1' and -- rd ready
                    DSP_buffer(to_integer(unsigned(dsp_issue_ptr))+i)((3*RF_CEIL)+2) = '1') or 
                    DSP_buffer(to_integer(unsigned(dsp_issue_ptr))+i)((3*RF_CEIL)+2) = '0') then
                DSP_RF_wire <= '1'; -- enable the issuing of the instruction
                dsp_instr_word_RF_wire <= DSP_buffer(to_integer(unsigned(dsp_issue_ptr))+i); -- drive the buffer to the output
                dsp_gnt_in_op <= '0'; -- block the input mul/div instructions from going to the output
                dsp_buf_release <= '1';
                dsp_release_index <= i;
                -- pragma translate_off
                pc_RF_wire(3)         <= pc_dsp_buf(to_integer(unsigned(dsp_issue_ptr))+i);
                instr_word_RF_wire(3) <= instr_word_dsp_buf(to_integer(unsigned(dsp_issue_ptr))+i);
                -- pragma translate_on
                exit; -- send to the output the first instruction ready, and then break from the loop giving priority to the first instruction
              end if;
            end if;
          else
            if DSP_buffer(to_integer(unsigned(dsp_issue_ptr))+i-DSP_BUF_SIZE)(DSP_buf_width-1) = '1' then
              if  ((valid_bits(to_integer(unsigned(DSP_buffer(to_integer(unsigned(dsp_issue_ptr))+i-DSP_BUF_SIZE)((1*RF_CEIL)-1 downto 0*RF_CEIL)))) = '1' and -- rs1 ready
                    DSP_buffer(to_integer(unsigned(dsp_issue_ptr))+i-DSP_BUF_SIZE)((3*RF_CEIL)+0) = '1') or 
                    DSP_buffer(to_integer(unsigned(dsp_issue_ptr))+i-DSP_BUF_SIZE)((3*RF_CEIL)+0) = '0') and 
                  ((valid_bits(to_integer(unsigned(DSP_buffer(to_integer(unsigned(dsp_issue_ptr))+i-DSP_BUF_SIZE)((3*RF_CEIL)-1 downto 2*RF_CEIL)))) = '1' and -- rs2 ready
                    DSP_buffer(to_integer(unsigned(dsp_issue_ptr))+i-DSP_BUF_SIZE)((3*RF_CEIL)+2) = '1') or 
                    DSP_buffer(to_integer(unsigned(dsp_issue_ptr))+i-DSP_BUF_SIZE)((3*RF_CEIL)+2) = '0') and 
                  ((valid_bits(to_integer(unsigned(DSP_buffer(to_integer(unsigned(dsp_issue_ptr))+i-DSP_BUF_SIZE)((2*RF_CEIL)-1 downto 1*RF_CEIL)))) = '1' and -- rd ready
                    DSP_buffer(to_integer(unsigned(dsp_issue_ptr))+i-DSP_BUF_SIZE)((3*RF_CEIL)+1) = '1') or 
                    DSP_buffer(to_integer(unsigned(dsp_issue_ptr))+i-DSP_BUF_SIZE)((3*RF_CEIL)+1) = '0') then
                DSP_RF_wire <= '1';
                dsp_instr_word_RF_wire <= DSP_buffer(to_integer(unsigned(dsp_issue_ptr))+i-DSP_BUF_SIZE);
                dsp_gnt_in_op <= '0';
                dsp_buf_release <= '1';
                dsp_release_index <= i;
                 -- pragma translate_off
                 pc_RF_wire(3)         <= pc_dsp_buf(to_integer(unsigned(dsp_issue_ptr))+i-DSP_BUF_SIZE);
                 instr_word_RF_wire(3) <= instr_word_dsp_buf(to_integer(unsigned(dsp_issue_ptr))+i-DSP_BUF_SIZE);
                 -- pragma translate_on
                exit;  -- send to the output the first instruction ready, and then break from the loop giving priority to the first instruction
              end if;
            end if;
          end if;
        end loop;
      --end if;
    end if;
    if branch_hit = '1' then
      for i in 0 to MUL_DIV_BUF_SIZE-1 loop 
        MUL_DIV_buffer(i)(MUL_DIV_buf_width-2) <= '0';
      end loop;
      for i in 0 to IE_BUF_SIZE-1 loop 
        IE_buffer(i)(IE_buf_width-2) <= '0';
      end loop;
      for i in 0 to LSU_BUF_SIZE-1 loop 
        LSU_buffer(i)(LSU_buf_width-2) <= '0';
      end loop;
      for i in 0 to DSP_BUF_SIZE-1 loop 
        DSP_buffer(i)(DSP_buf_width-2) <= '0';
      end loop;
    end if;
  end process;

  Issue_logic_sync : process(clk_i, rst_ni)
  begin
    if rst_ni = '0' then
      instr_valid_RF      <= '0';
      MUL_DIV_instr_RF    <= '0';
      IE_instr_RF         <= '0';
      LSU_instr_RF        <= '0';
      DSP_instr_RF        <= '0';
    elsif rising_edge(clk_i) then
      -- output signals for the rf stage
      instr_valid_RF      <= '0'; -- by default are zero unless an instruction is issued
      MUL_DIV_instr_RF    <= '0'; -- by default are zero unless an instruction is issued
      IE_instr_RF         <= '0'; -- by default are zero unless an instruction is issued
      LSU_instr_RF        <= '0'; -- by default are zero unless an instruction is issued
      DSP_instr_RF        <= '0'; -- by default are zero unless an instruction is issued
      mul_div_instr_word_RF         <= mul_div_instr_word_RF_wire;  -- the wires are updated whenver an instruction is ready to be issued
      ie_instr_word_RF              <= ie_instr_word_RF_wire;       -- the wires are updated whenver an instruction is ready to be issued
      lsu_instr_word_RF             <= lsu_instr_word_RF_wire;      -- the wires are updated whenver an instruction is ready to be issued
      dsp_instr_word_RF             <= dsp_instr_word_RF_wire;      -- the wires are updated whenver an instruction is ready to be issued
      -- if the input instruction is granted to be issued to the output port
      if instr_valid_ISSUE = '1' and issue_recovery = '0' then -- send the input op to the RF stage when not in recovery mode, esle flush it
        if (MUL_DIV_RF_wire = '1' and mul_div_gnt_in_op = '1') or
           (IE_RF_wire      = '1' and ie_gnt_in_op      = '1') or 
           (LSU_RF_wire     = '1' and lsu_gnt_in_op     = '1') or
           (MUL_DIV_RF_wire = '1' and dsp_gnt_in_op     = '1') then 
          -- push the input data to the output
          instr_valid_RF           <= instr_valid_ISSUE;
          MUL_DIV_instr_RF         <= MUL_DIV_instr_ISSUE;
          IE_instr_RF              <= IE_instr_ISSUE;
          LSU_instr_RF             <= LSU_instr_ISSUE;
          DSP_instr_RF             <= DSP_instr_ISSUE;
        end if;
      elsif issue_recovery = '1' then
        instr_valid_RF   <= '0';
        MUL_DIV_instr_RF <= '0'; -- is reset to '0' along with instr_valid_RF since instructions can issue in parallel in this stage
        IE_instr_RF      <= '0'; -- is reset to '0' along with instr_valid_RF since instructions can issue in parallel in this stage
        LSU_instr_RF     <= '0'; -- is reset to '0' along with instr_valid_RF since instructions can issue in parallel in this stage
        DSP_instr_RF     <= '0'; -- is reset to '0' along with instr_valid_RF since instructions can issue in parallel in this stage
      end if;

      if MUL_DIV_RF_wire = '1' and mul_div_gnt_in_op = '0' then
        instr_valid_RF <= '1';
        MUL_DIV_instr_RF <= '1';
      end if;
      if IE_RF_wire = '1' and ie_gnt_in_op = '0' then
        instr_valid_RF <= '1';
        IE_instr_RF <= '1';
      end if;
      if LSU_RF_wire = '1' and lsu_gnt_in_op = '0' then
        instr_valid_RF <= '1';
        LSU_instr_RF <= '1';
      end if;
      if DSP_RF_wire = '1' and dsp_gnt_in_op = '0' then
        instr_valid_RF <= '1';
        DSP_instr_RF <= '1';
      end if;

    end if;
  end process;

  Buffer_logic_comb : process(all)
  begin
    MUL_DIV_buff_full          <= '0';
    IE_buff_full               <= '0';
    LSU_buff_full              <= '0';
    DSP_buff_full              <= '0';
    mul_div_wr_ptr_inc_wire    <= mul_div_wr_ptr_inc;
    ie_wr_ptr_inc_wire         <= ie_wr_ptr_inc;
    lsu_wr_ptr_inc_wire        <= lsu_wr_ptr_inc;
    dsp_wr_ptr_inc_wire        <= dsp_wr_ptr_inc;
    MUL_DIV_buffer_wire        <= MUL_DIV_buffer;
    IE_buffer_wire             <= IE_buffer;
    LSU_buffer_wire            <= LSU_buffer;
    DSP_buffer_wire            <= DSP_buffer;
    -- Recovery signals
    mul_div_issue_rob_wire     <= '0';
    mul_div_issue_ptr_rob_wire <= mul_div_issue_ptr_rob;
    ie_issue_rob_wire          <= '0';
    ie_issue_ptr_rob_wire      <= ie_issue_ptr_rob;
    lsu_issue_rob_wire         <= '0';
    lsu_issue_ptr_rob_wire     <= lsu_issue_ptr_rob;
    dsp_issue_rob_wire         <= '0';
    dsp_issue_ptr_rob_wire     <= dsp_issue_ptr_rob;
    -- Recover the buffer state by turning off the instructions that need to be flushed
    if issue_recovery = '1' then
      if mul_div_issue_rec = '1' then
        MUL_DIV_buffer_wire(to_integer(unsigned(mul_div_issue_ptr_rec)))(MUL_DIV_buf_width-1) <= '0';
      end if;
      if ie_issue_rec = '1' then
        IE_buffer_wire(to_integer(unsigned(ie_issue_ptr_rec)))(IE_buf_width-1) <= '0';
      end if;
      if lsu_issue_rec = '1' then
        LSU_buffer_wire(to_integer(unsigned(lsu_issue_ptr_rec)))(LSU_buf_width-1) <= '0';
      end if;
      if dsp_issue_rec = '1' then
        DSP_buffer_wire(to_integer(unsigned(dsp_issue_ptr_rec)))(DSP_buf_width-1) <= '0';
      end if;
    end if;
    -- if the instruction count in the mul_div buffer becomes equal to the buffer size, send a mul_div buffer full signal 
    if mul_div_issue_ptr = mul_div_wr_ptr and mul_div_wr_ptr_inc = '1' then -- must be _ptr_wire and inc_wire instead _ptr and _inc
      MUL_DIV_buff_full <= '1';
    end if;
    if ie_issue_ptr = ie_wr_ptr and ie_wr_ptr_inc = '1' then -- must be _ptr_wire and inc_wire instead _ptr and _inc
      IE_buff_full <= '1';
    end if;
    if lsu_issue_ptr = lsu_wr_ptr and lsu_wr_ptr_inc = '1' then -- must be _ptr_wire and inc_wire instead _ptr and _inc
      LSU_buff_full <= '1';
    end if;
    if dsp_issue_ptr = dsp_wr_ptr and dsp_wr_ptr_inc = '1' then -- must be _ptr_wire and inc_wire instead _ptr and _inc
      DSP_buff_full <= '1';
    end if;
    if mul_div_buf_req = '1' then
      mul_div_issue_rob_wire <= '1'; 
      mul_div_issue_ptr_rob_wire <= mul_div_wr_ptr; -- send the wr pointer to the rob
      MUL_DIV_buffer_wire(to_integer(unsigned(mul_div_wr_ptr))) <= MUL_DIV_instr_ISSUE & 
                                                                   speculation_ISSUE &
                                                                   signed_op_ISSUE &
                                                                   decoded_instr_ISSUE_MUL_DIV & 
                                                                   --new_rd_valid_ISSUE & old_rd_valid_ISSUE & rs2_valid_ISSUE & rs1_valid_ISSUE &
                                                                   new_rd_valid_ISSUE & rs2_valid_ISSUE & rs1_valid_ISSUE &
                                                                   --new_rd_ISSUE & rd_read_only_ISSUE & rs2_ISSUE & rs1_ISSUE;
                                                                   new_rd_ISSUE & rs2_ISSUE & rs1_ISSUE;
      mul_div_wr_ptr_inc_wire <= '1';
      -- pragma translate_off
      pc_mul_div_buf(to_integer(unsigned(mul_div_wr_ptr))) <= pc_ISSUE;
      instr_word_mul_div_buf(to_integer(unsigned(mul_div_wr_ptr))) <= instr_word_ISSUE;
      -- pragma translate_on
    end if;
    if ie_buf_req = '1' then
      ie_issue_rob_wire <= '1'; 
      ie_issue_ptr_rob_wire <= ie_wr_ptr; -- send the wr pointer to the rob
      IE_buffer_wire(to_integer(unsigned(ie_wr_ptr))) <= IE_instr_ISSUE & 
                                                         speculation_ISSUE &
                                                         comparator_en_ISSUE &
                                                         instr_word_ISSUE & pc_ISSUE &
                                                         decoded_instr_ISSUE_IE &
                                                         --new_rd_valid_ISSUE & old_rd_valid_ISSUE & rs2_valid_ISSUE & rs1_valid_ISSUE &
                                                         new_rd_valid_ISSUE & rs2_valid_ISSUE & rs1_valid_ISSUE &
                                                         --new_rd_ISSUE & rd_read_only_ISSUE & rs2_ISSUE & rs1_ISSUE;
                                                         new_rd_ISSUE & rs2_ISSUE & rs1_ISSUE;
      ie_wr_ptr_inc_wire <= '1';
      -- pragma translate_off
      pc_ie_buf(to_integer(unsigned(ie_wr_ptr))) <= pc_ISSUE;
      instr_word_ie_buf(to_integer(unsigned(ie_wr_ptr))) <= instr_word_ISSUE;
      -- pragma translate_on
    end if;
    if lsu_buf_req = '1' then
      lsu_issue_rob_wire <= '1'; 
      lsu_issue_ptr_rob_wire <= lsu_wr_ptr; -- send the wr pointer to the rob
      LSU_buffer_wire(to_integer(unsigned(lsu_wr_ptr))) <= LSU_instr_ISSUE & 
                                                           speculation_ISSUE &
                                                           load_op_ISSUE & store_op_ISSUE & data_width_ISSUE & data_be_ISSUE &
                                                           instr_word_ISSUE &
                                                           decoded_instr_ISSUE_LSU &
                                                           --rd_read_only_valid_ISSUE & new_rd_valid_ISSUE & old_rd_valid_ISSUE & rs2_valid_ISSUE & rs1_valid_ISSUE &
                                                           new_rd_valid_ISSUE & rd_read_only_valid_ISSUE & rs2_valid_ISSUE & rs1_valid_ISSUE &
                                                           new_rd_ISSUE & rd_read_only_ISSUE & rs2_ISSUE & rs1_ISSUE; --rd_read_only_ISSUE is used both old_rd or rd_read_only
      lsu_wr_ptr_inc_wire <= '1';
      -- pragma translate_off
      pc_lsu_buf(to_integer(unsigned(lsu_wr_ptr))) <= pc_ISSUE;
      instr_word_lsu_buf(to_integer(unsigned(lsu_wr_ptr))) <= instr_word_ISSUE;
      -- pragma translate_on
    end if;
    if dsp_buf_req = '1' then
      dsp_issue_rob_wire <= '1'; 
      dsp_issue_ptr_rob_wire <= dsp_wr_ptr;
      DSP_buffer_wire(to_integer(unsigned(dsp_wr_ptr))) <= DSP_instr_ISSUE & 
                                                           speculation_ISSUE &
                                                           spm_rs1_ISSUE & spm_rs2_ISSUE & vec_read_rs1_ISSUE & vec_read_rs2_ISSUE & vec_write_rd_ISSUE & vec_width_ISSUE &
                                                           decoded_instr_ISSUE_DSP &
                                                           rd_read_only_valid_ISSUE & rs2_valid_ISSUE & rs1_valid_ISSUE &
                                                           rd_read_only_ISSUE & rs2_ISSUE & rs1_ISSUE; --rd_read_only_ISSUE is used for rd_read_only here
      dsp_wr_ptr_inc_wire <= '1';
      -- pragma translate_off
      pc_dsp_buf(to_integer(unsigned(dsp_wr_ptr))) <= pc_ISSUE;
      instr_word_dsp_buf(to_integer(unsigned(dsp_wr_ptr))) <= instr_word_ISSUE;
      -- pragma translate_on
    end if;
    if mul_div_buf_release = '1' and mul_div_buf_req = '0' then
      mul_div_wr_ptr_inc_wire <= '0';
    end if;
    if ie_buf_release = '1' and ie_buf_req = '0' then
      ie_wr_ptr_inc_wire <= '0';
    end if;
    if lsu_buf_release = '1' and lsu_buf_req = '0' then
      lsu_wr_ptr_inc_wire <= '0';
    end if;
    if dsp_buf_release = '1' and dsp_buf_req = '0' then
      dsp_wr_ptr_inc_wire <= '0';
    end if;
  end process;

  Buffer_logic_sync : process(clk_i, rst_ni)
    variable l,m,n,o : natural;
  begin
    if rst_ni = '0' then
      for i in 0 to MUL_DIV_BUF_SIZE-1 loop
        MUL_DIV_buffer(i)(MUL_DIV_buf_width-1) <= '0';
      end loop;
      for i in 0 to IE_BUF_SIZE-1 loop
        IE_buffer(i)(IE_buf_width-1) <= '0';
      end loop;
      for i in 0 to LSU_BUF_SIZE-1 loop
        LSU_buffer(i)(LSU_buf_width-1) <= '0';
      end loop;
      for i in 0 to DSP_BUF_SIZE-1 loop
        DSP_buffer(i)(DSP_buf_width-1) <= '0';
      end loop;
      mul_div_wr_ptr        <= (others => '0');
      ie_wr_ptr             <= (others => '0');
      lsu_wr_ptr            <= (others => '0');
      dsp_wr_ptr            <= (others => '0');
      mul_div_issue_ptr     <= (others => '0');
      ie_issue_ptr          <= (others => '0');
      lsu_issue_ptr         <= (others => '0');
      dsp_issue_ptr         <= (others => '0');
      mul_div_issue_ptr_rob <= (others => '0');
      ie_issue_ptr_rob      <= (others => '0');
      lsu_issue_ptr_rob     <= (others => '0');
      dsp_issue_ptr_rob     <= (others => '0');
      mul_div_wr_ptr_inc    <= '0';
      ie_wr_ptr_inc         <= '0';
      lsu_wr_ptr_inc        <= '0';
      dsp_wr_ptr_inc        <= '0';
      mul_div_issue_rob     <= '0';
      ie_issue_rob          <= '0';
      lsu_issue_rob         <= '0';
      dsp_issue_rob         <= '0';
    elsif rising_edge(clk_i) then
      MUL_DIV_buffer          <= MUL_DIV_buffer_wire;
      IE_buffer               <= IE_buffer_wire;
      LSU_buffer              <= LSU_buffer_wire;
      DSP_buffer              <= DSP_buffer_wire;
      mul_div_wr_ptr_inc      <= mul_div_wr_ptr_inc_wire;
      ie_wr_ptr_inc           <= ie_wr_ptr_inc_wire;
      lsu_wr_ptr_inc          <= lsu_wr_ptr_inc_wire;
      dsp_wr_ptr_inc          <= dsp_wr_ptr_inc_wire;
      mul_div_issue_rob       <= mul_div_issue_rob_wire;
      ie_issue_rob            <= ie_issue_rob_wire;
      lsu_issue_rob           <= lsu_issue_rob_wire;
      dsp_issue_rob           <= dsp_issue_rob_wire;
      mul_div_issue_ptr_rob   <= mul_div_issue_ptr_rob_wire;
      ie_issue_ptr_rob        <= ie_issue_ptr_rob_wire;
      lsu_issue_ptr_rob       <= lsu_issue_ptr_rob_wire;
      dsp_issue_ptr_rob       <= dsp_issue_ptr_rob_wire;
      -- If an instruction is issued, than the valid instruction bit in the buffer is reset -------
      if mul_div_buf_release = '1' then
        if to_integer(unsigned(mul_div_issue_ptr))+mul_div_release_index <= MUL_DIV_BUF_SIZE-1 then
          MUL_DIV_buffer(to_integer(unsigned(mul_div_issue_ptr))+mul_div_release_index)(MUL_DIV_buf_width-1) <= '0';
        else
          MUL_DIV_buffer(to_integer(unsigned(mul_div_issue_ptr))+mul_div_release_index-MUL_DIV_BUF_SIZE)(MUL_DIV_buf_width-1) <= '0';
        end if;
      end if;
      if ie_buf_release = '1' then
        if to_integer(unsigned(ie_issue_ptr))+ie_release_index <= IE_BUF_SIZE-1 then
          IE_buffer(to_integer(unsigned(ie_issue_ptr))+ie_release_index)(IE_buf_width-1) <= '0';
        else
          IE_buffer(to_integer(unsigned(ie_issue_ptr))+ie_release_index-IE_BUF_SIZE)(IE_buf_width-1) <= '0';
        end if;
      end if;
      if lsu_buf_release = '1' then
        if to_integer(unsigned(lsu_issue_ptr))+lsu_release_index <= LSU_BUF_SIZE-1 then
          LSU_buffer(to_integer(unsigned(lsu_issue_ptr))+lsu_release_index)(LSU_buf_width-1) <= '0';
        else
          LSU_buffer(to_integer(unsigned(lsu_issue_ptr))+lsu_release_index-LSU_BUF_SIZE)(LSU_buf_width-1) <= '0';
        end if;
      end if;
      if dsp_buf_release = '1' then
        if to_integer(unsigned(dsp_issue_ptr))+dsp_release_index <= DSP_BUF_SIZE-1 then
          DSP_buffer(to_integer(unsigned(dsp_issue_ptr))+dsp_release_index)(DSP_buf_width-1) <= '0';
        else
          DSP_buffer(to_integer(unsigned(dsp_issue_ptr))+dsp_release_index-DSP_BUF_SIZE)(DSP_buf_width-1) <= '0';
        end if;
      end if;
      ---------------------------------------------------------------------------------------------

      -------------------------------------- Write Pointer ----------------------------------------
      if mul_div_buf_req = '1' then -- increment the write pointer when an instruction is buffered
        if unsigned(mul_div_wr_ptr) < MUL_DIV_BUF_SIZE-1 then
          mul_div_wr_ptr <= std_logic_vector(unsigned(mul_div_wr_ptr)+1);
        else
          mul_div_wr_ptr <= (others => '0');
        end if;
      end if;
      if ie_buf_req = '1' then -- increment the write pointer when an instruction is buffered
        if unsigned(ie_wr_ptr) < IE_BUF_SIZE-1 then
          ie_wr_ptr <= std_logic_vector(unsigned(ie_wr_ptr)+1);
        else
          ie_wr_ptr <= (others => '0');
        end if;
      end if;
      if lsu_buf_req = '1' then -- increment the write pointer when an instruction is buffered
        if unsigned(lsu_wr_ptr) < LSU_BUF_SIZE-1 then
          lsu_wr_ptr <= std_logic_vector(unsigned(lsu_wr_ptr)+1);
        else
          lsu_wr_ptr <= (others => '0');
        end if;
      end if;
      if dsp_buf_req = '1' then -- increment the write pointer when an instruction is buffered
        if unsigned(dsp_wr_ptr) < DSP_BUF_SIZE-1 then
          dsp_wr_ptr <= std_logic_vector(unsigned(dsp_wr_ptr)+1);
        else
          dsp_wr_ptr <= (others => '0');
        end if;
      end if;
      ---------------------------------------------------------------------------------------------

      -------------------------------------- Issue Pointer ----------------------------------------
      if mul_div_buf_release = '1' then -- increment the issue pointer if the issued instruction was from the buffer
        if mul_div_release_index = 0 then -- if the issued instruction was the one directly at the issue pointer
          l := 0;
          for i in 1 to MUL_DIV_BUF_SIZE-1 loop -- loop throughout the buffer until the next valid instr is found and set the issue_ptr to it
            if to_integer(unsigned(mul_div_issue_ptr))+i <= MUL_DIV_BUF_SIZE-1 then
              if MUL_DIV_buffer(to_integer(unsigned(mul_div_issue_ptr))+i)(MUL_DIV_buf_width-1) = '1' then
                mul_div_issue_ptr <= std_logic_vector(unsigned(mul_div_issue_ptr)+i);
                l := l + 1;
                exit; -- exit on the first instruction found
              end if;
            else  -- when the loop index plus the issue_ptr overflow the buffer size
              if MUL_DIV_buffer(to_integer(unsigned(mul_div_issue_ptr))+i-MUL_DIV_BUF_SIZE)(MUL_DIV_buf_width-1) = '1' then
                mul_div_issue_ptr <= std_logic_vector(unsigned(mul_div_issue_ptr)+i-MUL_DIV_BUF_SIZE);
                l := l + 1;
                exit;
              end if;
            end if;
            if i = MUL_DIV_BUF_SIZE-1 and l = 0 then
              if unsigned(mul_div_issue_ptr) = MUL_DIV_BUF_SIZE-1 then
                mul_div_issue_ptr <= (others => '0');
              else
                mul_div_issue_ptr <= std_logic_vector(unsigned(mul_div_issue_ptr)+1);
              end if;
            end if; 
          end loop;
        end if;
      end if;
      if ie_buf_release = '1' then -- increment the issue pointer if the issued instruction was from the buffer
        if ie_release_index = 0 then -- if the issued instruction was the one directly at the issue pointer
          m := 0;
          for i in 1 to IE_BUF_SIZE-1 loop -- loop throughout the buffer until the next valid instr is found and set the issue_ptr to it
            if to_integer(unsigned(ie_issue_ptr))+i <= IE_BUF_SIZE-1 then
              if IE_buffer(to_integer(unsigned(ie_issue_ptr))+i)(IE_buf_width-1) = '1' then
                ie_issue_ptr <= std_logic_vector(unsigned(ie_issue_ptr)+i);
                m := m + 1;
                exit; -- exit on the first instruction found
              end if;
            else  -- when the loop index plus the issue_ptr overflow the buffer size
              if IE_buffer(to_integer(unsigned(ie_issue_ptr))+i-IE_BUF_SIZE)(IE_buf_width-1) = '1' then
                ie_issue_ptr <= std_logic_vector(unsigned(ie_issue_ptr)+i-IE_BUF_SIZE);
                m := m + 1;
                exit;
              end if;
            end if;
            if i = IE_BUF_SIZE-1 and m = 0 then
              if unsigned(ie_issue_ptr) = IE_BUF_SIZE-1 then
                ie_issue_ptr <= (others => '0');
              else
                ie_issue_ptr <= std_logic_vector(unsigned(ie_issue_ptr)+1);
              end if;
            end if; 
          end loop;
        end if;
      end if;
      if lsu_buf_release = '1' then -- increment the issue pointer if the issued instruction was from the buffer
        if lsu_release_index = 0 then -- if the issued instruction was the one directly at the issue pointer
          n := 0;
          for i in 1 to LSU_BUF_SIZE-1 loop -- loop throughout the buffer until the next valid instr is found and set the issue_ptr to it
            if to_integer(unsigned(lsu_issue_ptr))+i <= LSU_BUF_SIZE-1 then
              if LSU_buffer(to_integer(unsigned(lsu_issue_ptr))+i)(LSU_buf_width-1) = '1' then
                lsu_issue_ptr <= std_logic_vector(unsigned(lsu_issue_ptr)+i);
                n := n + 1;
                exit; -- exit on the first instruction found
              end if;
            else  -- when the loop index plus the issue_ptr overflow the buffer size
              if LSU_buffer(to_integer(unsigned(lsu_issue_ptr))+i-LSU_BUF_SIZE)(LSU_buf_width-1) = '1' then
                lsu_issue_ptr <= std_logic_vector(unsigned(lsu_issue_ptr)+i-LSU_BUF_SIZE);
                n := n + 1;
                exit;
              end if;
            end if;
            if i = LSU_BUF_SIZE-1 and n = 0 then
              if unsigned(lsu_issue_ptr) = LSU_BUF_SIZE-1 then
                lsu_issue_ptr <= (others => '0');
              else
                lsu_issue_ptr <= std_logic_vector(unsigned(lsu_issue_ptr)+1);
              end if;
            end if; 
          end loop;
        end if;
      end if;
      if dsp_buf_release = '1' then -- increment the issue pointer if the issued instruction was from the buffer
        if dsp_release_index = 0 then -- if the issued instruction was the one directly at the issue pointer
          o := 0;
          for i in 1 to DSP_BUF_SIZE-1 loop -- loop throughout the buffer until the next valid instr is found and set the issue_ptr to it
            if to_integer(unsigned(dsp_issue_ptr))+i <= DSP_BUF_SIZE-1 then
              if DSP_buffer(to_integer(unsigned(dsp_issue_ptr))+i)(DSP_buf_width-1) = '1' then
                dsp_issue_ptr <= std_logic_vector(unsigned(dsp_issue_ptr)+i);
                o := o + 1;
                exit; -- exit on the first instruction found
              end if;
            else  -- when the loop index plus the issue_ptr overflow the buffer size
              if DSP_buffer(to_integer(unsigned(dsp_issue_ptr))+i-DSP_BUF_SIZE)(DSP_buf_width-1) = '1' then
                dsp_issue_ptr <= std_logic_vector(unsigned(dsp_issue_ptr)+i-DSP_BUF_SIZE);
                o := o + 1;
                exit;
              end if;
            end if;
            if i = DSP_BUF_SIZE-1 and n = 0 then
              if unsigned(dsp_issue_ptr) = DSP_BUF_SIZE-1 then
                dsp_issue_ptr <= (others => '0');
              else
                dsp_issue_ptr <= std_logic_vector(unsigned(dsp_issue_ptr)+1);
              end if;
            end if; 
          end loop;
        end if;
      end if;
      ---------------------------------------------------------------------------------------------
    end if;
  end process;

  -- pragma translate_off
  process(clk_i ,rst_ni)
  begin
    if rst_ni = '0' then
    elsif rising_edge(clk_i) then
      if MUL_DIV_RF_wire = '1' then
        if mul_div_buf_release = '0'  then
          pc_RF(0)         <= pc_ISSUE;
          instr_word_RF(0) <= instr_word_ISSUE;
        else
          pc_RF(0)         <=  pc_RF_wire(0);
          instr_word_RF(0) <=  instr_word_RF_wire(0);
        end if;
      end if;
      if IE_RF_wire = '1' then
        if ie_buf_release = '0'  then
          pc_RF(1)            <= pc_ISSUE;
          instr_word_RF(1)    <= instr_word_ISSUE;
        else
          pc_RF(1)         <=  pc_RF_wire(1);
          instr_word_RF(1) <=  instr_word_RF_wire(1);
        end if;
      end if;
      if LSU_RF_wire = '1' then
        if lsu_buf_release = '0'  then
          pc_RF(2)         <= pc_ISSUE;
          instr_word_RF(2) <= instr_word_ISSUE;
        else
          pc_RF(2)         <=  pc_RF_wire(2);
          instr_word_RF(2) <=  instr_word_RF_wire(2);
        end if;
      end if;
      if DSP_RF_wire = '1' then
        if dsp_buf_release = '0'  then
          pc_RF(3)         <= pc_ISSUE;
          instr_word_RF(3) <= instr_word_ISSUE;
        else
          pc_RF(3)         <= pc_RF_wire(3);
          instr_word_RF(3) <= instr_word_RF_wire(3);
        end if;
      end if;
    end if;
  end process;
  -- pragma translate_on

end ISSUE;