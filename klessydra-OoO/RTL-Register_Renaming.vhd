-- ieee packages ------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_misc.all;
use ieee.numeric_std.all;
use std.textio.all;

-- local packages ------------
use work.riscv_klessydra.all;

entity REG_RENAMING is
  generic(
    RF_SIZE                      : natural;
    RF_CEIL                      : natural;
    Logical_RF_SIZE              : natural;
    Logical_RF_CEIL              : natural
    );
  port (
    clk_i                        : in  std_logic;
    rst_ni                       : in  std_logic;
    -- flushing signals
    branch_miss                  : in  std_logic;
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
    --old_rd_valid_ISSUE           : out std_logic;  -- indicates that the instruction to the Issue-Stage has this operand valid
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
    new_rd_rename_addr_CC        : out std_logic_vector(RF_CEIL-1 downto 0); -- contains the address of the renamed operand
    -- Recovery Buffer Signals
    rnm_recovery                 : in  std_logic;
    new_rd_addr_rec              : in  std_logic_vector(Logical_RF_CEIL-1 downto 0);
    new_rd_rec                   : in  std_logic_vector(RF_CEIL-1 downto 0);
    FRL_rd_rec                   : in  std_logic_vector(RF_CEIL-1 downto 0);
    FRL_rd_ptr_rec               : in  std_logic_vector(RF_CEIL-1 downto 0);
    FRL_commit_ptr_rec           : in  std_logic_vector(RF_CEIL-1 downto 0);
    new_rd_valid_rob             : out std_logic;
    new_rd_addr_rob              : out std_logic_vector(Logical_RF_CEIL-1 downto 0);
    new_rd_rob                   : out std_logic_vector(RF_CEIL-1 downto 0);
    FRL_rd_rob                   : out std_logic_vector(RF_CEIL-1 downto 0);
    FRL_rd_ptr_rob               : out std_logic_vector(RF_CEIL-1 downto 0);
    FRL_commit_ptr_rob           : out std_logic_vector(RF_CEIL-1 downto 0)
    );
end entity; --------------------------------------

architecture RENAMING of REG_RENAMING is

signal instr_valid_RENAME_int     : std_logic;
signal instr_valid_RENAME_int_lat : std_logic;

signal RAT                        : array_3d(3 downto 0)(Logical_RF_SIZE-1 downto 0)(RF_CEIL-1 downto 0); -- Register Alias Table has four memory banks that comprise the four read port 
signal FRL                        : array_2d(RF_SIZE-1 downto 0)(RF_CEIL-1 downto 0); -- Free Register List issues a free register to the instructions that write to the regfile
signal FRL_wire                   : array_2d(RF_SIZE-1 downto 0)(RF_CEIL-1 downto 0);
signal FRL_full                   : std_logic;
signal FRL_rd_ptr_inc             : std_logic;
signal FRL_rd_ptr_inc_wire        : std_logic;
signal FRL_rd_ptr                 : std_logic_vector(RF_CEIL-1 downto 0);
signal FRL_rd_ptr_wire            : std_logic_vector(RF_CEIL-1 downto 0);
signal FRL_commit_PTR             : std_logic_vector(RF_CEIL-1 downto 0);
signal FRL_commit_ptr_wire        : std_logic_vector(RF_CEIL-1 downto 0);
signal FRL_rd_data                : std_logic_vector(RF_CEIL-1 downto 0); 
signal FRL_halt_req               : std_logic;
signal halt_renaming              : std_logic;

signal old_rd_valid_RENAME        : std_logic; -- old_rd_valid is determined here in Renaming stage

signal FRL_rec_cnt                : std_logic_vector(RF_CEIL-1 downto 0);
signal FRL_rec_cnt_wire           : std_logic_vector(RF_CEIL-1 downto 0);

signal flush_rename               : std_logic;

  function rs1 (signal instr : in std_logic_vector(31 downto 0)) return integer is -- AAA theyhave to be changed
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

begin

  flush_rename <= branch_miss;

  halt_ID <= FRL_halt_req or halt_renaming;
  --instr_valid_RENAME_int <= instr_valid_RENAME or instr_valid_RENAME_int_lat;
  instr_valid_RENAME_int <= instr_valid_RENAME or instr_valid_RENAME_int_lat when instr_valid_ISSUE = '0' else instr_valid_RENAME;


  process(clk_i, rst_ni)
  begin
    if rst_ni = '0' then
      instr_valid_RENAME_int_lat <= '0';
    elsif rising_edge(clk_i) then
      if instr_valid_RENAME = '1' then
        instr_valid_RENAME_int_lat <= '1';
      elsif halt_renaming = '0' and frl_halt_req = '0' and instr_valid_RENAME = '0' then
        instr_valid_RENAME_int_lat <= '0';
      end if;
    end if;
  end process;

  RAT_logic_synch : process(clk_i, rst_ni)
  begin
    if rst_ni = '0' then
      RAT <= (others => (others => (others => '0')));  -- Alias register x0 to r0, and this alias will be static
      instr_valid_ISSUE     <= '0';
      instr_valid_RENAME_CC <= '0';
      new_rd_valid_rob      <= '0';
    elsif rising_edge(clk_i) then
      rs1_ISSUE             <= (others => '0'); -- reset condition needed for arithmetic ops checks in the IE Unit
      rs2_ISSUE             <= (others => '0'); -- reset condition needed for arithmetic ops checks in the IE Unit
      rd_read_only_ISSUE    <= (others => '0');
      new_rd_ISSUE          <= (others => '0'); -- reset condition needed for rd(instr_word_IE) /= 0 checks in the IE Unit
      rs1_rename_addr_CC    <= (others => '0');
      rs2_rename_addr_CC    <= (others => '0');
      old_rd_rename_addr_CC <= (others => '0');
      new_rd_rename_addr_CC <= (others => '0');
      instr_valid_ISSUE     <= '0';
      instr_valid_RENAME_CC <= '0';
      MUL_DIV_instr_ISSUE   <= '0';
      IE_instr_ISSUE        <= '0';
      LSU_instr_ISSUE       <= '0';
      DSP_instr_ISSUE       <= '0';
      new_rd_valid_rob      <= '0';
      if FRL_halt_req = '0' and halt_renaming = '0' and rnm_recovery = '0' and flush_rename = '0' then
        -- RAT read ------------------------------------------------------------
        if rd_valid_RENAME = '1' then
          new_rd_ISSUE <= FRL_rd_data;  -- read renamed new rd operand directly from the FRL
          new_rd_rename_addr_CC <= FRL_rd_data;
        end if;
        if rd_valid_RENAME = '1' or rd_read_only_RENAME = '1' then
          rd_read_only_ISSUE          <= RAT(0)(rd(instr_word_RENAME));  -- sends old_rd to the ISSUE stage
          old_rd_rename_addr_CC <= RAT(0)(rd(instr_word_RENAME));  -- sends old_rd to the CC stage
          new_rd_rob            <= RAT(0)(rd(instr_word_RENAME));  -- sends old_rd to the rec stage
          new_rd_valid_rob      <= '1';
          new_rd_addr_rob       <= instr_word_RENAME(11 downto 7); -- sends logical register to recover the RAT
          FRL_rd_ptr_rob        <= FRL_rd_ptr;  -- sends the FRL rd pointer
        end if;
        if rs1_valid_RENAME = '1' then
          rs1_ISSUE          <= RAT(1)(rs1(instr_word_RENAME)); -- read renamed rs1 operand
          rs1_rename_addr_CC <= RAT(1)(rs1(instr_word_RENAME));
        end if;
        if rs2_valid_RENAME = '1' then
          rs2_ISSUE          <= RAT(2)(rs2(instr_word_RENAME)); -- read renamed rs2 operand
          rs2_rename_addr_CC <= RAT(2)(rs2(instr_word_RENAME));
        end if;
        ------------------------------------------------------------------------
        -- RAT write -----------------------------------------------------------
        for i in 0 to 2 loop
          if rd_valid_RENAME = '1' and rd_read_only_RENAME = '0' then
            RAT(i)(rd(instr_word_RENAME)) <= FRL_rd_data;  -- broadcast the new register alias assigned by the FRL to all the RAT banks
          end if;
        end loop;
        ------------------------------------------------------------------------
        if instr_valid_RENAME_int = '1' then
          -- Push the instruction data to the Issue stage ----------------------
          instr_valid_ISSUE            <= '1';
          MUL_DIV_instr_ISSUE          <= MUL_DIV_instr_RENAME;
          IE_instr_ISSUE               <= IE_instr_RENAME;
          LSU_instr_ISSUE              <= LSU_instr_RENAME;
          DSP_instr_ISSUE              <= DSP_instr_RENAME;
          rs1_valid_ISSUE              <= rs1_valid_RENAME;
          rs2_valid_ISSUE              <= rs2_valid_RENAME;
          --old_rd_valid_ISSUE           <= old_rd_valid_RENAME;
          new_rd_valid_ISSUE           <= rd_valid_RENAME;
          rd_read_only_valid_ISSUE     <= rd_read_only_RENAME;
          decoded_instr_ISSUE_MUL_DIV  <= decoded_instr_RENAME_MUL_DIV;
          decoded_instr_ISSUE_IE       <= decoded_instr_RENAME_IE;
          decoded_instr_ISSUE_LSU      <= decoded_instr_RENAME_LSU;
          decoded_instr_ISSUE_DSP      <= decoded_instr_RENAME_DSP;
          instr_word_ISSUE             <= instr_word_RENAME;
          pc_ISSUE                     <= pc_RENAME;
          -- Pass MUL_DIV related signals
          signed_op_ISSUE              <= signed_op_RENAME; 
          -- Pass IE related signals
          comparator_en_ISSUE          <= comparator_en_RENAME; 
          -- Pass LSU related signals
          load_op_ISSUE                <= load_op_RENAME;  
          store_op_ISSUE               <= store_op_RENAME;  
          data_width_ISSUE             <= data_width_RENAME;  
          data_be_ISSUE                <= data_be_RENAME;  
          -- Pass DSP related signals ------------------------------------------
          spm_rs1_ISSUE                <= spm_rs1_RENAME;
          spm_rs2_ISSUE                <= spm_rs2_RENAME;
          vec_read_rs1_ISSUE           <= vec_read_rs1_RENAME;
          vec_read_rs2_ISSUE           <= vec_read_rs2_RENAME;
          vec_write_rd_ISSUE           <= vec_write_rd_RENAME;
          vec_width_ISSUE              <= vec_width_RENAME;
          ----------------------------------------------------------------------
          -- Push the instruction data to the Commit stage ---------------------
          instr_valid_RENAME_CC        <= '1';
          rs1_rename_valid_CC          <= rs1_valid_RENAME;
          rs2_rename_valid_CC          <= rs2_valid_RENAME;
          rd_read_only_rename_valid_CC <= rd_read_only_RENAME;
          old_rd_rename_valid_CC       <= old_rd_valid_RENAME;
          new_rd_rename_valid_CC       <= rd_valid_RENAME;
          ----------------------------------------------------------------------
        end if;
      elsif rnm_recovery = '1' then
      -- RAT recover ----------------------------------------------------------
        instr_valid_ISSUE <= '0';
        for i in 0 to 2 loop
          RAT(i)(to_integer(unsigned(new_rd_addr_rec))) <= new_rd_rec;
        end loop;
      -------------------------------------------------------------------------
      end if;
    end if;
  end process;

  RAT_logic_comb : process(all) 
  begin
    old_rd_valid_RENAME <= '0';
    if rd_valid_RENAME = '1' or rd_read_only_RENAME = '1' then
      if unsigned(RAT(0)(rd(instr_word_RENAME))) /= 0 then  -- read renamed old_rd operand
        old_rd_valid_RENAME <= '1';
      end if;
    end if;
  end process;

  -- Halt the pipeline going to the ISSUE stage since the queuee for that instruction is full
  halt_renaming_comb : process(all)
  begin
    halt_renaming <= '0';
    if instr_valid_RENAME_int = '0' then
      if MUL_DIV_instr_RENAME = '1' and MUL_DIV_buff_full = '1' then 
        halt_renaming <= '1';
      end if;
      if IE_instr_RENAME = '1' and IE_buff_full = '1' then
        halt_renaming <= '1';
      end if;
      if LSU_instr_RENAME = '1' and LSU_buff_full = '1' then
        halt_renaming <= '1';
      end if;
      if DSP_instr_RENAME = '1' and DSP_buff_full = '1' then
        halt_renaming <= '1';
      end if;
    end if;
  end process;

  FRL_logic_synch : process(clk_i, rst_ni) 
  begin
    if rst_ni = '0' then
      FRL_rd_ptr_inc <= '0';
      FRL_rd_ptr     <= (1 to RF_CEIL-1 => '0') & '1'; -- register 0 is never assigned by the FRL since it is always a free register equal to 0 (x0=x"0000_0000")
      FRL_commit_ptr <= (1 to RF_CEIL-1 => '0') & '1';
      FRL_rec_cnt    <= (others => '0');
      FRL_rd_rob     <= (others => '0');
      for i in 0 to RF_SIZE-1 loop
        FRL(i) <= std_logic_vector(to_unsigned(i,RF_CEIL)); -- Intialize the free register list entries with the loop index i
      end loop;
    elsif rising_edge(clk_i) then
      FRL            <= FRL_wire;
      FRL_rd_ptr     <= FRL_rd_ptr_wire;
      FRL_rd_ptr_inc <= FRL_rd_ptr_inc_wire;
      FRL_commit_ptr <= FRL_commit_ptr_wire;
      FRL_rec_cnt    <= FRL_rec_cnt_wire;
      FRL_rd_rob     <= FRL_rd_data; -- send the frl data to the recovery rob
      if unsigned(FRL_commit_en) /= 0 then
        FRL_commit_ptr_rob <= FRL_commit_ptr;
      end if;
    end if;
  end process;

  FRL_logic_comb : process(all)
  begin
    FRL_wire            <= FRL;
    FRL_rd_ptr_wire     <= FRL_rd_ptr;
    FRL_commit_ptr_wire <= FRL_commit_ptr;
    FRL_halt_req        <= '0';
    FRL_rd_ptr_inc_wire <= FRL_rd_ptr_inc;
    FRL_full            <= '0';
    FRL_rec_cnt_wire    <= (others => '0');
    if FRL_rd_ptr_inc = '1' and FRL_rd_ptr = FRL_commit_ptr then
      FRL_full <= '1';
    end if;
    if unsigned(FRL_commit_en) > 0 and rd_valid_RENAME = '0' then
      FRL_rd_ptr_inc_wire <= '0';
    end if;
    -- FRL read -------------------------------------------------------------
    if instr_valid_RENAME_int = '1' and flush_rename = '0' then
      if rd_valid_RENAME = '1' and rd_read_only_RENAME = '0' then  -- enable the FRL request when the 'rd' operand is valid and it is not read only
        if FRL_full = '0' then -- check if the FRL is FULL before a assigning a free register
          FRL_rd_data <= FRL(to_integer(unsigned(FRL_rd_ptr)));
          FRL_rd_ptr_inc_wire <= '1';  -- indicates that the read pointer was incremented, this is used to check is the FRL became full or not
          if unsigned(FRL_rd_ptr) /= RF_SIZE-1 then
            FRL_rd_ptr_wire <= std_logic_vector(unsigned(FRL_rd_ptr)+1);
          else
            FRL_rd_ptr_wire <= (1 to RF_CEIL-1 => '0') & '1';
          end if;
        else -- If the FRL is full halt the pipeline untill a register is committed by the CC
          FRL_halt_req <= '1';
        end if;
      end if;
    end if;
    -------------------------------------------------------------------------

    -- FRL commit -----------------------------------------------------------
      -- free the commited register by incrementing the commit pointer
    if unsigned(FRL_commit_en) /= 0 then
      for i in 0 to 11 loop -- the maximum decrement count is 12
        if i <= unsigned(commit_count)-1 then -- commit_count-1 because the index i starts from 0
          if unsigned(FRL_commit_PTR) + i <= RF_SIZE-1 then
            FRL_wire(to_integer(unsigned(FRL_commit_ptr)+i)) <= FRL_commit_addr(i);
          else
            ------------------------------------------------------------------------------------
            -- The index of FRL_wire is the value of the commit_ptr plus the commmit_count    --
            -- index minus the regfile size will be give the number of overflow regs.         --
            -- The addition of the 1 is because register 'x0' is never given as a destination --
            ------------------------------------------------------------------------------------
            FRL_wire(1+(i+to_integer(unsigned(FRL_commit_PTR))-RF_SIZE)) <= FRL_commit_addr(i); 
          end if;
        end if;
      end loop;
      if unsigned(FRL_commit_ptr) + unsigned(commit_count) <= RF_SIZE-1 then
        FRL_commit_ptr_wire <= std_logic_vector(unsigned(FRL_commit_ptr)+unsigned(commit_count));
      else
        FRL_commit_ptr_wire <= std_logic_vector(1+(unsigned(FRL_commit_ptr)+unsigned(commit_count)-RF_SIZE));
      end if;
    end if;
    -------------------------------------------------------------------------

    -- FRL recover ----------------------------------------------------------
    if rnm_recovery = '1' then
      FRL_commit_ptr_wire <= FRL_commit_ptr_rec;
      FRL_rd_ptr_wire     <= FRL_rd_ptr_rec;
      FRL_rec_cnt_wire    <= std_logic_vector(unsigned(FRL_rec_cnt)+1); -- increment the offset to the read pointer
      FRL_wire(to_integer(unsigned(FRL_rd_ptr_rec)+unsigned(FRL_rec_cnt))) <= FRL_rd_rec;
    end if;
    -------------------------------------------------------------------------
  end process;

end RENAMING;