-- ieee packages ------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_misc.all;
use ieee.numeric_std.all;
use std.textio.all;

-- local packages ------------
use work.riscv_klessydra.all;

entity COMMIT_COUNTER is
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
    dsp_ptr_RF_CC                : in  array_2D(2 downto 0)(RF_CEIL-1 downto 0);
    -- Recovery Buffer Signals
    CC_recovery                  : in  std_logic;
    count_addr_rec               : in  array_2D(15 downto 0)(RF_CEIL-1 downto 0);
    count_data_rec               : in  array_2D(15 downto 0)(RF_CEIL-1 downto 0);
    count_addr_rob               : out array_2D(15 downto 0)(RF_CEIL-1 downto 0);
    count_data_rob               : out array_2D(15 downto 0)(RF_CEIL-1 downto 0)
  );
end entity; --------------------------------------

architecture CC of COMMIT_COUNTER is
 
signal ctr_en               : std_logic_vector(RF_SIZE-1 downto 0); -- a buffer that stopres the enables for the commit counters
signal ctr_en_lat           : std_logic_vector(RF_SIZE-1 downto 0); -- a buffer that stopres the enables for the commit counters
signal CC_buffer            : array_2D(RF_SIZE-1 downto 0)(RF_CEIL-1 downto 0); -- commit counting buffer, each buffer has 5-bits of count, indicating the max allowed count
signal CC_buffer_wire       : array_2D(RF_SIZE-1 downto 0)(RF_CEIL-1 downto 0); -- wire signal of "CC_buffer"

signal old_rd_valid         : std_logic_vector(2 downto 0); -- contains all the old_rd valids in one array 
signal old_rd_data          : array_2D(2 downto 0)(31 downto 0); -- contains the old_rd results from the MUL_DIV, LSU, and IE in an array

signal inc_rnm_rs1          : std_logic_vector(RF_CEIL-1 downto 0);
signal inc_rnm_rs2          : std_logic_vector(RF_CEIL-1 downto 0);
signal inc_rnm_new_rd       : std_logic_vector(RF_CEIL-1 downto 0);
signal dec_rnm_old_rd       : std_logic_vector(RF_CEIL-1 downto 0);
signal dec_mul_div_rs1      : std_logic_vector(RF_CEIL-1 downto 0);
signal dec_mul_div_rs2      : std_logic_vector(RF_CEIL-1 downto 0);
signal dec_mul_div_new_rd   : std_logic_vector(RF_CEIL-1 downto 0);
signal dec_ie_rs1           : std_logic_vector(RF_CEIL-1 downto 0);
signal dec_ie_rs2           : std_logic_vector(RF_CEIL-1 downto 0);
signal dec_ie_new_rd        : std_logic_vector(RF_CEIL-1 downto 0);
signal dec_lsu_rs1          : std_logic_vector(RF_CEIL-1 downto 0);
signal dec_lsu_rs2          : std_logic_vector(RF_CEIL-1 downto 0);
signal dec_lsu_new_rd       : std_logic_vector(RF_CEIL-1 downto 0);
signal dec_dsp_rs1          : std_logic_vector(RF_CEIL-1 downto 0);
signal dec_dsp_rs2          : std_logic_vector(RF_CEIL-1 downto 0);
signal dec_dsp_rd_read      : std_logic_vector(RF_CEIL-1 downto 0);

signal FRL_commit_addr_wire : array_2D(11 downto 0)(RF_CEIL-1 downto 0);
signal FRL_commit_en_wire   : std_logic_vector(11 downto 0);
signal commit_count_wire    : std_logic_vector(3 downto 0);

-- recovery signals
signal ctr_rec_en           : array_2D(RF_SIZE-1 downto 0)(2 downto 0);

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

begin

  Count_logic_comb : process(all)
  begin
    CC_buffer_wire     <= CC_buffer;
    ctr_en             <= (others => '0');
    inc_rnm_rs1        <= (others => '0');
    inc_rnm_rs2        <= (others => '0');
    inc_rnm_new_rd     <= (others => '0');
    dec_rnm_old_rd     <= (others => '0');
    dec_mul_div_rs1    <= (others => '0');
    dec_mul_div_rs2    <= (others => '0');
    dec_mul_div_new_rd <= (others => '0');
    dec_ie_rs1         <= (others => '0');
    dec_ie_rs2         <= (others => '0');
    dec_ie_new_rd      <= (others => '0');
    dec_lsu_rs1        <= (others => '0');
    dec_lsu_rs2        <= (others => '0');
    dec_lsu_new_rd     <= (others => '0');
    dec_dsp_rs1        <= (others => '0');
    dec_dsp_rs2        <= (others => '0');
    dec_dsp_rd_read    <= (others => '0');

    ctr_rec_en         <= (others => (others => '0'));

    if instr_valid_RENAME_CC = '1' then
      if rs1_rename_valid_CC = '1' and unsigned(rs1_rename_addr_CC) /= 0 then
        inc_rnm_rs1(to_integer(unsigned(rs1_rename_addr_CC))) <= '1';
        ctr_en(to_integer(unsigned(rs1_rename_addr_CC))) <= '1';
        count_addr_rob(0) <= rs1_rename_addr_CC;
        count_data_rob(0) <= CC_buffer(to_integer(unsigned(rs1_rename_addr_CC)));
      end if;
      if rs2_rename_valid_CC = '1' and unsigned(rs2_rename_addr_CC) /= 0 then
        inc_rnm_rs2(to_integer(unsigned(rs2_rename_addr_CC))) <= '1';
        ctr_en(to_integer(unsigned(rs2_rename_addr_CC))) <= '1';
        count_addr_rob(1) <= rs2_rename_addr_CC;
        count_data_rob(1) <= CC_buffer(to_integer(unsigned(rs2_rename_addr_CC)));
      end if;
      if new_rd_rename_valid_CC then
        inc_rnm_new_rd(to_integer(unsigned(new_rd_rename_addr_CC))) <= '1';
        ctr_en(to_integer(unsigned(new_rd_rename_addr_CC))) <= '1';
        count_addr_rob(2) <= new_rd_rename_addr_CC;
        count_data_rob(2) <= CC_buffer(to_integer(unsigned(new_rd_rename_addr_CC)));
      end if;
      if rd_read_only_rename_valid_CC or old_rd_rename_valid_CC then
        dec_rnm_old_rd(to_integer(unsigned(old_rd_rename_addr_CC))) <= '1';
        ctr_en(to_integer(unsigned(old_rd_rename_addr_CC))) <= '1';
        count_addr_rob(3) <= old_rd_rename_addr_CC;
        count_data_rob(3) <= CC_buffer(to_integer(unsigned(old_rd_rename_addr_CC)));
      end if;
    end if;
    if unsigned(instr_valid_RF_CC) > 0 then
      if mul_div_valid_RF_CC(0) = '1' and unsigned(mul_div_ptr_RF_CC(0)) /= 0 then
        dec_mul_div_rs1(to_integer(unsigned(mul_div_ptr_RF_CC(0)))) <= '1';
        ctr_en(to_integer(unsigned(mul_div_ptr_RF_CC(0)))) <= '1';
        count_addr_rob(4) <= mul_div_ptr_RF_CC(0);
        count_data_rob(4) <= CC_buffer(to_integer(unsigned(mul_div_ptr_RF_CC(0))));
      end if;
      if mul_div_valid_RF_CC(1) = '1' and unsigned(mul_div_ptr_RF_CC(1)) /= 0 then
        dec_mul_div_rs2(to_integer(unsigned(mul_div_ptr_RF_CC(1))))    <= '1';
        ctr_en(to_integer(unsigned(mul_div_ptr_RF_CC(1)))) <= '1';
        count_addr_rob(5) <= mul_div_ptr_RF_CC(1);
        count_data_rob(5) <= CC_buffer(to_integer(unsigned(mul_div_ptr_RF_CC(1))));
      end if;
      if mul_div_valid_RF_CC(2) = '1' and unsigned(mul_div_ptr_RF_CC(2)) /= 0 then
        dec_mul_div_new_rd(to_integer(unsigned(mul_div_ptr_RF_CC(2)))) <= '1';
        ctr_en(to_integer(unsigned(mul_div_ptr_RF_CC(2)))) <= '1';
        count_addr_rob(6) <= mul_div_ptr_RF_CC(2);
        count_data_rob(6) <= CC_buffer(to_integer(unsigned(mul_div_ptr_RF_CC(2))));
      end if;
      if ie_valid_RF_CC(0) = '1' and unsigned(ie_ptr_RF_CC(0)) /= 0 then
        dec_ie_rs1(to_integer(unsigned(ie_ptr_RF_CC(0)))) <= '1';
        ctr_en(to_integer(unsigned(ie_ptr_RF_CC(0)))) <= '1';
        count_addr_rob(7) <= ie_ptr_RF_CC(0);
        count_data_rob(7) <= CC_buffer(to_integer(unsigned(ie_ptr_RF_CC(0))));
      end if;
      if ie_valid_RF_CC(1) = '1' and unsigned(ie_ptr_RF_CC(1)) /= 0 then
        dec_ie_rs2(to_integer(unsigned(ie_ptr_RF_CC(1)))) <= '1';
        ctr_en(to_integer(unsigned(ie_ptr_RF_CC(1)))) <= '1';
        count_addr_rob(8) <= ie_ptr_RF_CC(1);
        count_data_rob(8) <= CC_buffer(to_integer(unsigned(ie_ptr_RF_CC(1))));
      end if;
      if ie_valid_RF_CC(2) = '1' and unsigned(ie_ptr_RF_CC(2)) /= 0 then
        dec_ie_new_rd(to_integer(unsigned(ie_ptr_RF_CC(2)))) <= '1';
        ctr_en(to_integer(unsigned(ie_ptr_RF_CC(2)))) <= '1';
        count_addr_rob(9) <= ie_ptr_RF_CC(2);
        count_data_rob(9) <= CC_buffer(to_integer(unsigned(ie_ptr_RF_CC(2))));
      end if;
      if lsu_valid_RF_CC(0) = '1' and unsigned(lsu_ptr_RF_CC(0)) /= 0 then 
        dec_lsu_rs1(to_integer(unsigned(lsu_ptr_RF_CC(0)))) <= '1';
        ctr_en(to_integer(unsigned(lsu_ptr_RF_CC(0)))) <= '1';
        count_addr_rob(10) <= lsu_ptr_RF_CC(0);
        count_data_rob(10) <= CC_buffer(to_integer(unsigned(lsu_ptr_RF_CC(0))));
      end if;
      if lsu_valid_RF_CC(1) = '1' and unsigned(lsu_ptr_RF_CC(1)) /= 0 then 
        dec_lsu_rs2(to_integer(unsigned(lsu_ptr_RF_CC(1)))) <= '1';
        ctr_en(to_integer(unsigned(lsu_ptr_RF_CC(1)))) <= '1';
        count_addr_rob(11) <= lsu_ptr_RF_CC(1);
        count_data_rob(11) <= CC_buffer(to_integer(unsigned(lsu_ptr_RF_CC(1))));
      end if;
      if lsu_valid_RF_CC(2) = '1' and unsigned(lsu_ptr_RF_CC(2)) /= 0 then 
        dec_lsu_new_rd(to_integer(unsigned(lsu_ptr_RF_CC(2)))) <= '1';
        ctr_en(to_integer(unsigned(lsu_ptr_RF_CC(2)))) <= '1';
        count_addr_rob(12) <= lsu_ptr_RF_CC(2);
        count_data_rob(12) <= CC_buffer(to_integer(unsigned(lsu_ptr_RF_CC(2))));
      end if;
      if dsp_valid_RF_CC(0) = '1' and unsigned(dsp_ptr_RF_CC(0)) /= 0 then
        dec_dsp_rs1(to_integer(unsigned(dsp_ptr_RF_CC(0)))) <= '1';
        ctr_en(to_integer(unsigned(dsp_ptr_RF_CC(0)))) <= '1';
        count_addr_rob(13) <= dsp_ptr_RF_CC(0);
        count_data_rob(13) <= CC_buffer(to_integer(unsigned(dsp_ptr_RF_CC(0))));
      end if;
      if dsp_valid_RF_CC(1) = '1' and unsigned(dsp_ptr_RF_CC(1)) /= 0 then
        dec_dsp_rs2(to_integer(unsigned(dsp_ptr_RF_CC(1)))) <= '1';
        ctr_en(to_integer(unsigned(dsp_ptr_RF_CC(1)))) <= '1';
        count_addr_rob(14) <= dsp_ptr_RF_CC(1);
        count_data_rob(14) <= CC_buffer(to_integer(unsigned(dsp_ptr_RF_CC(1))));
      end if;
      if dsp_valid_RF_CC(2) = '1' and unsigned(dsp_ptr_RF_CC(2)) /= 0 then
        dec_dsp_rd_read(to_integer(unsigned(dsp_ptr_RF_CC(2)))) <= '1';
        ctr_en(to_integer(unsigned(dsp_ptr_RF_CC(2)))) <= '1';
        count_addr_rob(15) <= dsp_ptr_RF_CC(2);
        count_data_rob(15) <= CC_buffer(to_integer(unsigned(dsp_ptr_RF_CC(2))));
      end if;
    end if;
    if CC_recovery = '1' then
      for i in 0 to 2 loop
        ctr_rec_en(to_integer(unsigned(count_addr_rec(i))))(i) <= '1';
      end loop;
    end if;
    ---------------------------------------------------------------------------------------
    if CC_recovery = '0' then
      for i in 0 to RF_SIZE-1 loop
        if ctr_en(i) = '1' then
          CC_buffer_wire(i) <= std_logic_vector(unsigned(CC_buffer(i)) 
                                                      + inc_rnm_rs1(i)
                                                      + inc_rnm_rs2(i)
                                                      + inc_rnm_new_rd(i)
                                                      + inc_rnm_new_rd(i)  -- incremented twice
                                                      - dec_rnm_old_rd(i)
                                                      - dec_mul_div_rs1(i)
                                                      - dec_mul_div_rs2(i)
                                                      - dec_mul_div_new_rd(i)
                                                      - dec_ie_rs1(i)
                                                      - dec_ie_rs2(i)
                                                      - dec_ie_new_rd(i)
                                                      - dec_lsu_rs1(i)
                                                      - dec_lsu_rs2(i)
                                                      - dec_lsu_new_rd(i)
                                                      - dec_dsp_rs1(i)
                                                      - dec_dsp_rs2(i)
                                                      - dec_dsp_rd_read(i));
        end if;
      end loop;
    else
      for i in 0 to RF_SIZE-1 loop
        if unsigned(ctr_rec_en(i)) /= 0 then
          CC_buffer_wire(i) <= std_logic_vector(unsigned(CC_buffer(i)) 
                                              + unsigned(count_data_rec(0))    -- Set to either +1 or -1 from the rob
                                              + unsigned(count_data_rec(1))    -- Set to either +1 or -1 from the rob
                                              + unsigned(count_data_rec(2))    -- Set to either +1 or -1 from the rob
                                              + unsigned(count_data_rec(3))    -- Set to either +1 or -1 from the rob
                                              + unsigned(count_data_rec(4))    -- Set to either +1 or -1 from the rob
                                              + unsigned(count_data_rec(5))    -- Set to either +1 or -1 from the rob
                                              + unsigned(count_data_rec(6))    -- Set to either +1 or -1 from the rob
                                              + unsigned(count_data_rec(7))    -- Set to either +1 or -1 from the rob
                                              + unsigned(count_data_rec(8))    -- Set to either +1 or -1 from the rob
                                              + unsigned(count_data_rec(9))    -- Set to either +1 or -1 from the rob
                                              + unsigned(count_data_rec(10))   -- Set to either +1 or -1 from the rob
                                              + unsigned(count_data_rec(11))   -- Set to either +1 or -1 from the rob
                                              + unsigned(count_data_rec(12))   -- Set to either +1 or -1 from the rob
                                              + unsigned(count_data_rec(13))   -- Set to either +1 or -1 from the rob
                                              + unsigned(count_data_rec(14))   -- Set to either +1 or -1 from the rob
                                              + unsigned(count_data_rec(15))); -- Set to either +1 or -1 from the rob
        end if; 
      end loop;
    end if;
  end process;

  Commit_logic_comb : process(all)
    variable k : natural;
  begin
    FRL_commit_addr_wire <= (others => (others => '0'));
    FRL_commit_en_wire   <= (others => '0');
    k := 0;
    for i in 0 to RF_SIZE-1 loop
      if ctr_en_lat(i) = '1' and unsigned(CC_buffer(i)) = 0 then
        FRL_commit_en_wire(k)   <= '1';
        FRL_commit_addr_wire(k) <= std_logic_vector(to_unsigned(i,RF_CEIL));
        k := k + 1;
      end if;
    end loop;
  end process;

  commit_count_wire <= std_logic_vector(to_unsigned(add_vect_bits(FRL_commit_en_wire),4)); -- calls the function which the the bits of the std_logic_vector
  old_rd_count <= std_logic_vector(to_unsigned(add_vect_bits(mul_div_old_rd_valid_CC & ie_old_rd_valid_CC & lsu_old_rd_valid_CC),2));
  --commit_count <= std_logic_vector(unsigned(FRL_commit_en(0)) + FRL_commit_en(1) + FRL_commit_en(2) + FRL_commit_en(3));
  --old_rd_count <= std_logic_vector(unsigned(mul_div_old_rd_valid_CC) + unsigned(ie_old_rd_valid_CC) + unsigned(lsu_old_rd_valid_CC));
  old_rd_valid <= lsu_old_rd_valid_CC & ie_old_rd_valid_CC & mul_div_old_rd_valid_CC;
  old_rd_data  <= (lsu_old_rd_CC, ie_old_rd_CC, mul_div_old_rd_CC);

  process(clk_i, rst_ni)
  begin
    if rst_ni = '0' then
      CC_buffer       <= (others => (others => '0'));
      FRL_commit_addr <= (others => (others => '0'));
      FRL_commit_en   <= (others => '0');
      ctr_en_lat      <= (others => '0');
      commit_count    <= (others => '0');
    elsif rising_edge(clk_i) then
      CC_buffer       <= CC_buffer_wire;
      ctr_en_lat      <= ctr_en;
      FRL_commit_en   <= FRL_commit_en_wire;
      FRL_commit_addr <= FRL_commit_addr_wire;
      commit_count    <= commit_count_wire;
    end if;
  end process;

  --Recovery_Buffer_comb : process(all)
  --begin
  --  old_rd_ptr_inc_wire <= (others => '0');
  --  if old_rd_count /= "00" then
  --    old_rd_ptr_inc_wire <= '1';
  --  end if;
  --end process;

  --Recovery_Buffer_sync : process(clk_i, rst_ni)
  --begin
  --  if rst_ni = 0 then
  --    Recovery_Buffer_Full <= '0';
  --    old_rd_ptr_inc <= (others => '0');
  --    for i in 0 to 2 loop
  --      unsigned(old_rd_ptr(i)) <= i;
  --    end loop;
  --    for i in 0 to 3 loop
  --      unsigned(rec_buf_commit_ptr(i)) <= i;
  --    end loop;
  --  elsif rising_edge(clk_i) then
  --    old_rd_ptr_inc <= old_rd_ptr_inc_wire;
  --    if old_rd_count = "01" then
  --      for i in 0 to 2 loop
  --        if old_rd_valid(i) = '1' then
  --          -- for one valid result we write always to the ptr(0) position
  --          Recovery_Buffer(to_integer(unsigned(old_rd_ptr(0)))) <= old_rd_data(i);
  --        end if;
  --      end loop;
  --    elsif old_rd_count = "10" then
  --      if old_rd_valid(0) = '1' then
  --        -- for two valid results, if the "0" index is valid, than we write it to the ptr(0) position
  --        Recovery_Buffer(to_integer(unsigned(old_rd_ptr(0)))) <= old_rd_data(0);
  --      end if;
  --      if old_rd_valid(1) = '1' and old_rd_valid(0) = '0' then
  --        -- for two valid results, if the "1" index is valid, and the "0" is not than we write to the ptr(0) position
  --        Recovery_Buffer(to_integer(unsigned(old_rd_ptr(0)))) <= old_rd_data(1);
  --      end if;
  --      if old_rd_valid(1) = '1' and old_rd_valid(0) = '1' then
  --        -- for two valid results, if the "1" index is valid, and the "0" is also valid than we write to the ptr(1) position
  --        Recovery_Buffer(to_integer(unsigned(old_rd_ptr(1)))) <= old_rd_data(1);
  --      end if;
  --      if old_rd_valid(2) = '1' then
  --        -- for two valid results, if the "2" index is valid, than we write it to the ptr(1) position
  --        Recovery_Buffer(to_integer(unsigned(old_rd_ptr(1)))) <= old_rd_data(2);
  --      end if;
  --    elsif old_rd_count = "11" then
  --      for i in 0 to 2 loop
  --        Recovery_Buffer(to_integer(unsigned(old_rd_ptr(i)))) <= old_rd_data(i);
  --      end loop;
  --    end if;
  --    ----------------- old_rd Pointer Increment Logic ------------------- 
  --    for i in 0 to 2 loop
  --      if unsigned(old_rd_count = i+1) then
  --        old_rd_ptr(i) <= std_logic_vector(unsigned(old_rd_ptr(i)) + i);
  --      end if;
  --    end loop;
  --    --------------------------------------------------------------------
  --    ----------------- commit Pointer Increment Logic ------------------- 
  --    for i in 0 to 3 loop
  --      if unsigned(commit_count) = i=1 then
  --        rec_buf_commit_ptr(i) <= std_logic_vector(unsigned(rec_buf_commit_ptr(i)) + i+1);
  --      end if;
  --    end loop;
  --    --------------------------------------------------------------------
  --    ------------------- Recovery Buffer Full Logic --------------------- 
  --    if old_rd_ptr_inc = '1' and old_rd_ptr = rec_buf_commit_ptr then
  --      Recovery_Buffer_Full <= '1';
  --    else old_rd_ptr /= rec_buf_commit_ptr then
  --      Recovery_Buffer_Full <= '0';      
  --    end if;
  --    --------------------------------------------------------------------
  --  end if;
  --end process;

end CC;