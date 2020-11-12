-- ieee packages ------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_misc.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
use std.textio.all;

-- local packages ------------
use work.riscv_klessydra.all;

entity REORDER_BUFFER is
  generic (
    ROB_BUF_SIZE      : natural;
    ROB_BUF_CEIL      : natural;
    RF_CEIL           : natural;
    Logical_RF_CEIL   : natural;
    XLEN              : natural;
    MUL_DIV_BUF_SIZE  : natural;
    IE_BUF_SIZE       : natural;
    LSU_BUF_SIZE      : natural;
    DSP_BUF_SIZE      : natural;
    MUL_DIV_BUF_CEIL  : natural := integer(ceil(log2(real(MUL_DIV_BUF_SIZE))));
    IE_BUF_CEIL       : natural := integer(ceil(log2(real(IE_BUF_SIZE))));
    LSU_BUF_CEIL      : natural := integer(ceil(log2(real(LSU_BUF_SIZE))));
    DSP_BUF_CEIL      : natural := integer(ceil(log2(real(DSP_BUF_SIZE))))
    );
  port (
    -- core signals
    clk_i                        : in  std_logic;
    rst_ni                       : in  std_logic;
    -- ROB Trigger Signals
    branch_miss                  : in  std_logic;
    set_except_condition         : in  std_logic;
    -- pipeline instruction valids
    instr_rvalid_ID              : in  std_logic;
    instr_valid_RENAME           : in  std_logic;
    -- Decode Unit Signals
    decode_recovery              : out std_logic;
    -- Rename Unit Signals
    rnm_recovery                 : out std_logic;
    new_rd_addr_rec              : out std_logic_vector(Logical_RF_CEIL-1 downto 0);
    new_rd_rec                   : out std_logic_vector(RF_CEIL-1 downto 0);
    FRL_rd_rec                   : out std_logic_vector(RF_CEIL-1 downto 0);
    FRL_rd_ptr_rec               : out std_logic_vector(RF_CEIL-1 downto 0);
    FRL_commit_ptr_rec           : out std_logic_vector(RF_CEIL-1 downto 0);
    new_rd_valid_rob             : in  std_logic;
    new_rd_addr_rob              : in  std_logic_vector(Logical_RF_CEIL-1 downto 0);
    new_rd_rob                   : in  std_logic_vector(RF_CEIL-1 downto 0);
    FRL_rd_rob                   : in  std_logic_vector(RF_CEIL-1 downto 0);
    FRL_rd_ptr_rob               : in  std_logic_vector(RF_CEIL-1 downto 0);
    FRL_commit_ptr_rob           : in  std_logic_vector(RF_CEIL-1 downto 0);
    -- Issue Unit Signals
    issue_recovery               : out std_logic;
    mul_div_issue_rec            : out std_logic;
    ie_issue_rec                 : out std_logic;
    lsu_issue_rec                : out std_logic;
    dsp_issue_rec                : out std_logic;
    mul_div_issue_ptr_rec        : out std_logic_vector(MUL_DIV_BUF_CEIL-1 downto 0);
    ie_issue_ptr_rec             : out std_logic_vector(IE_BUF_CEIL-1 downto 0);
    lsu_issue_ptr_rec            : out std_logic_vector(LSU_BUF_CEIL-1 downto 0);
    dsp_issue_ptr_rec            : out std_logic_vector(DSP_BUF_CEIL-1 downto 0);
    mul_div_issue_rob            : in  std_logic;
    ie_issue_rob                 : in  std_logic;
    lsu_issue_rob                : in  std_logic;
    dsp_issue_rob                : in  std_logic;
    mul_div_issue_ptr_rob        : in  std_logic_vector(MUL_DIV_BUF_CEIL-1 downto 0);
    ie_issue_ptr_rob             : in  std_logic_vector(IE_BUF_CEIL-1 downto 0);
    lsu_issue_ptr_rob            : in  std_logic_vector(LSU_BUF_CEIL-1 downto 0);
    dsp_issue_ptr_rob            : in  std_logic_vector(DSP_BUF_CEIL-1 downto 0);
    -- Regfile Entity Signals
    mul_div_rd_valid             : in  std_logic;
    ie_rd_valid                  : in  std_logic;
    lsu_rd_valid                 : in  std_logic;
    mul_div_addr_new_rd          : in  std_logic_vector(RF_CEIL-1 downto 0);
    ie_addr_new_rd               : in  std_logic_vector(RF_CEIL-1 downto 0);
    lsu_addr_new_rd              : in  std_logic_vector(RF_CEIL-1 downto 0);
    mul_div_data_new_rd          : in  std_logic_vector(XLEN-1 downto 0);
    ie_data_new_rd               : in  std_logic_vector(XLEN-1 downto 0);
    lsu_data_new_rd              : in  std_logic_vector(XLEN-1 downto 0);
    regfile_recovery             : out std_logic;
    rf_addr_rec                  : out array_2D(2 downto 0)(RF_CEIL-1 downto 0);
    rf_data_rec                  : out array_2D(2 downto 0)(XLEN-1 downto 0);
    -- Commit Counter Signals
    CC_recovery                  : out std_logic;
    count_addr_rec               : out  array_2D(15 downto 0)(RF_CEIL-1 downto 0);
    count_data_rec               : out  array_2D(15 downto 0)(RF_CEIL-1 downto 0);
    count_addr_rob               : in  array_2D(15 downto 0)(RF_CEIL-1 downto 0);
    count_data_rob               : in  array_2D(15 downto 0)(RF_CEIL-1 downto 0);
    -- Instruction tag
    instr_tag                    : out std_logic_vector(ROB_BUF_CEIL-1 downto 0)
    );
end entity; --------------------------------------

architecture ROB of REORDER_BUFFER is

  signal ROB_valid_instr              : std_logic_vector(ROB_BUF_SIZE-1 downto 0);

  signal ROB_new_rd_valid             : std_logic_vector(ROB_BUF_SIZE-1 downto 0);
  signal ROB_new_rd_addr              : array_2D(ROB_BUF_SIZE-1 downto 0)(Logical_RF_CEIL-1 downto 0);
  signal ROB_new_rd                   : array_2D(ROB_BUF_SIZE-1 downto 0)(RF_CEIL-1 downto 0);
  signal ROB_FRL_rd                   : array_2D(ROB_BUF_SIZE-1 downto 0)(RF_CEIL-1 downto 0);
  signal ROB_FRL_rd_ptr               : array_2D(ROB_BUF_SIZE-1 downto 0)(RF_CEIL-1 downto 0);
  signal ROB_FRL_commit_ptr           : array_2D(ROB_BUF_SIZE-1 downto 0)(RF_CEIL-1 downto 0);
  signal ROB_new_rd_valid_wire        : std_logic_vector(ROB_BUF_SIZE-1 downto 0);
  signal ROB_new_rd_addr_wire         : array_2D(ROB_BUF_SIZE-1 downto 0)(Logical_RF_CEIL-1 downto 0);
  signal ROB_new_rd_wire              : array_2D(ROB_BUF_SIZE-1 downto 0)(RF_CEIL-1 downto 0);
  signal ROB_FRL_rd_wire              : array_2D(ROB_BUF_SIZE-1 downto 0)(RF_CEIL-1 downto 0);
  signal ROB_FRL_rd_ptr_wire          : array_2D(ROB_BUF_SIZE-1 downto 0)(RF_CEIL-1 downto 0);
  signal ROB_FRL_commit_ptr_wire      : array_2D(ROB_BUF_SIZE-1 downto 0)(RF_CEIL-1 downto 0);

  signal ROB_mul_div_issue            : std_logic_vector(ROB_BUF_SIZE-1 downto 0);
  signal ROB_ie_issue                 : std_logic_vector(ROB_BUF_SIZE-1 downto 0);
  signal ROB_lsu_issue                : std_logic_vector(ROB_BUF_SIZE-1 downto 0);
  signal ROB_dsp_issue                : std_logic_vector(ROB_BUF_SIZE-1 downto 0);
  signal ROB_mul_div_issue_ptr        : array_2D(ROB_BUF_SIZE-1 downto 0)(MUL_DIV_BUF_CEIL-1 downto 0);
  signal ROB_ie_issue_ptr             : array_2D(ROB_BUF_SIZE-1 downto 0)(IE_BUF_CEIL-1 downto 0);
  signal ROB_lsu_issue_ptr            : array_2D(ROB_BUF_SIZE-1 downto 0)(LSU_BUF_CEIL-1 downto 0);
  signal ROB_dsp_issue_ptr            : array_2D(ROB_BUF_SIZE-1 downto 0)(DSP_BUF_CEIL-1 downto 0);
  signal ROB_mul_div_issue_wire       : std_logic_vector(ROB_BUF_SIZE-1 downto 0);
  signal ROB_ie_issue_wire            : std_logic_vector(ROB_BUF_SIZE-1 downto 0);
  signal ROB_lsu_issue_wire           : std_logic_vector(ROB_BUF_SIZE-1 downto 0);
  signal ROB_dsp_issue_wire           : std_logic_vector(ROB_BUF_SIZE-1 downto 0);
  signal ROB_mul_div_issue_ptr_wire   : array_2D(ROB_BUF_SIZE-1 downto 0)(MUL_DIV_BUF_CEIL-1 downto 0);
  signal ROB_ie_issue_ptr_wire        : array_2D(ROB_BUF_SIZE-1 downto 0)(IE_BUF_CEIL-1 downto 0);
  signal ROB_lsu_issue_ptr_wire       : array_2D(ROB_BUF_SIZE-1 downto 0)(LSU_BUF_CEIL-1 downto 0);
  signal ROB_dsp_issue_ptr_wire       : array_2D(ROB_BUF_SIZE-1 downto 0)(DSP_BUF_CEIL-1 downto 0);

  signal ROB_mul_div_rd_valid         : std_logic_vector(ROB_BUF_SIZE-1 downto 0);
  signal ROB_ie_rd_valid              : std_logic_vector(ROB_BUF_SIZE-1 downto 0);
  signal ROB_lsu_rd_valid             : std_logic_vector(ROB_BUF_SIZE-1 downto 0);
  signal ROB_mul_div_addr_new_rd      : array_2D(ROB_BUF_SIZE-1 downto 0)(RF_CEIL-1 downto 0);
  signal ROB_ie_addr_new_rd           : array_2D(ROB_BUF_SIZE-1 downto 0)(RF_CEIL-1 downto 0);
  signal ROB_lsu_addr_new_rd          : array_2D(ROB_BUF_SIZE-1 downto 0)(RF_CEIL-1 downto 0);
  signal ROB_mul_div_data_new_rd      : array_2D(ROB_BUF_SIZE-1 downto 0)(XLEN-1 downto 0);
  signal ROB_ie_data_new_rd           : array_2D(ROB_BUF_SIZE-1 downto 0)(XLEN-1 downto 0);
  signal ROB_lsu_data_new_rd          : array_2D(ROB_BUF_SIZE-1 downto 0)(XLEN-1 downto 0);
  signal ROB_mul_div_rd_valid_wire    : std_logic_vector(ROB_BUF_SIZE-1 downto 0);
  signal ROB_ie_rd_valid_wire         : std_logic_vector(ROB_BUF_SIZE-1 downto 0);
  signal ROB_lsu_rd_valid_wire        : std_logic_vector(ROB_BUF_SIZE-1 downto 0);
  signal ROB_mul_div_addr_new_rd_wire : array_2D(ROB_BUF_SIZE-1 downto 0)(RF_CEIL-1 downto 0);
  signal ROB_ie_addr_new_rd_wire      : array_2D(ROB_BUF_SIZE-1 downto 0)(RF_CEIL-1 downto 0);
  signal ROB_lsu_addr_new_rd_wire     : array_2D(ROB_BUF_SIZE-1 downto 0)(RF_CEIL-1 downto 0);
  signal ROB_mul_div_data_new_rd_wire : array_2D(ROB_BUF_SIZE-1 downto 0)(XLEN-1 downto 0);
  signal ROB_ie_data_new_rd_wire      : array_2D(ROB_BUF_SIZE-1 downto 0)(XLEN-1 downto 0);
  signal ROB_lsu_data_new_rd_wire     : array_2D(ROB_BUF_SIZE-1 downto 0)(XLEN-1 downto 0);

  signal ROB_count_addr_wire          : array_3D(ROB_BUF_SIZE-1 downto 0)(15 downto 0)(RF_CEIL-1 downto 0);
  signal ROB_count_data_wire          : array_3D(ROB_BUF_SIZE-1 downto 0)(15 downto 0)(RF_CEIL-1 downto 0);
  signal ROB_count_addr               : array_3D(ROB_BUF_SIZE-1 downto 0)(15 downto 0)(RF_CEIL-1 downto 0);
  signal ROB_count_data               : array_3D(ROB_BUF_SIZE-1 downto 0)(15 downto 0)(RF_CEIL-1 downto 0);

  signal ROB_commit_ptr               : std_logic_vector(ROB_BUF_CEIL-1 downto 0); -- Indicates the pointer to which the instruction will commit
  signal ROB_offset_ptr               : std_logic_vector(ROB_BUF_CEIL-1 downto 0); -- used to offset from the commit_ptr position
  signal ROB_rename_ptr               : std_logic_vector(ROB_BUF_CEIL-1 downto 0);
  signal ROB_issue_ptr                : std_logic_vector(ROB_BUF_CEIL-1 downto 0);
  signal ROB_rf_ptr                   : std_logic_vector(ROB_BUF_CEIL-1 downto 0);

  signal count_inc         : std_logic;
  signal rf_valid_count    : std_logic_vector(1 downto 0);

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

  instr_tag <= ROB_rename_ptr; -- the tag will be sent to the input instructions of the issue stage, since after that they might issue in OoO

  Recovery_ROB_Comb : process(all)
  begin
    ROB_new_rd_valid_wire        <= ROB_new_rd_valid;
    ROB_new_rd_addr_wire         <= ROB_new_rd_addr;
    ROB_new_rd_wire              <= ROB_new_rd;
    ROB_FRL_rd_wire              <= ROB_FRL_rd;
    ROB_FRL_rd_ptr_wire          <= ROB_FRL_rd_ptr;
    ROB_FRL_commit_ptr_wire      <= ROB_FRL_commit_ptr;
    ROB_mul_div_issue_wire       <= ROB_mul_div_issue;
    ROB_ie_issue_wire            <= ROB_ie_issue;
    ROB_lsu_issue_wire           <= ROB_lsu_issue;
    ROB_dsp_issue_wire           <= ROB_dsp_issue;
    ROB_mul_div_issue_ptr_wire   <= ROB_mul_div_issue_ptr;
    ROB_ie_issue_ptr_wire        <= ROB_ie_issue_ptr;
    ROB_lsu_issue_ptr_wire       <= ROB_lsu_issue_ptr;
    ROB_dsp_issue_ptr_wire       <= ROB_dsp_issue_ptr;
    ROB_mul_div_rd_valid_wire    <= ROB_mul_div_rd_valid;
    ROB_ie_rd_valid_wire         <= ROB_ie_rd_valid;
    ROB_lsu_rd_valid_wire        <= ROB_lsu_rd_valid;
    ROB_mul_div_addr_new_rd_wire <= ROB_mul_div_addr_new_rd;
    ROB_ie_addr_new_rd_wire      <= ROB_ie_addr_new_rd;
    ROB_lsu_addr_new_rd_wire     <= ROB_lsu_addr_new_rd;
    ROB_mul_div_data_new_rd_wire <= ROB_mul_div_data_new_rd;
    ROB_ie_data_new_rd_wire      <= ROB_ie_data_new_rd;
    ROB_lsu_data_new_rd_wire     <= ROB_lsu_data_new_rd;
    ROB_count_addr_wire          <= ROB_count_addr;
    ROB_count_data_wire          <= ROB_count_data;
    if new_rd_valid_rob = '1' then
      ROB_valid_instr(to_integer(unsigned(ROB_rename_ptr)))             <= instr_valid_RENAME;
      ROB_new_rd_valid_wire(to_integer(unsigned(ROB_rename_ptr)))       <= new_rd_valid_rob;
      ROB_new_rd_addr_wire(to_integer(unsigned(ROB_rename_ptr)))        <= new_rd_addr_rob;
      ROB_new_rd_wire(to_integer(unsigned(ROB_rename_ptr)))             <= new_rd_rob;
      ROB_FRL_rd_wire(to_integer(unsigned(ROB_rename_ptr)))             <= FRL_rd_rob;
      ROB_FRL_rd_ptr_wire(to_integer(unsigned(ROB_rename_ptr)))         <= FRL_rd_ptr_rob;
      ROB_FRL_commit_ptr_wire(to_integer(unsigned(ROB_rename_ptr)))     <= FRL_commit_ptr_rob;
    end if;
    if mul_div_issue_rob = '1' then
      ROB_mul_div_issue_wire(to_integer(unsigned(ROB_issue_ptr)))       <= mul_div_issue_rob;
      ROB_mul_div_issue_ptr_wire(to_integer(unsigned(ROB_issue_ptr)))   <= mul_div_issue_ptr_rob;
    end if;
    if ie_issue_rob = '1' then
      ROB_ie_issue_wire(to_integer(unsigned(ROB_issue_ptr)))            <= ie_issue_rob;
      ROB_ie_issue_ptr_wire(to_integer(unsigned(ROB_issue_ptr)))        <= ie_issue_ptr_rob;
    end if;
    if lsu_issue_rob = '1' then
      ROB_lsu_issue_wire(to_integer(unsigned(ROB_issue_ptr)))            <= lsu_issue_rob;
      ROB_lsu_issue_ptr_wire(to_integer(unsigned(ROB_issue_ptr)))        <= lsu_issue_ptr_rob;
    end if;
    if dsp_issue_rob = '1' then
      ROB_dsp_issue_wire(to_integer(unsigned(ROB_issue_ptr)))           <= dsp_issue_rob;
      ROB_dsp_issue_ptr_wire(to_integer(unsigned(ROB_issue_ptr)))       <= dsp_issue_ptr_rob;
    end if;
    if mul_div_rd_valid = '1' then
      ROB_mul_div_rd_valid_wire(to_integer(unsigned(ROB_rf_ptr)))       <= mul_div_rd_valid;
      ROB_mul_div_addr_new_rd_wire(to_integer(unsigned(ROB_rf_ptr)))    <= mul_div_addr_new_rd;
      ROB_mul_div_data_new_rd_wire(to_integer(unsigned(ROB_rf_ptr)))    <= mul_div_data_new_rd;
    end if;
    if ie_rd_valid = '1' then
      ROB_ie_rd_valid_wire(to_integer(unsigned(ROB_rf_ptr)))            <= ie_rd_valid;
      ROB_ie_addr_new_rd_wire(to_integer(unsigned(ROB_rf_ptr)))         <= ie_addr_new_rd;
      ROB_ie_data_new_rd_wire(to_integer(unsigned(ROB_rf_ptr)))         <= ie_data_new_rd;
    end if;
    if lsu_rd_valid = '1' then
      ROB_lsu_rd_valid_wire(to_integer(unsigned(ROB_rf_ptr)))           <= lsu_rd_valid;
      ROB_lsu_addr_new_rd_wire(to_integer(unsigned(ROB_rf_ptr)))        <= lsu_addr_new_rd;
      ROB_lsu_data_new_rd_wire(to_integer(unsigned(ROB_rf_ptr)))        <= lsu_data_new_rd;
    end if;
    if new_rd_valid_rob = '1' or
       mul_div_rd_valid = '1' or
       ie_rd_valid      = '1' or
       lsu_rd_valid     = '1' then
      for i in 0 to 15 loop
        ROB_count_addr_wire(to_integer(unsigned(ROB_rename_ptr)))(i) <= count_addr_rob(i);
        ROB_count_data_wire(to_integer(unsigned(ROB_rename_ptr)))(i) <= count_data_rob(i);
      end loop;
    end if;
  end process;

  Recovery_ROB_Sync : process(clk_i, rst_ni)
    variable recovery_ptr    : integer;
  begin
    if rst_ni = '0' then
      ROB_new_rd_valid        <= (others => '0');
      ROB_new_rd_addr         <= (others => (others => '0'));
      ROB_new_rd              <= (others => (others => '0'));
      ROB_FRL_rd              <= (others => (others => '0'));
      ROB_FRL_rd_ptr          <= (others => (others => '0'));
      ROB_FRL_commit_ptr      <= (others => (others => '0'));
      ROB_mul_div_issue       <= (others => '0');
      ROB_ie_issue            <= (others => '0');
      ROB_lsu_issue           <= (others => '0');
      ROB_dsp_issue           <= (others => '0');
      ROB_mul_div_issue_ptr   <= (others => (others => '0'));
      ROB_ie_issue_ptr        <= (others => (others => '0'));
      ROB_lsu_issue_ptr       <= (others => (others => '0'));
      ROB_dsp_issue_ptr       <= (others => (others => '0'));
      ROB_mul_div_rd_valid    <= (others => '0');
      ROB_ie_rd_valid         <= (others => '0');
      ROB_lsu_rd_valid        <= (others => '0');
      ROB_mul_div_addr_new_rd <= (others => (others => '0'));
      ROB_ie_addr_new_rd      <= (others => (others => '0'));
      ROB_lsu_addr_new_rd     <= (others => (others => '0'));
      ROB_mul_div_data_new_rd <= (others => (others => '0'));
      ROB_ie_data_new_rd      <= (others => (others => '0'));
      ROB_lsu_data_new_rd     <= (others => (others => '0'));
      ROB_count_addr          <= (others => (others => (others => '0')));
      ROB_count_data          <= (others => (others => (others => '0')));
    elsif rising_edge(clk_i) then
      -- reset the recovery signals
      rnm_recovery            <= '0';
      issue_recovery          <= '0';
      regfile_recovery        <= '0';
      CC_recovery             <= '0';
      -- transfer the wire to the regs
      ROB_new_rd_valid        <= ROB_new_rd_valid_wire;
      ROB_new_rd_addr         <= ROB_new_rd_addr_wire;
      ROB_new_rd              <= ROB_new_rd_wire;
      ROB_FRL_rd              <= ROB_FRL_rd_wire;
      ROB_FRL_rd_ptr          <= ROB_FRL_rd_ptr_wire;
      ROB_FRL_commit_ptr      <= ROB_FRL_commit_ptr_wire;
      ROB_mul_div_issue       <= ROB_mul_div_issue_wire;
      ROB_ie_issue            <= ROB_ie_issue_wire;
      ROB_lsu_issue           <= ROB_lsu_issue_wire;
      ROB_dsp_issue           <= ROB_dsp_issue_wire;
      ROB_mul_div_issue_ptr   <= ROB_mul_div_issue_ptr_wire;
      ROB_ie_issue_ptr        <= ROB_ie_issue_ptr_wire;
      ROB_lsu_issue_ptr       <= ROB_lsu_issue_ptr_wire;
      ROB_dsp_issue_ptr       <= ROB_dsp_issue_ptr_wire;
      ROB_mul_div_rd_valid    <= ROB_mul_div_rd_valid_wire;
      ROB_ie_rd_valid         <= ROB_ie_rd_valid_wire;
      ROB_lsu_rd_valid        <= ROB_lsu_rd_valid_wire;
      ROB_mul_div_addr_new_rd <= ROB_mul_div_addr_new_rd_wire;
      ROB_ie_addr_new_rd      <= ROB_ie_addr_new_rd_wire;
      ROB_lsu_addr_new_rd     <= ROB_lsu_addr_new_rd_wire;
      ROB_mul_div_data_new_rd <= ROB_mul_div_data_new_rd_wire;
      ROB_ie_data_new_rd      <= ROB_ie_data_new_rd_wire;
      ROB_lsu_data_new_rd     <= ROB_lsu_data_new_rd_wire;
      ROB_count_addr          <= ROB_count_addr_wire;
      ROB_count_data          <= ROB_count_data_wire;
      if branch_miss = '1' or set_except_condition = '1' then
        if unsigned(ROB_commit_ptr) + unsigned(ROB_offset_ptr) <= ROB_BUF_SIZE-1 then 
          recovery_ptr := to_integer(unsigned(ROB_commit_ptr) + unsigned(ROB_offset_ptr));
          if ROB_valid_instr(recovery_ptr) = '1' then
            if ROB_new_rd_valid(recovery_ptr) = '1' then
              rnm_recovery       <= '1';
              new_rd_addr_rec    <= ROB_new_rd_addr(recovery_ptr);
              new_rd_rec         <= ROB_new_rd(recovery_ptr);
              FRL_rd_rec         <= ROB_FRL_rd(recovery_ptr);
              FRL_rd_ptr_rec     <= ROB_FRL_rd_ptr(recovery_ptr);
              FRL_commit_ptr_rec <= ROB_FRL_commit_ptr(recovery_ptr);
            end if;
            if ROB_mul_div_issue(recovery_ptr) = '1' then
              issue_recovery        <= '1';
              mul_div_issue_rec     <= ROB_mul_div_issue(to_integer(unsigned(ROB_issue_ptr)));
              mul_div_issue_ptr_rec <= ROB_mul_div_issue_ptr(to_integer(unsigned(ROB_issue_ptr)));
            end if;
            if ROB_ie_issue(recovery_ptr)  = '1' then
              issue_recovery        <= '1';
              ie_issue_rec          <= ROB_ie_issue(to_integer(unsigned(ROB_issue_ptr)));
              ie_issue_ptr_rec      <= ROB_ie_issue_ptr(to_integer(unsigned(ROB_issue_ptr)));
            end if;
            if ROB_lsu_issue(recovery_ptr) = '1'then
              issue_recovery        <= '1';
              lsu_issue_rec         <= ROB_lsu_issue(to_integer(unsigned(ROB_issue_ptr)));
              lsu_issue_ptr_rec     <= ROB_lsu_issue_ptr(to_integer(unsigned(ROB_issue_ptr)));
            end if;
            if ROB_dsp_issue(recovery_ptr) = '1'then
              issue_recovery        <= '1';
              dsp_issue_rec         <= ROB_dsp_issue(to_integer(unsigned(ROB_issue_ptr)));
              dsp_issue_ptr_rec     <= ROB_dsp_issue_ptr(to_integer(unsigned(ROB_issue_ptr)));
            end if;
            if ROB_mul_div_rd_valid(recovery_ptr) = '1' then
              regfile_recovery <= '1';
              rf_addr_rec(0) <= ROB_mul_div_addr_new_rd(to_integer(unsigned(ROB_rf_ptr)));
              rf_data_rec(0) <= ROB_mul_div_data_new_rd(to_integer(unsigned(ROB_rf_ptr)));
            end if;
            if ROB_ie_rd_valid(recovery_ptr) = '1' then
              regfile_recovery <= '1';
              rf_addr_rec(1) <= ROB_ie_addr_new_rd(to_integer(unsigned(ROB_rf_ptr)));
              rf_data_rec(1) <= ROB_ie_data_new_rd(to_integer(unsigned(ROB_rf_ptr)));
            end if;
            if ROB_lsu_rd_valid(recovery_ptr) = '1' then
              regfile_recovery <= '1';
              rf_addr_rec(2) <= ROB_lsu_addr_new_rd(to_integer(unsigned(ROB_rf_ptr)));
              rf_data_rec(2) <= ROB_lsu_data_new_rd(to_integer(unsigned(ROB_rf_ptr)));
            end if;
            if ROB_new_rd_valid(recovery_ptr) = '1'     or 
               ROB_mul_div_rd_valid(recovery_ptr) = '1' or
               ROB_ie_rd_valid(recovery_ptr) = '1'      or
               ROB_lsu_rd_valid(recovery_ptr) = '1'     then
              for i in 0 to 15 loop
                CC_recovery       <= '1';
                count_addr_rec(i) <= ROB_count_addr(recovery_ptr)(i);
                count_data_rec(i) <= ROB_count_data(recovery_ptr)(i);
              end loop;   
            end if;
          end if;
      end if;
      end if;
    end if;
  end process;

  rf_valid_count <= std_logic_vector(to_unsigned(add_vect_bits(mul_div_rd_valid & ie_rd_valid & lsu_rd_valid),2));

  ROB_Pointer_Sync : process(clk_i, rst_ni)
  begin
    if rst_ni = '0' then
      ROB_commit_ptr <= (others => '0');
      ROB_rename_ptr <= (others => '0');
      ROB_issue_ptr  <= (others => '0');
      ROB_rf_ptr     <= (others => '0');
      ROB_offset_ptr <= (1 to ROB_BUF_CEIL-1 => '0') & '1';
    elsif rising_edge(clk_i) then
      if new_rd_valid_rob = '1' then
        if unsigned(ROB_rename_ptr) = ROB_BUF_SIZE-1 then
          ROB_rename_ptr <= (others => '0');
        else
          ROB_rename_ptr <= std_logic_vector(unsigned(ROB_rename_ptr)+1);
        end if;
      end if;
    end if;
    if mul_div_issue_rob = '1' or ie_issue_rob = '1' or lsu_issue_rob = '1' or dsp_issue_rob = '1' then
      if unsigned(ROB_issue_ptr) = ROB_BUF_SIZE-1 then
        ROB_issue_ptr <= (others => '0');
      else
        ROB_issue_ptr <= std_logic_vector(unsigned(ROB_issue_ptr)+1);
      end if;
    end if;
    if unsigned(rf_valid_count) > 0 then
      if unsigned(ROB_rf_ptr) + unsigned(rf_valid_count)    = ROB_BUF_SIZE-1 then
        ROB_rf_ptr <= (others => '0');
      elsif unsigned(ROB_rf_ptr) + unsigned(rf_valid_count) = ROB_BUF_SIZE then
        ROB_rf_ptr <= (1 to ROB_BUF_CEIL-1 => '0') & '1';
      elsif unsigned(ROB_rf_ptr) + unsigned(rf_valid_count) = ROB_BUF_SIZE+1 then
        ROB_rf_ptr <= (2 to ROB_BUF_CEIL-1 => '0') & "01";
      else
        ROB_rf_ptr <= std_logic_vector(unsigned(ROB_rf_ptr)+unsigned(rf_valid_count));
      end if;
    end if;
    if count_inc = '1' then
      ROB_offset_ptr <= std_logic_vector(unsigned(ROB_offset_ptr)+1);
    else
      ROB_offset_ptr <= (1 to ROB_BUF_CEIL-1 => '0') & '1';
    end if;
  end process;

end ROB;