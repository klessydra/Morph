--------------------------------------------------------------------------------------------------------------
--  PC -- (Program Counters and hart interleavers)                                                          --
--  Author(s): Abdallah Cheikh abdallah.cheikh@uniroma1.it (abdallah93.as@gmail.com)                        --
--                                                                                                          --
--  Date Modified: 17-11-2019                                                                               --
--------------------------------------------------------------------------------------------------------------
--  Program Counter Managing Units -- synchronous process, sinle cycle.                                     --
--  Note: in the present version, gives priority to branching over trapping, except LSU and DSP traps       -- 
--  i.e. branch instructions are not interruptible. This can be changed but may be unsafe.                  --
--------------------------------------------------------------------------------------------------------------


-- ieee packages ------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_misc.all;
use ieee.numeric_std.all;
use std.textio.all;

-- local packages ------------
use work.riscv_klessydra.all;

entity Program_Counter is
  generic (
    THREAD_POOL_SIZE      : integer;
    ACCL_NUM              : natural
  );
  port (
    absolute_jump                     : in  std_logic;
    data_we_o_lat                     : in  std_logic;
    PC_offset_ID                      : in  std_logic_vector(31 downto 0);
    PC_offset                         : in  std_logic_vector(31 downto 0);
    id_taken_branch                   : in  std_logic;
    ie_taken_branch                   : in  std_logic;
    ls_taken_branch                   : in  std_logic;
    dsp_taken_branch                  : in  std_logic;
    set_branch_condition              : in  std_logic;
    set_branch_condition_ID           : in  std_logic;
    ie_except_condition               : in  std_logic;
    ls_except_condition               : in  std_logic;
    dsp_except_condition              : in  std_logic;
    set_mret_condition                : in  std_logic;
    set_wfi_condition                 : in  std_logic;
    instr_rvalid_IE                   : in  std_logic;
    pc_ID                             : in  std_logic_vector(31 downto 0);
    pc_IE                             : in  std_logic_vector(31 downto 0);
    MSTATUS                           : in  std_logic_vector(1 downto 0);
    MIP, MEPC, MCAUSE, MTVEC          : in  std_logic_vector(31 downto 0);
    instr_word_IE                     : in  std_logic_vector(31 downto 0);
    pc_IF                             : out std_logic_vector(31 downto 0);
    served_ie_except_condition        : out std_logic;
    served_ls_except_condition        : out std_logic;
    served_dsp_except_condition       : out std_logic;
    served_except_condition           : out std_logic;
    served_mret_condition             : out std_logic;
    served_irq                        : in  std_logic;
    taken_branch_pending              : out std_logic;
    taken_branch_pc_lat               : out std_logic_vector(31 downto 0);
    incremented_pc                    : out std_logic_vector(31 downto 0);
    mepc_incremented_pc               : out std_logic_vector(31 downto 0);
    mepc_interrupt_pc                 : out std_logic_vector(31 downto 0);
    irq_pending                       : out std_logic;
    clk_i                             : in  std_logic;
    rst_ni                            : in  std_logic;
    irq_i                             : in  std_logic;
    fetch_enable_i                    : in  std_logic;
    boot_addr_i                       : in  std_logic_vector(31 downto 0);
    instr_gnt_i                       : in  std_logic
    );
end entity;


architecture PC of Program_counter is

  -- pc updater signals
  signal taken_branch                     : std_logic;
  signal set_except_condition             : std_logic;
  signal pc_update_enable                 : std_logic;
  signal mret_condition_pending_internal  : std_logic;
  signal taken_branch_pending_internal    : std_logic;
  signal irq_pending_internal             : std_logic;
  signal relative_to_PC_ID                : std_logic_vector(31 downto 0);
  signal relative_to_PC                   : std_logic_vector(31 downto 0);
  signal pc_wire                          : std_logic_vector(31 downto 0);
  signal pc                               : std_logic_vector(31 downto 0);
  signal boot_pc                          : std_logic_vector(31 downto 0);
  signal mepc_incremented_pc_internal     : std_logic_vector(31 downto 0);
  signal incremented_pc_internal          : std_logic_vector(31 downto 0);
  signal mepc_interrupt_pc_internal       : std_logic_vector(31 downto 0);
  signal taken_branch_pc_lat_internal     : std_logic_vector(31 downto 0);
  signal taken_branch_pc_pending_internal : std_logic_vector(31 downto 0);

  --------------------------------------------------------------
  -- Subroutine implementing pc updating combinational logic  --
  --------------------------------------------------------------
  procedure pc_update(
    signal MTVEC                         : in    std_logic_vector(31 downto 0);
    signal instr_gnt_i, taken_branch     : in    std_logic;
    signal set_wfi_condition             : in    std_logic;
    signal taken_branch_pending          : inout std_logic;
    signal irq_pending                   : in    std_logic;
    signal ie_except_condition           : in    std_logic;
    signal ls_except_condition           : in    std_logic;
    signal dsp_except_condition          : in    std_logic;
    signal set_except_condition          : in    std_logic;
    signal set_mret_condition            : in    std_logic;
    signal pc_wire                       : inout std_logic_vector(31 downto 0);
    signal taken_branch_pc_lat           : in    std_logic_vector(31 downto 0);
    signal taken_branch_pc_pending       : in    std_logic_vector(31 downto 0);
    signal incremented_pc                : in    std_logic_vector(31 downto 0);
    signal boot_pc                       : in    std_logic_vector(31 downto 0);
    signal pc_update_enable              : in    std_logic;
    signal served_ie_except_condition    : out   std_logic;
    signal served_ls_except_condition    : out   std_logic;
    signal served_dsp_except_condition   : out   std_logic;
    signal served_except_condition       : out   std_logic;
    signal served_mret_condition         : out   std_logic) is
  begin
    if pc_update_enable = '1' then

      -- interrupt service launched in the previous instr. cycle
      -- this is done for a second instr. cycle for proper synchronization of flushing
      -- nothing pending
      if not taken_branch = '1' and not taken_branch_pending = '1' then
        pc_wire                     <= std_logic_vector(unsigned(pc)+4);
        served_except_condition     <= '0';
        served_ie_except_condition  <= '0';
        served_ls_except_condition  <= '0';
        served_dsp_except_condition <= '0';
        served_mret_condition   <= '0';
      -- taken_branch pending 
      elsif taken_branch = '1' then
        pc_wire                     <= taken_branch_pc_lat;
        taken_branch_pending        <= '0';
        served_ie_except_condition  <= '1' when ie_except_condition  = '1' else '0'; -- for CS units;
        served_ls_except_condition  <= '1' when ls_except_condition  = '1' else '0'; -- for CS units;
        served_dsp_except_condition <= '1' when dsp_except_condition = '1' else '0'; -- for CS units;
        served_except_condition     <= '1' when set_except_condition = '1' else '0'; -- for CS units;
        served_mret_condition       <= '1' when set_mret_condition   = '1' else '0'; -- for CS units;
      elsif  taken_branch_pending = '1' then
        pc_wire                     <= taken_branch_pc_pending;
        taken_branch_pending        <= '0';
        served_ie_except_condition  <= '1' when ie_except_condition  = '1' else '0'; -- for CS units;
        served_ls_except_condition  <= '1' when ls_except_condition  = '1' else '0'; -- for CS units;
        served_dsp_except_condition <= '1' when dsp_except_condition = '1' else '0'; -- for CS units;
        served_except_condition     <= '1' when set_except_condition = '1' else '0'; -- for CS units;
        served_mret_condition       <= '1' when set_mret_condition   = '1' else '0'; -- for CS units;
      else
        pc_wire <= boot_pc;                  -- default, should never occur
      end if;
      -- end of pc value update ---    
    else                                -- sets registers to record pending requests
      served_except_condition <= '0';
      served_mret_condition   <= '0';
      if taken_branch = '1' then
        taken_branch_pending <= '1';
      end if;
      if set_except_condition = '1' then
        served_except_condition <= '1';
      end if;
      if dsp_except_condition = '1' then
        served_dsp_except_condition <= '1';
      elsif ls_except_condition = '1' then
        served_ls_except_condition <= '1';
      elsif ie_except_condition = '1' then
        served_ie_except_condition <= '1';
      end if;
      if set_mret_condition = '1' then
        served_mret_condition <= '1';
      end if;
    end if;
  end pc_update;
  --------------------------------------------------------------------------------------

begin

  mepc_incremented_pc      <= mepc_incremented_pc_internal;
  mepc_interrupt_pc        <= mepc_interrupt_pc_internal;
  taken_branch_pc_lat      <= taken_branch_pc_lat_internal;
  incremented_pc           <= incremented_pc_internal;
  taken_branch_pending     <= taken_branch_pending_internal;
  irq_pending              <= irq_pending_internal;


  -- this is the multiplexer on the PC_IF
  pc_IF <= pc_wire;

  -- fixed connections 
  boot_pc <= boot_addr_i(31 downto 8) & std_logic_vector(to_unsigned(128, 8));

  mepc_incremented_pc_internal <= MEPC;
  mepc_interrupt_pc_internal   <= MEPC when MCAUSE(30) = '0' else std_logic_vector(unsigned(MEPC) + 4);  -- MCAUSE(30) = '0' indicates that we weren't executing a WFI instruction

  relative_to_PC_ID <= std_logic_vector(to_unsigned(0, 32)) when (absolute_jump = '1') else pc_ID;
  relative_to_PC    <= std_logic_vector(to_unsigned(0, 32)) when (absolute_jump = '1') else pc_IE;
  irq_pending_internal    <= ((MIP(11) or MIP(7) or MIP(3)) and MSTATUS(0));

  taken_branch <= '1' when dsp_taken_branch = '1'
	           else '1' when ls_taken_branch  = '1'
             else '1' when ie_taken_branch  = '1'
	           else '1' when id_taken_branch  = '1'
             else '0';
  set_except_condition <= '1' when dsp_except_condition  = '1' or ls_except_condition = '1' or ie_except_condition = '1'
             else '0';

    -- latch on the branch address, possibly useless but may be needed in future situations, served_irq has the highest priority, interrupt request are checked before executing any instructions in the IE_Stage

    taken_branch_pc_lat_internal <=
      MTVEC                                                            when dsp_except_condition    = '1'                      else  -- sets MTVEC address for exception trap
      MTVEC                                                            when ls_except_condition     = '1'                      else  -- sets MTVEC address for exception trap
      std_logic_vector(signed(relative_to_PC)+signed(PC_offset))       when set_branch_condition    = '1'                      else  -- sets a jump or a branch address
      std_logic_vector(signed(relative_to_PC_ID)+signed(PC_offset_ID)) when set_branch_condition_ID = '1'                      else  -- sets a jump or a branch address
      std_logic_vector(signed(relative_to_PC))                         when set_wfi_condition       = '1'                      else  -- sets a wfi address (spin lock)
      MTVEC                                                            when ie_except_condition     = '1'                      else  -- sets MTVEC address for exception trap
      mepc_incremented_pc_internal                                     when set_mret_condition      = '1' and MCAUSE(31) = '0' else  -- sets return address from exception subroutine
      mepc_interrupt_pc_internal                                       when set_mret_condition      = '1' and MCAUSE(31) = '1' else  -- sets return address from interrupt subroutine
      MTVEC                                                            when served_irq                                      else  -- sets MTVEC address for exception trap, 
     (others => '0');


    pc_update_enable <= '1' when instr_gnt_i = '1' else '0';

    pc_updater_sync : process(clk_i, rst_ni)
    begin
      if rst_ni = '0' then
        pc                           <= (31 downto 8 => '0') & std_logic_vector(to_unsigned(128, 8));  -- better to put 0 to ensure clear synthesis
      elsif rising_edge(clk_i) then
        pc <= pc_wire;
        --incremented_pc_internal <= std_logic_vector(unsigned(pc)+4); 
      end if;
    end process;

    pc_updater_comb : process(all)
    begin
      pc_wire <= pc;
      taken_branch_pc_pending_internal <= (others => '0');
      taken_branch_pending_internal    <= '0';
      served_ie_except_condition       <= '0';
      served_ls_except_condition       <= '0';
      served_dsp_except_condition      <= '0';
      served_except_condition          <= '0';
      served_mret_condition            <= '0';
      if taken_branch = '1' then 
        taken_branch_pc_pending_internal <= taken_branch_pc_lat_internal;
      end if;
      if fetch_enable_i = '0' then -- AAA this should be fixed as when fetch_enable_i becomes 0 , the pc should continue to update untill a an mret instruction is encountered
      else
        pc_update(
          MTVEC,
          instr_gnt_i,
          taken_branch,
          set_wfi_condition,
          taken_branch_pending_internal,
          irq_pending_internal,
          ie_except_condition, 
          ls_except_condition, 
          dsp_except_condition,
          set_except_condition, 
          set_mret_condition, 
          pc_wire, 
          taken_branch_pc_lat_internal, 
          taken_branch_pc_pending_internal,
          incremented_pc_internal, 
          boot_pc, 
          pc_update_enable, 
          served_ie_except_condition, 
          served_ls_except_condition,
          served_dsp_except_condition, 
          served_except_condition,
          served_mret_condition);
      end if;
    end process;

  -- end --   
--------------------------------------------------------------------- end of PC Managing Units ---
--------------------------------------------------------------------------------------------------  

end PC;
--------------------------------------------------------------------------------------------------
-- END of Program Counter architecture -----------------------------------------------------------
--------------------------------------------------------------------------------------------------