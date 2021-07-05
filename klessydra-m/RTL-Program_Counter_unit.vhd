--------------------------------------------------------------------------------------------------------------
--  PC -- (Program Counters and hart interleavers)                                                          --
--  Author(s): Abdallah Cheikh abdallah.cheikh@uniroma1.it (abdallah93.as@gmail.com)                        --
--                                                                                                          --
--  Date Modified: 17-11-2019                                                                               --
--------------------------------------------------------------------------------------------------------------
--  Program Counter Managing Units -- synchronous process, sinle cycle.                                     --
--  Note: in the present version, gives priority to branching over trapping, except LSU and DSP traps       -- 
--  i.e. branch instructions are not interruptible. This can be changed but may be unsafe.                  --
--  Implements as many PC units as the  number of harts supported                                           --
--  This entity also implements the hardware context counters that interleve the harts in the core.         --
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
    THREAD_POOL_SIZE                  : natural;
    ACCL_NUM                          : natural;
    morph_en                          : natural
  );
  port (
    absolute_jump                     : in  std_logic_vector(THREAD_POOL_SIZE-1 downto 0);
    data_we_o_lat                     : in  std_logic;
    absolute_address                  : in  std_logic_vector(31 downto 0);
    PC_offset                         : in  std_logic_vector(31 downto 0);
    taken_branch                      : in  std_logic;
    ie_taken_branch                   : in  std_logic;
    ls_taken_branch                   : in  std_logic;
    dsp_taken_branch                  : in  std_logic_vector(ACCL_NUM-1 downto 0);
    set_branch_condition              : in  std_logic;
    ie_except_condition               : in  std_logic;
    ls_except_condition               : in  std_logic;
    dsp_except_condition              : in  std_logic_vector(ACCL_NUM-1 downto 0);
    set_except_condition              : in  std_logic;
    set_mret_condition                : in  std_logic;
    set_wfi_condition                 : in  std_logic;
    harc_FETCH                        : in  natural range THREAD_POOL_SIZE-1 downto 0;
    harc_ID                           : in  natural range THREAD_POOL_SIZE-1 downto 0;
    harc_EXEC                         : in  natural range THREAD_POOL_SIZE-1 downto 0;
    instr_rvalid_IE                   : in  std_logic;
    pc_ID                             : in  std_logic_vector(31 downto 0);
    pc_IE                             : in  std_logic_vector(31 downto 0);
    MSTATUS                           : in  array_2d(THREAD_POOL_SIZE-1 downto 0)(1 downto 0);
    MIP, MEPC, MCAUSE, MTVEC          : in  array_2D(THREAD_POOL_SIZE-1 downto 0)(31 downto 0);
    instr_word_IE                     : in  std_logic_vector(31 downto 0);
    pc_IF                             : out std_logic_vector(31 downto 0);
    harc_IF                           : out natural range THREAD_POOL_SIZE-1 downto 0;
    served_ie_except_condition        : out std_logic_vector(THREAD_POOL_SIZE-1 downto 0);
    served_ls_except_condition        : out std_logic_vector(THREAD_POOL_SIZE-1 downto 0);
    served_dsp_except_condition       : out std_logic_vector(THREAD_POOL_SIZE-1 downto 0);
    served_except_condition           : out std_logic_vector(THREAD_POOL_SIZE-1 downto 0);
    served_mret_condition             : out std_logic_vector(THREAD_POOL_SIZE-1 downto 0);
    served_irq                        : in  std_logic_vector(THREAD_POOL_SIZE-1 downto 0);
    taken_branch_pending              : out std_logic_vector(THREAD_POOL_SIZE-1 downto 0);
    incremented_pc                    : out array_2D(THREAD_POOL_SIZE-1 downto 0)(31 downto 0);
    irq_pending                       : out std_logic_vector(THREAD_POOL_SIZE-1 downto 0);
    harc_sleep_wire                   : out std_logic_vector(THREAD_POOL_SIZE-1 downto 0);
    harc_sleep                        : out std_logic_vector(THREAD_POOL_SIZE-1 downto 0);
    halt_update                       : in  std_logic_vector(THREAD_POOL_SIZE-1 downto 0);
    PC_offset_ID                      : in  std_logic_vector(31 downto 0);
    set_branch_condition_ID           : in  std_logic;
    branch_addr_FETCH                 : in  std_logic_vector(31 downto 0);
    jump_addr_FETCH                   : in  std_logic_vector(31 downto 0);
    jalr_addr_FETCH                   : in  std_logic_vector(31 downto 0);
    branch_FETCH                      : in  std_logic;
    jump_FETCH                        : in  std_logic;
    jalr_FETCH                        : in  std_logic;
    clk_i                             : in  std_logic;
    rst_ni                            : in  std_logic;
    irq_i                             : in  std_logic;
    fetch_enable_i                    : in  std_logic;
    boot_addr_i                       : in  std_logic_vector(31 downto 0);
    instr_gnt_i                       : in  std_logic
    );
end entity;


architecture PC of Program_counter is

  subtype harc_range is natural range THREAD_POOL_SIZE-1 downto 0;
  subtype accl_range is integer range ACCL_NUM-1 downto 0;

  -- pc updater signals
  signal pc_update_enable                      : std_logic_vector(harc_range);
  signal taken_branch_replicated               : std_logic_vector(harc_range);
  signal ls_except_condition_replicated        : std_logic_vector(harc_range);
  signal ie_except_condition_replicated        : std_logic_vector(harc_range);
  signal dsp_except_condition_replicated       : std_logic_vector(harc_range);
  signal set_except_condition_replicated       : std_logic_vector(harc_range);
  signal set_trap_condition_replicated         : std_logic_vector(harc_range);
  signal set_mret_condition_replicated         : std_logic_vector(harc_range);
  signal set_branch_condition_ID_replicated    : std_logic_vector(harc_range);
  signal branch_FETCH_replicated               : std_logic_vector(harc_range);
  signal jump_FETCH_replicated                 : std_logic_vector(harc_range);
  signal jalr_FETCH_replicated                 : std_logic_vector(harc_range);
  signal set_branch_condition_replicated       : std_logic_vector(harc_range);
  signal set_wfi_condition_replicated          : std_logic_vector(harc_range);
  signal relative_to_PC                        : array_2D(harc_range)(31 downto 0);
  signal pc                                    : array_2D(harc_range)(31 downto 0);
  signal pc_wire                               : array_2D(harc_range)(31 downto 0);
  signal harc_IF_internal                      : harc_range;
  signal harc_IF_internal_wire                 : harc_range;
  signal mret_condition_pending_internal       : std_logic_vector(harc_range);
  signal incremented_pc_internal               : array_2D(harc_range)(31 downto 0);
  signal mepc_addr_internal                    : array_2D(harc_range)(31 downto 0);
  signal taken_branch_addr_internal            : array_2D(harc_range)(31 downto 0);
  signal taken_branch_pc_pending_internal      : array_2D(harc_range)(31 downto 0);
  signal taken_branch_pending_internal         : std_logic_vector(harc_range);
  signal irq_pending_internal                  : std_logic_vector(harc_range);

  signal taken_branch_pc_pending_internal_lat  : array_2D(harc_range)(31 downto 0);
  signal taken_branch_pending_internal_lat     : std_logic_vector(harc_range);
  signal served_ie_except_condition_lat        : std_logic_vector(harc_range);
  signal served_ls_except_condition_lat        : std_logic_vector(harc_range);
  signal served_dsp_except_condition_lat       : std_logic_vector(harc_range);
  signal served_except_condition_lat           : std_logic_vector(harc_range);
  signal served_mret_condition_lat             : std_logic_vector(harc_range);

  signal count                                 : std_logic_vector(harc_range);
  signal count_wire                            : std_logic_vector(harc_range);
  signal sub                                   : std_logic_vector(harc_range);
  signal sub_wire                              : std_logic_vector(harc_range);
  signal context_switch_halt                   : std_logic_vector(harc_range);
  signal context_switch_halt_wire              : std_logic;
  signal halt_en                               : std_logic_vector(harc_range);

  ------------------------------------------------------------------------------------------------------------
  -- Subroutine implementing pc updating combinational logic, that is replicated for the threads supported  --
  ------------------------------------------------------------------------------------------------------------
  procedure pc_update(
    signal fetch_enable_i                : in    std_logic;
    signal MTVEC                         : in    std_logic_vector(31 downto 0);
    signal instr_gnt_i, taken_branch     : in    std_logic;
    signal set_branch_condition_ID       : in    std_logic;
    signal branch_FETCH                  : in    std_logic;
    signal jump_FETCH                    : in    std_logic;
    signal jalr_FETCH                    : in    std_logic;
    signal set_wfi_condition             : in    std_logic;
    signal taken_branch_pending          : inout std_logic;
    signal taken_branch_pending_lat      : in    std_logic;
    signal irq_pending                   : in    std_logic;
    signal ie_except_condition           : in    std_logic;
    signal ls_except_condition           : in    std_logic;
    signal dsp_except_condition          : in    std_logic;
    signal set_except_condition          : in    std_logic;
    signal set_mret_condition            : in    std_logic;
    signal pc                            : inout std_logic_vector(31 downto 0);
    signal taken_branch_addr             : in    std_logic_vector(31 downto 0);
    signal taken_branch_pc_pending       : inout std_logic_vector(31 downto 0);
    signal taken_branch_pc_pending_lat   : in    std_logic_vector(31 downto 0);
    signal incremented_pc                : in    std_logic_vector(31 downto 0);
    signal pc_update_enable              : in    std_logic;
    signal served_ie_except_condition    : out   std_logic;
    signal served_ls_except_condition    : out   std_logic;
    signal served_dsp_except_condition   : out   std_logic;
    signal served_except_condition       : out   std_logic;
    signal served_mret_condition         : out   std_logic) is
  begin
    if pc_update_enable          = '1' then
      if taken_branch            = '1' or
         set_branch_condition_ID = '1' or
         branch_FETCH            = '1' or
         jump_FETCH              = '1' or 
         jalr_FETCH              = '1' then
        pc                          <= taken_branch_addr;
        taken_branch_pending        <= '0';
        served_ie_except_condition  <= ie_except_condition;
        served_ls_except_condition  <= ls_except_condition;
        served_dsp_except_condition <= dsp_except_condition;
        served_except_condition     <= set_except_condition;
        served_mret_condition       <= set_mret_condition;
      elsif taken_branch_pending_lat = '1' then
        pc                          <= taken_branch_pc_pending_lat;
        taken_branch_pending        <= '0';
        served_ie_except_condition  <= ie_except_condition;
        served_ls_except_condition  <= ls_except_condition;
        served_dsp_except_condition <= dsp_except_condition;
        served_except_condition     <= set_except_condition;
        served_mret_condition       <= set_mret_condition;
      else
        pc                          <= incremented_pc;
        served_except_condition     <= '0';
        served_ie_except_condition  <= '0';
        served_ls_except_condition  <= '0';
        served_dsp_except_condition <= '0';
        served_mret_condition       <= '0';
      end if;
      -- end of pc value update ---    
    else                                -- sets registers to record pending requests
      served_except_condition <= set_except_condition;
      served_mret_condition   <= set_mret_condition and fetch_enable_i;
      if taken_branch            = '1' or 
         set_branch_condition_ID = '1' or 
         branch_FETCH            = '1' or
         jump_FETCH              = '1' or 
         jalr_FETCH              = '1' then
        taken_branch_pending <= '1';
        taken_branch_pc_pending <= taken_branch_addr;
      end if;
      if dsp_except_condition = '1' then
        served_dsp_except_condition <= '1';
      elsif ls_except_condition = '1' then
        served_ls_except_condition <= '1';
      elsif ie_except_condition = '1' then
        served_ie_except_condition <= '1';
      end if;
    end if;
  end pc_update;
  --------------------------------------------------------------------------------------

begin

  harc_IF                  <= harc_IF_internal;
  incremented_pc           <= incremented_pc_internal;
  taken_branch_pending     <= taken_branch_pending_internal;
  irq_pending              <= irq_pending_internal;

  sleep_logic_en : if morph_en = 1 generate

  hardware_context_counter : process(clk_i, rst_ni)
  begin
    if rst_ni = '0' then
      harc_IF_internal    <= THREAD_POOL_SIZE-1;
      harc_sleep          <= (others => '0');
      context_switch_halt <= (others => '0');
      sub                 <= (others => '0');
      count               <= (others => '0');
      halt_en             <= (others => '1');
    elsif rising_edge(clk_i) then
      harc_IF_internal    <= harc_IF_internal_wire;
      harc_sleep          <= harc_sleep_wire;
      sub                 <= sub_wire;
      count               <= count_wire;
      for i in harc_range loop
        if harc_sleep_wire(harc_EXEC) = '1' and halt_en(i) = '1' then
          if harc_EXEC /= i then 
            context_switch_halt(i) <= '1';
            halt_en(i) <= '0';
          end if;
        elsif harc_IF_internal = i and instr_gnt_i = '1'  then
          context_switch_halt(i) <= '0';
        end if;
      end loop;
      if harc_sleep_wire = (harc_range => '0') then
        halt_en <= (others => '1'); -- AAA this maybe needed to be of index (i)
      end if;
    end if;
  end process;

  hardware_context_counter_comb : process(all)
  begin
    harc_sleep_wire <= harc_sleep;
    sub_wire        <= std_logic_vector(unsigned(harc_sleep_wire) - unsigned(harc_sleep));
    count_wire      <= count;
    if harc_sleep /= harc_sleep_wire then
      count_wire <= std_logic_vector(unsigned(count)+1);
    end if;

    if set_wfi_condition = '1' then
      harc_sleep_wire(harc_EXEC) <= '1';
    end if;
    for i in harc_range loop
      if (MIP(i)(11) or MIP(i)(7)) = '1' then
        harc_sleep_wire(0) <= '0';  -- Wake up hart 0 external or timer ints
      end if;
      if MIP(i)(3) = '1' then
        harc_sleep_wire(i) <= '0';  -- Wake up hart i with sw ints
      end if;
    end loop;
    harc_IF_internal_wire <= harc_IF_internal;
    if instr_gnt_i = '1' then
      for i in 1 to THREAD_POOL_SIZE loop -- the range starts from 1 is to avoid adding i+1 below
        if harc_IF_internal-i >= 0 then
          if harc_sleep(harc_IF_internal-i) = '0' then -- checks if the hart is not sleeping 
            harc_IF_internal_wire <= harc_IF_internal-i; -- go to the next non sleeping hart
            exit;
          end if;
        else  -- underflow condition
          if harc_sleep(harc_IF_internal-i+THREAD_POOL_SIZE) = '0' then -- loop back checking th first from the top
            harc_IF_internal_wire <= harc_IF_internal-i+THREAD_POOL_SIZE; -- go to the next non sleeping hart
            exit;
          end if;
        end if; 
      end loop;
    end if;
  end process;

  pc_IF <= pc(harc_IF_internal) when harc_sleep = (harc_range => '0') else pc_wire(harc_IF_internal);

  end generate;

  sleep_logic_dis : if morph_en = 0 generate

    context_switch_halt <= (others => '0');

    hardware_context_counter : process(all)
    begin
      if rst_ni = '0' then
        harc_IF_internal <= THREAD_POOL_SIZE -1;
      elsif rising_edge(clk_i) then
        if instr_gnt_i = '1' then
          harc_IF_internal <= harc_IF_internal - 1 when harc_IF_internal > 0 else THREAD_POOL_SIZE -1;
        end if;
      end if;
    end process hardware_context_counter;

    pc_IF <= pc(harc_IF_internal);

  end generate;

  ----------------------------------------------------------------------------------------------
  -- this part of logic and registers is replicated as many times as the supported threads:   --
  pc_update_logic : for h in harc_range generate

    mepc_addr_internal(h) <= MEPC(h) when MCAUSE(h)(30) = '0' else std_logic_vector(unsigned(MEPC(h)) + 4);  -- MCAUSE(30) = '0' indicates that we weren't executing a WFI instruction

    incremented_pc_internal(h) <= std_logic_vector(unsigned(pc(h))+4);
    irq_pending_internal(h)    <= ((MIP(h)(11) or MIP(h)(7) or MIP(h)(3)) and MSTATUS(h)(0)); -- prevents servicing interrupts during trap routines

    taken_branch_replicated(h) <=         '1' when dsp_taken_branch /= (accl_range => '0') and (harc_EXEC = h)
	                                   else '1' when ls_taken_branch  = '1' and (harc_EXEC = h)
	                                   else '1' when ie_taken_branch  = '1' and (harc_EXEC = h)
                                     else '0';
    dsp_except_condition_replicated(h) <= '1' when dsp_except_condition  /= (accl_range => '0') and (harc_EXEC  = h)
                                     else '0';
    ls_except_condition_replicated(h)  <= '1' when ls_except_condition = '1' and (harc_EXEC = h)
                                     else '0';
    ie_except_condition_replicated(h)  <= '1' when ie_except_condition = '1' and (harc_EXEC = h)
                                     else '0';
    set_except_condition_replicated(h) <= '1' when dsp_except_condition_replicated(h)  = '1' or ls_except_condition_replicated(h) = '1' or ie_except_condition_replicated(h) = '1'
                                     else '0'; -- replicated so that only one hart serves the exception and not more
    -- the abscence of the replicated singals below will create a problem with set_branch_condition_ID_replicated
    set_branch_condition_replicated(h) <= '1' when set_branch_condition = '1' and (harc_EXEC = h)
                                     else '0';
    set_wfi_condition_replicated(h)    <= '1' when set_wfi_condition = '1' and (harc_EXEC = h)
                                     else '0';
    set_mret_condition_replicated(h)   <= '1' when set_mret_condition = '1' and (harc_EXEC = h)
                                     else '0'; -- replicated so that only one hart serves the mret and not more
    set_branch_condition_ID_replicated(h) <= '1' when set_branch_condition_ID = '1' and (harc_ID = h)
                                     else '0';
    branch_FETCH_replicated(h)           <= '1' when branch_FETCH = '1' and (harc_FETCH = h)
                                     else '0';
    jump_FETCH_replicated(h)             <= '1' when jump_FETCH = '1' and (harc_FETCH = h)
                                     else '0';
    jalr_FETCH_replicated(h)             <= '1' when jalr_FETCH = '1' and (harc_FETCH = h)
                                     else '0';
    set_trap_condition_replicated(h) <= set_except_condition_replicated(h) or served_irq(h);

    -- latch on the branch address, possibly useless but may be needed in future situations, served_irq has the highest priority, interrupt request are checked before executing any instructions in the IE_Stage

    taken_branch_addr_internal(h) <=
      absolute_address      when absolute_jump(h)                      = '1' else  -- sets a jump or a branch address
      PC_offset             when set_branch_condition_replicated(h)    = '1' or set_wfi_condition_replicated(h) = '1'  else  -- sets a jump or a branch address
      MTVEC(h)              when set_trap_condition_replicated(h)      = '1' else  -- sets MTVEC address for traps
      mepc_addr_internal(h) when set_mret_condition_replicated(h)      = '1' else  -- sets return address from trap subroutine
      PC_offset_ID          when set_branch_condition_ID_replicated(h) = '1' else
      jalr_addr_FETCH       when jalr_FETCH_replicated(h)              = '1' else
      branch_addr_FETCH     when branch_FETCH_replicated(h)            = '1' else
      jump_addr_FETCH;


    pc_update_enable(h) <= '1' when instr_gnt_i = '1'
                                and ((harc_IF_internal = h and context_switch_halt(h) = '0')
                                or  taken_branch_replicated(h) = '1'
                                or  taken_branch_pending_internal_lat(h) = '1'
                                or  set_branch_condition_ID_replicated(h) = '1'
                                or  branch_FETCH_replicated(h) = '1'
                                or  jump_FETCH_replicated(h) = '1'
                                or  jalr_FETCH_replicated(h) = '1')
                                and halt_update(h) = '0'
                           else '0';


    pc_update_sync : process (clk_i, rst_ni)
    begin
      if rst_ni = '0' then 
        pc(h)                                <= (31 downto 8 => '0' ) & std_logic_vector( to_unsigned(128,8));
        taken_branch_pending_internal_lat(h) <= '0';
        served_ie_except_condition_lat(h)    <= '0';
        served_ls_except_condition_lat(h)    <= '0';
        served_dsp_except_condition_lat(h)   <= '0';
        served_except_condition_lat(h)       <= '0';
        served_mret_condition_lat(h)         <= '0';
      elsif rising_edge(clk_i) then
        pc(h)                                   <= pc_wire(h);
        taken_branch_pc_pending_internal_lat(h) <= taken_branch_pc_pending_internal(h);
        taken_branch_pending_internal_lat(h)    <= taken_branch_pending_internal(h);
        served_ie_except_condition_lat(h)       <= served_ie_except_condition(h);
        served_ls_except_condition_lat(h)       <= served_ls_except_condition(h);
        served_dsp_except_condition_lat(h)      <= served_dsp_except_condition(h);
        served_except_condition_lat(h)          <= served_except_condition(h);
        served_mret_condition_lat(h)            <= served_mret_condition(h);
      end if;
    end process;


    pc_updater_comb : process(all)
    begin
      pc_wire(h)                          <= pc(h);
      taken_branch_pc_pending_internal(h) <= taken_branch_pc_pending_internal_lat(h);
      taken_branch_pending_internal(h)    <= taken_branch_pending_internal_lat(h);
      served_ie_except_condition(h)       <= served_ie_except_condition_lat(h);
      served_ls_except_condition(h)       <= served_ls_except_condition_lat(h);
      served_dsp_except_condition(h)      <= served_dsp_except_condition_lat(h);
      served_except_condition(h)          <= served_except_condition_lat(h);
      served_mret_condition(h)            <= served_mret_condition_lat(h);

      pc_update(
        fetch_enable_i,
        MTVEC(h),
        instr_gnt_i,
        taken_branch_replicated(h),
        set_branch_condition_ID_replicated(h),
        branch_FETCH_replicated(h),
        jump_FETCH_replicated(h),
        jalr_FETCH_replicated(h),
        set_wfi_condition,
        taken_branch_pending_internal(h), 
        taken_branch_pending_internal_lat(h),
        irq_pending_internal(h),
        ie_except_condition_replicated(h),
        ls_except_condition_replicated(h), 
        dsp_except_condition_replicated(h),
        set_except_condition_replicated(h), 
        set_mret_condition_replicated(h), 
        pc_wire(h), 
        taken_branch_addr_internal(h), 
        taken_branch_pc_pending_internal(h),
        taken_branch_pc_pending_internal_lat(h), 
        incremented_pc_internal(h), 
        pc_update_enable(h), 
        served_ie_except_condition(h), 
        served_ls_except_condition(h),
        served_dsp_except_condition(h), 
        served_except_condition(h), 
        served_mret_condition(h)
      );
    end process;

  end generate pc_update_logic;
  -- end of replicated logic --   

--------------------------------------------------------------------- end of PC Managing Units ---
--------------------------------------------------------------------------------------------------  

end PC;
--------------------------------------------------------------------------------------------------
-- END of Program Counter architecture -----------------------------------------------------------
--------------------------------------------------------------------------------------------------