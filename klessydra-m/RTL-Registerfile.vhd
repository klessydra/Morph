----------------------------------------------------------------------------------------------------------------
--  Stage ID - (Instruction decode and registerfile read)                                                     --
--  Author(s): Abdallah Cheikh abdallah.cheikh@uniroma1.it (abdallah93.as@gmail.com)                          --
--                                                                                                            --
--  Date Modified: 07-04-2020                                                                                 --
----------------------------------------------------------------------------------------------------------------
--  Registerfiles of the incoming hart are read in this stage in parallel with the decoding                   --
--  Two types of registerfiles can be generaated for XILINX FPGAs LUTAM based or FF based dpeneding on the    --
--  setting of the generic variaabble chosen                                                                  --
--  The scratchpad memory mapper also exists in this stage, which maps the address to the corresponding SPM   --
--  This pipeline stage always takes one cycle latency                                                        --
----------------------------------------------------------------------------------------------------------------

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
entity REGISTERFILE is
  generic(
    THREAD_POOL_SIZE           : natural;
    RV32F                      : natural;
    lutram_rf                  : natural;
    latch_rf                   : natural;
    morph_en                   : natural;
    fetch_stage_en             : natural;
    fp_size                    : natural;
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
    clk_i                   : in  std_logic;
    rst_ni                  : in  std_logic;
  -- Branch Control Signals
    harc_ID                 : in  natural range THREAD_POOL_SIZE-1 downto 0;
    pc_ID                   : in  std_logic_vector(31 downto 0);  -- pc_ID is PC entering ID stage
    data_dependency         : in  std_logic;
    bypass_rs1              : in  std_logic;
    bypass_rs2              : in  std_logic;
    bypass_rd_read          : in  std_logic;
    bypass_fp_rs1           : in  std_logic;
    bypass_fp_rs2           : in  std_logic;
    bypass_fp_rd_read       : in  std_logic;
    jalr_stall              : in  std_logic;
    branch_stall            : in  std_logic;
    core_busy_IE            : in  std_logic;
    core_busy_LS            : in  std_logic;
    ls_parallel_exec        : in  std_logic;
    fpu_parallel_exec       : in  std_logic;
    dsp_parallel_exec       : in  std_logic;
    dsp_to_jump_wire        : in  std_logic;
    instr_rvalid_ID         : in  std_logic;
    instr_rvalid_ID_int     : in  std_logic;
    instr_word_ID           : in  std_logic_vector(31 downto 0);
    LS_WB_EN                : in  std_logic;
    IE_WB_EN                : in  std_logic;
    MUL_WB_EN               : in  std_logic;
    FP_LS_WB_EN             : in  std_logic;
    FP_RES_WB_EN            : in  std_logic;
    IE_WB                   : in  std_logic_vector(31 downto 0);
    MUL_WB                  : in  std_logic_vector(31 downto 0);
    LS_WB                   : in  std_logic_vector(31 downto 0);
    FP_LS_WB                : in  std_logic_vector(fp_size-1 downto 0);
    FP_RES_WB               : in  std_logic_vector(fp_size-1 downto 0);
    instr_word_LS_WB        : in  std_logic_vector(31 downto 0);
    instr_word_IE_WB        : in  std_logic_vector(31 downto 0);
    instr_word_FP_RES_WB    : in  std_logic_vector(fp_size-1 downto 0);
    harc_LS_WB              : in  natural range THREAD_POOL_SIZE-1 downto 0;
    harc_IE_WB              : in  natural range THREAD_POOL_SIZE-1 downto 0;
    harc_FP_RES_WB          : in  natural range THREAD_POOL_SIZE-1 downto 0;
    zero_rs1                : out std_logic;
    zero_rs2                : out std_logic;
    pass_BEQ                : out std_logic;
    pass_BNE                : out std_logic;
    pass_BLT                : out std_logic;
    pass_BLTU               : out std_logic;
    pass_BGE                : out std_logic;
    pass_BGEU               : out std_logic;
    RS1_Data_IE             : out std_logic_vector(31 downto 0);
    RS2_Data_IE             : out std_logic_vector(31 downto 0);
    RD_Data_IE              : out std_logic_vector(31 downto 0);
    RS1_Data_FLOAT          : out std_logic_vector(fp_size-1 downto 0);
    RS2_Data_FLOAT          : out std_logic_vector(fp_size-1 downto 0);
    RD_Data_FLOAT           : out std_logic_vector(fp_size-1 downto 0);
    return_address          : out array_2d(THREAD_POOL_SIZE-1 downto 0)(31 downto 0);
    rs1_to_sc               : out std_logic_vector(SPM_ADDR_WID-1 downto 0);
    rs2_to_sc               : out std_logic_vector(SPM_ADDR_WID-1 downto 0);
    rd_to_sc                : out std_logic_vector(SPM_ADDR_WID-1 downto 0);
    data_addr_internal_IE   : out std_logic_vector(31 downto 0);
    regfile                 : out array_3d(THREAD_POOL_SIZE-1 downto 0)(RF_SIZE-1 downto 0)(31 downto 0);
    fp_regfile              : out array_3d(THREAD_POOL_SIZE-1 downto 0)(RF_SIZE-1 downto 0)(fp_size-1 downto 0)
    );
end entity;  ------------------------------------------


-- Klessydra T03x (4 stages) pipeline implementation -----------------------
architecture RF of REGISTERFILE is

  subtype harc_range is natural range THREAD_POOL_SIZE - 1 downto 0;

  signal RF_res            : std_logic_vector(31 downto 0);
  signal FP_RF_res         : std_logic_vector(fp_size-1 downto 0);
  signal WB_EN_lat         : std_logic;
  signal FP_WB_EN_lat      : std_logic;
  signal harc_LAT          : harc_range;
  signal harc_FP_LAT       : harc_range;
  signal instr_word_LAT    : std_logic_vector(31 downto 0);
  signal instr_word_FP_LAT : std_logic_vector(31 downto 0);

  signal regfile_lutram_rs1 : array_2d((THREAD_POOL_SIZE*RF_SIZE)-1 downto 0)(31 downto 0);
  signal regfile_lutram_rs2 : array_2d((THREAD_POOL_SIZE*RF_SIZE)-1 downto 0)(31 downto 0);
  signal regfile_lutram_rd  : array_2d((THREAD_POOL_SIZE*RF_SIZE)-1 downto 0)(31 downto 0);

  attribute ram_style : string;
  attribute ram_style of RS1_Data_IE        : signal is "reg"; -- AAA i think these are unecessary and need to be removed
  attribute ram_style of RS2_Data_IE        : signal is "reg"; -- AAA i think these are unecessary and need to be removed
  attribute ram_style of RD_Data_IE         : signal is "reg"; -- AAA i think these are unecessary and need to be removed
  attribute ram_style of regfile_lutram_rs1 : signal is "distributed";
  attribute ram_style of regfile_lutram_rs2 : signal is "distributed";
  attribute ram_style of regfile_lutram_rd  : signal is "distributed";

  signal ID_rs1_to_sc           : std_logic_vector(SPM_ADDR_WID-1 downto 0);
  signal ID_rs2_to_sc           : std_logic_vector(SPM_ADDR_WID-1 downto 0);
  signal ID_rd_to_sc            : std_logic_vector(SPM_ADDR_WID-1 downto 0);

  signal RS1_Data_IE_wire       : std_logic_vector(31 downto 0);
  signal RS2_Data_IE_wire       : std_logic_vector(31 downto 0);
  signal RD_Data_IE_wire        : std_logic_vector(31 downto 0);

  signal RS1_Data_FLOAT_wire    : std_logic_vector(fp_size-1 downto 0);
  signal RS2_Data_FLOAT_wire    : std_logic_vector(fp_size-1 downto 0);
  signal RD_Data_FLOAT_wire     : std_logic_vector(fp_size-1 downto 0);

  -- instruction operands
  signal RS1_Addr_IE            : std_logic_vector(4 downto 0);   -- debugging signals
  signal RS2_Addr_IE            : std_logic_vector(4 downto 0);   -- debugging signals
  signal RD_Addr_IE             : std_logic_vector(4 downto 0);   -- debugging signals
  signal RD_EN                  : std_logic;
  signal WB_RD                  : std_logic_vector(31 downto 0);
  signal FP_WB_RD               : std_logic_vector(fp_size-1 downto 0);
  signal WB_EN                  : std_logic;
  signal FP_WB_EN               : std_logic;
  signal harc_WB                : harc_range;
  signal instr_word_WB          : std_logic_vector(31 downto 0);

  signal harc_FP_WB             : harc_range;
  signal instr_word_FP_WB       : std_logic_vector(31 downto 0);

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

begin


  ------------------------------------------------------------
  --  ██████╗ ███████╗ ██████╗ ███████╗██╗██╗     ███████╗  --
  --  ██╔══██╗██╔════╝██╔════╝ ██╔════╝██║██║     ██╔════╝  --
  --  ██████╔╝█████╗  ██║  ███╗█████╗  ██║██║     █████╗    --
  --  ██╔══██╗██╔══╝  ██║   ██║██╔══╝  ██║██║     ██╔══╝    --
  --  ██║  ██║███████╗╚██████╔╝██║     ██║███████╗███████╗  --
  --  ╚═╝  ╚═╝╚══════╝ ╚═════╝ ╚═╝     ╚═╝╚══════╝╚══════╝  --
  ------------------------------------------------------------

  RF_FF : if lutram_rf = 0 generate

  LATCH_REGFILE : if latch_rf = 1 generate

  RF_WR_ACCESS : process(all)  -- synch single state process
  begin
      for h in harc_range loop -- hardwire register 'x0' to 0
        regfile(h)(0) <= (others => '0');
      end loop;
      if WB_EN_lat = '1' then
        regfile(harc_LAT)(rd(instr_word_LAT)) <= RF_res;
      end if;
      if RV32F = 1 then
        if FP_WB_EN_lat = '1' then
          fp_regfile(harc_LAT)(rd(instr_word_FP_LAT)) <= FP_RF_res;
        end if;
      end if;
  end process;

  end generate; -- latch_rf = 1


  FF_REGFILE : if latch_rf = 0 generate

  RF_WR_ACCESS : process(clk_i, rst_ni, instr_word_ID)  -- synch single state process
  begin
    if rst_ni = '0' then
      for h in harc_range loop
        if latch_rf = 0 then
          regfile(h)(0) <= (others => '0');
        end if;
      end loop;
    elsif rising_edge(clk_i) then
      if WB_EN = '1' then
        regfile(harc_WB)(rd(instr_word_WB)) <= WB_RD;
      end if;
      if RV32F = 1 then
        if FP_WB_EN = '1' then
          fp_regfile(harc_FP_WB)(rd(instr_word_FP_WB)) <= FP_WB_RD;
        end if;
      end if;
    end if;  -- clk
  end process;

  end generate; -- latch_rf = 0


  RF_RD_ACCESS : process(clk_i, rst_ni, instr_word_ID)  -- synch single state process
  begin
    if rst_ni = '0' then
      WB_EN_lat <= '0';
      harc_LAT <= THREAD_POOL_SIZE-1;
      instr_word_LAT    <= (others => '0');
      instr_word_FP_LAT <= (others => '0');
    elsif rising_edge(clk_i) then
      harc_LAT    <= harc_WB;
      harc_FP_LAT <= harc_FP_WB;
      instr_word_LAT    <= instr_word_WB;
      instr_word_FP_LAT <= instr_word_FP_WB;
      WB_EN_lat <= WB_EN;
      if core_busy_IE = '1' or core_busy_LS = '1' or ls_parallel_exec = '0' or fpu_parallel_exec = '0' or dsp_parallel_exec = '0' then -- the instruction pipeline is halted
      elsif instr_rvalid_ID_int = '0' then -- wait for a valid instruction
      else  -- process the incoming instruction 

        data_addr_internal_IE <= std_logic_vector(signed(regfile(harc_ID)(rs1(instr_word_ID))) + signed(S_immediate(instr_word_ID))) -- normal condition
                                when morph_en = 0 or bypass_rs1 = '0' or harc_ID /= harc_WB else 
                                std_logic_vector(signed(WB_RD) + signed(S_immediate(instr_word_ID))); -- else bypass condition

        ----- REGISTERFILE READ IS DONE HERE --------------------------------------------------------------------------------------------------------------
         RS1_Data_IE <= RS1_Data_IE_wire; 
         RS2_Data_IE <= RS2_Data_IE_wire; 

        if accl_en = 1 then
          if dsp_to_jump_wire = '0' then
            RD_Data_IE <= regfile(harc_ID)(rd(instr_word_ID)); -- only the DSP unit reads the accelerator
          else
            RD_Data_IE <= regfile(harc_ID)(0);
          end if;
        end if;

        if RV32F = 1 then
          RS1_Data_FLOAT <= RS1_Data_FLOAT_wire; 
          RS2_Data_FLOAT <= RS2_Data_FLOAT_wire;
          RD_Data_FLOAT  <= RD_Data_FLOAT_wire;
        end if;

       -- pragma translate_off
        RD_Data_IE    <= regfile(harc_ID)(rd(instr_word_ID)) when  morph_en = 0 or bypass_rd_read = '0' or harc_ID /= harc_WB else WB_RD; -- reading the 'rd' data here is only for debugging purposes if the acclerator is disabled
        RS1_Addr_IE   <= std_logic_vector(to_unsigned(rs1(instr_word_ID), 5)); -- debugging signals
        RS2_Addr_IE   <= std_logic_vector(to_unsigned(rs2(instr_word_ID), 5)); -- debugging signals
        RD_Addr_IE    <= std_logic_vector(to_unsigned(rd(instr_word_ID), 5)); -- debugging signals
       -- pragma translate_on
       ----------------------------------------------------------------------------------------------------------------------------------------------------
      end if;  -- instr. conditions
      if WB_EN = '1' then
        if latch_rf = 1 then
          RF_res <= WB_RD;
        end if;
      end if;
    end if;  -- clk
  end process;

  RS1_Data_IE_wire <= regfile(harc_ID)(rs1(instr_word_ID)) when morph_en = 0 or bypass_rs1 = '0' or harc_ID /= harc_WB else WB_RD;
  RS2_Data_IE_wire <= regfile(harc_ID)(rs2(instr_word_ID)) when morph_en = 0 or bypass_rs2 = '0' or harc_ID /= harc_WB else WB_RD;
  RD_Data_IE_wire  <= regfile(harc_ID)(rd(instr_word_ID))  when morph_en = 0 or bypass_rd_read  = '0' or harc_ID /= harc_WB else WB_RD;

  RS1_Data_FLOAT_wire <= fp_regfile(harc_ID)(rs1(instr_word_ID)) when RV32F = 1 and (morph_en = 0 or bypass_fp_rs1 = '0' or harc_ID /= harc_WB) else FP_WB_RD;
  RS2_Data_FLOAT_wire <= fp_regfile(harc_ID)(rs2(instr_word_ID)) when RV32F = 1 and (morph_en = 0 or bypass_fp_rs2 = '0' or harc_ID /= harc_WB) else FP_WB_RD;
  RD_Data_FLOAT_wire  <= fp_regfile(harc_ID)(rd(instr_word_ID))  when RV32F = 1 and (morph_en = 0 or bypass_fp_rd_read = '0' or harc_ID /= harc_WB) else FP_WB_RD;

  end generate; -- lutram_rf = 0

  RF_LUTRAM : if lutram_rf = 1 generate

  RF_RD_EN : process(all)  -- synch single state process
  begin
    RD_EN <= '0';
    if core_busy_IE = '1' or core_busy_LS = '1' or ls_parallel_exec = '0' or fpu_parallel_exec = '0' or dsp_parallel_exec = '0' or data_dependency = '1' then -- the instruction pipeline is halted
    elsif instr_rvalid_ID_int = '0' then -- wait for a valid instruction
    else  -- process the incoming instruction 
      RD_EN <= '1';
    end if;  -- instr. conditions
    if morph_en = 1 then
      if rs1(instr_word_ID) /= 0 then
        RS1_Data_IE_wire <= regfile_lutram_rs1(32*harc_ID+rs1(instr_word_ID)) when bypass_rs1 = '0' or harc_ID /= harc_WB else WB_RD; 
      else
        RS1_Data_IE_wire <= (others => '0');
      end if;
      if rs2(instr_word_ID) /= 0 then
        RS2_Data_IE_wire <= regfile_lutram_rs2(32*harc_ID+rs2(instr_word_ID)) when bypass_rs2 = '0' or harc_ID /= harc_WB else WB_RD;
      else
        RS2_Data_IE_wire <= (others => '0');
      end if;
      if accl_en = 1 then
        if rd(instr_word_ID) /= 0 then
          RD_Data_IE_wire <= regfile_lutram_rd(32*harc_ID+rd(instr_word_ID)) when bypass_rd_read = '0' or harc_ID /= harc_WB else WB_RD; -- only the DSP unit reads the accelerator
        else
          RD_Data_IE_wire <= (others => '0');
        end if;
      end if;
    elsif morph_en = 0 then
      if rs1(instr_word_ID) /= 0 then
        RS1_Data_IE_wire <= regfile_lutram_rs1(32*harc_ID+rs1(instr_word_ID)); 
      else
        RS1_Data_IE_wire <= (others => '0');
      end if;
      if rs2(instr_word_ID) /= 0 then
        RS2_Data_IE_wire <= regfile_lutram_rs2(32*harc_ID+rs2(instr_word_ID));
      else
        RS2_Data_IE_wire <= (others => '0');
      end if;
      if accl_en = 1 then
        if rd(instr_word_ID) /= 0 then
          RD_Data_IE_wire <= regfile_lutram_rd(32*harc_ID+rd(instr_word_ID));
        else
          RD_Data_IE_wire <= (others => '0');
        end if;
      end if;
    end if;

  end process;

  RS1_ACCESS : process(clk_i)  -- synch single state process
  begin
    if rising_edge(clk_i) then
      if RD_EN = '1' then 
        data_addr_internal_IE <= std_logic_vector(signed(regfile_lutram_rs1(32*harc_ID+rs1(instr_word_ID))) + signed(S_immediate(instr_word_ID)))
                                when morph_en = 0 or bypass_rs1 = '0' or harc_ID /= harc_WB else 
                                std_logic_vector(signed(WB_RD) + signed(S_immediate(instr_word_ID))); -- else bypass condition
        RS1_Data_IE <= RS1_Data_IE_wire;
      -- pragma translate_off
       RS1_Addr_IE <= std_logic_vector(to_unsigned(rs1(instr_word_ID), 5)); -- debugging signals
       RS2_Addr_IE <= std_logic_vector(to_unsigned(rs2(instr_word_ID), 5)); -- debugging signals
       RD_Addr_IE  <= std_logic_vector(to_unsigned(rd(instr_word_ID), 5)); -- debugging signals
      -- pragma translate_on
      end if;  -- instr. conditions
      if WB_EN = '1' then
        regfile_lutram_rs1(32*harc_WB+rd(instr_word_WB)) <= WB_RD;
      end if;
    end if;  -- clk
  end process;

  RS2_ACCESS : process(clk_i)  -- synch single state process
  begin
    if rising_edge(clk_i) then
      if RD_EN = '1' then 
        RS2_Data_IE <= RS2_Data_IE_wire;
      end if;  -- instr. conditions
      if WB_EN = '1' then
        regfile_lutram_rs2(32*harc_WB+rd(instr_word_WB)) <= WB_RD;
      end if;
    end if;  -- clk
  end process;

  RD_LUTRAM : if accl_en = 1 generate
  RD_ACCESS : process(clk_i)  -- synch single state process
  begin
    if rising_edge(clk_i) then
      if accl_en = 1 then
        if RD_EN = '1' then
          RD_Data_IE <= RD_Data_IE_wire;
        end if;  -- instr. conditions
      elsif accl_en = 0 then
        if RD_EN = '1' then
          -- pragma translate_off
           RD_Data_IE  <= regfile_lutram_rd(32*harc_ID+rd(instr_word_ID)) when bypass_rd_read = '0' or harc_ID /= harc_WB else WB_RD; -- reading the 'rd' data here is only for debugging purposes when the acclerator is disabled
          -- pragma translate_on
        end if;  -- instr. conditions
      end if;
      if WB_EN = '1' then
        regfile_lutram_rd(32*harc_WB+rd(instr_word_WB)) <= WB_RD;
      end if;
    end if;  -- clk
  end process;
  end generate; -- accl_en = 1

  end generate; -- lutram_rf = 1

  RETURN_ADDR_REG : if fetch_stage_en = 1 generate
  RA_ACCESS : process(clk_i) begin
    if rising_edge(clk_i) then
      if WB_EN = '1' then
        if rd(instr_word_WB) = 1 then
          return_address(harc_WB) <= WB_RD;
        end if;
      end if; 
    end if;
  end process;
  end generate;

-----------------------------------------------------------------------------------------------------
-- Stage WB - (WRITEBACK)
-----------------------------------------------------------------------------------------------------

  instr_word_WB    <= instr_word_LS_WB when LS_WB_EN = '1' else instr_word_IE_WB;
  harc_WB          <= harc_LS_WB when LS_WB_EN = '1' else harc_IE_WB;
  WB_EN            <= LS_WB_EN or IE_WB_EN or MUL_WB_EN;
  WB_RD            <= LS_WB when LS_WB_EN = '1' else MUL_WB when MUL_WB_EN = '1' else IE_WB;  
  instr_word_FP_WB <= instr_word_LS_WB when FP_LS_WB_EN else instr_word_FP_RES_WB;
  harc_FP_WB       <= harc_LS_WB when FP_LS_WB_EN else harc_FP_RES_WB;
  FP_WB_EN         <= FP_LS_WB_EN or FP_RES_WB_EN;
  FP_WB_RD         <= FP_LS_WB when FP_LS_WB_EN else FP_RES_WB when FP_RES_WB_EN;

--------------------------------------------------------------------- end of WB Stage ----------------
------------------------------------------------------------------------------------------------------

  ------------------------------------------------------------------------------------------------------
  --   ██████╗ ██████╗ ███╗   ███╗██████╗  █████╗ ██████╗  █████╗ ████████╗ ██████╗ ██████╗ ███████╗  --
  --  ██╔════╝██╔═══██╗████╗ ████║██╔══██╗██╔══██╗██╔══██╗██╔══██╗╚══██╔══╝██╔═══██╗██╔══██╗██╔════╝  --
  --  ██║     ██║   ██║██╔████╔██║██████╔╝███████║██████╔╝███████║   ██║   ██║   ██║██████╔╝███████╗  --
  --  ██║     ██║   ██║██║╚██╔╝██║██╔═══╝ ██╔══██║██╔══██╗██╔══██║   ██║   ██║   ██║██╔══██╗╚════██║  --
  --  ╚██████╗╚██████╔╝██║ ╚═╝ ██║██║     ██║  ██║██║  ██║██║  ██║   ██║   ╚██████╔╝██║  ██║███████║  --
  --   ╚═════╝ ╚═════╝ ╚═╝     ╚═╝╚═╝     ╚═╝  ╚═╝╚═╝  ╚═╝╚═╝  ╚═╝   ╚═╝    ╚═════╝ ╚═╝  ╚═╝╚══════╝  --
  ------------------------------------------------------------------------------------------------------

  comparator_enable_comb : process(rst_ni, clk_i)
  begin
    if rst_ni = '0' then
      pass_BEQ  <= '0';
      pass_BNE  <= '0';
      pass_BLT  <= '0';
      pass_BGE  <= '0';
      pass_BLTU <= '0';
      pass_BGEU <= '0';
      zero_rs1  <= '0';
      zero_rs2  <= '0';
    elsif rising_edge(clk_i) then
      if core_busy_IE = '1' or core_busy_LS = '1' or ls_parallel_exec = '0' or fpu_parallel_exec = '0' or dsp_parallel_exec = '0' or data_dependency = '1' then -- the instruction pipeline is halted
      elsif instr_rvalid_ID_int = '0' then -- wait for a valid instruction
      else  -- process the incoming instruction 
        pass_BEQ  <= '0';
        pass_BNE  <= '0';
        pass_BLT  <= '0';
        pass_BGE  <= '0';
        pass_BLTU <= '0';
        pass_BGEU <= '0';
        zero_rs1  <= '0';
        zero_rs2  <= '0';
        if unsigned(RS1_Data_IE_wire) = 0 then
          zero_rs1 <= '1';
        end if;
        if unsigned(RS2_Data_IE_wire) = 0 then
          zero_rs2 <= '1';
        end if;
        if (signed(RS1_Data_IE_wire) = signed(RS2_Data_IE_wire)) then
          pass_BEQ <= '1';
        else
          pass_BNE <= '1';
        end if;
        if (signed(RS1_Data_IE_wire) < signed(RS2_Data_IE_wire)) then
          pass_BLT <= '1';
        else
          pass_BGE <= '1';
        end if;
        if (unsigned(RS1_Data_IE_wire) < unsigned(RS2_Data_IE_wire)) then
          pass_BLTU <= '1';
        else
          pass_BGEU <= '1';
        end if;
      end if;
    end if;
  end process;

------------------------------------------------------------------------------------------
--  ███████╗██████╗ ███╗   ███╗    ███╗   ███╗ █████╗ ██████╗ ██████╗ ███████╗██████╗   --
--  ██╔════╝██╔══██╗████╗ ████║    ████╗ ████║██╔══██╗██╔══██╗██╔══██╗██╔════╝██╔══██╗  --
--  ███████╗██████╔╝██╔████╔██║    ██╔████╔██║███████║██████╔╝██████╔╝█████╗  ██████╔╝  --
--  ╚════██║██╔═══╝ ██║╚██╔╝██║    ██║╚██╔╝██║██╔══██║██╔═══╝ ██╔═══╝ ██╔══╝  ██╔══██╗  --
--  ███████║██║     ██║ ╚═╝ ██║    ██║ ╚═╝ ██║██║  ██║██║     ██║     ███████╗██║  ██║  --
--  ╚══════╝╚═╝     ╚═╝     ╚═╝    ╚═╝     ╚═╝╚═╝  ╚═╝╚═╝     ╚═╝     ╚══════╝╚═╝  ╚═╝  --
------------------------------------------------------------------------------------------

  spm_mapper : if accl_en = 1 generate 
  Spm_Addr_Mapping : process(all)
  begin
  ID_rs1_to_sc <= std_logic_vector(to_unsigned(SPM_NUM, SPM_ADDR_WID));  -- we assign SPM_NUM to rs1_to_sc as a default case which is out of range (0 to SPM_NUM-1)
  ID_rs2_to_sc <= std_logic_vector(to_unsigned(SPM_NUM, SPM_ADDR_WID));  -- we assign SPM_NUM to rs2_to_sc as a default case which is out of range (0 to SPM_NUM-1)
  ID_rd_to_sc  <= std_logic_vector(to_unsigned(SPM_NUM, SPM_ADDR_WID));  -- we assign SPM_NUM to rd_to_sc  as a default case which is out of range (0 to SPM_NUM-1)
    for i in 0 to SPM_NUM-1 loop -- Decode the address and assign and set the scratchpad number (0 to SPM_NUM-1) to the operand
      if RS1_Data_IE_wire(31 downto Addr_Width) >= std_logic_vector(unsigned(SPM_STRT_ADDR(31 downto Addr_Width)) + (i)) and
         RS1_Data_IE_wire(31 downto Addr_Width) <  std_logic_vector(unsigned(SPM_STRT_ADDR(31 downto Addr_Width)) + (i+1)) then
        ID_rs1_to_sc <= std_logic_vector(to_unsigned(i, SPM_ADDR_WID));
      end if;
      if RS2_Data_IE_wire(31 downto Addr_Width) >= std_logic_vector(unsigned(SPM_STRT_ADDR(31 downto Addr_Width)) + (i)) and
         RS2_Data_IE_wire(31 downto Addr_Width) <  std_logic_vector(unsigned(SPM_STRT_ADDR(31 downto Addr_Width)) + (i+1)) then
        ID_rs2_to_sc <= std_logic_vector(to_unsigned(i, SPM_ADDR_WID));
      end if;
      if RD_Data_IE_wire(31 downto Addr_Width)  >= std_logic_vector(unsigned(SPM_STRT_ADDR(31 downto Addr_Width)) + (i)) and
         RD_Data_IE_wire(31 downto Addr_Width)  <  std_logic_vector(unsigned(SPM_STRT_ADDR(31 downto Addr_Width)) + (i+1)) then
        ID_rd_to_sc <= std_logic_vector(to_unsigned(i, SPM_ADDR_WID));
      end if;
    end loop;
  end process;

  Spm_Addr_Mapping_Synch : process(clk_i, rst_ni)
  begin
    if rst_ni = '0' then
    elsif rising_edge(clk_i) then
      rs1_to_sc <= ID_rs1_to_sc;
      rs2_to_sc <= ID_rs2_to_sc;
      rd_to_sc  <= ID_rd_to_sc;
    end if;
  end process;
  
  end generate;

---------------------------------------------------------------------- end of ID stage -----------
--------------------------------------------------------------------------------------------------
end RF;
--------------------------------------------------------------------------------------------------
-- END of ID architecture ------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------