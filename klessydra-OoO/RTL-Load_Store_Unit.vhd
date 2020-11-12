--------------------------------------------------------------------------------------------------------------
--  LSU -- (Load-Store Unit)                                                                                --
--  Author(s): Abdallah Cheikh abdallah.cheikh@uniroma1.it (abdallah93.as@gmail.com)                        --
--                                                                                                          --
--  Date Modified: 8-12-2019                                                                                --
--------------------------------------------------------------------------------------------------------------
--  The LSU performs all the operations that access the external memories. Including the amoswap, and the   --
--  custom burst load and store instructions. The LSU can allow superscalar execution with other execution  --
--  units if a store operation is executing, Loaded instructions write either to the regfile or the SPMs    --
--------------------------------------------------------------------------------------------------------------

-- ieee packages ------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_misc.all;
use ieee.numeric_std.all;
use std.textio.all;

-- local packages ------------
use work.riscv_klessydra.all;
--use work.klessydra_parameters.all;

-- LD-STR pinout --------------------
entity Load_Store_Unit is
  generic(
    accl_en                    : natural;
    SIMD                       : natural;
    SPM_NUM		                 : natural;  
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
    decoded_instruction_LSU     : in  std_logic_vector(LS_UNIT_INSTR_SET_SIZE-1 downto 0);
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
    data_err_i                 : in  std_logic;
    -- Recovery Buffer Signals
    branch_miss                : in  std_logic;
    speculative_lsu            : in  std_logic
	);
end entity;  ------------------------------------------

architecture LSU of Load_Store_Unit is

  signal nextstate_LS : fsm_LS_states;
  -- Memory fault signals
  signal data_addr_internal_lat     : std_logic_vector(31 downto 0);
  signal load_err                   : std_logic;
  signal store_err                  : std_logic;
  signal amo_store_lat              : std_logic;
  signal overflow_rs1_sc            : std_logic_vector(Addr_Width downto 0);
  signal overflow_rd_sc             : std_logic_vector(Addr_Width downto 0);
  signal busy_lsu_lat               : std_logic;
  signal sc_word_count              : integer;
  signal harc_LS                    : integer;
  signal ls_rs1_to_sc               : std_logic_vector(SPM_ADDR_WID-1 downto 0);
  signal ls_rd_to_sc                : std_logic_vector(SPM_ADDR_WID-1 downto 0);
  signal ls_sc_data_write           : std_logic_vector(Data_Width-1 downto 0);
  signal data_be_internal           : std_logic_vector(3 downto 0);
  signal lsu_data_rs1_wire_lat       : std_logic_vector(31 downto 0);  -- Wire to used to directly send the result of the address increment
  signal lsu_data_rs2_wire_lat       : std_logic_vector(31 downto 0);  -- Wire to used to directly send the result of the address increment
  signal lsu_data_rd_wire_lat        : std_logic_vector(31 downto 0);  -- Wire to used to directly send the result of the address increment
  signal lsu_data_rs1_lat            : std_logic_vector(31 downto 0);  -- Used to preserve the old data in case we start executing in parallel
  signal lsu_data_rs2_lat            : std_logic_vector(31 downto 0);  -- Used to preserve the old data in case we start executing in parallel
  signal lsu_data_rd_lat             : std_logic_vector(31 downto 0);  -- Used to preserve the old data in case we start executing in parallel

  signal add_op_A                   : std_logic_vector(31 downto 0);
  signal add_op_B                   : std_logic_vector(31 downto 0);
  signal add_out                    : std_logic_vector(31 downto 0);

begin

  -- Memory fault signals
  load_err  <= data_gnt_i and data_err_i and not(data_we_o);
  store_err <= data_gnt_i and data_err_i and data_we_o;

  -- Memory address signal
  data_addr_o <= data_addr_internal(31 downto 2) & "00";
  data_be_o <= to_stdlogicvector(to_bitvector(data_be_internal) sll
                                 to_integer(unsigned(data_addr_internal(1 downto 0))));

------------------------------------------------------------------------
--  ██╗     ███████╗██╗   ██╗    ███████╗██╗   ██╗███╗   ██╗ ██████╗  --
--  ██║     ██╔════╝██║   ██║    ██╔════╝╚██╗ ██╔╝████╗  ██║██╔════╝  --
--  ██║     ███████╗██║   ██║    ███████╗ ╚████╔╝ ██╔██╗ ██║██║       --
--  ██║     ╚════██║██║   ██║    ╚════██║  ╚██╔╝  ██║╚██╗██║██║       --
--  ███████╗███████║╚██████╔╝    ███████║   ██║   ██║ ╚████║╚██████╗  --
--  ╚══════╝╚══════╝ ╚═════╝     ╚══════╝   ╚═╝   ╚═╝  ╚═══╝ ╚═════╝  --
------------------------------------------------------------------------

  LSU_sync : process(clk_i, rst_ni)
  begin
	  
  if rst_ni = '0' then
	  --amo_store  <= '0';
	  amo_store_lat  <= '0';
	  LSU_WB_EN <= '0';
	  busy_lsu_lat <= '0';
	  LSU_WB <= (others => '0');
    misaligned_err <= '0';
    if accl_en = 1 then
      ls_rs1_to_sc             <= (others => '0');
      ls_rd_to_sc              <= (others => '0');
    end if;
	elsif rising_edge(clk_i) then
      LSU_WB_RD_ADDR <= lsu_addr_new_rd;
      --amo_store  <= '0';
      misaligned_err <= '0';
      LSU_WB       <= (others => '0');
      busy_lsu_lat <= busy_lsu;
      if accl_en = 1 then
        lsu_data_rs1_lat <= lsu_data_rs1_wire_lat;
        lsu_data_rs2_lat <= lsu_data_rs2_wire_lat;
        lsu_data_rd_lat  <= lsu_data_rd_wire_lat;
      end if;
      if LSU_instr = '0' and busy_lsu_lat = '0' then
        LSU_WB_EN <= '0';
      elsif branch_miss = '1' and speculative_lsu = '1' then
        LSU_WB_EN <= '0';
      elsif LSU_instr = '1' or busy_lsu_lat = '1' then
        if data_rvalid_i = '1' then
          ls_sc_data_write <= data_rdata_i;
        end if;
        LSU_WB_EN <= '0';
        case state_LS is	
          when normal =>

          --------------------------------------------------------------
          --  ██╗      ██████╗  █████╗ ██████╗      ██████╗ ██████╗   --
          --  ██║     ██╔═══██╗██╔══██╗██╔══██╗    ██╔═══██╗██╔══██╗  --
          --  ██║     ██║   ██║███████║██║  ██║    ██║   ██║██████╔╝  --
          --  ██║     ██║   ██║██╔══██║██║  ██║    ██║   ██║██╔═══╝   --
          --  ███████╗╚██████╔╝██║  ██║██████╔╝    ╚██████╔╝██║       --
          --  ╚══════╝ ╚═════╝ ╚═╝  ╚═╝╚═════╝      ╚═════╝ ╚═╝       --
          --------------------------------------------------------------

            if load_op = '1' then --or (amo_load = '1' and halt_LSU = '0')then  -- Load Instructions
              if ((data_addr_internal(1 downto 0) = "00" and data_width_ID = "10") or 
                  (data_addr_internal(0)          = '0'  and data_width_ID = "01") or
                                                             data_width_ID = "00") then
                if load_err = '1' then
                  ls_except_data <= LOAD_ERROR_EXCEPT_CODE;
                end if;
              else
                ls_except_data <= LOAD_MISALIGNED_EXCEPT_CODE;
                misaligned_err <= '1';
              end if;
            end if;

          -----------------------------------------------------------------------
          --  ███████╗████████╗ ██████╗ ██████╗ ███████╗     ██████╗ ██████╗   --
          --  ██╔════╝╚══██╔══╝██╔═══██╗██╔══██╗██╔════╝    ██╔═══██╗██╔══██╗  --
          --  ███████╗   ██║   ██║   ██║██████╔╝█████╗      ██║   ██║██████╔╝  --
          --  ╚════██║   ██║   ██║   ██║██╔══██╗██╔══╝      ██║   ██║██╔═══╝   --
          --  ███████║   ██║   ╚██████╔╝██║  ██║███████╗    ╚██████╔╝██║       --
          --  ╚══════╝   ╚═╝    ╚═════╝ ╚═╝  ╚═╝╚══════╝     ╚═════╝ ╚═╝       --
          -----------------------------------------------------------------------

            if store_op = '1' then --or amo_store = '1' or (amo_load_skip = '1' and halt_LSU = '0') then	
              if ((data_addr_internal(1 downto 0) = "00" and data_width_ID = "10") or 
                  (data_addr_internal(0)          = '0'  and data_width_ID = "01") or
                                                             data_width_ID = "00") then
                lsu_data_rs2_lat <= lsu_data_rs2;	
                if (store_err = '1') then
                  ls_except_data <= STORE_ERROR_EXCEPT_CODE;
                end if;
              else
                ls_except_data <= STORE_MISALIGNED_EXCEPT_CODE;
                misaligned_err <= '1';
              end if;
            end if;

            -- if amo_store = '1' or amo_load_skip = '1' then
            --   amo_store_lat <= amo_store;
            --   amo_store <= '0';
            -- end if;


          ------------------------------------------------------------------------------------------------------
          --  ██████╗ ██╗   ██╗██████╗ ███████╗████████╗    ██╗     ██████╗     ██╗███████╗████████╗██████╗   --
          --  ██╔══██╗██║   ██║██╔══██╗██╔════╝╚══██╔══╝    ██║     ██╔══██╗   ██╔╝██╔════╝╚══██╔══╝██╔══██╗  --
          --  ██████╔╝██║   ██║██████╔╝███████╗   ██║       ██║     ██║  ██║  ██╔╝ ███████╗   ██║   ██████╔╝  --
          --  ██╔══██╗██║   ██║██╔══██╗╚════██║   ██║       ██║     ██║  ██║ ██╔╝  ╚════██║   ██║   ██╔══██╗  --
          --  ██████╔╝╚██████╔╝██║  ██║███████║   ██║       ███████╗██████╔╝██╔╝   ███████║   ██║   ██║  ██║  --
          --  ╚═════╝  ╚═════╝ ╚═╝  ╚═╝╚══════╝   ╚═╝       ╚══════╝╚═════╝ ╚═╝    ╚══════╝   ╚═╝   ╚═╝  ╚═╝  --
          ------------------------------------------------------------------------------------------------------

            if accl_en = 1 then
              if decoded_instruction_LSU(KMEMLD_bit_position)   = '1' or
                 decoded_instruction_LSU(KBCASTLD_bit_position) = '1' then
                -- Illegal byte transfer handler, and illegal writeback address handler
                if lsu_data_rs2(Addr_Width downto 0) = (0 to Addr_Width => '0') then
                  null;
                elsif rd_to_sc = "100" then --  AAA change "100" to make it parametrizable -- Not a scratchpad destination address
                  ls_except_data              <= ILLEGAL_ADDRESS_EXCEPT_CODE;
                elsif lsu_data_rs1(1 downto 0) /= "00" then
                  ls_except_data              <= LOAD_MISALIGNED_EXCEPT_CODE;
                  misaligned_err              <= '1';
                elsif load_err = '1' then  -- AAA move to data_valid_waiting stage
                  ls_except_data                <= LOAD_ERROR_EXCEPT_CODE;
                elsif overflow_rd_sc(Addr_Width) = '1' then
                  ls_except_data                <= SCRATCHPAD_OVERFLOW_EXCEPT_CODE;
                else
                  lsu_data_rs1_lat <= lsu_data_rs1;
                  lsu_data_rs2_lat <= lsu_data_rs2;
                  lsu_data_rd_lat  <= lsu_data_old_rd;
                  ls_rd_to_sc      <= rd_to_sc;
                end if;
              end if;

              if decoded_instruction_LSU(KMEMSTR_bit_position) = '1' then
                -- Illegal byte transfer handler, and illegal writeback address handler
                if lsu_data_rs2(Addr_Width downto 0) = (0 to Addr_Width => '0') then
                  null;
                elsif rs1_to_sc = "100" then                              --  Not a scratchpad source address
                  ls_except_data              <= ILLEGAL_ADDRESS_EXCEPT_CODE;
                elsif lsu_data_old_rd(1 downto 0) /= "00" then
                  ls_except_data              <= STORE_MISALIGNED_EXCEPT_CODE;
                  misaligned_err              <= '1';  
                elsif store_err = '1' then
                  ls_except_data              <= STORE_ERROR_EXCEPT_CODE;
                elsif overflow_rs1_sc(Addr_Width) = '1' then
                  ls_except_data              <= SCRATCHPAD_OVERFLOW_EXCEPT_CODE;				
                else
                  lsu_data_rs1_lat <= lsu_data_rs1;
                  lsu_data_rs2_lat <= lsu_data_rs2;
                  lsu_data_rd_lat  <= lsu_data_old_rd;
                  ls_rs1_to_sc     <= rs1_to_sc;
                end if;
              end if;
            end if;
			  
        when data_valid_waiting =>

          ----------------------------------------------------------------------------------------------------
          --  ██╗   ██╗ █████╗ ██╗     ██╗██████╗     ██╗    ██╗ █████╗ ██╗████████╗██╗███╗   ██╗ ██████╗   --
          --  ██║   ██║██╔══██╗██║     ██║██╔══██╗    ██║    ██║██╔══██╗██║╚══██╔══╝██║████╗  ██║██╔════╝   --
          --  ██║   ██║███████║██║     ██║██║  ██║    ██║ █╗ ██║███████║██║   ██║   ██║██╔██╗ ██║██║  ███╗  --
          --  ╚██╗ ██╔╝██╔══██║██║     ██║██║  ██║    ██║███╗██║██╔══██║██║   ██║   ██║██║╚██╗██║██║   ██║  --
          --   ╚████╔╝ ██║  ██║███████╗██║██████╔╝    ╚███╔███╔╝██║  ██║██║   ██║   ██║██║ ╚████║╚██████╔╝  --
          --    ╚═══╝  ╚═╝  ╚═╝╚══════╝╚═╝╚═════╝      ╚══╝╚══╝ ╚═╝  ╚═╝╚═╝   ╚═╝   ╚═╝╚═╝  ╚═══╝ ╚═════╝   --
          ----------------------------------------------------------------------------------------------------

          if amo_store_lat = '1' or amo_load_skip = '1' then
            if data_rvalid_i = '1' then
              amo_store_lat <= '0';
            end if;
          end if;	

          if decoded_instruction_LSU(LW_bit_position) = '1'  or (decoded_instruction_LSU(AMOSWAP_bit_position) = '1' and amo_store_lat = '0' and amo_load_skip = '0') then
            if data_rvalid_i = '1' then
              LSU_WB <= data_rdata_i;
              LSU_WB_EN <= '1';
              --if decoded_instruction_LSU(AMOSWAP_bit_position) = '1' then
              --  amo_store <= '1';
              --end if;
            end if;
          end if;

          if decoded_instruction_LSU(LH_bit_position) = '1' or decoded_instruction_LSU(LHU_bit_position) = '1' then 
            if data_rvalid_i = '1' then
              case data_addr_internal(1) is
                when '0' =>
                  LSU_WB_EN <= '1';
                  if decoded_instruction_LSU(LH_bit_position) = '1' then
                    LSU_WB <= std_logic_vector(resize(signed(data_rdata_i(15 downto 0)), 32));
                  elsif decoded_instruction_LSU(LHU_bit_position) = '1' then
                    LSU_WB <= std_logic_vector(resize(unsigned(data_rdata_i(15 downto 0)), 32));
                  end if;
                when '1' =>
                  LSU_WB_EN <= '1';
                  if decoded_instruction_LSU(LH_bit_position) = '1' then
                    LSU_WB <= std_logic_vector(resize(signed(data_rdata_i(31 downto 16)), 32));
                  elsif decoded_instruction_LSU(LHU_bit_position) = '1' then
                    LSU_WB <= std_logic_vector(resize(unsigned(data_rdata_i(31 downto 16)), 32));
                  end if;
                when others =>
                  null;
              end case;
		        end if;
          end if;

          if decoded_instruction_LSU(LB_bit_position) = '1' or decoded_instruction_LSU(LBU_bit_position) = '1' then 
            if data_rvalid_i = '1' then		
              LSU_WB_EN <= '1';
              case data_addr_internal(1 downto 0) is
                when "00" =>
                  if decoded_instruction_LSU(LB_bit_position) = '1' then
                    LSU_WB <= std_logic_vector(resize(signed(data_rdata_i(7 downto 0)), 32));
                  elsif decoded_instruction_LSU(LBU_bit_position) = '1' then
                    LSU_WB <= std_logic_vector(resize(unsigned(data_rdata_i(7 downto 0)), 32));
                  end if;
                when "01" =>
                  if decoded_instruction_LSU(LB_bit_position) = '1' then
                    LSU_WB <= std_logic_vector(resize(signed(data_rdata_i(15 downto 8)), 32));
                  elsif decoded_instruction_LSU(LBU_bit_position) = '1' then
                    LSU_WB <= std_logic_vector(resize(unsigned(data_rdata_i(15 downto 8)), 32));					  
                  end if;
                when "10" =>
                  if decoded_instruction_LSU(LB_bit_position) = '1' then
                    LSU_WB <= std_logic_vector(resize(signed(data_rdata_i(23 downto 16)), 32));
                  elsif decoded_instruction_LSU(LBU_bit_position) = '1' then
                    LSU_WB <= std_logic_vector(resize(unsigned(data_rdata_i(23 downto 16)), 32));
                  end if;
                when "11" =>
                  if decoded_instruction_LSU(LB_bit_position) = '1' then
                    LSU_WB <= std_logic_vector(resize(signed(data_rdata_i(31 downto 24)), 32));
                  elsif decoded_instruction_LSU(LBU_bit_position) = '1' then
                    LSU_WB <= std_logic_vector(resize(unsigned(data_rdata_i(31 downto 24)), 32));
                  end if;
                when others =>
                    null;               
              end case;
            end if;
          end if;
        end case;
      end if;
    end if;
  end process;

  spm_bcast <= '1' when  decoded_instruction_LSU(KBCASTLD_bit_position) = '1' else '0';


-------------------------------------------------------------------------
--  ██╗     ███████╗██╗   ██╗     ██████╗ ██████╗ ███╗   ███╗██████╗   --
--  ██║     ██╔════╝██║   ██║    ██╔════╝██╔═══██╗████╗ ████║██╔══██╗  --
--  ██║     ███████╗██║   ██║    ██║     ██║   ██║██╔████╔██║██████╔╝  --
--  ██║     ╚════██║██║   ██║    ██║     ██║   ██║██║╚██╔╝██║██╔══██╗  --
--  ███████╗███████║╚██████╔╝    ╚██████╗╚██████╔╝██║ ╚═╝ ██║██████╔╝  --
--  ╚══════╝╚══════╝ ╚═════╝      ╚═════╝ ╚═════╝ ╚═╝     ╚═╝╚═════╝   --
-------------------------------------------------------------------------

  LSU_comb : process(all)

  variable data_addr_internal_wires         : std_logic_vector (31 downto 0);
  variable data_wdata_o_wires               : std_logic_vector (31 downto 0);
  variable data_be_internal_wires           : std_logic_vector (3 downto 0);
  variable data_we_o_wires                  : std_logic;
  variable data_req_o_wires                 : std_logic;
  variable ls_except_condition_wires        : std_logic;
  variable ls_taken_branch_wires            : std_logic;
  variable busy_lsu_wires                   : std_logic;

  begin
    data_addr_internal_wires         := std_logic_vector(signed(lsu_data_rs1));  -- The reset value was non-zero in order to keep the switching activity minimal
    nextstate_LS                     <= normal;
    data_be_internal_wires           := (others => '0');
    data_wdata_o_wires               := (others => '0');
    data_we_o_wires                  := '0';
    data_req_o_wires                 := '0';
    ls_except_condition_wires        := '0';
    ls_taken_branch_wires            := '0';
    busy_lsu_wires                   := '0';

    if accl_en = 1 then
      overflow_rs1_sc                  <= (others => '0');
      overflow_rd_sc                   <= (others => '0');
      ls_sc_data_write_wire            <= ls_sc_data_write;
      sc_word_count_wire               <= sc_word_count;
      kmemld_inflight                  <= (others => '0');
      kmemstr_inflight                 <= (others => '0');
      ls_sc_write_addr                 <= (others => '0');
      ls_sc_read_addr                  <= (others => '0');
      --halt_lsu                         <= '0';
      lsu_data_rs1_wire_lat            <= lsu_data_rs1_lat;
      lsu_data_rs2_wire_lat            <= lsu_data_rs2_lat;
      lsu_data_rd_wire_lat             <= lsu_data_rd_lat;
      harc_LS_wire                     <= harc_LS;
      ls_sci_req <= (others => '0');
      ls_sci_we  <= (others => '0');
    end if;

    if branch_miss = '1' and speculative_lsu = '1' then
      null;
    elsif LSU_instr = '1' or busy_lsu_lat = '1' then
      case state_LS is
        when normal =>

          --------------------------------------------------------------
          --  ██╗      ██████╗  █████╗ ██████╗      ██████╗ ██████╗   --
          --  ██║     ██╔═══██╗██╔══██╗██╔══██╗    ██╔═══██╗██╔══██╗  --
          --  ██║     ██║   ██║███████║██║  ██║    ██║   ██║██████╔╝  --
          --  ██║     ██║   ██║██╔══██║██║  ██║    ██║   ██║██╔═══╝   --
          --  ███████╗╚██████╔╝██║  ██║██████╔╝    ╚██████╔╝██║       --
          --  ╚══════╝ ╚═════╝ ╚═╝  ╚═╝╚═════╝      ╚═════╝ ╚═╝       --
          --------------------------------------------------------------

          if load_op = '1' then -- or (amo_load = '1' and halt_LSU = '0') then
            --if amo_load = '0' then
              data_addr_internal_wires := add_out;
            --else
            --  data_addr_internal_wires := lsu_data_rs1;
            --end if;
            if ((data_addr_internal_wires(1 downto 0) = "00" and data_width_ID = "10") or 
                (data_addr_internal_wires(0)          = '0'  and data_width_ID = "01") or
                                                                 data_width_ID = "00") then
              data_be_internal_wires := data_be;
              data_req_o_wires       := '1';
              if load_err = '1' then
                ls_except_condition_wires := '1';
                ls_taken_branch_wires     := '1';
              else
                busy_lsu_wires := '1';
                nextstate_LS <= data_valid_waiting;
              end if;
            else
              ls_except_condition_wires := '1';
              ls_taken_branch_wires     := '1';
            end if;
          end if;

          -----------------------------------------------------------------------
          --  ███████╗████████╗ ██████╗ ██████╗ ███████╗     ██████╗ ██████╗   --
          --  ██╔════╝╚══██╔══╝██╔═══██╗██╔══██╗██╔════╝    ██╔═══██╗██╔══██╗  --
          --  ███████╗   ██║   ██║   ██║██████╔╝█████╗      ██║   ██║██████╔╝  --
          --  ╚════██║   ██║   ██║   ██║██╔══██╗██╔══╝      ██║   ██║██╔═══╝   --
          --  ███████║   ██║   ╚██████╔╝██║  ██║███████╗    ╚██████╔╝██║       --
          --  ╚══════╝   ╚═╝    ╚═════╝ ╚═╝  ╚═╝╚══════╝     ╚═════╝ ╚═╝       --
          -----------------------------------------------------------------------

          if store_op = '1' then --or amo_store = '1' or (amo_load_skip = '1' and halt_LSU = '0') then
            --if amo_store = '0' and amo_load_skip = '0'  then
              data_addr_internal_wires := add_out;
            --elsif amo_store = '1' or amo_load_skip = '1' then
            --  data_addr_internal_wires := lsu_data_rs1;
            --end if;
            data_we_o_wires := '1';
            if ((data_addr_internal_wires(1 downto 0) = "00" and data_width_ID = "10") or 
                (data_addr_internal_wires(0)          = '0'  and data_width_ID = "01") or
                                                                 data_width_ID = "00") then
              data_req_o_wires   := '1';
              data_be_internal_wires := data_be;
              data_wdata_o_wires := lsu_data_rs2;		
              if store_err = '1' then
                ls_except_condition_wires  := '1';
                ls_taken_branch_wires      := '1';
              else
                nextstate_LS <= data_valid_waiting;
                busy_lsu_wires := '1';
              end if;
            else
              ls_except_condition_wires  := '1';
              ls_taken_branch_wires      := '1';
            end if;
            if data_width_ID = "01" then  -- store half word
              case data_addr_internal_wires(1) is
                when '0' =>
                  data_wdata_o_wires := lsu_data_rs2(31 downto 0);
                  data_we_o_wires        := '1';  -- is a writing
                  data_be_internal_wires := data_be;
                when '1' =>
                  data_wdata_o_wires := lsu_data_rs2(15 downto 0) & std_logic_vector(to_unsigned(0, 16));
                  data_we_o_wires        := '1';  -- is a writing
                  data_be_internal_wires := data_be;
                when others =>
                  null;
              end case;
            end if;
            if data_width_ID = "00" then  -- store byte
              case data_addr_internal_wires(1 downto 0) is
                when "00" =>
                  data_wdata_o_wires := lsu_data_rs2(31 downto 0);
                  data_we_o_wires        := '1';  -- is a writing
                  data_be_internal_wires := data_be;
                when "01" =>
                  data_wdata_o_wires := lsu_data_rs2(23 downto 0) & std_logic_vector(to_unsigned(0, 8));
                  data_we_o_wires        := '1';  -- is a writing
                  data_be_internal_wires := data_be;
                when "10" =>
                  data_wdata_o_wires := lsu_data_rs2(15 downto 0) & std_logic_vector(to_unsigned(0, 16));
                  data_we_o_wires        := '1';  -- is a writing
                  data_be_internal_wires := data_be;
                when "11" =>
                  data_wdata_o_wires := lsu_data_rs2(7 downto 0) & std_logic_vector(to_unsigned(0, 24));
                  data_we_o_wires        := '1';  -- is a writing
                  data_be_internal_wires := data_be;
                when others =>
                  null;
              end case;
            end if;
          end if;

          ------------------------------------------------------------------------------------------------------
          --  ██████╗ ██╗   ██╗██████╗ ███████╗████████╗    ██╗     ██████╗     ██╗███████╗████████╗██████╗   --
          --  ██╔══██╗██║   ██║██╔══██╗██╔════╝╚══██╔══╝    ██║     ██╔══██╗   ██╔╝██╔════╝╚══██╔══╝██╔══██╗  --
          --  ██████╔╝██║   ██║██████╔╝███████╗   ██║       ██║     ██║  ██║  ██╔╝ ███████╗   ██║   ██████╔╝  --
          --  ██╔══██╗██║   ██║██╔══██╗╚════██║   ██║       ██║     ██║  ██║ ██╔╝  ╚════██║   ██║   ██╔══██╗  --
          --  ██████╔╝╚██████╔╝██║  ██║███████║   ██║       ███████╗██████╔╝██╔╝   ███████║   ██║   ██║  ██║  --
          --  ╚═════╝  ╚═════╝ ╚═╝  ╚═╝╚══════╝   ╚═╝       ╚══════╝╚═════╝ ╚═╝    ╚══════╝   ╚═╝   ╚═╝  ╚═╝  --
          ------------------------------------------------------------------------------------------------------

            if accl_en = 1 then
              if decoded_instruction_LSU(KMEMLD_bit_position)   = '1' or
                 decoded_instruction_LSU(KBCASTLD_bit_position) = '1' then
                -- lsu_data_rs2(Addr_Width downto 0) instead of lsu_data_rs2(Addr_Width -1 downto 0) in order to allow reading sizes = MAX_SC_SIZE and not MAX_SC_SIZE - 1 
                overflow_rd_sc <= add_out(Addr_Width downto 0); -- If storing data to SC overflows it's address space
                if lsu_data_rs2(Addr_Width downto 0) = (0 to Addr_Width => '0') then
                  null;
                elsif rd_to_sc = "100" then
                  ls_except_condition_wires  := '1';
                  ls_taken_branch_wires      := '1';
                elsif(lsu_data_rs1(1 downto 0) /= "00") then
                  ls_except_condition_wires  := '1';
                  ls_taken_branch_wires      := '1';
                elsif load_err = '1' then
                  ls_except_condition_wires  := '1';
                  ls_taken_branch_wires      := '1';
                elsif overflow_rd_sc(Addr_Width) = '1' then
                  ls_except_condition_wires  := '1';
                  ls_taken_branch_wires      := '1';
                else
                  nextstate_LS    <= data_valid_waiting;
                  busy_lsu_wires  := '1';
                  sc_word_count_wire <= to_integer(unsigned(lsu_data_old_rd(SIMD_BITS+1 downto 1))/2);
                  kmemld_inflight(to_integer(unsigned(rd_to_sc))) <= '1';
                  harc_LS_wire <= 0;
                end if;
              end if;

            if decoded_instruction_LSU(KMEMSTR_bit_position) = '1' then
              -- lsu_data_rs2(Addr_Width downto 0) instead of lsu_data_rs2(Addr_Width -1 downto 0) in order to allow reading sizes = MAX_SC_SIZE and not MAX_SC_SIZE - 1 
              overflow_rs1_sc <= add_out(Addr_Width downto 0); -- If loading data from SC overflows it's address space
              if lsu_data_rs2(Addr_Width downto 0) = (0 to Addr_Width => '0') then
                null;
              elsif rs1_to_sc = "100" then
                ls_except_condition_wires  := '1';
                ls_taken_branch_wires      := '1';
              elsif(lsu_data_old_rd(1 downto 0) /= "00") then
                ls_except_condition_wires  := '1';
                ls_taken_branch_wires      := '1';
              elsif store_err = '1' then
                ls_except_condition_wires  := '1';
                ls_taken_branch_wires      := '1';
              elsif overflow_rs1_sc(Addr_Width) = '1' then
                ls_except_condition_wires  := '1';
                ls_taken_branch_wires      := '1';
              else
                nextstate_LS    <= data_valid_waiting;
                busy_lsu_wires  := '1';
                ls_sci_req(to_integer(unsigned(rs1_to_sc))) <= '1';
                ls_sc_read_addr <= lsu_data_rs1(Addr_Width - 1 downto SIMD_BITS+2);
                sc_word_count_wire <= to_integer(unsigned(lsu_data_rs1(SIMD_BITS+1 downto 1))/2);
                kmemstr_inflight(to_integer(unsigned(rs1_to_sc))) <= '1';
                harc_LS_wire <= 0;
              end if;
            end if;
          end if;

        when data_valid_waiting =>  


          ----------------------------------------------------------------------------------------------------
          --  ██╗   ██╗ █████╗ ██╗     ██╗██████╗     ██╗    ██╗ █████╗ ██╗████████╗██╗███╗   ██╗ ██████╗   --
          --  ██║   ██║██╔══██╗██║     ██║██╔══██╗    ██║    ██║██╔══██╗██║╚══██╔══╝██║████╗  ██║██╔════╝   --
          --  ██║   ██║███████║██║     ██║██║  ██║    ██║ █╗ ██║███████║██║   ██║   ██║██╔██╗ ██║██║  ███╗  --
          --  ╚██╗ ██╔╝██╔══██║██║     ██║██║  ██║    ██║███╗██║██╔══██║██║   ██║   ██║██║╚██╗██║██║   ██║  --
          --   ╚████╔╝ ██║  ██║███████╗██║██████╔╝    ╚███╔███╔╝██║  ██║██║   ██║   ██║██║ ╚████║╚██████╔╝  --
          --    ╚═══╝  ╚═╝  ╚═╝╚══════╝╚═╝╚═════╝      ╚══╝╚══╝ ╚═╝  ╚═╝╚═╝   ╚═╝   ╚═╝╚═╝  ╚═══╝ ╚═════╝   --
          ----------------------------------------------------------------------------------------------------

          data_addr_internal_wires := data_addr_internal_lat;

         -- if ls_sci_wr_gnt = '0' and ls_sci_we /=  (0 to SPM_NUM-1 => '0') then
         --   halt_lsu <= '1';
         -- end if;

          if decoded_instruction_LSU(KMEMLD_bit_position)   = '1' or
             decoded_instruction_LSU(KBCASTLD_bit_position) = '1' then
            if accl_en = 1 then
              if data_rvalid_i = '1' then
                 lsu_data_rs1_wire_lat <= std_logic_vector(unsigned(lsu_data_rs1_lat) + "100");
                 lsu_data_rd_wire_lat  <= std_logic_vector(unsigned(lsu_data_rd_lat)  + "100");
                 if unsigned(lsu_data_rs2_lat(Addr_Width downto 0)) >= 4 then
                   lsu_data_rs2_wire_lat <= std_logic_vector(unsigned(lsu_data_rs2_lat) - "100");
                 else
                   lsu_data_rs2_wire_lat <= (others => '0');
                 end if;
              end if;
              if lsu_data_rs2_lat(Addr_Width downto 0) /= (0 to Addr_Width => '0') then
                busy_lsu_wires             := '1';
                data_be_internal_wires     := "1111";
                data_req_o_wires           := '1';
                data_addr_internal_wires := lsu_data_rs1_wire_lat;
                nextstate_LS <= data_valid_waiting;
                ls_sci_we(to_integer(unsigned(ls_rd_to_sc))) <= '1';
                ls_sc_write_addr <= lsu_data_rd_lat(Addr_Width - 1 downto SIMD_BITS+2);
                kmemld_inflight(to_integer(unsigned(ls_rd_to_sc))) <= '1';
                if data_rvalid_i = '1' then
                  if lsu_data_rs2_lat(Addr_Width downto 0) >= (4 to Addr_Width => '0') & x"4" then
                    ls_sc_data_write_wire <= data_rdata_i;
                  elsif lsu_data_rs2_lat(Addr_Width downto 0) < (4 to Addr_Width => '0') & x"4" then
                    ls_sc_data_write_wire(8*to_integer(unsigned(lsu_data_rs2_lat)) - 1 downto 0) <= data_rdata_i(8*to_integer(unsigned(lsu_data_rs2_lat)) - 1 downto 0);
                  end if;
                end if;
                if data_rvalid_i = '1' then
                  ls_sci_req(to_integer(unsigned(ls_rd_to_sc))) <= '1';
                  if sc_word_count = SIMD-1 then
                    sc_word_count_wire <= 0;
                  else
                    sc_word_count_wire <= sc_word_count + 1;
                  end if;
                end if;
              end if;
            end if;

          elsif decoded_instruction_LSU(KMEMSTR_bit_position) = '1' then
            if accl_en = 1 then
              if data_rvalid_i = '1' then
                 lsu_data_rs1_wire_lat <= std_logic_vector(unsigned(lsu_data_rs1_lat) + "100");
                 lsu_data_rd_wire_lat  <= std_logic_vector(unsigned(lsu_data_rd_lat)  + "100");
                 if unsigned(lsu_data_rs2_lat(Addr_Width downto 0)) >= 4 then
                   lsu_data_rs2_wire_lat <= std_logic_vector(unsigned(lsu_data_rs2_lat) - "100");
                 else
                   lsu_data_rs2_wire_lat <= (others => '0');
                 end if;
              end if;
              if lsu_data_rs2_lat(Addr_Width downto 0) /= (0 to Addr_Width => '0') then
                busy_lsu_wires := '1';
                nextstate_LS <= data_valid_waiting;
                ls_sc_read_addr <= lsu_data_rs1_wire_lat(Addr_Width - 1 downto SIMD_BITS+2);
                kmemstr_inflight(to_integer(unsigned(ls_rs1_to_sc))) <= '1';
                if to_integer(unsigned(ls_data_gnt_i)) /= 0 then
                  data_be_internal_wires     := "1111";
                  data_req_o_wires           := '1';
                  data_we_o_wires            := '1';
                  data_addr_internal_wires := lsu_data_rd_lat;
                  data_wdata_o_wires := ls_sc_data_read_wire;
                end if;
                -- Increments the address of the SC memory every four words for KMEMSTR
                if data_rvalid_i = '1' then
                  ls_sci_req(to_integer(unsigned(ls_rs1_to_sc))) <= '1';
                  if sc_word_count = SIMD-1 then
                    sc_word_count_wire <= 0;
                  else
                    sc_word_count_wire <= sc_word_count + 1;
                  end if;
                end if;
              end if;
            end if;
	  
          elsif data_rvalid_i = '1' then
            if store_op = '1' or amo_store_lat = '1' or amo_load_skip = '1' then -- SW or AMOSWAP data writing
              if decoded_instruction_LSU(SW_bit_position) = '1' then  -- SW data writing
                data_wdata_o_wires     := lsu_data_rs2_lat(31 downto 0);
                data_we_o_wires        := '1';  -- is a writing
                data_be_internal_wires := data_be;
              end if;
              if decoded_instruction_LSU(SH_bit_position) = '1' then  -- SH data writing
                case data_addr_internal_wires(1) is
                  when '0' =>
                    data_wdata_o_wires := lsu_data_rs2_lat(31 downto 0);
                    data_we_o_wires        := '1';  -- is a writing
                    data_be_internal_wires := data_be;
                  when '1' =>
                    data_wdata_o_wires := lsu_data_rs2_lat(15 downto 0) & std_logic_vector(to_unsigned(0, 16));
                    data_we_o_wires        := '1';  -- is a writing
                    data_be_internal_wires := data_be;
                  when others =>
                    null;
                end case;
              end if;
              if decoded_instruction_LSU(SB_bit_position) = '1' then  -- SB data writng
                case data_addr_internal_wires(1 downto 0) is
                  when "00" =>
                    data_wdata_o_wires := lsu_data_rs2_lat(31 downto 0);
                    data_we_o_wires        := '1';  -- is a writing
                    data_be_internal_wires := data_be;
                  when "01" =>
                    data_wdata_o_wires := lsu_data_rs2_lat(23 downto 0) & std_logic_vector(to_unsigned(0, 8));
                    data_we_o_wires        := '1';  -- is a writing
                    data_be_internal_wires := data_be;
                  when "10" =>
                    data_wdata_o_wires := lsu_data_rs2_lat(15 downto 0) & std_logic_vector(to_unsigned(0, 16));
                    data_we_o_wires        := '1';  -- is a writing
                    data_be_internal_wires := data_be;
                  when "11" =>
                    data_wdata_o_wires := lsu_data_rs2_lat(7 downto 0) & std_logic_vector(to_unsigned(0, 24));
                    data_we_o_wires        := '1';  -- is a writing
                    data_be_internal_wires := data_be;
                  when others =>
                    null;
                end case;
              end if;
            end if;
		
            if load_op = '1'  or (decoded_instruction_LSU(AMOSWAP_bit_position) = '1' and amo_store_lat = '0' and amo_load_skip = '0')  then
              data_be_internal_wires := data_be;
              if decoded_instruction_LSU(AMOSWAP_bit_position) = '1' then
                busy_lsu_wires := '1';				  
              end if;
            end if;
          else
            nextstate_LS <= data_valid_waiting;
            busy_lsu_wires := '1';
            -- do not change to "if store_op = '0'" since that will disable store superscalar execution, because store_op resets to 0 on the next cycle
            if decoded_instruction_LSU(SW_bit_position) = '0' and  decoded_instruction_LSU(SH_bit_position) = '0' and decoded_instruction_LSU(SB_bit_position)  = '0' then
              busy_lsu_wires := '1';
            end if;
		  end if;

      end case;
    end if;
	
    data_addr_internal         <= data_addr_internal_wires;
    data_wdata_o               <= data_wdata_o_wires;
    data_be_internal           <= data_be_internal_wires;
    data_we_o                  <= data_we_o_wires;
    data_req_o                 <= data_req_o_wires;
    ls_except_condition        <= ls_except_condition_wires;
    ls_taken_branch   	       <= ls_taken_branch_wires;
    busy_lsu                    <= busy_lsu_wires;
  
  end process;

  fsm_LS_state : process(clk_i, rst_ni) -- also implements some aux signals
  begin
    if rst_ni = '0' then
      state_LS <= normal; 
      if accl_en = 1 then
        sc_word_count <= 0;
	    harc_LS <= 0;
      end if;
    elsif rising_edge(clk_i) then
      state_LS <= nextstate_LS;
	  data_addr_internal_lat   <= data_addr_internal;
      if accl_en = 1 then
	    --halt_lsu_lat             <= halt_lsu;
        sc_word_count            <= sc_word_count_wire;
        harc_LS                  <= harc_LS_wire;
      end if;
    end if;
  end process;

  -----------------------------------------------------------------------------------------------
  --   █████╗ ██████╗ ██████╗ ██████╗ ███████╗███████╗███████╗     ██████╗ ███████╗███╗   ██╗  --
  --  ██╔══██╗██╔══██╗██╔══██╗██╔══██╗██╔════╝██╔════╝██╔════╝    ██╔════╝ ██╔════╝████╗  ██║  --
  --  ███████║██║  ██║██║  ██║██████╔╝█████╗  ███████╗███████╗    ██║  ███╗█████╗  ██╔██╗ ██║  --
  --  ██╔══██║██║  ██║██║  ██║██╔══██╗██╔══╝  ╚════██║╚════██║    ██║   ██║██╔══╝  ██║╚██╗██║  --
  --  ██║  ██║██████╔╝██████╔╝██║  ██║███████╗███████║███████║    ╚██████╔╝███████╗██║ ╚████║  --
  --  ╚═╝  ╚═╝╚═════╝ ╚═════╝ ╚═╝  ╚═╝╚══════╝╚══════╝╚══════╝     ╚═════╝ ╚══════╝╚═╝  ╚═══╝  --
  -----------------------------------------------------------------------------------------------

  LSU_Mapper_comb : process(all)
  begin
    add_op_A <= (others => '0');
    add_op_B <= (others => '0');

    -- Perform the addition ---------------------------------------------------
    add_out <= std_logic_vector(signed(add_op_A) + signed(add_op_B));
    ---------------------------------------------------------------------------

    -- MAP input address generator -----------------------------------------------------------------------------------
    if load_op = '1' then  -- address building operands
      add_op_A <= lsu_data_rs1;
      add_op_B <= I_immediate(instr_word_LSU);
    end if;
    if store_op = '1' then -- address building operands
      add_op_A <= lsu_data_rs1;
      add_op_B <= S_immediate(instr_word_LSU);
    end if;
    if accl_en = 1 then
      if decoded_instruction_LSU(KMEMLD_bit_position)   = '1' or  -- calculates overflow spm write
         decoded_instruction_LSU(KBCASTLD_bit_position) = '1' then
        add_op_A <= (Addr_Width to 31 => '0') & lsu_data_old_rd(Addr_Width -1 downto 0);
        add_op_B <= (Addr_Width to 31 => '0') & std_logic_vector(unsigned(lsu_data_rs2(Addr_Width -1 downto 0))-1);
      end if;
      if decoded_instruction_LSU(KMEMSTR_bit_position) = '1' then -- calculates overflow spm read
        add_op_A <= (Addr_Width to 31 => '0') & lsu_data_rs1(Addr_Width -1 downto 0);
        add_op_B <= (Addr_Width to 31 => '0') & std_logic_vector(unsigned(lsu_data_rs2(Addr_Width -1 downto 0))-1);
      end if;
    end if;
    ---------------------------------------------------------------------------------------------------------------
  end process;

--------------------------------------------------------------------- end of LSU -----------------
--------------------------------------------------------------------------------------------------

end LSU;
--------------------------------------------------------------------------------------------------
-- END of Load-Store architecture ----------------------------------------------------------------
--------------------------------------------------------------------------------------------------
