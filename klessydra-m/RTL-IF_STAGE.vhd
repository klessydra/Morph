--------------------------------------------------------------------------------------------------------------
--  stage IF -- (Instruction Fetch)                                                                         --
--  Author(s): Abdallah Cheikh abdallah.cheikh@uniroma1.it (abdallah93.as@gmail.com)                        --
--                                                                                                          --
--  Date Modified: 26-02-2021                                                                               --
--------------------------------------------------------------------------------------------------------------
--  The fetch stage requests an instruction from the program memory, and the instruction arrives in the     --
--  next cycle going directly to the decode stage. The fetch stage does not hold any buffers                --
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

-- pipeline  pinout --------------------
entity IF_STAGE is
  generic(
    THREAD_POOL_SIZE           : natural;
    morph_en                   : natural;
    fetch_stage_en             : natural;
    branch_predict_en          : natural;
    btb_en                     : natural;
    btb_len                    : natural;
    TPS_CEIL                   : natural;
    RF_CEIL                    : natural
    );
  port(
    pc_IF                      : in  std_logic_vector(31 downto 0);
    harc_IF                    : in  natural range THREAD_POOL_SIZE-1 downto 0;
    busy_ID                    : in  std_logic;
    instr_rvalid_i             : in  std_logic;
    harc_sleep_wire            : in  std_logic_vector(THREAD_POOL_SIZE-1 downto 0);
    harc_sleep                 : in  std_logic_vector(THREAD_POOL_SIZE-1 downto 0);
    CORE_STATE                 : in  std_logic_vector(THREAD_POOL_BASELINE downto 0);
    CORE_STATE_FETCH           : out std_logic_vector(THREAD_POOL_BASELINE downto 0);
    served_irq                 : in  std_logic_vector(THREAD_POOL_SIZE-1 downto 0);
    flush_decode               : in  std_logic_vector(THREAD_POOL_SIZE-1 downto 0);
    harc_FETCH                 : out natural range THREAD_POOL_SIZE-1 downto 0;
    harc_ID                    : out natural range THREAD_POOL_SIZE-1 downto 0;
    pc_ID                      : out std_logic_vector(31 downto 0);  -- pc_ID is PC entering ID stage
    instr_rvalid_ID            : out std_logic; 
    instr_word_ID              : out std_logic_vector(31 downto 0);
    instr_word_FETCH           : out std_logic_vector(31 downto 0);
    rs1_valid_ID               : out std_logic;
    rs2_valid_ID               : out std_logic;
    rd_valid_ID                : out std_logic;
    rd_read_valid_ID           : out std_logic;
    instr_rvalid_IE            : in  std_logic;
    pc_IE                      : in  std_logic_vector(31 downto 0);
    return_address             : in  array_2d(THREAD_POOL_SIZE-1 downto 0)(31 downto 0);
    -- branch related signals
    absolute_jump              : in  std_logic_vector(THREAD_POOL_SIZE-1 downto 0);
    branch_instr               : in  std_logic;
    flush_hart_FETCH           : in  std_logic_vector(THREAD_POOL_SIZE-1 downto 0);
    branch_taken               : in  std_logic;
    decoded_branching_instr    : out std_logic_vector(BRANCHING_INSTR_SET_SIZE-1 downto 0);
    branch_predict_taken_ID    : out std_logic;
    branch_FETCH               : out std_logic;
    jump_FETCH                 : out std_logic;
    jalr_FETCH                 : out std_logic;
    halt_update_FETCH          : out std_logic_vector(THREAD_POOL_SIZE-1 downto 0);
    branch_addr_FETCH          : out std_logic_vector(31 downto 0);
    jump_addr_FETCH            : out std_logic_vector(31 downto 0);
    jalr_addr_FETCH            : out std_logic_vector(31 downto 0);
    -- clock, reset active low
    clk_i                      : in  std_logic;
    rst_ni                     : in  std_logic;
    -- program memory interface
    instr_req_o                : out std_logic;
    instr_gnt_i                : in  std_logic;
    instr_rdata_i              : in  std_logic_vector(31 downto 0)
    );
end entity;  ------------------------------------------


-- Klessydra T03x (4 stages) pipeline implementation -----------------------
architecture FETCH of IF_STAGE is

  subtype harc_range is natural range THREAD_POOL_SIZE-1 downto 0;

  -- state signals
  signal instr_word_FETCH_lat    : std_logic_vector(31 downto 0);
  signal instr_word_ID_int       : std_logic_vector(31 downto 0);
  signal instr_word_ID_lat       : std_logic_vector(31 downto 0);
  signal pc_FETCH                : std_logic_vector(31 downto 0);
  signal instr_rvalid_state      : std_logic;
  signal instr_rvalid_FETCH_lat  : std_logic;
  signal instr_rvalid_FETCH      : std_logic;
  signal instr_rvalid_ID_int     : std_logic;
  signal busy_FETCH              : std_logic;

  signal Immediate               : std_logic_vector(31 downto 0); -- AAA change this to variable as well in the ID_stage

  signal halt_update_FETCH_wire  : std_logic_vector(harc_range);

  signal flush_fetch             : std_logic_vector(harc_range);
  signal branch_stall            : std_logic;
  signal branch_stall_lat        : std_logic;
  signal jalr_stall              : std_logic;
  signal jalr_stall_lat          : std_logic;
  signal sys_stall               : std_logic;
  signal sys_stall_lat           : std_logic;

  signal rs1_valid_ID_int        : std_logic;
  signal rs2_valid_ID_int        : std_logic;
  signal rd_valid_ID_int         : std_logic;
  signal rd_read_valid_ID_int    : std_logic;
  signal rs1_valid_ID_lat        : std_logic;
  signal rs2_valid_ID_lat        : std_logic;
  signal rd_valid_ID_lat         : std_logic;
  signal rd_read_valid_ID_lat    : std_logic;

  signal hart_sleep_count        : std_logic_vector(TPS_CEIL-1 downto 0); -- number of sleeping harts

  signal branch_predict_taken_ID_wire : std_logic;


  --constant btb_width             : integer(ceil(log2(real(btb_len))));
  signal btb         : array_2D((2**btb_len)-1 downto 0)(1 downto 0); -- CCC for compressed instructions we should do btb_len-2

  signal btb_addr_rd : natural;

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



----------------------------------------------------------------------------------------------------
------------------------- ARCHITECTURE BEGIN -------------------------------------------------------
begin

----------------------------------------------------------------------------------------------------
-- stage IF -- (instruction fetch)
----------------------------------------------------------------------------------------------------
-- This pipeline stage is implicitly present as the program memory is synchronous
-- with 1 cycle latency.
-- The fsm_IF manages the interface with program memory. 
-- The PC_IF is updated by a dedicated unit which is transparent to the fsm_IF.
----------------------------------------------------------------------------------------------------

  Fetch_stage_comb : if fetch_stage_en = 0 generate

  instr_req_o <= not busy_ID;

  process(clk_i, rst_ni)
  begin
    if rising_edge(clk_i) then
      halt_update_FETCH <= (others => '0');
      if instr_gnt_i = '1' then
        pc_ID   <= pc_IF;
        harc_ID <= harc_IF;
      end if;
      if instr_rvalid_i = '1' then 
        instr_word_ID_lat <= instr_rdata_i;
      end if;
    end if;
  end process;

  branch_FETCH               <= '0';
  jump_FETCH                 <= '0';
  branch_predict_taken_ID    <= '0';
  CORE_STATE_FETCH           <= CORE_STATE;

  instr_rvalid_ID <= instr_rvalid_i;
  instr_word_ID   <= instr_rdata_i when instr_rvalid_i = '1' else instr_word_ID_lat;


  end generate;


  Fetch_stage_sync : if (morph_en = 1 and fetch_stage_en = 1) generate

  instr_req_o <= not busy_FETCH;
  busy_FETCH  <= '1' when busy_ID = '1' or ((jalr_stall = '1' or branch_stall = '1' or sys_stall = '1') and served_irq = (harc_range => '0')) else '0'; -- we should unblock when there is an irq, and even the irq was served by another thread 

  flush_fetch      <= flush_decode;
  rs1_valid_ID     <= rs1_valid_ID_int;      -- operand valid for dependency checking
  rs2_valid_ID     <= rs2_valid_ID_int;      -- operand valid for dependency checking
  rd_valid_ID      <= rd_valid_ID_int;       -- operand valid for dependency checking
  rd_read_valid_ID <= rd_read_valid_ID_int;  -- operand valid for dependency checking

  process(all)
  begin
    instr_word_ID    <= instr_word_ID_lat;
    instr_word_FETCH <= instr_word_FETCH_lat;
    if CORE_STATE(IMT_MODE) = '1' and instr_rvalid_ID_int = '0' then  -- if all harts are asleep and there is no more valid instr pushed from the FETCH stage (i.e. instr_rvalid_ID_int = '0'), we will switch back
      instr_rvalid_ID <= instr_rvalid_i;
      if instr_rvalid_i = '1' then
        instr_word_ID <= instr_rdata_i;
      end if;
    else
      instr_rvalid_ID <= instr_rvalid_ID_int; -- AAA change the name of this signal "instr_rvalid_ID_int" because it already exists in the ID stage
      instr_word_ID   <= instr_word_ID_int;
      if instr_rvalid_i = '1' then
        instr_word_FETCH <= instr_rdata_i;
      end if;
    end if;
    if CORE_STATE(IMT_MODE) = '1' then
      instr_rvalid_FETCH <= instr_rvalid_FETCH_lat;
    else
      instr_rvalid_FETCH <= instr_rvalid_i or instr_rvalid_FETCH_lat;
    end if; 
  end process;

  fetch_stage : process(clk_i, rst_ni)
  begin
    if rst_ni = '0' then
      harc_FETCH             <= THREAD_POOL_SIZE-1;
      hart_sleep_count       <= (others => '0');
      CORE_STATE_FETCH       <= '1' & (0 to THREAD_POOL_BASELINE-1 => '0');
      instr_rvalid_FETCH_lat <= '0';
      instr_rvalid_ID_int    <= '0';
    elsif rising_edge(clk_i) then
      hart_sleep_count <= std_logic_vector(to_unsigned(add_vect_bits(harc_sleep_wire),TPS_CEIL));
      instr_word_ID_lat <= instr_word_ID;
      -- signals that are triggered by instr_rvalid_i read harc_sleep
      if CORE_STATE(IMT_MODE) = '1' and instr_rvalid_FETCH = '0' then
        instr_rvalid_ID_int    <= '0'; -- reset this signal back to zero
        CORE_STATE_FETCH       <= '1' & (0 to THREAD_POOL_BASELINE-1 => '0');
      else
        instr_rvalid_ID_int <= '0';
        if instr_rvalid_FETCH           = '1' and   -- valid instruction
           flush_hart_FETCH(harc_FETCH) = '0' and   -- no branch flush 
           flush_fetch(harc_FETCH)      = '0' and   -- no ID stage flush
           served_irq(harc_FETCH)       = '0' and   -- no IRQ served flush
           busy_ID                      = '0' then  -- no ID stage stall
          instr_rvalid_ID_int    <= '1'; -- AAA change the name of instr_rvalid_ID_int as it conflicts with ano ther intrnal signal in the ID stage
          instr_word_ID_int      <= instr_word_FETCH;
          pc_ID                  <= pc_FETCH;
          harc_ID                <= harc_FETCH;
          instr_rvalid_FETCH_lat <= '0'; -- when we dispatched our instr, that means we don't need to latch
          CORE_STATE_FETCH       <= CORE_STATE;
        elsif busy_ID = '1' then  -- if we have a stall, then we latch the imput
          if instr_rvalid_i = '1' then -- if the pipeline was not pushed to the next stage, and there is the input, then we latch that input
            instr_rvalid_FETCH_lat <= '1';
          end if;
        end if;
      end if;
      -- signals that are triggered by instr_gnt_i read harc_sleep_wire
      if instr_gnt_i = '1' then
        if CORE_STATE(IMT_MODE) = '1' and instr_rvalid_FETCH = '0' then
          pc_ID      <= pc_IF;
          harc_ID    <= harc_IF;
        end if;
        pc_FETCH   <= pc_IF;
        harc_FETCH <= harc_IF;
      end if;
      if instr_rvalid_i = '1' then
        instr_word_FETCH_lat <= instr_rdata_i;
      end if;
    end if;
  end process;

  data_dependency_handler  : process(clk_i, rst_ni)
    variable OPCODE_wires  : std_logic_vector (6 downto 0);
  begin
    if rst_ni = '0' then
      rs1_valid_ID_int               <= '0';
      rs2_valid_ID_int               <= '0';
      rd_valid_ID_int                <= '0';
      rd_read_valid_ID_int           <= '0';
      jalr_stall_lat                 <= '0';
      branch_stall_lat               <= '0';
      sys_stall_lat                  <= '0';
      branch_predict_taken_ID        <= '0';
      halt_update_FETCH              <= (others => '0');
      decoded_branching_instr        <= (others => '0');  
    elsif rising_edge(clk_i) then
      halt_update_FETCH       <= halt_update_FETCH_wire;
      branch_predict_taken_ID <= branch_predict_taken_ID_wire;
      jalr_stall_lat          <= jalr_stall;
      branch_stall_lat        <= branch_stall;
      sys_stall_lat           <= sys_stall;
      decoded_branching_instr <= (others => '0');
      OPCODE_wires          := OPCODE(instr_word_FETCH);
      if CORE_STATE(IMT_MODE) = '1' and instr_rvalid_FETCH = '0' then
        rs1_valid_ID_int      <= '0';
        rs2_valid_ID_int      <= '0';
        rd_valid_ID_int       <= '0';
        rd_read_valid_ID_int  <= '0';
      end if;
      if instr_rvalid_FETCH           = '1' and
         flush_hart_FETCH(harc_FETCH) = '0' and 
         flush_fetch(harc_FETCH)      = '0' and
         served_irq(harc_FETCH)       = '0' and
         busy_ID                      = '0' then

        rs1_valid_ID_int      <= '0';
        rs2_valid_ID_int      <= '0';
        rd_valid_ID_int       <= '0';
        rd_read_valid_ID_int  <= '0'; 

        case OPCODE_wires is

          when OP_IMM =>      -- OP_IMM instruction
            rs1_valid_ID_int      <= '1';
            rd_valid_ID_int       <= '1';

          when LUI =>         -- LUI instruction
            rd_valid_ID_int       <= '1';


          when AUIPC =>       -- AUIPC instruction
            rd_valid_ID_int       <= '1';

          when OP =>          -- OP instruction
            rs1_valid_ID_int      <= '1';
            rs2_valid_ID_int      <= '1';
            rd_valid_ID_int       <= '1';

          when JAL =>        -- JAL instruction
            decoded_branching_instr <= JAL_FETCH_pattern;
            rd_valid_ID_int       <= '1';

          when JALR =>       -- JALR instruction
            decoded_branching_instr <= JALR_FETCH_pattern;
            rs1_valid_ID_int      <= '1';
            rd_valid_ID_int       <= '1';

          when BRANCH =>     -- BRANCH instruction
            decoded_branching_instr <= BRANCH_FETCH_pattern;
            rs1_valid_ID_int      <= '1';
            rs2_valid_ID_int      <= '1';

          when LOAD =>
            rs1_valid_ID_int      <= '1';
            rd_valid_ID_int       <= '1';

          when STORE =>
            rs1_valid_ID_int      <= '1';
            rs2_valid_ID_int      <= '1';

          when SYSTEM =>
            rs1_valid_ID_int      <= '1';
            rd_valid_ID_int       <= '1';

          when AMO =>
            rs1_valid_ID_int      <= '1';
            rs2_valid_ID_int      <= '1';
            rd_valid_ID_int       <= '1';

          when KMEM =>
            rs1_valid_ID_int      <= '1';
            rs2_valid_ID_int      <= '1'; -- even when not valid rs2 is equal to "00000" which is register x0 that is always valid
            rd_read_valid_ID_int  <= '1';

          when KDSP =>
            rs1_valid_ID_int      <= '1';
            rs2_valid_ID_int      <= '1';
            rd_read_valid_ID_int  <= '1';
    
          when others =>                -- ILLEGAL_INSTRUCTION
            null;

        end case;  -- OPCODE_wires cases                           
      end if;
    end if; -- clk_i
  end process;

  branch_handler_comb : process(all)  -- comb process
    variable OPCODE_wires  : std_logic_vector(6 downto 0);
    variable FUNCT3_wires  : std_logic_vector(2 downto 0);
    variable FUNCT12_wires : std_logic_vector(11 downto 0);

  begin
    OPCODE_wires                 := OPCODE(instr_word_FETCH);
    FUNCT3_wires                 := FUNCT3(instr_word_FETCH);
    FUNCT12_wires                := FUNCT12(instr_word_FETCH);
    Immediate                    <= B_immediate(instr_word_FETCH); -- defualts to B_Immediate unless we have a JAL
    jalr_addr_FETCH              <= return_address(harc_FETCH);  -- AAA componesate for return instructions that use an offset (although they should never be gerated by a gcc compiler)
    branch_addr_FETCH            <= std_logic_vector(unsigned(pc_FETCH)+unsigned(B_immediate(instr_word_FETCH)));
    jump_addr_FETCH              <= std_logic_vector(unsigned(pc_FETCH)+unsigned(UJ_immediate(instr_word_FETCH)));
    jalr_FETCH                   <= '0';
    jump_FETCH                   <= '0';
    branch_FETCH                 <= '0';
    branch_predict_taken_ID_wire <= '0';
    branch_stall                 <= branch_stall_lat;
    jalr_stall                   <= jalr_stall_lat;
    sys_stall                    <= sys_stall_lat;
    halt_update_FETCH_wire       <= halt_update_FETCH and not instr_gnt_i; -- latch the halt wire as long as we don't have a valid instr

    if branch_instr = '1' or served_irq /= (harc_range => '0') then
      branch_stall <= '0';
    end if;
    if absolute_jump(harc_FETCH) = '1' or served_irq /= (harc_range => '0') then
       jalr_stall <= '0';
    end if;
    if instr_rvalid_FETCH           = '1' and 
       --flush_hart_FETCH(harc_FETCH) = '0' and
       flush_fetch(harc_FETCH)      = '0' and
       --served_irq(harc_FETCH)                = '0' and
       --busy_ID                               = '0' and
       CORE_STATE(IMT_MODE) = '0' then
      case OPCODE_wires is

        when JAL =>         -- JAL instruction
          if CORE_STATE(SINGLE_HART_MODE) = '1' then -- when hart_sleep_count = 1, JAL will be handed in the decode stage
            jump_FETCH <= '1';
          end if;

        when JALR =>        -- JALR instruction
          jalr_FETCH <= '1';
          if CORE_STATE_FETCH(DUAL_HART_MODE) = '1' then
            halt_update_FETCH_wire(harc_FETCH) <= '1';
          end if;
          --jalr_stall <= '1';

        when BRANCH =>      -- BRANCH instruction
          if branch_predict_en = 0 then
            if CORE_STATE_FETCH(DUAL_HART_MODE) = '1' then
              halt_update_FETCH_wire(harc_FETCH) <= '1';
            end if;
            if branch_instr = '0' then
              branch_stall <= '1';
            end if;
          elsif btb_en = 1 then
            if btb(btb_addr_rd) > "01" then
              if CORE_STATE_FETCH(DUAL_HART_MODE) = '1' then
                halt_update_FETCH_wire(harc_FETCH) <= '1';
              end if;
              branch_FETCH <= '1';
              branch_predict_taken_ID_wire <= '1';
            end if;
          elsif btb_en = 0 then
            if CORE_STATE(SINGLE_HART_MODE) = '1' then
              if instr_word_FETCH(31) = '1' then -- branch prediction taken for negative offsets
                branch_FETCH <= '1';
                branch_predict_taken_ID_wire <= '1';
              end if;
            end if;
          end if;

        when others =>
          null;

      end case;  -- OPCODE_wires cases                           
    end if;
  end process;

  branch_target_buffer : if btb_en = 1 generate

  branch_target_buffer : process(clk_i, rst_ni)  -- comb process
    variable OPCODE_wires  : std_logic_vector(6 downto 0);
    variable btb_addr_wr   : natural;  -- CCC check in synthesis if changing these to std_logic_vector will result in a smaller or bigger layout
  begin
    if rst_ni = '0' then
      btb <= (others => "10"); -- the btb is intialized with "10" meaning that the branch is likely to be taken
    elsif rising_edge(clk_i) then
      if instr_gnt_i = '1' then
        btb_addr_rd <= to_integer(unsigned(pc_IF(btb_len+1 downto 2))); -- CCC compressed instructions should go down to 1 instead
      end if;
      -- AAA the btb_addr_wr might increment or decrement twice in case of a branch miss (i.e. were the branch stays in the IE for more than one cycle)
      btb_addr_wr := to_integer(unsigned(pc_IE(btb_len+1 downto 2))); -- CCC compressed instructions should go down to 1 instead
      if CORE_STATE(IMT_MODE) = '0' then
        if branch_instr = '1' then
          if branch_taken = '1' then
            if btb(btb_addr_wr) < "11" then
              btb(btb_addr_wr) <= std_logic_vector(unsigned(btb(btb_addr_wr))+1);
            end if; 
          else
            if btb(btb_addr_wr) > "00" then
              btb(btb_addr_wr) <= std_logic_vector(unsigned(btb(btb_addr_wr))-1);
            end if;
          end if;
        end if;
      end if;
    end if;                           
  end process;

  end generate; -- btb_en = 1

  end generate; -- fetch_stage_en = 1

--------------------------------------------------------------------- end of IF stage -------------
---------------------------------------------------------------------------------------------------

end FETCH;
--------------------------------------------------------------------------------------------------
-- END of IE architecture ------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------