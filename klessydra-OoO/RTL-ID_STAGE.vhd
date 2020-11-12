----------------------------------------------------------------------------------------------------------------
--  Stage ID - (Instruction decode and registerfile read)                                                     --
--  Author(s): Abdallah Cheikh abdallah.cheikh@uniroma1.it (abdallah93.as@gmail.com)                          --
--                                                                                                            --
--  Date Modified: 07-04-2020                                                                                 --
----------------------------------------------------------------------------------------------------------------
--  Does operation decoding, and issues the result in a one-hot decoding form to the next stage               --
--  In this stage we detect based on the incoming instruction whether superscalar execution can be enabled.   --
--  This pipeline stage always takes one cycle latency                                                        --
----------------------------------------------------------------------------------------------------------------

-- package riscv_kless is new work.riscv_klessydra;

-- ieee packages ------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_misc.all;
use ieee.numeric_std.all;
use std.textio.all;

-- local packages ------------
use work.riscv_klessydra.all;
--use work.riscv_kless.all;
--use work.klessydra_parameters.all;

-- pipeline  pinout --------------------
entity ID_STAGE is
  generic(
    RV32M                        : natural;
    accl_en                      : natural;
    RF_CEIL                      : natural;
    Logical_RF_CEIL              : natural
    );
  port (
    -- clock, reset active low
    clk_i                        : in  std_logic;
    rst_ni                       : in  std_logic;
    -- OD Signals
    pc_ID                        : in  std_logic_vector(31 downto 0);  -- pc_ID is PC entering ID stage
    instr_rvalid_ID              : in  std_logic;
    -- Instruction Related Signals
    instr_word_ID_lat            : in  std_logic_vector(31 downto 0);
    instr_word_RENAME            : out std_logic_vector(31 downto 0);
    pc_RENAME                    : out std_logic_vector(31 downto 0);
    decoded_instr_RENAME_MUL_DIV : out std_logic_vector(MUL_DIV_UNIT_INSTR_SET_SIZE-1 downto 0);
    decoded_instr_RENAME_IE      : out std_logic_vector(EXEC_UNIT_INSTR_SET_SIZE-1 downto 0);
    decoded_instr_RENAME_LSU     : out std_logic_vector(LS_UNIT_INSTR_SET_SIZE-1 downto 0);
    decoded_instr_RENAME_DSP     : out std_logic_vector(DSP_UNIT_INSTR_SET_SIZE-1 downto 0);
    instr_valid_RENAME           : out std_logic;
    MUL_DIV_instr_RENAME         : out std_logic;
    IE_instr_RENAME              : out std_logic;
    LSU_instr_RENAME             : out std_logic;
    DSP_instr_RENAME             : out std_logic;
    rs1_valid_RENAME             : out std_logic;
    rs2_valid_RENAME             : out std_logic;
    rd_valid_RENAME              : out std_logic;
    rd_read_only_RENAME          : out std_logic;
    -- halt signals
    halt_ID                      : in  std_logic;
    halt_IF                      : out std_logic;
    -- Jump Signals
    absolute_jump                : in  std_logic;
    branch_instr                 : in  std_logic;
    set_branch_condition_ID      : out std_logic;
    id_taken_branch              : out std_logic;
    PC_offset_ID                 : out std_logic_vector(31 downto 0);
    -- MUL_DIV Signals
    signed_op_RENAME             : out std_logic;
    -- IE Signals 
    comparator_en_RENAME         : out std_logic;
    -- LSU Signals
    load_op_RENAME               : out std_logic;
    store_op_RENAME              : out std_logic;
    data_width_RENAME            : out std_logic_vector(1 downto 0);
    data_be_RENAME               : out std_logic_vector(3 downto 0);
    -- DSP Unit Signals
    spm_rs1_RENAME               : out std_logic;
    spm_rs2_RENAME               : out std_logic;
    vec_read_rs1_RENAME          : out std_logic;
    vec_read_rs2_RENAME          : out std_logic;
    vec_write_rd_RENAME          : out std_logic;
    vec_width_RENAME             : out std_logic_vector(1 downto 0);
    -- Recovery Buffer Signals
    decode_recovery              : in  std_logic
    );
end entity;  ------------------------------------------


-- Klessydra T03x (4 stages) pipeline implementation -----------------------
architecture DECODE of ID_STAGE is

  signal dispatch_en             : std_logic;
  signal instr_rvalid_ID_int     : std_logic;
  signal instr_rvalid_ID_int_lat : std_logic;

  -- instruction operands
  signal S_Imm_IE                : std_logic_vector(11 downto 0);  -- debugging signals
  signal I_Imm_IE                : std_logic_vector(11 downto 0);  -- debugging signals
  signal B_Imm_IE                : std_logic_vector(11 downto 0);  -- debugging signals
  signal CSR_ADDR_IE             : std_logic_vector(11 downto 0);  -- debugging signals

  function rs1 (signal instr : in std_logic_vector(31 downto 0)) return integer is
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

  fsm_ID_sync : process(clk_i, rst_ni, instr_word_ID_lat)  -- synch single state process
    variable OPCODE_wires  : std_logic_vector (6 downto 0);
    variable FUNCT3_wires  : std_logic_vector (2 downto 0);
    variable FUNCT7_wires  : std_logic_vector (6 downto 0);
    variable FUNCT12_wires : std_logic_vector (11 downto 0);
  begin
    OPCODE_wires  := OPCODE(instr_word_ID_lat);
    FUNCT3_wires  := FUNCT3(instr_word_ID_lat);
    FUNCT7_wires  := FUNCT7(instr_word_ID_lat);
    FUNCT12_wires := FUNCT12(instr_word_ID_lat);
    if rst_ni = '0' then
      pc_RENAME            <= (others => '0');
      instr_valid_RENAME   <= '0';
      MUL_DIV_instr_RENAME <= '0';
      IE_instr_RENAME      <= '0';
      LSU_instr_RENAME     <= '0';
      DSP_instr_RENAME     <= '0';
      comparator_en_RENAME <= '0';
      rs1_valid_RENAME     <= '0';
      rs2_valid_RENAME     <= '0';
      rd_valid_RENAME      <= '0';
      rd_read_only_RENAME  <= '0';
    elsif rising_edge(clk_i) then
      if halt_ID = '1' then -- the instruction pipeline is halted
        instr_valid_RENAME <= '0';
      elsif decode_recovery = '1' then
        instr_valid_RENAME <= '0';
      elsif instr_rvalid_ID_int = '0' then -- wait for a valid instruction
        instr_valid_RENAME <= '0';
      else  -- process the incoming instruction 
        instr_valid_RENAME  <= '1';
        instr_word_RENAME   <= instr_word_ID_lat;
        -- pc propagation
        pc_RENAME           <= pc_ID;
        --S_Imm_IE           <= std_logic_vector(to_unsigned(S_immediate(instr_word_ID_lat), 12));
        --I_Imm_IE           <= std_logic_vector(to_unsigned(to_integer(unsigned(I_immediate(instr_word_ID_lat))), 12));
        --B_Imm_IE           <= std_logic_vector(to_unsigned(to_integer(unsigned(B_immediate(instr_word_ID_lat))), 12));
        --CSR_ADDR_IE        <= std_logic_vector(to_unsigned(to_integer(unsigned(CSR_ADDR(instr_word_ID_lat))), 12));

        comparator_en_RENAME <= '0';
        IE_instr_RENAME      <= '0';
        LSU_instr_RENAME     <= '0';
        DSP_instr_RENAME     <= '0';
        load_op_RENAME       <= '0';
        store_op_RENAME      <= '0';
        signed_op_RENAME     <= '0';
        rs1_valid_RENAME     <= '0';
        rs2_valid_RENAME     <= '0';
        rd_valid_RENAME      <= '0';
        if accl_en = 1 then
          rd_read_only_RENAME  <= '0';
          spm_rs1_RENAME       <= '0';
          spm_rs2_RENAME       <= '0';
          vec_write_rd_RENAME  <= '0';
          vec_read_rs1_RENAME  <= '0';
          vec_read_rs2_RENAME  <= '0';
          vec_width_RENAME     <= "00";
        end if;

        -----------------------------------------------------------------------------------------------------------
        --  ██╗███╗   ██╗███████╗████████╗██████╗     ██████╗ ███████╗ ██████╗ ██████╗ ██████╗ ███████╗██████╗   --
        --  ██║████╗  ██║██╔════╝╚══██╔══╝██╔══██╗    ██╔══██╗██╔════╝██╔════╝██╔═══██╗██╔══██╗██╔════╝██╔══██╗  --
        --  ██║██╔██╗ ██║███████╗   ██║   ██████╔╝    ██║  ██║█████╗  ██║     ██║   ██║██║  ██║█████╗  ██████╔╝  --
        --  ██║██║╚██╗██║╚════██║   ██║   ██╔══██╗    ██║  ██║██╔══╝  ██║     ██║   ██║██║  ██║██╔══╝  ██╔══██╗  --
        --  ██║██║ ╚████║███████║   ██║   ██║  ██║    ██████╔╝███████╗╚██████╗╚██████╔╝██████╔╝███████╗██║  ██║  --
        --  ╚═╝╚═╝  ╚═══╝╚══════╝   ╚═╝   ╚═╝  ╚═╝    ╚═════╝ ╚══════╝ ╚═════╝ ╚═════╝ ╚═════╝ ╚══════╝╚═╝  ╚═╝  --
        -----------------------------------------------------------------------------------------------------------

        -- process the instruction
        -- read data from the operand registers
        -- Decode Starts here

        case OPCODE_wires is
			
          when OP_IMM =>
            IE_instr_RENAME <= '1';
            if(rd(instr_word_ID_lat) /= 0) then
              case FUNCT3_wires is
                when ADDI =>            -- ADDI instruction
                  decoded_instr_RENAME_IE <= ADDI_pattern;
                  rs1_valid_RENAME <= '1';
                  rd_valid_RENAME  <= '1';
                when SLTI =>            -- SLTI instruction
                  decoded_instr_RENAME_IE <= SLTI_pattern;
                  rs1_valid_RENAME <= '1';
                  rd_valid_RENAME  <= '1';
                when SLTIU =>           -- SLTIU instruction
                  decoded_instr_RENAME_IE <= SLTIU_pattern;
                  rs1_valid_RENAME <= '1';
                  rd_valid_RENAME  <= '1';
                when ANDI =>            -- ANDI instruction
                  decoded_instr_RENAME_IE <= ANDI_pattern;
                  rs1_valid_RENAME <= '1';
                  rd_valid_RENAME  <= '1';
                when ORI =>             -- ORI instruction
                  decoded_instr_RENAME_IE <= ORI_pattern;
                  rs1_valid_RENAME <= '1';
                  rd_valid_RENAME  <= '1';
                when XORI =>            -- XORI instruction
                  decoded_instr_RENAME_IE <= XORI_pattern;
                  rs1_valid_RENAME <= '1';
                  rd_valid_RENAME  <= '1';
                when SLLI =>            -- SLLI instruction
                  decoded_instr_RENAME_IE <= SLLI_pattern;
                  rs1_valid_RENAME <= '1';
                  rd_valid_RENAME  <= '1';
                when SRLI_SRAI =>
                  case FUNCT7_wires is
                    when SRLI7 =>       -- SRLI instruction
                      decoded_instr_RENAME_IE <= SRLI7_pattern;
                      rs1_valid_RENAME <= '1';
                      rd_valid_RENAME  <= '1';
                    when SRAI7 =>       -- SRAI instruction
                      decoded_instr_RENAME_IE <= SRAI7_pattern;
                      rs1_valid_RENAME <= '1';
                      rd_valid_RENAME  <= '1';
                    when others =>  -- ILLEGAL_INSTRUCTION                                      
                      decoded_instr_RENAME_IE <= ILL_pattern;
                  end case;  -- FUNCT7_wires cases
                when others =>  -- ILLEGAL_INSTRUCTION                                  
                  decoded_instr_RENAME_IE <= ILL_pattern;
              end case;  -- FUNCT3_wires cases   
            else                -- R0_INSTRUCTION                             
              decoded_instr_RENAME_IE <= NOP_pattern;
            end if;  -- if rd(instr_word_ID_lat) /=0
				
          when LUI =>                   -- LUI instruction
            IE_instr_RENAME <= '1';
            if (rd(instr_word_ID_lat) /= 0) then
              decoded_instr_RENAME_IE <= LUI_pattern;
              rd_valid_RENAME  <= '1';
            else                        -- R0_INSTRUCTION
              decoded_instr_RENAME_IE <= NOP_pattern;
            end if;
				
          when AUIPC =>                 -- AUIPC instruction
            IE_instr_RENAME <= '1';
            if (rd(instr_word_ID_lat) /= 0) then
              decoded_instr_RENAME_IE <= AUIPC_pattern;
              rd_valid_RENAME  <= '1';
            else                        -- R0_INSTRUCTION
              decoded_instr_RENAME_IE <= NOP_pattern;
            end if;

          when OP =>
            if (rd(instr_word_ID_lat) /= 0) then
              case FUNCT7_wires is
                when OP_I1 =>
                  IE_instr_RENAME <= '1';
                  case FUNCT3_wires is
                    when ADD => --ADD instruction
                      decoded_instr_RENAME_IE <= ADD7_pattern;
                      rs1_valid_RENAME <= '1';
                      rs2_valid_RENAME <= '1';
                      rd_valid_RENAME  <= '1';
                    when SLT =>             -- SLT instruction 
                      comparator_en_RENAME <= '1';
                      decoded_instr_RENAME_IE <= SLT_pattern;
                      rs1_valid_RENAME <= '1';
                      rs2_valid_RENAME <= '1';
                      rd_valid_RENAME  <= '1';
                    when SLTU =>            -- SLTU instruction
                      comparator_en_RENAME <= '1';
                      decoded_instr_RENAME_IE <= SLTU_pattern;
                      rs1_valid_RENAME <= '1';
                      rs2_valid_RENAME <= '1';
                      rd_valid_RENAME  <= '1';
                    when ANDD =>            -- AND instruction
                      decoded_instr_RENAME_IE <= ANDD_pattern;
                      rs1_valid_RENAME <= '1';
                      rs2_valid_RENAME <= '1';
                      rd_valid_RENAME  <= '1';
                    when ORR =>             -- OR instruction
                      decoded_instr_RENAME_IE <= ORR_pattern;
                      rs1_valid_RENAME <= '1';
                      rs2_valid_RENAME <= '1';
                      rd_valid_RENAME  <= '1';
                    when XORR =>            -- XOR instruction        
                      decoded_instr_RENAME_IE <= XORR_pattern;
                      rs1_valid_RENAME <= '1';
                      rs2_valid_RENAME <= '1';
                      rd_valid_RENAME  <= '1';
                    when SLLL =>            -- SLL instruction        
                      decoded_instr_RENAME_IE <= SLLL_pattern;
                      rs1_valid_RENAME <= '1';
                      rs2_valid_RENAME <= '1';
                      rd_valid_RENAME  <= '1';
                    when SRLL =>       -- SRL instruction   
                      decoded_instr_RENAME_IE <= SRLL7_pattern;
                      rs1_valid_RENAME <= '1';
                      rs2_valid_RENAME <= '1';
                      rd_valid_RENAME  <= '1';
                    when others =>  -- ILLEGAL_INSTRUCTION                                      
                      decoded_instr_RENAME_IE <= ILL_pattern;
                  end case;
                when OP_I2 =>
                  IE_instr_RENAME <= '1';
                  case FUNCT3_wires is
                    when SUB7 =>
                      decoded_instr_RENAME_IE <= SUB7_pattern;
                      rs1_valid_RENAME <= '1';
                      rs2_valid_RENAME <= '1';
                      rd_valid_RENAME  <= '1';
                    when SRAA =>
                      decoded_instr_RENAME_IE <= SRAA7_pattern;
                      rs1_valid_RENAME <= '1';
                      rs2_valid_RENAME <= '1';
                      rd_valid_RENAME  <= '1';
                    when others =>  -- ILLEGAL_INSTRUCTION                                      
                      decoded_instr_RENAME_IE <= ILL_pattern;
                  end case;
                when OP_M  =>                 -- MUL/DIV instructions
                  if RV32M = 1 then
                    case FUNCT3_wires is
                      when MUL =>
                        MUL_DIV_instr_RENAME <= '1';
                        comparator_en_RENAME <= '1';
                        decoded_instr_RENAME_MUL_DIV <= MUL_pattern;
                        rs1_valid_RENAME <= '1';
                        rs2_valid_RENAME <= '1';
                        rd_valid_RENAME  <= '1';
                      when MULH =>
                        MUL_DIV_instr_RENAME <= '1';
                        comparator_en_RENAME <= '1';
                        signed_op_RENAME <= '1';
                        decoded_instr_RENAME_MUL_DIV <= MULH_pattern;
                        rs1_valid_RENAME <= '1';
                        rs2_valid_RENAME <= '1';
                        rd_valid_RENAME  <= '1';
                      when MULHSU =>
                        MUL_DIV_instr_RENAME <= '1';
                        comparator_en_RENAME <= '1';
                        signed_op_RENAME <= '1';
                        decoded_instr_RENAME_MUL_DIV <= MULHSU_pattern;
                        rs1_valid_RENAME <= '1';
                        rs2_valid_RENAME <= '1';
                        rd_valid_RENAME  <= '1';
                      when MULHU =>
                        MUL_DIV_instr_RENAME <= '1';
                        comparator_en_RENAME <= '1';
                        decoded_instr_RENAME_MUL_DIV <= MULHU_pattern;
                        rs1_valid_RENAME <= '1';
                        rs2_valid_RENAME <= '1';
                        rd_valid_RENAME  <= '1';
                      when DIV =>
                        MUL_DIV_instr_RENAME <= '1';
                        comparator_en_RENAME <= '1';
                        signed_op_RENAME <= '1';
                        decoded_instr_RENAME_MUL_DIV <= DIV_pattern;
                        rs1_valid_RENAME <= '1';
                        rs2_valid_RENAME <= '1';
                        rd_valid_RENAME  <= '1';
                      when DIVU =>
                        MUL_DIV_instr_RENAME <= '1';
                        comparator_en_RENAME <= '1';
                        decoded_instr_RENAME_MUL_DIV <= DIVU_pattern;
                        rs1_valid_RENAME <= '1';
                        rs2_valid_RENAME <= '1';
                        rd_valid_RENAME  <= '1';
                      when REMD =>
                        MUL_DIV_instr_RENAME <= '1';
                        comparator_en_RENAME <= '1';
                        signed_op_RENAME <= '1';
                        decoded_instr_RENAME_MUL_DIV <= REM_pattern;
                        rs1_valid_RENAME <= '1';
                        rs2_valid_RENAME <= '1';
                        rd_valid_RENAME  <= '1';
                      when REMDU =>
                        MUL_DIV_instr_RENAME <= '1';
                        comparator_en_RENAME <= '1';
                        decoded_instr_RENAME_MUL_DIV <= REMU_pattern;
                        rs1_valid_RENAME <= '1';
                        rs2_valid_RENAME <= '1';
                        rd_valid_RENAME  <= '1';
                      when others =>
                        IE_instr_RENAME <= '1';
                        decoded_instr_RENAME_IE <= ILL_pattern;
                    end case;
                  else
                    IE_instr_RENAME <= '1';
                    decoded_instr_RENAME_IE <= ILL_pattern;                  
                  end if;
                when others =>  -- ILLEGAL_INSTRUCTION                                      
                  IE_instr_RENAME <= '1';
                  decoded_instr_RENAME_IE <= ILL_pattern;
              end case;
            else                        -- R0_INSTRUCTION
              IE_instr_RENAME <= '1';
              decoded_instr_RENAME_IE <= NOP_pattern;
            end if;

          when JAL =>                   -- JAL instruction
            IE_instr_RENAME <= '1';
            decoded_instr_RENAME_IE <= JAL_pattern;
            if (rd(instr_word_ID_lat) /= 0) then
              rd_valid_RENAME  <= '1';
            end if;

          when JALR =>                  -- JALR instruction
            IE_instr_RENAME <= '1';
            decoded_instr_RENAME_IE <= JALR_pattern;
            rs1_valid_RENAME <= '1';
            if (rd(instr_word_ID_lat) /= 0) then
              rd_valid_RENAME  <= '1';
            end if;

          when BRANCH =>      -- BRANCH instruction         
            IE_instr_RENAME <= '1';
            case FUNCT3_wires is
              when BEQ =>               -- BEQ instruction   
                comparator_en_RENAME <= '1';
                decoded_instr_RENAME_IE <= BEQ_pattern;
                rs1_valid_RENAME <= '1';
                rs2_valid_RENAME <= '1';
              when BNE =>               -- BNE instruction
                comparator_en_RENAME <= '1';
                decoded_instr_RENAME_IE <= BNE_pattern;
                rs1_valid_RENAME <= '1';
                rs2_valid_RENAME <= '1';
              when BLT =>               -- BLT instruction   
                comparator_en_RENAME <= '1';
                decoded_instr_RENAME_IE <= BLT_pattern;
                rs1_valid_RENAME <= '1';
                rs2_valid_RENAME <= '1';
              when BLTU =>              -- BLTU instruction
                comparator_en_RENAME <= '1';
                decoded_instr_RENAME_IE <= BLTU_pattern;
                rs1_valid_RENAME <= '1';
                rs2_valid_RENAME <= '1';
              when BGE =>               -- BGE instruction
                comparator_en_RENAME <= '1';
                decoded_instr_RENAME_IE <= BGE_pattern;
                rs1_valid_RENAME <= '1';
                rs2_valid_RENAME <= '1';
              when BGEU =>              -- BGEU instruction
                comparator_en_RENAME <= '1';
                decoded_instr_RENAME_IE <= BGEU_pattern;
                rs1_valid_RENAME <= '1';
                rs2_valid_RENAME <= '1';
              when others =>  -- ILLEGAL_INSTRUCTION                      
                decoded_instr_RENAME_IE <= ILL_pattern;
            end case;  -- FUNCT3_wires cases

          when LOAD =>                  -- LOAD instruction
            load_op_RENAME <= '1';
            if (rd(instr_word_ID_lat) /= 0) then  -- is all in the next_state process
              case FUNCT3_wires is
                when LW =>
                  LSU_instr_RENAME  <= '1';
                  data_width_RENAME <= "10";
                  data_be_RENAME    <= "1111";
                  rs1_valid_RENAME  <= '1';
                  rd_valid_RENAME   <= '1';
                  decoded_instr_RENAME_LSU <= LW_pattern;
                when LH =>
                  LSU_instr_RENAME  <= '1';
                  data_width_RENAME <= "01";
                  data_be_RENAME    <= "0011";
                  rs1_valid_RENAME  <= '1';
                  rd_valid_RENAME   <= '1';
                  decoded_instr_RENAME_LSU <= LH_pattern;
                when LHU =>
                  LSU_instr_RENAME  <= '1';
                  data_width_RENAME <= "01";
                  data_be_RENAME    <= "0011";
                  rs1_valid_RENAME  <= '1';
                  rd_valid_RENAME   <= '1';
                  decoded_instr_RENAME_LSU <= LHU_pattern;
                when LB =>
                  LSU_instr_RENAME  <= '1';
                  data_width_RENAME <= "00";
                  data_be_RENAME    <= "0001";
                  rs1_valid_RENAME  <= '1';
                  rd_valid_RENAME   <= '1';
                  decoded_instr_RENAME_LSU <= LB_pattern;
                when LBU =>
                  LSU_instr_RENAME  <= '1';
                  data_width_RENAME <= "00";
                  data_be_RENAME    <= "0001";
                  rs1_valid_RENAME  <= '1';
                  rd_valid_RENAME   <= '1';
                  decoded_instr_RENAME_LSU <= LBU_pattern;
                when others =>          -- ILLEGAL_INSTRUCTION
                  IE_instr_RENAME <= '1';
                  decoded_instr_RENAME_IE <= ILL_pattern;
              end case;
            else                        -- R0_INSTRUCTION
              IE_instr_RENAME <= '1';
              decoded_instr_RENAME_IE <= NOP_pattern;
            end if;

          when STORE =>                 -- STORE instruction
            store_op_RENAME <= '1';
            case FUNCT3_wires is
              when SW =>                -- is all in the next_state process
                LSU_instr_RENAME  <= '1';
                data_width_RENAME <= "10";
                data_be_RENAME    <= "1111";
                rs1_valid_RENAME  <= '1';
                rs2_valid_RENAME  <= '1';
                decoded_instr_RENAME_LSU <= SW_pattern;
              when SH =>
                LSU_instr_RENAME  <= '1';
                data_width_RENAME <= "01";
                data_be_RENAME    <= "0011";
                rs1_valid_RENAME  <= '1';
                rs2_valid_RENAME  <= '1';
                decoded_instr_RENAME_LSU <= SH_pattern;
              when SB =>
                LSU_instr_RENAME  <= '1';
                data_width_RENAME <= "00";
                data_be_RENAME    <= "0001";
                rs1_valid_RENAME  <= '1';
                rs2_valid_RENAME  <= '1';
                decoded_instr_RENAME_LSU <= SB_pattern;
              when others =>  -- ILLEGAL_INSTRUCTION
                IE_instr_RENAME <= '1';
                decoded_instr_RENAME_IE <= ILL_pattern;
            end case;

          when MISC_MEM =>
            IE_instr_RENAME <= '1';
            case FUNCT3_wires is
              when FENCE =>             -- FENCE instruction
                decoded_instr_RENAME_IE <= FENCE_pattern;
              when FENCEI =>            -- FENCEI instruction
                decoded_instr_RENAME_IE <= FENCEI_pattern;
              when others =>            -- ILLEGAL_INSTRUCTION
                decoded_instr_RENAME_IE <= ILL_pattern;
            end case;  -- FUNCT3_wires cases

          when SYSTEM =>
            IE_instr_RENAME <= '1';
            case FUNCT3_wires is
              when PRIV =>
                if (rs1(instr_word_ID_lat) = 0 and rd(instr_word_ID_lat) = 0) then
                  case FUNCT12_wires is
                    when ECALL =>       -- ECALL instruction
                      decoded_instr_RENAME_IE <= ECALL_pattern;
                    when EBREAK =>      -- EBREAK instruction       
                      decoded_instr_RENAME_IE <= EBREAK_pattern;
                    when mret =>        -- mret instruction   
                      decoded_instr_RENAME_IE <= MRET_pattern;
                    when WFI =>         -- WFI instruction     
                      decoded_instr_RENAME_IE <= WFI_pattern;
                    when others =>  -- ILLEGAL_INSTRUCTION                                              
                      decoded_instr_RENAME_IE <= ILL_pattern;
                  end case;  -- FUNCT12_wires cases
                else  -- ILLEGAL_INSTRUCTION                            
                  decoded_instr_RENAME_IE <= ILL_pattern;
                end if;
              when CSRRW =>
                rs1_valid_RENAME <= '1';
                if (rd(instr_word_ID_lat) /= 0) then
                  rd_valid_RENAME  <= '1';
                end if;
                decoded_instr_RENAME_IE <= CSRRW_pattern;
              when CSRRS =>
                if(rd(instr_word_ID_lat) /= 0) then
                  rs1_valid_RENAME <= '1';
                  rd_valid_RENAME  <= '1';
                  decoded_instr_RENAME_IE <= CSRRS_pattern;
                else                    -- R0_INSTRUCTION
                  decoded_instr_RENAME_IE <= NOP_pattern;
                end if;
              when CSRRC =>
                if(rd(instr_word_ID_lat) /= 0) then
                  rs1_valid_RENAME <= '1';
                  rd_valid_RENAME  <= '1';
                  decoded_instr_RENAME_IE <= CSRRC_pattern;
                else                    -- R0_INSTRUCTION
                  decoded_instr_RENAME_IE <= NOP_pattern;
                end if;
              when CSRRWI =>
                rs1_valid_RENAME <= '1';
                if (rd(instr_word_ID_lat) /= 0) then
                  rd_valid_RENAME  <= '1';
                end if;
                decoded_instr_RENAME_IE <= CSRRWI_pattern;
              when CSRRSI =>
                if(rd(instr_word_ID_lat) /= 0) then
                  rs1_valid_RENAME <= '1';
                  rd_valid_RENAME  <= '1';
                  decoded_instr_RENAME_IE <= CSRRSI_pattern;
                else                    -- R0_INSTRUCTION
                  decoded_instr_RENAME_IE <= NOP_pattern; -- AAA highly likely not to be a NOP
                end if;
              when CSRRCI =>
                if(rd(instr_word_ID_lat) /= 0) then
                  rs1_valid_RENAME <= '1';
                  rd_valid_RENAME  <= '1';
                  decoded_instr_RENAME_IE <= CSRRCI_pattern;
                else                    -- R0_INSTRUCTION
                  decoded_instr_RENAME_IE <= NOP_pattern;
                end if;
              when others =>  -- ILLEGAL_INSTRUCTION                      
                decoded_instr_RENAME_IE <= ILL_pattern;
            end case;  -- FUNCT3_wires cases

          when KMEM =>
            if accl_en = 1 then
              case FUNCT7_wires is
                when KMEMLD =>          -- KMEMLD_INSTRUCTION
                  LSU_instr_RENAME     <= '1';
                  rs1_valid_RENAME     <= '1';
                  rs2_valid_RENAME     <= '1';
                  rd_read_only_RENAME  <= '1';
                  decoded_instr_RENAME_LSU <= KMEMLD_pattern;
                when KMEMSTR =>         -- KMEMSTR_INSTRUCTION
                  LSU_instr_RENAME     <= '1';
                  rs1_valid_RENAME     <= '1';
                  rs2_valid_RENAME     <= '1';
                  rd_read_only_RENAME  <= '1';
                  decoded_instr_RENAME_LSU <= KMEMSTR_pattern;
                when KBCASTLD =>         -- KBCASTLD_INSTRUCTION
                  LSU_instr_RENAME     <= '1';
                  rs1_valid_RENAME     <= '1';
                  rs2_valid_RENAME     <= '1';
                  rd_read_only_RENAME  <= '1';
                  decoded_instr_RENAME_LSU <= KBCASTLD_pattern;
                when others =>            -- ILLEGAL_INSTRUCTION
                  IE_instr_RENAME <= '1';
                  decoded_instr_RENAME_IE <= ILL_pattern;
              end case;
            end if;

          when KDSP =>
            if accl_en = 1 then
              case FUNCT7_wires is
                when KADDV =>           -- KADDV_INSTRUCTION
                  DSP_instr_RENAME     <= '1';
                  vec_write_rd_RENAME  <= '1';
                  vec_read_rs1_RENAME  <= '1';
                  vec_read_rs2_RENAME  <= '1';
                  spm_rs1_RENAME       <= '1';
                  spm_rs2_RENAME       <= '1';
                  rs1_valid_RENAME     <= '1';
                  rs2_valid_RENAME     <= '1';
                  rd_read_only_RENAME  <= '1';
                  decoded_instr_RENAME_DSP <= KADDV_pattern;
                when KSUBV =>           -- KSUBV_INSTRUCTION
                  DSP_instr_RENAME     <= '1';
                  vec_write_rd_RENAME  <= '1';
                  vec_read_rs1_RENAME  <= '1';
                  vec_read_rs2_RENAME  <= '1';
                  spm_rs1_RENAME       <= '1';
                  spm_rs2_RENAME       <= '1';
                  rs1_valid_RENAME     <= '1';
                  rs2_valid_RENAME     <= '1';
                  rd_read_only_RENAME  <= '1';
                  decoded_instr_RENAME_DSP <= KSUBV_pattern;
                when KVMUL =>           -- KVMUL_INSTRUCTION
                  DSP_instr_RENAME     <= '1';
                  vec_write_rd_RENAME  <= '1';
                  vec_read_rs1_RENAME  <= '1';
                  vec_read_rs2_RENAME  <= '1';
                  spm_rs1_RENAME       <= '1';
                  spm_rs2_RENAME       <= '1';
                  rs1_valid_RENAME     <= '1';
                  rs2_valid_RENAME     <= '1';
                  rd_read_only_RENAME  <= '1';
                  decoded_instr_RENAME_DSP <= KVMUL_pattern;
                when KVRED =>           -- KVRED_INSTRUCTION
                  DSP_instr_RENAME     <= '1';
                  vec_read_rs1_RENAME  <= '1';
                  spm_rs1_RENAME       <= '1';
                  rs1_valid_RENAME     <= '1';
                  rd_read_only_RENAME  <= '1';
                  decoded_instr_RENAME_DSP <= KVRED_pattern;
                when KDOTP =>           -- KDOTP_INSTRUCTION
                  DSP_instr_RENAME     <= '1';
                  vec_read_rs1_RENAME  <= '1';
                  vec_read_rs2_RENAME  <= '1';
                  spm_rs1_RENAME       <= '1';
                  spm_rs2_RENAME       <= '1';
                  rs1_valid_RENAME     <= '1';
                  rs2_valid_RENAME     <= '1';
                  rd_read_only_RENAME  <= '1';
                  decoded_instr_RENAME_DSP <= KDOTP_pattern;
                when KDOTPPS =>           -- KDOTPPS_INSTRUCTION
                  DSP_instr_RENAME     <= '1';
                  vec_read_rs1_RENAME  <= '1';
                  vec_read_rs2_RENAME  <= '1';
                  spm_rs1_RENAME       <= '1';
                  spm_rs2_RENAME       <= '1';
                  rs1_valid_RENAME     <= '1';
                  rs2_valid_RENAME     <= '1';
                  rd_read_only_RENAME  <= '1';
                  decoded_instr_RENAME_DSP <= KDOTPPS_pattern;
                when KSVADDSC =>           -- KSVADDSC_INSTRUCTION
                  DSP_instr_RENAME     <= '1';
                  vec_read_rs1_RENAME  <= '1';
                  vec_write_rd_RENAME  <= '1';
                  spm_rs1_RENAME       <= '1';
                  spm_rs2_RENAME       <= '1';
                  rs1_valid_RENAME     <= '1';
                  rs2_valid_RENAME     <= '1';
                  rd_read_only_RENAME  <= '1';
                  decoded_instr_RENAME_DSP <= KSVADDSC_pattern;
                when KSVADDRF =>           -- KSVADDRF_INSTRUCTION
                  DSP_instr_RENAME     <= '1';
                  vec_read_rs1_RENAME  <= '1';
                  vec_write_rd_RENAME  <= '1';
                  spm_rs1_RENAME       <= '1';
                  rs1_valid_RENAME     <= '1';
                  rs2_valid_RENAME     <= '1';
                  rd_read_only_RENAME  <= '1';
                  decoded_instr_RENAME_DSP <= KSVADDRF_pattern;
                when KSVMULSC =>           -- KSVMULSC_INSTRUCTION
                  DSP_instr_RENAME     <= '1';
                  vec_read_rs1_RENAME  <= '1';
                  vec_write_rd_RENAME  <= '1';
                  spm_rs1_RENAME       <= '1';
                  spm_rs2_RENAME       <= '1';
                  rs1_valid_RENAME     <= '1';
                  rs2_valid_RENAME     <= '1';
                  rd_read_only_RENAME  <= '1';
                  decoded_instr_RENAME_DSP <= KSVMULSC_pattern;
                when KSVMULRF =>           -- KSVMULRF_INSTRUCTION
                  DSP_instr_RENAME     <= '1';
                  vec_read_rs1_RENAME  <= '1';
                  vec_write_rd_RENAME  <= '1';
                  spm_rs1_RENAME       <= '1';
                  rs1_valid_RENAME     <= '1';
                  rs2_valid_RENAME     <= '1';
                  rd_read_only_RENAME  <= '1';
                  decoded_instr_RENAME_DSP <= KSVMULRF_pattern;
                when KSRAV =>           -- KSRAV_INSTRUCTION
                  DSP_instr_RENAME     <= '1';
                  vec_read_rs1_RENAME  <= '1';
                  vec_write_rd_RENAME  <= '1';
                  spm_rs1_RENAME       <= '1';
                  rs1_valid_RENAME     <= '1';
                  rs2_valid_RENAME     <= '1';
                  rd_read_only_RENAME  <= '1';
                  decoded_instr_RENAME_DSP <= KSRAV_pattern;
                when KSRLV =>           -- KSRLV_INSTRUCTION
                  DSP_instr_RENAME     <= '1';
                  vec_read_rs1_RENAME  <= '1';
                  vec_write_rd_RENAME  <= '1';
                  spm_rs1_RENAME       <= '1';
                  rs1_valid_RENAME     <= '1';
                  rs2_valid_RENAME     <= '1';
                  rd_read_only_RENAME  <= '1';
                  decoded_instr_RENAME_DSP <= KSRLV_pattern;
                when KRELU =>           -- KRELU_INSTRUCTION
                  DSP_instr_RENAME     <= '1';
                  vec_read_rs1_RENAME  <= '1';
                  vec_write_rd_RENAME  <= '1';
                  spm_rs1_RENAME       <= '1';
                  rs1_valid_RENAME     <= '1';
                  rd_read_only_RENAME  <= '1';
                  decoded_instr_RENAME_DSP <= KRELU_pattern;
                when KVSLT =>
                  DSP_instr_RENAME     <= '1';
                  vec_write_rd_RENAME  <= '1';
                  vec_read_rs1_RENAME  <= '1';
                  vec_read_rs2_RENAME  <= '1';
                  spm_rs1_RENAME       <= '1';
                  spm_rs2_RENAME       <= '1';
                  rs1_valid_RENAME     <= '1';
                  rs2_valid_RENAME     <= '1';
                  rd_read_only_RENAME  <= '1';
                  decoded_instr_RENAME_DSP <= KVSLT_pattern;
                when KSVSLT =>
                  DSP_instr_RENAME     <= '1';
                  vec_write_rd_RENAME  <= '1';
                  vec_read_rs1_RENAME  <= '1';
                  spm_rs1_RENAME       <= '1';
                  rs1_valid_RENAME     <= '1';
                  rs2_valid_RENAME     <= '1';
                  rd_read_only_RENAME  <= '1';
                  decoded_instr_RENAME_DSP <= KSVSLT_pattern;
                when KBCAST =>           -- KBCAST_INSTRUCTION
                  DSP_instr_RENAME     <= '1';
                  vec_write_rd_RENAME  <= '1';
                  rs1_valid_RENAME     <= '1';
                  rd_read_only_RENAME  <= '1';
                  decoded_instr_RENAME_DSP <= KBCAST_pattern;
                when KVCP =>           -- KVCP_INSTRUCTION
                  DSP_instr_RENAME     <= '1';
                  vec_read_rs1_RENAME  <= '1';
                  vec_write_rd_RENAME  <= '1';
                  spm_rs1_RENAME       <= '1';
                  rs1_valid_RENAME     <= '1';
                  rd_read_only_RENAME  <= '1';
                  decoded_instr_RENAME_DSP <= KVCP_pattern;
                when others =>            -- ILLEGAL_INSTRUCTION
                  IE_instr_RENAME <= '1';
                  decoded_instr_RENAME_IE <= ILL_pattern;
              end case;
            end if;

          when others =>                -- ILLEGAL_INSTRUCTION
            IE_instr_RENAME <= '1';
            decoded_instr_RENAME_IE <= ILL_pattern;

        end case;  -- OPCODE_wires cases                           
        -- Decode OF INSTRUCTION (END) --------------------------

      end if;  -- instr. conditions
    end if;  -- clk
  end process;

  -- If the instruction is not dispatched, we either take the input or the ltached instruction
  -- Else is the instruction is dispatched, we only take the input instruction in order to avoid sending latched instruction twice
  instr_rvalid_ID_int <= instr_rvalid_ID or instr_rvalid_ID_int_lat when instr_valid_RENAME = '0' else instr_rvalid_ID;

  process(clk_i, rst_ni)
  begin
    if rst_ni = '0' then
      instr_rvalid_ID_int_lat <= '0';
    elsif rising_edge(clk_i) then
      if instr_rvalid_ID = '1' then
        instr_rvalid_ID_int_lat <= '1';
      elsif halt_ID = '0' and instr_rvalid_ID = '0' then
        instr_rvalid_ID_int_lat <= '0';
      end if;
    end if;
  end process;

  fsm_ID_comb : process(all)  -- synch single state process
    variable OPCODE_wires  : std_logic_vector (6 downto 0);
    variable FUNCT3_wires  : std_logic_vector (2 downto 0);
    variable FUNCT7_wires  : std_logic_vector (6 downto 0);
    variable FUNCT12_wires : std_logic_vector (11 downto 0);
  begin
    OPCODE_wires  := OPCODE(instr_word_ID_lat);
    FUNCT3_wires  := FUNCT3(instr_word_ID_lat);
    FUNCT7_wires  := FUNCT7(instr_word_ID_lat);
    FUNCT12_wires := FUNCT12(instr_word_ID_lat);
    set_branch_condition_ID <= '0';
    id_taken_branch         <= '0';
    halt_IF                 <= halt_ID;
    --if halt_ID = '1' then -- the instruction pipeline is halted
    --elsif instr_rvalid_ID_int = '0' then -- wait for a valid instruction
    --else  -- process the incoming instruction 
      case OPCODE_wires is

        when JAL =>                   -- JAL instruction
          set_branch_condition_ID <= '1';
          id_taken_branch         <= '1';
          PC_offset_ID            <= UJ_immediate(instr_word_ID_lat);

        when JALR =>                  -- JALR instruction
          if absolute_jump = '0' then
            halt_IF <= '1';
          end if;

        when BRANCH =>      -- BRANCH instruction         
          case FUNCT3_wires is
            when BEQ =>               -- BEQ instruction
              --if branch_instr = '0' then
              --  halt_IF <= '1';
              --end if;
            when BNE =>               -- BNE instruction
              --if branch_instr = '0' then
              --  halt_IF <= '1';
              --end if;
            when BLT =>               -- BLT instruction
              --if branch_instr = '0' then
              --  halt_IF <= '1';
              --end if;
            when BLTU =>              -- BLTU instruction
              --if branch_instr = '0' then
              --  halt_IF <= '1';
              --end if;
            when BGE =>               -- BGE instruction
              --if branch_instr = '0' then
              --  halt_IF <= '1';
              --end if;
            when BGEU =>              -- BGEU instruction
              --if branch_instr = '0' then
              --  halt_IF <= '1';
              --end if;
            when others =>  -- ILLEGAL_INSTRUCTION
              null;                   
          end case;  -- FUNCT3_wires cases

        when SYSTEM =>
          case FUNCT3_wires is
            when PRIV =>
              if (rs1(instr_word_ID_lat) = 0 and rd(instr_word_ID_lat) = 0) then
                case FUNCT12_wires is
                  when ECALL =>       -- ECALL instruction
                  when mret =>        -- mret instruction   
                  when WFI =>         -- WFI instruction
                  when others =>
                    null;
                end case;  -- FUNCT12_wires cases
              end if;
            when others =>
              null;
          end case;  -- FUNCT3_wires cases
        when others =>
          null;
      end case;  -- OPCODE_wires cases                           
        -- Decode OF INSTRUCTION (END) --------------------------

    --end if;  -- instr. conditions
  end process;


---------------------------------------------------------------------- end of ID stage -----------
--------------------------------------------------------------------------------------------------
end DECODE;
--------------------------------------------------------------------------------------------------
-- END of ID architecture ------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------
