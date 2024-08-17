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
    THREAD_POOL_SIZE           : natural;
    RV32M                      : natural;
    RV32F                      : natural;
    morph_en                   : natural;
    fetch_stage_en             : natural;
    branch_predict_en          : natural;
    btb_en                     : natural;
    superscalar_exec_en        : natural;
    accl_en                    : natural;
    replicate_accl_en          : natural;
    SPM_NUM                    : natural;  
    Addr_Width                 : natural;
    SPM_STRT_ADDR              : std_logic_vector(31 downto 0);
    ACCL_NUM                   : natural;
    TPS_CEIL                   : natural;
    RF_SIZE                    : natural;
    RF_CEIL                    : natural;
    SPM_ADDR_WID               : natural
    );
  port (
  -- Branch Control Signals
    comparator_en              : out std_logic;
    ls_instr_req               : out std_logic;
    ie_instr_req               : out std_logic;
    dsp_instr_req              : out std_logic_vector(ACCL_NUM-1 downto 0);
    decoded_branching_instr    : in  std_logic_vector(BRANCHING_INSTR_SET_SIZE-1 downto 0);
    decoded_instruction_IE     : out std_logic_vector(EXEC_UNIT_INSTR_SET_SIZE-1 downto 0);
    decoded_instruction_LS     : out std_logic_vector(LS_UNIT_INSTR_SET_SIZE-1 downto 0);
    decoded_instruction_DSP    : out std_logic_vector(DSP_UNIT_INSTR_SET_SIZE-1 downto 0);
    data_be_ID                 : out std_logic_vector(3 downto 0);
    data_width_ID              : out std_logic_vector(1 downto 0);
    amo_store                  : in  std_logic;
    amo_load                   : out std_logic;
    amo_load_skip              : out std_logic;
    load_op                    : out std_logic;
    store_op                   : out std_logic;
    instr_word_IE              : out std_logic_vector(31 downto 0);
    MSTATUS                    : in  array_2D(THREAD_POOL_SIZE-1 downto 0)(1 downto 0);
    harc_FETCH                 : in  natural range THREAD_POOL_SIZE-1 downto 0;
    harc_ID                    : in  natural range THREAD_POOL_SIZE-1 downto 0;
    pc_ID                      : in  std_logic_vector(31 downto 0);  -- pc_ID is PC entering ID stage
    rs1_valid_ID               : in  std_logic;
    rs2_valid_ID               : in  std_logic;
    rd_valid_ID                : in  std_logic;
    rd_read_valid_ID           : in  std_logic;
    data_dependency            : out std_logic;
    bypass_rs1                 : out std_logic;
    bypass_rs2                 : out std_logic;
    bypass_rd_read             : out std_logic;
    bypass_fp_rs1              : out std_logic;
    bypass_fp_rs2              : out std_logic;
    bypass_fp_rd_read          : out std_logic;
    jalr_stall                 : out std_logic;
    branch_stall               : out std_logic;
    core_busy_IE               : in  std_logic;
    core_busy_LS               : in  std_logic;
    busy_LS                    : in  std_logic;
    busy_FPU                   : in  std_logic;
    busy_DSP                   : in  std_logic_vector(ACCL_NUM-1 downto 0);
    busy_ID                    : out std_logic;
    ls_parallel_exec           : out std_logic;
    fpu_parallel_exec          : out std_logic;
    dsp_parallel_exec          : out std_logic;
    dsp_to_jump_wire           : out std_logic;
    dsp_to_jump                : out std_logic_vector(THREAD_POOL_SIZE-1 downto 0);
    pc_IE                      : out std_logic_vector(31 downto 0);  -- pc_IE is pc entering stage IE ***
    WB_EN_next_ID              : out std_logic;
    instr_rvalid_ID            : in  std_logic; 
    instr_rvalid_IE            : out std_logic;  -- validity bit at IE input
    instr_rvalid_ID_int        : out std_logic;
    halt_IE                    : out std_logic;
    halt_LSU                   : out std_logic;
    instr_word_ID              : in  std_logic_vector(31 downto 0);
    instr_word_FETCH           : in  std_logic_vector(31 downto 0);
    spm_rs1                    : out std_logic;
    spm_rs2                    : out std_logic;
    harc_sleep                 : in  std_logic_vector(THREAD_POOL_SIZE-1 downto 0);
    CORE_STATE                 : in  std_logic_vector(THREAD_POOL_BASELINE downto 0);
    CORE_STATE_FETCH           : in  std_logic_vector(THREAD_POOL_BASELINE downto 0);
    CORE_STATE_ID              : out std_logic_vector(THREAD_POOL_BASELINE downto 0);
    set_except_condition       : in  std_logic;
    served_irq                 : in  std_logic_vector(THREAD_POOL_SIZE-1 downto 0);
    flush_decode               : out std_logic_vector(THREAD_POOL_SIZE-1 downto 0);
    jalr_flush                 : out std_logic_vector(THREAD_POOL_SIZE-1 downto 0);
    signed_op                  : out std_logic;
    harc_EXEC                  : out natural range THREAD_POOL_SIZE-1 downto 0;
    harc_WB                    : in  natural range THREAD_POOL_SIZE-1 downto 0;
    branch_instr               : in  std_logic;
    absolute_jump              : in  std_logic_vector(THREAD_POOL_SIZE-1 downto 0);
    harc_LS_WB                 : in  natural range THREAD_POOL_SIZE-1 downto 0;
    harc_IE_WB                 : in  natural range THREAD_POOL_SIZE-1 downto 0;
    LS_WB_EN                   : in  std_logic;
    IE_WB_EN                   : in  std_logic;
    MUL_WB_EN                  : in  std_logic;
    LS_WB_EN_wire              : in  std_logic;
    IE_WB_EN_wire              : in  std_logic;
    MUL_WB_EN_wire             : in  std_logic;
    instr_word_LS_WB           : in  std_logic_vector(31 downto 0);
    instr_word_IE_WB           : in  std_logic_vector(31 downto 0);
    vec_read_rs1_ID            : out std_logic;
    vec_read_rs2_ID            : out std_logic;
    vec_write_rd_ID            : out std_logic;
    vec_width_ID               : out std_logic_vector(1 downto 0);
    PC_offset_ID               : out std_logic_vector(31 downto 0);
    set_branch_condition_ID    : out std_logic;
    zero_rd                    : out std_logic;
    float_instr_req            : out std_logic; 
    decoded_instruction_FLOAT  : out std_logic_vector(FP_UNIT_INSTR_SET_SIZE-1 downto 0); 
    -- branch predictin
    flush_hart_ID              : in  std_logic_vector(THREAD_POOL_SIZE-1 downto 0);
    branch_predict_taken_ID    : in  std_logic;
    branch_predict_taken_IE    : out std_logic;
    -- clock, reset active low
    clk_i                      : in  std_logic;
    rst_ni                     : in  std_logic
    );

end entity;  ------------------------------------------


-- Klessydra T03x (4 stages) pipeline implementation -----------------------
architecture DECODE of ID_STAGE is

  subtype harc_range is natural range THREAD_POOL_SIZE-1 downto 0;
  subtype accl_range is integer range ACCL_NUM-1 downto 0; 

  signal branch_predict_taken_IE_int     : std_logic;
  signal branch_predict_taken_IE_int_lat : std_logic;

  signal harc_ID_to_DSP            : accl_range;
  signal dsp_instr_req_wire        : std_logic_vector(accl_range);
  -- instruction operands
  signal S_Imm_IE                  : std_logic_vector(11 downto 0);  -- debugging signals
  signal I_Imm_IE                  : std_logic_vector(11 downto 0);  -- debugging signals
  signal B_Imm_IE                  : std_logic_vector(11 downto 0);  -- debugging signals
  signal CSR_ADDR_IE               : std_logic_vector(11 downto 0);  -- debugging signals

  -- data dependency checker
  signal valid_buf_lat             : array_2D(harc_range)(31 downto 0);  -- valid buffer for data dependency checker
  signal valid_buf                 : array_2D(harc_range)(31 downto 0);  -- valid buffer for data dependency checker
  signal valid_buf_wire            : array_2D(harc_range)(31 downto 0);  -- valid buffer for data dependency checker
  signal rf_rs1_valid              : std_logic_vector(harc_range);
  signal rf_rs2_valid              : std_logic_vector(harc_range);
  signal rf_rd_read_valid          : std_logic_vector(harc_range);
  signal rs1_valid_int             : std_logic;
  signal rs2_valid_int             : std_logic;
  signal rd_valid_int              : std_logic;
  signal rd_read_valid_int         : std_logic;
  signal rs1_valid                 : std_logic;
  signal rs2_valid                 : std_logic;
  signal rd_valid                  : std_logic;
  signal rd_read_valid             : std_logic;

  signal Immediate                 : std_logic_vector(31 downto 0);

  signal instr_rvalid_ID_int_lat   : std_logic;

  signal block_valid_wb            : std_logic;
  signal zero_rd_wire              : std_logic;

  -- branch prediction
  signal branch_predict_taken_IE_wire : std_logic;

  signal data_dependency_JALR      : std_logic;

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

  function or_vect_bits(input_vector : std_logic_vector) return std_logic is
    variable result : std_logic := '0';
  begin
    for i in input_vector'range loop
      result := result or input_vector(i);
    end loop;
    return result;
  end function or_vect_bits;


begin

  zero_rd_wire  <= '1' when rd(instr_word_ID) = 0 else '0';

  rs1_valid     <= rs1_valid_int     when fetch_stage_en = 0 else rs1_valid_ID;  
  rs2_valid     <= rs2_valid_int     when fetch_stage_en = 0 else rs2_valid_ID;  
  rd_valid      <= rd_valid_int      when fetch_stage_en = 0 else rd_valid_ID;  
  rd_read_valid <= rd_read_valid_int when fetch_stage_en = 0 else rd_read_valid_ID;  


  fsm_ID_sync : process(clk_i, rst_ni, instr_word_ID)  -- synch single state process
    variable OPCODE_wires  : std_logic_vector(6 downto 0);
    variable FUNCT3_wires  : std_logic_vector(2 downto 0);
    variable FUNCT7_wires  : std_logic_vector(6 downto 0);
    variable FUNCT12_wires : std_logic_vector(11 downto 0);
  begin
    OPCODE_wires  := OPCODE(instr_word_ID);
    FUNCT3_wires  := FUNCT3(instr_word_ID);
    FUNCT7_wires  := FUNCT7(instr_word_ID);
    FUNCT12_wires := FUNCT12(instr_word_ID);
    if rst_ni = '0' then
      pc_IE               <= (others => '0');
      flush_decode        <= (others => '0');
      jalr_flush          <= (others => '0');
      CORE_STATE_ID       <= '1' & (0 to THREAD_POOL_BASELINE-1 => '0');
      harc_EXEC           <=  0;
      instr_rvalid_IE     <= '0';
      ie_instr_req        <= '0';
      ls_instr_req        <= '0';
      float_instr_req     <= '0';
      comparator_en       <= '0';
      WB_EN_next_ID       <= '0';
    elsif rising_edge(clk_i) then
      flush_decode     <= (others => '0');
      jalr_flush       <= (others => '0');
      ls_instr_req     <= '0';
      ie_instr_req     <= '0';
      float_instr_req  <= '0';
      WB_EN_next_ID    <= '0'; 
      instr_rvalid_IE  <= '0';

      if served_irq(harc_ID)             = '1'  or
         flush_decode(harc_ID)           = '1'  or
         -- harc_sleep(harc_ID)            = '1'  or -- used not to let a to-be-flushed harc to pass to the IE, but currently blocks all sleeping harts which is wrong
         core_busy_IE                    = '1'  or 
         core_busy_LS                    = '1'  or 
         ls_parallel_exec                = '0'  or 
         fpu_parallel_exec               = '0'  or 
         dsp_parallel_exec               = '0'  or 
         (data_dependency                = '1'  and flush_hart_ID(harc_ID) = '0') or 
         flush_hart_ID(harc_ID)          = '1'  then -- the instruction pipeline is halted
        halt_IE  <= '1';
        halt_LSU <= '1';
      elsif instr_rvalid_ID_int = '0' then -- wait for a valid instruction
        halt_IE  <= '0';
        halt_LSU <= '0';
      else  -- process the incoming instruction 
        zero_rd  <= zero_rd_wire;
        halt_IE  <= '0';
        halt_LSU <= '0';
        instr_rvalid_IE  <= '1';
        if dsp_to_jump_wire = '0' then
          instr_word_IE  <= instr_word_ID;
        else
          instr_word_IE  <= x"0000_006F";
        end if;
        -- pc propagation
        pc_IE               <= pc_ID;
        -- harc propagation
        harc_EXEC           <= harc_ID;
        if fetch_stage_en = 1 then
          CORE_STATE_ID       <= CORE_STATE_FETCH;
        else
          CORE_STATE_ID       <= CORE_STATE;
        end if;
        --S_Imm_IE           <= std_logic_vector(to_unsigned(S_immediate(instr_word_ID), 12));
        --I_Imm_IE           <= std_logic_vector(to_unsigned(to_integer(unsigned(I_immediate(instr_word_ID))), 12));
        --B_Imm_IE           <= std_logic_vector(to_unsigned(to_integer(unsigned(B_immediate(instr_word_ID))), 12));
        --CSR_ADDR_IE        <= std_logic_vector(to_unsigned(to_integer(unsigned(CSR_ADDR(instr_word_ID))), 12));

        
        branch_predict_taken_IE <= branch_predict_taken_IE_wire or branch_predict_taken_IE_int; -- either one or the other will be enabled at a time
        comparator_en        <= '0';
        ie_instr_req         <= '0';
        amo_load_skip        <= '0';
        amo_load             <= '0';
        load_op              <= '0';
        store_op             <= '0';
        signed_op            <= '0';
        if accl_en = 1 then
          spm_rs1          <= '0';
          spm_rs2          <= '0';
          vec_write_rd_ID  <= '0';
          vec_read_rs1_ID  <= '0';
          vec_read_rs2_ID  <= '0';
          vec_width_ID     <= "00";
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
            ie_instr_req <= '1';
            if zero_rd_wire = '0' then
              WB_EN_next_ID <= '1';
              case FUNCT3_wires is
                when ADDI =>            -- ADDI instruction
                  decoded_instruction_IE <= ADDI_pattern;
                when SLTI =>            -- SLTI instruction
                  decoded_instruction_IE <= SLTI_pattern;
                when SLTIU =>           -- SLTIU instruction
                  decoded_instruction_IE <= SLTIU_pattern;
                when ANDI =>            -- ANDI instruction
                  decoded_instruction_IE <= ANDI_pattern;
                when ORI =>             -- ORI instruction
                  decoded_instruction_IE <= ORI_pattern;
                when XORI =>            -- XORI instruction
                  decoded_instruction_IE <= XORI_pattern;
                when SLLI =>            -- SLLI instruction
                  decoded_instruction_IE <= SLLI_pattern;
                when SRLI_SRAI =>
                  case FUNCT7_wires is
                    when SRLI7 =>       -- SRLI instruction
                      decoded_instruction_IE <= SRLI7_pattern;
                    when SRAI7 =>       -- SRAI instruction
                      decoded_instruction_IE <= SRAI7_pattern;
                    when others =>  -- ILLEGAL_INSTRUCTION                                      
                      decoded_instruction_IE <= ILL_pattern;
                  end case;  -- FUNCT7_wires cases
                when others =>  -- ILLEGAL_INSTRUCTION                                  
                  decoded_instruction_IE <= ILL_pattern;
              end case;  -- FUNCT3_wires cases   
            else                -- R0_INSTRUCTION
              decoded_instruction_IE <= NOP_pattern;
            end if;  -- if rd(instr_word_ID) /=0
        
          when LUI =>                   -- LUI instruction
            ie_instr_req <= '1';
            if zero_rd_wire = '0' then
              WB_EN_next_ID <= '1';
              decoded_instruction_IE <= LUI_pattern;
            else                        -- R0_INSTRUCTION
              decoded_instruction_IE <= NOP_pattern;
            end if;
        
          when AUIPC =>                 -- AUIPC instruction
            ie_instr_req <= '1';
            if zero_rd_wire = '0' then
              WB_EN_next_ID <= '1';
              decoded_instruction_IE <= AUIPC_pattern;
            else                        -- R0_INSTRUCTION
              decoded_instruction_IE <= NOP_pattern;
            end if;

          when OP =>
            ie_instr_req <= '1';
            if zero_rd_wire = '0' then
              case FUNCT7_wires is
                when OP_I1 =>
                  WB_EN_next_ID <= '1';
                  case FUNCT3_wires is
                    when ADD => --ADD instruction
                      decoded_instruction_IE <= ADD7_pattern;
                    when SLT =>             -- SLT instruction 
                      comparator_en <= '1';
                      decoded_instruction_IE <= SLT_pattern;
                    when SLTU =>            -- SLTU instruction
                      comparator_en <= '1';
                      decoded_instruction_IE <= SLTU_pattern;
                    when ANDD =>            -- AND instruction
                      decoded_instruction_IE <= ANDD_pattern;
                    when ORR =>             -- OR instruction
                      decoded_instruction_IE <= ORR_pattern;
                    when XORR =>            -- XOR instruction        
                      decoded_instruction_IE <= XORR_pattern;
                    when SLLL =>            -- SLL instruction        
                      decoded_instruction_IE <= SLLL_pattern;
                    when SRLL =>       -- SRL instruction   
                      decoded_instruction_IE <= SRLL7_pattern;
                    when others =>  -- ILLEGAL_INSTRUCTION                                      
                      decoded_instruction_IE <= ILL_pattern;
                  end case;
                when OP_I2 =>
                  WB_EN_next_ID <= '1';
                  case FUNCT3_wires is
                    when SUB7 =>
                      decoded_instruction_IE <= SUB7_pattern;
                    when SRAA =>
                      decoded_instruction_IE <= SRAA7_pattern;
                    when others =>  -- ILLEGAL_INSTRUCTION                                      
                      decoded_instruction_IE <= ILL_pattern;
                  end case;
                when OP_M  =>                 -- MUL/DIV instructions
                  if RV32M = 1 then
                    case FUNCT3_wires is
                      when MUL =>
                        WB_EN_next_ID <= '1';
                        comparator_en <= '1';
                        decoded_instruction_IE <= MUL_pattern;
                      when MULH =>
                        comparator_en <= '1';
                        signed_op <= '1';
                        decoded_instruction_IE <= MULH_pattern;
                      when MULHSU =>
                        comparator_en <= '1';
                        signed_op <= '1';
                        decoded_instruction_IE <= MULHSU_pattern;
                      when MULHU =>
                        comparator_en <= '1';
                        decoded_instruction_IE <= MULHU_pattern;
                      when DIV =>
                        comparator_en <= '1';
                        signed_op <= '1';
                        decoded_instruction_IE <= DIV_pattern;
                      when DIVU =>
                        comparator_en <= '1';
                        decoded_instruction_IE <= DIVU_pattern;
                      when REMD =>
                        comparator_en <= '1';
                        signed_op <= '1';
                        decoded_instruction_IE <= REM_pattern;
                      when REMDU =>
                        comparator_en <= '1';
                        decoded_instruction_IE <= REMU_pattern;
                      when others =>
                        decoded_instruction_IE <= ILL_pattern;
                    end case;
                  else
                    decoded_instruction_IE <= ILL_pattern;                  
                  end if;
                when others =>  -- ILLEGAL_INSTRUCTION                                      
                  decoded_instruction_IE <= ILL_pattern;
              end case;
            else                        -- R0_INSTRUCTION
              decoded_instruction_IE <= NOP_pattern;
            end if;

          when JAL =>                   -- JAL instruction
            if zero_rd_wire = '0' then
              WB_EN_next_ID <= '1';
            end if;
            ie_instr_req <= '1';
            decoded_instruction_IE <= JAL_pattern;

          when JALR =>                  -- JALR instruction
            if zero_rd_wire = '0' then
              WB_EN_next_ID <= '1';
            end if;
            if rs1(instr_word_ID) /= 1 or bypass_rs1 = '1' or  data_dependency_JALR = '1' then
              jalr_flush(harc_ID) <= '1';
            end if;
            ie_instr_req <= '1';
            decoded_instruction_IE <= JALR_pattern;

          when BRANCH =>      -- BRANCH instruction         
            ie_instr_req <= '1';
            case FUNCT3_wires is
              when BEQ =>               -- BEQ instruction   
                comparator_en <= '1';
                decoded_instruction_IE <= BEQ_pattern;
              when BNE =>               -- BNE instruction
                comparator_en <= '1';
                decoded_instruction_IE <= BNE_pattern;
              when BLT =>               -- BLT instruction   
                comparator_en <= '1';
                decoded_instruction_IE <= BLT_pattern;
              when BLTU =>              -- BLTU instruction
                comparator_en <= '1';
                decoded_instruction_IE <= BLTU_pattern;
              when BGE =>               -- BGE instruction
                comparator_en <= '1';
                decoded_instruction_IE <= BGE_pattern;
              when BGEU =>              -- BGEU instruction
                comparator_en <= '1';
                decoded_instruction_IE <= BGEU_pattern;
              when others =>  -- ILLEGAL_INSTRUCTION                      
                decoded_instruction_IE <= ILL_pattern;
            end case;  -- FUNCT3_wires cases

          when LOAD =>                  -- LOAD instruction
            load_op <= '1';
            if zero_rd_wire = '0' then  -- is all in the next_state process
              case FUNCT3_wires is
                when LW =>
                  ls_instr_req <= '1';
                  data_width_ID <= "10";
                  data_be_ID   <= "1111";
                  decoded_instruction_LS <= LW_pattern;
                when LH =>
                  ls_instr_req <= '1';
                  data_width_ID <= "01";
                  data_be_ID <= "0011";
                  decoded_instruction_LS <= LH_pattern;
                when LHU =>
                  ls_instr_req <= '1';
                  data_width_ID <= "01";
                  data_be_ID <= "0011";
                  decoded_instruction_LS <= LHU_pattern;
                when LB =>
                  ls_instr_req <= '1';
                  data_width_ID <= "00";
                  data_be_ID <= "0001";
                  decoded_instruction_LS <= LB_pattern;
                when LBU =>
                  ls_instr_req <= '1';
                  data_width_ID <= "00";
                  data_be_ID <= "0001";
                  decoded_instruction_LS <= LBU_pattern;
                when others =>          -- ILLEGAL_INSTRUCTION
                  ie_instr_req <= '1';
                  decoded_instruction_IE <= ILL_pattern;
              end case;
            else                        -- R0_INSTRUCTION
              ie_instr_req <= '1';
              decoded_instruction_IE <= NOP_pattern;
            end if;

          when STORE =>                 -- STORE instruction
            store_op <= '1';
            case FUNCT3_wires is
              when SW =>                -- is all in the next_state process
                ls_instr_req <= '1';
                ie_instr_req <= '1';
                data_width_ID <= "10";
                data_be_ID <= "1111";
                decoded_instruction_LS <= SW_pattern;
                decoded_instruction_IE <= SW_MIP_pattern;
              when SH =>
                ls_instr_req <= '1';
                data_width_ID <= "01";
                data_be_ID <= "0011";
                decoded_instruction_LS <= SH_pattern;
              when SB =>
                ls_instr_req <= '1';
                data_width_ID <= "00";
                data_be_ID <= "0001";
                decoded_instruction_LS <= SB_pattern;
              when others =>  -- ILLEGAL_INSTRUCTION
                ie_instr_req <= '1';
                decoded_instruction_IE <= ILL_pattern;
            end case;

          when MISC_MEM =>
            ie_instr_req <= '1';
            case FUNCT3_wires is
              when FENCE =>             -- FENCE instruction
                decoded_instruction_IE <= FENCE_pattern;
              when FENCEI =>            -- FENCEI instruction
                decoded_instruction_IE <= FENCEI_pattern;
              when others =>            -- ILLEGAL_INSTRUCTION
                decoded_instruction_IE <= ILL_pattern;
            end case;  -- FUNCT3_wires cases

          when SYSTEM =>
            ie_instr_req <= '1';
            case FUNCT3_wires is
              when PRIV =>
                if (rs1(instr_word_ID) = 0 and zero_rd_wire = '1') then
                  case FUNCT12_wires is
                    when ECALL =>       -- ECALL instruction
                      flush_decode(harc_ID) <= '1';  -- flushes only the same hart op
                      decoded_instruction_IE <= ECALL_pattern;
                    when EBREAK =>      -- EBREAK instruction       
                      decoded_instruction_IE <= EBREAK_pattern;
                    when mret =>        -- mret instruction
                      flush_decode(harc_ID) <= '1';  -- flushes only the same hart op
                      decoded_instruction_IE <= MRET_pattern;
                    when WFI =>         -- WFI instruction
                      if MSTATUS(harc_ID)(0) = '1' then -- only flush if the wfi is enabled
                        flush_decode(harc_ID) <= '1';   -- flushes only the same hart op
                      end if;
                      decoded_instruction_IE <= WFI_pattern;
                    when others =>  -- ILLEGAL_INSTRUCTION                                              
                      decoded_instruction_IE <= ILL_pattern;
                  end case;  -- FUNCT12_wires cases
                else  -- ILLEGAL_INSTRUCTION
                  decoded_instruction_IE <= ILL_pattern;
                end if;
              when CSRRW =>
                decoded_instruction_IE <= CSRRW_pattern;
              when CSRRS =>
                decoded_instruction_IE <= CSRRS_pattern;
              when CSRRC =>
                decoded_instruction_IE <= CSRRC_pattern;
              when CSRRWI =>
                decoded_instruction_IE <= CSRRWI_pattern;
              when CSRRSI =>
                decoded_instruction_IE <= CSRRSI_pattern;
              when CSRRCI =>
                decoded_instruction_IE <= CSRRCI_pattern;
              when others =>  -- ILLEGAL_INSTRUCTION
                decoded_instruction_IE <= ILL_pattern;
            end case;  -- FUNCT3_wires cases

          when AMO =>
            data_width_ID <= "10";
            case FUNCT3_wires is
              when SINGLE =>
                ls_instr_req <= '1';
                decoded_instruction_LS <= AMOSWAP_pattern;
                if zero_rd_wire = '0' then
                  amo_load_skip          <= '0';
                  if amo_store = '1' then
                    amo_load <= '0';
                  elsif amo_store = '0' then
                    amo_load <= '1';
                  end if;
                else
                  amo_load_skip <= '1';
                end if;
              when others =>            -- ILLEGAL_INSTRUCTION
                ie_instr_req <= '1';
                decoded_instruction_IE <= ILL_pattern;
            end case;

          when LOAD_F =>
            if RV32F = 1 then
              load_op <= '1';
              data_width_ID <= "10";
              data_be_ID   <= "1111";
              ls_instr_req <= '1';
              decoded_instruction_LS <= FLW_pattern;
            end if;

          when STORE_F =>
            if RV32F = 1 then
              store_op <= '1';
              data_width_ID <= "10";
              data_be_ID <= "1111";
              ls_instr_req <= '1';
              decoded_instruction_LS <= FSW_pattern;
            end if;

          when FLOAT =>
            if RV32F = 1 then
              case FUNCT7_wires is
                when FADD =>  -- FADD INSTRUCTION
                  float_instr_req <= '1';
                  decoded_instruction_FLOAT <= FADD_pattern;
                when FSUB =>  -- FSUB INSTRUCTION
                  float_instr_req <= '1';
                  decoded_instruction_FLOAT <= FSUB_pattern;
                when FMUL =>  -- FMUL INSTRUCTION
                  float_instr_req <= '1';
                  decoded_instruction_FLOAT <= FMUL_pattern;
                when FDIV =>  -- FDIV INSTRUCTION
                  float_instr_req <= '1';
                  decoded_instruction_FLOAT <= FDIV_pattern;
                when others =>            -- ILLEGAL_INSTRUCTION
                  ie_instr_req <= '1';
                  decoded_instruction_IE <= ILL_pattern;
              end case;
            end if;

          when KMEM =>
            if accl_en = 1 then
              case FUNCT7_wires is
                when KMEMLD =>          -- KMEMLD_INSTRUCTION
                  ls_instr_req <= '1';
                  decoded_instruction_LS <= KMEMLD_pattern;
                when KMEMSTR =>         -- KMEMSTR_INSTRUCTION
                  ls_instr_req <= '1';
                  decoded_instruction_LS <= KMEMSTR_pattern;
                when KBCASTLD =>         -- KBCASTLD_INSTRUCTION
                  ls_instr_req <= '1';
                  decoded_instruction_LS <= KBCASTLD_pattern;
                when others =>            -- ILLEGAL_INSTRUCTION
                  ie_instr_req <= '1';
                  decoded_instruction_IE <= ILL_pattern;
              end case;
            end if;

          when KDSP =>
            if accl_en = 1 then
              if busy_DSP(harc_ID_to_DSP) = '0' then
                case FUNCT7_wires is
                  when KADDV =>           -- KADDV_INSTRUCTION
                    vec_write_rd_ID <= '1';
                    vec_read_rs1_ID <= '1';
                    vec_read_rs2_ID <= '1';
                    spm_rs1 <= '1';
                    spm_rs2 <= '1';
                    decoded_instruction_DSP <= KADDV_pattern;
                  when KSUBV =>           -- KSUBV_INSTRUCTION
                    vec_write_rd_ID <= '1';
                    vec_read_rs1_ID <= '1';
                    vec_read_rs2_ID <= '1';
                    spm_rs1 <= '1';
                    spm_rs2 <= '1';
                    decoded_instruction_DSP <= KSUBV_pattern;
                  when KVMUL =>           -- KVMUL_INSTRUCTION
                    vec_write_rd_ID <= '1';
                    vec_read_rs1_ID <= '1';
                    vec_read_rs2_ID <= '1';
                    spm_rs1 <= '1';
                    spm_rs2 <= '1';
                    decoded_instruction_DSP <= KVMUL_pattern;
                  when KVDIV =>           -- KVDIV_INSTRUCTION
                    vec_write_rd_ID <= '1';
                    vec_read_rs1_ID <= '1';
                    vec_read_rs2_ID <= '1';
                    spm_rs1 <= '1';
                    spm_rs2 <= '1';
                    decoded_instruction_DSP <= KVDIV_pattern;
                  when KVRED =>           -- KVRED_INSTRUCTION
                    vec_read_rs1_ID <= '1';
                    spm_rs1 <= '1';
                    decoded_instruction_DSP <= KVRED_pattern;
                  when KDOTP =>           -- KDOTP_INSTRUCTION
                    vec_read_rs1_ID <= '1';
                    vec_read_rs2_ID <= '1';
                    spm_rs1 <= '1';
                    spm_rs2 <= '1';
                    decoded_instruction_DSP <= KDOTP_pattern;
                  when KDOTPPS =>           -- KDOTPPS_INSTRUCTION
                    vec_read_rs1_ID <= '1';
                    vec_read_rs2_ID <= '1';
                    spm_rs1 <= '1';
                    spm_rs2 <= '1';
                    decoded_instruction_DSP <= KDOTPPS_pattern;
                  when KSVADDSC =>           -- KSVADDSC_INSTRUCTION
                    vec_read_rs1_ID <= '1';
                    vec_write_rd_ID  <= '1';
                    spm_rs1 <= '1';
                    spm_rs2 <= '1';
                    decoded_instruction_DSP <= KSVADDSC_pattern;
                  when KSVADDRF =>           -- KSVADDRF_INSTRUCTION
                    vec_read_rs1_ID <= '1';
                    vec_write_rd_ID <= '1';
                    spm_rs1 <= '1';
                    decoded_instruction_DSP <= KSVADDRF_pattern;
                  when KSVMULSC =>           -- KSVMULSC_INSTRUCTION
                    vec_read_rs1_ID <= '1';
                    vec_write_rd_ID  <= '1';
                    spm_rs1 <= '1';
                    spm_rs2 <= '1';
                    decoded_instruction_DSP <= KSVMULSC_pattern;
                  when KSVMULRF =>           -- KSVMULRF_INSTRUCTION
                    vec_read_rs1_ID <= '1';
                    vec_write_rd_ID <= '1';
                    spm_rs1 <= '1';
                    decoded_instruction_DSP <= KSVMULRF_pattern;
                  when KSRAV =>           -- KSRAV_INSTRUCTION
                    vec_read_rs1_ID <= '1';
                    vec_write_rd_ID  <= '1';
                    spm_rs1 <= '1';
                    decoded_instruction_DSP <= KSRAV_pattern;
                  when KSRLV =>           -- KSRLV_INSTRUCTION
                    vec_read_rs1_ID <= '1';
                    vec_write_rd_ID <= '1';
                    spm_rs1 <= '1';
                    decoded_instruction_DSP <= KSRLV_pattern;
                  when KRELU =>           -- KRELU_INSTRUCTION
                    vec_read_rs1_ID <= '1';
                    vec_write_rd_ID <= '1';
                    spm_rs1 <= '1';
                    decoded_instruction_DSP <= KRELU_pattern;
                  when KVSLT =>
                    vec_write_rd_ID <= '1';
                    vec_read_rs1_ID <= '1';
                    vec_read_rs2_ID <= '1';
                    spm_rs1 <= '1';
                    spm_rs2 <= '1';
                    decoded_instruction_DSP <= KVSLT_pattern;
                  when KSVSLT =>
                    vec_write_rd_ID <= '1';
                    vec_read_rs1_ID <= '1';
                    spm_rs1 <= '1';
                    decoded_instruction_DSP <= KSVSLT_pattern;
                  when KBCAST =>           -- KBCAST_INSTRUCTION
                    vec_write_rd_ID <= '1';
                    decoded_instruction_DSP <= KBCAST_pattern;
                  when KVCP =>           -- KVCP_INSTRUCTION
                    spm_rs1 <= '1';
                    vec_read_rs1_ID  <= '1';
                    vec_write_rd_ID  <= '1';
                    decoded_instruction_DSP <= KVCP_pattern;
                  when others =>            -- ILLEGAL_INSTRUCTION
                    ie_instr_req <= '1';
                    decoded_instruction_IE <= ILL_pattern;
                end case;
              else
                ie_instr_req <= '1';
                decoded_instruction_IE <= JAL_pattern;
              end if;
            end if;

          when others =>                -- ILLEGAL_INSTRUCTION
            ie_instr_req <= '1';
            decoded_instruction_IE <= ILL_pattern;

        end case;  -- OPCODE_wires cases                           
        -- Decode OF INSTRUCTION (END) --------------------------

      end if;  -- instr. conditions
    end if;  -- clk
  end process;

  Dependency_handling_en : if morph_en = 1 generate

  Dependency_handling_en : if fetch_stage_en = 0 generate

  branch_predict_taken_IE_int <= '0';

  branch_handler_comb : process(all)  -- comb process
    variable OPCODE_wires  : std_logic_vector (6 downto 0);
  begin
    OPCODE_wires                 := OPCODE(instr_word_ID);
    set_branch_condition_ID      <= '0';
    Immediate                    <= B_immediate(instr_word_ID); -- defualts to B_Immediate unless we have a JAL
    PC_offset_ID                 <= std_logic_vector(unsigned(pc_ID)+unsigned(Immediate));
    branch_stall                 <= '0';
    jalr_stall                   <= '0';
    branch_predict_taken_IE_wire <= '0';
    if instr_rvalid_ID_int    = '1' and 
       flush_hart_ID(harc_ID) = '0' and
       flush_decode(harc_ID)  = '0' and
       served_irq(harc_ID)    = '0' and
       CORE_STATE(IMT_MODE)   = '0' then
      case OPCODE_wires is

        when JAL =>         -- JAL instruction
          if CORE_STATE(SINGLE_HART_MODE) = '1' then
            set_branch_condition_ID <= '1';
            Immediate               <= UJ_immediate(instr_word_ID);
          end if;

        when JALR =>       -- JALR instruction
          if CORE_STATE(SINGLE_HART_MODE) = '1' then
            if absolute_jump(harc_ID) = '0' then
              jalr_stall <= '1';
            end if;
          end if;

        when BRANCH =>    -- BRANCH instruction
          if branch_predict_en = 0 then
            if branch_instr = '0' then
              branch_stall <= '1';
            end if;
          elsif CORE_STATE(SINGLE_HART_MODE) = '0' then -- means we have 0 threads or 1 thread sleeping
            null;
          else
            if instr_word_ID(31) = '1' then -- branch prediction taken
              set_branch_condition_ID   <= '1';
              branch_predict_taken_IE_wire <= '1';
            end if;
          end if;
    
        when others =>      -- ILLEGAL_INSTRUCTION
          null;

      end case;  -- OPCODE_wires cases                           
    end if;
        -- Decode OF INSTRUCTION (END) --------------------------
  end process;

  data_dependency_handler  : process(all)
    variable OPCODE_wires  : std_logic_vector (6 downto 0);
  begin
    OPCODE_wires                := OPCODE(instr_word_ID);
    rs1_valid_int               <= '0';
    rs2_valid_int               <= '0';
    rd_valid_int                <= '0';
    rd_read_valid_int           <= '0';
    if instr_rvalid_ID_int    = '1' and 
       flush_hart_ID(harc_ID) = '0' and
       flush_decode(harc_ID)  = '0' and
--       served_irq(harc_ID)             = '0' and
       CORE_STATE(IMT_MODE) = '0' then
  
      case OPCODE_wires is

        when OP_IMM =>      -- OP_IMM instruction
          rs1_valid_int <= '1';
          rd_valid_int  <= '1';

        when LUI =>         -- LUI instruction
          rd_valid_int  <= '1';

        when AUIPC =>       -- AUIPC instruction
          rd_valid_int  <= '1';

        when OP =>          -- OP instruction
          rs1_valid_int <= '1';
          rs2_valid_int <= '1';
          rd_valid_int  <= '1';

        when JAL =>         -- JAL instruction
          rd_valid_int  <= '1';

        when JALR =>       -- JALR instruction
          rs1_valid_int <= '1';
          rd_valid_int  <= '1';

        when BRANCH =>      -- BRANCH instruction         
          rs1_valid_int  <= '1';
          rs2_valid_int  <= '1';

        when LOAD =>
          rs1_valid_int <= '1';
          rd_valid_int  <= '1';

        when STORE =>
          rs1_valid_int <= '1';
          rs2_valid_int  <= '1';

        when SYSTEM =>
          rs1_valid_int <= '1';
          rd_valid_int  <= '1';

        when AMO =>
          rs1_valid_int <= '1';
          rs2_valid_int <= '1';
          rd_valid_int  <= '1';

        when KMEM =>
          rs1_valid_int <= '1';
          rs2_valid_int <= '1';
          rd_read_valid_int  <= '1';

        when KDSP =>
          rs1_valid_int <= '1';
          rs2_valid_int <= '1';
          rd_read_valid_int  <= '1';
    
        when others =>                -- ILLEGAL_INSTRUCTION
          null;

      end case;  -- OPCODE_wires cases                           
    end if;
        -- Decode OF INSTRUCTION (END) --------------------------
  end process;

  end generate; -- fetch_stage_en = 0

  valid_buf_control_sync : process (clk_i, rst_ni)
  begin
    if rst_ni = '0' then
      valid_buf     <= (others => (others => '1'));
      --if fetch_stage_en = 0 then
        valid_buf_lat <= (others => (others => '1'));
      --end if;
      --block_valid_wb <= '0';
      rf_rs1_valid     <= (others => '1');
      rf_rs2_valid     <= (others => '1');
      rf_rd_read_valid <= (others => '1');
      data_dependency_JALR <= '0';
    elsif rising_edge(clk_i) then
      if fetch_stage_en = 1 then
        if instr_rvalid_ID_int = '1' then
            if valid_buf(harc_FETCH)(rs1(instr_word_FETCH))     = '0' or   -- if the valid_bit of the operand to be read is '0', then JALR read an incorrect return address
               valid_buf_lat(harc_FETCH)(rs1(instr_word_FETCH)) = '0' then -- even if the valid_bit_lat was '0', JALR still didnt read a correct value.
              data_dependency_JALR <= '1';
            elsif busy_ID = '0' then
              data_dependency_JALR <= '0';
            end if;
          --end if;
        end if;
      end if;
      valid_buf     <= valid_buf_wire;
      --if fetch_stage_en = 0 then
        valid_buf_lat <= valid_buf;
      --elsif fetch_stage_en = 1 then
      --  for h in harc_range loop
      --    rf_rs1_valid(h)     <= valid_buf(h)(rs1(instr_word_ID)) when busy_ID = '1' else valid_buf(h)(rs1(instr_word_FETCH));
      --    rf_rs2_valid(h)     <= valid_buf(h)(rs2(instr_word_ID)) when busy_ID = '1' else valid_buf(h)(rs2(instr_word_FETCH));
      --    rf_rd_read_valid(h) <= valid_buf(h)(rd(instr_word_ID))  when busy_ID = '1' else valid_buf(h)(rd(instr_word_FETCH));
      --  end loop;
      --end if;
      --block_valid_wb <= '0';
      --if instr_rvalid_ID_int = '1' and instr_rvalid_IE = '1' then
      --  if harc_ID = harc_EXEC then
      --    if ( LS_WB_EN_wire = '1' or IE_WB_EN_wire = '1' or MUL_WB_EN_wire = '1' ) and rd_valid = '1' and 
      --       ( rd(instr_word_ID) = rd(instr_word_IE))  and not 
      --       ( rd(instr_word_ID) = rs1(instr_word_ID) and rs1_valid = '1') and not
      --       ( rd(instr_word_ID) = rs2(instr_word_ID) and rs2_valid = '1') then
      --      block_valid_wb <= '1';
      --    end if;
      --  end if;
      --end if;
      if instr_rvalid_ID_int = '1' then
        if flush_hart_ID(harc_ID) = '0' and flush_decode(harc_ID) = '0' and served_irq(harc_ID) = '0' then
          if rd_valid = '1' and busy_ID = '0' then
            if zero_rd_wire = '0' then
              valid_buf(harc_ID)(rd(instr_word_ID)) <= '0';
            end if;
          end if;
        end if;
      end if;
      if served_irq(harc_EXEC) = '1' or set_except_condition = '1' then
        valid_buf(harc_EXEC) <= (others => '1');
      end if;
    end if; -- clk
  end process;

  valid_buf_control_comb : process (all)
  begin
    valid_buf_wire <= valid_buf;
    --for h in harc_range loop
    --  rf_rs1_valid(h)     <= valid_buf_lat(h)(rs1(instr_word_ID));
    --  rf_rs2_valid(h)     <= valid_buf_lat(h)(rs2(instr_word_ID));
    --  rf_rd_read_valid(h) <= valid_buf_lat(h)(rd(instr_word_ID));
    --end loop;
    --if (IE_WB_EN = '1' or MUL_WB_EN = '1') and block_valid_wb = '0' then
    --  valid_buf_wire(harc_WB)(rd(instr_word_IE_WB)) <= '1';
    --end if;
    --if LS_WB_EN = '1' and block_valid_wb = '0' then
    --  valid_buf_wire(harc_WB)(rd(instr_word_LS_WB)) <= '1';
    --end if;
    if (IE_WB_EN_wire = '1' or MUL_WB_EN_wire = '1') and block_valid_wb = '0' then
      valid_buf_wire(harc_EXEC)(rd(instr_word_IE)) <= '1';
    end if;
    if LS_WB_EN_wire = '1' and block_valid_wb = '0' then
      valid_buf_wire(harc_EXEC)(rd(instr_word_IE)) <= '1';
    end if;
    block_valid_wb <= '0';
    if instr_rvalid_ID_int = '1' and instr_rvalid_IE = '1' then
      if harc_ID = harc_EXEC then
        if ( LS_WB_EN_wire = '1' or IE_WB_EN_wire = '1' or MUL_WB_EN_wire = '1' ) and rd_valid = '1' and 
           ( rd(instr_word_ID) = rd(instr_word_IE))  and not 
           ( rd(instr_word_ID) = rs1(instr_word_ID) and rs1_valid = '1') and not
           ( rd(instr_word_ID) = rs2(instr_word_ID) and rs2_valid = '1') then
          block_valid_wb <= '1';
        end if;
      end if;
    end if;
  end process;


  data_dep_checker_comb : process(all)
  begin
    data_dependency   <= '0';
    bypass_rs1        <= '0';
    bypass_rs2        <= '0';
    bypass_rd_read    <= '0';
    if RV32F = 1 then
      bypass_fp_rs1     <= '0';
      bypass_fp_rs2     <= '0';
      bypass_fp_rd_read <= '0';
    end if;
    -- AAA add the floating point data_dependency register
    if instr_rvalid_ID_int = '1' and flush_decode(harc_ID) = '0' then
      if valid_buf(harc_ID)(rs1(instr_word_ID)) = '0' and rs1_valid = '1' then
        data_dependency <= '1';
      end if; 
      if valid_buf(harc_ID)(rs2(instr_word_ID)) = '0' and rs2_valid = '1' then
        data_dependency <= '1';
      end if;
      if accl_en = 1 then
        if valid_buf(harc_ID)(rd(instr_word_ID)) = '0' and rd_read_valid = '1' then
          data_dependency <= '1';
        end if;
      end if;
      --if fetch_stage_en = 0 then
        if valid_buf_lat(harc_ID)(rs1(instr_word_ID)) = '0' and rs1_valid = '1' then
          bypass_rs1 <= '1';
        end if; 
        if valid_buf_lat(harc_ID)(rs2(instr_word_ID)) = '0' and rs2_valid = '1' then
          bypass_rs2 <= '1';
        end if;
        if accl_en = 1 then
          if valid_buf_lat(harc_ID)(rd(instr_word_ID)) = '0' and rd_read_valid = '1' then
            bypass_rd_read <= '1';
          end if;
        end if;
      --elsif fetch_stage_en = 1 then
      --  if rf_rs1_valid(harc_ID) = '0' and rs1_valid = '1' then
      --    bypass_rs1 <= '1';
      --  end if; 
      --  if rf_rs2_valid(harc_ID) = '0' and rs2_valid = '1' then
      --    bypass_rs2 <= '1';
      --  end if;
      --  if accl_en = 1 then
      --    if rf_rd_read_valid(harc_ID) = '0' and rd_read_valid = '1' then
      --      bypass_rd_read <= '1';
      --    end if;
      --  end if;
      --end if;
    end if; 
  end process;

  end generate; -- morph_en = 1

  -- set these internal signals to zero since the logic for them will be handled in the fetch stage
  fetch_stg_en : if fetch_stage_en = 1 generate

    branch_handler_comb : process(all)  -- comb process
      variable OPCODE_wires  : std_logic_vector (6 downto 0);
    begin
      OPCODE_wires                 := OPCODE(instr_word_ID);
      set_branch_condition_ID      <= '0';
      branch_predict_taken_IE_wire <= '0';
      Immediate                    <= B_immediate(instr_word_ID); -- defualts to B_Immediate unless we have a JAL
      PC_offset_ID                 <= std_logic_vector(unsigned(pc_ID)+unsigned(Immediate));
      branch_predict_taken_IE_int  <= branch_predict_taken_IE_int_lat;
      if instr_rvalid_ID = '1' then
        branch_predict_taken_IE_int <= branch_predict_taken_ID;
      end if;
      if instr_rvalid_ID_int    = '1' and 
        --flush_hart_ID(harc_ID) = '0' and
        flush_decode(harc_ID)  = '0' and
        --served_irq(harc_ID)             = '0' and
        CORE_STATE(IMT_MODE) = '0' then

        if decoded_branching_instr(JAL_FETCH_instr) = '1' then
          if CORE_STATE_ID(DUAL_HART_MODE) = '1' then
            set_branch_condition_ID <= '1';
            Immediate               <= UJ_immediate(instr_word_ID);
          end if;
        end if;

        if decoded_branching_instr(BRANCH_FETCH_instr) = '1' then
          if branch_predict_en = 0 then
            null;
          elsif btb_en = 0 then
            if unsigned(harc_sleep) = 1 then -- AAA this is wrong and should use hart_sleep_count instead -- means we have 0 threads or 1 thread sleeping
            --if CORE_STATE(DUAL_HART_MODE) = '1' then -- means we have 1 thread sleeping
              if instr_word_ID(31) = '1' then -- branch prediction taken
                set_branch_condition_ID      <= '1';
                branch_predict_taken_IE_wire <= '1';
              end if;
            end if;
          end if;
        end if;
                 
      end if;
          -- Decode OF INSTRUCTION (END) --------------------------
    end process;

    process(clk_i, rst_ni)
    begin
      if rst_ni = '0' then
        branch_predict_taken_IE_int_lat <= '0';
      elsif rising_edge(clk_i) then
        branch_predict_taken_IE_int_lat  <= branch_predict_taken_IE_int;
      end if;
    end process;

    branch_stall            <= '0';
    jalr_stall              <= '0';

  end generate; -- fetch_stage_en = 1

  Dependency_handling_dis : if morph_en = 0 generate
    data_dependency  <= '0';
    bypass_rs1       <= '0';
    bypass_rs2       <= '0';
    bypass_rd_read   <= '0';
    branch_stall     <= '0';
    jalr_stall       <= '0';
  end generate;

  instr_rvalid_ID_int <= instr_rvalid_ID or instr_rvalid_ID_int_lat when instr_rvalid_IE = '0' else instr_rvalid_ID;

  process(clk_i, rst_ni)
  begin
    if rst_ni = '0' then
      instr_rvalid_ID_int_lat <= '0';
    elsif rising_edge(clk_i) then
      if core_busy_LS      = '0' and 
         core_busy_IE      = '0' and 
         ls_parallel_exec  = '1' and 
         fpu_parallel_exec = '1' and 
         dsp_parallel_exec = '1' and 
         (data_dependency = '0' or (data_dependency = '1' and flush_hart_ID(harc_ID) = '1')) then
        instr_rvalid_ID_int_lat <= '0';
      elsif instr_rvalid_ID = '1' then -- else latch the internal instruction to maintain its state
        instr_rvalid_ID_int_lat <= '1';
      end if;
    end if;
  end process;


---------------------------------------------------------------------------------------------------------------------------------------------------------------
--  ███████╗██╗   ██╗██████╗ ███████╗██████╗ ███████╗ ██████╗ █████╗ ██╗      █████╗ ██████╗     ███████╗███╗   ██╗ █████╗ ██████╗ ██╗     ███████╗██████╗   --
--  ██╔════╝██║   ██║██╔══██╗██╔════╝██╔══██╗██╔════╝██╔════╝██╔══██╗██║     ██╔══██╗██╔══██╗    ██╔════╝████╗  ██║██╔══██╗██╔══██╗██║     ██╔════╝██╔══██╗  --
--  ███████╗██║   ██║██████╔╝█████╗  ██████╔╝███████╗██║     ███████║██║     ███████║██████╔╝    █████╗  ██╔██╗ ██║███████║██████╔╝██║     █████╗  ██████╔╝  --
--  ╚════██║██║   ██║██╔═══╝ ██╔══╝  ██╔══██╗╚════██║██║     ██╔══██║██║     ██╔══██║██╔══██╗    ██╔══╝  ██║╚██╗██║██╔══██║██╔══██╗██║     ██╔══╝  ██╔══██╗  --
--  ███████║╚██████╔╝██║     ███████╗██║  ██║███████║╚██████╗██║  ██║███████╗██║  ██║██║  ██║    ███████╗██║ ╚████║██║  ██║██████╔╝███████╗███████╗██║  ██║  --
--  ╚══════╝ ╚═════╝ ╚═╝     ╚══════╝╚═╝  ╚═╝╚══════╝ ╚═════╝╚═╝  ╚═╝╚══════╝╚═╝  ╚═╝╚═╝  ╚═╝    ╚══════╝╚═╝  ╚═══╝╚═╝  ╚═╝╚═════╝ ╚══════╝╚══════╝╚═╝  ╚═╝  --
---------------------------------------------------------------------------------------------------------------------------------------------------------------

  harc_ID_to_DSP <= 0 when replicate_accl_en = 0 else harc_ID;

  Superscalar_Enable : if superscalar_exec_en = 1 generate
  fsm_ID_comb : process(all)
  variable OPCODE_wires  : std_logic_vector (6 downto 0);
  begin
    OPCODE_wires  := OPCODE(instr_word_ID); 
    -- parallelism enablers, halts the pipeline when it is zero. -------------------
    ls_parallel_exec  <= '0' when (OPCODE_wires = LOAD or OPCODE_wires = STORE or OPCODE_wires = AMO or ((OPCODE_wires = LOAD_F or OPCODE_wires = STORE_F) and RV32F = 1) or OPCODE_wires = KMEM) and busy_LS = '1' and instr_rvalid_ID_int = '1' else '1';
    fpu_parallel_exec <= '0' when RV32F = 1 and (OPCODE_wires = FLOAT and busy_FPU = '1') else '1';
    dsp_parallel_exec <= '0' when (OPCODE_wires = KMEM or OPCODE_wires = AMO) and busy_DSP(harc_ID_to_DSP) = '1' and instr_rvalid_ID_int = '1' else '1';
    dsp_to_jump_wire  <= '1' when OPCODE_wires = KDSP and busy_DSP(harc_ID_to_DSP) = '1' else '0';
    busy_ID <= '0';  -- wait for a valid instruction or process the instruction 
    -- A data deoendency is only valid to make a stall when the current dependent instruction is not flushed 
    if core_busy_IE = '1' or core_busy_LS = '1' or ls_parallel_exec = '0'  or fpu_parallel_exec = '0' or dsp_parallel_exec = '0'  or (data_dependency = '1' and flush_hart_ID(harc_ID) = '0') or branch_stall = '1' or jalr_stall = '1' then
      busy_ID <= '1';  -- wait for the stall to finish, block new instructions
    end if; 
  end process;
  end generate;

  Superscalar_Disable: if superscalar_exec_en = 0 generate
  fsm_ID_comb : process(all)
  variable OPCODE_wires  : std_logic_vector (6 downto 0);
  begin
    OPCODE_wires      := OPCODE(instr_word_ID); 
    busy_ID           <= '0';
    ls_parallel_exec  <= '0' when busy_LS = '1' and instr_rvalid_ID_int = '1' else '1';
    fpu_parallel_exec <= '0' when RV32F = 1 and busy_FPU = '1' else '1';
    dsp_parallel_exec <= '0' when or_vect_bits(busy_DSP) = '1' and instr_rvalid_ID_int = '1' else '1';
    dsp_to_jump_wire  <= '1' when OPCODE_wires = KDSP and busy_DSP(harc_ID_to_DSP) = '1' else '0';
    -- A data deoendency is only valid to make a stall when the current dependent instruction is not flushed
    if core_busy_IE = '1' or core_busy_LS = '1' or ls_parallel_exec = '0'  or fpu_parallel_exec = '0' or dsp_parallel_exec = '0' or (data_dependency = '1' and flush_hart_ID(harc_ID) = '0') or branch_stall = '1' or jalr_stall = '1' then
      busy_ID <= '1';  -- wait for the stall to finish, block new instructions 
    end if; 
  end process;
  end generate;


  dsp_instr_req_wire <= (others => '0') when not (instr_rvalid_ID_int = '1'     and 
                                                  busy_ID = '0'                 and 
                                                  OPCODE(instr_word_ID) = KDSP  and 
                                                  dsp_to_jump_wire = '0'        and 
                                                  flush_hart_ID(harc_ID) = '0') else (harc_ID_to_DSP => '1', others => '0');

  --process(all)
  --begin
  --  dsp_instr_req_wire <= (others => '0');
  --  if instr_rvalid_ID_int = '1' and busy_ID = '0' then
  --    if OPCODE(instr_word_ID) = KDSP then
  --      if dsp_to_jump_wire = '0' then
  --        if flush_hart_ID(harc_ID) = '0' then
  --          dsp_instr_req_wire(harc_ID_to_DSP) <=  '1';
  --        end if;
  --      end if;
  --    end if;
  --  end if;
  --end process; --------------------------------------------------------------------------------

  process(clk_i, rst_ni)
  begin
    if rst_ni = '0' then
      dsp_to_jump  <= (others => '0');
    elsif rising_edge(clk_i) then
      dsp_to_jump(harc_ID)   <= dsp_to_jump_wire;
      dsp_instr_req <= dsp_instr_req_wire;
    end if;
  end process;

---------------------------------------------------------------------- end of ID stage -----------
--------------------------------------------------------------------------------------------------
end DECODE;
--------------------------------------------------------------------------------------------------
-- END of ID architecture ------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------