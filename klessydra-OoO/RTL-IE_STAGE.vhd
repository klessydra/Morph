--------------------------------------------------------------------------------------------------------------
--  stage IE -- (Instruction Execute)                                                                       --
--  Author(s): Abdallah Cheikh abdallah.cheikh@uniroma1.it (abdallah93.as@gmail.com)                        --
--                                                                                                          --
--  Date Modified: 01-03-2020                                                                               --
--------------------------------------------------------------------------------------------------------------
--  This stage is composed of an fsm unit fsm_IE that executes the incoming operations,                     --
--  Priveleged and CSR instructions are also executed here, load-store and custom instructions are not      --
--  The multipliers are a up to a three cycle latency instructions, while the dividers are up to 32 cycles  --
--  and drives the control signals for accessing data memory and stalling the pipeline if needed            --
--  fsm_IE may invoke separate units for handling specific instructions (exceptions, csrs)                  --
--------------------------------------------------------------------------------------------------------------

-- ieee packages ------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_misc.all;
use ieee.numeric_std.all;
use std.textio.all;

-- local packages ------------
use work.riscv_klessydra.all;

-- pipeline  pinout --------------------
entity IE_STAGE is
  generic(
    RF_CEIL                : natural
  );
  port (
	-- clock, and reset active low
    clk_i, rst_ni          : in  std_logic;
    fetch_enable_i         : in  std_logic;
    irq_i                  : in  std_logic;
    IE_instr               : in  std_logic;
    pc_IE                  : in  std_logic_vector(31 downto 0);
    instr_word_IE          : in  std_logic_vector(31 downto 0);
    ie_data_rs1            : in  std_logic_vector(31 downto 0);
    ie_data_rs2            : in  std_logic_vector(31 downto 0);
    ie_addr_rs1            : in  std_logic_vector(RF_CEIL-1 downto 0);
    ie_addr_rs2            : in  std_logic_vector(RF_CEIL-1 downto 0);
    ie_addr_new_rd         : in  std_logic_vector(RF_CEIL-1 downto 0);
    irq_pending            : in  std_logic;
    csr_instr_done         : in  std_logic;
    csr_access_denied_o    : in  std_logic;
    csr_rdata_o            : in  std_logic_vector(31 downto 0);
    data_addr_internal_IE  : in  std_logic_vector(31 downto 0);
    dbg_req_o              : in  std_logic;
    MSTATUS                : in  std_logic_vector(1 downto 0);
    instr_rvalid_IE        : in  std_logic;  -- validity bit at IE input
    taken_branch           : in  std_logic;
    comparator_en          : in  std_logic;
    decoded_instruction_IE : in  std_logic_vector(EXEC_UNIT_INSTR_SET_SIZE-1 downto 0);
    csr_addr_i             : out std_logic_vector(11 downto 0);
    ie_except_data         : out std_logic_vector(31 downto 0);
    ie_csr_wdata_i         : out std_logic_vector(31 downto 0);
    csr_op_i               : out std_logic_vector(2 downto 0);
    csr_wdata_en           : out std_logic;
    csr_instr_req          : out std_logic;
    busy_IE                : out std_logic;
    jump_instr             : out std_logic;
    jump_instr_lat         : out std_logic;
    WFI_Instr              : out std_logic;
    sleep_state            : out std_logic;
    set_branch_condition   : out std_logic;
    IE_except_condition    : out std_logic;
    set_mret_condition     : out std_logic;
    set_wfi_condition      : out std_logic;
    ie_taken_branch        : out std_logic;
    branch_instr           : out std_logic;
    branch_instr_lat       : out std_logic;
    PC_offset              : out std_logic_vector(31 downto 0);
    served_irq     	       : out std_logic;
    dbg_ack_i              : out std_logic;
    ebreak_instr           : out std_logic;
    absolute_jump          : out std_logic;
    IE_WB_EN               : out std_logic;
    IE_WB                  : out std_logic_vector(31 downto 0);
    IE_WB_RD_ADDR          : out std_logic_vector(RF_CEIL-1 downto 0);
    pc_WB                  : out std_logic_vector(31 downto 0);
    state_IE               : out fsm_IE_states;
    -- Branch Predictor Problems
    branch_prediction      : in  std_logic;
    branch_hit             : out std_logic;
    branch_miss            : out std_logic
	   );
end entity;  ------------------------------------------


-- Klessydra T03x (4 stages) pipeline implementation -----------------------
architecture EXECUTE of IE_STAGE is

  signal zero_rs1                   : std_logic;
  signal zero_rs2                   : std_logic;
  signal pass_BEQ                   : std_logic;
  signal pass_BNE                   : std_logic;
  signal pass_BLT                   : std_logic;
  signal pass_BLTU                  : std_logic;
  signal pass_BGE                   : std_logic;
  signal pass_BGEU                  : std_logic;

  signal nextstate_IE               : fsm_IE_states;

  signal add_op_A                   : std_logic_vector(31 downto 0);
  signal add_op_B                   : std_logic_vector(31 downto 0);
  signal sr_op_A                    : std_logic_vector(31 downto 0); 
  signal sr_op_B                    : std_logic_vector(4  downto 0);
  signal sl_op_A                    : std_logic_vector(31 downto 0);
  signal sl_op_B                    : std_logic_vector(4  downto 0);
  signal logic_op_A                 : std_logic_Vector(31 downto 0);
  signal logic_op_B                 : std_logic_Vector(31 downto 0);

  -- signals for counting intructions
  signal clock_cycle         : std_logic_vector(63 downto 0);   -- RDCYCLE
  signal external_counter    : std_logic_vector(63 downto 0);   -- RDTIME
  --signal instruction_counter : std_logic_vector(63 downto 0); -- RDINSTRET

begin

  ----------------------------------------------------------
  --  ██╗███████╗    ███████╗██╗   ██╗███╗   ██╗ ██████╗  --
  --  ██║██╔════╝    ██╔════╝╚██╗ ██╔╝████╗  ██║██╔════╝  --
  --  ██║█████╗      ███████╗ ╚████╔╝ ██╔██╗ ██║██║       --
  --  ██║██╔══╝      ╚════██║  ╚██╔╝  ██║╚██╗██║██║       --
  --  ██║███████╗    ███████║   ██║   ██║ ╚████║╚██████╗  --
  --  ╚═╝╚══════╝    ╚══════╝   ╚═╝   ╚═╝  ╚═══╝ ╚═════╝  --
  ----------------------------------------------------------

  fsm_IE_sync : process(clk_i, rst_ni)
  begin
    if rst_ni = '0' then
      IE_WB                  <= std_logic_vector(to_unsigned(0, 32));
      IE_WB_EN               <= '0';
      --instruction_counter    <= std_logic_vector(to_unsigned(0, 64));
      csr_instr_req          <= '0';
      csr_wdata_en           <= '0';
      csr_op_i               <= (others => '0');
      ie_except_data         <= (others => '0');
      ie_csr_wdata_i         <= (others => '0');
      csr_addr_i             <= (others => '0');
    elsif rising_edge(clk_i) then
		
      IE_WB_RD_ADDR <= ie_addr_new_rd;

      csr_instr_req <= '0';

      case state_IE is  -- stage state
        when sleep =>
          null;
        when reset =>
          null;
        when debug =>
          null;
        when normal =>
          -- check if there is a valid instruction and the thread it belongs to is not in a delay slot: 
          if  IE_INSTR = '0' then
            IE_WB_EN  <= '0';

            -- in a generic version we would have conditions on busy_WB 
            -- in all states of the IE stage, and similarly in the comb process, just
            -- like we did in the ID stage.
          elsif irq_pending = '1' then
            -- manage irq as an absolute branch to MTVEC, also defining mepc value properly in program counter unit
            -- irq is served only if we are not in a delay slot
            -- the current valid instruction is discarded, only its pc value gets used for mepc
            IE_WB_EN       <= '0';
          else
            --instruction_counter <= std_logic_vector(unsigned(instruction_counter)+1);  -- AAA should be updated or removed since the exec stage has been split
            pc_WB               <= pc_ie;
            csr_wdata_en        <= '0';
            -- misaligned_err      <= '0';

            -- EXECUTE OF INSTRUCTION -------------------------------------------

             IE_WB_EN <= '0';

            -------------------------- ADDER ------------------------------
            if decoded_instruction_IE(ADDI_bit_position)  = '1' or
               decoded_instruction_IE(ADD7_bit_position)  = '1' or
               decoded_instruction_IE(SUB7_bit_position)  = '1' or
               decoded_instruction_IE(AUIPC_bit_position) = '1' or
               decoded_instruction_IE(JAL_bit_position)   = '1' or 
               decoded_instruction_IE(JALR_bit_position)  = '1' then
              if (unsigned(ie_addr_new_rd) /= 0) then  -- this condition is only for JAL and JALR which still execute even when "rd = x0"
                IE_WB_EN <= '1';
              end if;
              IE_WB <= std_logic_vector(signed(add_op_A)+signed(add_op_B));
            end if;
            ---------------------------------------------------------------

            -----------------------  SHIFTERS -----------------------------
            if decoded_instruction_IE(SLLI_bit_position) = '1' or
               decoded_instruction_IE(SLLL_bit_position) = '1' then
              IE_WB_EN <= '1';
              IE_WB <= to_stdlogicvector(to_bitvector(sl_op_A) sll to_integer(unsigned(sl_op_B)));
            end if;
            if decoded_instruction_IE(SRLI7_bit_position) = '1' or
               decoded_instruction_IE(SRLL7_bit_position) = '1' then
              IE_WB_EN <= '1';
              IE_WB <= to_stdlogicvector(to_bitvector(sr_op_A) srl to_integer(unsigned(sr_op_B)));
            end if;
            if decoded_instruction_IE(SRAI7_bit_position) = '1' or
               decoded_instruction_IE(SRAA7_bit_position) = '1' then
              IE_WB_EN <= '1';
              IE_WB <= to_stdlogicvector(to_bitvector(sr_op_A) sra to_integer(unsigned(sr_op_B)));
            end if;
            --------------------------------------------------------------

            -------------------- LOGIC UNITS -----------------------------
            if decoded_instruction_IE(ANDI_bit_position) = '1' or
               decoded_instruction_IE(ANDD_bit_position) = '1' then
              IE_WB_EN <= '1';
              IE_WB <= logic_op_A and logic_op_B;
            end if;

            if decoded_instruction_IE(ORI_bit_position) = '1' or
               decoded_instruction_IE(ORR_bit_position) = '1' then
              IE_WB_EN <= '1';
              IE_WB <= logic_op_A or logic_op_B;
            end if;

            if decoded_instruction_IE(XORI_bit_position) = '1' or
               decoded_instruction_IE(XORR_bit_position) = '1' then
              IE_WB_EN <= '1';
              IE_WB <= logic_op_A xor logic_op_B;
            end if;
            --------------------------------------------------------------


            if decoded_instruction_IE(SLTI_bit_position) = '1' then
              if (signed(ie_data_rs1) < signed (I_immediate(instr_word_IE))) then
                IE_WB_EN       <= '1';
                IE_WB <= std_logic_vector(to_unsigned(1, 32));
              else
                IE_WB_EN       <= '1';
                IE_WB <= std_logic_vector(to_unsigned(0, 32));
              end if;
            end if;

            if decoded_instruction_IE(SLTIU_bit_position) = '1' then
              if (unsigned(ie_data_rs1) < unsigned (I_immediate(instr_word_IE))) then
                IE_WB_EN       <= '1';
                IE_WB <= std_logic_vector(to_unsigned(1, 32));
              else
                IE_WB_EN       <= '1';
                IE_WB <= std_logic_vector(to_unsigned(0, 32));
              end if;
            end if;

            if decoded_instruction_IE(LUI_bit_position) = '1' then
              IE_WB_EN <= '1';
              IE_WB <= U_immediate(instr_word_IE);
            end if;

            if decoded_instruction_IE(SLT_bit_position) = '1' then
              IE_WB_EN <= '1';
              if pass_BLT = '1' then
                IE_WB <= std_logic_vector(to_unsigned(1, 32));
              else
                IE_WB <= std_logic_vector(to_unsigned(0, 32));
              end if;
            end if;

            if decoded_instruction_IE(SLTU_bit_position) = '1' then
              IE_WB_EN <= '1';
              if pass_BLTU = '1' then
                IE_WB <= std_logic_vector(to_unsigned(1, 32));
              else
                IE_WB <= std_logic_vector(to_unsigned(0, 32));
              end if;
            end if;

            if decoded_instruction_IE(ECALL_bit_position) = '1' then
              ie_except_data                <= ECALL_EXCEPT_CODE;
              csr_wdata_en                  <= '1';
            end if;
            -----------------------------------------------------------

            --------------------- CSR OPS ---------------------------
            if decoded_instruction_IE(CSRRC_bit_position) = '1' or 
               decoded_instruction_IE(CSRRS_bit_position) = '1' or 
               decoded_instruction_IE(CSRRW_bit_position) = '1' then
              csr_op_i      <= FUNCT3(instr_word_IE);
              csr_instr_req <= '1';
              ie_csr_wdata_i <= ie_data_rs1;
              csr_wdata_en   <= '1';
              csr_addr_i     <= std_logic_vector(to_unsigned(to_integer(unsigned(CSR_ADDR(instr_word_IE))), 12));
            end if;

            if decoded_instruction_IE(CSRRSI_bit_position) = '1' or 
               decoded_instruction_IE(CSRRCI_bit_position) = '1' or 
               decoded_instruction_IE(CSRRWI_bit_position) = '1' then
              csr_op_i       <= FUNCT3(instr_word_IE);
              csr_instr_req  <= '1';
              ie_csr_wdata_i <= (RF_CEIL to 31 => '0') & ie_addr_rs1;
              csr_wdata_en   <= '1';
              csr_addr_i     <= std_logic_vector(to_unsigned(to_integer(unsigned(CSR_ADDR(instr_word_IE))), 12));
            end if;
            -------------------------------------------------------

            if decoded_instruction_IE(ILL_bit_position) = '1' then
              ie_except_data                       <= ILLEGAL_INSN_EXCEPT_CODE;
              csr_wdata_en                         <= '1';
            end if;

          -- EXECUTE OF INSTRUCTION (END) --------------------------
          end if;  -- instr_rvalid_IE values
          
        when csr_instr_wait_state =>
          csr_instr_req <= '0';
          if (csr_instr_done = '1' and csr_access_denied_o = '0') then
            if (unsigned(ie_addr_new_rd) /= 0) then
              IE_WB_EN <= '1';
              IE_WB <= csr_rdata_o;
            else
              IE_WB_EN <= '0';
            end if;
          elsif (csr_instr_done = '1' and csr_access_denied_o = '1') then  -- ILLEGAL_INSTRUCTION
            IE_WB_EN                             <= '0';
            csr_wdata_en                         <= '1';
            ie_except_data                       <= ILLEGAL_INSN_EXCEPT_CODE;
          else
            IE_WB_EN <= '0'; -- do nothing and wait
          end if;
      end case;  -- fsm_IE state cases
    end if;  -- reset, clk_i
  end process;

  -----------------------------------------------------------
  --  ██╗███████╗     ██████╗ ██████╗ ███╗   ███╗██████╗   --
  --  ██║██╔════╝    ██╔════╝██╔═══██╗████╗ ████║██╔══██╗  --
  --  ██║█████╗      ██║     ██║   ██║██╔████╔██║██████╔╝  --
  --  ██║██╔══╝      ██║     ██║   ██║██║╚██╔╝██║██╔══██╗  --
  --  ██║███████╗    ╚██████╗╚██████╔╝██║ ╚═╝ ██║██████╔╝  --
  --  ╚═╝╚══════╝     ╚═════╝ ╚═════╝ ╚═╝     ╚═╝╚═════╝   --
  -----------------------------------------------------------

  fsm_IE_comb : process(all)

    variable PC_offset_wires                  : std_logic_vector(31 downto 0);
    variable absolute_jump_wires              : std_logic;
    variable busy_IE_wires                    : std_logic;
    variable IE_except_condition_wires        : std_logic;
    variable set_branch_condition_wires       : std_logic;
    variable ie_taken_branch_wires            : std_logic;
    variable set_mret_condition_wires         : std_logic;
    variable set_wfi_condition_wires          : std_logic;
    variable jump_instr_wires                 : std_logic;
    variable branch_instr_wires               : std_logic;
    variable ebreak_instr_wires               : std_logic;
    variable dbg_ack_i_wires                  : std_logic;
    variable WFI_Instr_wires		              : std_logic;
    variable served_irq_wires                 : std_logic;
    variable nextstate_IE_wires               : fsm_IE_states;

  begin
    PC_offset_wires                  := (others => '0');
    nextstate_IE_wires               := normal;
    served_irq_wires		             := '0';
    absolute_jump_wires              := '0';
    busy_IE_wires                    := '0';
    IE_except_condition_wires        := '0';
    set_branch_condition_wires       := '0';
    set_wfi_condition_wires          := '0';    
    ie_taken_branch_wires            := '0';
    set_mret_condition_wires         := '0';
    jump_instr_wires                 := '0';
    branch_instr_wires               := '0';
    ebreak_instr_wires               := '0';
    dbg_ack_i_wires                  := '0';
    WFI_Instr_wires                  := '0';
    sleep_state                      <= '0';

    branch_miss                      <= '0';
    branch_hit                       <= '0';

    case state_IE is                  -- stage status
      when sleep =>
        if dbg_req_o = '1' then
          dbg_ack_i_wires    := '1';
          busy_IE_wires := '1';
          nextstate_IE_wires := sleep;
          sleep_state  <= '1';
        elsif irq_i = '1' or fetch_enable_i = '1' then
          nextstate_IE_wires := normal;
        else
          busy_IE_wires := '1';
          nextstate_IE_wires := sleep;
        end if;

      when debug =>
        dbg_ack_i_wires := '1';
        if dbg_req_o = '0' then
          nextstate_IE_wires := normal;
        else
          nextstate_IE_wires := debug;
          busy_IE_wires := '1';
        end if;

      when normal =>

        if IE_INSTR = '0' then
         -- does nothing and wait
        elsif irq_pending= '1' then
          -- manage irq as an absolute branch to MTVEC, also defining mepc value properly in program counter unit
          -- irq is served only if we are not in a delay slot
          -- the current valid instruction is discarded, only its pc value gets used for mepc
          served_irq_wires := '1';
          ie_taken_branch_wires     := '1';
          if decoded_instruction_IE(WFI_bit_position) = '1' then -- Inform the CSR unit that the last instruction before we went to the subroutine was a WFI instruction.
            WFI_Instr_wires := '1';
          end if;
        else                         -- process the instruction
 
        -- EXECUTE OF INSTRUCTION ---------------------

          if decoded_instruction_IE(JAL_bit_position) = '1' then  -- JAL instruction
            jump_instr_wires                     := '1';
            --set_branch_condition_wires           := '1';    -- moved to the decode stage
            --ie_taken_branch_wires                := '1';    -- moved to the decode stage
            --PC_offset_wires := UJ_immediate(instr_word_IE); -- moved to the decode stage
          end if;

          if decoded_instruction_IE(JALR_bit_position) = '1' then  --JALR instruction
            set_branch_condition_wires := '1';
            ie_taken_branch_wires      := '1';
            PC_offset_wires := std_logic_vector(signed(ie_data_rs1)
                                                                     + signed(I_immediate(instr_word_IE)))
                                                    and X"FFFFFFFE";  -- bitwise and to set '0' the LSB 
            jump_instr_wires    := '1';
            absolute_jump_wires := '1';
          end if;

          if decoded_instruction_IE(BEQ_bit_position) = '1' then
            branch_instr_wires := '1';
            if branch_prediction /= pass_BEQ then
              branch_miss <= '1';
              set_branch_condition_wires := '1';
              ie_taken_branch_wires      := '1';
              if pass_BEQ = '1' then
                PC_offset_wires := B_immediate(instr_word_IE);
              else
                PC_offset_wires := (others => '0');
              end if;
            else
              branch_hit <= '1';    
            end if;
          end if;

          if decoded_instruction_IE(BNE_bit_position) = '1' then
            branch_instr_wires := '1';
            if branch_prediction /= pass_BNE then
              branch_miss <= '1';
              set_branch_condition_wires := '1';
              ie_taken_branch_wires      := '1';
              if pass_BNE = '1' then
                PC_offset_wires := B_immediate(instr_word_IE);
              else
                PC_offset_wires := (others => '0');
              end if;
            else
              branch_hit <= '1';    
            end if;
          end if;

          if decoded_instruction_IE(BLT_bit_position) = '1' then
            branch_instr_wires := '1';
            if branch_prediction /= pass_BLT then
              branch_miss <= '1';
              set_branch_condition_wires := '1';
              ie_taken_branch_wires      := '1';
              if pass_BLT = '1' then
                PC_offset_wires := B_immediate(instr_word_IE);
              else
                PC_offset_wires := (others => '0');
              end if;
            else
              branch_hit <= '1';    
            end if;
          end if;

          if decoded_instruction_IE(BLTU_bit_position) = '1' then
            branch_instr_wires := '1';
            if branch_prediction /= pass_BLTU then
              branch_miss <= '1';
              set_branch_condition_wires := '1';
              ie_taken_branch_wires      := '1';
              if pass_BLTU = '1' then
                PC_offset_wires := B_immediate(instr_word_IE);
              else
                PC_offset_wires := (others => '0');
              end if;
            else
              branch_hit <= '1';    
            end if;
          end if;

          if decoded_instruction_IE(BGE_bit_position) = '1' then
            branch_instr_wires := '1';
            if branch_prediction /= pass_BGE then
              branch_miss <= '1';
              set_branch_condition_wires := '1';
              ie_taken_branch_wires      := '1';
              if pass_BGE = '1' then
                PC_offset_wires := B_immediate(instr_word_IE);
              else
                PC_offset_wires := (others => '0');
              end if;
            else
              branch_hit <= '1';    
            end if;
          end if;

          if decoded_instruction_IE(BGEU_bit_position) = '1' then
            branch_instr_wires := '1';
            if branch_prediction /= pass_BGEU then
              branch_miss <= '1';
              set_branch_condition_wires := '1';
              ie_taken_branch_wires      := '1';
              if pass_BGEU = '1' then
                PC_offset_wires := B_immediate(instr_word_IE);
              else
                PC_offset_wires := (others => '0');
              end if;
            else
              branch_hit <= '1';    
            end if;
          end if;

          --if decoded_instruction_IE(BEQ_bit_position) = '1' then
          --  branch_instr_wires := '1';
          --  PC_offset_wires := B_immediate(instr_word_IE);
          --  if pass_BEQ = '1' then
          --    set_branch_condition_wires := '1';
          --    ie_taken_branch_wires      := '1';
          --  end if;
          --end if;

          --if decoded_instruction_IE(BNE_bit_position) = '1' then
          --  branch_instr_wires := '1';
          --  PC_offset_wires := B_immediate(instr_word_IE);
          --  if pass_BNE = '1' then
          --    set_branch_condition_wires := '1';
          --    ie_taken_branch_wires      := '1';
          --  end if;
          --end if;

          --if decoded_instruction_IE(BLT_bit_position) = '1' then
          --  branch_instr_wires := '1';
          --  PC_offset_wires := B_immediate(instr_word_IE);
          --  if pass_BLT = '1' then
          --    set_branch_condition_wires := '1';
          --    ie_taken_branch_wires      := '1';
          --  end if;
          --end if;

          --if decoded_instruction_IE(BLTU_bit_position) = '1' then
          --  branch_instr_wires := '1';
          --  PC_offset_wires := B_immediate(instr_word_IE);
          --  if pass_BLTU = '1' then
          --    set_branch_condition_wires := '1';
          --    ie_taken_branch_wires      := '1';
          --  end if;
          --end if;

          --if decoded_instruction_IE(BGE_bit_position) = '1' then
          --  branch_instr_wires := '1';
          --  PC_offset_wires := B_immediate(instr_word_IE);
          --  if pass_BGE = '1' then
          --    set_branch_condition_wires := '1';
          --    ie_taken_branch_wires      := '1';
          --  end if;
          --end if;

          --if decoded_instruction_IE(BGEU_bit_position) = '1' then
          --  branch_instr_wires := '1';
          --  PC_offset_wires := B_immediate(instr_word_IE);
          --  if pass_BGEU = '1' then
          --    set_branch_condition_wires := '1';
          --    ie_taken_branch_wires      := '1';
          --  end if;
          --end if;

          if decoded_instruction_IE(SW_MIP_bit_position) = '1' then
            if data_addr_internal_IE(31 downto 8) = x"0000FF" then
              busy_IE_wires := '1';
              nextstate_IE_wires := csr_instr_wait_state;
            end if;
          end if;

          if decoded_instruction_IE(CSRRW_bit_position) = '1' or decoded_instruction_IE(CSRRWI_bit_position) = '1' or
             decoded_instruction_IE(CSRRC_bit_position) = '1' or decoded_instruction_IE(CSRRCI_bit_position) = '1' or
             decoded_instruction_IE(CSRRS_bit_position) = '1' or decoded_instruction_IE(CSRRSI_bit_position) = '1' then
            busy_IE_wires := '1';
            nextstate_IE_wires := csr_instr_wait_state;
          end if;

          if decoded_instruction_IE(ECALL_bit_position) = '1' then
            IE_except_condition_wires := '1';
            ie_taken_branch_wires     := '1';
          end if;

          if decoded_instruction_IE(EBREAK_bit_position) = '1' then
            ebreak_instr_wires := '1';
          end if;

          if decoded_instruction_IE(MRET_bit_position) = '1' then
            set_mret_condition_wires := '1';
            ie_taken_branch_wires    := '1';
            if fetch_enable_i = '0' then
              nextstate_IE_wires := sleep;
              busy_IE_wires      := '1';
            end if;
          end if;

          if decoded_instruction_IE(WFI_bit_position) = '1' then
            if MSTATUS(0) = '1' then
              set_wfi_condition_wires  := '1';
              ie_taken_branch_wires    := '1';
            end if;
          end if;

          if decoded_instruction_IE(ILL_bit_position) = '1' then  -- ILLEGAL_INSTRUCTION
            IE_except_condition_wires := '1';
            ie_taken_branch_wires     := '1';
          end if;

          if dbg_req_o = '1' then
            nextstate_IE_wires := debug;
            dbg_ack_i_wires    := '1';
            busy_IE_wires := '1';
          end if;

        -- EXECUTE OF INSTRUCTION (END)
        end if;  -- instr_rvalid_IE values 

      when csr_instr_wait_state =>
        if csr_instr_done = '0' then
          nextstate_IE_wires := csr_instr_wait_state;
          busy_IE_wires := '1';
        elsif (csr_instr_done = '1' and csr_access_denied_o = '1') then  -- ILLEGAL_INSTRUCTION
          nextstate_IE_wires        := normal;
          IE_except_condition_wires := '1';
          ie_taken_branch_wires     := '1';
        else
          nextstate_IE_wires := normal;
        end if;

      when others =>
        null;

    end case;  -- fsm_IE state cases

    PC_offset                  <= PC_offset_wires;
    absolute_jump              <= absolute_jump_wires;
    busy_IE                    <= busy_IE_wires;
    IE_except_condition        <= IE_except_condition_wires;
    set_branch_condition       <= set_branch_condition_wires;
    served_irq                 <= served_irq_wires;
    ie_taken_branch            <= ie_taken_branch_wires;
    set_mret_condition         <= set_mret_condition_wires;    
    set_wfi_condition          <= set_wfi_condition_wires;
    jump_instr                 <= jump_instr_wires;
    branch_instr               <= branch_instr_wires;
    ebreak_instr               <= ebreak_instr_wires;
    dbg_ack_i                  <= dbg_ack_i_wires;
    nextstate_IE               <= nextstate_IE_wires;
    WFI_Instr                  <= WFI_Instr_wires;
  end process;

  fsm_IE_state : process(clk_i, rst_ni) -- also implements the delay slot counters and some aux signals
  begin
    
    if rst_ni = '0' then
      branch_instr_lat <= '0'; 
      jump_instr_lat   <= '0';
      state_IE         <= sleep;
    elsif rising_edge(clk_i) then
      branch_instr_lat       <= branch_instr;
      jump_instr_lat         <= jump_instr;
      state_IE               <= nextstate_IE;
    end if;
  end process;

  --------------------------------------------------------------------------------
  --  ███████╗██╗   ██╗    ███╗   ███╗ █████╗ ██████╗ ██████╗ ███████╗██████╗   --
  --  ██╔════╝██║   ██║    ████╗ ████║██╔══██╗██╔══██╗██╔══██╗██╔════╝██╔══██╗  --
  --  █████╗  ██║   ██║    ██╔████╔██║███████║██████╔╝██████╔╝█████╗  ██████╔╝  --
  --  ██╔══╝  ██║   ██║    ██║╚██╔╝██║██╔══██║██╔═══╝ ██╔═══╝ ██╔══╝  ██╔══██╗  --
  --  ██║     ╚██████╔╝    ██║ ╚═╝ ██║██║  ██║██║     ██║     ███████╗██║  ██║  --
  --  ╚═╝      ╚═════╝     ╚═╝     ╚═╝╚═╝  ╚═╝╚═╝     ╚═╝     ╚══════╝╚═╝  ╚═╝  --
  --------------------------------------------------------------------------------

  IE_Mapper_comb : process(all)
  begin
    add_op_A   <= (others => '0');
    add_op_B   <= (others => '0');
    sl_op_A    <= (others => '0');
    sl_op_B    <= (others => '0');
    sr_op_A    <= (others => '0');
    sr_op_B    <= (others => '0');
    logic_op_A <= (others => '0');
    logic_op_B <= (others => '0');

    if decoded_instruction_IE(ADDI_bit_position) = '1' then
      add_op_A <= ie_data_rs1;
      add_op_B <= I_immediate(instr_word_IE);
    end if;
    if decoded_instruction_IE(ADD7_bit_position) = '1' then
      add_op_A <= ie_data_rs1;
      add_op_B <= ie_data_rs2;
    end if;
    if decoded_instruction_IE(SUB7_bit_position) = '1' then
      add_op_A <= ie_data_rs1;
      add_op_B <= std_logic_vector(unsigned(not(ie_data_rs2))+1);
    end if;
    if decoded_instruction_IE(AUIPC_bit_position) = '1' then
      add_op_A <= pc_ie;
      add_op_B <= U_immediate(instr_word_IE);
    end if;
    if decoded_instruction_IE(JAL_bit_position) = '1' or decoded_instruction_IE(JALR_bit_position) = '1' then
      add_op_A <= pc_ie;
      add_op_B <= (3 to 31 => '0') & "100";
    end if;

    if decoded_instruction_IE(SLLI_bit_position) = '1' then
      sl_op_A <= ie_data_rs1;
      sl_op_B <= SHAMT(instr_word_IE);
    end if;
    if decoded_instruction_IE(SRLI7_bit_position) = '1' then
      sr_op_A <= ie_data_rs1;
      sr_op_B <= SHAMT(instr_word_IE);
    end if;
    if decoded_instruction_IE(SRAI7_bit_position) = '1' then
      sr_op_A <= ie_data_rs1;
      sr_op_B <= SHAMT(instr_word_IE);
    end if;
    if decoded_instruction_IE(SLLL_bit_position) = '1' then
      sl_op_A <= ie_data_rs1;
      sl_op_B <= ie_data_rs2(4 downto 0);
    end if;
    if decoded_instruction_IE(SRLL7_bit_position) = '1' then
      sr_op_A <= ie_data_rs1;
      sr_op_B <= ie_data_rs2(4 downto 0);
    end if;
    if decoded_instruction_IE(SRAA7_bit_position) = '1' then
      sr_op_A <= ie_data_rs1;
      sr_op_B <= ie_data_rs2(4 downto 0);
    end if;

    if decoded_instruction_IE(ANDI_bit_position) = '1' or
       decoded_instruction_IE(ORI_bit_position)  = '1' or
       decoded_instruction_IE(XORI_bit_position) = '1' then
      logic_op_A <= ie_data_rs1;
      logic_op_B <= I_immediate(instr_word_IE);
    end if;
    if decoded_instruction_IE(ANDD_bit_position) = '1' or
       decoded_instruction_IE(ORR_bit_position)  = '1' or
       decoded_instruction_IE(XORR_bit_position) = '1' then
      logic_op_A <= ie_data_rs1;
      logic_op_B <= ie_data_rs2;
    end if;
  end process;


  ------------------------------------------------------------------------------------------------------
  --   ██████╗ ██████╗ ███╗   ███╗██████╗  █████╗ ██████╗  █████╗ ████████╗ ██████╗ ██████╗ ███████╗  --
  --  ██╔════╝██╔═══██╗████╗ ████║██╔══██╗██╔══██╗██╔══██╗██╔══██╗╚══██╔══╝██╔═══██╗██╔══██╗██╔════╝  --
  --  ██║     ██║   ██║██╔████╔██║██████╔╝███████║██████╔╝███████║   ██║   ██║   ██║██████╔╝███████╗  --
  --  ██║     ██║   ██║██║╚██╔╝██║██╔═══╝ ██╔══██║██╔══██╗██╔══██║   ██║   ██║   ██║██╔══██╗╚════██║  --
  --  ╚██████╗╚██████╔╝██║ ╚═╝ ██║██║     ██║  ██║██║  ██║██║  ██║   ██║   ╚██████╔╝██║  ██║███████║  --
  --   ╚═════╝ ╚═════╝ ╚═╝     ╚═╝╚═╝     ╚═╝  ╚═╝╚═╝  ╚═╝╚═╝  ╚═╝   ╚═╝    ╚═════╝ ╚═╝  ╚═╝╚══════╝  --
  ------------------------------------------------------------------------------------------------------

  comparator_enable_comb : process(all)
  begin
    pass_BEQ  <= '0';
    pass_BNE  <= '0';
    pass_BLT  <= '0';
    pass_BGE  <= '0';
    pass_BLTU <= '0';
    pass_BGEU <= '0';
    if comparator_en = '1' then
      if (signed(ie_data_rs1) = signed(ie_data_rs2)) then
        pass_BEQ <= '1';
      else
        pass_BNE <= '1';
      end if;
      if (signed(ie_data_rs1) < signed(ie_data_rs2)) then
        pass_BLT <= '1';
      else
        pass_BGE <= '1';
      end if;
      if (unsigned(ie_data_rs1) < unsigned(ie_data_rs2)) then
        pass_BLTU <= '1';
      else
        pass_BGEU <= '1';
      end if;
    end if;
  end process;

-------------------------------------------------------------------end of IE stage ---------------
--------------------------------------------------------------------------------------------------

end EXECUTE;
--------------------------------------------------------------------------------------------------
-- END of IE architecture ------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------