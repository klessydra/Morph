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
--use work.klessydra_parameters.all;

-- pipeline  pinout --------------------
entity IE_STAGE is
  generic(
    THREAD_POOL_SIZE          : natural;
    morph_en                  : natural;
    fetch_stage_en            : natural;
    branch_predict_en         : natural;
    RV32M                     : natural;
    TPS_CEIL                  : natural;
    RF_CEIL                   : natural
  );
  port (
  -- clock, and reset active low
    clk_i, rst_ni             : in  std_logic;
    instr_gnt_i               : in  std_logic;
    irq_i                     : in  std_logic;
    RS1_Data_IE               : in  std_logic_vector(31 downto 0);
    RS2_Data_IE               : in  std_logic_vector(31 downto 0);
    irq_pending               : in  std_logic_vector(THREAD_POOL_SIZE-1 downto 0);
    fetch_enable_i            : in  std_logic;
    csr_instr_done            : in  std_logic;
    csr_access_denied_o       : in  std_logic;
    csr_rdata_o               : in  std_logic_vector(31 downto 0);
    pc_IE                     : in  std_logic_vector(31 downto 0);
    instr_word_IE             : in  std_logic_vector(31 downto 0);
    data_addr_internal_IE     : in  std_logic_vector(31 downto 0);
    comparator_en             : in  std_logic;
    zero_rd                   : in  std_logic;
    signed_op                 : in  std_logic;
    zero_rs1                  : in  std_logic;
    zero_rs2                  : in  std_logic;
    pass_BEQ                  : in  std_logic;
    pass_BNE                  : in  std_logic;
    pass_BLT                  : in  std_logic;
    pass_BLTU                 : in  std_logic;
    pass_BGE                  : in  std_logic;
    pass_BGEU                 : in  std_logic;
    ie_instr_req              : in  std_logic;
    MSTATUS                   : in  array_2D(THREAD_POOL_SIZE-1 downto 0)(1 downto 0);
    harc_EXEC                 : in  integer range THREAD_POOL_SIZE-1 downto 0;
    instr_rvalid_IE           : in  std_logic;  -- validity bit at IE input
    WB_EN_next_ID             : in  std_logic;
    taken_branch              : in  std_logic;
    halt_IE                   : in  std_logic;
    decoded_instruction_IE    : in  std_logic_vector(EXEC_UNIT_INSTR_SET_SIZE-1 downto 0);
    harc_sleep_wire           : in  std_logic_vector(THREAD_POOL_SIZE-1 downto 0);
    harc_sleep                : in  std_logic_vector(THREAD_POOL_SIZE-1 downto 0);
    hart_sleep_count_ID       : in  std_logic_vector(TPS_CEIL-1 downto 0);
    csr_addr_i                : out std_logic_vector(11 downto 0);
    ie_except_data            : out std_logic_vector(31 downto 0);
    ie_csr_wdata_i            : out std_logic_vector(31 downto 0);
    csr_op_i                  : out std_logic_vector(2 downto 0);
    harc_to_csr               : out natural range THREAD_POOL_SIZE-1 downto 0;
    csr_instr_req             : out std_logic;
    core_busy_IE              : out std_logic;
    jump_instr                : out std_logic;
    jump_instr_lat            : out std_logic;
    WFI_Instr                 : out std_logic;
    sleep_state               : out std_logic;
    set_branch_condition      : out std_logic;
    IE_except_condition       : out std_logic;
    set_mret_condition        : out std_logic;
    set_wfi_condition         : out std_logic;
    ie_taken_branch           : out std_logic;
    branch_instr              : out std_logic;
    branch_instr_lat          : out std_logic;
    PC_offset                 : out std_logic_vector(31 downto 0);
    absolute_address          : out std_logic_vector(31 downto 0);
    served_irq                : out std_logic_vector(THREAD_POOL_SIZE-1 downto 0);
    ebreak_instr              : out std_logic;
    sys_instr                 : out std_logic_vector(THREAD_POOL_SIZE-1 downto 0);
    absolute_jump             : out std_logic_vector(THREAD_POOL_SIZE-1 downto 0);
    instr_rvalid_WB           : out std_logic;
    instr_word_IE_WB          : out std_logic_vector (31 downto 0);
    IE_WB_EN                  : out std_logic;
    IE_WB_EN_wire             : out std_logic;
    IE_WB                     : out std_logic_vector(31 downto 0);
    MUL_WB_EN                 : out std_logic;
    MUL_WB_EN_wire            : out std_logic;
    MUL_WB                    : out std_logic_vector(31 downto 0);
    harc_IE_WB                : out natural range THREAD_POOL_SIZE-1 downto 0;
    pc_WB                     : out std_logic_vector(31 downto 0);
    state_IE                  : out fsm_IE_states;
    -- branch prediction
    dsp_to_jump               : in  std_logic_vector(THREAD_POOL_SIZE-1 downto 0);
    jalr_flush                : in  std_logic_vector(THREAD_POOL_SIZE-1 downto 0);
    IE_flush_hart_FETCH       : out std_logic_vector(THREAD_POOL_SIZE-1 downto 0);
    IE_flush_hart_ID          : out std_logic_vector(THREAD_POOL_SIZE-1 downto 0);
    halt_update_IE            : out std_logic_vector(THREAD_POOL_SIZE-1 downto 0);
    branch_taken              : out std_logic;
    branch_predict_taken_IE   : in  std_logic
  );
end entity;  ------------------------------------------


-- Klessydra T03x (4 stages) pipeline implementation -----------------------
architecture EXECUTE of IE_STAGE is

  subtype harc_range is natural range THREAD_POOL_SIZE - 1 downto 0;

  signal instr_rvalid_IE_int        : std_logic;
  signal core_busy_IE_lat           : std_logic;

  --signal zero_rs1                   : std_logic;
  --signal zero_rs2                   : std_logic;
  --signal pass_BEQ                   : std_logic;
  --signal pass_BNE                   : std_logic;
  --signal pass_BLT                   : std_logic;
  --signal pass_BLTU                  : std_logic;
  --signal pass_BGE                   : std_logic;
  --signal pass_BGEU                  : std_logic;

  signal state_mulh, nextstate_mulh : mulh_states;
  signal state_mul, nextstate_mul   : mul_states;
  signal state_div, nextstate_div   : div_states;
  signal nextstate_IE               : fsm_IE_states;
  signal partial_mulh_a             : std_logic_vector(31 downto 0);
  signal partial_mulh_b             : std_logic_vector(31 downto 0);
  signal partial_mulh_c             : std_logic_vector(31 downto 0);
  signal partial_mulh_d             : std_logic_vector(31 downto 0);
  signal partial_mul_b              : std_logic_vector(31 downto 0);
  signal partial_mul_c              : std_logic_vector(31 downto 0);
  signal partial_mul_d              : std_logic_vector(31 downto 0);
  signal partial_mulh_a_wire        : std_logic_vector(31 downto 0);
  signal partial_mulh_b_wire        : std_logic_vector(31 downto 0);
  signal partial_mulh_c_wire        : std_logic_vector(31 downto 0);
  signal partial_mulh_d_wire        : std_logic_vector(31 downto 0);
  signal partial_mul_b_wire         : std_logic_vector(31 downto 0);
  signal partial_mul_c_wire         : std_logic_vector(31 downto 0);
  signal partial_mul_d_wire         : std_logic_vector(31 downto 0);
  signal MUL_int, MUL               : std_logic_vector(63 downto 0);
  signal MUL_low                    : std_logic_vector(31 downto 0);
  signal RS1_Data_IE_int            : std_logic_vector(31 downto 0);
  signal RS2_Data_IE_int            : std_logic_vector(31 downto 0);
  signal RS1_Data_IE_int_wire       : std_logic_vector(31 downto 0);
  signal RS2_Data_IE_int_wire       : std_logic_vector(31 downto 0);
  signal div_bypass_en              : std_logic;
  signal sub                        : std_logic_vector(32 downto 0);
  signal res_wire, res              : std_logic_vector(63 downto 0);
  signal div_count_wire, div_count  : unsigned(5 downto 0);

  signal Immediate                  : std_logic_vector(31 downto 0);
  signal absolute_address_int       : std_logic_vector(31 downto 0);

  signal WB_EN_next_IE              : std_logic;

  signal add_op_A                   : std_logic_vector(31 downto 0);
  signal add_op_B                   : std_logic_vector(31 downto 0);
  signal sr_op_A                    : std_logic_vector(31 downto 0); 
  signal sr_op_B                    : std_logic_vector(4  downto 0);
  signal sl_op_A                    : std_logic_vector(31 downto 0);
  signal sl_op_B                    : std_logic_vector(4  downto 0);
  signal logic_op_A                 : std_logic_Vector(31 downto 0);
  signal logic_op_B                 : std_logic_Vector(31 downto 0);

  signal halt_update_IE_wire        : std_logic_vector(harc_range);
  signal halt_update_IE_pending     : std_logic_vector(harc_range);

  signal flush_hart_int_wire        : std_logic_vector(harc_range);
  signal flush_hart_int             : std_logic_vector(harc_range);

  signal hart_sleep_count           : std_logic_vector(TPS_CEIL-1 downto 0); -- number of sleeping harts


  -- signals for counting intructions
  signal clock_cycle         : std_logic_vector(63 downto 0);  -- RDCYCLE
  signal external_counter    : std_logic_vector(63 downto 0);  -- RDTIME
  --signal instruction_counter : std_logic_vector(63 downto 0);  -- RDINSTRET

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

begin

  instr_rvalid_IE_int <= instr_rvalid_IE or core_busy_IE_lat;

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
      MUL_WB_EN              <= '0';
      --instruction_counter    <= std_logic_vector(to_unsigned(0, 64));
      core_busy_IE_lat       <= '0';
      WB_EN_next_IE          <= '0';
      halt_update_IE         <= (others => '0');
      halt_update_IE_pending <= (others => '0');
      --hart_sleep_count       <= (others => '0');
    elsif rising_edge(clk_i) then
      IE_WB_EN         <= '0';
      MUL_WB_EN        <= '0';
      WB_EN_next_IE    <= '0';
      core_busy_IE_lat <= core_busy_IE;
      hart_sleep_count <= std_logic_vector(to_unsigned(add_vect_bits(harc_sleep_wire),TPS_CEIL));
      -- Branch miss handling ---------------------------------------------
      if instr_gnt_i = '1' then
        halt_update_IE <= halt_update_IE_wire or halt_update_IE_pending;
        halt_update_IE_pending <= (others => '0'); -- AAA maybe make the pending signal into 1 bit instead of harc_range
      else
        halt_update_IE_pending <= halt_update_IE_wire; -- AAA maybe make the wire into a single wire istead of harc_range
      end if; 

      case state_IE is  -- stage state
        when sleep =>
          null;
        when normal =>
          -- check if there is a valid instruction and the thread it belongs to is not in a delay slot: 
          if  ie_instr_req = '0' and core_busy_IE_lat = '0' then
            -- in a generic version we would have conditions on busy_WB 
            -- in all states of the IE stage, and similarly in the comb process, just
            -- like we did in the ID stage.
          elsif instr_rvalid_IE_int = '0' then
            null;
          elsif irq_pending(harc_EXEC) = '1' then
            -- manage irq as an absolute branch to MTVEC, also defining mepc value properly in program counter unit
            -- irq is served only if we are not in a delay slot for the interrupted harc
            -- for simplicity presently only harc 0 is interrupted (decided in program counter unit)
            -- the current valid instruction is discarded, only its pc value gets used for mepc
          else
            IE_WB_EN            <= IE_WB_EN_wire;
            MUL_WB_EN           <= MUL_WB_EN_wire;
            pc_WB               <= pc_IE;
            instr_word_IE_WB    <= instr_word_IE;
            harc_IE_WB          <= harc_EXEC;
            -- misaligned_err      <= '0';

            -- EXECUTE OF INSTRUCTION -------------------------------------------

            -------------------------- ADDER ------------------------------
            if decoded_instruction_IE(ADDI_bit_position)  = '1' or
               decoded_instruction_IE(ADD7_bit_position)  = '1' or
               decoded_instruction_IE(SUB7_bit_position)  = '1' or
               decoded_instruction_IE(AUIPC_bit_position) = '1' or
               decoded_instruction_IE(JAL_bit_position)   = '1' or 
               decoded_instruction_IE(JALR_bit_position)  = '1' then
              IE_WB <= std_logic_vector(signed(add_op_A)+signed(add_op_B));
            end if;
            ---------------------------------------------------------------

            -----------------------  SHIFTERS -----------------------------
            if decoded_instruction_IE(SLLI_bit_position) = '1' or
               decoded_instruction_IE(SLLL_bit_position) = '1' then
              IE_WB <= to_stdlogicvector(to_bitvector(sl_op_A) sll to_integer(unsigned(sl_op_B)));
            end if;
            if decoded_instruction_IE(SRLI7_bit_position) = '1' or
               decoded_instruction_IE(SRLL7_bit_position) = '1' then
              IE_WB <= to_stdlogicvector(to_bitvector(sr_op_A) srl to_integer(unsigned(sr_op_B)));
            end if;
            if decoded_instruction_IE(SRAI7_bit_position) = '1' or
               decoded_instruction_IE(SRAA7_bit_position) = '1' then
              IE_WB <= to_stdlogicvector(to_bitvector(sr_op_A) sra to_integer(unsigned(sr_op_B)));
            end if;
            --------------------------------------------------------------

            -------------------- LOGIC UNITS -----------------------------
            if decoded_instruction_IE(ANDI_bit_position) = '1' or
               decoded_instruction_IE(ANDD_bit_position) = '1' then
              IE_WB <= logic_op_A and logic_op_B;
            end if;

            if decoded_instruction_IE(ORI_bit_position) = '1' or
               decoded_instruction_IE(ORR_bit_position) = '1' then
              IE_WB <= logic_op_A or logic_op_B;
            end if;

            if decoded_instruction_IE(XORI_bit_position) = '1' or
               decoded_instruction_IE(XORR_bit_position) = '1' then
              IE_WB <= logic_op_A xor logic_op_B;
            end if;
            --------------------------------------------------------------


            if decoded_instruction_IE(SLTI_bit_position) = '1' then
              if (signed(RS1_Data_IE) < signed (I_immediate(instr_word_IE))) then
                IE_WB <= std_logic_vector(to_unsigned(1, 32));
              else
                IE_WB <= std_logic_vector(to_unsigned(0, 32));
              end if;
            end if;

            if decoded_instruction_IE(SLTIU_bit_position) = '1' then
              if (unsigned(RS1_Data_IE) < unsigned (I_immediate(instr_word_IE))) then
                IE_WB <= std_logic_vector(to_unsigned(1, 32));
              else
                IE_WB <= std_logic_vector(to_unsigned(0, 32));
              end if;
            end if;

            if decoded_instruction_IE(LUI_bit_position) = '1' then
              IE_WB <= U_immediate(instr_word_IE);
            end if;

            if decoded_instruction_IE(SLT_bit_position) = '1' then
              if pass_BLT = '1' then
                IE_WB <= std_logic_vector(to_unsigned(1, 32));
              else
                IE_WB <= std_logic_vector(to_unsigned(0, 32));
              end if;
            end if;

            if decoded_instruction_IE(SLTU_bit_position) = '1' then
              if pass_BLTU = '1' then
                IE_WB <= std_logic_vector(to_unsigned(1, 32));
              else
                IE_WB <= std_logic_vector(to_unsigned(0, 32));
              end if;
            end if;

            if decoded_instruction_IE(ECALL_bit_position) = '1' then
              ie_except_data                <= ECALL_EXCEPT_CODE;
            end if;
            -----------------------------------------------------------

            --------------------- CSR OPS ---------------------------
            if decoded_instruction_IE(CSRRC_bit_position)  = '1' or 
               decoded_instruction_IE(CSRRS_bit_position)  = '1' or 
               decoded_instruction_IE(CSRRW_bit_position)  = '1' or
               decoded_instruction_IE(CSRRSI_bit_position) = '1' or 
               decoded_instruction_IE(CSRRCI_bit_position) = '1' or 
               decoded_instruction_IE(CSRRWI_bit_position) = '1' then
              if zero_rd = '0' then
                WB_EN_next_IE <= '1';
              end if;
            end if;

            -------------------------------------------------------

            if decoded_instruction_IE(ILL_bit_position) = '1' then
              ie_except_data                       <= ILLEGAL_INSN_EXCEPT_CODE;
            end if;

            if RV32M = 1 then
              if decoded_instruction_IE(MUL_bit_position) = '1' then
                MUL_WB <= MUL_low;
              end if;

              if decoded_instruction_IE(MULH_bit_position)   = '1' or 
                 decoded_instruction_IE(MULHU_bit_position)  = '1' or
                 decoded_instruction_IE(MULHSU_bit_position) = '1' then
                case state_mulh is
                  when mult =>
                    WB_EN_next_IE <= '1';
                  when others =>
                    null;
                end case;
                if core_busy_IE = '0' then
                  IE_WB  <= MUL(63 downto 32);
                end if;
              end if;

              if decoded_instruction_IE(DIVU_bit_position) = '1' or
                 decoded_instruction_IE(DIV_bit_position)  = '1' or
                 decoded_instruction_IE(REMU_bit_position) = '1' or
                 decoded_instruction_IE(REM_bit_position)  = '1' then
                if div_count_wire(5) = '1' or div_bypass_en = '1' then
                  WB_EN_next_IE <= '1';
                end if;
              end if;


              if decoded_instruction_IE(DIVU_bit_position) = '1' then
                if zero_rs2 = '1' then
                  IE_WB  <= (others => '1');
                elsif zero_rs1 = '1' then
                  IE_WB <= (others => '0');
                elsif pass_BEQ then
                  IE_WB <= (31 downto 1 => '0') & '1';
                elsif pass_BLTU then
                  IE_WB <= (others => '0');
                else
                  IE_WB <= res(31 downto 0);
                end if;
              end if;

              if decoded_instruction_IE(DIV_bit_position) = '1' then
                if zero_rs2 = '1' then
                  IE_WB  <= (others => '1');
                elsif zero_rs1 = '1' then
                  IE_WB <= (others => '0');
            --    elsif abs(signed(RS1_DATA_IE)) < abs(signed(RS2_DATA_IE)) then
              --    IE_WB <= (others => '0');
                elsif pass_BEQ then
                  if RS2_DATA_IE(31) = RS1_DATA_IE(31) then
                    IE_WB <= (31 downto 1 => '0') & '1';
                  else
                    IE_WB <= (31 downto 0 => '1');
                  end if;
                else
                  if RS1_DATA_IE(31) = RS2_DATA_IE(31) then
                    IE_WB <= res(31 downto 0);
                  else
                    IE_WB <= std_logic_vector(unsigned(not(res(31 downto 0)))+1);
                  end if;
                end if;
              end if;

              if decoded_instruction_IE(REMU_bit_position) = '1' then
                if zero_rs2 = '1' then
                  IE_WB <= RS1_Data_IE;
                elsif zero_rs1 = '1' then
                  IE_WB <= (others => '0');
                elsif pass_BEQ then
                  IE_WB <= (others => '0');
                elsif pass_BLTU then
                  IE_WB <= RS1_Data_IE;
                else
                  IE_WB <= res(63 downto 32);
                end if;
              end if;

              if decoded_instruction_IE(REM_bit_position) = '1' then
                if zero_rs2 = '1' then
                  IE_WB <= RS1_Data_IE;
                elsif zero_rs1 = '1' then
                  IE_WB <= (others => '0');
                elsif pass_BEQ then
                  IE_WB <= (others => '0');
            --    elsif abs(signed(RS1_DATA_IE)) < abs(signed(RS2_DATA_IE)) then
              --    IE_WB <= RS1_Data_IE;
            --    elsif abs(signed(RS1_DATA_IE)) = abs(signed(RS2_DATA_IE)) then
              --    IE_WB <= (others => '0');
                else
                  if RS1_DATA_IE(31) = '1' then
                    IE_WB <= std_logic_vector(unsigned(not(res(63 downto 32)))+1);
                  else
                    IE_WB <= res(63 downto 32);
                  end if;
                end if;
              end if;
            end if;

          -- EXECUTE OF INSTRUCTION (END) --------------------------
          end if;  -- instr_rvalid_IE values
          
        when csr_instr_wait_state =>
          if (csr_instr_done = '1' and csr_access_denied_o = '0') then
            if zero_rd = '0' then
              IE_WB_EN <= IE_WB_EN_wire;
              IE_WB <= csr_rdata_o;
            end if;
          elsif (csr_instr_done = '1' and csr_access_denied_o = '1') then  -- ILLEGAL_INSTRUCTION
            ie_except_data                       <= ILLEGAL_INSN_EXCEPT_CODE;
          end if;
      end case;  -- fsm_IE state cases
    end if;  -- reset, clk_i
  end process;

  div_bypass_en_gen : if RV32M = 1 generate
    -- AAA Bypass enable is not working, check KDOTP_test SIMD=2 Single accl number of vect elements = 1 
    --div_bypass_en <= '1' when zero_rs2 or zero_rs1 or pass_BEQ or (pass_BLTU and not signed_op) else '0';
    div_bypass_en <= '0';
  end generate;


  -----------------------------------------------------------
  --  ██╗███████╗     ██████╗ ██████╗ ███╗   ███╗██████╗   --
  --  ██║██╔════╝    ██╔════╝██╔═══██╗████╗ ████║██╔══██╗  --
  --  ██║█████╗      ██║     ██║   ██║██╔████╔██║██████╔╝  --
  --  ██║██╔══╝      ██║     ██║   ██║██║╚██╔╝██║██╔══██╗  --
  --  ██║███████╗    ╚██████╗╚██████╔╝██║ ╚═╝ ██║██████╔╝  --
  --  ╚═╝╚══════╝     ╚═════╝ ╚═════╝ ╚═╝     ╚═╝╚═════╝   --
  -----------------------------------------------------------

  fsm_IE_comb : process(all)

    variable absolute_jump_wires              : std_logic_vector(harc_range);
    variable core_busy_IE_wires               : std_logic;
    variable IE_except_condition_wires        : std_logic;
    variable set_branch_condition_wires       : std_logic;
    variable ie_taken_branch_wires            : std_logic;
    variable set_mret_condition_wires         : std_logic;
    variable set_wfi_condition_wires          : std_logic;
    variable jump_instr_wires                 : std_logic;
    variable branch_instr_wires               : std_logic;
    variable ebreak_instr_wires               : std_logic;
    variable WFI_Instr_wires                  : std_logic;
    variable served_irq_wires                 : std_logic_vector(harc_range);
    variable nextstate_IE_wires               : fsm_IE_states;

  begin
    Immediate                        <= B_immediate(instr_word_IE);
    PC_offset                        <= std_logic_vector(signed(pc_IE) + signed(Immediate));
    served_irq_wires                 := (others => '0');
    absolute_address_int             <= std_logic_vector(signed(RS1_Data_IE) + signed(I_immediate(instr_word_IE)));
    absolute_address                 <= absolute_address_int(31 downto 1) & '0'; -- set LSB to '0'
    nextstate_IE_wires               := normal;
    harc_to_csr                      <= harc_EXEC;
    absolute_jump_wires              := (others => '0');
    sys_instr                        <= (others => '0');
    core_busy_IE_wires               := '0';
    IE_except_condition_wires        := '0';
    set_branch_condition_wires       := '0';
    set_wfi_condition_wires          := '0';    
    ie_taken_branch_wires            := '0';
    set_mret_condition_wires         := '0';
    jump_instr_wires                 := '0';
    branch_instr_wires               := '0';
    ebreak_instr_wires               := '0';
    WFI_Instr_wires                  := '0';
    sleep_state                      <= '0';
    ie_csr_wdata_i                   <= RS1_Data_IE;
    csr_instr_req                    <= '0';
    csr_op_i                         <= (others => '0');
    csr_addr_i                       <= (others => '0');
    IE_WB_EN_wire                    <= (WB_EN_next_IE or (instr_rvalid_IE and WB_EN_next_ID  and not decoded_instruction_IE(MUL_bit_position))) and not served_irq_wires(harc_EXEC);
    --branch prediction signals
    halt_update_IE_wire              <= halt_update_IE_pending and not instr_gnt_i; -- latch the halt wire as long as we don't have a valid instr
    branch_taken                     <= '0';

    if RV32M = 1 then
      MUL_WB_EN_wire                 <= WB_EN_next_ID and decoded_instruction_IE(MUL_bit_position) and not served_irq_wires(harc_EXEC);
      RS1_Data_IE_int_wire           <= RS1_Data_IE_int;
      RS2_Data_IE_int_wire           <= RS2_Data_IE_int;
      partial_mulh_a_wire            <= (others => '0');
      partial_mulh_b_wire            <= (others => '0');
      partial_mulh_c_wire            <= (others => '0');
      partial_mulh_d_wire            <= (others => '0');
      partial_mul_b_wire             <= (others => '0');
      partial_mul_c_wire             <= (others => '0');
      partial_mul_d_wire             <= (others => '0');
      MUL_int                        <= (others => '0');
      MUL                            <= (others => '0');
      MUL_low                        <= (others => '0');
      div_count_wire                 <= (others => '0');
      res_wire                       <= (others => '0');
      sub                            <= (others => '0');
      nextstate_mul                  <= mult;
      nextstate_mulh                 <= init;
      nextstate_div                  <= init;
    end if;

    if fetch_stage_en = 0 then
      flush_hart_int_wire <= (others => '0');
      for h in harc_range loop
        IE_flush_hart_ID(h) <= flush_hart_int_wire(h);
      end loop;
    elsif fetch_stage_en = 1 then
      for h in harc_range loop
        flush_hart_int_wire(h)    <= '0'; -- reset the wire if the pipeline is not halted, else latch the flush signal
        IE_flush_hart_ID(h)       <= flush_hart_int_wire(h) or flush_hart_int(h); -- flushes two cycles of the current hart, hence its "int_wire or int"
        IE_flush_hart_FETCH(h)    <= flush_hart_int_wire(h); -- flushes only one cycle of the current hart
      end loop;
    end if;

    case state_IE is                  -- stage status
      when sleep =>
        if fetch_stage_en = 1 then
          if unsigned(hart_sleep_count) = 1 then
            halt_update_IE_wire(harc_EXEC) <= '1';  -- maintain the halt from the mret
          end if;
        end if;
        if irq_i = '1' or irq_pending(harc_EXEC) = '1' then
          flush_hart_int_wire(harc_EXEC) <= '1';
          nextstate_IE_wires             := normal;
          served_irq_wires(harc_EXEC)    := '1';
          ie_taken_branch_wires          := '1';
        elsif fetch_enable_i = '1' then
          flush_hart_int_wire(harc_EXEC) <= '1';
          nextstate_IE_wires := normal;
        else
          core_busy_IE_wires := '1';
          nextstate_IE_wires := sleep;
        end if;

      when normal =>

        if ie_instr_req = '0' and core_busy_IE_lat = '0' then
        elsif instr_rvalid_IE_int = '0' then
        elsif irq_pending(harc_EXEC)= '1' then
          -- manage irq as an absolute branch to MTVEC, also defining mepc value properly in program counter unit
          -- irq is served only if we are not in a delay slot for the interrupted harc
          -- for simplicity presently only harc 0 is interrupted (decided in program counter unit)
          -- the current valid instruction is discarded, only its pc value gets used for mepc
          flush_hart_int_wire(harc_EXEC) <= '1';
          served_irq_wires(harc_EXEC) := '1';
          ie_taken_branch_wires       := '1';
          if fetch_stage_en = 1 then
            if unsigned(hart_sleep_count) = 1 then
              halt_update_IE_wire(harc_EXEC) <= '1'; -- AAA find a cleaner way to do this when having two threads and a fetch stage
            end if;
          end if;
          if decoded_instruction_IE(WFI_bit_position) = '1' then -- Inform the CSR unit that the last instruction before we went to the subroutine was a WFI instruction.
            WFI_Instr_wires := '1';
          end if;
        else                         -- process the instruction

        -- EXECUTE OF INSTRUCTION ---------------------

          if decoded_instruction_IE(JAL_bit_position) = '1' then  -- JAL instruction
            jump_instr_wires               := '1';
            if dsp_to_jump(harc_EXEC) = '1' then -- dsp_tp_jump is handled here, and not in previous stages
              flush_hart_int_wire(harc_EXEC) <= '1';
            end if;
            if fetch_stage_en = 0 then
              if unsigned(hart_sleep_count) < 2 or dsp_to_jump(harc_EXEC) = '1' then -- dsp_tp_jump is handled here, and not in previous stages
                set_branch_condition_wires := '1';
                ie_taken_branch_wires      := '1';
                Immediate                  <= UJ_immediate(instr_word_IE);
              end if;
            else
              if unsigned(hart_sleep_count) = 0 or dsp_to_jump(harc_EXEC) = '1' then -- dsp_tp_jump is handled here, and not in previous stages
                set_branch_condition_wires := '1';
                ie_taken_branch_wires      := '1';
                Immediate                  <= UJ_immediate(instr_word_IE);
              end if;
            end if;
          end if;

          if decoded_instruction_IE(JALR_bit_position) = '1' then  -- JALR instruction
            if jalr_flush(harc_EXEC) = '1' or morph_en = 0 or fetch_stage_en = 0 or unsigned(hart_sleep_count) = 0 then
              set_branch_condition_wires     := '1';
              ie_taken_branch_wires          := '1';
              jump_instr_wires               := '1';
              absolute_jump_wires(harc_EXEC) := '1';
              flush_hart_int_wire(harc_EXEC) <= '1';
              if fetch_stage_en = 1 then
                if unsigned(hart_sleep_count_ID) = 1 then
                  halt_update_IE_wire(harc_EXEC) <= '1';
                end if;
              end if;
            end if;
          end if;

          if decoded_instruction_IE(BEQ_bit_position) = '1' then
            branch_instr_wires := '1';
            if pass_BEQ = '1' then
              branch_taken <= '1';
              if (unsigned(hart_sleep_count_ID) = 1 and fetch_stage_en = 0) then
                halt_update_IE_wire(harc_EXEC) <= '1';
              end if;
            else
              Immediate <= (std_logic_vector(to_unsigned(4, 32)));
            end if;
            if (unsigned(hart_sleep_count) = 1 and fetch_stage_en = 0) or unsigned(hart_sleep_count) = 0 or branch_predict_en = 0 or morph_en = 0  then
              if pass_BEQ = '1' then
                set_branch_condition_wires := '1';
                ie_taken_branch_wires      := '1';
              end if;
            else
              if pass_BEQ = not branch_predict_taken_IE then
                flush_hart_int_wire(harc_EXEC)     <= '1';
                if fetch_stage_en = 1 then
                  if unsigned(hart_sleep_count_ID) = 1 then
                    halt_update_IE_wire(harc_EXEC) <= '1';
                  end if;
                end if;
                set_branch_condition_wires  := '1';
                ie_taken_branch_wires       := '1';
              end if;
            end if;
          end if;

          if decoded_instruction_IE(BNE_bit_position) = '1' then
            branch_instr_wires := '1';
            if pass_BNE = '1' then
              branch_taken <= '1';
              if (unsigned(hart_sleep_count_ID) = 1 and fetch_stage_en = 0) then
                halt_update_IE_wire(harc_EXEC) <= '1';
              end if;
            else
              Immediate <= (std_logic_vector(to_unsigned(4, 32)));
            end if;
            if (unsigned(hart_sleep_count) = 1 and fetch_stage_en = 0) or unsigned(hart_sleep_count) = 0 or branch_predict_en = 0  or morph_en = 0 then
              if pass_BNE = '1' then
                set_branch_condition_wires := '1';
                ie_taken_branch_wires      := '1';
              end if;
            else
              if pass_BNE = not branch_predict_taken_IE then
                flush_hart_int_wire(harc_EXEC)     <= '1';
                if fetch_stage_en = 1 then
                  if unsigned(hart_sleep_count_ID) = 1 then
                    halt_update_IE_wire(harc_EXEC) <= '1';
                  end if;
                end if;
                set_branch_condition_wires  := '1';
                ie_taken_branch_wires       := '1';
              end if;
            end if;
          end if;

          if decoded_instruction_IE(BLT_bit_position) = '1' then
            branch_instr_wires := '1';
            if pass_BLT = '1' then
              branch_taken <= '1';
              if (unsigned(hart_sleep_count_ID) = 1 and fetch_stage_en = 0) then
                halt_update_IE_wire(harc_EXEC) <= '1';
              end if;
            else
              Immediate <= (std_logic_vector(to_unsigned(4, 32)));
            end if;
            if (unsigned(hart_sleep_count) = 1 and fetch_stage_en = 0) or unsigned(hart_sleep_count) = 0 or branch_predict_en = 0 or morph_en = 0 then
              if pass_BLT = '1' then
                set_branch_condition_wires := '1';
                ie_taken_branch_wires      := '1';
              end if;
            else
              if pass_BLT = not branch_predict_taken_IE then
                flush_hart_int_wire(harc_EXEC)     <= '1';
                if fetch_stage_en = 1 then
                  if unsigned(hart_sleep_count_ID) = 1 then
                    halt_update_IE_wire(harc_EXEC) <= '1';
                  end if;
                end if;
                set_branch_condition_wires  := '1';
                ie_taken_branch_wires       := '1';
              end if;
            end if;
          end if;

          if decoded_instruction_IE(BLTU_bit_position) = '1' then
            branch_instr_wires := '1';
            if pass_BLTU = '1' then
              branch_taken <= '1';
              if (unsigned(hart_sleep_count_ID) = 1 and fetch_stage_en = 0) then
                halt_update_IE_wire(harc_EXEC) <= '1';
              end if;
            else
              Immediate <= (std_logic_vector(to_unsigned(4, 32)));
            end if;
            if (unsigned(hart_sleep_count) = 1 and fetch_stage_en = 0) or unsigned(hart_sleep_count) = 0 or branch_predict_en = 0 or morph_en = 0 then
              if pass_BLTU = '1' then
                set_branch_condition_wires := '1';
                ie_taken_branch_wires      := '1';
              end if;
            else
              if pass_BLTU = not branch_predict_taken_IE then
                flush_hart_int_wire(harc_EXEC)     <= '1';
                if fetch_stage_en = 1 then
                  if unsigned(hart_sleep_count_ID) = 1 then
                    halt_update_IE_wire(harc_EXEC) <= '1';
                  end if;
                end if;
                set_branch_condition_wires  := '1';
                ie_taken_branch_wires       := '1';
              end if;
            end if;
          end if;

          if decoded_instruction_IE(BGE_bit_position) = '1' then
            branch_instr_wires := '1';
            if pass_BGE = '1' then
              branch_taken <= '1';
              if (unsigned(hart_sleep_count_ID) = 1 and fetch_stage_en = 0) then
                halt_update_IE_wire(harc_EXEC) <= '1';
              end if;
            else
              Immediate <= (std_logic_vector(to_unsigned(4, 32)));
            end if;
            if (unsigned(hart_sleep_count) = 1 and fetch_stage_en = 0) or unsigned(hart_sleep_count) = 0 or branch_predict_en = 0 or morph_en = 0 then
              if pass_BGE = '1' then
                set_branch_condition_wires := '1';
                ie_taken_branch_wires      := '1';
              end if;
            else
              if pass_BGE = not branch_predict_taken_IE then
                flush_hart_int_wire(harc_EXEC)     <= '1';
                if fetch_stage_en = 1 then
                  if unsigned(hart_sleep_count_ID) = 1 then
                    halt_update_IE_wire(harc_EXEC) <= '1';
                  end if;
                end if;
                set_branch_condition_wires  := '1';
                ie_taken_branch_wires       := '1';
              end if;
            end if;
          end if;

          if decoded_instruction_IE(BGEU_bit_position) = '1' then
            branch_instr_wires := '1';
            if pass_BGEU = '1' then
              branch_taken <= '1';
              if (unsigned(hart_sleep_count_ID) = 1 and fetch_stage_en = 0) then
                halt_update_IE_wire(harc_EXEC) <= '1';
              end if;
            else
              Immediate <= (std_logic_vector(to_unsigned(4, 32)));
            end if;
            if (unsigned(hart_sleep_count) = 1 and fetch_stage_en = 0) or unsigned(hart_sleep_count) = 0 or branch_predict_en = 0 or morph_en = 0 then
              if pass_BGEU = '1' then
                set_branch_condition_wires := '1';
                ie_taken_branch_wires      := '1';
              end if;
            else
              if pass_BGEU = not branch_predict_taken_IE then
                flush_hart_int_wire(harc_EXEC)     <= '1';
                if fetch_stage_en = 1 then
                  if unsigned(hart_sleep_count_ID) = 1 then
                    halt_update_IE_wire(harc_EXEC) <= '1';
                  end if;
                end if;
                set_branch_condition_wires  := '1';
                ie_taken_branch_wires       := '1';       
              end if;        
            end if;        
          end if;

          if decoded_instruction_IE(SW_MIP_bit_position) = '1' then
            if data_addr_internal_IE(31 downto 8) = x"0000FF" and halt_IE = '0' then
              core_busy_IE_wires := '1';
              nextstate_IE_wires := csr_instr_wait_state;
            end if;
            if data_addr_internal_IE(31 downto 8) = x"0000FF" then
              csr_op_i <= CSRRW;
              if halt_IE = '0' then
                csr_instr_req  <= '1';
                ie_csr_wdata_i <= RS2_Data_IE;
                csr_addr_i     <= MIP_ADDR;
                for i in harc_range loop
                  if data_addr_internal_IE(7 downto 0) = std_logic_vector(to_unsigned((4*i),8)) then
                    harc_to_csr <= i;
                  end if;
                end loop;
              end if;
            end if;
          end if;

          if decoded_instruction_IE(CSRRW_bit_position) = '1' or decoded_instruction_IE(CSRRWI_bit_position) = '1' or
             decoded_instruction_IE(CSRRC_bit_position) = '1' or decoded_instruction_IE(CSRRCI_bit_position) = '1' or
             decoded_instruction_IE(CSRRS_bit_position) = '1' or decoded_instruction_IE(CSRRSI_bit_position) = '1' then
            if halt_IE = '0' then
              core_busy_IE_wires := '1';
              nextstate_IE_wires := csr_instr_wait_state;
              csr_instr_req      <= '1';
              csr_op_i           <= FUNCT3(instr_word_IE);
              csr_addr_i         <= std_logic_vector(to_unsigned(to_integer(unsigned(CSR_ADDR(instr_word_IE))), 12));
            end if;
          end if;

          if decoded_instruction_IE(CSRRWI_bit_position) = '1' or
             decoded_instruction_IE(CSRRCI_bit_position) = '1' or
             decoded_instruction_IE(CSRRSI_bit_position) = '1' then
            ie_csr_wdata_i <= std_logic_vector(resize(to_unsigned(rs1(instr_word_IE), 5), 32));
          end if;

          if decoded_instruction_IE(ECALL_bit_position) = '1' then
            IE_except_condition_wires := '1';
            ie_taken_branch_wires     := '1';
            if fetch_stage_en = 1 then
              if unsigned(hart_sleep_count) = 1 then
                halt_update_IE_wire(harc_EXEC) <= '1';  -- AAA find a cleaner way to do this when having two threads and a fetch stage
              end if;
            end if;
          end if;

          if decoded_instruction_IE(EBREAK_bit_position) = '1' then
            ebreak_instr_wires := '1';
          end if;

          if decoded_instruction_IE(MRET_bit_position) = '1' then
            set_mret_condition_wires := '1';
            ie_taken_branch_wires    := '1';
            sys_instr(harc_EXEC)     <= '1';
            if fetch_enable_i = '0' then
              nextstate_IE_wires      := sleep;
              core_busy_IE_wires      := '1';
            end if;
            if fetch_stage_en = 1 then
              if unsigned(hart_sleep_count) = 1 then
                halt_update_IE_wire(harc_EXEC) <= '1'; -- AAA find a cleaner way to do this when having two threads and a fetch stage
              end if;
            end if;
          end if;

          if decoded_instruction_IE(WFI_bit_position) = '1' then
            Immediate <= (others => '0');
            if MSTATUS(harc_EXEC)(0) = '1' then
              set_wfi_condition_wires  := '1';
              ie_taken_branch_wires    := '1';
            end if;
            if fetch_stage_en = 1 then
              if unsigned(hart_sleep_count) = 1 then
                halt_update_IE_wire(harc_EXEC) <= '1'; -- AAA find a cleaner way to do this when having two threads and a fetch stage
              end if;
            end if;
          end if;

          if decoded_instruction_IE(ILL_bit_position) = '1' then  -- ILLEGAL_INSTRUCTION
            IE_except_condition_wires := '1';
            ie_taken_branch_wires     := '1';
          end if;

          if RV32M = 1 then

            if decoded_instruction_IE(MULH_bit_position)   = '1' or
               decoded_instruction_IE(MULHU_bit_position)  = '1' or
               decoded_instruction_IE(MULHSU_bit_position) = '1' then
              case state_mulh is
                when init =>
                  if RS1_Data_IE(31) = '1' and signed_op = '1' then
                    RS1_Data_IE_int_wire <= std_logic_vector(signed(not(RS1_Data_IE))+1);
                  else
                    RS1_Data_IE_int_wire <= RS1_Data_IE;
                  end if;
                  if RS2_Data_IE(31) = '1' and signed_op = '1' and decoded_instruction_IE(MULHSU_bit_position) = '0' then
                    RS2_Data_IE_int_wire <= std_logic_vector(signed(not(RS2_Data_IE))+1);
                  else
                    RS2_Data_IE_int_wire <= RS2_Data_IE;
                  end if;
                  nextstate_mulh <= mult;
                  core_busy_IE_wires := '1';
                when mult =>
                    partial_mulh_a_wire <= std_logic_vector( unsigned(RS1_Data_IE_int(31 downto 16)) 
                                                      * unsigned(RS2_Data_IE_int(31 downto 16)));
                    partial_mulh_b_wire <= std_logic_vector( unsigned(RS1_Data_IE_int(15 downto 0))   
                                                      * unsigned(RS2_Data_IE_int(31 downto 16)));
                    partial_mulh_c_wire <= std_logic_vector( unsigned(RS1_Data_IE_int(31 downto 16)) 
                                                      * unsigned(RS2_Data_IE_int(15 downto 0)));
                    partial_mulh_d_wire <= std_logic_vector( unsigned(RS1_Data_IE_int(15 downto 0))   
                                                      * unsigned(RS2_Data_IE_int(15 downto 0)));
                  nextstate_mulh <= accum;
                  core_busy_IE_wires := '1';
                when accum =>
                  MUL_int <= std_logic_vector((        unsigned(partial_mulh_a) & unsigned(partial_mulh_d)) +
                                            (x"0000" & unsigned(partial_mulh_b) & x"0000")                  +
                                            (x"0000" & unsigned(partial_mulh_c) & x"0000"));
                  if (RS1_Data_IE(31) /= RS2_Data_IE(31) and decoded_instruction_IE(MULH_bit_position) = '1') or
                                        (RS1_Data_IE(31) = '1' and decoded_instruction_IE(MULHSU_bit_position) = '1') then
                    MUL <= std_logic_vector(signed(not(MUL_int))+1);
                  else
                    MUL <= MUL_int;
                  end if;
              end case;
            end if;

            if decoded_instruction_IE(MUL_bit_position) = '1' then
              partial_mul_b_wire <= std_logic_vector( unsigned(RS1_Data_IE(15 downto 0))   
                                                * unsigned(RS2_Data_IE(31 downto 16)));
              partial_mul_c_wire <= std_logic_vector( unsigned(RS1_Data_IE(31 downto 16)) 
                                                * unsigned(RS2_Data_IE(15 downto 0)));
              partial_mul_d_wire <= std_logic_vector( unsigned(RS1_Data_IE(15 downto 0))   
                                                * unsigned(RS2_Data_IE(15 downto 0)));
              MUL_low <= std_logic_vector((unsigned(partial_mul_d_wire(31 downto 16)) +
                                           unsigned(partial_mul_b_wire(15 downto 0))  +
                                           unsigned(partial_mul_c_wire(15 downto 0))) &
                                           unsigned(partial_mul_d_wire(15 downto 0)));
            end if;

            if decoded_instruction_IE(DIV_bit_position)  = '1' or 
               decoded_instruction_IE(REM_bit_position)  = '1' or
               decoded_instruction_IE(DIVU_bit_position) = '1' or 
               decoded_instruction_IE(REMU_bit_position) = '1' then
              case state_div is
                when init =>
                  if RS1_Data_IE(31) = '0' or signed_op = '0' then
                    res_wire <= (31 downto 0 => '0') & RS1_Data_IE;
                  else
                    RS1_Data_IE_int_wire <= std_logic_vector(signed(not(RS1_Data_IE)) + 1);
                    res_wire <= (31 downto 0 => '0') & RS1_Data_IE_int_wire;
                  end if;
                  if RS2_Data_IE(31) = '0' or signed_op = '0' then
                    RS2_Data_IE_int_wire <= RS2_Data_IE;
                  else
                    RS2_Data_IE_int_wire <= std_logic_vector(signed(not(RS2_Data_IE)) + 1);
                  end if;
                  nextstate_div <= divide;
                  core_busy_IE_wires := '1';
                when divide =>
                  if div_count(5) /= '1' then
                    div_count_wire <= div_count + 1;
                    nextstate_div <= divide;
                    core_busy_IE_wires := '1';
                  end if;
                  if sub(32) = '1' then -- RS2_Data_IE is the divisor
                    res_wire <= res(62 downto 0) & '0';
                  else
                    res_wire <= sub(31 downto 0) & res(30 downto 0) & '1';
                  end if;
                  sub <= std_logic_vector(('0' & unsigned(res(62 downto 31))) - ('0' & unsigned(RS2_Data_IE_int)));
              end case; 
            end if;

          end if; -- END RV32M

        -- EXECUTE OF INSTRUCTION (END)
        end if;  -- instr_rvalid_IE values 

      when csr_instr_wait_state =>
        if csr_instr_done = '0' then
          nextstate_IE_wires := csr_instr_wait_state;
          core_busy_IE_wires := '1';
        elsif (csr_instr_done = '1' and csr_access_denied_o = '1') then  -- ILLEGAL_INSTRUCTION
          nextstate_IE_wires        := normal;
          IE_except_condition_wires := '1';
          ie_taken_branch_wires     := '1';
        else
          nextstate_IE_wires := normal;
        end if;


    end case;  -- fsm_IE state cases

    absolute_jump              <= absolute_jump_wires;
    core_busy_IE               <= core_busy_IE_wires;
    IE_except_condition        <= IE_except_condition_wires;
    set_branch_condition       <= set_branch_condition_wires;
    served_irq                 <= served_irq_wires;
    ie_taken_branch            <= ie_taken_branch_wires;
    set_mret_condition         <= set_mret_condition_wires;    
    set_wfi_condition          <= set_wfi_condition_wires;
    jump_instr                 <= jump_instr_wires;
    branch_instr               <= branch_instr_wires;
    ebreak_instr               <= ebreak_instr_wires;
    nextstate_IE               <= nextstate_IE_wires;
    WFI_Instr                  <= WFI_Instr_wires;
  end process;

  fsm_IE_state : process(clk_i, rst_ni) -- also implements the delay slot counters and some aux signals
  begin
    
    if rst_ni = '0' then
      flush_hart_int     <= (others => '0');
      branch_instr_lat   <= '0'; 
      jump_instr_lat     <= '0';
      state_IE           <= sleep;
      if RV32M = 1 then 
        state_mulh       <= init;
        state_mul        <= mult;
        state_div        <= init;
      end if;
    elsif rising_edge(clk_i) then
      flush_hart_int         <= flush_hart_int_wire;
      branch_instr_lat       <= branch_instr;
      jump_instr_lat         <= jump_instr;
      state_IE               <= nextstate_IE;
      if RV32M = 1 then
        state_mulh           <= nextstate_mulh;
        state_mul            <= nextstate_mul;
        state_div            <= nextstate_div;
        div_count            <= div_count_wire;
        res                  <= res_wire;
        RS1_Data_IE_int      <= RS1_Data_IE_int_wire;  -- used by the divider as well
        RS2_Data_IE_int      <= RS2_Data_IE_int_wire;  -- used by the divider as well
        --partial_mul_b        <= partial_mul_b_wire;
        --partial_mul_c        <= partial_mul_c_wire;
        --partial_mul_d        <= partial_mul_d_wire;
        partial_mulh_a       <= partial_mulh_a_wire;
        partial_mulh_b       <= partial_mulh_b_wire;
        partial_mulh_c       <= partial_mulh_c_wire;
        partial_mulh_d       <= partial_mulh_d_wire;
      end if;
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
      add_op_A <= RS1_data_IE;
      add_op_B <= I_immediate(instr_word_IE);
    end if;
    if decoded_instruction_IE(ADD7_bit_position) = '1' then
      add_op_A <= RS1_data_IE;
      add_op_B <= RS2_data_IE;
    end if;
    if decoded_instruction_IE(SUB7_bit_position) = '1' then
      add_op_A <= RS1_data_IE;
      add_op_B <= std_logic_vector(unsigned(not(RS2_data_IE))+1);
    end if;
    if decoded_instruction_IE(AUIPC_bit_position) = '1' then
      add_op_A <= pc_IE;
      add_op_B <= U_immediate(instr_word_IE);
    end if;
    if decoded_instruction_IE(JAL_bit_position) = '1' or decoded_instruction_IE(JALR_bit_position) = '1' then -- for the link register
      add_op_A <= pc_IE;
      add_op_B <= (3 to 31 => '0') & "100";
    end if;

    if decoded_instruction_IE(SLLI_bit_position) = '1' then
      sl_op_A <= RS1_Data_IE;
      sl_op_B <= SHAMT(instr_word_IE);
    end if;
    if decoded_instruction_IE(SRLI7_bit_position) = '1' then
      sr_op_A <= RS1_Data_IE;
      sr_op_B <= SHAMT(instr_word_IE);
    end if;
    if decoded_instruction_IE(SRAI7_bit_position) = '1' then
      sr_op_A <= RS1_Data_IE;
      sr_op_B <= SHAMT(instr_word_IE);
    end if;
    if decoded_instruction_IE(SLLL_bit_position) = '1' then
      sl_op_A <= RS1_Data_IE;
      sl_op_B <= RS2_Data_IE(4 downto 0);
    end if;
    if decoded_instruction_IE(SRLL7_bit_position) = '1' then
      sr_op_A <= RS1_Data_IE;
      sr_op_B <= RS2_Data_IE(4 downto 0);
    end if;
    if decoded_instruction_IE(SRAA7_bit_position) = '1' then
      sr_op_A <= RS1_Data_IE;
      sr_op_B <= RS2_Data_IE(4 downto 0);
    end if;

    if decoded_instruction_IE(ANDI_bit_position) = '1' or
       decoded_instruction_IE(ORI_bit_position)  = '1' or
       decoded_instruction_IE(XORI_bit_position) = '1' then
      logic_op_A <= RS1_Data_IE;
      logic_op_B <= I_immediate(instr_word_IE);
    end if;
    if decoded_instruction_IE(ANDD_bit_position) = '1' or
       decoded_instruction_IE(ORR_bit_position)  = '1' or
       decoded_instruction_IE(XORR_bit_position) = '1' then
      logic_op_A <= RS1_Data_IE;
      logic_op_B <= RS2_Data_IE;
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

  --comparator_enable_comb : process(all)
  --begin
  --  pass_BEQ  <= '0';
  --  pass_BNE  <= '0';
  --  pass_BLT  <= '0';
  --  pass_BGE  <= '0';
  --  pass_BLTU <= '0';
  --  pass_BGEU <= '0';
  --  zero_rs1  <= '0';
  --  zero_rs2  <= '0';
  --  if comparator_en = '1' then
  --    if unsigned(RS1_Data_IE) = 0 then
  --      zero_rs1 <= '1';
  --    end if;
  --    if unsigned(RS2_Data_IE) = 0 then
  --      zero_rs2 <= '1';
  --    end if;
  --    if (signed(RS1_Data_IE) = signed(RS2_Data_IE)) then
  --      pass_BEQ <= '1';
  --    else
  --      pass_BNE <= '1';
  --    end if;
  --    if (signed(RS1_Data_IE) < signed(RS2_Data_IE)) then
  --      pass_BLT <= '1';
  --    else
  --      pass_BGE <= '1';
  --    end if;
  --    if (unsigned(RS1_Data_IE) < unsigned(RS2_Data_IE)) then
  --      pass_BLTU <= '1';
  --    else
  --      pass_BGEU <= '1';
  --    end if;
  --  end if;
  --end process;

-------------------------------------------------------------------end of IE stage ---------------
--------------------------------------------------------------------------------------------------

end EXECUTE;
--------------------------------------------------------------------------------------------------
-- END of IE architecture ------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------