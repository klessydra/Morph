-- ieee packages ------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_misc.all;
use ieee.numeric_std.all;
use std.textio.all;

-- local packages ------------
use work.riscv_klessydra.all;

entity MULTIPLIER_DIVIDER is
  generic (
    RF_CEIL                     : natural
    );
  port (
    clk_i                       : in  std_logic;
    rst_ni                      : in  std_logic;
    fetch_enable_i              : in  std_logic;

    irq_pending                 : in  std_logic;
    dbg_req_o                   : in  std_logic;
    signed_op                   : in  std_logic;
    MUL_DIV_instr               : in  std_logic;
    mul_div_addr_new_rd         : in  std_logic_vector(RF_CEIL-1 downto 0);
    decoded_instruction_MUL_DIV : in  std_logic_vector(MUL_DIV_UNIT_INSTR_SET_SIZE-1 downto 0);
    pc_mul_div                  : in  std_logic_vector(31 downto 0); 
    mul_div_data_rs1            : in  std_logic_vector(31 downto 0);
    mul_div_data_rs2            : in  std_logic_vector(31 downto 0);
    mul_div_old_rd              : in  std_logic_vector(31 downto 0);
    mul_div_old_rd_valid        : in  std_logic;
    mul_div_old_rd_valid_CC     : out std_logic;
    pc_mul_div_WB               : out std_logic_vector(31 downto 0);
    mul_div_old_rd_CC           : out std_logic_vector(31 downto 0);
    MUL_DIV_WB                  : out std_logic_vector(31 downto 0);
    MUL_DIV_WB_EN               : out std_logic;
    MUL_DIV_WB_RD_ADDR          : out std_logic_vector(RF_CEIL-1 downto 0); -- contains the address of the register that has the new valid result
    busy_mul_div                : out std_logic;
    -- Recovery Buffer Signals
    branch_miss                 : in  std_logic;
    speculative_mul_div         : in  std_logic
  );
end entity; --------------------------------------

architecture MUL_DIV of MULTIPLIER_DIVIDER is

  signal pass_BEQ                         : std_logic;
  signal pass_BLTU                        : std_logic;
  signal zero_rs1                         : std_logic;
  signal zero_rs2                         : std_logic;

  signal state_mul, nextstate_mul         : mul_states;
  signal state_div, nextstate_div         : div_states;
  signal state_MUL_DIV, nextstate_MUL_DIV : fsm_IE_states;
  signal partial_mult_a                   : std_logic_vector(31 downto 0);
  signal partial_mult_b                   : std_logic_vector(31 downto 0);
  signal partial_mult_c                   : std_logic_vector(31 downto 0);
  signal partial_mult_d                   : std_logic_vector(31 downto 0);
  signal partial_mult_a_wire              : std_logic_vector(31 downto 0);
  signal partial_mult_b_wire              : std_logic_vector(31 downto 0);
  signal partial_mult_c_wire              : std_logic_vector(31 downto 0);
  signal partial_mult_d_wire              : std_logic_vector(31 downto 0);
  signal MUL_int, MUL                     : std_logic_vector(63 downto 0);
  signal mul_div_data_rs1_int             : std_logic_vector(31 downto 0);
  signal mul_div_data_rs2_int             : std_logic_vector(31 downto 0);
  signal mul_div_data_rs1_int_wire        : std_logic_vector(31 downto 0);
  signal mul_div_data_rs2_int_wire        : std_logic_vector(31 downto 0);
  signal sub                              : std_logic_vector(32 downto 0);
  signal res_wire, res                    : std_logic_vector(63 downto 0);
  signal div_bypass_en                    : std_logic;
  signal div_count_wire, div_count        : unsigned(5 downto 0);

begin

  div_bypass_en <= '1' when zero_rs2 or zero_rs1 or pass_BEQ or (pass_BLTU and not signed_op) else '0';

  fsm_IE_sync : process(clk_i, rst_ni)
  begin
    if rst_ni = '0' then
      MUL_DIV_WB                  <= std_logic_vector(to_unsigned(0, 32));
      MUL_DIV_WB_EN               <= '0';
      --instruction_counter    <= std_logic_vector(to_unsigned(0, 64));
    elsif rising_edge(clk_i) then

      MUL_DIV_WB_RD_ADDR <= mul_div_addr_new_rd;

      case state_MUL_DIV is  -- stage state
        when debug =>
          null;
        when normal =>
          -- check if there is a valid instruction and the thread it belongs to is not in a delay slot: 
          if  MUL_DIV_instr = '0' then
            MUL_DIV_WB_EN <= '0';
          elsif branch_miss = '1' and speculative_mul_div = '1' then
            MUL_DIV_WB_EN <= '0';
          elsif irq_pending = '1' then
            MUL_DIV_WB_EN <= '0';
          else
            --instruction_counter <= std_logic_vector(unsigned(instruction_counter)+1);  -- AAA should be updated or removed since the exec stage has been split
            mul_div_old_rd_CC     <= mul_div_old_rd;
            pc_mul_div_WB         <= pc_mul_div;
            -- EXECUTE OF INSTRUCTION -------------------------------------------

             MUL_DIV_WB_EN <= '0';

            if decoded_instruction_MUL_DIV(MUL_bit_position) = '1' then
              MUL_DIV_WB_EN <= '1';
              MUL_DIV_WB    <= MUL(31 downto 0);
            end if;

            if decoded_instruction_MUL_DIV(MULH_bit_position)   = '1' or 
               decoded_instruction_MUL_DIV(MULHU_bit_position)  = '1' or
               decoded_instruction_MUL_DIV(MULHSU_bit_position) = '1' then
              MUL_DIV_WB_EN <= '1';
              MUL_DIV_WB    <= MUL(63 downto 32);
            end if;

            if decoded_instruction_MUL_DIV(DIVU_bit_position) = '1' then
              if div_count(5) = '1' or div_bypass_en = '1' then
                MUL_DIV_WB_EN <= '1';
              end if;
              if zero_rs2 = '1' then
                MUL_DIV_WB  <= (others => '1');
              elsif zero_rs1 = '1' then
                MUL_DIV_WB <= (others => '0');
              elsif pass_BEQ then
                MUL_DIV_WB <= (31 downto 1 => '0') & '1';
              elsif pass_BLTU then
                MUL_DIV_WB <= (others => '0');
              else
                MUL_DIV_WB <= res(31 downto 0);
              end if;
            end if;

            if decoded_instruction_MUL_DIV(DIV_bit_position) = '1' then
              if div_count(5) = '1' or div_bypass_en = '1' then
                MUL_DIV_WB_EN <= '1';
              end if;
              if zero_rs2 = '1' then
                MUL_DIV_WB  <= (others => '1');
              elsif zero_rs1 = '1' then
                MUL_DIV_WB <= (others => '0');
          --    elsif abs(signed(mul_div_data_rs1)) < abs(signed(mul_div_data_rs2)) then
            --    MUL_DIV_WB <= (others => '0');
              elsif pass_BEQ then
                if mul_div_data_rs2(31) = mul_div_data_rs1(31) then
                  MUL_DIV_WB <= (31 downto 1 => '0') & '1';
                else
                  MUL_DIV_WB <= (31 downto 0 => '1');
                end if;
              else
                if mul_div_data_rs1(31) = mul_div_data_rs2(31) then
                  MUL_DIV_WB <= res(31 downto 0);
                else
                  MUL_DIV_WB <= std_logic_vector(unsigned(not(res(31 downto 0)))+1);
                end if;
              end if;
            end if;

            if decoded_instruction_MUL_DIV(REMU_bit_position) = '1' then
              if div_count(5) = '1' or div_bypass_en = '1' then
                MUL_DIV_WB_EN <= '1';
              end if;
              if zero_rs2 = '1' then
                MUL_DIV_WB <= mul_div_data_rs1;
              elsif zero_rs1 = '1' then
                MUL_DIV_WB <= (others => '0');
              elsif pass_BEQ then
                MUL_DIV_WB <= (others => '0');
              elsif pass_BLTU then
                MUL_DIV_WB <= mul_div_data_rs1;
              else
                MUL_DIV_WB <= res(63 downto 32);
              end if;
            end if;

            if decoded_instruction_MUL_DIV(REM_bit_position) = '1' then
              if div_count(5) = '1'  or div_bypass_en = '1' then
                MUL_DIV_WB_EN <= '1';
              end if;
              if zero_rs2 = '1' then
                MUL_DIV_WB <= mul_div_data_rs1;
              elsif zero_rs1 = '1' then
                MUL_DIV_WB <= (others => '0');
              elsif pass_BEQ then
                MUL_DIV_WB <= (others => '0');
          --    elsif abs(signed(mul_div_data_rs1)) < abs(signed(mul_div_data_rs2)) then
            --    MUL_DIV_WB <= mul_div_data_rs1;
          --    elsif abs(signed(mul_div_data_rs1)) = abs(signed(mul_div_data_rs2)) then
            --    MUL_DIV_WB <= (others => '0');
              else
                if mul_div_data_rs1(31) = '1' then
                  MUL_DIV_WB <= std_logic_vector(unsigned(not(res(63 downto 32)))+1);
                else
                  MUL_DIV_WB <= res(63 downto 32);
                end if;
              end if;
            end if;

          -- EXECUTE OF INSTRUCTION (END) --------------------------
          end if;  -- instr_rvalid_IE values
          
        when others =>
          null;
      end case;  -- fsm_IE state cases
    end if;  -- reset, clk_i
  end process;

  fsm_IE_comb : process(all)
    variable core_busy_IE_wires      : std_logic;
  begin
    core_busy_IE_wires         := '0';
    mul_div_data_rs1_int_wire  <= mul_div_data_rs1;
    mul_div_data_rs2_int_wire  <= mul_div_data_rs2_int;
    partial_mult_a_wire        <= (others => '0');
    partial_mult_b_wire        <= (others => '0');
    partial_mult_c_wire        <= (others => '0');
    partial_mult_d_wire        <= (others => '0');
    MUL_int                    <= (others => '0');
    MUL                        <= (others => '0');
    div_count_wire             <= (others => '0');
    res_wire                   <= (others => '0');
    sub                        <= (others => '0');
    nextstate_MUL_DIV          <= normal;
    nextstate_mul              <= init;
    nextstate_div              <= init;

    if rst_ni = '0' then
      if fetch_enable_i = '0' then
        busy_MUL_DIV <= '1';
      end if;
      nextstate_MUL_DIV <= normal;  -- ignored, but allows combinational synthesis
    else
      case state_MUL_DIV is                  -- stage status

        when debug =>
          if dbg_req_o = '0' then
            nextstate_MUL_DIV <= normal;
          else
            nextstate_MUL_DIV <= debug;
            busy_MUL_DIV <= '1';
          end if;

        when normal =>

          if  MUL_DIV_instr = '0' then
            MUL_DIV_WB_EN <= '0';
          elsif branch_miss = '1' and speculative_mul_div = '1' then
            MUL_DIV_WB_EN <= '0';
          elsif irq_pending = '1' then
            MUL_DIV_WB_EN <= '0';
          else
            -- EXECUTE OF INSTRUCTION ---------------------
            if decoded_instruction_MUL_DIV(MUL_bit_position)    = '1' or 
               decoded_instruction_MUL_DIV(MULH_bit_position)   = '1' or
               decoded_instruction_MUL_DIV(MULHU_bit_position)  = '1' or
               decoded_instruction_MUL_DIV(MULHSU_bit_position) = '1' then
              case state_mul is
                when init =>
                  if mul_div_data_rs1(31) = '1' and signed_op = '1' then
                    mul_div_data_rs1_int_wire <= std_logic_vector(signed(not(mul_div_data_rs1))+1);
                  else
                    mul_div_data_rs1_int_wire <= mul_div_data_rs1;
                  end if;
                  if mul_div_data_rs2(31) = '1' and signed_op = '1' and decoded_instruction_MUL_DIV(MULHSU_bit_position) = '0' then
                    mul_div_data_rs2_int_wire <= std_logic_vector(signed(not(mul_div_data_rs2))+1);
                  else
                    mul_div_data_rs2_int_wire <= mul_div_data_rs2;
                  end if;
                  nextstate_mul <= mult;
                  busy_MUL_DIV <= '1';
                when mult =>
                  --MUL_PART1_wire <= std_logic_vector(unsigned(mul_div_data_rs1_int_wire)*
                  --                                   unsigned(mul_div_data_rs2_int_wire(15 downto 0)));
                  --MUL_PART2_wire <= std_logic_vector(unsigned(mul_div_data_rs1_int_wire)*
                  --                                   unsigned(mul_div_data_rs2_int_wire(31 downto 16)));
                  partial_mult_a_wire <= std_logic_vector( unsigned(mul_div_data_rs1_int(31 downto 16)) 
                                                    * unsigned(mul_div_data_rs2_int(31 downto 16)));
                  partial_mult_b_wire <= std_logic_vector( unsigned(mul_div_data_rs1_int(15 downto 0))   
                                                    * unsigned(mul_div_data_rs2_int(31 downto 16)));
                  partial_mult_c_wire <= std_logic_vector( unsigned(mul_div_data_rs1_int(31 downto 16)) 
                                                    * unsigned(mul_div_data_rs2_int(15 downto 0)));
                  partial_mult_d_wire <= std_logic_vector( unsigned(mul_div_data_rs1_int(15 downto 0))   
                                                    * unsigned(mul_div_data_rs2_int(15 downto 0)));
                  nextstate_mul <= accum;
                  busy_MUL_DIV <= '1';
                when accum =>
              --MUL <= std_logic_vector(signed(MSB_RS_OP(0) & (mul_div_data_rs1))*
              --                        signed(MSB_RS_OP(1) & (mul_div_data_rs2)));
              --MUL_int <= std_logic_vector((unsigned(MUL_PART2) & x"0000")+(x"0000" & unsigned(MUL_PART1)));
                  MUL_int <= std_logic_vector((        unsigned(partial_mult_a) & unsigned(partial_mult_d)) +
                                            (x"0000" & unsigned(partial_mult_b) & x"0000")                  +
                                            (x"0000" & unsigned(partial_mult_c) & x"0000"));
                  if (mul_div_data_rs1(31) /= mul_div_data_rs2(31) and decoded_instruction_MUL_DIV(MULH_bit_position) = '1') or
                                        (mul_div_data_rs1(31) = '1' and decoded_instruction_MUL_DIV(MULHSU_bit_position) = '1') then
                    MUL <= std_logic_vector(signed(not(MUL_int))+1);
                  else
                    MUL <= MUL_int;
                  end if;
              end case;
            end if;

            if decoded_instruction_MUL_DIV(DIV_bit_position)  = '1' or 
               decoded_instruction_MUL_DIV(REM_bit_position)  = '1' or
               decoded_instruction_MUL_DIV(DIVU_bit_position) = '1' or 
               decoded_instruction_MUL_DIV(REMU_bit_position) = '1' then
              case state_div is
                when init =>
                  if mul_div_data_rs1(31) = '0' or signed_op = '0' then
                    res_wire <= (31 downto 0 => '0') & mul_div_data_rs1;
                  else
                    mul_div_data_rs1_int_wire <= std_logic_vector(signed(not(mul_div_data_rs1)) + 1);
                    res_wire <= (31 downto 0  => '0') & mul_div_data_rs1_int_wire;
                  end if;
                  if mul_div_data_rs2(31) = '0' or signed_op = '0' then
                    mul_div_data_rs2_int_wire <= mul_div_data_rs2;
                  else
                    mul_div_data_rs2_int_wire <= std_logic_vector(signed(not(mul_div_data_rs2)) + 1);
                  end if;
                  nextstate_div <= divide;
                  busy_MUL_DIV <= '1';
                when divide =>
                  if div_count(5) /= '1' then
                    div_count_wire <= div_count + 1;
                    nextstate_div <= divide;
                    busy_MUL_DIV <= '1';
                  end if;
                  if sub(32) = '1' then -- mul_div_data_rs2 is the divisor
                    res_wire <= res(62 downto 0) & '0';
                  else
                    res_wire <= sub(31 downto 0) & res(30 downto 0) & '1';
                  end if;
                  sub <= std_logic_vector(('0' & unsigned(res(62 downto 31))) - ('0' & unsigned(mul_div_data_rs2_int)));
              end case; 
            end if;

          end if;
        when others =>
          null;
      end case;
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
    pass_BLTU <= '0';
    zero_rs1  <= '0';
    zero_rs2  <= '0';
    if MUL_DIV_instr = '1' then
      if unsigned(mul_div_data_rs1) = 0 then
        zero_rs1 <= '1';
      end if;
      if unsigned(mul_div_data_rs2) = 0 then
        zero_rs2 <= '1';
      end if;
      if (signed(mul_div_data_rs1) = signed(mul_div_data_rs2)) then
        pass_BEQ <= '1';
      end if;
      if (unsigned(mul_div_data_rs1) < unsigned(mul_div_data_rs2)) then
        pass_BLTU <= '1';
      end if;
    end if;
  end process;


end MUL_DIV;