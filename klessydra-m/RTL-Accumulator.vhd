--------------------------------------------------------------------------------------------------------
--  Accumulator --                                                                                    --
--  Author(s): Abdallah Cheikh abdallah.cheikh@uniroma1.it (abdallah93.as@gmail.com)                  --
--                                                                                                    --
--  Date Modified: 17-11-2019                                                                         --
--------------------------------------------------------------------------------------------------------
--  The accumulator performs a a reduction using addition on three instructions. KVRED, KDOTP, and    --
--  KDOTPPS. Eacj SIMD configuration has it's own accumulator, repllicating the dsp will also         --
--  replicate the accumulator as well.                                                                --
--------------------------------------------------------------------------------------------------------


-- ieee packages ------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_misc.all;
use ieee.numeric_std.all;
use std.textio.all;

-- local packages ------------
use work.riscv_klessydra.all;
--use work.klessydra_parameters.all;

entity ACCUMULATOR is
    generic(
      multithreaded_accl_en : natural;
      SIMD                  : natural;
      --------------------------------
      ACCL_NUM              : natural;
      FU_NUM                : natural;
      Data_Width            : natural;
      SIMD_Width            : natural;
      SIMD_BITS             : natural
    );
	port(
      clk_i                             : in  std_logic;
      rst_ni                            : in  std_logic;
      MVTYPE_DSP                        : in  array_2d(ACCL_NUM-1 downto 0)(1 downto 0);
      accum_en                          : in  std_logic_vector(ACCL_NUM-1 downto 0);
      accum_stage_1_en                  : in  std_logic_vector(ACCL_NUM-1 downto 0);
      accum_stage_2_en                  : in  std_logic_vector(ACCL_NUM-1 downto 0);
      recover_state_wires               : in  std_logic_vector(ACCL_NUM-1 downto 0);
      halt_dsp_lat                      : in  std_logic_vector(ACCL_NUM-1 downto 0);
      state_DSP                         : in  array_2d(ACCL_NUM-1 downto 0)(1 downto 0);
      decoded_instruction_DSP_lat       : in  array_2d(ACCL_NUM-1 downto 0)(DSP_UNIT_INSTR_SET_SIZE -1 downto 0);
      dsp_in_accum_operands             : in  array_2d(FU_NUM-1 downto 0)(SIMD_Width-1 downto 0);
      dsp_out_accum_results             : out array_2d(FU_NUM-1 downto 0)(31 downto 0)
	);
end entity;
architecture ACCUM_STG of ACCUMULATOR is
  
  subtype accl_range is integer range ACCL_NUM-1 downto 0;
  subtype fu_range   is integer range FU_NUM-1 downto 0;

  signal reduction_tree              : array_3d(fu_range)(SIMD_BITS+1 downto 0)(SIMD_Width-1 downto 0);
  signal reduction_tree_wire         : array_3d(fu_range)(SIMD_BITS+1 downto 0)(SIMD_Width-1 downto 0);
  signal reduction_tree_en           : array_2d(fu_range)(SIMD_BITS+1 downto 0);
  signal dsp_out_accum_results_wire  : array_2d(FU_NUM-1 downto 0)(31 downto 0);


begin
--
--  -- The accumulator for the DSP unit written below for all SIMD widths
--
  ACCUM_replicated : for f in fu_range generate
--
--  ACCUM_SIMD_1 : if SIMD=1 generate
--    fsm_ACCUM_STAGE : process(clk_i, rst_ni)
--      variable h : integer;
--    begin
--      if rst_ni = '0' then
--      elsif rising_edge(clk_i) then
--        dsp_out_accum_results(f) <= (others => '0');
--        for g in 0 to (ACCL_NUM - FU_NUM) loop
--          if multithreaded_accl_en = 1 then
--            h := g;  -- set the spm rd/wr ports equal to the "for-loop"
--          elsif multithreaded_accl_en = 0 then
--            h := f;  -- set the spm rd/wr ports equal to the "for-generate" 
--          end if;
--          case state_DSP(h) is	
--            when dsp_exec =>
--              if decoded_instruction_DSP_lat(h)(KDOTP_bit_position)   = '1' or -- acccumulate 32-bit types
--                 decoded_instruction_DSP_lat(h)(KDOTPPS_bit_position) = '1' or
--                 decoded_instruction_DSP_lat(h)(KVRED_bit_position)   = '1' then
--                if MVTYPE_DSP(h) = "10" then
--                  if (accum_stage_1_en(h) = '1' or recover_state_wires(h) = '1') and halt_dsp_lat(h) = '0' then
--                    accum_partial_results_stg_1(f)(31 downto 0)  <= dsp_in_accum_operands(f)(31 downto 0);
--                  end if;
--                  if (accum_stage_2_en(h) = '1' or recover_state_wires(h) = '1') and halt_dsp_lat(h) = '0' then
--                    dsp_out_accum_results(f) <= std_logic_vector(unsigned(accum_partial_results_stg_1(f)(31 downto 0)) +
--                                                              unsigned(dsp_out_accum_results(f)));
--                  end if;
--                elsif MVTYPE_DSP(h) = "01" then
--                  if (accum_stage_1_en(h) = '1' or recover_state_wires(h) = '1') and halt_dsp_lat(h) = '0' then
--                    accum_partial_results_stg_1(f)(15 downto 0)  <= std_logic_vector(unsigned(dsp_in_accum_operands(f)(15 downto 0)) + unsigned(dsp_in_accum_operands(f)(31  downto 16)));
--                  end if;
--                  if (accum_stage_2_en(h) = '1' or recover_state_wires(h) = '1') and halt_dsp_lat(h) = '0' then
--                    dsp_out_accum_results(f) <= std_logic_vector(unsigned(accum_partial_results_stg_1(f)(15 downto 0))  + 
--                                                              unsigned(dsp_out_accum_results(f)));                
--                  end if;
--                elsif MVTYPE_DSP(h) = "00" then
--                  if (accum_stage_1_en(h) = '1' or recover_state_wires(h) = '1') and halt_dsp_lat(h) = '0' then
--                    accum_partial_results_stg_1(f)(7 downto 0)  <= std_logic_vector(unsigned(dsp_in_accum_operands(f)(7  downto 0))  + 
--                                                                                    unsigned(dsp_in_accum_operands(f)(15 downto 8)));
--
--                    accum_partial_results_stg_1(f)(15 downto 8) <= std_logic_vector(unsigned(dsp_in_accum_operands(f)(23 downto 16)) +
--                                                                                    unsigned(dsp_in_accum_operands(f)(31 downto 24)));
--                  end if;
--                  if (accum_stage_2_en(h) = '1' or recover_state_wires(h) = '1') and halt_dsp_lat(h) = '0' then
--                    dsp_out_accum_results(f) <= std_logic_vector(unsigned(accum_partial_results_stg_1(f)(7  downto 0)) +
--                                                                 unsigned(accum_partial_results_stg_1(f)(15 downto 8)) + 
--                                                                 unsigned(dsp_out_accum_results(f)));                
--                  end if;
--                end if;
--              end if;
--            when others =>
--              null;
--          end case;
--        end loop;
--      end if;
--    end process;
--  end generate ACCUM_SIMD_1;
--
--  ACCUM_SIMD_2 : if SIMD=2 generate
--    fsm_ACCUM_STAGE : process(clk_i, rst_ni)
--      variable h : integer;
--    begin
--      if rst_ni = '0' then
--      elsif rising_edge(clk_i) then
--        dsp_out_accum_results(f) <= (others => '0');
--        for g in 0 to (ACCL_NUM - FU_NUM) loop
--          if multithreaded_accl_en = 1 then
--            h := g;  -- set the spm rd/wr ports equal to the "for-loop"
--          elsif multithreaded_accl_en = 0 then
--            h := f;  -- set the spm rd/wr ports equal to the "for-generate" 
--          end if;
--          case state_DSP(h) is
--            when dsp_exec =>
--              if (decoded_instruction_DSP_lat(h)(KDOTP_bit_position)   = '1'  or -- acccumulate 32-bit types
--                  decoded_instruction_DSP_lat(h)(KDOTPPS_bit_position) = '1'  or
--                  decoded_instruction_DSP_lat(h)(KVRED_bit_position)   = '1') and
--                  MVTYPE_DSP(h) = "10" then
--                if (accum_stage_1_en(h) = '1' or recover_state_wires(h) = '1') and halt_dsp_lat(h) = '0' then
--                  accum_partial_results_stg_1(f)(31 downto 0)  <= std_logic_vector(unsigned(dsp_in_accum_operands(f)(31 downto 0))  + unsigned(dsp_in_accum_operands(f)(63  downto 32)));
--                end if;
--                if (accum_stage_2_en(h) = '1' or recover_state_wires(h) = '1') and halt_dsp_lat(h) = '0' then
--                  dsp_out_accum_results(f) <= std_logic_vector(unsigned(accum_partial_results_stg_1(f)(31 downto 0)) +
--                                                            unsigned(dsp_out_accum_results(f)));
--                end if;
--              elsif (decoded_instruction_DSP_lat(h)(KDOTP_bit_position)    = '1'  or  -- acccumulate 8-bit and 16-bit types
--                     decoded_instruction_DSP_lat(h)(KDOTPPS_bit_position)  = '1'  or 
--                     decoded_instruction_DSP_lat(h)(KVRED_bit_position)    = '1') and
--                    (MVTYPE_DSP(h) = "01" or MVTYPE_DSP(h) = "00") then
--                if (accum_stage_1_en(h) = '1' or recover_state_wires(h) = '1') and halt_dsp_lat(h) = '0' then
--                  accum_partial_results_stg_1(f)(15 downto 0)  <= std_logic_vector(unsigned(dsp_in_accum_operands(f)(15 downto 0))   + unsigned(dsp_in_accum_operands(f)(31  downto 16)));
--                  accum_partial_results_stg_1(f)(31 downto 16) <= std_logic_vector(unsigned(dsp_in_accum_operands(f)(47 downto 32))  + unsigned(dsp_in_accum_operands(f)(63 downto 48)));
--                end if;
--                if (accum_stage_2_en(h) = '1' or recover_state_wires(h) = '1') and halt_dsp_lat(h) = '0' then
--                  dsp_out_accum_results(f) <= std_logic_vector(unsigned(accum_partial_results_stg_1(f)(15 downto 0))  + 
--                                                            unsigned(accum_partial_results_stg_1(f)(31 downto 16)) +
--                                                            unsigned(dsp_out_accum_results(f)));                
--                end if;
--              end if;
--            when others =>
--              null;
--          end case;
--        end loop;
--      end if;
--    end process; 
--  end generate ACCUM_SIMD_2;
--
--  ACCUM_SIMD_4 : if SIMD=4 generate
--    fsm_ACCUM_STAGE : process(clk_i, rst_ni)
--      variable h : integer;
--    begin
--      if rst_ni = '0' then
--      elsif rising_edge(clk_i) then
--        dsp_out_accum_results(f) <= (others => '0');
--        for g in 0 to (ACCL_NUM - FU_NUM) loop
--          if multithreaded_accl_en = 1 then
--            h := g;  -- set the spm rd/wr ports equal to the "for-loop"
--          elsif multithreaded_accl_en = 0 then
--            h := f;  -- set the spm rd/wr ports equal to the "for-generate" 
--          end if;
--          case state_DSP(h) is
--            when dsp_exec =>
--              if (decoded_instruction_DSP_lat(h)(KDOTP_bit_position)   = '1'  or -- acccumulate 32-bit types
--                  decoded_instruction_DSP_lat(h)(KDOTPPS_bit_position) = '1'  or 
--                  decoded_instruction_DSP_lat(h)(KVRED_bit_position)   = '1') and
--                  MVTYPE_DSP(h) = "10" then
--                if (accum_stage_1_en(h) = '1' or recover_state_wires(h) = '1') and halt_dsp_lat(h) = '0' then
--                  accum_partial_results_stg_1(f)(31 downto 0)  <= std_logic_vector(unsigned(dsp_in_accum_operands(f)(31 downto 0))  + unsigned(dsp_in_accum_operands(f)(63  downto 32)));
--                  accum_partial_results_stg_1(f)(63 downto 32) <= std_logic_vector(unsigned(dsp_in_accum_operands(f)(95 downto 64)) + unsigned(dsp_in_accum_operands(f)(127 downto 96)));
--                end if;
--                if (accum_stage_2_en(h) = '1' or recover_state_wires(h) = '1') and halt_dsp_lat(h) = '0' then
--                  dsp_out_accum_results(f) <= std_logic_vector(unsigned(accum_partial_results_stg_1(f)(31 downto 0)) + 
--                                                            unsigned(accum_partial_results_stg_1(f)(63 downto 32)) +
--                                                            unsigned(dsp_out_accum_results(f)));
--                end if;
--              elsif (decoded_instruction_DSP_lat(h)(KDOTP_bit_position)    = '1'  or  -- acccumulate 8-bit and 16-bit types
--                     decoded_instruction_DSP_lat(h)(KDOTPPS_bit_position)  = '1'  or 
--                     decoded_instruction_DSP_lat(h)(KVRED_bit_position)    = '1') and
--                    (MVTYPE_DSP(h) = "01" or MVTYPE_DSP(h) = "00") then
--                if (accum_stage_1_en(h) = '1' or recover_state_wires(h) = '1') and halt_dsp_lat(h) = '0' then
--                  accum_partial_results_stg_1(f)(15 downto 0)  <= std_logic_vector(unsigned(dsp_in_accum_operands(f)(15 downto 0))   + unsigned(dsp_in_accum_operands(f)(31  downto 16)));
--                  accum_partial_results_stg_1(f)(31 downto 16) <= std_logic_vector(unsigned(dsp_in_accum_operands(f)(47 downto 32))  + unsigned(dsp_in_accum_operands(f)(63 downto 48)));
--                  accum_partial_results_stg_1(f)(47 downto 32) <= std_logic_vector(unsigned(dsp_in_accum_operands(f)(79 downto 64))  + unsigned(dsp_in_accum_operands(f)(95 downto 80)));
--                  accum_partial_results_stg_1(f)(63 downto 48) <= std_logic_vector(unsigned(dsp_in_accum_operands(f)(111 downto 96)) + unsigned(dsp_in_accum_operands(f)(127 downto 112)));
--                end if;
--                if (accum_stage_2_en(h) = '1' or recover_state_wires(h) = '1') and halt_dsp_lat(h) = '0' then
--                  dsp_out_accum_results(f) <= std_logic_vector(unsigned(accum_partial_results_stg_1(f)(15 downto 0))  + 
--                                                            unsigned(accum_partial_results_stg_1(f)(31 downto 16)) +
--                                                            unsigned(accum_partial_results_stg_1(f)(47 downto 32)) +
--                                                            unsigned(accum_partial_results_stg_1(f)(63 downto 48)) +
--                                                            unsigned(dsp_out_accum_results(f)));                
--                end if;
--              end if;
--            when others =>
--              null;
--          end case;
--        end loop;
--      end if;
--    end process;
--  end generate ACCUM_SIMD_4;
--
--  ACCUM_SIMD_8 : if SIMD=8 generate
--    fsm_ACCUM_STAGE : process(clk_i, rst_ni)
--      variable h : integer;
--    begin
--      if rst_ni = '0' then
--      elsif rising_edge(clk_i) then
--        dsp_out_accum_results(f) <= (others => '0');
--        for g in 0 to (ACCL_NUM - FU_NUM) loop
--          if multithreaded_accl_en = 1 then
--            h := g;  -- set the spm rd/wr ports equal to the "for-loop"
--          elsif multithreaded_accl_en = 0 then
--            h := f;  -- set the spm rd/wr ports equal to the "for-generate" 
--          end if;
--          case state_DSP(h) is
--            when dsp_exec =>
--              if (decoded_instruction_DSP_lat(h)(KDOTP_bit_position)   = '1'  or -- acccumulate 32-bit types
--                  decoded_instruction_DSP_lat(h)(KDOTPPS_bit_position) = '1'  or
--                  decoded_instruction_DSP_lat(h)(KVRED_bit_position)   = '1') and
--                  MVTYPE_DSP(h) = "10" then
--              if (accum_stage_1_en(h) = '1' or recover_state_wires(h) = '1') and halt_dsp_lat(h) = '0' then
--                accum_partial_results_stg_1(f)(31 downto 0)   <= std_logic_vector(unsigned(dsp_in_accum_operands(f)(31 downto 0))  + unsigned(dsp_in_accum_operands(f)(63  downto 32)));
--                accum_partial_results_stg_1(f)(63 downto 32)  <= std_logic_vector(unsigned(dsp_in_accum_operands(f)(95 downto 64)) + unsigned(dsp_in_accum_operands(f)(127 downto 96)));
--                accum_partial_results_stg_1(f)(95 downto 64)  <= std_logic_vector(unsigned(dsp_in_accum_operands(f)(159 downto 128)) + unsigned(dsp_in_accum_operands(f)(191 downto 160)));
--                accum_partial_results_stg_1(f)(127 downto 96) <= std_logic_vector(unsigned(dsp_in_accum_operands(f)(223 downto 192)) + unsigned(dsp_in_accum_operands(f)(255 downto 224)));
--              end if;
--              if (accum_stage_2_en(h) = '1' or recover_state_wires(h) = '1') and halt_dsp_lat(h) = '0' then
--                dsp_out_accum_results(f) <= std_logic_vector(unsigned(accum_partial_results_stg_1(f)(31  downto 0))  + 
--                                                          unsigned(accum_partial_results_stg_1(f)(63  downto 32)) +
--                                                          unsigned(accum_partial_results_stg_1(f)(95  downto 64)) +
--                                                          unsigned(accum_partial_results_stg_1(f)(127 downto 96)) +
--                                                          unsigned(dsp_out_accum_results(f)));
--              end if;
--              elsif (decoded_instruction_DSP_lat(h)(KDOTP_bit_position)    = '1'  or  -- acccumulate 8-bit and 16-bit types
--                     decoded_instruction_DSP_lat(h)(KDOTPPS_bit_position)  = '1'  or
--                     decoded_instruction_DSP_lat(h)(KVRED_bit_position)    = '1') and
--                    (MVTYPE_DSP(h) = "01" or MVTYPE_DSP(h) = "00") then
--                if (accum_stage_1_en(h) = '1' or recover_state_wires(h) = '1') and halt_dsp_lat(h) = '0' then
--                  accum_partial_results_stg_1(f)(15 downto 0)    <= std_logic_vector(unsigned(dsp_in_accum_operands(f)(15 downto 0))    + unsigned(dsp_in_accum_operands(f)(31  downto 16)));
--                  accum_partial_results_stg_1(f)(31 downto 16)   <= std_logic_vector(unsigned(dsp_in_accum_operands(f)(47 downto 32))   + unsigned(dsp_in_accum_operands(f)(63 downto 48)));
--                  accum_partial_results_stg_1(f)(47 downto 32)   <= std_logic_vector(unsigned(dsp_in_accum_operands(f)(79 downto 64))   + unsigned(dsp_in_accum_operands(f)(95 downto 80)));
--                  accum_partial_results_stg_1(f)(63 downto 48)   <= std_logic_vector(unsigned(dsp_in_accum_operands(f)(111 downto 96))  + unsigned(dsp_in_accum_operands(f)(127 downto 112)));
--                  accum_partial_results_stg_1(f)(79 downto 64)   <= std_logic_vector(unsigned(dsp_in_accum_operands(f)(143 downto 128)) + unsigned(dsp_in_accum_operands(f)(159 downto 144)));
--                  accum_partial_results_stg_1(f)(95 downto 80)   <= std_logic_vector(unsigned(dsp_in_accum_operands(f)(175 downto 160)) + unsigned(dsp_in_accum_operands(f)(191 downto 176)));
--                  accum_partial_results_stg_1(f)(111 downto 96)  <= std_logic_vector(unsigned(dsp_in_accum_operands(f)(207 downto 192)) + unsigned(dsp_in_accum_operands(f)(223 downto 208)));
--                  accum_partial_results_stg_1(f)(127 downto 112) <= std_logic_vector(unsigned(dsp_in_accum_operands(f)(239 downto 224)) + unsigned(dsp_in_accum_operands(f)(255 downto 240)));
--                end if;
--                if (accum_stage_2_en(h) = '1' or recover_state_wires(h) = '1') and halt_dsp_lat(h) = '0' then
--                  dsp_out_accum_results(f) <= std_logic_vector(unsigned(accum_partial_results_stg_1(f)(15  downto 0))  + 
--                                                            unsigned(accum_partial_results_stg_1(f)(31  downto 16)) +
--                                                            unsigned(accum_partial_results_stg_1(f)(47  downto 32)) +
--                                                            unsigned(accum_partial_results_stg_1(f)(63  downto 48)) +
--                                                            unsigned(accum_partial_results_stg_1(f)(79  downto 64)) +
--                                                            unsigned(accum_partial_results_stg_1(f)(95  downto 80)) +
--                                                            unsigned(accum_partial_results_stg_1(f)(111 downto 96)) +
--                                                            unsigned(accum_partial_results_stg_1(f)(127 downto 112)) +
--                                                            unsigned(dsp_out_accum_results(f)));                
--                end if;
--              end if;
--            when others =>
--              null;
--          end case;
--        end loop;
--      end if;
--    end process;
--  end generate ACCUM_SIMD_8;

    process(all)
      variable h : integer;
      variable tree_idx : natural := 0; -- A trick to avoid simulation errors in Modelsim / Questasim. As using a literal for an index will be detected during simulation time if the value is in range, but putting a variable as an index, will be only detected at runtime.
    begin
      reduction_tree_wire(f) <= reduction_tree(f);
      dsp_out_accum_results_wire(f) <= ( others => '0');
      for g in 0 to (ACCL_NUM - FU_NUM) loop
        if multithreaded_accl_en = 1 then
          h := g;  -- set the spm rd/wr ports equal to the "for-loop"
        elsif multithreaded_accl_en = 0 then
          h := f;  -- set the spm rd/wr ports equal to the "for-generate" 
        end if;
        if accum_en(h) = '1' then
          if reduction_tree_en(f)(SIMD_BITS) then
            dsp_out_accum_results_wire(f) <= std_logic_vector(unsigned(dsp_out_accum_results(f)) + unsigned(reduction_tree(f)(SIMD_BITS)(31 downto 0)));
          end if;
          if (MVTYPE_DSP(h) = "00") then
            for j in 0 to SIMD_BITS loop
              reduction_tree_wire(f)(tree_idx+1)(15+32*j downto 32*j) <= x"00" & std_logic_vector(unsigned(reduction_tree(f)(tree_idx)(7+32*j downto 32*j)) + unsigned(reduction_tree(f)(tree_idx)(23+32*j downto 16+32*j)));
            end loop;
          elsif (MVTYPE_DSP(h) = "01") then
            if SIMD >= 2 then
              for j in 0 to SIMD_BITS-1 loop
                reduction_tree_wire(f)(tree_idx+2)(31+32*j downto 32*j) <= x"0000" & std_logic_vector(unsigned(reduction_tree(f)(tree_idx+1)(15+64*j downto 64*j)) + unsigned(reduction_tree(f)(tree_idx+1)(47+64*j downto 32+64*j)));
              end loop;
            end if;
          else
            if SIMD > 4 then
              for i in 2 to SIMD_BITS loop
                for j in 0 to SIMD_BITS-1 loop
                  reduction_tree_wire(f)(i+1)(31+32*j downto 32*j) <= std_logic_vector(unsigned(reduction_tree(f)(i)(31+64*j downto 64*j)) + unsigned(reduction_tree(f)(i)(63+64*j downto 32+64*j)));
                end loop;
              end loop;
            end if;
          end if;
        end if;
      end loop;
    end process;

    process(clk_i, rst_ni)
      variable h : integer;
      variable tree_idx : natural := 0; -- A trick to avoid simulation errors in Modelsim / Questasim. As using a literal for an index will be detected during simulation time if the value is in range, but putting a variable as an index, will be only detected at runtime.
    begin
      if rst_ni = '0' then
      elsif rising_edge(clk_i) then
        reduction_tree(f) <= reduction_tree_wire(f);
        dsp_out_accum_results(f) <= dsp_out_accum_results_wire(f);
        for g in 0 to (ACCL_NUM - FU_NUM) loop
          if multithreaded_accl_en = 1 then
            h := g;  -- set the spm rd/wr ports equal to the "for-loop"
          elsif multithreaded_accl_en = 0 then
            h := f;  -- set the spm rd/wr ports equal to the "for-generate" 
          end if;
          reduction_tree_en(f)(0) <= accum_stage_1_en(h);
          for i in 0 to SIMD_BITS-1 loop
            reduction_tree_en(f)(i+1) <= reduction_tree_en(f)(i);
          end loop;
          if (accum_stage_1_en(h) = '1' or recover_state_wires(h) = '1') and halt_dsp_lat(h) = '0' then
            if (MVTYPE_DSP(h) = "00") then
              for j in 0 to SIMD-1 loop
                reduction_tree(f)(tree_idx)(15+16*j downto 16*j) <= x"00" & std_logic_vector(unsigned(dsp_in_accum_operands(f)( 7+16*j downto  0+16*j)) + 
                                                                                             unsigned(dsp_in_accum_operands(f)(15+16*j downto  8+16*j))
                                                                                            );
              end loop;
            elsif (MVTYPE_DSP(h) = "01") then
              if SIMD = 1 then
                dsp_out_accum_results(f)(31 downto 0) <= x"0000" & std_logic_vector(unsigned(dsp_in_accum_operands(f)(15 downto  0)) + 
                                                                                    unsigned(dsp_in_accum_operands(f)(31 downto 16))
                                                                                   );
              else
                for j in 0 to SIMD-1 loop
                  reduction_tree(f)(tree_idx+1)(15+16*j downto 16*j) <= std_logic_vector(unsigned(dsp_in_accum_operands(f)(15+32*j downto  0+32*j)) + 
                                                                                         unsigned(dsp_in_accum_operands(f)(31+32*j downto 16+32*j))
                                                                                        );
                end loop;
              end if;
            elsif (MVTYPE_DSP(h) = "10") then
              if SIMD = 1 then -- just pass the operand to the final stage
                dsp_out_accum_results <= dsp_in_accum_operands;
              elsif SIMD = 2 then
                dsp_out_accum_results(f)(31 downto 0) <= std_logic_vector(unsigned(dsp_in_accum_operands(f)(31 downto  0)) + 
                                                                          unsigned(dsp_in_accum_operands(f)(63 downto 32))
                                                                         );
              elsif SIMD > 2 then
                for j in 0 to SIMD-1 loop
                  reduction_tree(f)(tree_idx+2)(31+32*j downto 32*j) <= std_logic_vector(unsigned(dsp_in_accum_operands(f)(31+32*j downto  0+32*j)) + 
                                                                                         unsigned(dsp_in_accum_operands(f)(63+32*j downto 32+32*j))
                                                                                        );
                end loop;
              end if;
            end if;
          end if;
        end loop;
      end if;
    end process;

  end generate ACCUM_replicated;

------------------------------------------------------------------------ end of ACCUM Unit -------
--------------------------------------------------------------------------------------------------

end ACCUM_STG;
--------------------------------------------------------------------------------------------------
-- END of ACCUM Unit architecture ----------------------------------------------------------------
--------------------------------------------------------------------------------------------------
