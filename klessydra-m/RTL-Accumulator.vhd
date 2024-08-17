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
      THREAD_POOL_SIZE      : natural;
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
      recover_state_wires               : in  std_logic_vector(ACCL_NUM-1 downto 0);
      halt_dsp_lat                      : in  std_logic_vector(ACCL_NUM-1 downto 0);
      state_DSP                         : in  array_2d(ACCL_NUM-1 downto 0)(1 downto 0);
      decoded_instruction_DSP_lat       : in  array_2d(ACCL_NUM-1 downto 0)(DSP_UNIT_INSTR_SET_SIZE -1 downto 0);
      dsp_in_accum_operands             : in  array_2d(FU_NUM-1 downto 0)(SIMD_Width-1 downto 0);
      dsp_out_accum_results             : out array_2d(FU_NUM-1 downto 0)(Data_Width-1 downto 0);
      dsp_out_accum_en                  : out std_logic_vector(ACCL_NUM-1 downto 0)
	);
end entity;
architecture ACCUM_STG of ACCUMULATOR is
  
  subtype accl_range is integer range ACCL_NUM-1 downto 0;
  subtype fu_range   is integer range FU_NUM-1 downto 0;

  signal a                          : array_2d(fu_range)(SIMD_Width-1 downto 0);
  signal b                          : array_2d(fu_range)(SIMD_Width-1 downto 0);
  signal c                          : array_2d(fu_range)(SIMD_Width-1 downto 0);
  signal d                          : array_2d(fu_range)(SIMD_Width-1 downto 0);
  signal e                          : array_2d(fu_range)(Data_Width-1 downto 0);
  signal reduction_tree             : array_3d(fu_range)(SIMD_BITS+1 downto 0)(SIMD_Width-1 downto 0);
  signal reduction_tree_wire        : array_3d(fu_range)(SIMD_BITS+1 downto 0)(SIMD_Width-1 downto 0);
  signal reduction_tree_en          : array_2d(fu_range)(SIMD_BITS+1 downto 0);
  signal dsp_out_accum_results_wire : array_2d(fu_range)(Data_Width-1 downto 0);
  signal dsp_out_accum_en_wire      : std_logic_vector(accl_range);
  signal en_res                     : std_logic_vector(fu_range);
  signal harc_active                : natural range THREAD_POOL_SIZE-1 downto 0;                   


begin
--
--  -- The accumulator for the DSP unit written below for all SIMD widths
--
  ACCUM_replicated : for f in fu_range generate

  EN_MAP : if multithreaded_accl_en = 0 generate

    process(all)
      variable h : integer;
    begin
      for g in 0 to (ACCL_NUM - FU_NUM) loop
        if multithreaded_accl_en = 1 then
          h := g;  -- set the spm rd/wr ports equal to the "for-loop"
        elsif multithreaded_accl_en = 0 then
          h := f;  -- set the spm rd/wr ports equal to the "for-generate" 
        end if;
        dsp_out_accum_en_wire(f) <= en_res(f);
        en_res(f) <= '0';
        if SIMD = 1 and MVTYPE_DSP(h) = "10" then
          e(f) <= dsp_in_accum_operands(f)(Data_Width-1 downto 0);
        else
          e(f) <= reduction_tree(f)(SIMD_BITS+1)(Data_Width-1 downto 0);
        end if;
        if (MVTYPE_DSP(h) = "00") then
          for i in 0 to SIMD-1 loop
            a(f)(15+16*i downto 16*i) <= x"00" & reduction_tree(f)(0)( 7+32*i downto    32*i);
            b(f)(15+16*i downto 16*i) <= x"00" & reduction_tree(f)(0)(23+32*i downto 16+32*i);
          end loop;
          if (SIMD > 1) then
            for i in 0 to (SIMD/2)-1 loop
              c(f)(31+32*i downto 32*i) <= x"000000" & reduction_tree(f)(1)( 7+64*i downto    64*i);
              d(f)(31+32*i downto 32*i) <= x"000000" & reduction_tree(f)(1)(39+64*i downto 32+64*i);
            end loop;
          end if;
          en_res(f) <= reduction_tree_en(f)(SIMD_BITS+1);
        elsif (MVTYPE_DSP(h) = "01") then
          for i in 0 to SIMD-1 loop
            a(f)(15+16*i downto 16*i) <= dsp_in_accum_operands(f)(15+32*i downto  0+32*i);
            b(f)(15+16*i downto 16*i) <= dsp_in_accum_operands(f)(31+32*i downto 16+32*i);
          end loop;
          if (SIMD > 1) then
            for i in 0 to (SIMD/2)-1 loop
              c(f)(31+32*i downto 32*i) <= x"0000" & reduction_tree(f)(1)(15+64*i downto    64*i);
              d(f)(31+32*i downto 32*i) <= x"0000" & reduction_tree(f)(1)(47+64*i downto 32+64*i);
            end loop;
          end if;
          en_res(f) <= reduction_tree_en(f)(SIMD_BITS+1);
        else -- MVTYPE_DSP(h) = "10"
          if (SIMD = 1 ) then
            en_res(f) <= accum_stage_1_en(f); -- reduction tree is skipped for SIMD_1 and VTYPE = XLEN
          else -- SIMD > 1
            en_res(f) <= reduction_tree_en(f)(SIMD_BITS+1);
            for i in 0 to (SIMD/2)-1 loop
              c(f)(31+32*i downto 32*i) <= dsp_in_accum_operands(f)(31+64*i downto    64*i);
              d(f)(31+32*i downto 32*i) <= dsp_in_accum_operands(f)(63+64*i downto 32+64*i);
            end loop;
          end if;
        end if;
      end loop;
    end process;

    process(clk_i, rst_ni)
    begin
      if rst_ni = '0' then
        dsp_out_accum_en(f) <= '0';
      elsif rising_edge(clk_i) then
        dsp_out_accum_en(f) <= dsp_out_accum_en_wire(f);
      end if;
    end process;

  end generate EN_MAP; 

  EN_MAP_MTH : if multithreaded_accl_en = 1 generate

    process(all)
      variable h        : integer;
    begin
      for g in 0 to (ACCL_NUM - FU_NUM) loop
        if multithreaded_accl_en = 1 then
          h := g;  -- set the spm rd/wr ports equal to the "for-loop"
        elsif multithreaded_accl_en = 0 then
          h := f;  -- set the spm rd/wr ports equal to the "for-generate" 
        end if;
        dsp_out_accum_en_wire(h) <= '0';
        if accum_en(h) then
          dsp_out_accum_en_wire(h) <= en_res(f);
          harc_active <= h;
        end if;
      end loop;
      en_res(f) <= '0';
      if SIMD = 1 and MVTYPE_DSP(harc_active) = "10" then
        e(f) <= dsp_in_accum_operands(f)(Data_Width-1 downto 0);
      else
        e(f) <= reduction_tree(f)(SIMD_BITS+1)(Data_Width-1 downto 0);
      end if;
      if (MVTYPE_DSP(harc_active) = "00") then
        for i in 0 to SIMD-1 loop
          a(f)(15+16*i downto 16*i) <= x"00" & reduction_tree(f)(0)( 7+32*i downto    32*i);
          b(f)(15+16*i downto 16*i) <= x"00" & reduction_tree(f)(0)(23+32*i downto 16+32*i);
        end loop;
        if (SIMD > 1) then
          for i in 0 to (SIMD/2)-1 loop
            c(f)(31+32*i downto 32*i) <= x"000000" & reduction_tree(f)(1)( 7+64*i downto    64*i);
            d(f)(31+32*i downto 32*i) <= x"000000" & reduction_tree(f)(1)(39+64*i downto 32+64*i);
          end loop;
        end if;
        en_res(f) <= reduction_tree_en(f)(SIMD_BITS+1);
      elsif (MVTYPE_DSP(harc_active) = "01") then
        for i in 0 to SIMD-1 loop
          a(f)(15+16*i downto 16*i) <= dsp_in_accum_operands(f)(15+32*i downto  0+32*i);
          b(f)(15+16*i downto 16*i) <= dsp_in_accum_operands(f)(31+32*i downto 16+32*i);
        end loop;
        if (SIMD > 1) then
          for i in 0 to (SIMD/2)-1 loop
            c(f)(31+32*i downto 32*i) <= x"0000" & reduction_tree(f)(1)(15+64*i downto    64*i);
            d(f)(31+32*i downto 32*i) <= x"0000" & reduction_tree(f)(1)(47+64*i downto 32+64*i);
          end loop;
        end if;
        en_res(f) <= reduction_tree_en(f)(SIMD_BITS+1);
      else -- MVTYPE_DSP(harc_active) = "10"
        if (SIMD = 1 ) then
          en_res(f) <= accum_stage_1_en(harc_active); -- reduction tree is skipped for SIMD_1 and VTYPE = XLEN
        else -- SIMD > 1
          en_res(f) <= reduction_tree_en(f)(SIMD_BITS+1);
          for i in 0 to (SIMD/2)-1 loop
            c(f)(31+32*i downto 32*i) <= dsp_in_accum_operands(f)(31+64*i downto    64*i);
            d(f)(31+32*i downto 32*i) <= dsp_in_accum_operands(f)(63+64*i downto 32+64*i);
          end loop;
        end if;
      end if;
    end process;


    process(clk_i, rst_ni)
      variable h : integer;
    begin
      if rst_ni = '0' then
      elsif rising_edge(clk_i) then
        dsp_out_accum_en(f) <= dsp_out_accum_en_wire(f);
        for g in 0 to (ACCL_NUM - FU_NUM) loop
          if multithreaded_accl_en = 1 then
            h := g;  -- set the spm rd/wr ports equal to the "for-loop"
          elsif multithreaded_accl_en = 0 then
            h := f;  -- set the spm rd/wr ports equal to the "for-generate" 
          end if;
          dsp_out_accum_en(h) <= dsp_out_accum_en_wire(h);
        end loop;
      end if;
    end process;

  end generate EN_MAP_MTH; 



  EN_MAP_OUT : if multithreaded_accl_en = 0 generate

  end generate EN_MAP_OUT; 

    process(all)
      variable h        : integer;
      variable tree_idx : natural := 0; -- A trick to avoid simulation errors in Modelsim / Questasim. As using a literal for an index will be detected during simulation time if the value is in range, but putting a variable as an index, will be only detected at runtime.
    begin
      reduction_tree_wire(f) <= reduction_tree(f);
      dsp_out_accum_results_wire(f) <= ( others => '0');
      for g in 0 to (ACCL_NUM - FU_NUM) loop
        if multithreaded_accl_en = 1 then
          h := g;  -- set the spm rd/wr ports equal to the "for-loop"
          if (MVTYPE_DSP(harc_active) = "00") then
            for i in 0 to (2*SIMD)-1 loop
              reduction_tree_wire(f)(0)(15+16*i downto 16*i) <= x"00" & std_logic_vector(unsigned(dsp_in_accum_operands(f)( 7+16*i downto   16*i)) + 
                                                                                         unsigned(dsp_in_accum_operands(f)(15+16*i downto 8+16*i)));
            end loop;
          end if;
        elsif multithreaded_accl_en = 0 then
          h := f;  -- set the spm rd/wr ports equal to the "for-generate" 
          if (MVTYPE_DSP(h) = "00") then
            for i in 0 to (2*SIMD)-1 loop
              reduction_tree_wire(f)(0)(15+16*i downto 16*i) <= x"00" & std_logic_vector(unsigned(dsp_in_accum_operands(f)( 7+16*i downto   16*i)) + 
                                                                                         unsigned(dsp_in_accum_operands(f)(15+16*i downto 8+16*i)));
            end loop;
          end if;
        end if;
        for i in 0 to SIMD-1 loop
          reduction_tree_wire(f)(1)(31+32*i downto 32*i) <= x"0000" & std_logic_vector(unsigned(a(f)(15+16*i downto 16*i)) +
                                                                                       unsigned(b(f)(15+16*i downto 16*i)));
        end loop;
        if (SIMD > 1) then
          for i in 0 to SIMD-1 loop
            reduction_tree_wire(f)(tree_idx+2)(31+32*i downto 32*i) <= std_logic_vector(unsigned(c(f)(31+32*i downto 32*i)) +
                                                                                        unsigned(d(f)(31+32*i downto 32*i)));
          end loop;
        end if;
        if (SIMD > 2) then
          for i in 2 to SIMD_BITS loop
            for j in 0 to (SIMD/2)-1-(i-2) loop
              reduction_tree_wire(f)(i+1)(31+32*j downto 32*j) <= std_logic_vector(unsigned(reduction_tree(f)(i)(31+64*j downto    64*j)) +
                                                                                   unsigned(reduction_tree(f)(i)(63+64*j downto 32+64*j)));
            end loop;
          end loop;
        end if;
        -- SIMD 1 for XLEN-bits goes directly to the output and does not pass through the reduction tree stages
        if en_res(f) then
          dsp_out_accum_results_wire(f) <= std_logic_vector(unsigned(dsp_out_accum_results(f)) + 
                                                            unsigned(e(f)));
        end if;
      end loop;
    end process;

    process(clk_i, rst_ni)
      variable h : integer;
      variable tree_idx : natural := 0; -- A trick to avoid simulation errors in Modelsim / Questasim. As using a literal for an index will be detected during simulation time if the value is in range, but putting a variable as an index, will be only detected at runtime.
    begin
      if rst_ni = '0' then
        reduction_tree(f)(0)     <= (others => '0');
        reduction_tree_en(f)     <= (others => '0');
      elsif rising_edge(clk_i) then
        reduction_tree(f)        <= reduction_tree_wire(f);
        dsp_out_accum_results(f) <= dsp_out_accum_results_wire(f);
        for g in 0 to (ACCL_NUM - FU_NUM) loop
          if multithreaded_accl_en = 1 then
            h := g;  -- set the spm rd/wr ports equal to the "for-loop"
          elsif multithreaded_accl_en = 0 then
            h := f;  -- set the spm rd/wr ports equal to the "for-generate" 
          end if;
          if multithreaded_accl_en = 0 then
            if (MVTYPE_DSP(h) = "00") then
              reduction_tree_en(f)(0) <= accum_stage_1_en(f); -- AAA try to replace index '0' by MVTYPE_DSP
              for i in 0 to SIMD_BITS loop
                reduction_tree_en(f)(i+1) <= reduction_tree_en(f)(i);
              end loop;
            elsif (MVTYPE_DSP(h) = "01") then
              reduction_tree_en(f)(1) <= accum_stage_1_en(f);
              if (SIMD > 1) then
                for i in 1 to SIMD_BITS loop
                  reduction_tree_en(f)(i+1) <= reduction_tree_en(f)(i);
                end loop;
              end if;
            elsif (MVTYPE_DSP(h) = "10") then
              if SIMD > 1 then
                reduction_tree_en(f)(tree_idx+2) <= accum_stage_1_en(f);
                if (SIMD > 2) then
                  for i in 2 to SIMD_BITS loop
                    reduction_tree_en(f)(i+1) <= reduction_tree_en(f)(i);
                  end loop;
                end if;
              end if;
            end if;
          elsif multithreaded_accl_en = 1 then
            if (MVTYPE_DSP(harc_active) = "00") then
              reduction_tree_en(f)(0) <= or_vect_bits(accum_stage_1_en);
              for i in 0 to SIMD_BITS loop
                reduction_tree_en(f)(i+1) <= reduction_tree_en(f)(i);
              end loop;
            elsif (MVTYPE_DSP(harc_active) = "01") then
              reduction_tree_en(f)(1) <= or_vect_bits(accum_stage_1_en);
              if (SIMD > 1) then
                for i in 1 to SIMD_BITS loop
                  reduction_tree_en(f)(i+1) <= reduction_tree_en(f)(i);
                end loop;
              end if;
            elsif (MVTYPE_DSP(harc_active) = "10") then
              if SIMD > 1 then
                reduction_tree_en(f)(tree_idx+2) <= or_vect_bits(accum_stage_1_en);
                if (SIMD > 2) then
                  for i in 2 to SIMD_BITS loop
                    reduction_tree_en(f)(i+1) <= reduction_tree_en(f)(i);
                  end loop;
                end if;
              end if;
            end if;
          end if;
          if (accum_stage_1_en(h) = '1' or recover_state_wires(h) = '1') and halt_dsp_lat(h) = '0' then
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
