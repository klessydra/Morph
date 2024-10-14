
-- ieee packages ------------
library IEEE;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- local packages ------------
use work.riscv_klessydra.all;

entity fpu_top_wrapper is  
  generic (
    THREAD_POOL_SIZE : natural;
    fp_size          : natural;
    fp_exp_size      : natural;
    fp_mnt_size      : natural;
    fp_bias          : natural
  );
  port (
    clk_i                     : in  std_logic;
    rst_ni                    : in  std_logic;
    float_instr_req           : in  std_logic;
    harc_EXEC                 : in  natural range THREAD_POOL_SIZE-1 downto 0;
    instr_word_IE             : in  std_logic_vector(31 downto 0);
    decoded_instruction_FLOAT : in  std_logic_vector(FP_UNIT_INSTR_SET_SIZE-1 downto 0);
    RS1_DATA_FLOAT            : in  std_logic_vector(fp_size-1 downto 0);
    RS2_DATA_FLOAT            : in  std_logic_vector(fp_size-1 downto 0);
    RD_DATA_FLOAT             : in  std_logic_vector(fp_size-1 downto 0);
    harc_FP_RES_WB            : out natural range THREAD_POOL_SIZE-1 downto 0;
    instr_word_FP_RES_WB      : out std_logic_vector(31  downto 0);
    busy_FPU                  : out std_logic;
    FP_RES_WB_EN              : out std_logic;
    FP_RES_WB                 : out std_logic_vector(fp_size-1 downto 0);
    float_flag                : out std_logic_vector(4 downto 0)
  );
end entity fpu_top_wrapper;

architecture behavioral of fpu_top_wrapper is

  component fpu_top is
    generic (
      size          : natural;
      exponent_size : natural;
      mantissa_size : natural;
      bias          : natural
    );
    port (
      clk_i            : in  std_logic;
      rst_ni           : in  std_logic;
      op_mode          : in  std_logic_vector(FP_UNIT_INSTR_SET_SIZE-1 downto 0);
      round_mode       : in  std_logic_vector(7 downto 0);
      data_a           : in  std_logic_vector(size-1 downto 0);
      data_b           : in  std_logic_vector(size-1 downto 0);
      data_c           : in  std_logic_vector(size-1 downto 0);
      valid_i          : in  std_logic;
      result           : out std_logic_vector(size-1 downto 0);
      div_ready        : out std_logic;
      fp_div_ready     : out std_logic;
      fp_sqrt_ready    : out std_logic;
      flag_uf          : out std_logic;
      flag_of          : out std_logic;
      flag_in          : out std_logic;
      flag_dz          : out std_logic;
      flag_nx          : out std_logic
    );
  end component fpu_top;

  signal div_ready : std_logic;
  signal fp_div_ready  : std_logic;
  signal fp_sqrt_ready : std_logic;
  signal div_done  : std_logic;
  signal FP_RES_WB_wire : std_logic_vector(fp_size-1 downto 0);

begin

  KFPU_INST : fpu_top
  generic map(
    size          => fp_size,
    exponent_size => fp_exp_size,
    mantissa_size => fp_mnt_size,
    bias          => fp_bias
  )
  port map(
    clk_i         => clk_i,
    rst_ni        => rst_ni,
    op_mode       => decoded_instruction_FLOAT,
    --round_mode => instr_word_IE(14 downto 12),
    round_mode    => "00000001",
    data_a        => RS1_DATA_FLOAT,
    data_b        => RS2_DATA_FLOAT,
    data_c        => RD_DATA_FLOAT,
    valid_i       => float_instr_req,
    result        => FP_RES_WB_wire,
    div_ready     => div_ready,
    fp_div_ready  => fp_div_ready,
    fp_sqrt_ready => fp_sqrt_ready,
    flag_in       => float_flag(4),
    flag_dz       => float_flag(3),
    flag_uf       => float_flag(2),
    flag_of       => float_flag(1),
    flag_nx       => float_flag(0)
  );

  -- AAA temporary process until the FPU is pipelined
  process(clk_i)
  begin
    if rising_edge(clk_i) then
      if float_instr_req = '1' or div_done = '1' then
        harc_FP_RES_WB       <= harc_EXEC;
        instr_word_FP_RES_WB <= instr_word_IE;
        FP_RES_WB            <= FP_RES_WB_wire;
        FP_RES_WB_EN         <= '1';
      end if;
    end if;
  end process;

  busy_FPU <= not(div_ready);

end architecture behavioral;