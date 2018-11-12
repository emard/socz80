--
--	Video RAM for the SocZ80 VGA - 2K text modes
--
--	Same interface as VideoMem but only has 2K which is mapped twice.
--	Accesses to the top 2K write to the low 2K but set parity, accesses
--	to the low 2K clear parity.
--

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
library UNISIM;
use UNISIM.VComponents.all;

entity TextRAM is
	port(
                cpu_clk             : in  std_logic;
                cpu_reset           : in  std_logic;
                cpu_write           : in  std_logic;
                cpu_cs              : in  std_logic;
                cpu_address         : in  std_logic_vector(15 downto 0);
                cpu_data_in         : in  std_logic_vector(7 downto 0);
                cpu_data_out        : out std_logic_vector(7 downto 0);
                cpu_wait            : out std_logic;

                video_clk           : in  std_logic;
                video_address       : in  std_logic_vector(15 downto 0);
                video_data_out      : out std_logic_vector(8 downto 0)
	);
end TextRAM;

architecture behaviour of TextRAM is
    -- Delay for read cycles
    signal is_read          : std_logic;
    signal was_read         : std_logic;

    signal S_ram_we : std_logic;
begin
    cpu_wait <= cpu_cs and (not cpu_write and not was_read);
    is_read <= cpu_cs and not cpu_write;

    ramwait: process(cpu_clk)
    begin
      if rising_edge(cpu_clk) then
        was_read <= is_read;
      end if;
    end process;
    
    S_ram_we <= cpu_write and cpu_cs;
    text_dpram: entity work.bram_true2p_2clk
    generic map (
        dual_port => true,
        pass_thru_a => false,
        data_width => 9,
        addr_width => 11
    )
    port map (
        clk_a      => cpu_clk,
        we_a       => S_ram_we,
        addr_a     => cpu_address(10 downto 0),
        data_in_a(8) => cpu_address(11),
        data_in_a(7 downto 0) => cpu_data_in,
        data_out_a(8) => open,
        data_out_a(7 downto 0) => cpu_data_out,

        clk_b      => video_clk,
        we_b       => '0',
        addr_b     => video_address(10 downto 0),
        data_out_b => video_data_out
    );

end;
