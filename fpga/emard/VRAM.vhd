--
--	Video RAM for the SocZ80 VGA
--
--	We use as much memory as we can get and we dual port it between
--	the host (R/W) and the pixel engine (R/O).
--
--	Although it's not really important in this case we multiplex the
--	devices by the low address bits so the video scan bandwidth scales
--	better for any future case.
--
--      Possibly we should do something clever to not waste the parity bits?
--
--	Question: Could we use the mostly unused RAMB8WER devices instead
--	and build a bigger VideoRAM ??
--
--	This module instantiates an 8K block of memory using four RAMB16WER
--	devices. You can then plug up to four of these into the VideoMemory
--	interface according to your taste.

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity VideoRAM is
	port(
                cpu_clk             : in  std_logic;
                cpu_reset           : in  std_logic;
                cpu_write           : in  std_logic;
                cpu_cs              : in  std_logic;
                cpu_address         : in  std_logic_vector(12 downto 0);
                cpu_data_in         : in  std_logic_vector(7 downto 0);
                cpu_data_out        : out std_logic_vector(7 downto 0);
		
                video_clk           : in  std_logic;
                video_address       : in  std_logic_vector(12 downto 0);
                video_data_out      : out std_logic_vector(8 downto 0)
	);
end VideoRAM;

architecture behaviour of VideoRAM is
    signal S_ram_we : std_logic;
begin
    S_ram_we <= cpu_write and cpu_cs;
    video_dpram: entity work.bram_true2p_2clk
    generic map (
        dual_port => true,
        pass_thru_a => false,
        data_width => 8,
        addr_width => 11
    )
    port map (
        clk_a      => cpu_clk,
        we_a       => S_ram_we,
        addr_a     => cpu_address(12 downto 2),
        data_in_a  => cpu_data_in,
        data_out_a => cpu_data_out,

        clk_b      => video_clk,
        we_b       => '0',
        addr_b     => video_address(12 downto 2),
        data_out_b => video_data_out
    );
end;
