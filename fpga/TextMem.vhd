--
--	Video RAM for the SocZ80 VGA
--
--	This option provides a 4K dual ported RAM for the text modes
--	(well you can use it with the PixelEngine but it's not much fun)
--
--	All the memories should have the same interface so you can plug
--	then together as you want.
--
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
library UNISIM;
use UNISIM.VComponents.all;

entity TextMem is
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
end TextMem;


architecture behaviour of TextMem is

    signal cpu_ram0_data_out : std_logic_vector(7 downto 0);
    signal cpu_ram1_data_out : std_logic_vector(7 downto 0);
    signal cpu_ram0_select : std_logic;
    signal cpu_ram1_select : std_logic;

    signal video_ram0_data_out : std_logic_vector(8 downto 0);
    signal video_ram1_data_out : std_logic_vector(8 downto 0);
    signal ram0_wait: std_logic;
    signal ram1_wait: std_logic;
    signal video_ram0_select : std_logic;
    signal video_ram1_select : std_logic;

  begin 
    cpu_ram0_select <= cpu_cs and not cpu_address(0);
    cpu_ram1_select <= cpu_cs and cpu_address(0);

    cpu_data_out <= cpu_ram0_data_out when cpu_ram0_select='1' else
                      cpu_ram1_data_out;
    
    cpu_wait <= ram0_wait or ram1_wait;

    video_ram0_select <= not video_address(0);
    video_ram1_select <= video_address(0);

    video_data_out <= video_ram0_data_out when video_ram0_select='1' else
                      video_ram1_data_out;


    tram0: entity work.TextRAM
    port map(
                cpu_clk => cpu_clk,
                cpu_reset => cpu_reset,
                cpu_write => cpu_write,
                cpu_cs => cpu_ram0_select,
                cpu_address => '0'&cpu_address(15 downto 1),
                cpu_data_in => cpu_data_in,
                cpu_data_out => cpu_ram0_data_out,
                cpu_wait => ram0_wait,
                video_clk => video_clk,
                video_address => '0'&video_address(15 downto 1),
                video_data_out => video_ram0_data_out
    );

    tram1: entity work.TextRAM
    port map(
                cpu_clk => cpu_clk,
                cpu_reset => cpu_reset,
                cpu_write => cpu_write,
                cpu_cs => cpu_ram1_select,
                cpu_address => '0'&cpu_address(15 downto 1),
                cpu_data_in => cpu_data_in,
                cpu_data_out => cpu_ram1_data_out,
                cpu_wait => ram1_wait,
                video_clk => video_clk,
                video_address => '0'&video_address(15 downto 1),
                video_data_out => video_ram1_data_out
    );

end;
