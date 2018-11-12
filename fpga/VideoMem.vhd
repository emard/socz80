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
--
--	This module multiplexes up to four VideoRAM instances into the video
--	memory for the system. We pass in 64K of addressing and decode
--	from the top level but only actually use the low 32K at this point
--

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
library UNISIM;
use UNISIM.VComponents.all;

entity VideoMem is
	port(
                cpu_clk             : in  std_logic;
                cpu_reset           : in  std_logic;
                cpu_write           : in  std_logic;
                cpu_cs              : in  std_logic;
                cpu_address         : in  std_logic_vector(15 downto 0);
                cpu_data_in         : in  std_logic_vector(7 downto 0);
                cpu_data_out        : out std_logic_vector(7 downto 0);
		
                video_clk           : in  std_logic;
                video_address       : in  std_logic_vector(15 downto 0);
                video_data_out      : out std_logic_vector(8 downto 0)
	);
end VideoMem;


architecture behaviour of VideoMem is

    signal cpu_ram0_data_out : std_logic_vector(7 downto 0);
    signal cpu_ram1_data_out : std_logic_vector(7 downto 0);
    signal cpu_ram0_select : std_logic;
    signal cpu_ram1_select : std_logic;

    signal video_ram0_data_out : std_logic_vector(8 downto 0);
    signal video_ram1_data_out : std_logic_vector(8 downto 0);
    signal video_ram0_select : std_logic;
    signal video_ram1_select : std_logic;

    signal cpu_ram2_data_out : std_logic_vector(7 downto 0);
    signal cpu_ram3_data_out : std_logic_vector(7 downto 0);
    signal cpu_ram2_select : std_logic;
    signal cpu_ram3_select : std_logic;

    signal video_ram2_data_out : std_logic_vector(8 downto 0);
    signal video_ram3_data_out : std_logic_vector(8 downto 0);
    signal video_ram2_select : std_logic;
    signal video_ram3_select : std_logic;

  begin 
    cpu_ram0_select <= cpu_cs and not cpu_address(0) and not cpu_address(1);
    cpu_ram1_select <= cpu_cs and cpu_address(0) and not cpu_address(1);
    cpu_ram2_select <= cpu_cs and not cpu_address(0) and cpu_address(1);
    cpu_ram3_select <= cpu_cs and cpu_address(0) and cpu_address(1);
    cpu_ram3_select <= cpu_cs and cpu_address(0) and cpu_address(1);

    cpu_data_out <= cpu_ram0_data_out when cpu_ram0_select='1' else
                      cpu_ram1_data_out when cpu_ram1_select='1' else
                      cpu_ram2_data_out when cpu_ram2_select='1' else
                      cpu_ram3_data_out;

    video_ram0_select <= not video_address(0) and not video_address(1);
    video_ram1_select <= video_address(0) and not video_address(1);
    video_ram2_select <= not video_address(0) and video_address(1);
    video_ram3_select <= video_address(0) and video_address(1);

    video_data_out <= video_ram0_data_out when video_ram0_select='1' else
                      video_ram1_data_out when video_ram1_select='1' else
                      video_ram2_data_out when video_ram2_select='1' else
                      video_ram3_data_out;


    vram0: entity work.VideoRAM
    port map(
                cpu_clk => cpu_clk,
                cpu_reset => cpu_reset,
                cpu_write => cpu_write,
                cpu_cs => cpu_ram0_select,
                cpu_address => cpu_address(14 downto 2),
                cpu_data_in => cpu_data_in,
                cpu_data_out => cpu_ram0_data_out,
                video_clk => video_clk,
                video_address => video_address(14 downto 2),
                video_data_out => video_ram0_data_out
    );

    vram1: entity work.VideoRAM
    port map(
                cpu_clk => cpu_clk,
                cpu_reset => cpu_reset,
                cpu_write => cpu_write,
                cpu_cs => cpu_ram1_select,
                cpu_address => cpu_address(14 downto 2),
                cpu_data_in => cpu_data_in,
                cpu_data_out => cpu_ram1_data_out,
                video_clk => video_clk,
                video_address => video_address(14 downto 2),
                video_data_out => video_ram1_data_out
    );

    vram2: entity work.VideoRAM
    port map(
                cpu_clk => cpu_clk,
                cpu_reset => cpu_reset,
                cpu_write => cpu_write,
                cpu_cs => cpu_ram2_select,
                cpu_address => cpu_address(14 downto 2),
                cpu_data_in => cpu_data_in,
                cpu_data_out => cpu_ram2_data_out,
                video_clk => video_clk,
                video_address => video_address(14 downto 2),
                video_data_out => video_ram2_data_out
    );

    vram3: entity work.VideoRAM
    port map(
                cpu_clk => cpu_clk,
                cpu_reset => cpu_reset,
                cpu_write => cpu_write,
                cpu_cs => cpu_ram3_select,
                cpu_address => cpu_address(14 downto 2),
                cpu_data_in => cpu_data_in,
                cpu_data_out => cpu_ram3_data_out,
                video_clk => video_clk,
                video_address => video_address(14 downto 2),
                video_data_out => video_ram3_data_out
    );

end;
