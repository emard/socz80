--
--	A VGA video engine for the SocZ80 system
--
--	Alan Cox, 2014
--
--	DummyEngine is a simple RAMless example that generates a pretty
--	pattern on the screen for testing purposes.
--
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity DummyEngine is
    port ( clk              : in  std_logic;
           reset            : in  std_logic;
           cpu_address      : in  std_logic_vector(2 downto 0);
           data_in          : in  std_logic_vector(7 downto 0);
           data_out         : out std_logic_vector(7 downto 0);
           enable           : in  std_logic;
           read_notwrite    : in  std_logic;
           
           -- interfaces from the VGA port
           vga_reset        : in std_logic;
           vsync            : in std_logic;
           hsync            : in std_logic;
           pixelclk         : in std_logic;
           pixelfetch       : in std_logic;
           pixelcol         : in unsigned(9 downto 0);
           pixelrow         : in unsigned(8 downto 0);
           pixel            : out std_logic_vector(7 downto 0);

           -- interfaces to the video RAM
           video_clock      : out std_logic;
           video_address    : out std_logic_vector(15 downto 0);
           video_data       : in std_logic_vector(8 downto 0)
        
    );
end DummyEngine;

architecture Behavioral of DummyEngine is

    signal pixelx: std_logic_vector(9 downto 0);
    signal pixely: std_logic_vector(8 downto 0);
    
begin
    video_clock <= pixelclk;
    pixelx <= std_logic_vector(pixelcol);
    pixely <= std_logic_vector(pixelrow);
    
    video_address <= "0000000000000000";
    pixel <= pixelx(7 downto 0) xor pixely(7 downto 0);

    data_out <= "11111111";
    
end;

