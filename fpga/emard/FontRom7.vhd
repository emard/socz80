library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity FontRom7 is
    port
    (
      clk  : in std_logic;
      addr : in std_logic_vector(11 downto 0);
      data : out std_logic_vector(7 downto 0)
    );
end;

architecture Behavioral of FontRom7 is
begin
    video_font_rom: entity work.FontRom
    port map (
        clk        => clk,
        addr       => addr(10 downto 0),
        data       => data
    );
end;
