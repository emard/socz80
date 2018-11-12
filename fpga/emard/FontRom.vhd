library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity FontRom is
    port
    (
      clk  : in std_logic;
      addr : in std_logic_vector(10 downto 0);
      data : out std_logic_vector(7 downto 0)
    );
end;

architecture Behavioral of FontRom is
begin
    data <= addr(7 downto 0);
end;
