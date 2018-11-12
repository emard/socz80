--
--	Glue for the joystick, keyboard and similar I/O ports
--
--	Alan Cox, (c) 2014
--

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity IODevice is
    port ( clk              : in  std_logic;
           reset            : in  std_logic;
           cpu_address      : in  std_logic_vector(2 downto 0);
           data_in          : in  std_logic_vector(7 downto 0);
           data_out         : out std_logic_vector(7 downto 0);
           enable           : in  std_logic;
           read_notwrite    : in  std_logic;
           joystick0        : in  std_logic_vector(7 downto 0);
           joystick1        : in  std_logic_vector(7 downto 0);
           joystick2        : in  std_logic_vector(7 downto 0);
           sound            : out std_logic
    );
end IODevice;

--
-- Connect unsupported joysticks to all 1 at the top level
-- Connect unsupported sound to nowhere
--
-- TODO:
-- Add keyboard and mouse ports
--

architecture Behavioral of IODevice is
begin

-- Partially implemented only at this point
    with cpu_address select
        data_out <=      
            -- Joystick 0
            joystick0 when "100",
            -- Joystick 1
            joystick1 when "101",
            -- Joystick 2
            joystick2 when "110",
            "11111111" when others;

     IODevice_proc: process(clk)
     begin
       if rising_edge(clk) then
         if enable = '1' and read_notwrite = '0' then
           case cpu_address is
             when "000" => sound <= data_in(0);
             when others =>
           end case;
         end if;
       end if;
     end process;
end Behavioral;

