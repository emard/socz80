--+-----------------------------------+-------------------------------------+--
--|                      ___   ___    | (c) 2014 Alan Cox                   |--
--|   ___  ___   ___ ___( _ ) / _ \   |                                     |--
--|  / __|/ _ \ / __|_  / _ \| | | |  |                                     |--
--|  \__ \ (_) | (__ / / (_) | |_| |  | A Z80 FPGA computer, just for fun   |--
--|  |___/\___/ \___/___\___/ \___/   |                                     |--
--|                                   |              http://sowerbutts.com/ |--
--+-----------------------------------+-------------------------------------+--
--| Logic MegaWing Seven Segment Display Interface                          |--
--+-------------------------------------------------------------------------+--

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity sevenseg is
    port ( clk              : in  std_logic;
           reset            : in  std_logic;
           cpu_address      : in  std_logic_vector(2 downto 0);
           data_in          : in  std_logic_vector(7 downto 0);
           data_out         : out std_logic_vector(7 downto 0);
           enable           : in  std_logic;
           read_notwrite    : in  std_logic;
           segment_pins     : out std_logic_vector(7 downto 0);
           mux_pins         : out std_logic_vector(3 downto 0)
    );
end sevenseg;

architecture Behavioral of sevenseg is

    signal digit0 : std_logic_vector(7 downto 0);
    signal digit1 : std_logic_vector(7 downto 0);
    signal digit2 : std_logic_vector(7 downto 0);
    signal digit3 : std_logic_vector(7 downto 0);
    signal strobe : std_logic_vector(9 downto 0);
    signal next_strobe : std_logic_vector(9 downto 0);
    -- 128MHz input clock is too high for the display strobe so
    -- downclock it by 12bits to get us a nice 31.25Khz
    signal downclk: unsigned(11 downto 0);

begin

    with cpu_address select
        data_out <=
            -- First byte identifies the wing
            "00000001"   when "000",        
            digit0  when "001",
            digit1  when "010",
            digit2  when "011",
            digit3  when "100",
            "11111111"  when others;

    sevenseg_proc: process(clk)
    begin
      if rising_edge(clk) then
        if reset = '1' then
          digit0 <= "00000000";
          digit1 <= "00000000";
          digit2 <= "00000000";
          digit3 <= "00000000";
          strobe <= "1111111110";
          next_strobe <= "1111111101";
          downclk <= to_unsigned(0,12);
        else
          if enable = '1' and read_notwrite = '0' then 
            case cpu_address is
              when "000" => -- no change
              when "001" => digit0 <= data_in;
              when "010" => digit1 <= data_in;
              when "011" => digit2 <= data_in;
              when "100" => digit3 <= data_in;
              when others => -- no change
            end case;
          end if;

          -- We do a power of 2 clock (cheaper), and we keep
          -- a 10 bit strobe with 4 external bits as its a
          -- trivial way to get the 10% duty cycle
          -- We wire the mux to bits within the shift to keep
          -- a fairly even pulse spacing and avoid flicker
          downclk <= downclk + 1;
          if downclk = 0 then
            strobe <= next_strobe;
	    mux_pins <= strobe(7)&strobe(5)&strobe(2)&strobe(0);
            case strobe is
              when "1111111110" => segment_pins <= not digit0;
              when "1111111011" => segment_pins <= not digit1;
              when "1111011111" => segment_pins <= not digit2;
              when "1101111111" => segment_pins <= not digit3;
              -- blanking time
              when others => segment_pins <= "11111111";
            end case;
          end if;
          next_strobe <= strobe(0)&strobe(9 downto 1);
        end if;
      end if;
    end process;
end Behavioral;

