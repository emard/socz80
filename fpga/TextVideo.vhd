--
--      A VGA video engine for the SocZ80 system
--
--      Alan Cox, 2014
--
--      State machine driven text mode video engine
--
--      Ports are usually mapped at 0xD8
--
--      0xD8: ink colour (R/W)
--      0xD9: paper colour (R/W)
--      0xDA: bright colour (R/W)
--      0xDE: control(R/W)
--              bit 7: debug(turns on various on display info)
--              bit 1: bright/inverse
--                      0 - bit 7 = bright
--                      1 - bit 7 = inverse
--		bit 0: mode
--                      0 - 40 characters (requires 2K RAM)
--                      1 - 25 characters (requires 4K RAM)
--		
--       0xDF: identify (RO) reports 0
--

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity TextEngine is
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
end TextEngine;

architecture Behavioral of TextEngine is

    signal pixel_x: std_logic_vector(9 downto 0);
    signal pixel_y: std_logic_vector(8 downto 0);
    signal video: unsigned(15 downto 0);
    signal videonext: unsigned(15 downto 0);
    signal videosave: unsigned(15 downto 0);
    signal character: std_logic_vector(8 downto 0);
    signal pixelbits: std_logic_vector(7 downto 0);
    signal nextbits: std_logic_vector(7 downto 0);
    signal state: std_logic_vector(3 downto 0);
    signal rom_address: std_logic_vector(10 downto 0);
    signal rom_data: std_logic_vector(7 downto 0);
    
    signal doubler: std_logic;
    
    signal stripe: std_logic_vector(7 downto 0);    
    signal rstripe: std_logic_vector(2 downto 0);    
    signal gstripe: std_logic_vector(2 downto 0);

    signal ink: std_logic_vector(7 downto 0);
    signal paper: std_logic_vector(7 downto 0);
    signal brightink: std_logic_vector(7 downto 0);
    signal control: std_logic_vector(7 downto 0);
    
    signal inverse: std_logic;
    signal bright: std_logic;
    signal charbit8: std_logic;
    signal charbit7: std_logic;
    signal thisink: std_logic_vector(7 downto 0);

begin
    -- CPU side
    with cpu_address select
      data_out <= "00000000" when "111",     -- Video type is 0 rev 0 (text)
                  ink when "000",            -- Ink colour
                  paper when "001",          -- Background
                  brightink when "010",      -- Bright ink
                  control when "110",        -- Control register
                  "11111111" when others;

    --
    -- Simple register programming interface
    --
    CPULogic: process(clk)
    begin
      if rising_edge(clk) then
        if enable = '1' and read_notwrite = '0' then
          case cpu_address is
            when "000" => ink <= data_in;
            when "001" => paper <= data_in;
            when "010" => brightink <= data_in;
            when "110" => control <= data_in;
            when others =>
          end case;
        end if;
      end if;
    end process;

    -- Propogate the clock
    video_clock <= pixelclk;

    -- Type conversions for ease of handling
    pixel_x <= std_logic_vector(pixelcol);
    pixel_y <= std_logic_vector(pixelrow);
    video_address <= std_logic_vector(video);

    -- Index into the ROM table. The scan line number is the lower bits
    -- which makes it all nice and easy for an 8x16 font
    --
    rom_address <= character(6 downto 0)&pixel_y(3 downto 0);

    -- Pixel colouring
    
    rstripe <= "111" when pixel_x = "0000000000" else "000";
    gstripe <= "111" when video = to_unsigned(0,16) else "000";
    stripe <= pixel_y(3) & not pixel_y(3) & gstripe & rstripe when control(7) = '1'
      else paper;
    
    thisink <= ink when bright = '0' else brightink;

    pixel <= thisink when (pixelbits(7) xor inverse) = '1' else stripe;

    Engine: process(pixelclk)
    begin
      if rising_edge(pixelclk) then
        if doubler = '0' or control(0) = '1' then
          -- For the top 8 states we clock the pixel out
          -- we don't need to for 1111 but it is harmless there
          if state(3) = '1' then
            pixelbits <= pixelbits(6 downto 0) & '0';
          end if;
          case state is
            -- Wait while vsync is occuring
            when "0000" => state(0) <= vsync;
            -- Reset to start of frame buffer
            when "0001" => video <= to_unsigned(0,16);
                           videosave <= to_unsigned(0,16);
                           state <= "0010";
            when "0010" => state <= "0011";
            -- Fetch first byte
            when "0011" => character <= video_data;
                           -- This implicitly sets up the ROM address but we can't
                           -- read the ROM until next clock
                           state <= "0100";
            when "0100" => charbit8 <= character(8);
                           charbit7 <= character(7);
                           state <= "0101";
            -- Fetch ROM bits
            when "0101" => nextbits <= rom_data;
                           state <= "0110";
            -- Main wait point. Branch according to pixel and vsync state
            -- vsync -> state 0, pixelfetch -> state 0111, both can't occur
            when "0110" => state(0) <= pixelfetch;
                           state(1) <= vsync;
                           state(2) <= vsync;
            -- First pixel onto the screen
            when "0111" => pixelbits <= nextbits;
                           if control(1) = '0' then
                             inverse <= charbit8; 
                             bright <= charbit7;
                           else
                             inverse <= charbit7;
                             bright <= charbit8;
                           end if;
                           state <= "1000";
            -- 1xxx rotate the pixelbits and display
            when "1000" => video <= video + 1;
                           state <= "1001";
            when "1001" => state <= "1010";
            when "1010" => character <= video_data;
                           state <= "1011";
            when "1011" => charbit8 <= character(8);
                           charbit7 <= character(7);
                           state <= "1100";
            when "1100" => nextbits <= rom_data;
                           state <= "1101";
            when "1101" => state <= "1110";
            when "1110" => if pixelfetch = '1' then
                             state <= "0111";
                           else
                             if pixel_y(3 downto 0) = "1111" then
                               videosave <= video;
                             end if;
                             state <= "1111";
                           end if;
            when "1111" => video <= videosave;
                           state <= "0010";
            -- Spare states
            when others => state <= "0000";
          end case;
        end if;
        doubler <= not doubler;
      end if;
    end process;

    FontRom: entity work.FontRom
    port map(
      clk => pixelclk,
      addr => rom_address,
      data => rom_data
    );
    
end;
