--
--	A VGA video engine for the SocZ80 system
--
--	Alan Cox, 2014
--
--	VideoVGA generates the clocking signals and video output stream. It's
--	mostly oblivious to the bitstream provider.
--
--	It provides the pixel engines with the row/column number and clock.
--	It provides the 8bit video output, hsync and vsync for the display.
--
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity VideoVGA is
    port ( clk              : in  std_logic;
           reset            : in  std_logic;
           
           -- interfaces to the VGA port
           vsync            : out std_logic;
           hsync            : out std_logic;
           videoblank       : out std_logic;
           rgb8             : out std_logic_vector(7 downto 0);
           
           -- interfaces to the pixel engines
           scan_reset       : out std_logic;
           scan_clk         : out std_logic;
           scan_fetch       : out std_logic;
           scan_col         : out unsigned(9 downto 0);
           scan_row         : out unsigned(8 downto 0);
           scan_data        : in std_logic_vector(7 downto 0)
    );
end VideoVGA;

architecture Behavioral of VideoVGA is

    -- 128MHz input clock is too high for the display so
    -- downclock it to get us 64MHz (We want 63 really FIXME)
    signal pixelclk: std_logic;

    -- Scanning position in pixelclk space including blanks
    signal pixelcol: unsigned(9 downto 0);
    -- Scanning position in lines including blanks
    signal pixelrow: unsigned(8 downto 0);
    -- Scanning position in lines including blanks but with
    -- a lead in for the video engine (and exported to it)
    -- this one increments ahead of the data fetch beginning
    signal pixelrow_li: unsigned(8 downto 0);
    -- True when vsync is on
    signal vsync_flag: std_logic;
    -- True when hsync is on
    signal hsync_flag: std_logic;
    -- Buffer for vsync for signal quality
    signal vsync_next: std_logic;
    -- Buffer for hsync delay/signal quality
    signal hsync_next: std_logic;
    -- Delay
    signal hsync_next2: std_logic;
    -- Delay
    signal hsync_next3: std_logic;
    -- Used to pass reset across the clock domains
    signal vga_reset: std_logic;
    -- Blanking logic
    signal blank_v: std_logic;
    signal blank_h: std_logic;
    signal blank: std_logic;
    -- Blank trails the real signal because everything is offset a pixel clock
    signal blanktail: std_logic;
    -- Data fetch helping clock for pixel engines
    -- runs at twice the pixel clock
    signal fetch_h: std_logic;
    signal fetch_v: std_logic;
    signal fetch_l: std_logic;
    signal fetch: std_logic;
    signal worksleep: std_logic;

begin
    -- VGA connector wiring
    
    -- Vertical is active low
    vsync <= not vsync_flag;
    -- Horizontal is active low
    hsync <= not hsync_flag;
    -- Video blank
    videoblank <= blank;

    -- The various pulse states themselves don't need
    -- to be clocked but do need buffering
    hsync_next3 <=
      '1' when pixelcol >= 680 and pixelcol < 744
      else '0';

    vsync_next <=
      '1' when pixelrow >= 490 and pixelrow < 492
      else '0';          

    blank_h <= '0' when pixelcol < 640 else '1';
    blank_v <= '0' when pixelrow < 480 else '1';
    blank <= blank_h or blank_v;

    -- fetch <= (fetch_h and fetch_v); -- or fetch_l;
    
    fetch_h <= '1' when pixelcol < 638 or pixelcol = 847 else '0';
    fetch_v <= '1' when pixelrow < 480 else '1';
    fetch_l <= '1' when pixelcol >= 847 and pixelrow = 499 else '0';
    --fetch <= fetch_l or (fetch_h and fetch_v);
    fetch <= not blank;

    -- Pixel engine wiring
    
    scan_fetch <= fetch;
    scan_clk <= pixelclk;
    scan_col <= pixelcol;
    scan_row <= pixelrow_li;

    -- Linkage between the video and the data stream

    -- No video in the blanking area
    with blanktail select
             rgb8 <= scan_data  when '0',
            "00000000" when '1';

    --
    -- CPU clocked side of things
    --
   pixelclock: process(clk)
   begin
     if rising_edge(clk) then
        if reset = '1' then
          -- Propogate reset to the VGA pixel clock domain
          pixelclk <= '0';
          vga_reset <= '1'; 
        end if;
       
        -- Generate the video clock here for now
        -- Invert every other time so pixelclk is 64MHz
        pixelclk <= not pixelclk;
        if pixelclk = '0' then
          vga_reset <= '0';
        end if;
      end if;
    end process;

    -- Scan line counters. We count along the line and down the lines
    -- We propogate sync pulses one clock delayed for cleannes and also
    -- for data fetch time
    -- The operating clock is effectively double the pixel rate, so we can
    -- clock the RAMs etc fast enough for our needs
    scancounter: process(pixelclk)
    begin
      if rising_edge(pixelclk) then
       blanktail <= blank;
       if worksleep = '0' then
        hsync_flag <= hsync_next;
        hsync_next <= hsync_next2;
        hsync_next2 <= hsync_next3;
        vsync_flag <= vsync_next;
        if vga_reset = '1' then
          pixelcol <= to_unsigned(0,10);
          pixelrow <= to_unsigned(0,9);
          pixelrow_li <= to_unsigned(0,9);
        end if;
        -- Bump the row counter to the drawing engine at the end
        -- of the data fetch as that is rather more useful behaviour
        if pixelcol = 640 then
          if pixelrow = 524 then
            pixelrow_li <= to_unsigned(0,9);
          else
            pixelrow_li <= pixelrow + 1;
          end if;
        end if;
        if pixelcol = 847 then
          pixelcol <= to_unsigned(0,10);
            -- At the end of the scan line update the true pixelrow
          pixelrow <= pixelrow_li;
        else
          pixelcol <= pixelcol + 1;
        end if;
       end if;
       worksleep <= not worksleep;
      end if;
    end process;
             
end Behavioral;

