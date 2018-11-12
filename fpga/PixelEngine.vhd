--
--	A VGA video engine for the SocZ80 system
--
--	Alan Cox, 2014
--
--	The PixelEngine hooks up to the video RAM and to VideoVGA
--	to provide 640/320/160 wide modes at various bpps and with some
--	ability to do palettes per group of pixels.
--
--	WIP
--
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity PixelEngine is
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
end PixelEngine;

architecture Behavioral of PixelEngine is
    -- Next video fetch address
    signal pixeladdr: unsigned(15 downto 0);
    -- Next control fetch address
    signal controladdr: unsigned(15 downto 0);
    -- Type conversion of the controladdr
    signal control_tmp: std_logic_vector(15 downto 0);
    -- Are we doing a control or a data fetch ?
    signal controlmode: std_logic;
    -- Control flags for this line
    signal controlflag: std_logic_vector(4 downto 0);
    -- Counts the number of pixels to do before we do another fetch
    signal pixelbit: unsigned(2 downto 0);
    -- The last fetched colour pixel
    signal pixeldata: std_logic_vector(8 downto 0);
    -- Type conversion of the pixeladdr
    signal video_tmp: std_logic_vector(15 downto 0);
    -- 9bit data stream from video RAM
    signal video_bits: std_logic_vector(8 downto 0);
    -- True if we are in a horizontal area with pixels
    signal data_fetch_h: std_logic;
    -- True if we are in a vertical area with pixels
    signal data_fetch_v: std_logic;
    -- True if we are pixel fetching
    signal data_fetch: std_logic;
    -- Operation chain for the pixel engine
    signal opchain: std_logic_vector(31 downto 0);
    -- Lux xor for colour control
    signal luxor: std_logic_vector(7 downto 0);
    -- Lux xor for colour control (buffered)
    signal luxor_next: std_logic_vector(7 downto 0);
    -- Pixel code
    signal rgblookup: std_logic_vector(7 downto 0);
    
    -- Registers for programming
    signal opchain_reg: std_logic_vector(31 downto 0);
    signal modebits: std_logic_vector(7 downto 0);
    signal index_reg: std_logic_vector(7 downto 0);
    
    -- Base colours
    type cluts is array (255 downto 0) of std_logic_vector(7 downto 0);
    signal clut: cluts;

begin

    -- FIXME, dummy stuff for now
    with cpu_address select
        data_out <=
            index_reg when "000",
            "11111111"  when others;

    -- Fetch only when in display rectangle
    -- CHECK: Our sync outputs lag by a clock so the data needs to do so too
    data_fetch_h <= '1' when pixelcol < 640 else '0';
    data_fetch_v <= '1' when pixelrow < 480 else '0';
    data_fetch <= data_fetch_h and data_fetch_v;

    -- Export the pixel clock to the VRAMs
    video_clock <= pixelclk;
    video_tmp <= std_logic_vector(pixeladdr);
    control_tmp <= std_logic_vector(controladdr);

    video_address <= video_tmp(15 downto 0) when controlmode = '0' else
                     control_tmp(15 downto 0);

    	  
    -- The luxor allows you to change the section of the colour map in use
    -- per scan line. The top of modebits is the mask to use for the mode.
    -- set modebits 7:4 to 0 and you can blank the line handily 
    rgblookup <= (modebits(7 downto 4) and pixeldata(7 downto 4)) &"0000" xor luxor;
    pixel <= clut(to_integer(unsigned(rgblookup)));

    --
    -- CPU clocked side of things
    --
    -- Register writes, configuration bits
    --
    vgacontroller: process(clk)
    begin
      if rising_edge(clk) then
        if enable = '1' and read_notwrite = '0' then 
          --
          -- We scale the CLUT entries so that when we support the boards
          -- with 4096 colour mode the software interface is constant
          --
          case cpu_address is
            when "000" => index_reg <= data_in;
            when "001" => clut(to_integer(unsigned(index_reg)))(7 downto 5) <= data_in(7 downto 5);
            when "010" => clut(to_integer(unsigned(index_reg)))(4 downto 2) <= data_in(7 downto 5);
            when "011" => clut(to_integer(unsigned(index_reg)))(1 downto 0) <= data_in(7 downto 6);
            when others => -- no change
          end case;
        end if;
      end if;
    end process;

    -- Handle the business of putting bits on the display
    -- This is basically a tiny engine which repetetively executes
    -- 16 2 bit op codes along each line
    pixelengine: process(pixelclk)
    begin
      if rising_edge(pixelclk) then
        -- Set the lux xor byte if it changed last cycle
        luxor <= luxor_next;
        -- We only run the bitstream loader during displayed video
        -- Fetch a line every 8 pixels
        -- Roll the register for the others
        if data_fetch = '1' then
          case opchain(31 downto 30) is
            -- Operation 0, load for next scan byte
            when "00" =>
              pixeldata <= video_data;
              pixeladdr <= pixeladdr + 1;
            -- Rotate pixels
            when "01" =>
              case modebits(1 downto 0) is
                 -- 1 bit per pixel
                 when "00" => pixeldata <= '0' & pixeldata(6 downto 0) & '0';
                 -- 2 bits per pixel
                 when "01" => pixeldata <= '0' & pixeldata(5 downto 0) & "00";
                 -- Four bits per pixel
                 when others => pixeldata <= '0' & pixeldata(3 downto 0) & "0000";
              end case;
            -- Display the same pixel
            when "10" =>
            -- Magic mode, swap the luxor based on the low bits of the data
            when others =>
              -- FIXME , once palette logic is done this will go into the
              -- high 4 bits of the luxor on the next pixel
              luxor_next <= "0000" & pixeldata(3 downto 0);
          end case;
          -- Rotate the opchain
          opchain <= opchain(29 downto 0) & opchain(31 downto 30);
        else
          -- put the control signal on the bus from pixel 640-840
          controlmode <= '1';
          -- The control machine. Load bytes from the control
          -- table to set up the video stream for this line
          -- During the video load the entries but also on the last
          -- blanking line read to start again
          if pixelrow < 480 or pixelrow = 495 then
             case std_logic_vector(pixelcol) is
               when "1010001010" => pixeladdr(15 downto 8) <= unsigned(video_data(7 downto 0));
                           controladdr <= controladdr + 1;
               when "1010001011" => pixeladdr(7 downto 5) <= unsigned(video_data(7 downto 5));
                           pixeladdr(4 downto 0) <= "00000";
                           controlflag <= video_data(4 downto 0);
                           controladdr <= controladdr + 1;
               when "1010001100" => 
                           if controlflag(3) = '1' then
                             luxor <= video_data(7 downto 0);
                             controladdr <= controladdr + 1;
                            end if;
               when "1010001101" => 
                           if controlflag(2) = '1' then
                             modebits <= video_data(7 downto 0);
                             controladdr <= controladdr + 1;
                           end if;
               when "1010001110" =>
                           if controlflag(2) = '1' then
                             opchain(31 downto 24) <= video_data(7 downto 0);
                             controladdr <= controladdr + 1;
                           end if;
               when "1010001111" =>
                           if controlflag(2) = '1' then
                             opchain(23 downto 16) <= video_data(7 downto 0);
                             controladdr <= controladdr + 1;
                           end if;
               when "1010010000" =>
                           if controlflag(2) = '1' then
                             opchain(15 downto 8) <= video_data(7 downto 0);
                             controladdr <= controladdr + 1;
                           end if;
               when "1010010001" =>
                           if controlflag(2) = '1' then
                             opchain(7 downto 0) <= video_data(7 downto 0);
                             controladdr <= controladdr + 1;
                           end if;
               when "1010010010" => controlmode <= '0';
               -- Preload pixel 0 for the scan out
               when "1010010011" => pixeldata <= video_data;
                           pixeladdr <= pixeladdr + 1;
               when others =>
             end case;
          end if;
          -- vsync is negative polarity
          if vsync = '0' then
            controladdr <= to_unsigned(0,16);
            opchain <= opchain_reg;
          end if;
        end if;
      end if;
    end process;
          
end Behavioral;

