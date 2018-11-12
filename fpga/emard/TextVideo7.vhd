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
--              bit 4: default font
--                     0: teletext
--                     1: conventional
--                     bit 7 inverts the selection in teletext mode,
--                     but in character mode the default always applies.
--		bit 3: reveal
--              bit 2: teletext mode
--              bit 1: bright/inverse
--                      0 - bit 7 = bright
--                      1 - bit 7 = inverse
--		bit 0: mode
--                      0 - 40 characters (requires 2K RAM)
--                      1 - 80 characters (requires 4K RAM)
--		
--       0xDF: identify (RO) reports 0
--
--	TODO
--              Flashing
--		Double Height
--

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity TextEngine7 is
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
end TextEngine7;

architecture Behavioral of TextEngine7 is

    signal pixel_x: std_logic_vector(9 downto 0);
    signal pixel_y: std_logic_vector(8 downto 0);
    signal vclock: std_logic;
    signal video: unsigned(15 downto 0);
    signal videonext: unsigned(15 downto 0);
    signal videosave: unsigned(15 downto 0);
    signal pixelbits: std_logic_vector(7 downto 0);
    signal nextbits: std_logic_vector(7 downto 0);
    signal state: std_logic_vector(3 downto 0);
    signal rom_address: std_logic_vector(11 downto 0);
    signal rom_data: std_logic_vector(7 downto 0);
    
    signal doubler: std_logic;

    signal pixelreg: std_logic_vector(7 downto 0);

    signal character: std_logic_vector(8 downto 0);
    signal symbol: std_logic_vector(8 downto 0);

    -- Flash on/off
    signal flashon: std_logic;

    -- Signals that immediately affect the engine
    signal fg: std_logic_vector(2 downto 0);
    signal bg: std_logic_vector(2 downto 0);
    signal graphics: std_logic;
    signal separate: std_logic;

    -- Signals that affect the character engine
    -- Set when we clock in the first pixel
    signal conceal: std_logic;
    signal double: std_logic;
    signal hold: std_logic;
    signal flash: std_logic;
    signal box: std_logic;

    -- Buffered copies of changes that wil affect the engine as
    -- we start to display the symbol we have processed
    -- 'Set-At' copies in effect
    signal nextfg: std_logic_vector(2 downto 0);
    signal nextbg: std_logic_vector(2 downto 0);
    signal nextseparate: std_logic;
    signal nextgraphics: std_logic;
    signal nextdouble: std_logic;
    
    -- Buffered copies of signals that affect the character engine
    -- 'Set-After'
    signal nextconceal: std_logic;
    signal nexthold: std_logic;
    signal nextflash: std_logic;
    signal nextbox: std_logic;

    -- Double buffered copies of signals that affect the pixel
    -- being rendered 'Set-after' copies.
    signal nextnextfg: std_logic_vector(2 downto 0);
    signal nextnextbg: std_logic_vector(2 downto 0);
    signal nextnextdouble: std_logic;
    signal nextnextgraphics: std_logic;

    signal heldsym: std_logic_vector(8 downto 0);
    signal held: std_logic;

    signal next_dh: std_logic;
    
    signal isgraphics: std_logic;
    signal graphicsnow: std_logic;
    signal gfxcodebase: std_logic_vector(6 downto 0);
    signal fontcodetmp: std_logic_vector(6 downto 0);
    signal symcode: std_logic_vector(6 downto 0);
    
    signal ginverse: std_logic;
    signal inverse: std_logic;

    -- We need the invert to hit on the first pixel row not as we parse
    -- the symbol in the clocks leading up to it
    signal nextinverse: std_logic;
    signal tinverse: std_logic;
    signal pixcol: std_logic_vector(2 downto 0);
    signal bright: std_logic;
    signal nextbright: std_logic;

    -- Are we doing separate effect ?
    signal separate_mask: std_logic_vector(7 downto 0);
    signal separator: std_logic_vector(7 downto 0);
    signal nextseparator: std_logic_vector(7 downto 0);
    signal separating: std_logic;
    signal separatingv: std_logic;

    -- Text and control registers
    signal ink: std_logic_vector(7 downto 0) := x"ff";
    signal paper: std_logic_vector(7 downto 0);
    signal brightink: std_logic_vector(7 downto 0);
    signal control: std_logic_vector(7 downto 0);
    signal texcol: std_logic_vector(7 downto 0);
    

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
    
    -- Character parsing helpers
    isgraphics <= '1' when character(5) = '1' and nextgraphics = '1' else '0';

    -- Serves the same purpose but checks the symbol as it becomes live
    -- not as it is parsed
    graphicsnow <= '1' when symbol(5) = '1' and graphics = '1' else '0';

    -- Index into the ROM table. The scan line number is the lower bits
    -- which makes it all nice and easy for an 8x16 font
    -- This gets hairy because we have to fold up 96 symbols of text and
    -- 64 of graphics into 128

    -- Graphics code (0-31) or inverse thereof    
    gfxcodebase <= "00"&symbol(4 downto 0) when symbol(6) = '0' else
                   "00"&not symbol(4 downto 0);

    -- Text symbols 32-127, identity mapped
    fontcodetmp <= symbol(6 downto 0);

    -- The cases we have are
    -- Graphics code and graphics bit set: use 0-31, figure inverse out
    -- Graphics code and graphics bit clear: use 1:1 fonts
    -- Text mode: use 1:1 fonts    
    symcode <= gfxcodebase when graphicsnow = '1' else
               fontcodetmp;
    -- The top bit drives the font selector
    rom_address <= (control(4) xor (control(2) and symbol(7)))
                                      &symcode&pixel_y(3 downto 0);

    -- Pixel colouring
    ginverse <= graphics and symbol(5) and symbol(6);

    separating <= symbol(8) and control(2) and graphicsnow and separate;
    separate_mask <= "00000000" when separating = '1' and separatingv = '1'
                else "11101110" when separating = '1'
                else "11111111";
    
    -- Compute the inverse based upon the settings
    nextinverse <= ginverse or tinverse; --  xor (flash and flashclock)
    
    pixcol <= "000" when separator(7) = '0'
            else fg when (pixelbits(7) = '1' xor inverse = '1')
            else bg;

    texcol <= ink when pixelbits(7) = '1' and bright = '0' else
              brightink when pixelbits(7) = '1' else paper;
    -- Colour expansion BGR to BBGGGRRR    
    pixelreg <= pixcol(2)&pixcol(2)&pixcol(1)&pixcol(1)&pixcol(1)&pixcol(0)&pixcol(0)&pixcol(0)
                 when control(2) = '1' else texcol;
    
    Engine: process(pixelclk)
    begin
      if rising_edge(pixelclk) then
       if vclock = '0' then
        pixel <= pixelreg;
        if doubler = '0' or control(0) = '1' then
          -- For the top 8 states we clock the pixel out
          -- we don't need to for 1111 but it is harmless there
          if state(3) = '1' then
            pixelbits <= pixelbits(6 downto 0) & '0';
            separator <= separator(6 downto 0) & '0';
          end if;
          case state is
            -- Wait while vsync is occuring
            when "0000" => 
            -- Reset to start of frame buffer
                           video <= to_unsigned(0,16);
                           videosave <= to_unsigned(0,16);
                           -- Move to 0001 on vsync dropping
                           state(0) <= vsync;
            when "0001" => 
            -- Fetch first byte
            -- This implicitly sets up the ROM address but we can't
            -- read the ROM until next clock
                           character <= video_data;
                           state <= "0010";
            when "0010"|"1010" => 
            -- Parse the character code
                           if control(1) = '1' then
                             tinverse <= character(7);
                             nextbright <= character(8);
                           else
                             tinverse <= character(8);
                             nextbright <= character(7);
                           end if;
                           -- 00xxxxx is a control code
                           if control(2) = '1' and character(6 downto 5) = "00" then
                             -- 00G0CCC is a colour change (graphics/colour)
                             if character(3) = '0' then
                                 -- These should be set-after which means
                                 -- we need to buffer the colour setting
                                 nextnextfg <= character(2 downto 0);
                                 nextnextgraphics <= character(4);
                                 nextconceal <= '0';
                                 nexthold <= '0';
                             else if character(4) = '0' then
                               -- 0001FFV-- Fields, bit 0 is value
                               case character(2 downto 1) is
                                 -- Flash and double clear immediately
                                 when "00" => nextflash <= not character(0);
                                              flash <= flash and not character(0);
                                 when "01" => nextbox <= not character(0);
                                 when "10" => nextnextdouble <= not character(0);
                                              nextdouble <= nextdouble and not character(0);
                                              -- FIXME high/low bits
                                 when "11" => -- SO/SI (spare)
											when others => -- keep tools happy
                               end case;
                               -- 0011xx is miscellaneous stuff
                               else case character(2 downto 0) is
                                 when "000" => nextconceal <= control(3);
                                               conceal <= conceal and not control(3);
                                 -- FIXME: set immediately, but watch as they
                                 -- affect the bitstream engine ??
                                 when "001" => nextseparate <= '0';
                                 when "010" => nextseparate <= '1';
                                 -- 011 is a font switch on some systems
                                 when "011" => 
                                 -- Switch the colours but these affect the
                                 -- engine so nextbg is in fact 'set immediately'
                                 -- double check 100 - might need to be not nextbg
                                 when "100" => --nextbg <= "000";
                                               nextnextbg <= "000";
                                 when "101" => nextbg <= nextfg;
                                               nextnextbg <= nextfg;
                                 when "110" => nexthold <= '1';
                                               hold <= '1';
                                 when "111" => nexthold <= '0';
                                 when others =>
                               end case;
                               end if;
                             end if;
                           end if;
                           state(0) <= '1';
                           -- goes to 0011 or 1011
            -- Fetch the first processed character bits
            -- We don't need to do hold processing etc thi stime
            when "0011" => if control(2) = '1' and character(6 downto 5) = "00" then
                             symbol <= "000100000";
                           else
                             symbol <= "0"&character(7 downto 0);
                           end if;
                           state <= "0100";
            when "0100" => nextbits <= rom_data;
                           nextseparator <= separate_mask;
                           state <= "0101";
            -- 0101 is first byte (waiting for pixelfetch to rise)
            -- 0111 is later bytes (act on it falling)
            when "0101" | "0111" => 
                           -- FIXME: do these need to move into the pixelfetch
                           -- = 1 case
                           bright <= nextbright;
                           -- These directly affect the bistream engine so
                           -- must be set at this moment exactly
                           fg <= nextfg;
                           bg <= nextbg;
                           double <= nextdouble;
                           separate <= nextseparate;
                           graphics <= nextgraphics;
                           inverse <= nextinverse;
                           -- Double buffered for 'set-after' behaviour
                           nextfg <= nextnextfg;
                           nextbg <= nextnextbg;
                           nextgraphics <= nextnextgraphics;
                           -- These just affect the byte engine
                           -- The 'next' copy is thus effectively the
                           -- 'set-after' copy
                           conceal <= nextconceal;
                           flash <= nextflash;
                           hold <= nexthold;
                           box <= nextbox;
                           -- End of frame
                           if vsync = '0' then
                             state <= "0000";
                           -- Clock out the bits if we are being fetched
                           else if pixelfetch = '1' then
                             pixelbits <= nextbits;
                             separator <= nextseparator;
                             state <= "1000";
                           -- In state 0101 we spin for pixelfetch
                           -- In state 0111 its the end of line
                           else if state(1) = '1' then
                             -- End of line, reset the attributes
                             nextfg <= "111";
                             nextbg <= "000";
                             nextdouble <= '0';
                             nextgraphics <= '0';
                             nextnextfg <= "111";
                             nextnextbg <= "000";
                             nextnextdouble <= '0';
                             nextnextgraphics <= '0';
                             nexthold <= '0';
                             nextflash <= '0';
                             nextconceal <= '0';
                             nextbox <= '0';
                             nextseparate <= '0';
                             held <= '0';
                             
                             graphics <= '0';
                             double <= '0';
                             -- Clock out a blank lead in bit
                             pixelbits <= "00000000";
                             -- New character line ?
                             if pixel_y(3 downto 0) = "1111" then
                               videosave <= video;
                             else
                               video <= videosave;
                             end if;
                             if pixel_y(3 downto 0) = "0100" or
                                pixel_y(3 downto 0) = "1010" or
										  pixel_y(3 downto 0) = "1111" then
                                  separatingv <= '1';
                             else
                                separatingv <= '0';
                             end if;
                             -- New line processing state
                             state <= "0001";
                           end if;
                           end if;
                           end if;
            -- 1xxx rotate the pixelbits and display
            -- as we clock through the bits we move the video
            -- pointer on and load the next byte
            when "1000" => video <= video + 1;
            -- We propogate this early, it affects the symbol lookup
                           graphics <= nextgraphics;
                           double <= nextdouble;
                           state <= "1001";
            when "1001" => character <= video_data;
                           state <= "1010";
            -- 1010 see 0010, I have no pride 8)
            when "1011" => -- Decide what symbol to use for this control space
                           if control(2) = '1' and character(6 downto 5) = "00" then
                             if nextgraphics = '0' or hold = '0' or held = '0' or conceal = '1' or (flash = '1' and flashon = '0') then
                               symbol <= "000100000";
                             else
                               -- Use the held character and separate bit
                               symbol <= heldsym;
                               nextseparate <= symbol(8);
                             end if;
                           else
                             -- Use the character provided
                             symbol <= nextseparate & character(7 downto 0);
                           end if; 
                           state <= "1100";

            when "1100" => 
                           if isgraphics = '1' then
                             held <= '1';
                             heldsym <= separate & character(7 downto 0);
                           end if;

                           state <= "1101";
            when "1101" => nextbits <= rom_data;
                           nextseparator <= separate_mask;
                           state <= "1110";
            when "1110" => state <= "0111";
            -- Spare states
            when others => state <= "0000";
          end case;
        end if;
        doubler <= not doubler;
       end if;
       vclock <= not vclock;
      end if;
    end process;

    FontRom7: entity work.FontRom7
    port map(
      clk => pixelclk,
      addr => rom_address,
      data => rom_data
    );
    
end;

