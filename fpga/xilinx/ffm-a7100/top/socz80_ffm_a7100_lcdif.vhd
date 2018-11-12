----------------------------
-- ULX3S Top level for MINIMIG
-- http://github.com/emard
----------------------------

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.ALL;
use IEEE.numeric_std.all;
-- use IEEE.MATH_REAL.ALL;

library unisim;
use unisim.vcomponents.all;

use work.video_mode_pack.all;

entity socz80_ffm_a7100_lcdif is
generic
(
  C_dvid_ddr: boolean := true; -- use vendor-specific DDR-differential output buffeers
  C_test_picture: boolean := false; -- false: normal, true: show video test picture
  C_video_mode: integer := 5 -- 0:640x360, 1:640x480, 2:800x480, 3:800x600, 5:1024x768
);
port
(
  clk_100mhz_p, clk_100mhz_n: in std_logic;
  -- RS232
  uart3_txd: out std_logic; -- rs232 txd
  uart3_rxd: in std_logic; -- rs232 rxd
  -- SD card (SPI)
  sd_m_clk, sd_m_cmd: out std_logic;
  sd_m_d: inout std_logic_vector(3 downto 0); 
  sd_m_cdet: in std_logic;
  -- SDRAM
  dr_clk: out std_logic;
  dr_cke: out std_logic;
  dr_cs_n: out std_logic;
  dr_a: out std_logic_vector(12 downto 0);
  dr_ba: out std_logic_vector(1 downto 0);
  dr_ras_n, dr_cas_n: out std_logic;
  dr_dqm: out std_logic_vector(3 downto 0);
  dr_d: inout std_logic_vector(31 downto 0);
  dr_we_n: out std_logic;
  -- FFM Module IO
  fioa: inout std_logic_vector(7 downto 0);
  fiob: inout std_logic_vector(31 downto 20);
  -- ADV7513 video chip
  dv_clk: inout std_logic;
  dv_sda: inout std_logic;
  dv_scl: inout std_logic;
  dv_int: inout std_logic;
  dv_de: inout std_logic;
  dv_hsync: inout std_logic;
  dv_vsync: inout std_logic;
  dv_spdif: inout std_logic;
  dv_mclk: inout std_logic;
  dv_i2s: inout std_logic_vector(3 downto 0);
  dv_sclk: inout std_logic;
  dv_lrclk: inout std_logic;
  dv_d: inout std_logic_vector(23 downto 0);
  -- Low-Cost HDMI video out
  vid_d_p, vid_d_n: out std_logic_vector(3 downto 0)
);
end;

architecture struct of socz80_ffm_a7100_lcdif is
    constant clk_freq_mhz        : natural := 125; -- this is the frequency which the PLL outputs, in MHz.

    -- SDRAM configuration
    constant sdram_address_width : natural := 22;
    constant sdram_column_bits   : natural := 8;
    constant cycles_per_refresh  : natural := (64000*clk_freq_mhz)/4096-1;

    -- For simulation, we don't need a long init stage. but for real DRAM we need approx 101us.
    -- The constant below has a different value when interpreted by the synthesis and simulator 
    -- tools in order to achieve the desired timing in each.
    constant sdram_startup_cycles: natural := 101 * clk_freq_mhz
    -- pragma translate_off
    - 10000 -- reduce the value the simulator uses
    -- pragma translate_on
    ;

    -- signals for clocking
    signal clk                  : std_logic;  -- buffered system clock (all logic should be clocked by this)

    -- system reset signals
    signal power_on_reset       : std_logic_vector(1 downto 0) := (others => '1');
    signal system_reset         : std_logic;
    signal reset_button_clk1    : std_logic;
    signal reset_button_sync    : std_logic; -- reset button signal, synchronised to our clock
    signal reset_request_uart   : std_logic; -- reset request signal from FTDI UART (when you send "!~!~!~" to the UART, this line is asserted)

    -- CPU control
    signal coldboot             : std_logic;
    signal cpu_clk_enable       : std_logic;
    signal cpu_m1_cycle         : std_logic;
    signal cpu_req_mem          : std_logic;
    signal cpu_req_io           : std_logic;
    signal req_mem              : std_logic;
    signal req_io               : std_logic;
    signal req_read             : std_logic;
    signal req_write            : std_logic;
    signal virtual_address      : std_logic_vector(15 downto 0);
    signal physical_address     : std_logic_vector(25 downto 0);
    signal mem_wait             : std_logic;
    signal cpu_wait             : std_logic;
    signal dram_wait            : std_logic;
    signal mmu_wait             : std_logic;
    signal spimaster0_wait      : std_logic;
    signal spimaster1_wait      : std_logic;
    signal vram_wait            : std_logic;

    -- chip selects
    signal mmu_cs               : std_logic;
    signal rom_cs               : std_logic;
    signal sram_cs              : std_logic;
    signal vram_cs 	        : std_logic;
    signal dram_cs              : std_logic;
    signal uartA_cs             : std_logic;
    signal uart0_cs             : std_logic;
    signal timer_cs             : std_logic;
    signal spimaster0_cs        : std_logic;
    signal spimaster1_cs        : std_logic;
    signal clkscale_cs          : std_logic;
    signal gpio_cs              : std_logic;
    signal iodevice_cs          : std_logic;
    signal sevenseg_cs	        : std_logic;
    signal vga_cs               : std_logic;

    -- data bus
    signal cpu_data_in          : std_logic_vector(7 downto 0);
    signal cpu_data_out         : std_logic_vector(7 downto 0);
    signal rom_data_out         : std_logic_vector(7 downto 0);
    signal sram_data_out        : std_logic_vector(7 downto 0);
    signal vram_data_out        : std_logic_vector(7 downto 0);
    signal dram_data_out        : std_logic_vector(7 downto 0);
    signal uart0_data_out       : std_logic_vector(7 downto 0);
    signal timer_data_out       : std_logic_vector(7 downto 0);
    signal spimaster0_data_out  : std_logic_vector(7 downto 0);
    signal spimaster1_data_out  : std_logic_vector(7 downto 0);
    signal mmu_data_out         : std_logic_vector(7 downto 0);
    signal clkscale_out         : std_logic_vector(7 downto 0);
    signal gpio_data_out        : std_logic_vector(7 downto 0);
    signal iodevice_data_out    : std_logic_vector(7 downto 0);
    signal sevenseg_data_out    : std_logic_vector(7 downto 0);
    signal vga_data_out         : std_logic_vector(7 downto 0);
	 
    -- GPIO
    signal gpio_input           : std_logic_vector(7 downto 0);
    signal gpio_output          : std_logic_vector(7 downto 0);

    -- Interrupts
    signal cpu_interrupt_in     : std_logic;
    signal timer_interrupt      : std_logic;
    signal uart0_interrupt      : std_logic;
	 
    -- Video engine
    signal pixel_clock          : std_logic;
    signal pixel_vclock         : std_logic;
    signal pixel_fetch          : std_logic;
    signal pixel_col            : unsigned(9 downto 0);
    signal pixel_row            : unsigned(8 downto 0);
    signal pixel                : std_logic_vector(7 downto 0);
    signal vga_reset            : std_logic;
    signal vsync		: std_logic;
    signal hsync		: std_logic;
    signal blank		: std_logic;
    signal rgb8                 : std_logic_vector(7 downto 0);
    signal S_vga_r, S_vga_g, S_vga_b : std_logic_vector(2 downto 0);
    signal S_vga_vsync, S_vga_hsync, S_vga_blank : std_logic;
    signal S_vgatest_r, S_vgatest_g, S_vgatest_b : std_logic_vector(7 downto 0);
    signal S_vgatest_vsync, S_vgatest_hsync, S_vgatest_blank : std_logic;

     -- Video RAM
    signal pixel_data           : std_logic_vector(8 downto 0);
    signal pixel_address        : std_logic_vector(15 downto 0);

    -- Turdo
    signal turbo_enable_clk     : std_logic;
    signal turbo_clk            : std_logic;

  -- FFM-A7100 related signals


  -- keyboard
  alias ps2_clk1 : std_logic is fioa(6);
  alias ps2_data1 : std_logic is fioa(4);
  -- mouse
  alias ps2_clk2 : std_logic is fioa(3);
  alias ps2_data2 : std_logic is fioa(1);

  -- END ALIASING

  signal clk_100MHz: std_logic; -- converted from differential to single ended
  signal clk_fb_main, clk_fb_sdram: std_logic; -- feedback internally used in clock generator
  signal clk_pixel: std_logic;
  signal clk_pixel_shift: std_logic;

  signal n_joy1: std_logic_vector(5 downto 0);
  signal n_joy2: std_logic_vector(5 downto 0);
 
  signal ps2k_clk_in: std_logic;
  signal ps2k_clk_out: std_logic;
  signal ps2k_dat_in: std_logic;
  signal ps2k_dat_out: std_logic;	
  signal ps2m_clk_in: std_logic;
  signal ps2m_clk_out: std_logic;
  signal ps2m_dat_in: std_logic;
  signal ps2m_dat_out: std_logic;	
 
  constant pll_reset: std_logic := '0';
  signal pll_locked_main: std_logic;

  -- emard video
  --signal S_vga_r, S_vga_g, S_vga_b : std_logic_vector(2 downto 0);
  --signal S_vga_vsync, S_vga_hsync, S_vga_blank : std_logic;
  --signal S_vgatest_r, S_vgatest_g, S_vgatest_b : std_logic_vector(7 downto 0);
  --signal S_vgatest_vsync, S_vgatest_hsync, S_vgatest_blank : std_logic;

  signal ddr_d: std_logic_vector(3 downto 0);
  signal dvid_crgb: std_logic_vector(7 downto 0); -- clock, red, green, blue

  -- end emard video
begin
  clkin_ibufgds: ibufgds
  port map (I => clk_100MHz_P, IB => clk_100MHz_N, O => clk_100MHz);

  clk_main: mmcme2_base
  generic map
  (
    clkin1_period    => 10.0,       --   100      MHz (10 ns)
    clkfbout_mult_f  => 12.5,       --  1250.0    MHz *12.5 common multiply
    divclk_divide    => 2,          --   625.0    MHz /2 common divide
    clkout0_divide_f => 2.0,        --   312.5    MHz /2.0 divide
    clkout1_divide   => 5,          --   125.0    MHz /5 divide
    clkout2_divide   => 10,         --    62.5    MHz /10 divide
    clkout3_divide   => 5,          --   125.0    MHz /10 divide phase 180
    clkout3_phase    => 180.0,      --   180.0 degrees
    clkout4_divide   => 10,         --    62.5    MHz /10 divide
    bandwidth        => "LOW"
  )
  port map
  (
    pwrdwn   => '0',
    rst      => '0',
    clkin1   => clk_100MHz,
    clkfbin  => clk_fb_main,
    clkfbout => clk_fb_main,
    clkout0  => clk_pixel_shift,    --  312.5     MHz
    clkout1  => clk,                --  125.0     MHz
    clkout2  => clk_pixel,          --   62.5     MHz
    clkout3  => dr_clk,             --  125.0     MHz 180 deg
    clkout4  => open,               --   62.5     MHz
    locked   => pll_locked_main
  );

  -- btn(0) used as reset has inverted logic
  sd_m_d(1) <= '1';
  sd_m_d(2) <= '1';

  -- Joystick bits(5-0) = fire2,fire,right,left,down,up mapped to GPIO header
  n_joy1(3)<= fiob(20) ; -- up
  n_joy1(2)<= fiob(21) ; -- down
  n_joy1(1)<= fiob(22) ; -- left
  n_joy1(0)<= fiob(23) ; -- right
  n_joy1(4)<= fiob(24) ; -- fire
  n_joy1(5)<= fiob(25) ; -- fire2

  n_joy2(3)<= fiob(26) ; -- up
  n_joy2(2)<= fiob(27) ; -- down
  n_joy2(1)<= fiob(28) ; -- left 
  n_joy2(0)<= fiob(29) ; -- right  
  n_joy2(4)<= fiob(30) ; -- fire
  n_joy2(5)<= fiob(31) ; -- fire2 
  
    -- Hold CPU reset high for 8 clock cycles on startup,
    -- and when the user presses their reset button.
    process(clk)
    begin
        if rising_edge(clk) then
            -- Xilinx advises using two flip-flops are used to bring external
            -- signals which feed control logic into our clock domain.
            reset_button_clk1 <= '0'; -- connect reset button if available
            reset_button_sync <= reset_button_clk1;

            -- reset the system when requested
            if (power_on_reset(0) = '1' or reset_button_sync = '1' or reset_request_uart = '1') then
                system_reset <= '1';
            else
                system_reset <= '0';
            end if;

            -- shift 0s into the power_on_reset shift register from the MSB
            power_on_reset <= '0' & power_on_reset(power_on_reset'length-1 downto 1);

        end if;
    end process;

    -- VGA output pins
    -- vsync_pin <= vsync;
    -- hsync_pin <= hsync;
    -- rgb8_pins <= rgb8;

    -- GPIO input signal routing
    gpio_input(0) <= coldboot;
    -- FIXME: wouldn't it be fun to have a turbo switch ?
    gpio_input(1) <= '0';
    gpio_input(2) <= '0';
    gpio_input(3) <= '0';
    gpio_input(4) <= '0';
    gpio_input(5) <= '0';
    gpio_input(6) <= '0';
    gpio_input(7) <= '0';

    -- GPIO output signal routing
    --led(0) <= gpio_output(0);
    --led(1) <= gpio_output(1);
    --led(2) <= gpio_output(2);
    --led(3) <= gpio_output(3);
    --led(5) <= gpio_output(5);
    --led(6) <= gpio_output(6);
    --led(7) <= gpio_output(7);
    -- User LED (LED1) on Papilio Pro indicates when the CPU is being asked to wait (eg by the SDRAM cache)
    --led(4) <= cpu_wait;

    -- Interrupt signal for the CPU
    cpu_interrupt_in <= (timer_interrupt or uart0_interrupt);

    -- Turbo (eventually)
    -- Wiring it to a switch at this point is helpful for debugging
    turbo_enable_clk <= turbo_clk; -- or not sw(0);

    -- Z80 CPU core
    cpu: entity work.Z80cpu
    port map (
                 reset => system_reset,
                 clk => clk,
                 clk_enable => turbo_enable_clk,
                 m1_cycle => cpu_m1_cycle,
                 interrupt => cpu_interrupt_in,
                 nmi => '0',
                 req_mem => cpu_req_mem,
                 req_io => cpu_req_io,
                 req_read => req_read,
                 req_write => req_write,
                 mem_wait => cpu_wait,
                 address => virtual_address,
                 data_in => cpu_data_in,
                 data_out => cpu_data_out
             );

    -- Memory management unit
    mmu: entity work.MMU
    port map (
                reset => system_reset,
                clk => clk,
                address_in => virtual_address,
                address_out => physical_address,
                cpu_data_in => cpu_data_out,
                cpu_data_out => mmu_data_out,
                req_mem_in => cpu_req_mem,
                req_io_in  => cpu_req_io,
                req_mem_out => req_mem,
                req_io_out => req_io,
                req_read => req_read,
                req_write => req_write,
                io_cs => mmu_cs,
                cpu_wait => mmu_wait,
                access_violated => open -- for now!!
            );

    -- This process determines which IO or memory device the CPU is addressing
    -- and asserts the appropriate chip select signals.
    cs_process: process(req_mem, req_io, physical_address, virtual_address)
    begin
        -- memory chip selects: default to unselected
        rom_cs   <= '0';
        sram_cs  <= '0';
        vram_cs  <= '0';
        dram_cs  <= '0';

        -- io chip selects: default to unselected
        mmu_cs        <= '0';
        timer_cs      <= '0';
        spimaster0_cs <= '0';
        spimaster1_cs <= '0';
        clkscale_cs   <= '0';
        gpio_cs       <= '0';
        iodevice_cs   <= '0';
        sevenseg_cs   <= '0';
        vga_cs        <= '0';

        -- memory address decoding
        -- address space is organised as:
        -- 0x0 000 000 - 0x0 FFF FFF  16MB DRAM (cached)    (mapped to 8MB DRAM twice)
        -- 0x1 000 000 - 0x1 FFF FFF  16MB DRAM (uncached)  (mapped to 8MB DRAM twice)
        -- 0x2 000 000 - 0x2 000 FFF   4KB monitor ROM      (FPGA block RAM)
        -- 0x2 001 000 - 0x2 001 FFF   4KB SRAM             (FPGA block RAM)
        -- 0x2 010 000 - 0x2 01F FFF  64KB SRAM (partial)   (FPGA block RAM for video)
        -- 0x2 020 000 - 0x3 FFF FFF  unused space for future expansion
        if physical_address(25) = '0' then
        -- bottom 32MB: DRAM handles this
            dram_cs <= req_mem;
        else if physical_address(24 downto 16) = "000000001" then
	    vram_cs <= req_mem;
	else
        -- top 32MB: other memory devices
            case physical_address(24 downto 12) is
                when "0000000000000" =>  rom_cs <= req_mem;
                when "0000000000001" => sram_cs <= req_mem;
                when others => -- undecoded memory space
            end case;
        end if;
        end if;

        -- IO address decoding
        case virtual_address(7 downto 3) is
            when "00000" => uart0_cs            <= req_io;  -- 00 ... 07
            when "00010" => timer_cs            <= req_io;  -- 10 ... 17
            when "00011" => spimaster0_cs       <= req_io;  -- 18 ... 1F
            when "00100" => gpio_cs             <= req_io;  -- 20 ... 27
            -- 28-2F is uart1 on the standard edition
            when "00110" => spimaster1_cs       <= req_io;  -- 30 ... 37
            -- unused ports
            when "10000" => sevenseg_cs         <= req_io;  -- 80 ... 87
            when "11011" => vga_cs              <= req_io;  -- D8 ... DF
            when "11101" => iodevice_cs         <= req_io;  -- E8 ... EF
            when "11110" => clkscale_cs         <= req_io;  -- F0 ... F7
            when "11111" => mmu_cs              <= req_io;  -- F8 ... FF
            when others =>
        end case;
    end process;

    -- the selected memory device can request the CPU to wait
    mem_wait <=
       dram_wait       when dram_cs='1' else
       spimaster0_wait when spimaster0_cs='1' else
       spimaster1_wait when spimaster1_cs='1' else
       vram_wait       when vram_cs='1' else
       '0';

    -- the MMU can, at any time, request the CPU wait (this is used when
    -- translating IO to memory requests, to implement a wait state for
    -- the "17th page")
    cpu_wait <= mem_wait or mmu_wait;

    -- input mux for CPU data bus
    cpu_data_in <=
       rom_data_out        when        rom_cs='1' else
       dram_data_out       when       dram_cs='1' else
       sram_data_out       when       sram_cs='1' else
       vram_data_out       when       vram_cs='1' else
       uart0_data_out      when      uart0_cs='1' else
       timer_data_out      when      timer_cs='1' else
       mmu_data_out        when        mmu_cs='1' else
       spimaster0_data_out when spimaster0_cs='1' else
       spimaster1_data_out when spimaster1_cs='1' else
       clkscale_out        when   clkscale_cs='1' else
       gpio_data_out       when       gpio_cs='1' else
       iodevice_data_out   when   iodevice_cs='1' else
       sevenseg_data_out   when   sevenseg_cs='1' else
       vga_data_out	   when        vga_cs='1' else
       -- Load empty I/O space as 0xFF
       "11111111"          when        req_io='1' else
       rom_data_out; -- default case

   dram: entity work.DRAM
   generic map(
               sdram_address_width => sdram_address_width,
               sdram_column_bits   => sdram_column_bits,
               sdram_startup_cycles=> sdram_startup_cycles,
               cycles_per_refresh  => cycles_per_refresh
              )
   port map(
               clk             => clk,
               reset           => '0', -- important to note that we DO NOT reset the SDRAM controller on reset (it would stop refreshing, which would be bad)

               -- interface to synthetic CPU
               cs => dram_cs,
               req_read => req_read,
               req_write => req_write,
               mem_address => physical_address(24 downto 0),
               mem_wait => dram_wait,
               data_in => cpu_data_out,
               data_out => dram_data_out,
               coldboot => coldboot,

               -- interface to hardware SDRAM chip
               -- SDRAM_CLK       => dr_CLK,
               SDRAM_CKE       => dr_CKE,
               SDRAM_CS        => dr_CS_n,
               SDRAM_nRAS      => dr_RAS_n,
               SDRAM_nCAS      => dr_CAS_n,
               SDRAM_nWE       => dr_WE_n,
               SDRAM_DQM       => dr_DQM(1 downto 0),
               SDRAM_BA        => dr_BA,
               SDRAM_ADDR      => dr_A,
               SDRAM_DQ        => dr_D(15 downto 0)
           );
   dr_dqm(3 downto 2) <= (others => '1');
   dr_d(31 downto 16) <= (others => 'Z');

   -- 4KB system ROM implemented in block RAM
   rom: entity work.MonZ80
   port map(
               clk => clk,
               A => physical_address(11 downto 0),
               D => rom_data_out
           );

   -- 4KB SRAM memory implemented in block RAM
   sram: entity work.SSRAM
   generic map(
                  AddrWidth => 12
              )
   port map(
               clk => clk,
               ce => sram_cs,
               we => req_write,
               A => physical_address(11 downto 0),
               DIn => cpu_data_out,
               DOut => sram_data_out
           );
   -- Video memory
   -- TextMem creates a 2K video memory for text mode
   -- VideoMem creates a 32K memory for graphics
   tram: entity work.TextMem
   port map(
               cpu_clk => clk,
               cpu_reset => system_reset,
               cpu_write => req_write,
               cpu_address => physical_address(15 downto 0),
               cpu_data_in => cpu_data_out,
               cpu_data_out => vram_data_out,
               cpu_cs => vram_cs,
               cpu_wait => vram_wait,

               video_clk => pixel_vclock,
               video_address => pixel_address,
               video_data_out => pixel_data
           );

   -- UART connected to FTDI USB UART
   uart0: entity work.uart_interface
   generic map ( watch_for_reset => 1, clk_frequency => (clk_freq_mhz * 1000000) )
   port map(
               clk => clk,
               reset => system_reset,
               reset_out => reset_request_uart, -- result of watching for reset sequence on the input
               serial_in => uart3_rxd,
               serial_out => uart3_txd,
               serial_rts => open,
               serial_cts => '0',
               cpu_address => virtual_address(2 downto 0),
               cpu_data_in => cpu_data_out,
               cpu_data_out => uart0_data_out,
               enable => uart0_cs,
               interrupt => uart0_interrupt,
               req_read => req_read,
               req_write => req_write
           );

   -- Timer device (internally scales the clock to 1MHz)
   timer: entity work.timer
   generic map ( clk_frequency => (clk_freq_mhz * 1000000) )
   port map(
               clk => clk,
               reset => system_reset,
               cpu_address => virtual_address(2 downto 0),
               data_in => cpu_data_out,
               data_out => timer_data_out,
               enable => timer_cs,
               req_read => req_read,
               req_write => req_write,
               interrupt => timer_interrupt
           );

   -- SPI master device connected to Papilio Pro 8MB flash ROM
   spimaster0: entity work.spimaster
   port map(
               clk => clk,
               reset => system_reset,
               cpu_address => virtual_address(2 downto 0),
               cpu_wait => spimaster0_wait,
               data_in => cpu_data_out,
               data_out => spimaster0_data_out,
               enable => spimaster0_cs,
               req_read => req_read,
               req_write => req_write,
               slave_cs => open,
               slave_clk => open,
               slave_mosi => open,
               slave_miso => '1'
           );

   -- SPI master device connected to LogicStart ADC
   -- On the 'standard edition' this is connected to the SD Card
   spimaster1: entity work.spimaster
   port map(
               clk => clk,
               reset => system_reset,
               cpu_address => virtual_address(2 downto 0),
               cpu_wait => spimaster1_wait,
               data_in => cpu_data_out,
               data_out => spimaster1_data_out,
               enable => spimaster1_cs,
               req_read => req_read,
               req_write => req_write,
               slave_cs => sd_m_d(3),
               slave_clk => sd_m_clk,
               slave_mosi => sd_m_cmd,
               slave_miso => sd_m_d(0)
           );

   -- GPIO to FPGA pins and/or internal signals
   gpio: entity work.gpio
   port map(
               clk => clk,
               reset => system_reset,
               cpu_address => virtual_address(2 downto 0),
               data_in => cpu_data_out,
               data_out => gpio_data_out,
               enable => gpio_cs,
               read_notwrite => req_read,
               input_pins => gpio_input,
               output_pins => gpio_output
           );

   -- IO Devices found on various wings where we want commonality
   IODevice: entity work.IODevice
   port map (
               clk => clk,
               reset => system_reset,
               cpu_address => virtual_address(2 downto 0),
               data_in => cpu_data_out,
               data_out => iodevice_data_out,
               enable => iodevice_cs,
               read_notwrite => req_read,
               sound => open,
               joystick0 => "11111111",
               -- 0xFF indicates not present to the OS
               joystick1 => "11111111",
               joystick2 => "11111111"
   );

   -- Wing specific device MUX and enumeration
   --sevenseg: entity work.sevenseg
   --port map (
   --            clk => clk,
   --            reset => system_reset,
   --            cpu_address => virtual_address(2 downto 0),
   --            data_in => cpu_data_out,
   --            data_out => sevenseg_data_out,
   --            enable => sevenseg_cs,
   --            read_notwrite => req_read,
   --            segment_pins => segment_pins,
   --            mux_pins => mux_pins
   --);

   -- An attempt to allow the CPU clock to be scaled back to run
   -- at slower speeds without affecting the clock signal sent to
   -- IO devices. Basically this was an attempt to make CP/M games
   -- playable :) Very limited success. Might be simpler to remove
   -- this entirely.
   clkscale: entity work.clkscale
   port map (
               clk => clk,
               reset => system_reset,
               cpu_address => virtual_address(2 downto 0),
               data_in => cpu_data_out,
               data_out => clkscale_out,
               enable => clkscale_cs,
               read_notwrite => req_read,
               clk_enable => turbo_clk
           );

  G_not_test_picture:
  if not C_test_picture generate
    -- VGA graphics engine
    TextEngine: entity work.TextEngine7
    port map (
               clk => clk,
               reset => system_reset,
               cpu_address => virtual_address(2 downto 0),
               data_in => cpu_data_out,
               data_out => vga_data_out,
               enable => vga_cs,
               read_notwrite => req_read,

               -- Video port
               vsync => vsync,
               hsync => hsync,
               vga_reset => vga_reset,
               pixelclk => pixel_clock,
               pixelfetch => pixel_fetch,
               pixelcol => pixel_col,
               pixelrow => pixel_row,
               pixel => pixel,

               -- Video RAM
              video_clock => pixel_vclock,
              video_data => pixel_data,
              video_address => pixel_address
    );

    VideoVGA:entity work.VideoVGA
    port map (
               clk => clk,
               reset => system_reset,
               vsync => vsync,
               hsync => hsync,
               videoblank => blank,
               rgb8 => rgb8,
               scan_reset => vga_reset,
               scan_clk => pixel_clock,
               scan_fetch => pixel_fetch,
               scan_col => pixel_col,
               scan_row => pixel_row,
               scan_data => pixel
    );

    S_vga_r(2 downto 0) <= rgb8(7 downto 5);
    S_vga_g(2 downto 0) <= rgb8(4 downto 2);
    S_vga_b(2 downto 1) <= rgb8(1 downto 0);
    -- S_vga_b(2 downto 1) <= "01";
    S_vga_hsync <= hsync;
    S_vga_vsync <= vsync;
    S_vga_blank <= blank;
  end generate;


  G_yes_test_picture:
  if C_test_picture generate
    vga_test_picture: entity work.vga
    generic map
    (
      C_resolution_x => C_video_modes(C_video_mode).visible_width,
      C_hsync_front_porch => C_video_modes(C_video_mode).h_front_porch,
      C_hsync_pulse => C_video_modes(C_video_mode).h_sync_pulse,
      C_hsync_back_porch => C_video_modes(C_video_mode).h_back_porch,
      C_resolution_y => C_video_modes(C_video_mode).visible_height,
      C_vsync_front_porch => C_video_modes(C_video_mode).v_front_porch,
      C_vsync_pulse => C_video_modes(C_video_mode).v_sync_pulse,
      C_vsync_back_porch => C_video_modes(C_video_mode).v_back_porch
    )
    port map
    (
      clk_pixel => clk_pixel, -- 25 MHz
      test_picture => '1', -- show test picture
      red_byte => (others => '0'),
      green_byte => (others => '0'),
      blue_byte => (others => '0'),
      vga_r => S_vgatest_r,
      vga_g => S_vgatest_g,
      vga_b => S_vgatest_b,
      vga_hsync => S_vgatest_hsync,
      vga_vsync => S_vgatest_vsync,
      vga_blank => S_vgatest_blank
    );

    S_vga_r(2 downto 0) <= S_vgatest_r(7 downto 5);
    S_vga_g(2 downto 0) <= S_vgatest_g(7 downto 5);
    S_vga_b(2 downto 0) <= S_vgatest_b(7 downto 5);
    S_vga_hsync <= S_vgatest_hsync;
    S_vga_vsync <= S_vgatest_vsync;
    S_vga_blank <= S_vgatest_blank;
  end generate;


  vga2dvi_converter: entity work.vga2dvid
  generic map
  (
      C_ddr     => C_dvid_ddr,
      C_depth   => 3 -- 3bpp (3 bit per pixel)
  )
  port map
  (
      clk_pixel => clk_pixel, -- 62.5 MHz
      clk_shift => clk_pixel_shift, -- 5*62.5 MHz

      in_red   => S_vga_r,
      in_green => S_vga_g,
      in_blue  => S_vga_b,

      in_hsync => S_vga_hsync,
      in_vsync => S_vga_vsync,
      in_blank => S_vga_blank,

      -- single-ended output ready for vedor specific output buffers
      out_clock => dvid_crgb(7 downto 6),
      out_red   => dvid_crgb(5 downto 4),
      out_green => dvid_crgb(3 downto 2),
      out_blue  => dvid_crgb(1 downto 0)
  );

  G_dvi_sdr: if not C_dvid_ddr generate
  -- no vendor specific DDR and differntial buffers
  -- clk_pixel_shift = 10x clk_pixel
  gpdi_sdr_se: for i in 0 to 3 generate
    vid_d_p(i) <= dvid_crgb(2*i);
    vid_d_n(i) <= not dvid_crgb(2*i);
  end generate;
  end generate;

  G_dvi_ddr: if C_dvid_ddr generate
  -- vendor specific DDR and differential buffers
  -- clk_pixel_shift = 5x clk_pixel
  gpdi_ddr_diff: for i in 0 to 3 generate
    gpdi_ddr:   oddr generic map (DDR_CLK_EDGE => "SAME_EDGE", INIT => '0', SRTYPE => "SYNC") port map (D1=>dvid_crgb(2*i), D2=>dvid_crgb(2*i+1), Q=>ddr_d(i), C=>clk_pixel_shift, CE=>'1', R=>'0', S=>'0');
    gpdi_diff:  obufds port map(i => ddr_d(i), o => vid_d_p(i), ob => vid_d_n(i));
  end generate;
  end generate;

  -- adv7513 routing
  dv_clk <= clk_pixel;
  dv_de <= not S_vga_blank;
  dv_hsync <= S_vga_hsync;
  dv_vsync <= S_vga_vsync;
  dv_d(23 downto 21) <= S_vga_r;
  dv_d(20 downto 16) <= (others => S_vga_r(0));
  dv_d(15 downto 13) <= S_vga_g;
  dv_d(12 downto 8) <= (others => S_vga_g(0));
  dv_d(7 downto 5) <= S_vga_b;
  dv_d(4 downto 0) <= (others => S_vga_b(0));

  i2c_send: entity work.i2c_sender
  port map
  (
        clk => clk_pixel,
        resend => '1',
        sioc => dv_scl,
        siod => dv_sda
  );

end struct;
