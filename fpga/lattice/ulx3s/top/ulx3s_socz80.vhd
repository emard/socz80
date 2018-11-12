-- (c)EMARD
-- License=BSD

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
-- use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;
-- use ieee.math_real.all; -- to calculate log2 bit size

library ecp5u;
use ecp5u.components.all;

entity ulx3s_socz80 is
  generic
  (
    -- video parameters common for vgahdmi and vgatext
    C_dvid_ddr: boolean := true; -- generate HDMI with DDR
    C_video_mode: integer := 1 -- 0:640x360, 1:640x480, 2:800x480, 3:800x600, 5:1024x768
  );
  port
  (
  clk_25mhz: in std_logic;  -- main clock input from 25MHz clock source

  -- UART0 (FTDI USB slave serial)
  ftdi_rxd: out   std_logic;
  ftdi_txd: in    std_logic;
  -- FTDI additional signaling
  ftdi_ndtr: inout  std_logic;
  ftdi_ndsr: inout  std_logic;
  ftdi_nrts: inout  std_logic;
  ftdi_txden: inout std_logic;

  -- UART1 (WiFi serial)
  wifi_rxd: out   std_logic;
  wifi_txd: in    std_logic;
  -- WiFi additional signaling
  wifi_en: inout  std_logic := 'Z'; -- '0' will disable wifi by default
  wifi_gpio0, wifi_gpio5, wifi_gpio16, wifi_gpio17: inout std_logic := 'Z';

  -- USB
  usb_fpga_dp, usb_fpga_dn: inout std_logic; -- single ended
  --usb_fpga_pu_dp, usb_fpga_pu_dn: out std_logic; -- pull up/down control
  --usb_fpga_bd_dp, usb_fpga_bd_dn: inout std_logic; -- differential bidirectional

  -- ADC MAX11125
  adc_csn, adc_sclk, adc_mosi: out std_logic;
  adc_miso: in std_logic;

  -- SDRAM
  sdram_clk: out std_logic;
  sdram_cke: out std_logic;
  sdram_csn: out std_logic;
  sdram_rasn: out std_logic;
  sdram_casn: out std_logic;
  sdram_wen: out std_logic;
  sdram_a: out std_logic_vector (12 downto 0);
  sdram_ba: out std_logic_vector(1 downto 0);
  sdram_dqm: out std_logic_vector(1 downto 0);
  sdram_d: inout std_logic_vector (15 downto 0);

  -- Onboard blinky
  led: out std_logic_vector(7 downto 0);
  btn: in std_logic_vector(6 downto 0);
  sw: in std_logic_vector(3 downto 0);
  oled_csn, oled_clk, oled_mosi, oled_dc, oled_resn: out std_logic;

  -- GPIO
  gp, gn: inout std_logic_vector(27 downto 0);

  -- SHUTDOWN: logic '1' here will shutdown power on PCB >= v1.7.5
  shutdown: out std_logic := '0';

  -- Audio jack 3.5mm
  audio_l, audio_r, audio_v: inout std_logic_vector(3 downto 0) := (others => 'Z');

  -- Onboard antenna 433 MHz
  ant_433mhz: out std_logic;

  -- Digital Video (differential outputs)
  gpdi_dp, gpdi_dn: out std_logic_vector(3 downto 0);

  -- i2c shared for digital video and RTC
  gpdi_scl, gpdi_sda: inout std_logic;

  -- Flash ROM (SPI0)
  flash_miso   : in      std_logic;
  flash_mosi   : out     std_logic;
  --flash_clk    : out     std_logic; -- not GPIO, needs vendor-specific module
  flash_csn    : out     std_logic;
  flash_holdn  : out     std_logic := '1';
  flash_wpn    : out     std_logic := '1';

  -- SD card (SPI1)
  sd_cmd: inout std_logic := 'Z';
  sd_d: inout std_logic_vector(3 downto 0) := (others => 'Z');
  sd_clk: inout std_logic := 'Z';
  sd_cdn, sd_wp: in std_logic;

  nc: inout std_logic_vector(8 downto 0) -- not connected pins to force compiler to use some logic
  );
end;

architecture Behavioral of ulx3s_socz80 is
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
    signal clk_feedback         : std_logic;  -- PLL clock feedback
    signal clk_unbuffered       : std_logic;  -- unbuffered system clock
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
	 
     -- Video RAM
    signal pixel_data           : std_logic_vector(8 downto 0);
    signal pixel_address        : std_logic_vector(15 downto 0);

    -- Turdo
    signal turdo_enable_clk     : std_logic;
    signal turdo_clk            : std_logic;
    
    -- ULX3S related signals
    signal S_rxd, S_txd: std_logic;
    signal rs232_break: std_logic; -- CPU reset, also reset flash clk module
    signal S_flash_csn, S_flash_clk: std_logic;

    signal clk_pixel_shift, clk_pixel: std_logic;
    signal dvid_crgb: std_logic_vector(7 downto 0);
    signal ddr_d: std_logic_vector(3 downto 0);

    component OLVDS
      port(A: in std_logic; Z, ZN: out std_logic);
    end component;
begin
  ddr_640x480_25MHz: if (C_video_mode=0 or C_video_mode=1) generate
  clk_25M: entity work.clk_25_125_250_25_83
    port map
    (
      CLKI        =>  clk_25MHz,
      CLKOP       =>  clk_pixel_shift, -- 125 MHz
      CLKOS       =>  open,            -- 250 MHz
      CLKOS2      =>  clk_pixel,       --  25 MHz
      CLKOS3      =>  open             --  83.333 MHz
    );
    clk <= clk_pixel_shift; -- CPU clock 125 MHz
  end generate;

  G_no_passthru_autodetect: if true generate
    -- both USB and WiFi can upload binary executable to f32c
    -- (not both on the same time)
    S_rxd <= ftdi_txd;
    ftdi_rxd <= S_txd;
    wifi_gpio0 <= btn(0); -- pressing BTN0 will escape to ESP32 file select menu
    -- sd_d(3) <= S_f32c_sd_csn;
    -- sd_clk <= S_f32c_sd_clk;
    -- S_f32c_sd_miso <= sd_d(0);
    -- sd_cmd <= S_f32c_sd_mosi;
    sd_d(2 downto 1) <= (others => '1');
  end generate;
  
  led(6 downto 1) <= btn(6 downto 1);

  G_dvid_sdr: if not C_dvid_ddr generate
    -- this module instantiates single ended inverters to simulate differential
    G_sdr_se: for i in 0 to 3 generate
      gpdi_dp(i) <= dvid_crgb(2*i);
      gpdi_dn(i) <= not dvid_crgb(2*i);
    end generate;
  end generate;

  G_dvid_ddr: if C_dvid_ddr generate
    -- this module instantiates vendor specific buffers for ddr-differential
    G_ddr_diff: for i in 0 to 3 generate
      gpdi_ddr: ODDRX1F port map(D0=>dvid_crgb(2*i), D1=>dvid_crgb(2*i+1), Q=>ddr_d(i), SCLK=>clk_pixel_shift, RST=>'0');
      gpdi_diff: OLVDS port map(A => ddr_d(i), Z => gpdi_dp(i), ZN => gpdi_dn(i));
    end generate;
  end generate;

  flash_clock: entity work.ecp5_flash_clk
  port map
  (
    flash_csn => rs232_break,
    flash_clk => S_flash_clk
  );
  flash_csn <= S_flash_csn;

end Behavioral;
