--
--	Video RAM for the SocZ80 VGA
--
--	We use as much memory as we can get and we dual port it between
--	the host (R/W) and the pixel engine (R/O).
--
--	Although it's not really important in this case we multiplex the
--	devices by the low address bits so the video scan bandwidth scales
--	better for any future case.
--
--      Possibly we should do something clever to not waste the parity bits?
--
--	Question: Could we use the mostly unused RAMB8WER devices instead
--	and build a bigger VideoRAM ??
--
--	This module instantiates an 8K block of memory using four RAMB16WER
--	devices. You can then plug up to four of these into the VideoMemory
--	interface according to your taste.

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
library UNISIM;
use UNISIM.VComponents.all;

entity VideoRAM is
	port(
                cpu_clk             : in  std_logic;
                cpu_reset           : in  std_logic;
                cpu_write           : in  std_logic;
                cpu_cs              : in  std_logic;
                cpu_address         : in  std_logic_vector(12 downto 0);
                cpu_data_in         : in  std_logic_vector(7 downto 0);
                cpu_data_out        : out std_logic_vector(7 downto 0);
		
                video_clk           : in  std_logic;
                video_address       : in  std_logic_vector(12 downto 0);
                video_data_out      : out std_logic_vector(8 downto 0)
	);
end VideoRAM;

architecture behaviour of VideoRAM is

    signal cpu_data_out_full: std_logic_vector(8 downto 0);
    signal cpu_ram0_data_out : std_logic_vector(8 downto 0);
    signal cpu_ram1_data_out : std_logic_vector(8 downto 0);
    signal cpu_ram0_select : std_logic;
    signal cpu_ram1_select : std_logic;

    signal cpu_ram0_doa   : std_logic_vector(31 downto 0);
    signal cpu_ram0_dopa  : std_logic_vector(3 downto 0);
    signal cpu_ram1_doa   : std_logic_vector(31 downto 0);
    signal cpu_ram1_dopa  : std_logic_vector(3 downto 0);

    signal video_ram0_data_out : std_logic_vector(8 downto 0);
    signal video_ram1_data_out : std_logic_vector(8 downto 0);
    signal video_ram0_select : std_logic;
    signal video_ram1_select : std_logic;

    signal video_ram0_dob   : std_logic_vector(31 downto 0);
    signal video_ram0_dopb  : std_logic_vector(3 downto 0);
    signal video_ram1_dob   : std_logic_vector(31 downto 0);
    signal video_ram1_dopb  : std_logic_vector(3 downto 0);

    signal cpu_ram2_data_out : std_logic_vector(8 downto 0);
    signal cpu_ram3_data_out : std_logic_vector(8 downto 0);
    signal cpu_ram2_select : std_logic;
    signal cpu_ram3_select : std_logic;

    signal cpu_ram2_doa   : std_logic_vector(31 downto 0);
    signal cpu_ram2_dopa  : std_logic_vector(3 downto 0);
    signal cpu_ram3_doa   : std_logic_vector(31 downto 0);
    signal cpu_ram3_dopa  : std_logic_vector(3 downto 0);

    signal video_ram2_data_out : std_logic_vector(8 downto 0);
    signal video_ram3_data_out : std_logic_vector(8 downto 0);
    signal video_ram2_select : std_logic;
    signal video_ram3_select : std_logic;

    signal video_ram2_dob   : std_logic_vector(31 downto 0);
    signal video_ram2_dopb  : std_logic_vector(3 downto 0);
    signal video_ram3_dob   : std_logic_vector(31 downto 0);
    signal video_ram3_dopb  : std_logic_vector(3 downto 0);

begin

    -- multiplex between our BRAMs using the low bits as a chip select
    cpu_ram0_select <= cpu_cs and not cpu_address(0) and not cpu_address(1);
    cpu_ram1_select <= cpu_cs and cpu_address(0) and not cpu_address(1);
    cpu_ram2_select <= cpu_cs and not cpu_address(0) and cpu_address(1);
    cpu_ram3_select <= cpu_cs and cpu_address(0) and cpu_address(1);


    cpu_data_out_full <= cpu_ram0_data_out when cpu_ram0_select='1' else
                         cpu_ram1_data_out when cpu_ram1_select='1' else
                         cpu_ram2_data_out when cpu_ram2_select='1' else
                        cpu_ram3_data_out;
    cpu_data_out <= cpu_data_out_full(7 downto 0);

    video_ram0_select <= not video_address(0) and not video_address(1);
    video_ram1_select <= video_address(0) and not video_address(1);
    video_ram2_select <= not video_address(0) and video_address(1);
    video_ram3_select <= video_address(0) and video_address(1);

    video_data_out <= video_ram0_data_out when video_ram0_select='1' else
                      video_ram1_data_out when video_ram1_select='1' else
                      video_ram2_data_out when video_ram2_select='1' else
                      video_ram3_data_out;

    -- in 9-bit wide mode, we use DI[7:0], DIP[0], ADDR[13:3], and WE[3:0] all get connected to one write enable

    ram0: RAMB16BWER
    generic map (
        DATA_WIDTH_A => 9,
        DOA_REG => 0,
        DATA_WIDTH_B => 9,
        DOB_REG => 0,
        RSTTYPE => "SYNC",
        SIM_DEVICE => "SPARTAN6",
        WRITE_MODE_A => "WRITE_FIRST",
        WRITE_MODE_B => "WRITE_FIRST"
    )
    port map (
        CLKA => cpu_clk,
        ADDRA(13 downto 3) => cpu_address(12 downto 2),
        ADDRA(2 downto 0) => "000",
        DIA(31 downto 8) => "000000000000000000000000",
        DIA(7 downto 0) => cpu_data_in(7 downto 0),
        DIPA(3 downto 1) => "000",
        DIPA(0) => '0',
        DOA(31 downto 0) => cpu_ram0_doa,
        DOPA(3 downto 0) => cpu_ram0_dopa,
        ENA => cpu_ram0_select,
        WEA(0) => cpu_write,
        WEA(1) => cpu_write,
        WEA(2) => cpu_write,
        WEA(3) => cpu_write,
        RSTA => cpu_reset,
        REGCEA => '0',
        -- Port B (video)
        ADDRB(13 downto 3) => video_address(12 downto 2),
		  ADDRB(2 downto 0) => "000",
        CLKB => video_clk,
        DIB(31 downto 0) => "00000000000000000000000000000000",
        DIPB(3 downto 1) => "000",
        DIPB(0) => '0',
        DOB(31 downto 0) => video_ram0_dob,
        DOPB(3 downto 0) => video_ram0_dopb,
        ENB => video_ram0_select,
        WEB => (others => '0'),
        RSTB => cpu_reset,
        REGCEB => '0'
    );
    -- doing this here stops the simulator from complaining about partially associated formals.
    cpu_ram0_data_out <= cpu_ram0_dopa(0) & cpu_ram0_doa(7 downto 0);
    video_ram0_data_out <= video_ram0_dopb(0) & video_ram0_dob(7 downto 0);

    ram1: RAMB16BWER
    generic map (
        DATA_WIDTH_A => 9,
        DOA_REG => 0,
        DATA_WIDTH_B => 9,
        DOB_REG => 0,
        RSTTYPE => "SYNC",
        SIM_DEVICE => "SPARTAN6",
        WRITE_MODE_A => "WRITE_FIRST",
        WRITE_MODE_B => "WRITE_FIRST"
    )
    port map (
        CLKA => cpu_clk,
        ADDRA(13 downto 3) => cpu_address(12 downto 2),
        ADDRA(2 downto 0) => "000",
        DIA(31 downto 8) => "000000000000000000000000",
        DIA(7 downto 0) => cpu_data_in(7 downto 0),
        DIPA(3 downto 1) => "000",
        DIPA(0) => '0',
        DOA(31 downto 0) => cpu_ram1_doa,
        DOPA(3 downto 0) => cpu_ram1_dopa,
        ENA => cpu_ram1_select,
        WEA(0) => cpu_write,
        WEA(1) => cpu_write,
        WEA(2) => cpu_write,
        WEA(3) => cpu_write,
        RSTA => cpu_reset,
        REGCEA => '0',
        -- Port B (video)
        ADDRB(13 downto 3) => video_address(12 downto 2),
		  ADDRB(2 downto 0) => "000",
        CLKB => video_clk,
        DIB(31 downto 0) => "00000000000000000000000000000000",
        DIPB(3 downto 1) => "000",
        DIPB(0) => '0',
        DOB(31 downto 0) => video_ram1_dob,
        DOPB(3 downto 0) => video_ram1_dopb,
        ENB => video_ram1_select,
        WEB => (others => '0'),
        RSTB => cpu_reset,
        REGCEB => '0'
    );
    -- doing this here stops the simulator from complaining about partially associated formals.
    cpu_ram1_data_out <= cpu_ram1_dopa(0) & cpu_ram1_doa(7 downto 0);
    video_ram1_data_out <= video_ram1_dopb(0) & video_ram1_dob(7 downto 0);

    ram2: RAMB16BWER
    generic map (
        DATA_WIDTH_A => 9,
        DOA_REG => 0,
        DATA_WIDTH_B => 9,
        DOB_REG => 0,
        RSTTYPE => "SYNC",
        SIM_DEVICE => "SPARTAN6",
        WRITE_MODE_A => "WRITE_FIRST",
        WRITE_MODE_B => "WRITE_FIRST"
    )
    port map (
        CLKA => cpu_clk,
        ADDRA(13 downto 3) => cpu_address(12 downto 2),
        ADDRA(2 downto 0) => "000",
        DIA(31 downto 8) => "000000000000000000000000",
        DIA(7 downto 0) => cpu_data_in(7 downto 0),
        DIPA(3 downto 1) => "000",
        DIPA(0) => '0',
        DOA(31 downto 0) => cpu_ram2_doa,
        DOPA(3 downto 0) => cpu_ram2_dopa,
        ENA => cpu_ram2_select,
        WEA(0) => cpu_write,
        WEA(1) => cpu_write,
        WEA(2) => cpu_write,
        WEA(3) => cpu_write,
        RSTA => cpu_reset,
        REGCEA => '0',
        -- Port B (video)
        ADDRB(13 downto 3) => video_address(12 downto 2),
		  ADDRB(2 downto 0) => "000",
        CLKB => video_clk,
        DIB(31 downto 0) => "00000000000000000000000000000000",
        DIPB(3 downto 1) => "000",
        DIPB(0) => '0',
        DOB(31 downto 0) => video_ram2_dob,
        DOPB(3 downto 0) => video_ram2_dopb,
        ENB => video_ram2_select,
        WEB => (others => '0'),
        RSTB => cpu_reset,
        REGCEB => '0'
    );
    -- doing this here stops the simulator from complaining about partially associated formals.
    cpu_ram2_data_out <= cpu_ram2_dopa(0) & cpu_ram2_doa(7 downto 0);
    video_ram2_data_out <= video_ram2_dopb(0) & video_ram2_dob(7 downto 0);

    ram3: RAMB16BWER
    generic map (
        DATA_WIDTH_A => 9,
        DOA_REG => 0,
        DATA_WIDTH_B => 9,
        DOB_REG => 0,
        RSTTYPE => "SYNC",
        SIM_DEVICE => "SPARTAN6",
        WRITE_MODE_A => "WRITE_FIRST",
        WRITE_MODE_B => "WRITE_FIRST"
    )
    port map (
        CLKA => cpu_clk,
        ADDRA(13 downto 3) => cpu_address(12 downto 2),
        ADDRA(2 downto 0) => "000",
        DIA(31 downto 8) => "000000000000000000000000",
        DIA(7 downto 0) => cpu_data_in(7 downto 0),
        DIPA(3 downto 1) => "000",
        DIPA(0) => '0',
        DOA(31 downto 0) => cpu_ram3_doa,
        DOPA(3 downto 0) => cpu_ram3_dopa,
        ENA => cpu_ram3_select,
        WEA(0) => cpu_write,
        WEA(1) => cpu_write,
        WEA(2) => cpu_write,
        WEA(3) => cpu_write,
        RSTA => cpu_reset,
        REGCEA => '0',
        -- Port B (video)
        ADDRB(13 downto 3) => video_address(12 downto 2),
		  ADDRB(2 downto 0) => "000",
        CLKB => video_clk,
        DIB(31 downto 0) => "00000000000000000000000000000000",
        DIPB(3 downto 1) => "000",
        DIPB(0) => '0',
        DOB(31 downto 0) => video_ram3_dob,
        DOPB(3 downto 0) => video_ram3_dopb,
        ENB => video_ram3_select,
        WEB => (others => '0'),
        RSTB => cpu_reset,
        REGCEB => '0'
    );
    -- doing this here stops the simulator from complaining about partially associated formals.
    cpu_ram3_data_out <= cpu_ram3_dopa(0) & cpu_ram3_doa(7 downto 0);
    video_ram3_data_out <= video_ram3_dopb(0) & video_ram3_dob(7 downto 0);

end;
