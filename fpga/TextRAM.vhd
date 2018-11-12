--
--	Video RAM for the SocZ80 VGA - 2K text modes
--
--	Same interface as VideoMem but only has 2K which is mapped twice.
--	Accesses to the top 2K write to the low 2K but set parity, accesses
--	to the low 2K clear parity.
--

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
library UNISIM;
use UNISIM.VComponents.all;

entity TextRAM is
	port(
                cpu_clk             : in  std_logic;
                cpu_reset           : in  std_logic;
                cpu_write           : in  std_logic;
                cpu_cs              : in  std_logic;
                cpu_address         : in  std_logic_vector(15 downto 0);
                cpu_data_in         : in  std_logic_vector(7 downto 0);
                cpu_data_out        : out std_logic_vector(7 downto 0);
                cpu_wait            : out std_logic;

                video_clk           : in  std_logic;
                video_address       : in  std_logic_vector(15 downto 0);
                video_data_out      : out std_logic_vector(8 downto 0)
	);
end TextRAM;

architecture behaviour of TextRAM is

    signal cpu_ram0_data_out : std_logic_vector(8 downto 0);

    signal cpu_ram0_doa   : std_logic_vector(31 downto 0);
    signal cpu_ram0_dopa  : std_logic_vector(3 downto 0);

    signal video_ram0_data_out : std_logic_vector(8 downto 0);

    signal video_ram0_dob   : std_logic_vector(31 downto 0);
    signal video_ram0_dopb  : std_logic_vector(3 downto 0);
    
    signal ram0_addr        : std_logic_vector(10 downto 0);
    signal ram0_cs          : std_logic;
    signal ram0_data        : std_logic_vector(7 downto 0);
    
    -- Delay for read cycles
    signal is_read          : std_logic;
    signal was_read         : std_logic;

begin

    video_data_out <= video_ram0_dopb(0) & video_ram0_dob(7 downto 0);
    cpu_data_out <= cpu_ram0_doa(7 downto 0);
    
    -- in 9-bit wide mode, we use DI[7:0], DIP[0], ADDR[13:3], and WE[3:0] all get connected to one write enable

    cpu_wait <= cpu_cs and (not cpu_write and not was_read);
    
    is_read <= cpu_cs and not cpu_write;

    ramwait: process(cpu_clk)
    begin
      if rising_edge(cpu_clk) then
        was_read <= is_read;
      end if;
    end process;
    


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
        ADDRA(13 downto 3) => cpu_address(10 downto 0),
        ADDRA(2 downto 0) => "000",
        DIA(31 downto 8) => "000000000000000000000000",
        DIA(7 downto 0) => cpu_data_in(7 downto 0),
        DIPA(3 downto 1) => "000",
        -- Eww I'm a bad boy 8)
        DIPA(0) => cpu_address(11),
        DOA(31 downto 0) => cpu_ram0_doa,
        DOPA(3 downto 0) => cpu_ram0_dopa,
        ENA => cpu_cs or was_read,
        WEA(0) => cpu_write,
        WEA(1) => cpu_write,
        WEA(2) => cpu_write,
        WEA(3) => cpu_write,
        RSTA => cpu_reset,
        REGCEA => '0',
        -- Port B (video)
        ADDRB(13 downto 3) => video_address(10 downto 0),
        ADDRB(2 downto 0) => "000",
        CLKB => video_clk,
        DIB(31 downto 0) => "00000000000000000000000000000000",
        DIPB(3 downto 1) => "000",
        DIPB(0) => '0',
        DOB(31 downto 0) => video_ram0_dob,
        DOPB(3 downto 0) => video_ram0_dopb,
        ENB => '1',
        WEB => (others => '0'),
        RSTB => cpu_reset,
        REGCEB => '0'
    );
    -- doing this here stops the simulator from complaining about partially associated formals.
    cpu_ram0_data_out <= cpu_ram0_dopa(0) & cpu_ram0_doa(7 downto 0);
    video_ram0_data_out <= video_ram0_dopb(0) & video_ram0_dob(7 downto 0);


end;
