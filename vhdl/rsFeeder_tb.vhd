--  Copyright (c) 2015, Daniel Sumorok
--  All rights reserved.

--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions are met:

--  1. Redistributions of source code must retain the above copyright notice, this
--     list of conditions and the following disclaimer.
--  2. Redistributions in binary form must reproduce the above copyright notice,
--     this list of conditions and the following disclaimer in the documentation
--     and/or other materials provided with the distribution.

--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
--  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
--  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
--  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
--  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
--  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
--  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
--  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
--  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
--  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity rsFeeder_tb is
  
end entity rsFeeder_tb;

architecture rtl of rsFeeder_tb is

  component rsFeeder is
    generic (
      width        : natural;
      inputLength  : natural;
      outputLength : natural);
    port (
      clk           : in  std_logic;
      resetn        : in  std_logic;
      dataIn_tdata  : in  std_logic_vector(width-1 downto 0);
      dataIn_tvalid : in  std_logic;
      dataIn_tready : out std_logic;
      dataIn_tlast  : in  std_logic;
      dataOut       : out std_logic_vector(width-1 downto 0);
      padLengthOut  : out natural range inputLength-1 downto 0;
      start         : out std_logic);
  end component rsFeeder;

  constant CLK_PER : time    := 10 ns;
  constant WIDTH   : natural := 8;
  
  signal clk            : std_logic                          := '0';
  signal resetn         : std_logic                          := '1';
  signal dataIn1_tdata  : std_logic_vector(width-1 downto 0) := (others => '0');
  signal dataIn1_tvalid : std_logic                          := '0';
  signal dataIn1_tready : std_logic;
  signal dataIn1_tlast  : std_logic                          := '0';
  signal valid1         : std_logic                          := '0';
  signal last1          : std_logic                          := '0';
  
begin  -- architecture rtl

  genClk: process is
  begin  -- process genClk
    clk <= '0';
    wait for CLK_PER / 2;

    clk <= '1';
    wait for CLK_PER / 2;
  end process genClk;

  setReset: process is
  begin  -- process setReset
    resetn <= '0';

    wait for CLK_PER*10;
    wait until falling_edge(clk);
    resetn <= '1';

    wait;
  end process setReset;

  setValid1: process is
  begin  -- process setValid1
    valid1 <= '0';
    last1  <= '0';
    wait until resetn = '1';

    for i in 0 to 39 loop
      valid1 <= '1';
      wait for CLK_PER;      
    end loop;  -- i

    for i in 0 to 93 loop
      valid1 <= not valid1;
      wait for CLK_PER;      
    end loop;  -- i

    valid1 <= '1';
    last1  <= '1';
    wait for CLK_PER;      
    
    for i in 0 to 9 loop
      last1  <= '0';
      valid1 <= '1';
      wait for CLK_PER;      
    end loop;  -- i

    for i in 0 to 8 loop
      if i = 8 then
        last1 <= '1';
      else
        last1 <= '0';
      end if;
      valid1 <= '1';
      wait for CLK_PER;      
    end loop;  -- i


    for j in 0 to 5 loop
      for i in 0 to 3 loop
        if i = 3 then
          last1 <= '1';
        else
          last1 <= '0';
        end if;
        valid1 <= '1';
        wait for CLK_PER;      
      end loop;  -- i
    end loop;

    valid1 <= '0';
    last1  <= '0';

    wait for CLK_PER * 30;
    valid1 <= '1';
    last1  <= '1';

    wait for CLK_PER*10;
    
    valid1 <= '0';
    last1  <= '0';
    
    wait;    
  end process setValid1;

  setValid: process (clk) is
  begin  -- process setValid
    if rising_edge(clk) then
      dataIn1_tvalid <= valid1;
      dataIn1_tlast  <= last1;
    end if;
  end process setValid;
  
  setData: process (clk, resetn) is
    variable data : unsigned(WIDTH-1 downto 0) := (others => '0');
  begin  -- process setData
    if resetn = '0' then
      dataIn1_tdata <= (others => '0');
    elsif rising_edge(clk) then
      if dataIn1_tvalid = '1' and dataIn1_tready = '1' then
        data          := unsigned(dataIn1_tdata);
        data          := data + "1";
        dataIn1_tdata <= std_logic_vector(data);
      end if;
    end if;
  end process setData;

  uut1 : rsFeeder
    generic map (
      width        => WIDTH,
      inputLength  => 10,
      outputLength => 12)
    port map (
      clk           => clk,
      resetn        => resetn,
      dataIn_tdata  => dataIn1_tdata,
      dataIn_tvalid => dataIn1_tvalid,
      dataIn_tready => dataIn1_tready,
      dataIn_tlast  => dataIn1_tlast,
      dataOut       => open,
      padLengthOut  => open,
      start         => open);
  
end architecture rtl;
