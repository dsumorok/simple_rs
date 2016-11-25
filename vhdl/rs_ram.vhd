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

entity rs_ram is  
  generic (
    WIDTH : natural := 8;
    DEPTH : natural := 2048);
  port (
    clk     : in  std_logic;
    inAddr  : in  natural range DEPTH-1 downto 0;
    inData  : in  std_logic_vector(WIDTH-1 downto 0);
    wrEn    : in  std_logic;
    outAddr : in  natural range DEPTH-1 downto 0;
    outData : out std_logic_vector(WIDTH-1 downto 0));
end entity rs_ram;

architecture rtl of rs_ram is
  type ramArray_t is array(DEPTH-1 downto 0) of
    std_logic_vector(WIDTH-1 downto 0);

  signal data : ramArray_t := (others => (others => '0'));

  signal inAddrBuffer : natural range DEPTH-1 downto 0 := 0;
  signal inBuffer     : std_logic_vector(WIDTH-1 downto 0);
  signal outBuffer    : std_logic_vector(WIDTH-1 downto 0);
  signal wrBuffer     : std_logic := '0';
begin  -- architecture rtl

  ramProcess: process (clk) is
  begin  -- process ramProcess
    if rising_edge(clk) then
      wrBuffer     <= wrEn;
      inBuffer     <= inData;
      inAddrBuffer <= inAddr;

      if wrBuffer = '1' then
        data(inAddrBuffer) <= inBuffer;
      end if;

      outBuffer          <= data(outAddr);
      outData      <= outBuffer;
    end if;
  end process ramProcess;

end architecture rtl;
