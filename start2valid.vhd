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

entity start2valid is
  generic (
    cWidth : natural;
    width  : natural;
    length : natural);
  port (
    clk          : in  std_logic;
    resetn       : in  std_logic;
    dataIn       : in  std_logic_vector(width-1 downto 0);
    countIn      : in  std_logic_vector(cWidth downto 0);
    startIn      : in  std_logic;
    dataOut      : out std_logic_vector(width downto 0);
    dataOutValid : out std_logic);
end entity start2valid;

architecture rtl of start2valid is
  signal dataOutCount   : natural range 0 to length-1 := 0;
  signal lastIn         : std_logic                   := '0';
  signal skipCount      : unsigned(cWidth-1 downto 0) := (others => '0');
  signal dataOut_i      : std_logic_vector(width downto 0);
  signal dataOutValid_i : std_logic;
begin  -- architecture rtl

  bufferControl: process (clk, resetn) is
  begin  -- process bufferControl
    if resetn = '0' then
      dataOutValid   <= '0';
      dataoutvalid_i <= '0';
      dataOutCount   <= 0;
    elsif rising_edge(clk) then
      if startIn = '1' then
        dataOutValid_i <= '1';
        dataOutCount <= 0;
      elsif dataOutCount = length-1 then
        dataOutCount   <= 0;
        dataOutValid_i <= '0';
      else
        dataOutCount <= dataOutCount + 1;
      end if;

      if skipCount = 0 then
        dataOutValid <= dataOutValid_i;
      else
        dataOutValid <= '0';
      end if;
    end if;
  end process bufferControl;

  bufferData: process (clk) is
  begin  -- process bufferData
    if rising_edge(clk) then
      if resetn = '0' then
        lastIn <= '0';
      elsif startIn = '1' then
        lastIn <= countIn(cWidth);
      elsif dataOutCount = length-1 then
        lastIn <= '0';
      end if;

      if startIn = '1' then
        skipCount <= unsigned(countIn(cWidth-1 downto 0));
      elsif skipCount /= 0 then
        skipCount <= skipCount - "1";
      end if;

      if dataOutCount = length-2 and resetn = '1' then
        dataOut_i(width) <= lastIn;
      else
        dataOut_i(width) <= '0';
      end if;

      dataOut_i(width-1 downto 0) <= dataIn;
      dataOut                     <= dataOut_i;
    end if;
  end process bufferData;

end architecture rtl;
