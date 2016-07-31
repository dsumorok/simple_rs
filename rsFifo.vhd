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

entity rsFifo is
  generic (
    width : natural;
    depth : natural);
  port (
    clk            : in  std_logic;
    resetn         : in  std_logic;
    dataIn_tdata   : in  std_logic_vector(width-1 downto 0);
    dataIn_tvalid  : in  std_logic;
    dataIn_tready  : out std_logic;
    dataOut_tdata  : out std_logic_vector(width-1 downto 0);
    dataOut_tvalid : out std_logic;
    dataOut_tready : in  std_logic);
end entity rsFifo;

architecture rtl of rsFifo is
  signal inAddress      : natural range depth-1 downto 0 := 0;
  signal outAddress     : natural range depth-1 downto 0 := 0;
  signal nextOutAddress : natural range depth-1 downto 0 := 0;
  signal count          : natural range depth downto 0   := 0;
  signal full           : std_logic                      := '0';
  signal empty          : std_logic                      := '1';
  signal wea            : std_logic                      := '0';
  signal wea_1          : std_logic                      := '0';
  signal rdFifo         : std_logic                      := '0';

  type ramArray_t is array(depth-1 downto 0) of
    std_logic_vector(width-1 downto 0);

  signal fifoData : ramArray_t := (others => (others => '0'));

begin  -- architecture rtl

  calcWEA: process (dataIn_tvalid, full) is
  begin  -- process calcWEA
    if dataIn_tvalid = '1' and full = '0' then
      wea <= '1';
    else
      wea <= '0';
    end if;    
  end process calcWEA;

  calcRdFifo: process (dataOut_tready, empty) is
  begin  -- process calcRdFifo
    if dataOut_tready = '1' and empty = '0' then
      rdFifo <= '1';
    else
      rdFifo <= '0';
    end if;
  end process calcRdFifo;
  
  mainProc: process (clk) is
  begin  -- process mainProc
    if rising_edge(clk) then
      if resetn = '0' then
        inAddress      <= 0;
        outAddress     <= 0;
        nextOutAddress <= 1;
        count          <= 0;
        full           <= '0';
        empty          <= '1';
        wea_1          <= '0';
      else
        wea_1 <= wea;

        if wea = '0' and rdFifo = '1' then
          count <= count - 1;
        elsif wea = '1' and rdFifo = '0' then
          count <= count + 1;
        end if;

        if wea = '1' then
          if inAddress = depth-1 then
            inAddress <= 0;
          else
            inAddress <= inAddress + 1;
          end if;
        end if;

        if rdFifo = '1' then
          if nextOutAddress = depth-1 then
            nextOutAddress <= 0;
          else
            nextOutAddress <= nextOutAddress + 1;
          end if;

          outAddress     <= nextOutAddress;
        end if;

        if count = (depth-1) then
          if wea = '1' and rdFifo = '0' then
            full <= '1';
          else
            full <= '0';
          end if;
        end if;

        if count = 1 then
          if rdFifo = '1' and wea_1 = '0' then
            empty <= '1';
          else
            empty <= '0';
          end if;
        end if;

      end if;
    end if;
  end process mainProc;

  fifoRAM: process (clk) is
    variable RAMRdAddress : natural range depth-1 downto 0;
  begin  -- process fifoRAM
    if rising_edge(clk) then
      if rdFifo = '1' then
        RAMRdAddress := nextOutAddress;
      else
        RAMRdAddress := outAddress;
      end if;

      dataOut_tdata <= fifoData(RAMRdAddress);

      if wea = '1' then
        fifoData(inAddress) <= dataIn_tdata;
      end if;
    end if;
  end process fifoRAM;
  
  dataIn_tready  <= not full;
  dataOut_tvalid <= not empty;
  
end architecture rtl;
