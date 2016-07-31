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

entity rsOutputStage is
  generic (
    width          : natural;
    inBlockLength  : natural;
    outBlockLength : natural;
    fifoDepth      : natural);
  port (
    clk              : in  std_logic;
    resetn           : in  std_logic;
    dataIn           : in  std_logic_vector(width-1 downto 0);
    startIn          : in  std_logic;
    padLengthIn      : in  natural range inBlockLength-1 downto 0;
    lastIn           : in  std_logic;
    padLengthValid   : in  std_logic;
    dataOut_tdata    : out std_logic_vector(width-1 downto 0);
    dataOut_tvalid   : out std_logic;
    dataOut_tlast    : out std_logic;
    dataOut_tready   : in  std_logic;
    dataIn_tready    : out std_logic;
    feederInputValid : in  std_logic;
    feederInputLast  : in  std_logic);
end entity rsOutputStage;

architecture rtl of rsOutputStage is
  component start2valid is
    generic (
      cWidth : natural;
      width  : natural;
      length : natural);
    port (
      clk          : in  std_logic;
      resetn       : in  std_logic;
      dataIn       : in  std_logic_vector(width-1 downto 0);
      startIn      : in  std_logic;
      countIn      : in  std_logic_vector(cWidth downto 0);
      dataOut      : out std_logic_vector(width downto 0);
      dataOutValid : out std_logic);
  end component start2valid;

  component rsFifo is
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
  end component rsFifo;

  function log2 (
    constant x : natural)
    return natural is

    variable result : natural := 0;
    variable xx     : natural;
  begin  -- function log2
    xx := x;
    
    while xx > 0 loop
      result := result + 1;
      xx     := xx / 2;
    end loop;

    return result;
  end function log2;

  constant cWidth : natural := log2(inBlockLength);

  signal fifoIn           : std_logic_vector(width downto 0);
  signal fifoIn_tvalid    : std_logic;
  signal fifoOut_tdata    : std_logic_vector(width downto 0);
  signal dataOut_tvalid_i : std_logic := '0';
  signal dataIn_tready_i  : std_logic := '1';

  constant lengthDiff     : integer := outBlockLength - inBlockLength;
  constant fifoMargin     : integer := (2 * abs(lengthDiff)) + 3;
  constant realFifoDepth  : integer := fifoDepth + fifoMargin;
  constant countThreshold : integer := realFifoDepth - fifoMargin;
  signal count            : integer;

  signal outputValid : std_logic := '0';

  signal countIn_tdata   : std_logic_vector(cWidth downto 0) := (others => '0');
  signal countOut_tready : std_logic := '0';
  signal countOut_tdata  : std_logic_vector(cWidth downto 0);

begin  -- architecture rtl

  start2valid_i : start2valid
    generic map (
      cWidth => cWidth,
      width  => width,
      length => outBlockLength)
    port map (
      clk          => clk,
      resetn       => resetn,
      dataIn       => dataIn,
      countIn      => countOut_tdata,
      startIn      => startIn,
      dataOut      => fifoIn,
      dataOutValid => fifoIn_tvalid);

  outputFifo : rsFifo
    generic map (
      width => width+1,
      depth => realFifoDepth)
    port map (
      clk            => clk,
      resetn         => resetn,
      dataIn_tdata   => fifoIn,
      dataIn_tvalid  => fifoIn_tvalid,
      dataIn_tready  => open,
      dataOut_tdata  => fifoOut_tdata,
      dataOut_tvalid => dataOut_tvalid_i,
      dataOut_tready => dataOut_tready);

  dataOut_tdata  <= fifoOut_tdata(width-1 downto 0);
  dataOut_tlast  <= fifoOut_tdata(width);
  countIn_tdata  <= lastIn &
                    std_logic_vector(to_unsigned(padLengthIn, cWidth));
  
  countFifo : rsFifo
    generic map (
      width => cWidth + 1,
      depth => ((realFifoDepth - 1) / outBlockLength) + 1)
    port map (
      clk            => clk,
      resetn         => resetn,
      dataIn_tdata   => countIn_tdata,
      dataIn_tvalid  => padLengthValid,
      dataIn_tready  => open,
      dataOut_tdata  => countOut_tdata,
      dataOut_tvalid => open,
      dataOut_tready => startIn);

  dataOut_tvalid <= dataOut_tvalid_i;

  genInputReady: process (clk, resetn) is
  begin  -- process genInputReady
    if resetn = '0' then
      dataIn_tready_i <= '1';
      count           <= 0;
      outputValid     <= '0';
    elsif rising_edge(clk) then
      outputValid <= dataOut_tready and dataOut_tvalid_i;

      if outputValid = '1' then
        if feederInputValid = '0' then
          count <= count - 1;
        elsif feederInputLast = '1' then
          count <= count + lengthDiff;
        end if;
      else
        if feederInputValid = '1' then
          if feederInputLast = '0' then
            count <= count + 1;
          else
            count <= count + 1 + lengthDiff;
          end if;
        end if;
      end if;

      if count < countThreshold then
        dataIn_tready_i <= '1';
      else
        dataIn_tready_i <= '0';
      end if;
      
    end if;
  end process genInputReady;

  dataIn_tready <= dataIn_tready_i;
  
end architecture rtl;
