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

entity rsFeeder is
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
    lastOut       : out std_logic;
    start         : out std_logic;
    inputValid    : out std_logic;
    inputLast     : out std_logic);
end entity rsFeeder;

architecture rtl of rsFeeder is

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

  constant cWidth : natural := log2(inputLength);
  constant cDepth : natural := 2;

  signal inCount         : natural range inputLength-1 downto 0 := 0;
  signal lastInput       : std_logic                            := '0';
  signal countIn_tvalid  : std_logic                            := '0';
  signal countIn_tdata   : std_logic_vector(cWidth downto 0)    := (others => '0');
  signal countIn_tready  : std_logic;
  signal countOut_tvalid : std_logic;
  signal countOut_tdata  : std_logic_vector(cWidth downto 0);
  signal countOut_tready : std_logic                            := '0';
  signal dataIn_tvalid_i : std_logic                            := '0';
  signal dataIn_tready_i : std_logic;
  signal dataOut_i       : std_logic_vector(width-1 downto 0);
  signal dataOut_tready  : std_logic                            := '0';
  signal outCount        : natural range outputLength downto 0  := 0;
  signal padCount        : natural range inputLength-1 downto 0 := 0;
  signal inputValid_i    : std_logic                            := '0';
  signal inputLast_i     : std_logic                            := '0';
  signal lastOut_i       : std_logic                            := '0';

begin  -- architecture rtl

  dataIn_tvalid_i <= dataIn_tvalid   and countIn_tready;
  dataIn_tready   <= dataIn_tready_i and countIn_tready;
  inputValid_i    <= dataIn_tvalid_i and dataIn_tready_i and countIn_tready;
  inputLast       <= inputLast_i;
  lastOut         <= lastOut_i;

  inputLogic: process (clk, resetn) is
  begin  -- process inputLogic
    if resetn = '0' then
      inCount        <= 0;
      lastInput      <= '0';
      countIn_tvalid <= '0';
      countIn_tdata  <= (others => '0');
      inputValid     <= '0';
      inputLast_i    <= '0';
    elsif rising_edge(clk) then
      inputValid  <= inputValid_i;
      inputLast_i <= lastInput or dataIn_tlast;

      if countIn_tready = '1' then
        countIn_tvalid <= '0';
      end if;

      if inputValid_i = '1' then
        lastInput      <= '0';

        if lastInput = '1' or dataIn_tlast = '1' then
          inCount        <= 0;
          countIn_tdata  <= dataIn_tlast &
                            std_logic_vector(to_unsigned(inCount, cWidth));
          countIn_tvalid <= '1';
        else
          inCount <= inCount + 1;

          if inCount = (inputLength-2) then
            lastInput <= '1';
          end if;
        end if;

      end if;
    end if;
  end process inputLogic;

  countFifo : rsFifo
    generic map (
      width => cWidth + 1,
      depth => cDepth)
    port map (
      clk            => clk,
      resetn         => resetn,
      dataIn_tdata   => countIn_tdata,
      dataIn_tvalid  => countIn_tvalid,
      dataIn_tready  => countIn_tready,
      dataOut_tdata  => countOut_tdata,
      dataOut_tvalid => countOut_tvalid,
      dataOut_tready => countOut_tready);
  
  fifo_i : rsFifo
    generic map (
      width => width,
      depth => inputLength+5)
    port map (
      clk            => clk,
      resetn         => resetn,
      dataIn_tdata   => dataIn_tdata,
      dataIn_tvalid  => dataIn_tvalid_i,
      dataIn_tready  => dataIn_tready_i,
      dataOut_tdata  => dataOut_i,
      dataOut_tvalid => open,
      dataOut_tready => dataOut_tready);

  outputLogic: process (clk, resetn) is
    variable countIn   : natural range inputLength downto 0;
    variable countInU  : unsigned(cWidth-1 downto 0);
  begin  -- process outputLogic
    if resetn = '0' then
      outCount        <= 0;
      dataOut_tready  <= '0';
      countOut_tready <= '0';
      padCount        <= 0;
      start           <= '0';
      dataOut         <= (others => '0');
      padLengthOut    <= 0;
      lastOut_i       <= '0';
    elsif rising_edge(clk) then
      countInU := unsigned(countOut_tdata(cWidth-1 downto 0));
      countIn  := to_integer(countInU);

      start          <= countOut_tready;
      
      dataOut <= (others => '0');
      countOut_tready <= '0';

      if padCount = 0 then
        dataOut <= dataOut_i;
      else
        dataOut <= (others => '0');
      end if;

      if outCount = 0 then
        dataOut_tready <= '0';

        if countOut_tvalid = '1' then
          outCount <= 1;
          countOut_tready <= '1';

          padLengthOut <= (inputLength-1) - countIn;
          lastOut_i    <= countOut_tdata(cWidth);
          
          if countIn = inputLength-1 then
            dataOut_tready <= '1';
            padCount       <= 0;
          else
            padCount <= countIn + 1;
          end if;
        end if;
      else
        if padCount = inputLength-1 then
          padCount <= 0;
        elsif padCount /= 0 then
          padCount <= padCount + 1;
        end if;

        if outCount = outputLength-1 then
          outCount <= 0;
        else
          outCount <= outCount + 1;          
        end if;

        if outCount = inputLength then
          dataOut_tready <= '0';
        elsif padCount = inputLength-1 then
          dataOut_tready <= '1';
        end if;
        
      end if;
      
    end if;
  end process outputLogic;
  
end architecture rtl;
