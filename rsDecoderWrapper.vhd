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

library work;
use work.gf.all;
use work.rsUtil.all;

entity rsDecoderWrapper is
  generic (
    M            : natural := 8;
    n            : natural := 254;
    k            : natural := 204;
    primPoly     : natural := 0;
    alpha        : natural := 2;
    outputParity : boolean := true;
    extraCycles  : natural := 0);
  port (
    clk            : in  std_logic;
    resetn         : in  std_logic;
    dataIn_tdata   : in  std_logic_vector(M-1 downto 0);
    dataIn_tvalid  : in  std_logic;
    dataIn_tready  : out std_logic;
    dataIn_tlast   : in  std_logic;
    dataOut_tdata  : out std_logic_vector(M-1 downto 0);
    dataOut_tvalid : out std_logic;
    dataOut_tready : in  std_logic;
    dataOut_tlast  : out std_logic);
end entity rsDecoderWrapper;

architecture rtl of rsDecoderWrapper is

  function getOutputBlockSize (
    constant n            : natural;
    constant k            : natural;
    constant outputParity : boolean)
    return natural is
  begin  -- function getOutputBlockSize
    if outputParity then
      return n;
    else
      return k;
    end if;
  end function getOutputBlockSize;

  component rsDecoder is
    generic (
      M           : natural;
      n           : natural;
      k           : natural;
      primPoly    : natural := 0;
      alpha       : natural := 2;
      extraCycles : natural := 0);
    port (
      clk      : in  std_logic;
      resetn   : in  std_logic;
      inEl     : in  std_logic_vector(M-1 downto 0);
      inStart  : in  std_logic;
      outEl    : out std_logic_vector(M-1 downto 0);
      outStart : out std_logic);
  end component rsDecoder;

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
      lastOut       : out std_logic;
      start         : out std_logic;
      inputValid    : out std_logic;
      inputLast     : out std_logic);
  end component rsFeeder;

  component rsOutputStage is
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
  end component rsOutputStage;

  constant outputBlockSize : natural := getOutputBlockSize(n, k, outputParity);
  constant fifoDepth       : natural := coreDecoderLatency(n, k) + 2*n + 10;

  signal decoderIn        : std_logic_vector(M-1 downto 0) := (others => '0');
  signal decoderInStart   : std_logic                      := '0';
  signal decoderOutCount  : natural range 0 to n-1         := 0;
  signal decoderOut       : std_logic_vector(M-1 downto 0);
  signal decoderOutStart  : std_logic;
  signal decoderOut_1     : std_logic_vector(M-1 downto 0) := (others => '0');
  signal dataIn_tvalid_i  : std_logic                      := '0';
  signal dataIn_tready_i  : std_logic                      := '0';
  signal outputReady      : std_logic                      := '1';
  signal padLength        : natural range n-1 downto 0;
  signal tlast            : std_logic;
  signal feederInputValid : std_logic;
  signal feederInputLast  : std_logic;

begin  -- architecture rtl

  dataIn_tready   <= dataIn_tready_i and outputReady;
  dataIn_tvalid_i <= dataIn_tvalid   and outputReady;
  
  feeder_i : rsFeeder
    generic map (
      width        => M,
      inputLength  => n,
      outputLength => n+extraCycles)
    port map (
      clk            => clk,
      resetn         => resetn,
      dataIn_tdata   => dataIn_tdata,
      dataIn_tvalid  => dataIn_tvalid_i,
      dataIn_tready  => dataIn_tready_i,
      dataIn_tlast   => dataIn_tlast,
      dataOut        => decoderIn,
      padLengthOut   => padLength,
      lastOut        => tlast,
      start          => decoderInStart,
      inputValid     => feederInputValid,
      inputLast      => feederInputLast);

  decoder_i : rsDecoder
    generic map (
      M           => M,
      n           => n,
      k           => k,
      primPoly    => primPoly,
      alpha       => alpha,
      extraCycles => extraCycles)
    port map (
      clk      => clk,
      resetn   => resetn,
      inEl     => decoderIn,
      inStart  => decoderInStart,
      outEl    => decoderOut,
      outStart => decoderOutStart);

  outputStage_i : rsOutputStage
    generic map (
      width          => M,
      inBlockLength  => n,
      outBlockLength => outputBlockSize,
      fifoDepth      => fifoDepth)
    port map (
      clk              => clk,
      resetn           => resetn,
      dataIn           => decoderOut,
      startIn          => decoderOutStart,
      padLengthIn      => padLength,
      lastIn           => tlast,
      padLengthValid   => decoderInStart,
      dataOut_tdata    => dataOut_tdata,
      dataOut_tvalid   => dataOut_tvalid,
      dataOut_tlast    => dataOut_tlast,
      dataOut_tready   => dataOut_tready,
      dataIn_tready    => outputReady,
      feederInputValid => feederInputValid,
      feederInputLast  => feederInputLast);

end architecture rtl;
