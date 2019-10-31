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

entity rsDecoder is
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
end entity rsDecoder;

architecture rtl of rsDecoder is
                       
  component calcSyndromes is
    generic (
      M        : natural;
      n        : natural;
      k        : natural;
      primPoly : natural;
      alpha    : natural);
    port (
      clk            : in  std_logic;
      resetn         : in  std_logic;
      inEl           : in  std_logic_vector;
      inStart        : in  std_logic;
      syndromes      : out std_logic_vector;
      syndromesStart : out std_logic);
  end component calcSyndromes;

  component mea is
    generic (
      M           : natural;
      n           : natural;
      k           : natural;
      primPoly    : natural;
      alpha       : natural;
      extraCycles : natural);
    port (
      clk            : in  std_logic;
      resetn         : in  std_logic;
      syndromesIn    : in  std_logic_vector;
      syndromesStart : in  std_logic;
      errorLocator   : out std_logic_vector;
      errorEvaluator : out std_logic_vector;
      outStart       : out std_logic);
  end component mea;

  component calcErrors is
    generic (
      M        : natural;
      n        : natural;
      k        : natural;
      primPoly : natural;
      alpha    : natural);
    port (
      clk            : in  std_logic;
      resetn         : in  std_logic;
      errorEvaluator : in  std_logic_vector;
      errorLocator   : in  std_logic_vector;
      inStart        : in  std_logic;
      errorVal       : out std_logic_vector;
      errorStart     : out std_logic);
  end component calcErrors;

  component correctErrors is
    generic (
      M        : natural;
      n        : natural;
      k        : natural);
    port (
      clk           : in  std_logic;
      resetn        : in  std_logic;
      dataIn        : in  std_logic_vector;
      dataInStart   : in  std_logic;
      errorsIn      : in  std_logic_vector;
      errorsInStart : in  std_logic;
      dataOut       : out std_logic_vector;
      dataOutStart  : out std_logic);
  end component correctErrors;

  constant t            : natural := (n-k)/2;

  signal inEl_i         : std_logic_vector(M-1 downto 0) := (others => '0');
  signal inElStart_i    : std_logic := '0';
  signal syndromes      : std_logic_vector(2*t*m-1 downto 0);
  signal syndromesStart : std_logic;
  signal errorLocator   : std_logic_vector((t+1)*M-1 downto 0);
  signal errorEvaluator : std_logic_vector(t*M-1 downto 0);
  signal meaStart       : std_logic;
  signal errorVal       : std_logic_vector(M-1 downto 0);
  signal errorStart     : std_logic := '0';
  signal outStart_i     : std_logic := '0';

Begin  -- architecture rtl

  bufferInput: process (clk) is
  begin  -- process bufferInput
    if rising_edge(clk) then
      inEl_i      <= inEl;
      inElStart_i <= inStart;
    end if;
  end process bufferInput;

  syndromes_i : calcSyndromes
    generic map (
      M        => M,
      n        => n,
      k        => k,
      primPoly => primPoly,
      alpha    => alpha)
    port map (
      clk            => clk,
      resetn         => resetn,
      inEl           => inEl_i,
      inStart        => inElStart_i,
      syndromes      => syndromes,
      syndromesStart => syndromesStart);

  mea_i : mea
    generic map (
      M           => M,
      n           => n,
      k           => k,
      primPoly    => primPoly,
      alpha       => alpha,
      extraCycles => extraCycles)
    port map (
      clk            => clk,
      resetn         => resetn,
      syndromesIn    => syndromes,
      syndromesStart => syndromesStart,
      errorLocator   => errorLocator,
      errorEvaluator => errorEvaluator,
      outStart       => meaStart);

  errors_i : calcErrors
    generic map (
      M        => M,
      n        => n,
      k        => k,
      primPoly => primPoly,
      alpha    => alpha)
    port map (
      clk            => clk,
      resetn         => resetn,
      errorEvaluator => errorEvaluator,
      errorLocator   => errorLocator,
      inStart        => meaStart,
      errorVal       => errorVal,
      errorStart     => errorStart);

  correct_i : correctErrors
    generic map (
      M        => M,
      n        => n,
      k        => k)
    port map (
      clk           => clk,
      resetn        => resetn,
      dataIn        => inEl_i,
      dataInStart   => inElStart_i,
      errorsIn      => errorVal,
      errorsInStart => errorStart,
      dataOut       => outEl,
      dataOutStart  => outStart);

end architecture rtl;
