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

entity calcErrors is
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
end entity calcErrors;

architecture rtl of calcErrors is
  package gf_pack is new work.gf generic map (
      M           => M,
      primPolyReq => primPoly,
      alpha       => alpha);
  use gf_pack.all;

  component chien_search is
    generic (
      alpha    : natural;
      M        : natural;
      primPoly : natural;
      polyLen  : natural);
    port (
      clk       : in std_logic;
      resetn    : in std_logic;
      coefsIn   : in std_logic_vector;
      loadCoefs : in std_logic;
      valueOut  : out std_logic_vector;
      outValid  : out std_logic);
  end component chien_search;

  constant Q      : natural := 2**M;
  constant t      : natural := (n-k)/2;
  constant pAlpha : natural := inverse(alpha);
  
  signal elVal         : gfEl_t               := gfZero;
  signal numerator     : gfEl_t               := gfZero;
  signal numerator_1   : gfEl_t               := gfZero;
  signal denominator   : gfEl_t               := gfZero;
  signal denominator_1 : gfEl_t               := gfZero;
  signal deomInv       : gfEl_t               := gfZero;
  signal err           : std_logic            := '0';
  signal err_1         : std_logic            := '0';
  signal errorVal_i    : gfEl_t               := gfZero;

  signal elOut         : std_logic_vector(M-1 downto 0);
  signal eeOut         : std_logic_vector(M-1 downto 0);
  signal d_elOut       : std_logic_vector(M-1 downto 0);
  signal eeIn          : std_logic_vector((t+1)*M-1 downto 0) := (others => '0');
  signal d_el          : std_logic_vector((t+1)*M-1 downto 0) := (others => '0');

  signal inStart_1  : std_logic                    := '0';
  signal cDone      : std_logic                    := '0';
  signal outValidSR : std_logic_vector(5 downto 0) := (others => '0');
begin  -- architecture rtl

  setElCoef: process (clk) is
  begin  -- process setElCoef
    if rising_edge(clk) then
      if inStart = '1' then
        elVal <= gfOne;
      else
        elVal <= elVal * pAlpha;
      end if;
    end if;
  end process setElCoef;

  setdEl: process (errorLocator) is
  begin  -- process setdEl
    for i in 0 to t-1 loop
      for j in 0 to M-1 loop
        if (i mod 2) = 0 then
          d_el(i*M + j) <= errorLocator((i+1)*M + j);
        end if;
      end loop;  -- j
    end loop;  -- i
  end process setdEl;

  pipeInStart: process (clk) is
  begin  -- process pipeInStart
    if rising_edge(clk) then
      inStart_1 <= inStart;
    end if;
  end process pipeInStart;
  
  eeIn(t*M-1 downto 0) <=  errorEvaluator;
  
  evalEl : chien_search
    generic map (
      alpha    => pAlpha,
      M        => M,
      primPoly => primPolyUsed,
      polyLen  => t+1)
    port map (
      clk       => clk,
      resetn    => '1',
      coefsIn   => errorLocator,
      loadCoefs => inStart_1,
      valueOut  => elOut,
      outValid  => open);
  
  evalNum : chien_search
    generic map (
      alpha    => pAlpha,
      M        => M,
      primPoly => primPolyUsed,
      polyLen  => t+1)
    port map (
      clk       => clk,
      resetn    => '1',
      coefsIn   => eeIn,
      loadCoefs => inStart,
      valueOut  => eeOut,
      outValid  => open);
  
  evalDenom : chien_search
    generic map (
      alpha    => pAlpha,
      M        => M,
      primPoly => primPolyUsed,
      polyLen  => t+1)
    port map (
      clk       => clk,
      resetn    => resetn,
      coefsIn   => d_el,
      loadCoefs => inStart,
      valueOut  => d_elOut,
      outValid  => cDone);

  correct: process (clk) is
  begin  -- process correct
    if rising_edge(clk) then
      if resetn = '0' then
        outValidSR <= (others => '0');
      else
        outValidSR <= outValidSR(outValidSR'length-2 downto 0) & cDone;
      end if;
      
      if gfEl_t(elOut) = gfZero then
        err <= '1';
      else
        err <= '0';
      end if;
      err_1 <= err;

      numerator   <= gfEl_t(eeOut);
      numerator_1 <= numerator;

      denominator   <= inverse( gfEl_t(d_elOut) );
      denominator_1 <= denominator;

      errorVal_i <= numerator_1 * denominator_1;

      if err_1 = '1' then
        errorVal <= std_logic_vector(errorVal_i);
      else
        errorVal <= std_logic_vector(gfZero);
      end if;
      
    end if;
  end process correct;

  errorStart <= outValidSR(outValidSR'length-1);
  
end architecture rtl;
