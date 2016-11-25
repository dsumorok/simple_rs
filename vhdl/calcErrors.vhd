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
    errorEvaluator : in  gfPoly_t((n-k)/2-1 downto 0, M-1 downto 0);
    errorLocator   : in  gfPoly_t((n-k)/2 downto 0, M-1 downto 0);
    inStart        : in  std_logic;
    errorVal       : out gfEl_t(M-1 downto 0);
    errorStart     : out std_logic);
end entity calcErrors;

architecture rtl of calcErrors is

  component chien_search is
    generic (
      alpha    : natural := 2;
      M        : natural := 8;
      primPoly : natural := 285;
      polyLen  : natural := 1);
    port (
      clk       : in std_logic;
      resetn    : in std_logic;
      coefsIn   : in gfPoly_t(polyLen-1 downto 0, M-1 downto 0);
      loadCoefs : in std_logic;
      valueOut  : out gfEl_t(M-1 downto 0);
      outValid  : out std_logic);
  end component chien_search;

  constant Q : natural := 2**M;
  constant t : natural := (n-k)/2;

  type gfArray_t is array(natural range <>) of gfEl_t(M-1 downto 0);

  function genLogTable
    return gfArray_t is

    constant alphaEl : gfEl_t(M-1 downto 0) := gfEl_t(to_unsigned(alpha, M));
    variable val     : gfEl_t(M-1 downto 0) := alphaEl;
    variable index   : natural;
    variable table   : gfArray_t(0 to Q-1) := (others => (others => '-'));
  begin  -- function genLogTable
    table(1) := (others => '0');

    for i in 1 to Q-1 loop
      index := to_integer(unsigned(val));
      table(index) := gfEl_t(to_unsigned(i, M));
      val := mulEl(val, alphaEl, primPoly);
    end loop;  -- i

    return table;
  end function genLogTable;

  function genPowTable
      return gfArray_t is

    constant alphaEl : gfEl_t(M-1 downto 0) := gfEl_t(to_unsigned(alpha, M));
    variable val     : gfEl_t(M-1 downto 0) := gfEl_t(to_unsigned(1, M));
    variable table   : gfArray_t(0 to Q-1) := (others => (others => '-'));
  begin  -- function genPowTable
    table(1) := (others => '0');

    for i in 0 to Q-2 loop
      table(i) := val;
      val      := mulEl(val, alphaEl, primPoly);
    end loop;  -- i

    return table;
  end function genPowTable;

  function calcInvTable
    return gfArray_t is

    variable ret : gfEl_t(M-1 downto 0);
    variable val : gfEl_t(M-1 downto 0);

    variable invTable : gfArray_t(Q-1 downto 0) := (others => (others => '0'));
    constant logTable : gfArray_t(0 to Q-1) := genLogTable;
    constant powTable : gfArray_t(0 to Q-1) := genPowTable;
    variable intLog : natural;
  begin  -- function calcInvTable

    invTable(0) := (others => '-');
    invTable(1) := gfEl_t( to_unsigned(1, M));

    for i in 2 to Q-1 loop
      intLog := to_integer( unsigned( logTable(i)));
      invTable(i) := powTable((2**M)-1 - intLog);
    end loop;  -- i

    return invTable;
  end function calcInvTable;
  
  constant invTable : gfArray_t(Q-1 downto 0) := calcInvTable;
  constant pAlpha : natural := to_integer(unsigned(invTable(alpha)));
  
  signal elVal : gfEl_t(M-1 downto 0) := (others => '0');
  signal d_el  : gfPoly_t(t downto 0, M-1 downto 0)
    := (others => (others => '0'));

  signal numerator     : gfEl_t(M-1 downto 0) := (others => '0');
  signal numerator_1   : gfEl_t(M-1 downto 0) := (others => '0');
  signal denominator   : gfEl_t(M-1 downto 0) := (others => '0');
  signal denominator_1 : gfEl_t(M-1 downto 0) := (others => '0');
  signal deomInv       : gfEl_t(M-1 downto 0) := (others => '0');
  signal err           : std_logic            := '0';
  signal err_1         : std_logic            := '0';
  signal errorVal_i    : gfEl_t(M-1 downto 0) := (others => '0');
  signal elOut         : gfEl_t(M-1 downto 0) := (others => '0');
  signal eeOut         : gfEl_t(M-1 downto 0) := (others => '0');
  signal d_elOut       : gfEl_t(M-1 downto 0) := (others => '0');
  constant gfZero      : gfEl_t(M-1 downto 0) := (others => '0');
  signal eeIn          : gfPoly_t(t downto 0, M-1 downto 0)
    := (others => (others => '0'));
  signal inStart_1  : std_logic                    := '0';
  signal cDone      : std_logic                    := '0';
  signal outValidSR : std_logic_vector(5 downto 0) := (others => '0');
begin  -- architecture rtl

  setElCoef: process (clk) is
  begin  -- process setElCoef
    if rising_edge(clk) then
      if inStart = '1' then
        elVal <= gfEl_t(to_unsigned(1, M));
      else
        elVal <= mulEl(elVal, invTable(alpha), primPoly);
      end if;
    end if;
  end process setElCoef;

  setdEl: process (errorLocator) is
  begin  -- process setdEl
    for i in 0 to t-1 loop
      for k in 0 to M-1 loop
        if (i mod 2) = 0 then
          d_el(i, k) <= errorLocator(i+1, k);
        end if;
      end loop;  -- k
    end loop;  -- i
  end process setdEl;

  pipeInStart: process (clk) is
  begin  -- process pipeInStart
    if rising_edge(clk) then
      inStart_1 <= inStart;
    end if;
  end process pipeInStart;
  
  set_eeIn: process (errorEvaluator) is
  begin  -- process set_eeIn
    for i in 0 to t-1 loop
      for k in 0 to M-1 loop
        eeIn(i, k) <= errorEvaluator(i, k);
      end loop;  -- k
    end loop;  -- i      
  end process set_eeIn;
  
  evalEl : chien_search
    generic map (
      alpha    => pAlpha,
      M        => M,
      primPoly => primPoly,
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
      primPoly => primPoly,
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
      primPoly => primPoly,
      polyLen  => t+1)
    port map (
      clk       => clk,
      resetn    => resetn,
      coefsIn   => d_el,
      loadCoefs => inStart,
      valueOut  => d_elOut,
      outValid  => cDone);

  correct: process (clk) is
    variable tableIndex : natural;
  begin  -- process correct
    if rising_edge(clk) then
      if resetn = '0' then
        outValidSR <= (others => '0');
      else
        outValidSR <= outValidSR(outValidSR'length-2 downto 0) & cDone;
      end if;
      
      if elOut = gfZero then
        err <= '1';
      else
        err <= '0';
      end if;
      err_1 <= err;

      numerator   <= eeOut;
      numerator_1 <= numerator;

      tableIndex    := to_integer(unsigned(d_elOut));
      denominator   <= invTable(tableIndex);
      denominator_1 <= denominator;

      errorVal_i <= mulEl(numerator_1, denominator_1, primPoly);

      if err_1 = '1' then
        errorVal <= errorVal_i;
      else
        errorVal <= gfZero;
      end if;
      
    end if;
  end process correct;

  errorStart <= outValidSR(outValidSR'length-1);
  
end architecture rtl;
