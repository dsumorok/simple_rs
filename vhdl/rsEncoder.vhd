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

entity rsEncoder is  
  generic (
    M        : natural;
    n        : natural;
    k        : natural;
    primPoly : natural := 0;
    alpha    : natural := 2);
  port (
    clk      : in  std_logic;
    resetn   : in  std_logic;
    inEl     : in  std_logic_vector(M-1 downto 0);
    inStart  : in  std_logic;
    outEl    : out std_logic_vector(M-1 downto 0);
    outStart : out std_logic);
end entity rsEncoder;

architecture rtl of rsEncoder is
  constant t         : natural := (n-k) / 2;

  type gfArray_t is array(natural range <>) of gfEl_t(M-1 downto 0);

  function mulPoly (
    constant a        : gfArray_t;
    constant b        : gfArray_t;
    constant primPoly : natural)
    return gfArray_t is

    constant aLen   : natural := a'length;
    constant bLen   : natural := b'length;
    constant rLen   : natural := aLen + bLen - 1;
    variable result : gfArray_t(rLen-1 downto 0)
      := (others => (others => '0'));
  begin --  function mulPoly
    for i in 0 to rLen-1 loop
      for j in 0 to aLen-1 loop
        for k in 0 to bLen-1 loop
          if (j+k) = i then
            result(i) := result(i)
                       + mulEl(a(j), b(k), primPoly);
          end if;
        end loop; -- k
      end loop; -- j
    end loop; -- i

    return result;
  end function mulPoly;

  function calc_generator (
    constant M        : natural;
    constant t        : natural;
    constant primPoly : natural;
    constant alpha    : natural)
    return gfArray_t is

    variable result  : gfArray_t(2*t downto 0)
      := (others => (others => '0'));
    variable factor  : gfArray_t(1 downto 0);
    variable tmpEl   : gfEl_t(M-1 downto 0);

    constant gfZero : gfEl_t(M-1 downto 0) := gfEl_t(to_unsigned(0, M));
    constant gfOne  : gfEl_t(M-1 downto 0) := gfEl_t(to_unsigned(1, M));
  begin --  function calc_generator
    factor(0) := gfOne;
    factor(1) := gfOne;
    result(0) := gfOne;

    for i in 0 to 2*t-1 loop
      factor(0) := mulEl(factor(0), alpha, primPoly);

      result(i+1 downto 0) :=
        mulPoly(result(i downto 0), factor, primPoly);
    end loop; --  i

    return result;
  end function calc_generator;

  constant realPrimPoly : natural := calcPrimPoly(primPoly, alpha, M);

  constant generator : gfArray_t(2*t downto 0) :=
    calc_generator(M, t, realPrimPoly, alpha);
  
  signal outVec : gfArray_t(2*t downto 0) :=
    (others => (others => '0'));
  signal inputBuffer : gfArray_t(2*t+1 downto 0) :=
    (others => (others => '0'));
  signal scale      : gfEl_t(M-1 downto 0)             := (others => '0');
  signal count      : integer range 0 to n             := 0;
  signal parityOut  : std_logic                        := '0';
  signal pad        : std_logic                        := '0';
  signal init       : std_logic                        := '1';
  signal outStart_i : std_logic                        := '0';
Begin  -- architecture rtl

  shifter: process (clk) is
  begin  -- process shifter
    if rising_edge(clk) then
      if resetn = '0' or count = n then
        count <= 0;
      elsif inStart = '1' then
        count <= 2;
      elsif count /= 0 then
        count <= count + 1;
      end if;

      if resetn = '0' or count = n then
        init <= '1';
      elsif count = 2*t then
        init <= '0';
      end if;

      if resetn = '0' or count = n then
        pad <= '0';
      elsif count = (n - (2*t)) then
        pad <= '1';
      end if;
      
      if init = '1' then
        scale <= (others => '0');
      else
        scale <= outVec(2*t-1) +
                 mulEl(scale, generator(2*t-1), realPrimPoly);
      end if;
      
      for i in 2*t-1 downto 0 loop
        outVec(i+1) <= outVec(i) +
                       MulEl(scale, generator(i), realPrimPoly);
      end loop;  -- i

      if pad = '1' then
        outVec(0) <= (others => '0');
      else
        outVec(0) <= gfEl_t(inEl);
      end if;

      parityOut <= init;

      inputBuffer((2*t)+1 downto 1)    <= inputBuffer((2*t) downto 0);
      inputBuffer(0) <= gfEl_t(inEl);

      if parityOut = '1' then
        outEl <= std_logic_vector(outVec(2*t));
      else
        outEl <= std_logic_vector(inputBuffer(2*t));
      end if;

      if count = 2*t+1 then
        outStart_i <= '1';
      else
        outStart_i <= '0';
      end if;

      outStart <= outStart_i;
    end if;
  end process shifter;

end architecture rtl;
