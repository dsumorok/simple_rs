--  Copyright (c) 2015,2019 Daniel Sumorok
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

package gf is
  generic (
    M           : natural;
    primPolyReq : natural := 0;
    alpha       : natural := 2);

  type gfEl_t is array(M-1 downto 0) of std_logic;
  type gfPoly_t is array(natural range <>) of gfEl_t;

  -- Multiply two field elements
  function mulEl (
    constant a        : gfEl_t;
    constant b        : gfEl_t;
    constant poly     : natural)
    return gfEl_t;

  -- Multiply two field elements
  function "*" (
    constant a        : gfEl_t;
    constant b        : gfEl_t)
    return gfEl_t;

  -- Multiply two field elements
  function "*" (
    constant a        : gfEl_t;
    constant b        : natural)
    return gfEl_t;

  -- Multiply two polynomials
  function "*" (
    constant a        : gfPoly_t;
    constant b        : gfPoly_t)
    return gfPoly_t;

  -- Add two field elements
  function "+" (
    constant a : gfEl_t;
    constant b : gfEl_t)
    return gfEl_t;

  -- Generate a field element
  function gfEl (
    constant value : natural)
    return gfEl_t;

  function calcPrimPoly (
    constant primPolyIn : natural)
    return natural;

  function to_vector (
    constant poly : gfPoly_t)
    return std_logic_vector;

  function to_gfPoly(
    constant vector : std_logic_vector)
    return gfPoly_t;

  function calcInvTable
    return gfPoly_t;

  function genLogTable(
    constant base : gfEl_t)
    return gfPoly_t;

  function genPowTable(
    constant base : gfEl_t)
    return gfPoly_t;

  constant primPolyUsed : natural := calcPrimPoly(primPolyReq);
  constant gfZero       : gfEl_t  := gfEl_t(to_unsigned(0, M));
  constant gfOne        : gfEl_t  := gfEl_t(to_unsigned(1, M));

end package gf;

package body gf is

  -- purpose: Multiply two field elements
  function mulEl (
    constant a        : gfEl_t;
    constant b        : gfEl_t;
    constant poly     : natural)
    return gfEl_t is

    constant MM     : natural := a'length;
    constant prim   : std_logic_vector(MM downto 0)
      := std_logic_vector(to_unsigned(poly, MM+1));
    constant in1    : std_logic_vector(MM-1 downto 0)
      := std_logic_vector(a);
    constant in2    : std_logic_vector(MM-1 downto 0)
      := std_logic_vector(b);
    variable prod1  : std_logic_vector(MM*2-1 downto 0)
      := (others => '0');
    variable result : gfEl_t;
  begin   -- function mulEl
    -- First, multiply polynomials
    for i in 0 to (MM-1) loop
      if in1(i) = '1' then
        prod1(2*MM-1 downto MM) :=
          prod1(2*MM-1 downto MM) xor in2;
      end if;
      prod1 := '0' & prod1(MM*2-1 downto 1);
    end loop;

    -- Next, calculation remainder modulo primitive polynomial
    for i in 0 to (MM-1) loop
      if prod1(2*MM-1) = '1' then
        prod1(2*MM-1 downto MM-1) :=
          prod1(2*MM-1 downto MM-1) xor prim;
      end if;
      prod1 := prod1(MM*2-2 downto 0) & '0';
    end loop;

    result := gfEl_t(prod1(MM*2-1 downto MM));

    return result;
  end function mulEl;

  -- purpose: Multiply two field elements
  function "*" (
    constant a        : gfEl_t;
    constant b        : gfEl_t)
    return gfEl_t is
  begin  -- function "*"
    return mulEl(a, b, primPolyUsed);
  end function "*";

  -- purpose: Multiply two field elements
  function "*" (
    constant a        : gfEl_t;
    constant b        : natural)
    return gfEl_t is
  begin  -- function mulEl
    return a * gfEl(b);
  end function "*";

  -- purpose: Add two field elements
  function "+" (
    constant a : gfEl_t;
    constant b : gfEl_t)
    return gfEl_t is

    variable result : gfEl_t;
  begin --  function "+"

    for i in 0 to M-1 loop
      result(i) := a(i) xor b(i);
    end loop; -- i

    return result;
  end function "+";

  function "*" (
    constant a        : gfPoly_t;
    constant b        : gfPoly_t)
    return gfPoly_t is

    constant aLen   : natural := a'length;
    constant bLen   : natural := b'length;
    constant rLen   : natural := aLen + bLen - 1;
    variable result : gfPoly_t(rLen-1 downto 0)
      := (others => (others => '0'));
  begin --  function mulPoly
    for i in 0 to rLen-1 loop
      for j in 0 to aLen-1 loop
        for k in 0 to bLen-1 loop
          if (j+k) = i then
            result(i) := result(i)
                       + mulEl(a(j), b(k), primPolyUsed);
          end if;
        end loop; -- k
      end loop; -- j
    end loop; -- i

    return result;
  end function "*";

  function gfEl (
    constant value : natural)
    return gfEl_t is
  begin --  function gfEl
    return gfEl_t(to_unsigned(value, M));
  end function gfEl;

  function modGF2Poly (
    constant MM : natural;
    constant a  : natural;
    constant b  : natural)
    return natural is

    variable av : unsigned(MM-1 downto 0);
    variable bv : unsigned(MM-1 downto 0);
    variable c  : natural := 1;
  begin  -- function modGF2Poly
    if b = 0 then
      return 0;
    end if;

    av := to_unsigned(a, MM);
    bv := to_unsigned(b, MM);

    while bv(MM-1) = '0' loop
      c  := c + 1;
      bv := bv(MM-2 downto 0) & '0';
    end loop;

    for i in 0 to c-1 loop
      if av(MM-1) = '1' then
        av := av xor bv;
      end if;
      av := av(MM-2 downto 0) & '0';
    end loop;  -- i

    return to_integer(av) / (2**c);
  end function modGF2Poly;

  -- purpose: Generates a primative polynomial
  function calcPrimPoly (
    constant primPolyIn : natural)
    return natural is

    variable result    : natural;
    variable remainder : natural;
    variable pow       : gfEl_t;
    constant alphaEl   : gfEl_t := gfEl_t(to_unsigned(alpha, M));
    constant loopStart : natural := (2**M)+1;
    constant loopEnd   : natural := 2**(M+1);
    constant loopEnd2  : natural := (2**M)-2;
  begin  -- function calcPrimPoly
    if primPolyIn > 0 then
      return primPolyIn;
    end if;

    for i in loopStart to loopEnd loop
      result := 0;
      for j in 2 to i-1 loop
        if modGF2Poly(M+1, i, j) = 0 then
          exit;
        end if;
        result := j;
      end loop;  -- result

      -- Check that alpha generates the field
      if result = i-1 then
        pow := alphaEl;

        for j in 1 to loopEnd2 loop
          pow := mulEl(pow, alphaEl, i);
          if to_integer(unsigned(pow)) = 1 then
            if j = (2**M)-2 then
              return i;
            end if;
            exit;
          end if;
        end loop;  -- j
      end if;
    end loop;  -- i

    return 0;
  end function calcPrimPoly;

  function to_vector (
    constant poly : gfPoly_t)
    return std_logic_vector is

    constant pLen   : integer := poly'length * M;
    variable vector : std_logic_vector(pLen-1 downto 0);

  begin

    for i in 0 to poly'length-1 loop
      for j in 0 to M-1 loop
        vector(i*M + j) := poly(i)(j);
      end loop;  -- j
    end loop;  -- i

    return vector;
  end function to_vector;

  function to_gfPoly(
    constant vector : std_logic_vector)
    return gfPoly_t is

    constant pLen : integer := vector'length / M;
    variable poly : gfPoly_t(pLen-1 downto 0);

  begin

    for i in 0 to pLen-1 loop
      for j in 0 to M-1 loop
        poly(i)(j) := vector(i*M + j);
      end loop;  -- j
    end loop;  -- i

    return poly;
  end function to_gfPoly;

  function genLogTable(
    constant base : gfEl_t)
    return gfPoly_t is

    constant Q     : natural            := 2**M;
    variable val   : gfEl_t             := base;
    variable table : gfPoly_t(0 to Q-1) := (others => gfZero);
    variable index : natural;
  begin  -- function genLogTable
    table(1) := (others => '0');

    for i in 1 to Q-1 loop
      index        := to_integer(unsigned(val));
      table(index) := gfEl_t(to_unsigned(i, M));
      val          := val * base;
    end loop;  -- i

    return table;
  end function genLogTable;

  function genPowTable(
    constant base : gfEl_t)
      return gfPoly_t is

    constant Q     : natural            := 2**M;
    variable val   : gfEl_t             := gfOne;
    variable table : gfPoly_t(0 to Q-1) := (others => gfZero);
  begin  -- function genPowTable
    table(1) := (others => '0');

    for i in 0 to Q-2 loop
      table(i) := val;
      val      := val * base;
    end loop;  -- i

    return table;
  end function genPowTable;

  function calcInvTable
    return gfPoly_t is

    constant Q        : natural                := 2**M;
    variable invTable : gfPoly_t(Q-1 downto 0) := (others => gfZero);
    constant logTable : gfPoly_t(0 to Q-1)     := genLogTable(gfEl(alpha));
    constant powTable : gfPoly_t               := genPowTable(gfEl(alpha));
    variable intLog   : natural;
  begin  -- function calcInvTable

    invTable(0) := gfZero;
    invTable(1) := gfOne;

    for i in 2 to Q-1 loop
      intLog := to_integer( unsigned( logTable(i)));
      invTable(i) := powTable((2**M)-1 - intLog);
    end loop;  -- i

    return invTable;
  end function calcInvTable;

end package body gf;
