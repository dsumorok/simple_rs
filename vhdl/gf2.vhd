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

package gf2 is
  generic (
    M           : natural;
    primPolyReq : natural := 0;
    alpha       : natural := 2);

  type gfEl_t is array(natural range <>) of std_logic;
  type gfList_t is array(natural range <>) of gfEl_t;
  type gfPoly_t is array(natural range <>, natural range <>) of std_logic;
  type gfArray_t is array(natural range <>) of gfEl_t(M-1 downto 0);

  type newType_t is record
    M        : natural;
    primPoly : natural;
    alpha    : natural;
    val      : gfEl_t;
  end record newType_t;
  
  function genPowTableX
    return gfList_t;

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

  function "*" (
    constant a        : gfEl_t;
    constant b        : natural)
    return gfEl_t;
  
  function "*" (
    constant a        : gfArray_t;
    constant b        : gfArray_t)
    return gfArray_t;
  
  -- Add two field elements
  function "+" (
    constant a : gfEl_t;
    constant b : gfEl_t)
    return gfEl_t;

  -- Generate a field element
  function gfEl (
    constant value : natural;
    constant MM    : natural)
    return gfEl_t;

  function gfEl (
    constant value : natural)
    return gfEl_t;

  function setCoef (
    constant poly  : gfPoly_t;
    constant el    : gfEl_t;
    constant index : natural)
    return gfPoly_t;

  function setCoefs (
    constant poly1 : gfPoly_t;
    constant poly2 : gfPoly_t)
    return gfPoly_t;

  function getCoefs (
    constant poly   : gfPoly_t;
    constant cRange : natural)
    return gfPoly_t;

  function getCoef (
    constant poly  : gfPoly_t;
    constant index : natural)
    return gfEl_t;

  function calcPrimPoly (
    constant primPolyIn : natural;
    constant alphaIn    : natural;
    constant MM         : natural)
    return natural;

  constant primPolyUsed : natural              := calcPrimPoly(primPolyReq, alpha, M);
  constant gfZero       : gfEl_t(M-1 downto 0) := gfEl_t(to_unsigned(0, M));
  constant gfOne        : gfEl_t(M-1 downto 0) := gfEl_t(to_unsigned(1, M));

end package gf2;

package body gf2 is

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
    variable result : gfEl_t(MM-1 downto 0);
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
    return a * gfEl(b, a'length);
  end function "*";

  -- purpose: Add two field elements
  function "+" (
    constant a : gfEl_t;
    constant b : gfEl_t)
    return gfEl_t is

    constant MM     : natural := a'length;
    variable result : gfEl_t(MM-1 downto 0);
  begin --  function "+"

    for i in 0 to M-1 loop
      result(i) := a(i) xor b(i);
    end loop; -- i

    return result;
  end function "+";

  function "*" (
    constant a        : gfArray_t;
    constant b        : gfArray_t)
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
                       + mulEl(a(j), b(k), primPolyUsed);
          end if;
        end loop; -- k
      end loop; -- j
    end loop; -- i

    return result;
  end function "*";
  
  -- purpose: Generate a field element
  function gfEl (
    constant value : natural;
    constant MM    : natural)
    return gfEl_t is
  begin --  function gfEl
    return gfEl_t(to_unsigned(value, MM));
  end function gfEl;

  function gfEl (
    constant value : natural)
    return gfEl_t is
  begin --  function gfEl
    return gfEl_t(to_unsigned(value, M));
  end function gfEl;

  -- purpose: Retrieves a set of coefficients
  function getCoefs (
    constant poly   : gfPoly_t;
    constant cRange : natural)
    return gfPoly_t is

    constant MM     : natural := poly'length(2);
    variable result : gfPoly_t(cRange-1 downto 0, MM-1 downto 0);
  begin  -- function getCoefs

    for i in 0 to cRange-1 loop
      for j in 0 to MM-1 loop
        result(i,j) := poly(i,j);
      end loop;  -- j
    end loop;  -- i

    return result;
  end function getCoefs;

  -- purpose: Copies poly1 to poly2
  function setCoefs (
    constant poly1 : gfPoly_t;
    constant poly2 : gfPoly_t)
    return gfPoly_t is

    constant MM     : natural := poly2'length(2);
    constant pLen   : natural := poly2'length(1);
    constant M1     : natural := poly1'length(2);
    constant pLen1  : natural := poly1'length(1);
    variable result : gfPoly_t(pLen-1 downto 0, MM-1 downto 0)
      := (others => (others => '0'));
  begin  -- function setCoefs
    for i in 0 to pLen1-1 loop
      for j in 0 to M1-1 loop
        result(i,j) := poly1(i,j);
      end loop;  -- j
    end loop;  -- i

    return result;
  end function setCoefs;

  -- purpose: Sets a coefficent of a polynomial
  function setCoef (
    constant poly  : gfPoly_t;
    constant el    : gfEl_t;
    constant index : natural)
    return gfPoly_t is

    constant pLen : natural := poly'length(1);
    constant MM    : natural := poly'length(2);
    variable result : gfPoly_t(pLen-1 downto 0, MM-1 downto 0) := poly;
  begin  -- function setCoef
    for i in MM-1 downto 0 loop
      result(index, i) := el(i);
    end loop;  -- i

    return result;
  end function setCoef;

  -- purpose: Retreive a coefficent
  function getCoef (
    constant poly  : gfPoly_t;
    constant index : natural)
    return gfEl_t is

    constant pLen   : natural := poly'length(1);
    constant MM     : natural := poly'length(2);
    variable result : gfEl_t(MM-1 downto 0);
  begin  -- function getCoef
    for i in MM-1 downto 0 loop
      result(i) := poly(index, i);
    end loop;  -- i

    return result;
  end function getCoef;

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
    constant primPolyIn : natural;
    constant alphaIn    : natural;
    constant MM         : natural)
    return natural is

    variable result    : natural;
    variable remainder : natural;
    variable pow       : gfEl_t(MM-1 downto 0);
    constant alphaEl   : gfEl_t := gfEl_t(to_unsigned(alphaIn, MM));
    constant loopStart : natural := (2**MM)+1;
    constant loopEnd   : natural := 2**(MM+1);
    constant loopEnd2  : natural := (2**MM)-2;
  begin  -- function calcPrimPoly
    if primPolyIn > 0 then
      return primPolyIn;
    end if;

    for i in loopStart to loopEnd loop
      result := 0;
      for j in 2 to i-1 loop
        if modGF2Poly(MM+1, i, j) = 0 then
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
            if j = (2**MM)-2 then
              return i;
            end if;
            exit;
          end if;
        end loop;  -- j
      end if;
    end loop;  -- i

    return 0;
  end function calcPrimPoly;

  function genPowTableX
      return gfList_t is

    constant Q       : natural              := 2**M;
    constant alphaEl : gfEl_t(M-1 downto 0) := gfEl_t(to_unsigned(alpha, M));
    variable val     : gfEl_t(M-1 downto 0) := gfEl_t(to_unsigned(1, M));
    variable table   : gfList_t(0 to Q-1)(M-1 downto 0) := (others => (others => '0'));
  begin  -- function genPowTable
    table(1) := (others => '0');

    for i in 0 to Q-2 loop
      table(i) := val;
      val      := val * alphaEl;
    end loop;  -- i

    return table;
  end function genPowTableX;

end package body gf2;
