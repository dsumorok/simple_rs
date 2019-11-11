--  Copyright (c) 2019 Daniel Sumorok
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

package aes_pkg is

  package aes_gf is new work.gf generic map (
    M           => 8,
    primPolyReq => 283,
    alpha       => 3);
  use aes_gf.all;

  type aesWord_t is array(3 downto 0) of gfEl_t;
  type aesWordArray_t is array(natural range <>) of aesWord_t;                      

  function to_aesWord (
    constant vecIn : std_logic_vector(31 downto 0))
    return aesWord_t;
                      
  function to_vector (
    constant wordIn : aesWord_t)
    return std_logic_vector;
                      
  function transformation
    return gfPoly_t;

  function genSboxTable
      return gfPoly_t;

  function sbox (
    constant el : gfEl_t)
      return gfEl_t;

  function "xor" (
    constant in1 : aesWord_t;
    constant in2 : aesWord_t)
      return aesWord_t;

  constant aesZero : aesWord_t := (others => gfZero);
                      
  constant xformTable : gfPoly_t := transformation;
  constant sboxTable  : gfPoly_t := genSboxTable;
end package aes_pkg;

package body aes_pkg is

  function to_aesWord (
    constant vecIn : std_logic_vector(31 downto 0))
    return aesWord_t is
  begin
    return aesWord_t(to_gfPoly(vecIn));
  end function to_aesWord;
    
  function to_vector (
    constant wordIn : aesWord_t)
    return std_logic_vector is
  begin
    return to_vector(gfPoly_t(wordIn));
  end function to_vector;

  -- purpose: generate affine-transformation table for S-box
  function transformation
    return gfPoly_t is

    variable table : gfPoly_t(255 downto 0);
    variable idx   : gfEl_t;
    variable col   : gfEl_t;
    
  begin  -- function transformation
    for i in 0 to 255 loop
      idx      := gfEl_t(to_unsigned(i, 8));
      col      := X"1f";
      table(i) := X"00";

      for j in 0 to 7 loop
        if idx(j) = '1' then
          table(i) := table(i) + col;
        end if;

        col := col(6 downto 0) & col(7);
      end loop;  -- j

      table(i) := table(i) + X"63";
    end loop;  -- i

    return table;
  end function transformation;

  -- purpose: generate S-box transformation
  function genSboxTable
    return gfPoly_t is

    variable table        : gfPoly_t(255 downto 0);
  begin  -- function genSboxTable

    for i in 0 to 255 loop
      table(i) := xformTable( inverse(i) );
    end loop;  -- i

    return table;
  end function genSboxTable;

  -- purpose: Perfoms S-box transformation on an element
  function sbox (
    constant el : gfEl_t)
    return gfEl_t is
  begin  -- function sbox

    return sboxTable( to_integer( unsigned( el ) ) );
  end function sbox;

  function "xor" (
    constant in1 : aesWord_t;
    constant in2 : aesWord_t)
    return aesWord_t is
    variable result : aesWord_t;
  begin
    for i in 0 to 3 loop
      result(i) := in1(i) + in2(i);
    end loop;

    return result;
  end function "xor";

  
end package body aes_pkg;
