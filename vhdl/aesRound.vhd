--  Copyright (c) 2019, Daniel Sumorok
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
use work.aes_pkg.all;
use work.aes_pkg.aes_gf.all;

entity aesRound is
  port (
    clk        : in  std_logic;
    reset      : in  std_logic;
    keyIn      : in  std_logic_vector(31 downto 0);
    ptIn       : in  std_logic_vector(31 downto 0);
    start      : in  std_logic;
    lastRound  : in  std_logic;
    ctOut      : out std_logic_vector(31 downto 0);
    outStart   : out std_logic);
end entity aesRound;

architecture behavior of aesRound is
  signal subBytes     : aesWordArray_t(3 downto 0)   := (others => aesZero);
  signal shiftRows    : aesWordArray_t(3 downto 0)   := (others => aesZero);
  signal mixColumns   : aesWord_t                    := aesZero;
  signal inWord       : aesWord_t                    := aesZero;
  signal control      : std_logic_vector(5 downto 0) := (others => '0');
  signal outStart_i   : std_logic                    := '0';
  signal lastRound_i  : std_logic_vector(3 downto 0) := (others => '0');
  signal test         : std_logic_vector(31 downto 0);

  function dotProduct (
    constant wordIn : aesWord_t;
    constant pIn    : std_logic_vector(31 downto 0))
    return gfEl_t is

    variable word2 : aesWord_t := to_aesWord(pIn);
    variable prod  : gfEl_t    := gfZero;
  begin  -- function dotProduct

    for i in 3 downto 0 loop
      prod := prod + (wordIn(i) * word2(i));
    end loop;  -- i
    
    return prod;
  end function dotProduct;
  
begin  -- architecture behavior
  mainProc: process (clk) is
    variable shiftWord : aesWord_t;
  begin  -- process mainProc
    if rising_edge(clk) then
      if start = '1' then
        lastRound_i(0) <= lastRound;
      end if;

      lastRound_i(3 downto 1) <= lastRound_i(2 downto 0);
      
      if reset = '1' then
        control <= (others => '0');
      else
        control <= control(4 downto 0) & start;
      end if;

      -- AddRoundKey step
      inWord <= to_aesWord(ptIn xor keyIn);

      -- SubBytes step
      for i in 3 downto 0 loop
        subBytes(0)(i) <= sbox(inWord(i));
      end loop;  -- i
      subBytes(3 downto 1) <= subBytes(2 downto 0);

      -- ShiftRows step
      if control(4) = '1' then
        for i in 3 downto 0 loop
          for j in 3 downto 0 loop
            shiftRows((i+3-j) mod 4)(j) <= subBytes(i)(j);
          end loop;  -- j
        end loop;  -- i
      else
        shiftRows <= shiftRows(2 downto 0) & aesZero;
      end if;

      -- MixColumns step
      if lastRound_i(3) = '1' then
        mixColumns <= shiftRows(3);
      else
        mixColumns(3) <= dotProduct(shiftRows(3), X"02030101");
        mixColumns(2) <= dotProduct(shiftRows(3), X"01020301");
        mixColumns(1) <= dotProduct(shiftRows(3), X"01010203");
        mixColumns(0) <= dotProduct(shiftRows(3), X"03010102");
      end if;

      if reset = '1' then
        outStart_i <= '0';
      else
        outStart_i <= control(5);
      end if;
      
    end if;    
  end process mainProc;

  outStart <= outStart_i;
  ctOut    <= to_vector(mixColumns);
  
end architecture behavior;
