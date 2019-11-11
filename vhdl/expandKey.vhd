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
use aes_gf.all;

entity expandKey is
  port (
    clk      : in  std_logic;
    reset    : in  std_logic;
    keyIn    : in  std_logic_vector(31 downto 0);
    rCon     : in  std_logic_vector(7 downto 0);
    start    : in  std_logic;
    keyOut   : out std_logic_vector(31 downto 0);
    rConOut  : out std_logic_vector(7 downto 0);
    outStart : out std_logic);
end entity expandKey;

architecture behavior of expandKey is
  signal subWord    : aesWord_t                     := aesZero;
  signal afterrCon  : aesWord_t                     := aesZero;
  signal savedWords : aesWordArray_t(5 downto 0)    := (others => aesZero);
  signal temp       : aesWord_t                     := aesZero;
  signal control    : std_logic_vector(5 downto 0)  := (others => '0');
  signal keyOut_i   : std_logic_vector(31 downto 0) := (others => '0');
  signal rConOut_i  : std_logic_vector(7 downto 0)  := (others => '0');
  signal rConIn     : std_logic_vector(7 downto 0)  := (others => '0');
  signal rCon1      : std_logic_vector(7 downto 0)  := (others => '0');
  signal rCon2      : std_logic_vector(7 downto 0)  := (others => '0');
  signal rCon3      : std_logic_vector(7 downto 0)  := (others => '0');
  signal outStart_i : std_logic                     := '0';

begin  -- architecture behavior

  mainProc1: process (clk) is
  begin  -- process mainProc1
    if rising_edge(clk) then
      -- Control word
      if reset = '1' then
        control <= (others => '0');

      elsif start = '1' then
        control <= control(4 downto 0) & '1';

      else
        control <= control(4 downto 0) & '0';
      end if;

      -- Save input
      savedWords <= savedWords(savedWords'length-2 downto 0) &
                    to_aesWord(keyIn);

      -- S-Box
      for i in 3 downto 0 loop
        subWord(i) <= sbox( savedWords(0)( (i + 3) mod 4) );
      end loop;  -- i

      -- Save rCon
      if start = '1' then
        rCon1 <= rCon;
      end if;

      if control(3) = '1' then
        rCon2  <= rCon1;
      end if;

      -- Add rCon
      afterrCon(3)          <= subWord(3) + gfEl_t(rCon2);
      afterrCon(2 downto 0) <= subWord(2 downto 0);

      -- Output valid signal
      if reset = '1' then
        outStart_i <= '0';
      else
        outStart_i <= control(5);
      end if;

      if control(5) = '1' then
        -- Generate first word of output key
        temp       <= savedWords(5) xor afterrCon;

        -- Generate new rCon
        rConOut_i  <= std_logic_vector( gfEl_t(rCon2) * 2 );
      else

        -- Generate next word of output key
        temp <= savedWords(5) xor temp;
      end if;

    end if;
  end process mainProc1;

  outStart <= outStart_i;
  rConOut  <= rConOut_i;
  keyOut   <= to_vector(temp);

end architecture behavior;
