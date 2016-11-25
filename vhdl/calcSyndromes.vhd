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

entity calcSyndromes is
  generic (
    M        : natural;
    n        : natural;
    k        : natural;
    primPoly : natural;
    alpha    : natural);
  port (
    clk            : in  std_logic;
    resetn         : in  std_logic;
    inEl           : in  gfEl_t(M-1 downto 0);
    inStart        : in  std_logic;
    syndromes      : out gfPoly_t(2*((n-k)/2)-1 downto 0, M-1 downto 0);
    syndromesStart : out std_logic);
End entity calcSyndromes;

architecture rtl of calcSyndromes is
  
  constant t         : natural := (n-k) / 2;

  type gfArray_t is array(natural range <>) of gfEl_t(M-1 downto 0);

  function calcEvalPoints
    return gfArray_t is

    constant t      : natural
      := (n-k)/2;
    constant x      : gfEl_t(M-1 downto 0)
      := gfEl(alpha, M);
    variable retVal : gfArray_t(2*t-1 downto 0)
      := (others => (others => '0'));
    variable val    : gfEl_t(M-1 downto 0)
      := (others => '0');
  begin  -- function calcEvalPoints
    val := x;

    for i in 0 to 2*t-1 loop
      retVal(i) := val;
      val       := mulEl(val, x, primPoly);
    end loop;  -- i

    return retVal;
  end function calcEvalPoints;
  
  constant evalPoints : gfArray_t(2*t-1 downto 0)
    := calcEvalPoints;
  signal startInSR  : std_logic_vector(2*t+n downto 0)
    := (others => '0');
  signal outputs1   : gfArray_t(2*t-1 downto 0)
    := (others => (others => '0'));
  signal inputArray : gfArray_t(2*t downto 0)
    := (others => (others => '0'));
  signal syndromesStart_i : std_logic := '0';
begin  -- architecture rtl

  setShift: process (clk) is
    variable prevResult : gfEl_t(M-1 downto 0);
  begin  -- process setShift

    if rising_edge(clk) then
      if resetn = '0' then
        startInSR <= (others => '0');
      else
        startInSR  <= startInSR(2*t+n-1 downto 0) & inStart;
      end if;
      inputArray <= inputArray(2*t-1 downto 0) & inEl;

      for i in 0 to 2*t-1 loop
        if startInSR(i) = '0' then
          prevResult := outputs1(i);
        else
          prevResult := (others => '0');
        end if;

        outputs1(i) <= mulEl(evalPoints(i), prevResult, primPoly) +
                       inputArray(i);

        if startInSR(i+n) = '1' then
          for j in 0 to M-1 loop
            syndromes(i,j) <= outputs1(i)(j);
          end loop;  -- j
        end if;
        
      end loop;  -- i

      syndromesStart_i <= startInSR(2*t+n);
    end if;


  end process setShift;

  syndromesStart <= syndromesStart_i;

end architecture rtl;
