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
    inEl           : in  std_logic_vector;
    inStart        : in  std_logic;
    syndromes      : out std_logic_vector;
    syndromesStart : out std_logic);
End entity calcSyndromes;

architecture rtl of calcSyndromes is
  package gf_pack is new work.gf generic map (
      M           => M,
      primPolyReq => primPoly,
      alpha       => alpha);
  use gf_pack.all;

  constant t         : natural := (n-k) / 2;

  function calcEvalPoints
    return gfPoly_t is

    constant x      : gfEl_t                   := gfEl(alpha);
    variable val    : gfEl_t                   := gfZero;
    variable retVal : gfPoly_t(2*t-1 downto 0) := (others => gfZero);

  begin  -- function calcEvalPoints
    val := x;

    for i in 0 to 2*t-1 loop
      retVal(i) := val;
      val       := val * x;
    end loop;  -- i

    return retVal;
  end function calcEvalPoints;

  constant evalPoints     : gfPoly_t(2*t-1 downto 0)         := calcEvalPoints;
  signal startInSR        : std_logic_vector(2*t+n downto 0) := (others => '0');
  signal state            : gfPoly_t(2*t-1 downto 0)         := (others => gfZero);
  signal syndromes_i      : gfPoly_t(2*t-1 downto 0)         := (others => gfZero);
  signal inputArray       : gfPoly_t(2*t-1 downto 0)         := (others => gfZero);
  signal syndromesStart_i : std_logic                        := '0';
begin  -- architecture rtl

  setShift: process (clk) is
    variable prevResult : gfEl_t;
  begin  -- process setShift

    if rising_edge(clk) then
      if resetn = '0' then
        startInSR <= (others => '0');
      else
        startInSR  <= startInSR(2*t+n-1 downto 0) & inStart;
      end if;
      inputArray <= inputArray(2*t-2 downto 0) & gfEl_t(inEl);

      for i in 0 to 2*t-1 loop
        if startInSR(i) = '0' then
          prevResult := state(i);
        else
          prevResult := gfZero;
        end if;

        state(i) <= (evalPoints(i) * prevResult) +
                    inputArray(i);

        if startInSR(i+n) = '1' then
          syndromes_i(i) <= state(i);
        end if;

      end loop;  -- i

      syndromesStart_i <= startInSR(2*t+n);
    end if;
  end process setShift;

  syndromesStart <= syndromesStart_i;
  syndromes      <= to_vector(syndromes_i);

end architecture rtl;
