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
use work.rsUtil.all;

entity chien_search is
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
end entity chien_search;

architecture rtl of chien_search is
  package gf_pack is new work.gf generic map (
      M           => M,
      primPolyReq => primPoly,
      alpha       => alpha);
  use gf_pack.all;

  function genAFactors
    return gfPoly_t is

    variable result : gfPoly_t(polyLen-1 downto 0);
    variable factor : gfEl_t := gfOne;
  begin  -- function genAFactors
    for i in 0 to polyLen-1 loop
      result(i) := factor;
      factor    := factor * alpha;
    end loop;  -- i

    return result;
  end function genAFactors;

  constant factors : gfPoly_t(polyLen-1 downto 0) := genAFactors;
  signal coefs     : gfPoly_t(polyLen-1 downto 0) := (others => gfZero);

  constant numStages   : natural := calcNumStages(addsPerSum, polyLen);
  constant stageLength : natural := addsPerSum ** numStages;

  type adderTree_t is array(numStages downto 0)
    of gfPoly_t(stageLength-1 downto 0);

  signal adderTree  : adderTree_t := (others => (others => gfZero));

  signal count      : natural range 0 to numStages   := 0;
  signal outValid_i : std_logic                      := '0';
  signal valueOut_i : std_logic_vector(M-1 downto 0) := (others => '0');
begin  -- architecture rtl

  mainLogic: process (clk) is
    variable sumCount : natural;
    variable sum      : gfEl_t;
  begin  -- process mainLogic
    if rising_edge(clk) then
      if resetn = '0' then
        count <= 0;
      elsif loadCoefs = '1' then
        count <= 1;
      elsif count = numStages then
        count <= 0;
      elsif count /= 0 then
        count <= count + 1;
      end if;

      if count = numStages and resetn = '1' then
        outValid_i <= '1';
      else
        outValid_i <= '0';
      end if;
      
      if loadCoefs = '1' then
        coefs <= to_gfPoly(coefsIn);
      else
        for i in 0 to polyLen-1 loop
          coefs(i) <= coefs(i) * factors(i);
        end loop;  -- i
      end if;

      for i in 0 to polyLen-1 loop
        adderTree(numStages)(i) <= coefs(i);        
      end loop;  -- i

      for stage in numStages-1 downto 0 loop
        sumCount := addsPerSum ** stage;
        for sumNumber in 0 to sumCount-1 loop
          sum := (others => '0');
          for sumIndex in 0 to addsPerSum-1 loop
            sum := sum + adderTree(stage+1)(sumIndex + sumNumber*addsPerSum);
          end loop;  -- sumIndex

          adderTree(stage)(sumNumber) <= sum;
        end loop;  -- sumNumber
      end loop;  -- stage

      valueOut_i <= std_logic_vector(adderTree(0)(0));
    end if;
  end process mainLogic;

  outValid <= outValid_i;
  valueOut <= valueOut_i;

end architecture rtl;
