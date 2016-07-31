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
use work.rsUtil.all;

entity chien_search is
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
end entity chien_search;

architecture rtl of chien_search is

  type gfArray_t is array(natural range <>) of gfEl_t(M-1 downto 0);

  function poly2array (
    constant p : gfPoly_t)
    return gfArray_t is

    constant pLen : natural := p'length(1);

    variable result : gfArray_t(pLen-1 downto 0);
  begin  -- function poly2array
    for i in 0 to pLen-1 loop
      for j in 0 to M-1 loop
        result(i)(j) := p(i,j);
      end loop;  -- j
    end loop;  -- i

    return result;
  end function poly2array;

  function genAFactors
    return gfArray_t is

    variable result : gfArray_t(polyLen-1 downto 0);
    variable factor : gfEl_t(M downto 1) := gfEl_t(to_unsigned(1, M));
  begin  -- function genAFactors
    for i in 0 to polyLen-1 loop
      result(i) := factor;
      factor := mulEl(factor, alpha, primPoly);
    end loop;  -- i

    return result;
  end function genAFactors;


  constant factors : gfArray_t(polyLen-1 downto 0)
    := genAFactors;
  signal coefs     : gfArray_t(polyLen-1 downto 0)
    := (others => (others => '0'));

  constant numStages   : natural := calcNumStages(addsPerSum, polyLen);
  constant stageLength : natural := addsPerSum ** numStages;

  type adderTree_t is array(numStages downto 0)
    of gfArray_t(stageLength-1 downto 0);

  signal adderTree  : adderTree_t
    := (others => (others => (others => '0')));
  signal count      : natural range 0 to numStages := 0;
  signal outValid_i : std_logic                    := '0';
begin  -- architecture rtl

  mainLogic: process (clk) is
    variable sumCount : natural;
    variable sum : gfEl_t(M-1 downto 0);
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
        coefs <= poly2array(coefsIn);
      else
        for i in 0 to polyLen-1 loop
          coefs(i)        <= mulEl(coefs(i), factors(i), primPoly);
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

      valueOut <= adderTree(0)(0);
    end if;
  end process mainLogic;

  outValid <= outValid_i;

end architecture rtl;
