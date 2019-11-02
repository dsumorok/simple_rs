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

entity mea is
  generic (
    M           : natural;
    n           : natural;
    k           : natural;
    primPoly    : natural;
    alpha       : natural;
    extraCycles : natural);
  port (
    clk            : in  std_logic;
    resetn         : in  std_logic;
    syndromesIn    : in  std_logic_vector;
    syndromesStart : in  std_logic;
    errorLocator   : out std_logic_vector;
    errorEvaluator : out std_logic_vector;
    outStart       : out std_logic);
end entity mea;

architecture rtl of mea is
  package gf_pack is new work.gf generic map (
      M           => M,
      primPolyReq => primPoly,
      alpha       => alpha);
  use gf_pack.all;

  type configRecord is record
    t          : natural;
    pipeline   : boolean;
    period     : natural;
    workPeriod : natural;
    workSize   : natural;
  end record configRecord;

  function genConfiguration (
    constant latency : natural)
    return configRecord is

    variable result     : configRecord;
    variable t          : natural;
    variable pipeline   : boolean;
    variable period     : natural;
    variable workPeriod : natural;
    variable workSize   : natural;
  begin
    t      := (n-k) / 2;
    --                 latency = 2*t*period + 1
    -- ==>       (latency - 1) = 2*t*period
    -- ==> (latency-1) / (2*t) = period
    period := (latency-1) / (2*t);

    if period = 1 then
      workPeriod := 1;
      workSize   := 2*t;
      pipeline   := false;
    else
      -- if period > 1, we use
      workPeriod := period-1;
      workSize   := (2*t+(workPeriod-1)) / workPeriod;
      pipeline   := true;
    end if;

    result.t          := t;
    result.pipeline   := pipeline;
    result.period     := period;
    result.workPeriod := workPeriod;
    result.workSize   := workSize;

    return result;
  end function genConfiguration;

  constant config     : configRecord := genConfiguration(n+extraCycles);
  constant t          : natural      := config.t;
  constant period     : natural      := config.period;
  constant workPeriod : natural      := config.workPeriod;
  constant workSize   : natural      := config.workSize;
  constant pipeline   : boolean      := config.pipeline;

  -----------------------------------------------------------------------------
  -- Performs a circular right shift
  -----------------------------------------------------------------------------
  function crshift (
    constant x     : gfPoly_t;
    constant shift : natural)
    return gfPoly_t is

    constant xlen       : natural := x'length;
    variable result     : gfPoly_t(xlen-1 downto 0);
    constant shiftStart : natural := shift mod xlen;
  begin  -- function cshift
    if shiftStart = 0 then
      result := x;
    else
      result := x(shiftStart-1 downto 0) & x(xlen-1 downto shiftStart);
    end if;

    return result;
  end function crshift;

  -----------------------------------------------------------------------------
  -- Performs a circular shift
  -----------------------------------------------------------------------------
  function finalShift (
    constant x : gfPoly_t)
    return gfPoly_t is

    variable sr         : gfPoly_t(x'length-1 downto 0);
    variable extraShift : natural;
    variable shiftAmt   : natural;
  begin  -- function finalShift
    extraShift := (workPeriod * workSize) mod x'length ;
    shiftAmt := x'length - extraShift;
    sr := crshift(x, shiftAmt);

    return sr(x'length-1 downto 0);
  end function finalShift;

  -----------------------------------------------------------------------------
  -- State used to solve the key equation
  -----------------------------------------------------------------------------
  type meaState_t is record
    X             : gfPoly_t(2*t-1 downto 0);
    V             : gfPoly_t(2*t-1 downto 0);
    VTop          : gfEl_t;
    sigma         : integer range -2*t to 2*t;
    sigmaNegative : std_logic;
  end record meaState_t;

  type wuState_t is record
    W             : gfPoly_t(2*t-1 downto 0);
    U             : gfPoly_t(2*t-1 downto 0);
    UTop          : gfEl_t;
  end record wuState_t;

  -----------------------------------------------------------------------------
  -- Initialize state with a new set of syndromes
  -----------------------------------------------------------------------------

  function zeroWUState
    return wuState_t is

    variable result : wuState_t;

  begin  -- function zeroWUState
    result.W             := (others => gfZero);
    result.U             := (others => gfZero);
    result.UTop          := gfZero;

    return result;
  end function zeroWUState;

  function zeroMEAState
    return meaState_t is

    variable result : meaState_t;

  begin  -- function zeroMEAState
    result.X             := (others => gfZero);
    result.V             := (others => gfZero);
    result.VTop          := gfZero;
    result.sigma         := 0;
    result.sigmaNegative := '0';

    return result;
  end function zeroMEAState;

  function initState (
    constant S : gfPoly_t(2*t-1 downto 0))
    return meaState_t is

    variable result : meaState_t;

  begin  -- function initState
    result.V             := S(2*t-1 downto 0);
    result.VTop          := S(2*t-1);
    result.X             := (others => gfZero);
    result.X(0)          := gfOne;
    result.sigma         := -1;
    result.sigmaNegative := '1';

    result.V := crshift(result.V, workSize*workPeriod+1);
    result.X := crshift(result.X, workSize*workPeriod+1);

    return result;
  end function initState;

  function initWUState (
    constant S : gfPoly_t(2*t-1 downto 0))
    return wuState_t is

    variable result : wuState_t;

  begin  -- function initWUState
    result.U             := (others => (others => '0'));
    result.U(2*t-1)      := gfOne;
    result.UTop          := gfOne;
    result.W             := (others => (others => '0'));

    result.U := crshift(result.U, workSize*workPeriod);
    result.W := crshift(result.W, workSize*workPeriod);

    return result;
  end function initWUState;

  -----------------------------------------------------------------------------
  -- Main work function for solving the key equation
  -----------------------------------------------------------------------------
  function iterateNextState (
    constant mst : meaState_t;
    constant mns : meaState_t;
    constant wus : wuState_t)
    return meaState_t is

    variable result : meaState_t;
  begin  -- function iterateNextState
      result.V := crshift(mns.V, workSize);
      result.X := crshift(mns.X, workSize);

      for i in workSize-1 downto 0 loop
        result.V(2*t-(workSize-1)+i-1) := (wus.Utop * mst.V(i)) +
                                          (mst.Vtop * wus.U(i));

        result.X(2*t-(workSize-1)+i-1) := (wus.Utop * mst.X(i)) +
                                          (mst.Vtop * wus.W(i));
      end loop;  -- i

      result.sigma := mst.sigma;
      if mst.sigma < 0 then
        result.sigmaNegative := '1';
      else
        result.sigmaNegative := '0';
      end if;

      result.Vtop := (others => '0');

      return result;
  end function iterateNextState;

  -----------------------------------------------------------------------------
  -- This is a step in solving the key equation
  -----------------------------------------------------------------------------
  function doSwap (
    constant mst : meaState_t;
    constant mns : meaState_t;
    constant wus : wuState_t)
    return meaState_t is

    variable result : meaState_t;
    variable xx     : gfPoly_t(2*t-1 downto 0);
    variable vv     : gfPoly_t(2*t-1 downto 0);
    variable uu     : gfPoly_t(2*t-1 downto 0);
    variable ww     : gfPoly_t(2*t-1 downto 0);
  begin  -- function doSwap

    xx    := finalShift(mns.X);
    xx    := crshift(xx, 2*t-1);

    vv    := finalShift(mns.V);
    vv    := crshift(vv, 2*t-1);

    uu    := finalShift(wus.U);
    ww    := finalShift(wus.W);

    if vv(n-k-1) /= gfZero and (mns.sigmaNegative = '1') then
      result.sigma := -mns.sigma - 1;

      result.V    := uu;
      result.Vtop := uu(2*t-1);
      result.X    := ww;
    else
      if mns.sigma /= -2*t then
        result.sigma := mns.sigma - 1;
      end if;

      result.V    := vv;
      result.Vtop := vv(2*t-1);
      result.X    := xx;
    end if;

    result.sigmaNegative := '0';

    return result;
  end function doSwap;

  function doSwap (
    constant mst : meaState_t;
    constant mns : meaState_t;
    constant wus : wuState_t)
    return wuState_t is

    variable result : wuState_t;
    variable xx     : gfPoly_t(2*t-1 downto 0);
    variable vv     : gfPoly_t(2*t-1 downto 0);
    variable uu     : gfPoly_t(2*t-1 downto 0);
    variable ww     : gfPoly_t(2*t-1 downto 0);
  begin  -- function doSwap
    xx    := finalShift(mns.X);
    xx    := crshift(xx, 2*t-1);

    vv    := finalShift(mns.V);
    vv    := crshift(vv, 2*t-1);

    uu    := finalShift(wus.U);
    ww    := finalShift(wus.W);

    if vv(n-k-1) /= gfZero and (mns.sigmaNegative = '1') then
      result.U    := vv;
      result.Utop := vv(2*t-1);
      result.W    := xx;
    else
      result.U    := uu;
      result.Utop := uu(2*t-1);
      result.W    := ww;
    end if;

    return result;
  end function doSwap;

  signal SIn          : gfPoly_t(2*t-1 downto 0);
  signal count        : natural range 0 to period*2*t        := 0;
  signal done1        : std_logic                            := '0';
  signal el_i         : gfPoly_t(t downto 0)                 := (others => gfZero);
  signal ee_i         : gfPoly_t(t-1 downto 0)               := (others => gfZero);
  signal el_out       : std_logic_vector((t+1)*M-1 downto 0) := (others => '0');
  signal ee_out       : std_logic_vector(t*M-1 downto 0)     := (others => '0');
  signal outValid_i   : std_logic                            := '0';
  signal shiftRight   : std_logic                            := '0';
  signal shiftCount   : natural range 0 to t+1               := 0;
  signal currentState : meaState_t                           := zeroMEAState;
  signal nextState    : meaState_t                           := zeroMEAState;
  signal wuState      : wuState_t                            := zeroWUState;
  constant ssInitVal  : std_logic_vector :=
    std_logic_vector(to_unsigned(2**(period-1), period));

  signal stateShift   : std_logic_vector(period-1 downto 0)
    := ssInitVal;
begin  -- architecture rtl
  errorLocator   <= el_out;
  errorEvaluator <= ee_out;

  SIn <= to_gfPoly(syndromesIn);

  stage1Control: process (clk) is
  begin  -- process stage1Control
    if rising_edge(clk) then
      -- count keeps track of whether we are processing data
      if resetn = '0' then
        count <= 0;
      elsif syndromesStart = '1' then
        count <= 1;
      elsif count = period*2*t then
        count <= 0;
      elsif count /= 0 then
        count <= count + 1;
      end if;

      -- done1 goes high when this stage is complete
      if count = period*2*t and resetn = '1' then
        done1 <= '1';
      else
        done1 <= '0';
      end if;
    end if;
  end process stage1Control;

  -- This stage is where most of the work is done.  It takes (2*t*period + 1)
  -- cycles to process a set of syndromes and generate what are almost the
  -- error locator and error evaluator polynomials
  stage1: process (clk) is
    variable nextWuState : wuState_t;
  begin  -- process stage1
    if rising_edge(clk) then
      nextWuState := doSwap(nextState, nextState, wuState);

      if syndromesStart = '1' then
        nextState  <= initState(SIn);
        stateShift <= ssInitVal;
      else
        if pipeline then
          nextState  <= iterateNextState(currentState, nextState, wuState);
          stateShift <= stateShift(period-2 downto 0) & stateShift(period-1);
        else
          nextState  <= iterateNextState(doSwap(nextState, nextState, wuState),
                                         nextState, nextWuState);
        end if;
      end if;

      if syndromesStart = '1' then
        wuState <= initWUState(SIn);
      else
        if pipeline then
          if stateShift(period-1) = '1' then
            wuState <= doSwap(currentState, nextState, wuState);
          else
            wuState.U <= crshift(wuState.U, workSize);
            wuState.W <= crshift(wuState.W, workSize);
          end if;
        else
          wuState <= nextWuState;
        end if;
      end if;

      if stateShift(period-1) = '1' then
        currentState <= doSwap(currentState, nextState, wuState);
      else
        currentState.V <= crshift(currentState.V, workSize);
        currentState.X <= crshift(currentState.X, workSize);
      end if;
    end if;
  end process stage1;

  stage2: process (clk) is
  begin  -- process stage2
    if rising_edge(clk) then
      if resetn = '0' then
        shiftCount <= 0;
      elsif done1 = '1' then
        shiftCount <= 1;
      elsif shiftCount = t+1 then
        shiftCount <= 0;
      elsif shiftCount /= 0 then
        shiftCount <= shiftCount + 1;
      end if;

      if done1 = '1' then
        el_i <= finalShift(nextState.X)(2*t-1 downto t-1);
        ee_i <= finalShift(nextState.V)(2*t-2 downto t-1);
      else
        el_i <= gfZero & el_i(el_i'length-1 downto 1);
        ee_i <= gfZero & ee_i(ee_i'length-1 downto 1);
      end if;

      outValid_i <= '0';

      if done1 = '1' then
        if finalShift(nextState.X)(t-1) = gfZero then
          shiftRight <= '1';
        else
          shiftRight <= '0';
          outValid_i <= '1';
        end if;
      elsif el_i(1) /= gfZero or shiftCount = t then
        shiftRight <= '0';
        outValid_i <= shiftRight;
      end if;

      if outValid_i = '1' then
        -- Convert from array of array to 2-dimentional array
        el_out <= to_vector(el_i);
        ee_out <= to_vector(ee_i);
      end if;

      if shiftCount = t+1 and resetn = '1' then
        outStart <= '1';
      else
        outStart <= '0';
      end if;
    end if;
  end process stage2;

end architecture rtl;
