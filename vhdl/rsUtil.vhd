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

package rsUtil is

  function coreDecoderLatency (
    constant n       : natural;
    constant k       : natural)
    return natural;

  function calcNumStages (
    constant addsPerStage : natural;
    constant polyLen      : natural)
    return natural;

  constant addsPerSum  : natural := 6;

end package rsUtil;

package body rsUtil is

  -- purpose: Estimates the latancy of rsDecoder
  function coreDecoderLatency (
    constant n       : natural;
    constant k       : natural)
    return natural is
    variable result : natural := 0;
    constant t      : natural := n-k;
  begin  -- function coreDecoderLatency
    -- Initial Buffer
    result := 1;

    -- Latency of CalcSyndromes
    result := result + 2*t+n + 1;

    -- Maximum latency of mea
    result := result + n + t + 1;

    -- chien_search
    result := result + calcNumStages(addsPerSum, n-k+1) + 6;

    return result;
  end function coreDecoderLatency;

  function calcNumStages (
    constant addsPerStage : natural;
    constant polyLen      : natural)
    return natural is

    variable result      : natural := 1;
    variable stageLength : natural := addsPerStage;
  begin  -- function calcNumStages
    while stageLength < polyLen loop
      stageLength := stageLength * addsPerStage;
      result      := result + 1;
    end loop;

    return result;
  end function calcNumStages;

end package body rsUtil;
