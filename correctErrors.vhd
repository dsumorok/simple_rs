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

entity correctErrors is
  generic (
    M        : natural;
    n        : natural;
    k        : natural);
  port (
    clk           : in  std_logic;
    resetn        : in  std_logic;
    dataIn        : in  gfEl_t(M-1 downto 0);
    dataInStart   : in  std_logic;
    errorsIn      : in  gfEl_t(M-1 downto 0);
    errorsInStart : in  std_logic;
    dataOut       : out std_logic_vector(M-1 downto 0);
    dataOutStart  : out std_logic);
end entity correctErrors;

architecture rtl of correctErrors is
  component rs_ram is
    generic (
      WIDTH : natural := 8;
      DEPTH : natural := 2048);
    port (
      clk     : in  std_logic;
      inAddr  : in  natural range DEPTH-1 downto 0;
      inData  : in  std_logic_vector(WIDTH-1 downto 0);
      wrEn    : in  std_logic;
      outAddr : in  natural range DEPTH-1 downto 0;
      outData : out std_logic_vector(WIDTH-1 downto 0));
  end component rs_ram;

  constant fifoDepth : natural := coreDecoderLatency(n, k) + n;

  signal inRamInAddr  : natural range fifoDepth-1 downto 0 := 0;
  signal inRamCount   : natural range n-1 downto 0 := 0;
  signal inRamInData  : std_logic_vector(M-1 downto 0);
  signal inRamWrEn    : std_logic                    := '0';
  signal inRamOutAddr : natural range fifoDepth-1 downto 0 := fifoDepth-n;
  signal inRamOutData : std_logic_vector(M-1 downto 0);
  signal outCount     : natural range 0 to n;

  signal reorderInData  : std_logic_vector(M-1 downto 0) := (others => '0');
  signal reorderInAddr  : natural range 2*n-1 downto 0   := 2*n-1;
  signal reorderWrEn    : std_logic                      := '0';
  signal reorderOutData : std_logic_vector(M-1 downto 0) := (others => '0');
  signal reorderOutAddr : natural range 2*n-1 downto 0   := 0;
  signal reorderStartSR : std_logic_vector(5 downto 0)   := (others => '0');

  signal error_0 : gfEl_t(M-1 downto 0) := (others => '0');
  signal error_1 : gfEl_t(M-1 downto 0) := (others => '0');
  signal error_2 : gfEl_t(M-1 downto 0) := (others => '0');

  signal errorsInStart_1 : std_logic := '0';
  signal errorsInStart_2 : std_logic := '0';
begin  -- architecture rtl

  saveInput: process (clk) is
  begin  -- process saveInput
    if rising_edge(clk) then
      inRamInData <= std_logic_vector(dataIn);
      if resetn = '0' then
        inRamWrEn   <= '0';
      elsif dataInStart = '1' then
        inRamWrEn   <= '1';
      elsif inRamCount = n-1 then
        inRamWrEn <= '0';
      end if;

      if resetn = '0' then
        inRamInAddr <= 0;
        inRamCount  <= 0;
      elsif inRamWrEn = '1' then
        if inRamCount = n-1 then
          inRamCount <= 0;
        else
          inRamCount <= inRamCount + 1;
        end if;

        if inRamInAddr = fifoDepth-1 then
          inRamInAddr <= 0;
        else
          inRamInAddr <= inRamInAddr + 1;          
        end if;
      end if;
    end if;
  end process saveInput;

  readInRam_i: process (clk) is
  begin  -- process readInRam_i
    if rising_edge(clk) then
      if resetn = '0' then
        inRamOutAddr <= fifoDepth-n;
        outCount     <= 0;
        reorderStartSR <= (others => '0');
      else
        if outCount = n then
          reorderStartSR(0) <= '1';
        else
          reorderStartSR(0) <= '0';
        end if;

        reorderStartSR(5 downto 1) <= reorderStartSR(4 downto 0);
        
        if errorsInStart = '1' then
          outCount     <= 1;
        elsif outCount = n then
          outCount <= 0;
        elsif outCount /= 0 then
          outCount     <= outCount + 1;
        end if;

        if errorsInStart = '1' then
          if inRamOutAddr > fifoDepth - (2*n) then
            inRamOutAddr <= inRamOutAddr + (2*n-1) - fifoDepth;
          else
            inRamOutAddr <= inRamOutAddr + 2*n-1;
          end if;
        elsif outCount /= 0 and outCount /= n then
          if inRamOutAddr = 0 then
            inRamOutAddr <= fifoDepth-1;
          else
            inRamOutAddr <= inRamOutAddr - 1;
          end if;
        end if;
        
      end if;
    end if;
  end process readInRam_i;

  reorderProc: process (clk) is
  begin  -- process reorderProc
    if rising_edge(clk) then
      error_0 <= errorsIn;
      error_1 <= error_0;
      error_2 <= error_1;

      if resetn = '0' then
        reorderWrEn <= '0';
        reorderInAddr <= 2*n-1;
        errorsInStart_1 <= '0';
        errorsInStart_2 <= '0';
        reorderOutAddr  <= 0;
      else
        errorsInStart_1 <= errorsInStart;
        errorsInStart_2 <= errorsInStart_1;

        if errorsInStart_2 = '1' then
          reorderWrEn <= '1';
        elsif reorderInAddr = 0 or reorderInAddr = n then
          reorderWrEn <= '0';
        end if;

        if errorsInStart_2 = '1' then
          if reorderInAddr = 0 then
            reorderInAddr  <= 2*n-1;
          else
            reorderInAddr  <= reorderInAddr - 1;
          end if;
        elsif reorderInAddr /= 0 and reorderInAddr /= n then
          reorderInAddr  <= reorderInAddr - 1;
        end if;

        if reorderStartSR(3) = '1' then
          if reorderOutAddr = 2*n-1 then
            reorderOutAddr <= 0;
          else
            reorderOutAddr <= reorderOutAddr + 1;
          end if;
        elsif reorderOutAddr /= 0 and reorderOutAddr /= n then
          if reorderOutAddr = 2*n-1 then
            reorderOutAddr <= 0;
          else
            reorderOutAddr <= reorderOutAddr + 1;
          end if;
        end if;
      end if;

      dataOut      <= reorderOutData;
      dataOutStart <= reorderStartSR(5);
    end if;
  end process reorderProc;
  
  inRam_i : rs_ram
    generic map (
      WIDTH => M,
      DEPTH => fifoDepth)
    port map (
      clk     => clk,
      inAddr  => inRamInAddr,
      inData  => inRamInData,
      wrEn    => inRamWrEn,
      outAddr => inRamOutAddr,
      outData => inRamOutData);

  reorderInData <= std_logic_vector(error_2) xor inRamOutData;

  reorderRam_i : rs_ram
    generic map (
      WIDTH => M,
      DEPTH => n*2)
    port map (
      clk     => clk,
      inAddr  => reorderInAddr,
      inData  => reorderInData,
      wrEn    => reorderWrEn,
      outAddr => reorderOutAddr,
      outData => reorderOutData);
    
end architecture rtl;
