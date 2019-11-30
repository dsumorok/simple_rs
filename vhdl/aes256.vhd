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

library std;
use std.env.all;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity aes256 is
  port (
    clk     : in  std_logic;
    reset   : in  std_logic;
    pt      : in  std_logic_vector(127 downto 0);
    key     : in  std_logic_vector(255 downto 0);
    start   : in  std_logic;
    busy    : out std_logic;
    ct      : out std_logic_vector(127 downto 0);
    ctValid : out std_logic);
end entity aes256;

architecture behavior of aes256 is
  component expandKey256 is
    port (
      clk      : in  std_logic;
      reset    : in  std_logic;
      keyIn    : in  std_logic_vector(31 downto 0);
      rCon     : in  std_logic_vector(7 downto 0);
      start    : in  std_logic;
      keyOut   : out std_logic_vector(31 downto 0);
      RconOut  : out std_logic_vector(7 downto 0);
      outStart : out std_logic);
  end component expandKey256;

  component aesRound is
    port (
      clk        : in  std_logic;
      reset      : in  std_logic;
      keyIn      : in  std_logic_vector(31 downto 0);
      ptIn       : in  std_logic_vector(31 downto 0);
      start      : in  std_logic;
      lastRound  : in  std_logic;
      ctOut      : out std_logic_vector(31 downto 0);
      outStart   : out std_logic);
  end component aesRound;

  signal start1    : std_logic                      := '0';
  signal start2    : std_logic                      := '0';
  signal key_i     : std_logic_vector(255 downto 0) := (others => '0');
  signal pt_i      : std_logic_vector(127 downto 0) := (others => '0');
  signal keyIn     : std_logic_vector(31 downto 0)  := (others => '0');
  signal ptIn      : std_logic_vector(31 downto 0)  := (others => '0');
  signal keyOut    : std_logic_vector(31 downto 0)  := (others => '0');
  signal ctOut     : std_logic_vector(31 downto 0)  := (others => '0');
  signal rConIn    : std_logic_vector(7 downto 0)   := (others => '0');
  signal rConOut   : std_logic_vector(7 downto 0)   := (others => '0');
  signal ct_i      : std_logic_vector(127 downto 0) := (others => '0');
  signal feedback  : std_logic                      := '0';
  signal feedback2 : std_logic                      := '0';
  signal outStart  : std_logic                      := '0';
  signal lastRound : std_logic                      := '0';
  signal ctValid_i : std_logic                      := '0';
  signal busy_i    : std_logic                      := '0';
  signal count     : integer range 0 to 101         := 0;
  signal shiftKey  : std_logic                      := '0';
  
begin  -- architecture behavior

  mainProc: process (clk) is
  begin  -- process mainProc
    if rising_edge(clk) then
      -- buffer start signal
      if reset = '1' then
        start1 <= '0';
      elsif start = '1' then
        start1 <= not busy_i;
      elsif count = 6 then
        start1 <= '1';
      else
        start1 <= '0';
      end if;

      -- The AES round takes 7 cycles, so the first 4 words of the
      -- 8-word key are shifted in during the first 4 cycles, and the
      -- next four words don't start to shift in until 3 cycles later.
      if (count = 3) or (count = 4) or (count = 5) then
        shiftKey <= '0';
      else
        shiftKey <= '1';
      end if;
      
      -- Shift 128-bit input words and 256-bit keys words in 32 bits
      -- at a time
      if start = '1' then
        key_i <= key;
        pt_i  <= pt;
      else
        if shiftKey = '1' then
          key_i <= key_i(223 downto 0) & X"00000000";
        end if;
        pt_i  <= pt_i(95 downto 0) & X"00000000";
      end if;

      -- count cycles so we know when the operation is complete
      if reset = '1' then
        count <= 0;
      elsif start1 = '1' and count = 0 then
        count <= 1;
      elsif count = 101 then
        count <= 0;
      elsif count /= 0 then
        count <= count + 1;
      end if;

      -- After first key and plaintext are fed in, subsequent outputs
      -- are fed back in.  feedback controls the multiplexor
      if reset = '1' or count = 94 then
        feedback <= '0';
      elsif count = 13 then
        feedback <= '1';
      end if;

      -- After first key and plaintext are fed in, subsequent outputs
      -- are fed back in.  feedback controls the multiplexor
      if reset = '1' or count = 94 then
        feedback2 <= '0';
      elsif count = 5 then
        feedback2 <= '1';
      end if;

      -- The last encryption round is handled slightly differently
      if count = 90 then
        lastRound <= '1';
      else
        lastRound <= '0';
      end if;

      -- The busy signal goes high while the block is worked on
      if reset = '1' then
        busy_i <= '0';
      elsif start = '1' then
        busy_i <= '1';
      elsif count = 101 then
        busy_i <= '0';
      end if;

      -- The ctValid signal goes high when the operation is complete
      -- and the output is valid
      if count = 101 then
        ctValid_i <= '1';
      else
        ctValid_i <= '0';
      end if;

      -- This is the output
      ct_i <= ct_i(95 downto 0) & (ctOut xor keyOut);
      
    end if;
  end process mainProc;
  
  keyIn  <= keyOut   when feedback = '1' else key_i(255 downto 224);
  ptIn   <= ctOut    when feedback2 = '1' else pt_i(127 downto 96);
  start2 <= outStart when feedback = '1' else start1;
  rConIn <= rConOut  when feedback = '1' else X"01";

  ctValid <= ctValid_i;
  busy    <= busy_i;
  ct      <= ct_i;

  expand_i : expandKey256
    port map (
      clk      => clk,
      reset    => reset,
      keyIn    => keyIn,
      rCon     => rConIn,
      start    => start2,
      keyOut   => keyOut,
      RconOut  => RconOut,
      outStart => open);

  round_i : aesRound
    port map (
      clk        => clk,
      reset      => reset,
      keyIn      => keyIn,
      ptIn       => ptIn,
      start      => start2,
      lastRound  => lastRound,
      ctOut      => ctOut,
      outStart   => outStart);
  
end architecture behavior;
