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

library work;

entity aes_tb is
  
end entity aes_tb;

architecture behavior of aes_tb is
  component expandKey is
    port (
      clk      : in  std_logic;
      reset    : in  std_logic;
      keyIn    : in  std_logic_vector(31 downto 0);
      rCon     : in  std_logic_vector(7 downto 0);
      start    : in  std_logic;
      keyOut   : out std_logic_vector(31 downto 0);
      RconOut  : out std_logic_vector(7 downto 0);
      outStart : out std_logic);
  end component expandKey;

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

  component aes1 is
    port (
      clk     : in  std_logic;
      reset   : in  std_logic;
      pt      : in  std_logic_vector(127 downto 0);
      key     : in  std_logic_vector(127 downto 0);
      start   : in  std_logic;
      busy    : out std_logic;
      ct      : out std_logic_vector(127 downto 0);
      ctValid : out std_logic);
  end component aes1;
  
  constant CLK_PER : time := 1 us / 100;

  signal clk    : std_logic := '0';
  signal reset  : std_logic := '1';
  signal resetn : std_logic := '0';

  constant inputBlock : std_logic_vector(127 downto 0) :=
    X"3243f6a8885a308d313198a2e0370734";

  constant key : std_logic_vector(127 downto 0) :=
    X"2b7e151628aed2a6abf7158809cf4f3c";

  signal busy    : std_logic;
  signal ct      : std_logic_vector(127 downto 0);
  signal ctValid : std_logic;
  
  signal start     : std_logic := '0';
  signal started   : std_logic := '0';

  -- signal inputWord : std_logic_vector(31 downto 0) :=
  --   (others => '0');

  -- signal keyWord : std_logic_vector(31 downto 0) :=
  --   (others => '0');
 
  signal rCon          : std_logic_vector(7 downto 0)  := X"01";
  signal keyOut        : std_logic_vector(31 downto 0);
  signal RconOut       : std_logic_vector(7 downto 0);
  signal round         : integer                       := 0;
  signal outStart      : std_logic;
  signal count         : integer                       := 0;
  signal lastRound     : std_logic                     := '0';
  signal ctOut         : std_logic_vector(31 downto 0);
  signal roundOutStart : std_logic;
  signal feedback : std_logic := '0';
begin  -- architecture behavior
  
  resetn <= not reset;

  genClk: process is
  begin  -- process genClk
    clk <= '0';
    wait for CLK_PER / 2;

    clk <= '1';
    wait for CLK_PER / 2;
  end process genClk;

  getReset: process is
  begin  -- process getReset
    reset <= '1';

    wait for clk_per * 10;
    wait until falling_edge(clk);
    reset <= '0';
 
    wait;
  end process getReset;

  genInput: process (clk, resetn) is
  begin  -- process genInput
    if resetn = '0' then
      -- inputWord <= (others => '0');
      -- keyWord   <= (others => '0');
      start     <= '0';
      started   <= '0';
      count     <= 0;
      feedback  <= '0';
      lastRound <= '0';

    elsif rising_edge(clk) then

      started <=  '1';
      start   <= not started;
      -- if feedback = '1' then
      --   rCon      <= RconOut;
      --   start     <= outStart when (round /= 9) else '0';
      --   keyWord   <= keyOut;
      --   inputWord <= ctOut;
      -- else
      --   rCon      <= X"01";
      --   start     <= not started;
      --   keyWord   <= key( (4-count)*32-1 downto (3-count)*32);
      --   inputWord <= inputBlock( (4-count)*32-1 downto (3-count)*32 );
      -- end if;

      -- if round = 8 then
      --   lastRound <= '1';
      -- end if;
      
      -- if started = '0' and start = '0' then
      --   round <= 0;
      -- elsif outStart = '1' then
      --   round <= round + 1;
      -- end if;

      -- if started = '0' and start = '0' then
      --   count <= 1;
      -- else
      --   count <= (count + 1) mod 4;
      -- end if;

      -- if count = 3 then
      --   feedback <= '1';
      -- end if;
    end if;
  end process genInput;

  waitEnd: process is
  begin  -- process waitEnd
    wait for 1.2 us;
    finish(0);
  end process waitEnd;

  -- expand_i : expandKey
  --   port map (
  --     clk      => clk,
  --     reset    => reset,
  --     keyIn    => keyWord,
  --     rCon     => rCon,
  --     start    => start,
  --     keyOut   => keyOut,
  --     RconOut  => RconOut,
  --     outStart => outStart);

  -- round_i : aesRound
  --   port map (
  --     clk        => clk,
  --     reset      => reset,
  --     keyIn      => keyWord,
  --     ptIn       => inputWord,
  --     start      => start,
  --     lastRound  => lastRound,
  --     ctOut      => ctOut,
  --     outStart   => roundOutStart);

  aes_i : aes1
    port map (
      clk     => clk,
      reset   => reset,
      pt      => inputBlock,
      key     => key,
      start   => start,
      busy    => busy,
      ct      => ct,
      ctValid => ctValid);
  
end architecture behavior;
