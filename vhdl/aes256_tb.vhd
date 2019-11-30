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

entity aes256_tb is

end entity aes256_tb;

architecture behavior of aes256_tb is
  component aes256 is
    port (
      clk     : in  std_logic;
      reset   : in  std_logic;
      pt      : in  std_logic_vector(127 downto 0);
      key     : in  std_logic_vector(255 downto 0);
      start   : in  std_logic;
      busy    : out std_logic;
      ct      : out std_logic_vector(127 downto 0);
      ctValid : out std_logic);
  end component aes256;

  constant CLK_PER : time := 1 us / 100;

  constant inputBlock : std_logic_vector(127 downto 0) :=
    X"00112233445566778899aabbccddeeff";

  constant key : std_logic_vector(255 downto 0) :=
    X"000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f";

  constant expectedOutput : std_logic_vector(127 downto 0) :=
    X"8ea2b7ca516745bfeafc49904b496089";

  signal clk     : std_logic := '0';
  signal reset   : std_logic := '1';
  signal start   : std_logic := '0';
  signal started : std_logic := '0';
  signal busy    : std_logic;
  signal ct      : std_logic_vector(127 downto 0);
  signal ctValid : std_logic;
begin  -- architecture behavior

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

  genInput: process (clk, reset) is
  begin  -- process genInput
    if reset = '1' then
      start     <= '0';
      started   <= '0';

    elsif rising_edge(clk) then

      started <=  '1';
      start   <= not started;
    end if;
  end process genInput;

  timeout: process is
  begin  -- process timeout
    wait for 1.2 us;
    finish(1);
  end process timeout;

  waitDone: process (clk) is
  begin  -- process waitDone
    if rising_edge(clk) then
      if ctValid = '1' then
        assert ct = expectedOutput report "Encrypt Failed" severity failure;
        finish(0);
      end if;
    end if;
  end process waitDone;

  aes_i : aes256
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
