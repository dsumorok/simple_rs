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

entity rs_tb is
  
end entity rs_tb;

architecture behavior of rs_tb is

  component rsDecoderWrapper is
    generic (
      M            : natural;
      n            : natural;
      k            : natural;
      primPoly     : natural := 0;
      alpha        : natural := 2;
      outputParity : boolean := true;
      extraCycles  : natural := 0);
    port (
      clk            : in  std_logic;
      resetn         : in  std_logic;
      dataIn_tdata   : in  std_logic_vector(M-1 downto 0);
      dataIn_tvalid  : in  std_logic;
      dataIn_tready  : out std_logic;
      dataIn_tlast   : in  std_logic;
      dataOut_tdata  : out std_logic_vector(M-1 downto 0);
      dataOut_tvalid : out std_logic;
      dataOut_tready : in  std_logic;
      dataOut_tlast  : out std_logic);
  end component rsDecoderWrapper;
  
  component rsEncoderWrapper is
    generic (
      M        : natural;
      n        : natural;
      k        : natural;
      primPoly : natural := 0;
      alpha    : natural := 2);
    port (
      clk            : in  std_logic;
      resetn         : in  std_logic;
      dataIn_tdata   : in  std_logic_vector(M-1 downto 0);
      dataIn_tvalid  : in  std_logic;
      dataIn_tready  : out std_logic;
      dataIn_tlast   : in  std_logic;
      dataOut_tdata  : out std_logic_vector(M-1 downto 0);
      dataOut_tvalid : out std_logic;
      dataOut_tready : in  std_logic;
      dataOut_tlast  : out std_logic);
  end component rsEncoderWrapper;
  
  component rsFifo is
    generic (
      width : natural;
      depth : natural);
    port (
      clk            : in  std_logic;
      resetn         : in  std_logic;
      dataIn_tdata   : in  std_logic_vector(width-1 downto 0);
      dataIn_tvalid  : in  std_logic;
      dataIn_tready  : out std_logic;
      dataOut_tdata  : out std_logic_vector(width-1 downto 0);
      dataOut_tvalid : out std_logic;
      dataOut_tready : in  std_logic);
  end component rsFifo;
  
  constant CLK_PER : time := 1 us / 100;
  
  constant M            : natural := 8;
  constant n            : natural := 254;
  constant k            : natural := 204;
  constant primPoly     : natural := 0;
  constant alpha        : natural := 2;
  constant feedPeriod   : natural := 1;
  constant outputPeriod : natural := 1;
  constant inputLength  : natural := 30*k-5;
  constant outputParity : boolean := true;
  constant extraCycles  : natural := 0;

  constant Q : natural := 2**M;
  constant t : natural := (n-k)/2;

  signal clk    : std_logic := '0';
  signal reset  : std_logic := '1';
  signal resetn : std_logic := '0';

  signal encoderIn        : std_logic_vector(M-1 downto 0) := (others => '0');
  signal encoderInValid   : std_logic                      := '0';
  signal encoderInReady   : std_logic                      := '0';
  signal encoderInLast    : std_logic                      := '0';
  signal encoderOut       : std_logic_vector(M-1 downto 0);
  signal encoderOutValid  : std_logic;
  signal encoderOutReady  : std_logic                      := '0';
  signal encoderOutLast   : std_logic;
  signal decoderIn        : std_logic_vector(M-1 downto 0) := (others => '0');
  signal decoderInValid   : std_logic                      := '0';
  signal decoderInReady   : std_logic;
  signal decoderInLast    : std_logic                      := '0';
  signal decoderOut       : std_logic_vector(M-1 downto 0) := (others => '0');
  signal decoderOutValid  : std_logic;
  signal decoderOutReady  : std_logic                      := '1';
  signal decoderOutLast   : std_logic;
  signal decoderWordCount : natural                        := 0;

  signal fifoDataIn      : std_logic_vector(M-1 downto 0)  := (others => '0');
  signal fifoDataOut     : std_logic_vector(M-1 downto 0)  := (others => '0');
  signal readFifo        : std_logic                       := '0';
  signal writeFifo       : std_logic                       := '0';
  signal feedCount       : natural range 1 to feedPeriod   := 1;
  signal outputCount     : natural range 1 to outputPeriod := 1;
  signal errorCount      : natural                         := 0;
  signal errorStartIndex : natural                         := 0;
  signal count           : natural                         := 0;

begin  -- architecture behavior

  resetn <= not reset;

  -- Reed Solomon encoder
  encoder_i : rsEncoderWrapper
    generic map (
      M        => M,
      n        => n,
      k        => k,
      primPoly => primPoly,
      alpha    => alpha)
    port map (
      clk            => clk,
      resetn         => resetn,
      dataIn_tdata   => encoderIn,
      dataIn_tvalid  => encoderInValid,
      dataIn_tready  => encoderInReady,
      dataIn_tlast   => encoderInLast,
      dataOut_tdata  => encoderOut,
      dataOut_tvalid => encoderOutValid,
      dataOut_tready => encoderOutReady,
      dataOut_tlast  => encoderOutLast);

  encoderOutReady <= decoderInReady;
  
  -- Saves encoder output for later comparison
  outFifo_i : rsFifo
    generic map (
      width => M,
      depth => 10*n)
    port map (
      clk            => clk,
      resetn         => resetn,
      dataIn_tdata   => fifoDataIn,
      dataIn_tvalid  => writeFifo,
      dataIn_tready  => open,
      dataOut_tdata  => fifoDataOut,
      dataOut_tvalid => open,
      dataOut_tready => readFifo);

  writeFifo <= encoderOutValid and encoderOutReady when outputParity else
               encoderInValid and encoderInReady;
  fifoDataIn <= encoderOut when outputParity else encoderIn;
  readFifo  <= decoderOutValid and decoderOutReady;
  
  -- Reed Solomon decoder
  decoder_i : rsDecoderWrapper
    generic map (
      M            => M,
      n            => n,
      k            => k,
      primPoly     => primPoly,
      alpha        => alpha,
      outputParity => outputParity,
      extraCycles  => extraCycles)
    port map (
      clk            => clk,
      resetn         => resetn,
      dataIn_tdata   => decoderIn,
      dataIn_tvalid  => decoderInValid,
      dataIn_tready  => decoderInReady,
      dataIn_tlast   => decoderInLast,
      dataOut_tdata  => decoderOut,
      dataOut_tvalid => decoderOutValid,
      dataOut_tready => decoderOutReady,
      dataOut_tlast  => decoderOutLast);
  
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

    wait until count = 8*k;
    wait until falling_edge(clk);
    -- Test reset by asserting it
    reset <= '1';
    wait until falling_edge(clk);
    reset <= '0';
 
    wait;
  end process getReset;

  -- Generates input data for the encoder.
  -- Just generates a counter
  genInput: process (clk, reset) is
    variable encoderInI : natural;
  begin  -- process genInput
    if reset = '1' then
      encoderIn      <= (others => '0');
      encoderInValid <= '0';
      encoderInLast  <= '0';
      count          <=  0;
      feedCount      <= feedPeriod;
      outputCount    <= outputPeriod;
    elsif rising_edge(clk) then
      if outputCount = outputPeriod then
        decoderOutReady <= '1';
        outputCount     <= 1;
      else
        outputCount     <= outputCount + 1;
        decoderOutReady <= '0';
      end if;

      if encoderInReady = '1' then
        encoderInLast <= '0';

        if feedCount = feedPeriod then
          if count = inputLength-1 then
            encoderInLast <= '1';
          end if;

          if count /= inputLength then
            encoderInValid <= '1';
            count          <= count + 1;
          else
            encoderInValid <= '0';
          end if;
          feedCount  <= 1;
        else
          encoderInValid <= '0';
          feedCount      <= feedCount + 1;
        end if;

        if encoderInValid = '1' then
          encoderInI := to_integer(unsigned(encoderIn));
          encoderInI := (encoderInI - 1) mod Q;
          encoderIn <=
            std_logic_vector(to_unsigned(encoderInI, M));
        end if;
        
      end if;
        
      end if;
  end process genInput;

  -- Adds error to the encoder output and feeds the decoder
  genDecoderInput: process (clk) is
    variable errBit   : std_logic_vector(M-1 downto 0) := (others => '0');
    variable outStart : natural := n-t;
  begin  -- process genDecoderInput
    if resetn = '0' then
      decoderWordCount <= 0;
      decoderInValid   <= '0';
      decoderInLast    <= '0';
      decoderIn        <= (others => '0');
      errorStartIndex  <= n-t;

    elsif rising_edge(clk) then
      if decoderInReady = '1' then
        decoderInValid <= encoderOutValid;
        decoderInLast  <= encoderOutLast;

        if encoderOutValid = '1' then
          if decoderWordCount = n-1 then
            decoderWordCount <= 0;

            if errorStartIndex < t then
              errorStartIndex <= errorStartIndex + n - t;
            else
              errorStartIndex <= errorStartIndex - t;
            end if;
          else
            decoderWordCount <= decoderWordCount + 1;
          end if;

        end if;

        if decoderWordCount < errorStartIndex then
          decoderIn <= encoderOut;
        elsif decoderWordCount < (errorStartIndex + t) then
          errBit := std_logic_vector( to_unsigned((n-decoderWordCount) + 15, M));
          decoderIn <= encoderOut xor errBit;
        else
          decoderIn <= encoderOut;
        end if;
        
      end if;

    end if;
  end process genDecoderInput;

  -- Counts the errors after decoding.
  -- The errorCount should be zero
  countErrors: process (clk, resetn) is
  begin  -- process countErrors
    if resetn = '0' then
      errorCount <= 0;
    elsif rising_edge(clk) then
      if decoderOutValid = '1' and decoderOutReady = '1' then
        if decoderOut /= fifoDataOut then
          errorCount <= errorCount + 1;
        end if;
      end if;
    end if;
  end process countErrors;
  
end architecture behavior;
