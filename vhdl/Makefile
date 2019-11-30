GHDL = ghdl
STD=08

DESIGNS = aes128_tb aes256_tb rs_tb

aes128_tb_SOURCES = \
	gf.vhd \
	aes_pkg.vhd \
	aesRound.vhd \
	expandKey128.vhd \
	aes128.vhd \
	aes128_tb.vhd

aes256_tb_SOURCES = \
	gf.vhd \
	aes_pkg.vhd \
	aesRound.vhd \
	expandKey256.vhd \
	aes256.vhd \
	aes256_tb.vhd

rs_tb_SOURCES = \
	gf.vhd \
	rsUtil.vhd \
	rs_ram.vhd \
	rsFifo.vhd \
	start2valid.vhd \
	calcErrors.vhd \
	calcSyndromes.vhd \
	chien_search.vhd \
	correctErrors.vhd \
	mea.vhd \
	rsDecoder.vhd \
	rsEncoder.vhd \
	rsFeeder.vhd \
	rsOutputStage.vhd \
	rsEncoderWrapper.vhd \
	rsDecoderWrapper.vhd \
	rs_tb.vhd

all: $(DESIGNS)

include ghdl.mk
