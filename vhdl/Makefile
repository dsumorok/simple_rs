TOP = rs_tb
GHDL = ghdl
WORKDIR = work
SIMTIME = 120us
WAVE = $(TOP).ghw
OPTFILE = $(TOP)_optfile
STD=08

SOURCES = \
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

OBJECTS = $(patsubst %.vhd,$(WORKDIR)/%.o,$(SOURCES))

ALL: $(TOP)

$(WORKDIR): Makefile
	mkdir -p $(WORKDIR)
	$(GHDL) --clean --std=$(STD) --workdir=$(WORKDIR)
	$(GHDL) -i --std=$(STD) --workdir=$(WORKDIR) $(SOURCES)

$(WORKDIR)/%.o: %.vhd
	$(GHDL) -a --std=$(STD) --workdir=$(WORKDIR) $<

clean:
	rm -rf $(WORKDIR)

$(TOP): $(WORKDIR) $(OBJECTS)
	$(GHDL) -e --std=$(STD) --workdir=$(WORKDIR) $@

sim: $(TOP)
	./$(TOP) --wave=$(WAVE) --read-wave-opt=$(OPTFILE)
