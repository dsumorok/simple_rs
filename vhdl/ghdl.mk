define add_design

$(1)-work/work-obj$(STD).cf: Makefile
	mkdir -p $(1)-work
	$(GHDL) --clean --std=$(STD) --workdir=$(1)-work
	$(GHDL) -i --std=$(STD) --workdir=$(1)-work $(2)

$(1)-work/%.o: %.vhd
	$(GHDL) -a --std=$(STD) --workdir=$(1)-work -O3 $$<

$(1): $(patsubst %.vhd,$(1)-work/%.o,$(2))
	$(GHDL) -e --std=$(STD) --workdir=$(1)-work $(1)

$(1)_dep.mk: $(1)-work/work-obj$(STD).cf $2
	$(GHDL) --gen-depends --std=$(STD) --workdir=$(1)-work $(1) > $(1)_dep.mk

$(1)-sim: $(1)
	./$(1) --wave=$(1).ghw --read-wave-opt=$(1)_optfile

-include $(1)_dep.mk

clean: $(1)-clean

$(1)-clean:
	rm -rf $(1) $(1).ghw $(1)_dep.mk e~$(1).o $(1)-work

.PHONY: $(1)-clean

endef

$(foreach design, $(DESIGNS), $(eval $(call add_design,$(design), \
	$($(design)_SOURCES))))

.PHONY: all clean
