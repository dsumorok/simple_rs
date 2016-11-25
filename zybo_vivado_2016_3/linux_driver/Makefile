KSRC ?= linux
ARCH ?= arm

obj-m += rs_driver.o

all:
	make -C $(KSRC) M=$(PWD) ARCH=$(ARCH) modules

clean:
	make -C $(KSRC) M=$(PWD) ARCH=$(ARCH) clean
