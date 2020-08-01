TCLSH	= tclsh8.7
srcdir	= .

all: tm

tm: library/*
	tbuild

clean:
	rm -rf tm

test: all
	$(TCLSH) "$(srcdir)/tests/all.tcl" $(TESTFLAGS)
