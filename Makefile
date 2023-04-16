.PHONY: build test clean

TARGET  = accsat
DESTDIR = /usr/local/bin
RUSTEXE = rewriter
RUSTBIN = $(RUSTEXE)/target/release/$(RUSTEXE)

all : $(TARGET) $(RUSTBIN)

$(TARGET) : *.rkt
	raco exe -o $(TARGET) main.rkt

$(RUSTBIN) : $(RUSTEXE)/src/*
	cargo build --release --manifest-path $(RUSTEXE)/Cargo.toml

test :
	raco test --process *.rkt

install : $(TARGET)
	install -m 755 $(TARGET) $(DESTDIR)

uninstall :
	rm -f $(DESTDIR)/$(TARGET)

clean :
	cargo clean --manifest-path $(RUSTEXE)/Cargo.toml
	rm -f $(TARGET) $(RUSTBIN)
