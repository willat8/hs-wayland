LDLIBS=-lwayland-client -lxkbcommon -lwayland-cursor -lpixman-1 -lcairo -lpng16 -lm

CFLAGS=-I/usr/include/cairo

westondeps = ../weston/protocol/weston_desktop_shell-weston-desktop-shell-protocol.o ../weston/.libs/libtoytoolkit.a

all: hsmyth
.PHONY: all

%.hs: %.hsc
	hsc2hs $(CFLAGS) $^

hsmyth: Myth/Internal.hs Myth/Status.hs Main.hs
	ghc -O2 -o $@ --make $(LDLIBS) Main $(westondeps)
	strip $@

install:
	install -m755 -o root -g root hsmyth /usr/local/bin/myth

