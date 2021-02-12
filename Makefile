LDLIBS=-lwayland-client -lxkbcommon -lwayland-cursor -lpixman-1 -lcairo -lpng16 -lm

westondeps = ../weston/protocol/weston_desktop_shell-weston-desktop-shell-protocol.o ../weston/.libs/libtoytoolkit.a

all: hsmyth
.PHONY: all

%.hs: %.hsc
	hsc2hs -C -iquoteinclude -I /usr/include/cairo $^

hsmyth: Myth/Internal.hs Myth/Status.hs Myth/Render.hs Myth/Common.hs Myth/Alert.hs Main.hs
	ghc -O2 -threaded -o $@ --make $(LDLIBS) Main $(westondeps)
	strip -p --strip-unneeded --remove-section=.comment $@

install:
	install -m755 -o root -g root hsmyth /usr/local/bin/myth

