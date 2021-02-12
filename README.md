First ensure the system has been booted with `/etc/binfmt.d/qemu-aarch64-static.conf` having the following contents:

    :aarch64:M::\x7fELF\x02\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\xb7\x00:\xff\xff\xff\xff\xff\xff\xff\xfc\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff\xff:/usr/bin/qemu-aarch64-static:

Build on x86 with

    docker run --rm -v $(pwd):/usr/src/hs-wayland -w /usr/src/hs-wayland willat8/wayland-hs-buildenv-debian-rpi3:20170720 make

