<!--
SPDX-FileCopyrightText: © 2024 Jean-Pierre De Jesus DIAZ <me@jeandudey.tech>
SPDX-License-Identifier: GPL-3.0-or-later
-->

# Guix-Raspberry

This GNU Guix channel provides a EDK II firmware distribution for
the Raspberry Pi 4, _built_ free of binary blobs completely from
source.

It still requires the `start4.elf` and `fixup4.dat` files from
the Raspberry Pi [firmware](https://github.com/raspberrypi/firmware)
repository.

You don't need to add this to your channels unless you specifically
need it, but for building the EDK II distribution:

```
git clone https://github.com/jeandudey/guix-raspberry
cd guix-raspberry
guix build -L . edk2-rpi4
cp -r $(guix build -L . edk2-rpi4)/share/firmware <the path to your mounted microSD FAT32 partition>
```

You can then download the `start4.elf` and `fixup4.dat` files from
the Raspberry Pi [firmware](https://github.com/raspberrypi/firmware)
repository and place them manually in the microSD card too.
