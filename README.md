# HpView

A Healpix map viewer, written using [Free
Pascal](http://www.freepascal.org/) and
[Lazarus](http://www.lazarus-ide.org/).

## System requirements

Only [cfitsio](http://heasarc.gsfc.nasa.gov/fitsio/fitsio.html) is
required. The program implements its own Healpix projection routines.
Simply put `cfitsio.lib` in the source directory and compile using
Lazarus:

    lazbuild hpview.lpr

The program has been compiled and used on the following platforms:

- Linux Mint 17.2 64-bit (using Gtk+)
- Windows 7 64-bit

