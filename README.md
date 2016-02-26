# HpView

A Healpix map viewer, written using [Free
Pascal](http://www.freepascal.org/) and
[Lazarus](http://www.lazarus-ide.org/).

## System requirements

Only [cfitsio](http://heasarc.gsfc.nasa.gov/fitsio/fitsio.html) is
required. The program implements its own Healpix projection routines.

Compile the program using Lazarus:

    lazbuild hpview.lpr

(under Windows, you have to download a precompiled binary from
http://heasarc.gsfc.nasa.gov/fitsio/fitsio.html and put `cfitsio.lib`
in the source directory.)

The program has been compiled and used on the following platforms:

- Linux Mint 17.2 64-bit (using Gtk+)
- Windows 7 64-bit

## Screenshots

![hpview_screenshot](./hpview_screenshot.png)
