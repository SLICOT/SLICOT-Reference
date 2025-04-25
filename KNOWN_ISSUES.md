Known Issues
============

Last Updated: April, 25th 2025

Failing test with Flang on Windows / MSYS2
------------------------------------------

The tests for the routines

* `UD01DB`
* `UD01ND`

fail if SLICOT is compiled using SHARED libraries. In general
these two routines will not work with SLICOT as shared library
under Windows if it is compiled with FLang.

The behavior was observed using
```shell
$ flang --version
flang version 19.1.7
Target: x86_64-w64-windows-gnu
Thread model: posix
InstalledDir: C:/msys64/clang64/bin
```
and
```shell
$ flang --version
flang version 20.1.3
Target: x86_64-w64-windows-gnu
Thread model: posix
InstalledDir: C:/msys64/clang64/bin
```


The bug is known to LLVM community: https://github.com/llvm/llvm-project/issues/69952

