fpm - Fortran Package Manager
-----------------------------

SLICOT provides experimental support for the Fortran Package Manager tool.
By adding
```
[dependencies]
slicot = { git = "https://github.com/SLICOT/SLICOT-Reference", branch = "main" }
```
to your project, SLICOT can be used. Afterwards, just type
```shell
fpm build
```
to build your project with SLICOT support.
