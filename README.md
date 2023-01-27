# mpasDual2esmf

Based on MPAS-Tools/mesh_tools/mpas2esmf:
https://github.com/MPAS-Dev/MPAS-Tools/tree/5cc1b97ef1e71fa7986d2ca9d7528207ab3ce3d5/mesh_tools/mpas2esmf @ 299f56f

This tool generates ESMF files from an MPAS Dual mesh file.

To build, ensure `nc-config` is in your $PATH and call `make`.

By default, the ESMF and SCRIP NetCDF files created are 64BIT offset format.
