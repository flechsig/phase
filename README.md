# PHASE

PHASE is a physical optics and ray-tracing package.

## Build System

PHASE can be built with **CMake** (described below). The traditional autotools
build in `src/` (see `src/README.md`) is still available and unchanged.

## Requirements

- CMake >= 3.20
- C compiler (e.g. clang/gcc)
- C++ compiler with C++17 support
- Fortran compiler
  - Recommended: `gfortran`
  - `flang` is known to be problematic for legacy Fortran sources used by `phaseqt`/`phasesrv`

Dependency requirements depend on enabled modules:

- Always for current default build:
  - HDF5 (C + HL)
- For `phaseqt` / `phasesrv` / `opti`:
  - FFTW3
- For `phaseqt`:
  - Qt (Qt6 preferred, Qt5 supported)
  - Qwt >= 6.2 (if `USE_QWT=ON`); Qwt 6.1.x as shipped with Ubuntu 24.04 is too old
    (the sources use `QWT_OVERRIDE`)
- For `opti`:
  - ROOT with Minuit

## Example: Ubuntu 24.04 (Qt5)

Ubuntu 24.04 ships Qwt 6.1.4, which is too old for `phaseqt`. Build Qwt >= 6.2
(tested with 6.3.0, installs to `/usr/local/qwt-6.3.0`, where `FindQwt.cmake` looks):

```bash
sudo apt install cmake gfortran g++ libhdf5-dev libfftw3-dev \
  qtbase5-dev libqt5svg5-dev qttools5-dev libgl1-mesa-dev
# Qwt 6.3.0 from https://qwt.sourceforge.io (do not install libqwt-qt5-dev)
qmake qwt.pro && make -j && sudo make install
cmake -S . -B build -DUSE_QT6=OFF -DCMAKE_Fortran_COMPILER=gfortran
cmake --build build -j
```

Without Qwt, build the non-GUI parts only with `-DBUILD_PHASEQT=OFF`.

## Quick Start (Full Build)

```bash
cmake -S . -B build \
  -DCMAKE_Fortran_COMPILER=gfortran
cmake --build build -j
```

## Install

```bash
cmake --install build
```

Custom install prefix:

```bash
cmake -S . -B build \
  -DCMAKE_Fortran_COMPILER=gfortran \
  -DCMAKE_INSTALL_PREFIX=/your/prefix
cmake --build build -j
cmake --install build
```

## Building a single module

`hdf5tools`, `phasesrv` and `fkoe` can still be configured on their own, as before:

```bash
cmake -S src/phasesrv -B build-phasesrv -DCMAKE_Fortran_COMPILER=gfortran
```

`phaseqt` must be built from the top-level project (`-DBUILD_PHASEQT=ON`).

## Main Build Options

```text
USE_MPI         (default: OFF)
USE_DEBUG       (default: OFF)
USE_OPENMP      (default: OFF)
USE_QT6         (default: ON)
USE_QWT         (default: ON)

BUILD_PHASEQT   (default: ON)
BUILD_PHASESRV  (default: ON)
BUILD_HDF5TOOLS (default: ON)
BUILD_FKOE      (default: ON)
BUILD_OPTI      (default: OFF)
BUILD_BASELIB   (default: OFF)
BUILD_PHASELIB  (default: OFF)

GRIDSIZE        (default: 1)
```

## Typical Build Variants

### 1) Full desktop/headless toolchain (recommended)

```bash
cmake -S . -B build \
  -DCMAKE_Fortran_COMPILER=gfortran \
  -DBUILD_OPTI=ON
cmake --build build -j
```

### 2) Headless/minimal tools only

Builds `hdf5tools` + `fkoe`, skips GUI/server/opti:

```bash
cmake -S . -B build-min \
  -DBUILD_PHASEQT=OFF \
  -DBUILD_PHASESRV=OFF \
  -DBUILD_OPTI=OFF
cmake --build build-min -j
```

### 3) Build a single target

```bash
cmake -S . -B build \
  -DCMAKE_Fortran_COMPILER=gfortran \
  -DBUILD_OPTI=ON
cmake --build build --target phaseqt -j
cmake --build build --target phasesrv -j
cmake --build build --target phaseopti -j
```

Other useful targets include:

- `txtlog2phase_hdf5`
- `add_phaseu_2h5`
- `wave2phase_hdf5`
- `genesis2phase_hdf5`
- `wave2genesis_hdf5`
- `genesis_hdf5_2wave`
- `phase_hdf5_2wave`
- `fkoe`
- `opti_root_example`

## Notes

- If CMake picks `flang` automatically and `phaseqt`/`phasesrv` fail, reconfigure with:

```bash
cmake -S . -B build -DCMAKE_Fortran_COMPILER=gfortran
```

- If Qt6 is installed in your home directory (for example on Ubuntu), pass its prefix explicitly:

```bash
cmake -S . -B build \
  -DCMAKE_Fortran_COMPILER=gfortran \
  -DPHASE_QT_PREFIX=$HOME/Qt/6.7.2/gcc_64
```

  Alternative (path to config directory):

```bash
cmake -S . -B build \
  -DCMAKE_Fortran_COMPILER=gfortran \
  -DQt6_DIR=$HOME/Qt/6.7.2/gcc_64/lib/cmake/Qt6
```

- Existing legacy source warnings may still appear during compilation; this is expected for now.

## Linux Runtime Note (Qt)

If `phaseqt` fails with a message like
`libQt6PrintSupport.so.6: cannot open shared object file`, either:

1. configure with `-DPHASE_QT_PREFIX=/path/to/Qt/<version>/<kit>` and reinstall (recommended), or
2. set runtime lookup path manually:

```bash
export LD_LIBRARY_PATH=/path/to/Qt/<version>/<kit>/lib:$LD_LIBRARY_PATH
```
