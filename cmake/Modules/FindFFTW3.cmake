# FindFFTW3.cmake
# Sucht die FFTW3-Bibliothek (double precision)

find_path(FFTW3_INCLUDE_DIR
    NAMES fftw3.h
    PATHS
        /usr/include
        /usr/local/include
        /opt/homebrew/include
        $ENV{FFTW3_ROOT}/include
        ${FFTW3_ROOT}/include
)

find_library(FFTW3_LIBRARY
    NAMES fftw3
    PATHS
        /usr/lib
        /usr/local/lib
        /opt/homebrew/lib
        $ENV{FFTW3_ROOT}/lib
        ${FFTW3_ROOT}/lib
)

set(FFTW3_LIBRARIES ${FFTW3_LIBRARY})
set(FFTW3_INCLUDE_DIRS ${FFTW3_INCLUDE_DIR})

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(FFTW3 DEFAULT_MSG FFTW3_LIBRARY FFTW3_INCLUDE_DIR)

mark_as_advanced(FFTW3_INCLUDE_DIR FFTW3_LIBRARY)
