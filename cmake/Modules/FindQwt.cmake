# FindQwt.cmake
# Sucht Qwt unter Linux und macOS (Framework)
#
# Setzt QWT_INCLUDE_DIRS, QWT_LIBRARIES und QWT_FOUND

find_path(QWT_INCLUDE_DIR
    NAMES qwt.h
    PATHS
        /usr/include
        /usr/local/include
        /usr/local/qwt-6.3.0/include
        /usr/local/qwt-6.3.0/lib/qwt.framework/Headers
        $ENV{QWT_ROOT}/include
        $ENV{QWT_ROOT}/lib/qwt.framework/Headers
        ${QWT_ROOT}/include
        ${QWT_ROOT}/lib/qwt.framework/Headers
    PATH_SUFFIXES
        qwt qwt-qt5 qwt6   # Debian/Ubuntu install headers in /usr/include/qwt
)

if(APPLE)
    # Suche nach Qwt als Framework
    find_library(QWT_LIBRARY
        NAMES qwt
        PATHS
            /Library/Frameworks
            /usr/local/qwt-6.3.0/lib
            /usr/local/qwt-6.3.0/lib/qwt.framework
            ${QWT_ROOT}/lib
        PATH_SUFFIXES
            Qwt.framework
            qwt.framework
    )
else()
    # Linux: Suche nach libqwt
    find_library(QWT_LIBRARY
        NAMES qwt qwt-qt5
        PATHS
            /usr/lib
            /usr/local/lib
            /usr/local/qwt-6.3.0/lib
            ${QWT_ROOT}/lib
    )
endif()

set(QWT_INCLUDE_DIRS ${QWT_INCLUDE_DIR})
set(QWT_LIBRARIES ${QWT_LIBRARY})

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(Qwt DEFAULT_MSG QWT_LIBRARY QWT_INCLUDE_DIR)

mark_as_advanced(QWT_INCLUDE_DIR QWT_LIBRARY)
