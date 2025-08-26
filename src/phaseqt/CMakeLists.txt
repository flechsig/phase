# File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseqt/CMakeLists.txt
# Date      : <2025-08-13 15:17:26 flechsig> 
# Time-stamp: <2025-08-19 16:31:43 flechsig> 
# Author    : Flechsig Uwe, uwe.flechsig&#64;psi.&#99;&#104;

#
cmake_minimum_required(VERSION 3.16)
project(phaseqt LANGUAGES CXX)

option(GRIDSIZE "define gridsize" 255)
add_compile_definitions(GRIDSIZE=${GRIDSIZE})

INCLUDE_DIRECTORIES ( ../phase )

message(">>> build for qt6")

find_package(Qt6 REQUIRED COMPONENTS Core Gui Widgets)

qt_standard_project_setup()

qt_add_executable(phaseqt
      main.cpp
      mainwindow.cpp mainwindow.h
    )
    
    qt_generate_deploy_app_script(
       TARGET phaseqt
       OUTPUT_SCRIPT deploy_script
       NO_UNSUPPORTED_PLATFORM_ERROR
     )
     
    target_link_libraries(phaseqt PRIVATE
       Qt6::Core
       Qt6::Gui
       Qt6::Widgets
    )

    install(TARGETS phaseqt
       BUNDLE  DESTINATION .
       RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
       LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    )
    install(SCRIPT ${deploy_script})
