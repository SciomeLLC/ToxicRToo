#! /bin/sh

#### CMAKE CONFIGURATION ####

CMAKE_BIN=""

if test -z "$CMAKE_BIN"; then
  # Look for a cmake3 binary in the current path
  CMAKE_BIN=`which cmake3 2>/dev/null`
fi

if test -z "$CMAKE_BIN"; then
  # Look for a cmake binary in the current path
  CMAKE_BIN=`which cmake 2>/dev/null`
fi

if test -z "$CMAKE_BIN"; then
  # Check for a MacOS specific path
  CMAKE_BIN=`which /Applications/CMake.app/Contents/bin/cmake 2>/dev/null`
fi

echo set CMAKE_BIN=$CMAKE_BIN