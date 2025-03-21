# Configure Script for ToxicRToo
# Modified 3/20/2025

AC_INIT([ToxicRToo], 0.99.5.0)

## Set R_HOME, respecting an environment variable if set
: ${R_HOME=$(R RHOME)}
if test -z "${R_HOME}"; then
   AC_MSG_ERROR([Could not determine R_HOME.])
fi

CXX=`"${R_HOME}/bin/R" CMD config CXX`
if test -z "${CXX}"; then
    AC_MSG_ERROR([No C++ compiler available])
fi
CXXFLAGS=`"${R_HOME}/bin/R" CMD config CXXFLAGS`
CPPFLAGS=`"${R_HOME}/bin/R" CMD config CPPFLAGS`
CXX11FLAGS=`"${R_HOME}/bin/R" CMD config CXX11FLAGS`
AC_LANG(C++)
AC_REQUIRE_CPP
AC_PROG_CXX

## Default to build from source
need_to_build="yes"

## But: Can we use pkg-config?
AC_PATH_PROG(have_pkg_config, pkg-config, no)
## If yes, also check for whether pkg-config knows nlopt
if test x"${have_pkg_config}" != x"no"; then
    AC_MSG_CHECKING([if pkg-config knows NLopt])
    if pkg-config --exists nlopt; then
        AC_MSG_RESULT([yes])
        ## Since nlopt has been found, test for minimal version requirement
        AC_MSG_CHECKING([for pkg-config checking NLopt version])
        if pkg-config --atleast-version=2.7.0 nlopt; then
            AC_MSG_RESULT([>= 2.7.0])
            nlopt_include=$(pkg-config --cflags nlopt)
            nlopt_libs=$(pkg-config --libs nlopt)
            ## copy headers to inst/include for reverse LinkingTo dependencies
            headers_location=$(pkg-config --variable=includedir nlopt)
            need_to_build="no"
        else
            AC_MSG_RESULT([insufficient: NLopt 2.7.0 or later is preferred.])
        fi
    else
        AC_MSG_RESULT([no])
    fi
fi

## So do we need to build
if test x"${need_to_build}" != x"no"; then
    AC_PATH_PROG(CMAKE_BIN, cmake, no)
    if test x"${CMAKE_BIN}" = x"no"; then
        . tools/cmake_config.sh
        # Check for CMake (which should have been installed via installr in R)
        AC_PATH_PROG([CMAKE_BIN], [cmake])
        if test -z "$CMAKE_BIN"; then
            AC_MSG_ERROR([CMake not found. Please install it using installr::install.CMake.])
        fi
        AC_MSG_RESULT([Found CMake at $CMAKE_BIN])
    fi
    AC_MSG_RESULT([using cmake via $CMAKE_BIN])
    ## 'uname -m' on M1 give x86_64 which is ... not helping
    machine=`"${R_HOME}/bin/Rscript" -e 'cat(Sys.info()[["machine"]])'`
    AC_MSG_RESULT([using NLopt via local cmake build on ${machine}])
    tools/make_nlopt.sh "$CMAKE_BIN"
    ## cmake_call.sh installs into nlopt/include, nlopt/lib
    nlopt_include="-Inlopt/include"
    nlopt_libs="nlopt/lib/libnlopt.a"
    ## copy headers to inst/include for reverse LinkingTo dependencies
    headers_location="src/nlopt/include"
fi

## does the actual copy of the headers into inst/include
AC_MSG_RESULT([Copying headers found in ${headers_location} into inst/include])
cp ${headers_location}/nlopt.* inst/include/

# Check for GSL
AC_PATH_PROG([GSL_CONFIG], [gsl-config])
if test -n "$GSL_CONFIG"; then
    AC_MSG_RESULT([GSL found])
    GSL_CPPFLAGS=`$GSL_CONFIG --cflags`
    GSL_LIBS=`$GSL_CONFIG --libs`
else
    AC_MSG_ERROR([
        gsl-config not found, is GSL installed?
        To install GSL in Ubuntu type: 
        sudo apt install gsl 
        To install GSL in Fedora type:
        sudo yum -y install gsl
        To install GSL on macOS using Homebrew type: 
        brew install gsl
    ])
fi

SUBDIR_SOURCES="$(cd src/ && ls {code_base,polyK}/*.cpp | tr '\n' ' ')"
SRC_SOURCES="$( cd src/ && ls *.cpp | tr '\n' ' ')"


## Now use all the values
AC_SUBST(SRC_SOURCES)
AC_SUBST(SUBDIR_SOURCES)
AC_SUBST([NLOPT_CPPFLAGS],["$nlopt_include"])
AC_SUBST([NLOPT_LIBS],["$nlopt_libs"])
AC_MSG_RESULT([Found nlopt installation at $NLOPT_INCLUDE and $NLOPT_LIBDIR])

AC_SUBST(OSFLAG)
AC_SUBST(GSL_CPPFLAGS)
AC_SUBST(GSL_LIBS)
AC_SUBST(CMAKE_BIN)

AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT