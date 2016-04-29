AC_INIT(ocplib-concur,0.1)
CONFIGURE_ARGS=$*

AC_COPYRIGHT(Copyright 2016 OcamlPro SAS)
AC_CONFIG_MACRO_DIR([m4])

AC_ARG_WITH([typerex],[], [  
  OBUILD_ROOTDIR=$withval
 ], [
  OBUILD_ROOTDIR=.
  ])
OBUILD_DSTDIR=$OBUILD_ROOTDIR/_obuild
AC_SUBST(OBUILD_ROOTDIR)
AC_SUBST(OBUILD_DSTDIR)
  
AC_ARG_ENABLE(version-check,
 [  --disable-version-check do not check OCaml version],
 [VERSION_CHECK="$enableval"],
 [VERSION_CHECK="yes"])

AC_PROG_CC

AC_PROG_OCAML

if test "$OCAMLC" = "no"; then
   AC_MSG_ERROR([You must install the OCaml compiler])
fi

if test "$OCAMLVERSION" = ""; then
   AC_MSG_ERROR([No OCaml version. ocamlc -version is not parsable.])
fi

# Check that OCaml version is greater or equal to 4.00.0
if test "$VERSION_CHECK" = "yes" ; then
  AX_SPLIT_VERSION(OCAMLVERSION)
  OCAML_MAJOR_VERSION=$AX_MAJOR_VERSION
  OCAML_MINOR_VERSION=$AX_MINOR_VERSION
  OCAML_POINT_VERSION=$AX_POINT_VERSION

  AX_COMPARE_VERSION( [$OCAMLVERSION], [lt], [3.12.1],
     AC_MSG_ERROR([Your version of OCaml: $OCAMLVERSION is not supported]))
fi

AC_PROG_FINDLIB
if test "$OCAMLFIND" = "no"; then
   AC_MSG_ERROR([You must install OCaml package 'ocamlfind'])
fi

AC_CHECK_OCAML_PKG([ocplib-endian])
if test "$OCAML_PKG_ocplib_endian" = "no"; then
   AC_MSG_ERROR([Please install OCaml package 'ocplib-endian'.])
fi

AC_CHECK_OCAML_PKG([lwt])
AC_ARG_ENABLE(lwt,  [  --disable-lwt           to disable lwt],
                    [OCAML_PKG_lwt=no],[])

AC_CHECK_OCAML_PKG([async])
AC_ARG_ENABLE(async,[  --disable-async         to disable async],
                       [OCAML_PKG_async=no],[])
CONCUR_TARGETS=""

if test "$OCAML_PKG_lwt" = "no"; then
   AC_MSG_ERROR([Please install OCaml package 'lwt'.])
else
   LWT_ENABLED=true
   CONCUR_TARGETS="${CONCUR_TARGETS} lwt"
fi

if test "$OCAML_PKG_async" = "no"; then
   ASYNC_ENABLED=false
else
   CONCUR_TARGETS="${CONCUR_TARGETS} async"
   ASYNC_ENABLED=true
fi

AC_CHECK_PROG([OCPBUILD],ocp-build,yes,no)
 if test "$OCAML_PKG_ocp_build" = "no"; then
   AC_MSG_ERROR([Please install OCaml package 'ocp-build'.])
 fi

if test "x$prefix" = "xNONE"; then
#  prefix=$ac_default_prefix
  prefix="`eval dirname $OCAMLLIB`"
  base="`eval basename $prefix`"
  if test "x$base" = "xlib"; then
    prefix="`eval dirname $prefix`"
  fi
fi

test "x$exec_prefix" = xNONE && exec_prefix='${prefix}'

libdir=${libdir}/${PACKAGE_NAME}

bindir="`eval echo ${bindir}`"
bindir="`eval echo ${bindir}`"
mandir="`eval echo ${mandir}`"
mandir="`eval echo ${mandir}`"
libdir="`eval echo ${libdir}`"
libdir="`eval echo ${libdir}`"
datarootdir="`eval echo ${datarootdir}`"
datarootdir="`eval echo ${datarootdir}`"
datadir=${datarootdir}/${PACKAGE_NAME}

AC_SUBST(OCAML_MAJOR_VERSION)
AC_SUBST(OCAML_MINOR_VERSION)
AC_SUBST(OCAML_POINT_VERSION)
AC_SUBST(CONFIGURE_ARGS)
AC_SUBST(VERSION)
AC_SUBST(ASYNC_ENABLED)
AC_SUBST(LWT_ENABLED)
AC_SUBST(CONCUR_TARGETS)

AC_CONFIG_FILES(
  Makefile.config 
  config.ocpgen
)
AC_OUTPUT

echo
echo Executables will be installed in ${bindir}
echo Libraries will be installed in ${libdir}
echo Data files will be installed in ${datadir}
echo Manual pages will be installed in ${mandir}
