dnl -*- Autoconf -*-

AC_INIT([rcas-web], [0.0.4])
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([gnu color-tests -Wall -Wno-portability foreign])
AM_SILENT_RULES([yes])

dnl We need Rscript to run RCAS jobs
AC_ARG_VAR(RSCRIPT, override location of "Rscript" executable)

dnl We require pkg.m4 (from pkg-config) and guile.m4 (from Guile.)
dnl Make sure they are available.
m4_pattern_forbid([PKG_CHECK_MODULES])
m4_pattern_forbid([GUILE_MODULE_AVAILABLE])

PKG_CHECK_MODULES([GUILE], [guile-2.2 >= 2.1.3])
AC_PATH_PROG([GUILE], [guile])
AC_PATH_PROG([GUILD], [guild])
if test "x$GUILD" = "x"; then
   AC_MSG_ERROR(['guild' binary not found; please check your guile-2.x installation.])
fi

GUILE_MODULE_REQUIRED([json])
GUILE_MODULE_REQUIRED([redis])

dnl xgroup was added in the 1.3.0 release.
AC_ARG_VAR(HAVE_RECENT_GUILE_REDIS)
GUILE_MODULE_CHECK([HAVE_RECENT_GUILE_REDIS], [(redis commands)], [(defined? 'xgroup)], [has recent Guile Redis bindings])

if test "x$HAVE_RECENT_GUILE_REDIS" = "xno"; then
   AC_MSG_ERROR([please check that Guile Redis 1.3.0 or later is available.])
fi


dnl Check for required programmes and store their full path in the
dnl given variables.  The variables are used to substitute
dnl placeholders in the scripts.
AS_IF([test -z "$RSCRIPT"],
      [AC_PATH_PROG([RSCRIPT], [Rscript])],
      [AC_MSG_NOTICE([Using $RSCRIPT as Rscript executable.])])

dnl Ensure that the given R package is avaliable.  Optionally, check
dnl the version number.
AC_DEFUN([AX_TEST_R_PACKAGE], [
    pushdef([PKG],$1)
    pushdef([VERSION],$2)
    AC_MSG_CHECKING([for R package PKG VERSION])

    if test -z "$VERSION"
    then
      TEST=$( R --silent --vanilla -e 'if(is.na(packageDescription("PKG"))) stop("not found")' 2>/dev/null )
    else
      TEST=$( R --silent --vanilla -e 'if(!(packageDescription("PKG")$Version >= "VERSION")) stop("not found")' 2>/dev/null )
    fi
    AS_IF([test $? != 0],
      [AC_MSG_ERROR([R package PKG VERSION could not be found])],
      [AC_MSG_RESULT([OK])])
    popdef([PKG])
    popdef([VERSION])
])

AX_TEST_R_PACKAGE([RCAS], [1.3.4])

guilemoduledir="${datarootdir}/guile/site/${GUILE_EFFECTIVE_VERSION}"
AC_SUBST([guilemoduledir])
AC_SUBST([GUILE_EFFECTIVE_VERSION])

dnl Substitute placeholders to generate these target files
AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([rcas/config.scm])
AC_CONFIG_FILES([rcas.conf.example])
AC_CONFIG_FILES([scripts/rcas-web], [chmod +x scripts/rcas-web])
AC_CONFIG_FILES([pre-inst-env:build-aux/pre-inst-env.in],
  [chmod +x pre-inst-env])

AC_OUTPUT
