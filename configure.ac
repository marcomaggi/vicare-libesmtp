dnl @configure_input@
dnl

AC_PREREQ([2.68])
AC_INIT([Vicare/Libesmtp],[0.1d0],[marco.maggi-ipsu@poste.it],
  [vicare-libesmtp],[http://marcomaggi.github.com/vicare.html])
AC_CONFIG_SRCDIR([src])
AC_CONFIG_MACRO_DIR([autotools])
AC_CONFIG_AUX_DIR([autotools])
AC_CANONICAL_BUILD
AC_CANONICAL_HOST
AC_CANONICAL_TARGET
AM_INIT_AUTOMAKE([-Wall foreign dist-xz no-dist-gzip])
AM_MAINTAINER_MODE

VICARE_OPTION_DEBUGGING_MODE
VICARE_OPTION_NAUSICAA([no])

#page
#### compiler

AC_PROG_CC
AC_PROG_CC_C_O

AM_PROG_AR
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET

LT_PREREQ([2.4])
LT_INIT([shared disable-static])

dnl AC_LANG([C])
dnl AC_DEFINE([_GNU_SOURCE],[1],[enable everything GNU])

#page
#### vicare configuration

AC_CHECK_HEADERS([vicare.h],,[AC_MSG_FAILURE([missing vicare.h header],[2])])
AC_CHECK_PROG([VICARE],[vicare],[vicare],[:])

#page
#### libesmtp configuration

AC_CHECK_HEADERS([libesmtp.h],,
  [AC_MSG_FAILURE([missing libesmtp.h header],[2])])
AC_CHECK_LIB([esmtp],[smtp_version],,
  [AC_MSG_FAILURE([missing Libesmtp library],[2])])

VICARE_EXTENSION_LIBRARY_VERSION([libesmtp],[0],[0],[0])

#page
#### symbols

m4_define([VICARE_INCLUDES],[
AC_INCLUDES_DEFAULT
#ifdef HAVE_VICARE_H
#  include <vicare.h>
#endif
#ifdef HAVE_LIBESMTP_H
#  include <libesmtp.h>
#endif
])

dnl Stuff
dnl VICARE_CONSTANT_TESTS([XML_TRUE XML_FALSE])

#page
#### finish

AC_CONFIG_HEADERS([config.h])
AC_CONFIG_FILES([Makefile]
        [src/Makefile]
        [doc/Makefile]
        [tests/Makefile]
        [lib/Makefile]
        [lib/vicare/mail/libesmtp/constants.sls]
        [meta/slackware/doinst.sh])
AC_OUTPUT

### end of file
# Local Variables:
# mode: autoconf
# page-delimiter: "^#page"
# End:
