## Process this file with automake to produce Makefile.in

vicare_libesmtp_CURRENT	= @vicare_libesmtp_VERSION_INTERFACE_CURRENT@
vicare_libesmtp_REVISION= @vicare_libesmtp_VERSION_INTERFACE_REVISION@
vicare_libesmtp_AGE	= @vicare_libesmtp_VERSION_INTERFACE_AGE@
vicare_libesmtp_VERSION	= $(vicare_libesmtp_CURRENT):$(vicare_libesmtp_REVISION):$(vicare_libesmtp_AGE)

lib_LTLIBRARIES			= libvicare-libesmtp.la
libvicare_libesmtp_la_LDFLAGS	= -version-info $(vicare_libesmtp_VERSION)
libvicare_libesmtp_la_SOURCES	= \
	vicare-libesmtp-version.c		\
	vicare-libesmtp-internals.h

### end of file
