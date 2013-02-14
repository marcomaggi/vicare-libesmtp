## Process this file with automake to produce Makefile.in

EXTRA_DIST	= \
	vicare/mail/libesmtp/constants.sls.in

# These files are hand-written.
nobase_dist_libvicare_DATA = \
	vicare/mail/libesmtp.sls			\
	vicare/mail/libesmtp/unsafe-capi.sls

# These files are created at configuration or make time.
nobase_nodist_libvicare2_DATA = \
	vicare/mail/libesmtp/constants.sls		\
	vicare/mail/libesmtp/features.sls

# if WANT_NAUSICAA
# nobase_dist_libvicare_DATA	+= nausicaa/mail/libesmtp.vicare.sls
# endif

include $(top_srcdir)/meta/makefiles/lib.am

### end of file
