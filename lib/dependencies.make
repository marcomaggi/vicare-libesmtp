## dependencies.make --
#
# Automatically built.

EXTRA_DIST +=  \
	lib/vicare/email/libesmtp/constants.vicare.sls.in

lib/vicare/email/libesmtp.fasl: \
		lib/vicare/email/libesmtp.vicare.sls \
		lib/vicare/email/libesmtp/unsafe-capi.fasl \
		lib/vicare/email/libesmtp/constants.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_email_libesmtp_fasldir = $(bundledlibsdir)/vicare/email
lib_vicare_email_libesmtp_vicare_slsdir  = $(bundledlibsdir)/vicare/email
nodist_lib_vicare_email_libesmtp_fasl_DATA = lib/vicare/email/libesmtp.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_email_libesmtp_vicare_sls_DATA = lib/vicare/email/libesmtp.vicare.sls
endif
EXTRA_DIST += lib/vicare/email/libesmtp.vicare.sls
CLEANFILES += lib/vicare/email/libesmtp.fasl

lib/vicare/email/libesmtp/unsafe-capi.fasl: \
		lib/vicare/email/libesmtp/unsafe-capi.vicare.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_email_libesmtp_unsafe_capi_fasldir = $(bundledlibsdir)/vicare/email/libesmtp
lib_vicare_email_libesmtp_unsafe_capi_vicare_slsdir  = $(bundledlibsdir)/vicare/email/libesmtp
nodist_lib_vicare_email_libesmtp_unsafe_capi_fasl_DATA = lib/vicare/email/libesmtp/unsafe-capi.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_email_libesmtp_unsafe_capi_vicare_sls_DATA = lib/vicare/email/libesmtp/unsafe-capi.vicare.sls
endif
EXTRA_DIST += lib/vicare/email/libesmtp/unsafe-capi.vicare.sls
CLEANFILES += lib/vicare/email/libesmtp/unsafe-capi.fasl

lib/vicare/email/libesmtp/constants.fasl: \
		lib/vicare/email/libesmtp/constants.vicare.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_email_libesmtp_constants_fasldir = $(bundledlibsdir)/vicare/email/libesmtp
lib_vicare_email_libesmtp_constants_vicare_slsdir  = $(bundledlibsdir)/vicare/email/libesmtp
nodist_lib_vicare_email_libesmtp_constants_fasl_DATA = lib/vicare/email/libesmtp/constants.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_email_libesmtp_constants_vicare_sls_DATA = lib/vicare/email/libesmtp/constants.vicare.sls
endif
CLEANFILES += lib/vicare/email/libesmtp/constants.fasl

lib/vicare/email/libesmtp/features.fasl: \
		lib/vicare/email/libesmtp/features.vicare.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_email_libesmtp_features_fasldir = $(bundledlibsdir)/vicare/email/libesmtp
lib_vicare_email_libesmtp_features_vicare_slsdir  = $(bundledlibsdir)/vicare/email/libesmtp
nodist_lib_vicare_email_libesmtp_features_fasl_DATA = lib/vicare/email/libesmtp/features.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_email_libesmtp_features_vicare_sls_DATA = lib/vicare/email/libesmtp/features.vicare.sls
endif
CLEANFILES += lib/vicare/email/libesmtp/features.fasl


### end of file
# Local Variables:
# mode: makefile-automake
# End:
