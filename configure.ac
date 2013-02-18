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
#### C language preprocessor symbols from the file "libesmtp.h"

m4_define([VICARE_INCLUDES],[
AC_INCLUDES_DEFAULT
#ifdef HAVE_VICARE_H
#  include <vicare.h>
#endif
#ifdef HAVE_LIBESMTP_H
#  include <libesmtp.h>
#endif
])

dnl enum header_option
VICARE_CONSTANT_TESTS([Hdr_OVERRIDE
    Hdr_PROHIBIT])

dnl protocol progress
VICARE_CONSTANT_TESTS([SMTP_EV_CONNECT
    SMTP_EV_MAILSTATUS
    SMTP_EV_RCPTSTATUS
    SMTP_EV_MESSAGEDATA
    SMTP_EV_MESSAGESENT
    SMTP_EV_DISCONNECT
    SMTP_EV_SYNTAXWARNING])

dnl protocol extension progress
VICARE_CONSTANT_TESTS([SMTP_EV_ETRNSTATUS])

dnl required extensions
VICARE_CONSTANT_TESTS([SMTP_EV_EXTNA_DSN
    SMTP_EV_EXTNA_8BITMIME
    SMTP_EV_EXTNA_STARTTLS
    SMTP_EV_EXTNA_ETRN
    SMTP_EV_EXTNA_CHUNKING
    SMTP_EV_EXTNA_BINARYMIME])

dnl Extensions specific events
VICARE_CONSTANT_TESTS([SMTP_EV_DELIVERBY_EXPIRED])

dnl STARTTLS
VICARE_CONSTANT_TESTS([SMTP_EV_WEAK_CIPHER
    SMTP_EV_STARTTLS_OK
    SMTP_EV_INVALID_PEER_CERTIFICATE
    SMTP_EV_NO_PEER_CERTIFICATE
    SMTP_EV_WRONG_PEER_CERTIFICATE
    SMTP_EV_NO_CLIENT_CERTIFICATE
    SMTP_EV_UNUSABLE_CLIENT_CERTIFICATE
    SMTP_EV_UNUSABLE_CA_LIST])

dnl enum rfc2822_timeouts
VICARE_CONSTANT_TESTS([Timeout_GREETING
    Timeout_ENVELOPE
    Timeout_DATA
    Timeout_TRANSFER
    Timeout_DATA2
    Timeout_OVERRIDE_RFC2822_MINIMUM])

dnl enum notify_flags
VICARE_CONSTANT_TESTS([Notify_NOTSET
    Notify_NEVER
    Notify_SUCCESS
    Notify_FAILURE
    Notify_DELAY])

dnl enum e8bitmime_body
VICARE_CONSTANT_TESTS([E8bitmime_NOTSET
    E8bitmime_7BIT
    E8bitmime_8BITMIME
    E8bitmime_BINARYMIME])

dnl enum by_mode
VICARE_CONSTANT_TESTS([By_NOTSET
    By_NOTIFY
    By_RETURN])

dnl enum starttls_option
VICARE_CONSTANT_TESTS([Starttls_DISABLED
    Starttls_ENABLED
    Starttls_REQUIRED])

dnl
VICARE_CONSTANT_TESTS([SMTP_ERR_NOTHING_TO_DO
    SMTP_ERR_DROPPED_CONNECTION
    SMTP_ERR_INVALID_RESPONSE_SYNTAX
    SMTP_ERR_STATUS_MISMATCH
    SMTP_ERR_INVALID_RESPONSE_STATUS
    SMTP_ERR_INVAL
    SMTP_ERR_EXTENSION_NOT_AVAILABLE])

dnl enum ret_flags
VICARE_CONSTANT_TESTS([Ret_NOTSET
    Ret_FULL
    Ret_HDRS])

dnl Deprecated - these will be removed in a future release
VICARE_CONSTANT_TESTS([SMTP_ERR_HOST_NOT_FOUND
    SMTP_ERR_NO_ADDRESS
    SMTP_ERR_NO_RECOVERY
    SMTP_ERR_TRY_AGAIN])

dnl libESMTP versions of some getaddrinfo error numbers
VICARE_CONSTANT_TESTS([SMTP_ERR_EAI_AGAIN
    SMTP_ERR_EAI_FAIL
    SMTP_ERR_EAI_MEMORY
    SMTP_ERR_EAI_ADDRFAMILY
    SMTP_ERR_EAI_NODATA
    SMTP_ERR_EAI_FAMILY
    SMTP_ERR_EAI_BADFLAGS
    SMTP_ERR_EAI_NONAME
    SMTP_ERR_EAI_SERVICE
    SMTP_ERR_EAI_SOCKTYPE
    SMTP_ERR_UNTERMINATED_RESPONSE
    SMTP_ERR_CLIENT_ERROR])

dnl Protocol monitor callback.  Values for writing
VICARE_CONSTANT_TESTS([SMTP_CB_READING
    SMTP_CB_WRITING
    SMTP_CB_HEADERS])

#page
#### C language functions from the header file "libesmtp.h"

AC_CHECK_FUNCS([smtp_version smtp_create_session smtp_add_message smtp_enumerate_messages smtp_set_server smtp_set_hostname smtp_set_reverse_path smtp_add_recipient smtp_enumerate_recipients smtp_set_header smtp_set_header_option smtp_set_resent_headers smtp_set_messagecb smtp_set_eventcb smtp_set_monitorcb smtp_start_session smtp_destroy_session smtp_message_transfer_status smtp_reverse_path_status smtp_message_reset_status smtp_recipient_status smtp_recipient_check_complete smtp_recipient_reset_status smtp_errno smtp_strerror smtp_set_application_data smtp_get_application_data smtp_message_set_application_data smtp_message_get_application_data smtp_recipient_set_application_data smtp_recipient_get_application_data smtp_option_require_all_recipients smtp_auth_set_context smtp_set_timeout smtp_dsn_set_ret smtp_dsn_set_envid smtp_dsn_set_notify smtp_dsn_set_orcpt smtp_size_set_estimate smtp_8bitmime_set_body smtp_deliverby_set_mode smtp_starttls_enable smtp_starttls_set_ctx smtp_starttls_set_password_cb smtp_etrn_add_node smtp_etrn_enumerate_nodes smtp_etrn_node_status smtp_etrn_set_application_data smtp_etrn_get_application_data])

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
