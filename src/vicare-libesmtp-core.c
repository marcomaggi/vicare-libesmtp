/*
  Part of: Vicare/Libesmtp
  Contents: Libesmtp for Vicare
  Date: Thu Feb 14, 2013

  Abstract



  Copyright (C) 2013 Marco Maggi <marco.maggi-ipsu@poste.it>

  This program is  free software: you can redistribute  it and/or modify
  it under the  terms of the GNU General Public  License as published by
  the Free Software Foundation, either  version 3 of the License, or (at
  your option) any later version.

  This program  is distributed in the  hope that it will  be useful, but
  WITHOUT   ANY  WARRANTY;   without  even   the  implied   warranty  of
  MERCHANTABILITY  or FITNESS  FOR A  PARTICULAR PURPOSE.   See  the GNU
  General Public License for more details.

  You  should have received  a copy  of the  GNU General  Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/


/** --------------------------------------------------------------------
 ** Headers.
 ** ----------------------------------------------------------------- */

#include "vicare-libesmtp-internals.h"



/** --------------------------------------------------------------------
 ** Data structures conversion.
 ** ----------------------------------------------------------------- */

static void
smtp_status_to_scheme_struct (ikpcb * pcb, ikptr s_status, const smtp_status_t * status)
{
  /* fprintf(stderr, "%s: converting status %p\n", __func__, (void*)status); */
  pcb->root0 = &s_status;
  {
    /* SMTP protocol status code */
    IK_ASS(IK_LIBESMTP_STATUS_CODE(s_status),
	   ika_integer_from_int(pcb, status->code));
    /* Text from the server */
    if (status->text) {
      IK_ASS(IK_LIBESMTP_STATUS_TEXT(s_status),
	     ika_bytevector_from_cstring(pcb, status->text));
    } else {
      IK_LIBESMTP_STATUS_TEXT(s_status) = IK_FALSE;
    }
    /* RFC 2034 enhanced status code triplet */
    IK_ASS(IK_LIBESMTP_STATUS_ENH_CLASS(s_status),
	   ika_integer_from_int(pcb, status->enh_class));
    IK_ASS(IK_LIBESMTP_STATUS_ENH_SUBJECT(s_status),
	   ika_integer_from_int(pcb, status->enh_subject));
    IK_ASS(IK_LIBESMTP_STATUS_ENH_DETAIL(s_status),
	   ika_integer_from_int(pcb, status->enh_detail));
  }
  pcb->root0 = NULL;
}


/** --------------------------------------------------------------------
 ** Version functions.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_smtp_version (ikpcb * pcb)
{
#ifdef HAVE_SMTP_VERSION
  /* We expect a short string like "1.0.6". */
#undef  VERSION_LEN
#define VERSION_LEN	16
  char	version[VERSION_LEN];
  int	rv;
  rv = smtp_version(version, VERSION_LEN, 0);
  return (rv)? ika_bytevector_from_cstring(pcb, version) : IK_FALSE;
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Library errors.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_smtp_errno (ikpcb * pcb)
{
#ifdef HAVE_SMTP_ERRNO
  return ika_integer_from_int(pcb, smtp_errno());
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_smtp_strerror (ikptr s_errno, ikpcb * pcb)
{
#ifdef HAVE_SMTP_STRERROR
  int		err = ik_integer_to_int(s_errno);
#undef  BUFFER_SIZE
#define BUFFER_SIZE	256
  char		ptr[BUFFER_SIZE];
  const char *	rv;
  rv = smtp_strerror(err, ptr, BUFFER_SIZE);
  return (rv)? ika_bytevector_from_cstring(pcb, ptr) : IK_FALSE;
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Session management.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_smtp_create_session (ikpcb * pcb)
{
#ifdef HAVE_SMTP_CREATE_SESSION
  smtp_session_t	sex;
  sex = smtp_create_session();
  return (sex)? ika_pointer_alloc(pcb, (long)sex) : IK_FALSE;
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_smtp_destroy_session (ikptr s_session, ikpcb * pcb)
{
#ifdef HAVE_SMTP_DESTROY_SESSION
  smtp_session_t	sex = IK_LIBESMTP_SESSION(s_session);
  if (sex) {
    int		rv;
    ikptr	sk;
    /* Hypothesis: at  some future time,  closing the session  may cause
       invocation   of  Scheme   callbacks,  so   we  save   the  Scheme
       continuation.  (Marco Maggi; Thu Feb 14, 2013) */
    sk = ik_enter_c_function(pcb);
    {
#ifdef HAVE_SMTP_GSASL_SET_CONTEXT
      smtp_gsasl_set_context(sex, NULL);
#endif
#ifdef HAVE_SMTP_AUTH_SET_CONTEXT
      smtp_auth_set_context (sex, NULL);
#endif
      rv = smtp_destroy_session(sex);
    }
    ik_leave_c_function(pcb, sk);
    /* *NOTE* We cannot set the pointer field of S_SESSION to NULL here:
       we are assuming  that a callback may be called  by the destructor
       function; this  means the garbage  collectory may have  moved the
       struct fields somewhere else.  We cannot track this movement here
       because we cannot occupy the  "pcb->root" fields while a callback
       is called. */
    return IK_BOOLEAN_FROM_INT(rv);
  } else
    return IK_TRUE;
#else
  feature_failure(__func__);
#endif
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_smtp_set_hostname (ikptr s_session, ikptr s_local_hostname, ikpcb * pcb)
{
#ifdef HAVE_SMTP_SET_HOSTNAME
  smtp_session_t	sex   = IK_LIBESMTP_SESSION(s_session);
  char *	hname = IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK_OR_FALSE(s_local_hostname);
  int		rv;
  rv = smtp_set_hostname(sex, hname);
  return IK_BOOLEAN_FROM_INT(rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_smtp_set_server (ikptr s_session, ikptr s_remote_server, ikpcb * pcb)
{
#ifdef HAVE_SMTP_SET_SERVER
  smtp_session_t	sex   = IK_LIBESMTP_SESSION(s_session);
  char *	server = IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_remote_server);
  int		rv;
  rv = smtp_set_server(sex, server);
  return IK_BOOLEAN_FROM_INT(rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_smtp_set_timeout (ikptr s_session, ikptr s_which, ikptr s_value, ikpcb * pcb)
{
#ifdef HAVE_SMTP_SET_TIMEOUT
  smtp_session_t	sex	= IK_LIBESMTP_SESSION(s_session);
  int			which	= ik_integer_to_int(s_which);
  long			value	= ik_integer_to_long(s_value);
  int			rv;
  rv = smtp_set_timeout(sex, which, value);
  return IK_BOOLEAN_FROM_INT(rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_smtp_set_eventcb (ikptr s_session, ikptr s_callback, ikpcb * pcb)
{
#ifdef HAVE_SMTP_SET_EVENTCB
  smtp_session_t	sex = IK_LIBESMTP_SESSION(s_session);
  smtp_eventcb_t	cb  = IK_POINTER_DATA_VOIDP(s_callback);
  int			rv;
  rv = smtp_set_eventcb(sex, cb, NULL);
  return IK_BOOLEAN_FROM_INT(rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_smtp_set_monitorcb (ikptr s_session, ikptr s_callback, ikptr s_headers, ikpcb * pcb)
{
#ifdef HAVE_SMTP_SET_MONITORCB
  smtp_session_t	sex	= IK_LIBESMTP_SESSION(s_session);
  smtp_monitorcb_t	cb	= IK_POINTER_DATA_VOIDP(s_callback);
  int			headers	= IK_BOOLEAN_TO_INT(s_headers);
  int			rv;
  rv = smtp_set_monitorcb(sex, cb, NULL, headers);
  return IK_BOOLEAN_FROM_INT(rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_smtp_start_session (ikptr s_session, ikpcb * pcb)
{
#ifdef HAVE_SMTP_START_SESSION
  smtp_session_t	sex = IK_LIBESMTP_SESSION(s_session);
  ikptr			sk;
  int			rv;
  sk = ik_enter_c_function(pcb);
  {
    rv = smtp_start_session(sex);
  }
  ik_leave_c_function(pcb, sk);
  return IK_BOOLEAN_FROM_INT(rv);
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Message management.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_smtp_add_message (ikptr s_session, ikpcb * pcb)
{
#ifdef HAVE_SMTP_ADD_MESSAGE
  smtp_session_t	sex = IK_LIBESMTP_SESSION(s_session);
  smtp_message_t	mes;
  mes = smtp_add_message(sex);
  return (mes)? ika_pointer_alloc(pcb, (long)mes) : IK_FALSE;
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_smtp_enumerate_messages (ikptr s_session, ikptr s_callback, ikpcb * pcb)
{
#ifdef HAVE_SMTP_ENUMERATE_MESSAGES
  smtp_session_t		sex = IK_LIBESMTP_SESSION(s_session);
  smtp_enumerate_messagecb_t	cb  = IK_POINTER_DATA_VOIDP(s_callback);
  ikptr				sk;
  sk = ik_enter_c_function(pcb);
  {
    smtp_enumerate_messages(sex, cb, NULL);
  }
  ik_leave_c_function(pcb, sk);
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_smtp_set_reverse_path (ikptr s_message, ikptr s_mailbox, ikpcb * pcb)
{
#ifdef HAVE_SMTP_SET_REVERSE_PATH
  smtp_message_t	msg = IK_LIBESMTP_MESSAGE(s_message);
  char *	mailbox = IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK_OR_FALSE(s_mailbox);
  int		rv;
  rv = smtp_set_reverse_path(msg, mailbox);
  return IK_BOOLEAN_FROM_INT(rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_smtp_set_messagecb (ikptr s_message, ikptr s_callback, ikpcb * pcb)
{
#ifdef HAVE_SMTP_SET_MESSAGECB
  smtp_message_t	msg = IK_LIBESMTP_MESSAGE(s_message);
  smtp_messagecb_t	cb  = IK_POINTER_DATA_VOIDP(s_callback);
  int			rv;
  /* fprintf(stderr, "%s: setting message cb: %p\n", __func__, (void *)cb); */
  rv = smtp_set_messagecb(msg, cb, NULL);
  return IK_BOOLEAN_FROM_INT(rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_smtp_set_message_fp (ikptr s_message, ikptr s_file_pointer, ikpcb * pcb)
{
#ifdef HAVE_SMTP_SET_MESSAGECB
  smtp_message_t	msg = IK_LIBESMTP_MESSAGE(s_message);
  FILE *		fp  = IK_POINTER_DATA_VOIDP(s_file_pointer);
  int			rv;
  /* "smtp_set_message_fp()" is a C preprocessor macro!!! */
  rv = smtp_set_message_fp(msg, fp);
  return IK_BOOLEAN_FROM_INT(rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_smtp_set_message_str (ikptr s_message, ikptr s_string, ikpcb * pcb)
{
#ifdef HAVE_SMTP_SET_MESSAGECB
  smtp_message_t	msg = IK_LIBESMTP_MESSAGE(s_message);
  char *		str = IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_string);
  int			rv;
  /* fprintf(stderr, "%s: message string: \"%s\"\n", __func__, str); */
  /* "smtp_set_message_str()" is a C preprocessor macro!!! */
  rv = smtp_set_message_str(msg, str);
  return IK_BOOLEAN_FROM_INT(rv);
#else
  feature_failure(__func__);
#endif
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_smtp_message_transfer_status (ikptr s_message, ikptr s_status, ikpcb * pcb)
{
#ifdef HAVE_SMTP_MESSAGE_TRANSFER_STATUS
  smtp_message_t	msg = IK_LIBESMTP_MESSAGE(s_message);
  const smtp_status_t *	status;
  /* fprintf(stderr, "%s: enter\n", __func__); */
  status = smtp_message_transfer_status(msg);
  if (status) {
    smtp_status_to_scheme_struct(pcb, s_status, status);
    /* fprintf(stderr, "%s: successful leave\n", __func__); */
    return s_status;
  } else {
    /* fprintf(stderr, "%s: erroneous leave\n", __func__); */
    return IK_FALSE;
  }
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_smtp_reverse_path_status (ikptr s_message, ikptr s_status, ikpcb * pcb)
{
#ifdef HAVE_SMTP_REVERSE_PATH_STATUS
  smtp_message_t	msg = IK_LIBESMTP_MESSAGE(s_message);
  const smtp_status_t *	status;
  /* fprintf(stderr, "%s: enter\n", __func__); */
  status = smtp_reverse_path_status(msg);
  if (status) {
    smtp_status_to_scheme_struct(pcb, s_status, status);
    /* fprintf(stderr, "%s: successful leave\n", __func__); */
    return s_status;
  } else {
    /* fprintf(stderr, "%s: erroneous leave\n", __func__); */
    return IK_FALSE;
  }
#else
  feature_failure(__func__);
#endif
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_smtp_message_reset_status (ikptr s_message, ikpcb * pcb)
{
#ifdef HAVE_SMTP_MESSAGE_RESET_STATUS
  smtp_message_t	msg = IK_LIBESMTP_MESSAGE(s_message);
  int			rv;
  rv = smtp_message_reset_status(msg);
  return IK_BOOLEAN_FROM_INT(rv);
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Recipient management.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_smtp_add_recipient (ikptr s_message, ikptr s_mailbox, ikpcb * pcb)
{
#ifdef HAVE_SMTP_ADD_RECIPIENT
  smtp_message_t	msg = IK_LIBESMTP_MESSAGE(s_message);
  char *		mailbox = IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_mailbox);
  smtp_recipient_t	rec;
  rec = smtp_add_recipient(msg, mailbox);
  return (rec)? ika_pointer_alloc(pcb, (long)rec) : IK_FALSE;
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_smtp_enumerate_recipients (ikptr s_message, ikptr s_callback, ikpcb * pcb)
{
#ifdef HAVE_SMTP_ENUMERATE_RECIPIENTS
  smtp_message_t		msg = IK_LIBESMTP_MESSAGE(s_message);
  smtp_enumerate_recipientcb_t	cb  = IK_POINTER_DATA_VOIDP(s_callback);
  int				rv;
  ikptr				sk;
  sk = ik_enter_c_function(pcb);
  {
    rv = smtp_enumerate_recipients(msg, cb, NULL);
  }
  ik_leave_c_function(pcb, sk);
  return IK_BOOLEAN_FROM_INT(rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_smtp_option_require_all_recipients (ikptr s_session, ikptr s_onoff, ikpcb * pcb)
{
#ifdef HAVE_SMTP_OPTION_REQUIRE_ALL_RECIPIENTS
  smtp_session_t	sex	= IK_LIBESMTP_SESSION(s_session);
  int			onoff	= IK_BOOLEAN_TO_INT(s_onoff);
  int			rv;
  rv = smtp_option_require_all_recipients(sex, onoff);
  return IK_BOOLEAN_FROM_INT(rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_smtp_recipient_status (ikptr s_recipient, ikptr s_status, ikpcb * pcb)
{
#ifdef HAVE_SMTP_RECIPIENT_STATUS
  smtp_recipient_t	rec = IK_LIBESMTP_RECIPIENT(s_recipient);
  const smtp_status_t *	status;
  /* fprintf(stderr, "%s: enter\n", __func__); */
  status = smtp_recipient_status(rec);
  if (status) {
    smtp_status_to_scheme_struct(pcb, s_status, status);
    /* fprintf(stderr, "%s: successful leave\n", __func__); */
    return s_status;
  } else {
    /* fprintf(stderr, "%s: erroneous leave\n", __func__); */
    return IK_FALSE;
  }
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_smtp_recipient_check_complete (ikptr s_recipient, ikpcb * pcb)
{
#ifdef HAVE_SMTP_RECIPIENT_CHECK_COMPLETE
  smtp_recipient_t	rec = IK_LIBESMTP_RECIPIENT(s_recipient);
  int			rv;
  rv = smtp_recipient_check_complete(rec);
  return IK_BOOLEAN_FROM_INT(rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_smtp_recipient_reset_status (ikptr s_recipient, ikpcb * pcb)
{
#ifdef HAVE_SMTP_RECIPIENT_RESET_STATUS
  smtp_recipient_t	rec = IK_LIBESMTP_RECIPIENT(s_recipient);
  int			rv;
  rv = smtp_recipient_reset_status(rec);
  return IK_BOOLEAN_FROM_INT(rv);
#else
  feature_failure(__func__);
#endif
}



/** --------------------------------------------------------------------
 ** Headers management.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_smtp_set_header (ikptr s_message,
		      ikptr s_header_name, ikptr s_value_1, ikptr s_value_2,
		      ikpcb * pcb)
{
#ifdef HAVE_SMTP_SET_HEADER
  smtp_message_t	msg = IK_LIBESMTP_MESSAGE(s_message);
  char *	header_name = IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_header_name);
  int		rv;
  /* fprintf(stderr, "%s: header name: %s\n", __func__, header_name); */
  if (0 == strcmp(header_name, "Date"))
    {
      time_t	time = (time_t)ik_integer_to_long(s_value_1);
      rv = smtp_set_header(msg, header_name, &time);
    }
  else if (0 == strcmp(header_name, "Message-Id"))
    {
      char *	value = IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_value_1);
      rv = smtp_set_header(msg, header_name, value);
    }
  else if ((0 == strcmp(header_name, "From")) ||
	   (0 == strcmp(header_name, "Disposition-Notification-To")))
    {
      char *	phrase  = IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_value_1);
      char *	mailbox = IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_value_2);
      rv = smtp_set_header(msg, header_name, phrase, mailbox);
    }
  else if ((0 == strcmp(header_name, "To"))		||
	   (0 == strcmp(header_name, "Cc"))		||
	   (0 == strcmp(header_name, "Bcc"))		||
	   (0 == strcmp(header_name, "Reply-To"))	||
	   (0 == strcmp(header_name, "Sender")))
    {
      char *	phrase  = IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_value_1);
      char *	address = IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_value_2);
      rv = smtp_set_header(msg, header_name, phrase, address);
    }
  else
    {
      char *	value = IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_value_1);
      rv = smtp_set_header(msg, header_name, value);
    }
  return IK_BOOLEAN_FROM_INT(rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_smtp_set_header_option (ikptr s_message,
			     ikptr s_header_name, ikptr s_option,
			     ikpcb * pcb)
{
#ifdef HAVE_SMTP_SET_HEADER_OPTION
  smtp_message_t	msg = IK_LIBESMTP_MESSAGE(s_message);
  char *	header_name = IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_header_name);
  enum header_option	option = ik_integer_to_int(s_option);
  int			rv;
  rv = smtp_set_header_option(msg, header_name, option);
  return IK_BOOLEAN_FROM_INT(rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_smtp_set_resent_headers (ikptr s_message, ikptr s_onoff, ikpcb * pcb)
{
#ifdef HAVE_SMTP_SET_RESENT_HEADERS
  smtp_message_t	msg   = IK_LIBESMTP_MESSAGE(s_message);
  int			onoff = IK_BOOLEAN_TO_INT(s_onoff);
  int			rv;
  rv = smtp_set_resent_headers(msg, onoff);
  return IK_BOOLEAN_FROM_INT(rv);
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Application data.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_smtp_set_application_data (ikptr s_session, ikptr s_data_pointer, ikpcb * pcb)
{
#ifdef HAVE_SMTP_SET_APPLICATION_DATA
  smtp_session_t	sex  = IK_LIBESMTP_SESSION(s_session);
  void *		data = IK_POINTER_DATA_VOIDP(s_data_pointer);
  void *		rv;
  rv = smtp_set_application_data(sex, data);
  return ika_pointer_alloc(pcb, (long)rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_smtp_get_application_data (ikptr s_session, ikpcb * pcb)
{
#ifdef HAVE_SMTP_GET_APPLICATION_DATA
  smtp_session_t	sex  = IK_LIBESMTP_SESSION(s_session);
  void *		rv;
  rv = smtp_get_application_data(sex);
  return ika_pointer_alloc(pcb, (long)rv);
#else
  feature_failure(__func__);
#endif
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_smtp_message_set_application_data (ikptr s_message, ikptr s_data_pointer, ikpcb * pcb)
{
#ifdef HAVE_SMTP_MESSAGE_SET_APPLICATION_DATA
  smtp_message_t	sex  = IK_LIBESMTP_MESSAGE(s_message);
  void *		data = IK_POINTER_DATA_VOIDP(s_data_pointer);
  void *		rv;
  rv = smtp_message_set_application_data(sex, data);
  return ika_pointer_alloc(pcb, (long)rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_smtp_message_get_application_data (ikptr s_message, ikpcb * pcb)
{
#ifdef HAVE_SMTP_MESSAGE_GET_APPLICATION_DATA
  smtp_message_t	sex  = IK_LIBESMTP_MESSAGE(s_message);
  void *		rv;
  rv = smtp_message_get_application_data(sex);
  return ika_pointer_alloc(pcb, (long)rv);
#else
  feature_failure(__func__);
#endif
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_smtp_recipient_set_application_data (ikptr s_recipient, ikptr s_data_pointer, ikpcb * pcb)
{
#ifdef HAVE_SMTP_RECIPIENT_SET_APPLICATION_DATA
  smtp_recipient_t	sex  = IK_LIBESMTP_RECIPIENT(s_recipient);
  void *		data = IK_POINTER_DATA_VOIDP(s_data_pointer);
  void *		rv;
  rv = smtp_recipient_set_application_data(sex, data);
  return ika_pointer_alloc(pcb, (long)rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_smtp_recipient_get_application_data (ikptr s_recipient, ikpcb * pcb)
{
#ifdef HAVE_SMTP_RECIPIENT_GET_APPLICATION_DATA
  smtp_recipient_t	sex  = IK_LIBESMTP_RECIPIENT(s_recipient);
  void *		rv;
  rv = smtp_recipient_get_application_data(sex);
  return ika_pointer_alloc(pcb, (long)rv);
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Auth extension.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_smtp_auth_set_context (ikptr s_session, ikptr s_auth_context, ikpcb * pcb)
{
#ifdef HAVE_SMTP_AUTH_SET_CONTEXT
  smtp_session_t	sex = IK_LIBESMTP_SESSION(s_session);
  auth_context_t	ctx;
  int			rv;
  if (IK_FALSE == s_auth_context)
    ctx = NULL;
  else
    ctx = IK_LIBESMTP_AUTH_CONTEXT(s_auth_context);
  rv = smtp_auth_set_context(sex, ctx);
  return IK_BOOLEAN_FROM_INT(rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_smtp_gsasl_set_context (ikptr s_session, ikptr s_gsasl_context, ikpcb * pcb)
{
#ifdef HAVE_SMTP_GSASL_SET_CONTEXT
  smtp_session_t	sex = IK_LIBESMTP_SESSION(s_session);
  struct Gsasl *	ctx;
  int			rv;
  if (IK_FALSE == s_gsasl_context)
    ctx = NULL;
  else
    ctx = IK_POINTER_DATA_VOIDP(s_gsasl_context);
  rv = smtp_gsasl_set_context(sex, ctx);
  return IK_BOOLEAN_FROM_INT(rv);
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** SMTP StartTLS extension.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_smtp_starttls_enable (ikptr s_session, ikptr s_how, ikpcb * pcb)
{
#ifdef HAVE_SMTP_STARTTLS_ENABLE
  smtp_session_t	sex = IK_LIBESMTP_SESSION(s_session);
  enum starttls_option	how = ik_integer_to_int(s_how);
  int			rv;
  rv = smtp_starttls_enable(sex, how);
  return IK_BOOLEAN_FROM_INT(rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_smtp_starttls_set_ctx (ikptr s_session, ikptr s_ssl_context, ikpcb * pcb)
{
#if ((defined HAVE_SMTP_STARTTLS_SET_CTX) && (defined HAVE_OPENSSL_SSL_H))
  smtp_session_t	sex = IK_LIBESMTP_SESSION(s_session);
  SSL_CTX *		ctx = IK_POINTER_DATA_VOIDP(s_ssl_context);
  int			rv;
  rv = smtp_starttls_set_ctx(sex, ctx);
  return IK_BOOLEAN_FROM_INT(rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_smtp_starttls_set_password_cb (ikptr s_callback, ikpcb * pcb)
{
#ifdef HAVE_SMTP_STARTTLS_SET_PASSWORD_CB
  smtp_starttls_passwordcb_t	cb  = IK_POINTER_DATA_VOIDP(s_callback);
  int				rv;
  rv = smtp_starttls_set_password_cb(cb, NULL);
  return IK_BOOLEAN_FROM_INT(rv);
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** SMTP Deliver By extension.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_smtp_deliverby_set_mode (ikptr s_message, ikptr s_time,
			      ikptr s_by_mode, ikptr s_trace,
			      ikpcb * pcb)
{
#ifdef HAVE_SMTP_DELIVERBY_SET_MODE
  smtp_message_t	msg   = IK_LIBESMTP_MESSAGE(s_message);
  long			time  = ik_integer_to_long(s_time);
  enum by_mode		mode  = ik_integer_to_int(s_by_mode);
  int			trace = ik_integer_to_int(s_trace);
  int			rv;
  rv = smtp_deliverby_set_mode(msg, time, mode, trace);
  return IK_BOOLEAN_FROM_INT(rv);
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** SMTP Deliver Status Notification extension.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_smtp_dsn_set_ret (ikptr s_message, ikptr s_flags, ikpcb * pcb)
{
#ifdef HAVE_SMTP_DSN_SET_RET
  smtp_message_t	msg   = IK_LIBESMTP_MESSAGE(s_message);
  enum ret_flags	flags = ik_integer_to_int(s_flags);
  int			rv;
  rv = smtp_dsn_set_ret(msg, flags);
  return IK_BOOLEAN_FROM_INT(rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_smtp_dsn_set_envid (ikptr s_message, ikptr s_envid, ikpcb * pcb)
{
#ifdef HAVE_SMTP_DSN_SET_ENVID
  smtp_message_t	msg   = IK_LIBESMTP_MESSAGE(s_message);
  const char *		envid = IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_envid);
  int			rv;
  rv = smtp_dsn_set_envid(msg, envid);
  return IK_BOOLEAN_FROM_INT(rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_smtp_dsn_set_notify (ikptr s_recipient, ikptr s_flags, ikpcb * pcb)
{
#ifdef HAVE_SMTP_DSN_SET_NOTIFY
  smtp_recipient_t	rec   = IK_LIBESMTP_RECIPIENT(s_recipient);
  enum notify_flags	flags = ik_integer_to_int(s_flags);
  int			rv;
  rv = smtp_dsn_set_notify(rec, flags);
  return IK_BOOLEAN_FROM_INT(rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_smtp_dsn_set_orcpt (ikptr s_recipient, ikptr s_address_type, ikptr s_address, ikpcb * pcb)
{
#ifdef HAVE_SMTP_DSN_SET_ORCPT
  smtp_recipient_t	rec   = IK_LIBESMTP_RECIPIENT(s_recipient);
  const char *		address_type = \
    IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_address_type);
  const char *		address      = \
    IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_address);
  int			rv;
  rv = smtp_dsn_set_orcpt(rec, address_type, address);
  return IK_BOOLEAN_FROM_INT(rv);
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** SMTP Size extension.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_smtp_size_set_estimate (ikptr s_message, ikptr s_size, ikpcb * pcb)
{
#ifdef HAVE_SMTP_SIZE_SET_ESTIMATE
  smtp_message_t	msg  = IK_LIBESMTP_MESSAGE(s_message);
  unsigned long		size = ik_integer_to_ulong(s_size);
  int			rv;
  rv = smtp_size_set_estimate(msg, size);
  return IK_BOOLEAN_FROM_INT(rv);
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** SMTP 8bit-MIME Transport extension.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_smtp_8bitmime_set_body (ikptr s_message, ikptr s_body, ikpcb * pcb)
{
#ifdef HAVE_SMTP_8bitmime_set_body
  smtp_message_t	msg  = IK_LIBESMTP_MESSAGE(s_message);
  enum e8bitmime_body	body = ik_integer_to_int(s_body);
  int			rv;
  rv = smtp_8bitmime_set_body(msg, body);
  return IK_BOOLEAN_FROM_INT(rv);
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** SMTP Remote Message Queue Starting (ETRN) extension.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_smtp_etrn_add_node (ikptr s_session, ikptr s_option, ikptr s_node, ikpcb * pcb)
{
#ifdef HAVE_SMTP_ETRN_ADD_NODE
  smtp_session_t	sex    = IK_LIBESMTP_SESSION(s_session);
  int			option = ik_integer_to_int(s_option);
  const char *		node   = IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_node);
  smtp_etrn_node_t	rv;
  rv = smtp_etrn_add_node(sex, option, node);
  return (rv)? ika_pointer_alloc(pcb, (long)rv) : IK_FALSE;
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_smtp_etrn_enumerate_nodes (ikptr s_session, ikptr s_callback, ikpcb * pcb)
{
#ifdef HAVE_SMTP_ETRN_ENUMERATE_NODES
  smtp_session_t		sex = IK_LIBESMTP_SESSION(s_session);
  smtp_etrn_enumerate_nodecb_t	cb  = IK_POINTER_DATA_VOIDP(s_callback);
  ikptr				sk;
  int				rv;
  sk = ik_enter_c_function(pcb);
  {
    rv = smtp_etrn_enumerate_nodes(sex, cb, NULL);
  }
  ik_leave_c_function(pcb, sk);
  return IK_BOOLEAN_FROM_INT(rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_smtp_etrn_node_status (ikptr s_etrn_node, ikptr s_status, ikpcb * pcb)
{
#ifdef HAVE_SMTP_ETRN_NODE_STATUS
  smtp_etrn_node_t	node   = IK_LIBESMTP_ETRN_NODE(s_etrn_node);
  const smtp_status_t *	status;
  status = smtp_etrn_node_status(node);
  if (status) {
    smtp_status_to_scheme_struct(pcb, s_status, status);
    return s_status;
  } else
    return IK_FALSE;
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_smtp_etrn_set_application_data (ikptr s_etrn_node, ikptr s_data_pointer, ikpcb * pcb)
{
#ifdef HAVE_SMTP_ETRN_SET_APPLICATION_DATA
  smtp_etrn_node_t	node = IK_LIBESMTP_ETRN_NODE(s_etrn_node);
  void *		data = IK_POINTER_DATA_VOIDP(s_data_pointer);
  void *		rv;
  rv = smtp_etrn_set_application_data(node, data);
  return (rv)? ika_pointer_alloc(pcb, (long)rv) : IK_FALSE;
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_smtp_etrn_get_application_data (ikptr s_etrn_node, ikpcb * pcb)
{
#ifdef HAVE_SMTP_ETRN_GET_APPLICATION_DATA
  smtp_etrn_node_t	node = IK_LIBESMTP_ETRN_NODE(s_etrn_node);
  void *		rv;
  rv = smtp_etrn_get_application_data(node);
  return (rv)? ika_pointer_alloc(pcb, (long)rv) : IK_FALSE;
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Still to be implemented.
 ** ----------------------------------------------------------------- */

#if 0
ikptr
ikrt_libesmtp_template (ikpcb * pcb)
{
#ifdef HAVE_LIBESMTP_TEMPLATE
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}
#endif

/* end of file */
