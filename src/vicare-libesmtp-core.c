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

ikptr
ikrt_smtp_version (ikpcb * pcb)
{
#ifdef HAVE_SMTP_VERSION
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}

ikptr
ikrt_smtp_create_session (ikpcb * pcb)
{
#ifdef HAVE_SMTP_CREATE_SESSION
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}

ikptr
ikrt_smtp_add_message (ikpcb * pcb)
{
#ifdef HAVE_SMTP_ADD_MESSAGE
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}

ikptr
ikrt_smtp_enumerate_messages (ikpcb * pcb)
{
#ifdef HAVE_SMTP_ENUMERATE_MESSAGES
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}

ikptr
ikrt_smtp_set_server (ikpcb * pcb)
{
#ifdef HAVE_SMTP_SET_SERVER
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}

ikptr
ikrt_smtp_set_hostname (ikpcb * pcb)
{
#ifdef HAVE_SMTP_SET_HOSTNAME
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}

ikptr
ikrt_smtp_set_reverse_path (ikpcb * pcb)
{
#ifdef HAVE_SMTP_SET_REVERSE_PATH
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}

ikptr
ikrt_smtp_add_recipient (ikpcb * pcb)
{
#ifdef HAVE_SMTP_ADD_RECIPIENT
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}

ikptr
ikrt_smtp_enumerate_recipients (ikpcb * pcb)
{
#ifdef HAVE_SMTP_ENUMERATE_RECIPIENTS
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}

ikptr
ikrt_smtp_set_header (ikpcb * pcb)
{
#ifdef HAVE_SMTP_SET_HEADER
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}

ikptr
ikrt_smtp_set_header_option (ikpcb * pcb)
{
#ifdef HAVE_SMTP_SET_HEADER_OPTION
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}

ikptr
ikrt_smtp_set_resent_headers (ikpcb * pcb)
{
#ifdef HAVE_SMTP_SET_RESENT_HEADERS
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}

ikptr
ikrt_smtp_set_messagecb (ikpcb * pcb)
{
#ifdef HAVE_SMTP_SET_MESSAGECB
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}

ikptr
ikrt_smtp_set_eventcb (ikpcb * pcb)
{
#ifdef HAVE_SMTP_SET_EVENTCB
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}

ikptr
ikrt_smtp_set_monitorcb (ikpcb * pcb)
{
#ifdef HAVE_SMTP_SET_MONITORCB
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}

ikptr
ikrt_smtp_start_session (ikpcb * pcb)
{
#ifdef HAVE_SMTP_START_SESSION
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}

ikptr
ikrt_smtp_destroy_session (ikpcb * pcb)
{
#ifdef HAVE_SMTP_DESTROY_SESSION
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}

ikptr
ikrt_smtp_message_transfer_status (ikpcb * pcb)
{
#ifdef HAVE_SMTP_MESSAGE_TRANSFER_STATUS
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}

ikptr
ikrt_smtp_reverse_path_status (ikpcb * pcb)
{
#ifdef HAVE_SMTP_REVERSE_PATH_STATUS
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}

ikptr
ikrt_smtp_message_reset_status (ikpcb * pcb)
{
#ifdef HAVE_SMTP_MESSAGE_RESET_STATUS
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}

ikptr
ikrt_smtp_recipient_status (ikpcb * pcb)
{
#ifdef HAVE_SMTP_RECIPIENT_STATUS
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}

ikptr
ikrt_smtp_recipient_check_complete (ikpcb * pcb)
{
#ifdef HAVE_SMTP_RECIPIENT_CHECK_COMPLETE
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}

ikptr
ikrt_smtp_recipient_reset_status (ikpcb * pcb)
{
#ifdef HAVE_SMTP_RECIPIENT_RESET_STATUS
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}

ikptr
ikrt_smtp_errno (ikpcb * pcb)
{
#ifdef HAVE_SMTP_ERRNO
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}

ikptr
ikrt_smtp_strerror (ikpcb * pcb)
{
#ifdef HAVE_SMTP_STRERROR
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}

ikptr
ikrt_smtp_set_application_data (ikpcb * pcb)
{
#ifdef HAVE_SMTP_SET_APPLICATION_DATA
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}

ikptr
ikrt_smtp_get_application_data (ikpcb * pcb)
{
#ifdef HAVE_SMTP_GET_APPLICATION_DATA
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}

ikptr
ikrt_smtp_message_set_application_data (ikpcb * pcb)
{
#ifdef HAVE_SMTP_MESSAGE_SET_APPLICATION_DATA
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}

ikptr
ikrt_smtp_message_get_application_data (ikpcb * pcb)
{
#ifdef HAVE_SMTP_MESSAGE_GET_APPLICATION_DATA
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}

ikptr
ikrt_smtp_recipient_set_application_data (ikpcb * pcb)
{
#ifdef HAVE_SMTP_RECIPIENT_SET_APPLICATION_DATA
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}

ikptr
ikrt_smtp_recipient_get_application_data (ikpcb * pcb)
{
#ifdef HAVE_SMTP_RECIPIENT_GET_APPLICATION_DATA
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}

ikptr
ikrt_smtp_option_require_all_recipients (ikpcb * pcb)
{
#ifdef HAVE_SMTP_OPTION_REQUIRE_ALL_RECIPIENTS
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}

ikptr
ikrt_smtp_auth_set_context (ikpcb * pcb)
{
#ifdef HAVE_SMTP_AUTH_SET_CONTEXT
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}

ikptr
ikrt_smtp_set_timeout (ikpcb * pcb)
{
#ifdef HAVE_SMTP_SET_TIMEOUT
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}

ikptr
ikrt_smtp_dsn_set_ret (ikpcb * pcb)
{
#ifdef HAVE_SMTP_DSN_SET_RET
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}

ikptr
ikrt_smtp_dsn_set_envid (ikpcb * pcb)
{
#ifdef HAVE_SMTP_DSN_SET_ENVID
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}

ikptr
ikrt_smtp_dsn_set_notify (ikpcb * pcb)
{
#ifdef HAVE_SMTP_DSN_SET_NOTIFY
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}

ikptr
ikrt_smtp_dsn_set_orcpt (ikpcb * pcb)
{
#ifdef HAVE_SMTP_DSN_SET_ORCPT
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}

ikptr
ikrt_smtp_size_set_estimate (ikpcb * pcb)
{
#ifdef HAVE_SMTP_SIZE_SET_ESTIMATE
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}

ikptr
ikrt_smtp_8bitmime_set_body (ikpcb * pcb)
{
#ifdef HAVE_SMTP_8bitmime_set_body
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}

ikptr
ikrt_smtp_deliverby_set_mode (ikpcb * pcb)
{
#ifdef HAVE_SMTP_DELIVERBY_SET_MODE
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}

ikptr
ikrt_smtp_starttls_enable (ikpcb * pcb)
{
#ifdef HAVE_SMTP_STARTTLS_ENABLE
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}

ikptr
ikrt_smtp_starttls_set_ctx (ikpcb * pcb)
{
#ifdef HAVE_SMTP_STARTTLS_SET_CTX
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}

ikptr
ikrt_smtp_starttls_set_password_cb (ikpcb * pcb)
{
#ifdef HAVE_SMTP_STARTTLS_SET_PASSWORD_CB
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}

ikptr
ikrt_smtp_etrn_add_node (ikpcb * pcb)
{
#ifdef HAVE_SMTP_ETRN_ADD_NODE
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}

ikptr
ikrt_smtp_etrn_enumerate_nodes (ikpcb * pcb)
{
#ifdef HAVE_SMTP_ETRN_ENUMERATE_NODES
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}

ikptr
ikrt_smtp_etrn_node_status (ikpcb * pcb)
{
#ifdef HAVE_SMTP_ETRN_NODE_STATUS
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}

ikptr
ikrt_smtp_etrn_set_application_data (ikpcb * pcb)
{
#ifdef HAVE_SMTP_ETRN_SET_APPLICATION_DATA
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}

ikptr
ikrt_smtp_etrn_get_application_data (ikpcb * pcb)
{
#ifdef HAVE_SMTP_ETRN_GET_APPLICATION_DATA
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}

/* end of file */
