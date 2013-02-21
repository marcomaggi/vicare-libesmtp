/*
  Part of: Vicare/Libesmtp
  Contents: Libesmtp for Vicare
  Date: Thu Feb 21, 2013

  Abstract

	Authentication functions

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

/* ------------------------------------------------------------------ */

ikptr
ikrt_auth_client_init (ikpcb * pcb)
{
#ifdef HAVE_AUTH_CLIENT_INIT
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}

ikptr
ikrt_auth_client_exit (ikpcb * pcb)
{
#ifdef HAVE_AUTH_CLIENT_EXIT
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}

ikptr
ikrt_auth_create_context (ikpcb * pcb)
{
#ifdef HAVE_AUTH_CREATE_CONTEXT
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}

ikptr
ikrt_auth_destroy_context (ikpcb * pcb)
{
#ifdef HAVE_AUTH_DESTROY_CONTEXT
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}

ikptr
ikrt_auth_set_mechanism_flags (ikpcb * pcb)
{
#ifdef HAVE_AUTH_SET_MECHANISM_FLAGS
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}

ikptr
ikrt_auth_set_mechanism_ssf (ikpcb * pcb)
{
#ifdef HAVE_AUTH_SET_MECHANISM_SSF
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}

ikptr
ikrt_auth_set_interact_cb (ikpcb * pcb)
{
#ifdef HAVE_AUTH_SET_INTERACT_CB
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}

ikptr
ikrt_auth_client_enabled (ikpcb * pcb)
{
#ifdef HAVE_AUTH_CLIENT_ENABLED
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}

ikptr
ikrt_auth_set_mechanism (ikpcb * pcb)
{
#ifdef HAVE_AUTH_SET_MECHANISM
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}

ikptr
ikrt_auth_mechanism_name (ikpcb * pcb)
{
#ifdef HAVE_AUTH_MECHANISM_NAME
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}

ikptr
ikrt_auth_response (ikpcb * pcb)
{
#ifdef HAVE_AUTH_RESPONSE
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}

ikptr
ikrt_auth_get_ssf (ikpcb * pcb)
{
#ifdef HAVE_AUTH_GET_SSF
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}

ikptr
ikrt_auth_encode (ikpcb * pcb)
{
#ifdef HAVE_AUTH_ENCODE
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}

ikptr
ikrt_auth_decode (ikpcb * pcb)
{
#ifdef HAVE_AUTH_DECODE
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}

ikptr
ikrt_auth_set_external_id (ikpcb * pcb)
{
#ifdef HAVE_AUTH_SET_EXTERNAL_ID
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}

/* end of file */
