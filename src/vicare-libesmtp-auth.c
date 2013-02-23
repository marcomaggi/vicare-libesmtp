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
 ** Auth module initialisation and finalisation.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_auth_client_init (ikpcb * pcb)
{
#ifdef HAVE_AUTH_CLIENT_INIT
  auth_client_init();
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_auth_client_exit (ikpcb * pcb)
{
#ifdef HAVE_AUTH_CLIENT_EXIT
  auth_client_exit();
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Auth context initialisation and finalisation.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_auth_create_context (ikpcb * pcb)
{
#ifdef HAVE_AUTH_CREATE_CONTEXT
  auth_context_t	ctx;
  ctx = auth_create_context();
  return (ctx)? ika_pointer_alloc(pcb, (long)ctx) : IK_FALSE;
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_auth_destroy_context (ikptr s_auth_context, ikpcb * pcb)
{
#ifdef HAVE_AUTH_DESTROY_CONTEXT
  auth_context_t	ctx = IK_LIBESMTP_AUTH_CONTEXT(s_auth_context);
  if (ctx) {
    int		rv;
    rv = auth_destroy_context(ctx);
    return IK_BOOLEAN_FROM_INT(rv);
  } else
    return IK_TRUE;
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_auth_client_enabled (ikptr s_auth_context, ikpcb * pcb)
{
#ifdef HAVE_AUTH_CLIENT_ENABLED
  auth_context_t	ctx = IK_LIBESMTP_AUTH_CONTEXT(s_auth_context);
  int			rv;
  rv = auth_client_enabled(ctx);
  return IK_BOOLEAN_FROM_INT(rv);
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Auth mechanism.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_auth_set_mechanism (ikptr s_auth_context, ikptr s_name, ikpcb * pcb)
{
#ifdef HAVE_AUTH_SET_MECHANISM
  auth_context_t	ctx  = IK_LIBESMTP_AUTH_CONTEXT(s_auth_context);
  const char *		name = IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_name);
  int			rv;
  rv = auth_set_mechanism(ctx, name);
  return IK_BOOLEAN_FROM_INT(rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_auth_mechanism_name (ikptr s_auth_context, ikpcb * pcb)
{
#ifdef HAVE_AUTH_MECHANISM_NAME
  auth_context_t	ctx   = IK_LIBESMTP_AUTH_CONTEXT(s_auth_context);
  const char *		name;
  name = auth_mechanism_name(ctx);
  return (name)? ika_bytevector_from_cstring(pcb, name) : IK_FALSE;
#else
  feature_failure(__func__);
#endif
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_auth_set_mechanism_flags (ikptr s_auth_context, ikptr s_set, ikptr s_clear, ikpcb * pcb)
{
#ifdef HAVE_AUTH_SET_MECHANISM_FLAGS
  auth_context_t	ctx   = IK_LIBESMTP_AUTH_CONTEXT(s_auth_context);
  unsigned		set   = ik_integer_to_uint(s_set);
  unsigned		clear = ik_integer_to_uint(s_clear);
  int			rv;
  rv = auth_set_mechanism_flags(ctx, set, clear);
  return IK_BOOLEAN_FROM_INT(rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_auth_set_mechanism_ssf (ikptr s_auth_context, ikptr s_min_ssf, ikpcb * pcb)
{
#ifdef HAVE_AUTH_SET_MECHANISM_SSF
  auth_context_t	ctx     = IK_LIBESMTP_AUTH_CONTEXT(s_auth_context);
  int			min_ssf = ik_integer_to_int(s_min_ssf);
  int			rv;
  rv = auth_set_mechanism_ssf(ctx, min_ssf);
  return IK_BOOLEAN_FROM_INT(rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_auth_get_ssf (ikptr s_auth_context, ikpcb * pcb)
{
#ifdef HAVE_AUTH_GET_SSF
  auth_context_t	ctx = IK_LIBESMTP_AUTH_CONTEXT(s_auth_context);
  int			ssf;
  ssf = auth_get_ssf(ctx);
  return ika_integer_from_int(pcb, ssf);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_auth_set_external_id (ikptr s_auth_context, ikptr s_identity, ikpcb * pcb)
{
#ifdef HAVE_AUTH_SET_EXTERNAL_ID
  auth_context_t	ctx = IK_LIBESMTP_AUTH_CONTEXT(s_auth_context);
  const char *		id  = IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_identity);
  int			rv;
  rv = auth_set_external_id(ctx, id);
  return IK_BOOLEAN_FROM_INT(rv);
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Auth interaction.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_auth_set_interact_cb (ikptr s_auth_context, ikptr s_callback, ikpcb * pcb)
{
#ifdef HAVE_AUTH_SET_INTERACT_CB
  auth_context_t	ctx = IK_LIBESMTP_AUTH_CONTEXT(s_auth_context);
  auth_interact_t	cb  = IK_POINTER_DATA_VOIDP(s_callback);
  int			rv;
  rv = auth_set_interact_cb(ctx, cb, NULL);
  return IK_BOOLEAN_FROM_INT(rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_auth_response (ikptr s_auth_context, ikptr s_challenge, ikpcb * pcb)
{
#ifdef HAVE_AUTH_RESPONSE
  auth_context_t	ctx = IK_LIBESMTP_AUTH_CONTEXT(s_auth_context);
  const char *		challenge = IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_challenge);
  int			len;
  const char *		rv;
  rv = auth_response(ctx, challenge, &len);
  return (rv)? ika_bytevector_from_cstring_len(pcb, rv, len) : IK_FALSE;
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Encoding and decoding.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_auth_encode (ikptr s_auth_context, ikptr s_src_buffer, ikpcb * pcb)
{
#ifdef HAVE_AUTH_ENCODE
  auth_context_t	ctx = IK_LIBESMTP_AUTH_CONTEXT(s_auth_context);
  const char *		src_ptr = IK_BYTEVECTOR_DATA_CHARP(s_src_buffer);
  int			src_len = IK_BYTEVECTOR_LENGTH(s_src_buffer);
  char *		dst_ptr;
  int			dst_len;
  auth_encode(&dst_ptr, &dst_len, src_ptr, src_len, ctx);
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_auth_decode (ikptr s_auth_context, ikptr s_src_buffer, ikpcb * pcb)
{
#ifdef HAVE_AUTH_DECODE
  auth_context_t	ctx = IK_LIBESMTP_AUTH_CONTEXT(s_auth_context);
  const char *		src_ptr = IK_BYTEVECTOR_DATA_CHARP(s_src_buffer);
  int			src_len = IK_BYTEVECTOR_LENGTH(s_src_buffer);
  char *		dst_ptr;
  int			dst_len;
  auth_decode(&dst_ptr, &dst_len, src_ptr, src_len, ctx);
  return IK_VOID;
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
