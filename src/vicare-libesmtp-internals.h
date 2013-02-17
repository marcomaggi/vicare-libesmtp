/*
  Part of: Vicare/Libesmtp
  Contents: internal header file
  Date: Thu Feb 14, 2013

  Abstract



  Copyright (C) 2013 Marco Maggi <marco.maggi-ipsu@poste.it>

  This program is  free software: you can redistribute  it and/or modify
  it under the  terms of the GNU General Public  License as published by
  the Free Software Foundation, either version  3 of the License, or (at
  your option) any later version.

  This program  is distributed in the  hope that it will  be useful, but
  WITHOUT   ANY  WARRANTY;   without  even   the  implied   warranty  of
  MERCHANTABILITY  or FITNESS  FOR A  PARTICULAR PURPOSE.   See the  GNU
  General Public License for more details.

  You should  have received  a copy  of the  GNU General  Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef VICARE_LIBESMTP_INTERNALS_H
#define VICARE_LIBESMTP_INTERNALS_H 1


/** --------------------------------------------------------------------
 ** Headers.
 ** ----------------------------------------------------------------- */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <vicare.h>
#include <string.h>
#include <libesmtp.h>


/** --------------------------------------------------------------------
 ** Handling of Scheme objects.
 ** ----------------------------------------------------------------- */

/* Accessors for the fields of the Scheme structure "smtp_session_t". */
#define IK_LIBESMTP_SESSION_POINTER(SESSION)		IK_FIELD((SESSION),0)
#define IK_LIBESMTP_SESSION_MESSAGES_TABLE(SESSION)	IK_FIELD((SESSION),1)
#define IK_LIBESMTP_SESSION_OWNER(SESSION)		IK_FIELD((SESSION),2)
#define IK_LIBESMTP_SESSION_DESTRUCTOR(SESSION)		IK_FIELD((SESSION),3)
#define IK_LIBESMTP_SESSION(SESSION)	\
  ((smtp_session_t)IK_POINTER_DATA_VOIDP(IK_LIBESMTP_SESSION_POINTER(SESSION)))

/* Accessors for the fields of the Scheme structure "smtp_message_t". */
#define IK_LIBESMTP_MESSAGE_POINTER(MESSAGE)		IK_FIELD((MESSAGE),0)
#define IK_LIBESMTP_MESSAGE_RECIPIENTS_TABLE(MESSAGE)	IK_FIELD((MESSAGE),1)
#define IK_LIBESMTP_MESSAGE_OWNER(MESSAGE)		IK_FIELD((MESSAGE),2)
#define IK_LIBESMTP_MESSAGE_DESTRUCTOR(MESSAGE)		IK_FIELD((MESSAGE),3)
#define IK_LIBESMTP_MESSAGE(MESSAGE)	\
  ((smtp_message_t)IK_POINTER_DATA_VOIDP(IK_LIBESMTP_MESSAGE_POINTER(MESSAGE)))

/* Accessors for the fields of the Scheme structure "smtp_recipient_t". */
#define IK_LIBESMTP_RECIPIENT_POINTER(RECIPIENT)	IK_FIELD((RECIPIENT),0)
#define IK_LIBESMTP_RECIPIENT_OWNER(RECIPIENT)		IK_FIELD((RECIPIENT),1)
#define IK_LIBESMTP_RECIPIENT_DESTRUCTOR(RECIPIENT)	IK_FIELD((RECIPIENT),2)
#define IK_LIBESMTP_RECIPIENT(RECIPIENT)	\
  ((smtp_recipient_t)IK_POINTER_DATA_VOIDP(IK_LIBESMTP_RECIPIENT_POINTER(RECIPIENT)))


/** --------------------------------------------------------------------
 ** Support for missing functions.
 ** ----------------------------------------------------------------- */

static IK_UNUSED void
feature_failure_ (const char * funcname)
{
  ik_abort("called unavailable Libesmtp specific function, %s\n", funcname);
}

#define feature_failure(FN)     { feature_failure_(FN); return IK_VOID; }


/** --------------------------------------------------------------------
 ** Done.
 ** ----------------------------------------------------------------- */


#endif /* VICARE_LIBESMTP_INTERNALS_H */

/* end of file */
