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

#define ika_integer_from_libesmtp_errcode(PCB,CODE)	IK_FIX(CODE)
               /* ika_integer_from_int((PCB),(CODE)) */

/* Accessors for the fields of the Scheme structure "libesmtp3". */
#define IK_LIBESMTP_LIBESMTP3_POINTER(CONN)		IK_FIELD((CONN),0)
#define IK_LIBESMTP_LIBESMTP3_PATHNAME(CONN)		IK_FIELD((CONN),1)
#define IK_LIBESMTP_CONNECTION(CONN)	\
  IK_POINTER_DATA_VOIDP(IK_LIBESMTP_LIBESMTP3_POINTER(CONN))


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
