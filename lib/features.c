/*
  Part of: Vicare/Libesmtp
  Contents: print platform features library
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


#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <stdio.h>
#include <stdlib.h>


int
main (int argc, const char *const argv[])
{
  printf(";;; -*- coding: utf-8-unix -*-\n\
;;;\n\
;;;Part of: Vicare/Libesmtp\n\
;;;Contents: static platform inspection\n\
;;;Date: Thu Feb 14, 2013\n\
;;;\n\
;;;Abstract\n\
;;;\n\
;;;\n\
;;;\n\
;;;Copyright (C) 2013 Marco Maggi <marco.maggi-ipsu@poste.it>\n\
;;;\n\
;;;This program is free software:  you can redistribute it and/or modify\n\
;;;it under the terms of the  GNU General Public License as published by\n\
;;;the Free Software Foundation, either version 3 of the License, or (at\n\
;;;your option) any later version.\n\
;;;\n\
;;;This program is  distributed in the hope that it  will be useful, but\n\
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of\n\
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU\n\
;;;General Public License for more details.\n\
;;;\n\
;;;You should  have received a  copy of  the GNU General  Public License\n\
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.\n\
;;;\n\
\n\
\n\
#!r6rs\n\
(library (vicare mail libesmtp features)\n\
  (export\n\
    HAVE_SMTP_VERSION\n\
    HAVE_SMTP_CREATE_SESSION\n\
    HAVE_SMTP_ADD_MESSAGE\n\
    HAVE_SMTP_ENUMERATE_MESSAGES\n\
    HAVE_SMTP_SET_SERVER\n\
    HAVE_SMTP_SET_HOSTNAME\n\
    HAVE_SMTP_SET_REVERSE_PATH\n\
    HAVE_SMTP_ADD_RECIPIENT\n\
    HAVE_SMTP_ENUMERATE_RECIPIENTS\n\
    HAVE_SMTP_SET_HEADER\n\
    HAVE_SMTP_SET_HEADER_OPTION\n\
    HAVE_SMTP_SET_RESENT_HEADERS\n\
    HAVE_SMTP_SET_MESSAGECB\n\
    HAVE_SMTP_SET_EVENTCB\n\
    HAVE_SMTP_SET_MONITORCB\n\
    HAVE_SMTP_START_SESSION\n\
    HAVE_SMTP_DESTROY_SESSION\n\
    HAVE_SMTP_MESSAGE_TRANSFER_STATUS\n\
    HAVE_SMTP_REVERSE_PATH_STATUS\n\
    HAVE_SMTP_MESSAGE_RESET_STATUS\n\
    HAVE_SMTP_RECIPIENT_STATUS\n\
    HAVE_SMTP_RECIPIENT_CHECK_COMPLETE\n\
    HAVE_SMTP_RECIPIENT_RESET_STATUS\n\
    HAVE_SMTP_ERRNO\n\
    HAVE_SMTP_STRERROR\n\
    HAVE_SMTP_SET_APPLICATION_DATA\n\
    HAVE_SMTP_GET_APPLICATION_DATA\n\
    HAVE_SMTP_MESSAGE_SET_APPLICATION_DATA\n\
    HAVE_SMTP_MESSAGE_GET_APPLICATION_DATA\n\
    HAVE_SMTP_RECIPIENT_SET_APPLICATION_DATA\n\
    HAVE_SMTP_RECIPIENT_GET_APPLICATION_DATA\n\
    HAVE_SMTP_OPTION_REQUIRE_ALL_RECIPIENTS\n\
    HAVE_SMTP_AUTH_SET_CONTEXT\n\
    HAVE_SMTP_GSASL_SET_CONTEXT\n\
    HAVE_SMTP_SET_TIMEOUT\n\
    HAVE_SMTP_DSN_SET_RET\n\
    HAVE_SMTP_DSN_SET_ENVID\n\
    HAVE_SMTP_DSN_SET_NOTIFY\n\
    HAVE_SMTP_DSN_SET_ORCPT\n\
    HAVE_SMTP_SIZE_SET_ESTIMATE\n\
    HAVE_SMTP_8bitmime_set_body\n\
    HAVE_SMTP_DELIVERBY_SET_MODE\n\
    HAVE_SMTP_STARTTLS_ENABLE\n\
    HAVE_SMTP_STARTTLS_SET_CTX\n\
    HAVE_SMTP_STARTTLS_SET_PASSWORD_CB\n\
    HAVE_SMTP_ETRN_ADD_NODE\n\
    HAVE_SMTP_ETRN_ENUMERATE_NODES\n\
    HAVE_SMTP_ETRN_NODE_STATUS\n\
    HAVE_SMTP_ETRN_SET_APPLICATION_DATA\n\
    HAVE_SMTP_ETRN_GET_APPLICATION_DATA\n\
    HAVE_AUTH_CLIENT_INIT\n\
    HAVE_AUTH_CLIENT_EXIT\n\
    HAVE_AUTH_CREATE_CONTEXT\n\
    HAVE_AUTH_DESTROY_CONTEXT\n\
    HAVE_AUTH_SET_MECHANISM_FLAGS\n\
    HAVE_AUTH_SET_MECHANISM_SSF\n\
    HAVE_AUTH_SET_INTERACT_CB\n\
    HAVE_AUTH_CLIENT_ENABLED\n\
    HAVE_AUTH_SET_MECHANISM\n\
    HAVE_AUTH_MECHANISM_NAME\n\
    HAVE_AUTH_RESPONSE\n\
    HAVE_AUTH_GET_SSF\n\
    HAVE_AUTH_ENCODE\n\
    HAVE_AUTH_DECODE\n\
    HAVE_AUTH_SET_EXTERNAL_ID\n\
    )\n\
  (import (rnrs))\n\
\n\
;;;; helpers\n\
\n\
(define-syntax define-inline-constant\n\
  (syntax-rules ()\n\
    ((_ ?name ?value)\n\
     (define-syntax ?name (identifier-syntax ?value)))))\n\
\n\
\n\
;;;; code\n\n");


printf("(define-inline-constant HAVE_SMTP_VERSION %s)\n",
#ifdef HAVE_SMTP_VERSION
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_SMTP_CREATE_SESSION %s)\n",
#ifdef HAVE_SMTP_CREATE_SESSION
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_SMTP_ADD_MESSAGE %s)\n",
#ifdef HAVE_SMTP_ADD_MESSAGE
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_SMTP_ENUMERATE_MESSAGES %s)\n",
#ifdef HAVE_SMTP_ENUMERATE_MESSAGES
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_SMTP_SET_SERVER %s)\n",
#ifdef HAVE_SMTP_SET_SERVER
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_SMTP_SET_HOSTNAME %s)\n",
#ifdef HAVE_SMTP_SET_HOSTNAME
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_SMTP_SET_REVERSE_PATH %s)\n",
#ifdef HAVE_SMTP_SET_REVERSE_PATH
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_SMTP_ADD_RECIPIENT %s)\n",
#ifdef HAVE_SMTP_ADD_RECIPIENT
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_SMTP_ENUMERATE_RECIPIENTS %s)\n",
#ifdef HAVE_SMTP_ENUMERATE_RECIPIENTS
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_SMTP_SET_HEADER %s)\n",
#ifdef HAVE_SMTP_SET_HEADER
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_SMTP_SET_HEADER_OPTION %s)\n",
#ifdef HAVE_SMTP_SET_HEADER_OPTION
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_SMTP_SET_RESENT_HEADERS %s)\n",
#ifdef HAVE_SMTP_SET_RESENT_HEADERS
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_SMTP_SET_MESSAGECB %s)\n",
#ifdef HAVE_SMTP_SET_MESSAGECB
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_SMTP_SET_EVENTCB %s)\n",
#ifdef HAVE_SMTP_SET_EVENTCB
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_SMTP_SET_MONITORCB %s)\n",
#ifdef HAVE_SMTP_SET_MONITORCB
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_SMTP_START_SESSION %s)\n",
#ifdef HAVE_SMTP_START_SESSION
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_SMTP_DESTROY_SESSION %s)\n",
#ifdef HAVE_SMTP_DESTROY_SESSION
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_SMTP_MESSAGE_TRANSFER_STATUS %s)\n",
#ifdef HAVE_SMTP_MESSAGE_TRANSFER_STATUS
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_SMTP_REVERSE_PATH_STATUS %s)\n",
#ifdef HAVE_SMTP_REVERSE_PATH_STATUS
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_SMTP_MESSAGE_RESET_STATUS %s)\n",
#ifdef HAVE_SMTP_MESSAGE_RESET_STATUS
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_SMTP_RECIPIENT_STATUS %s)\n",
#ifdef HAVE_SMTP_RECIPIENT_STATUS
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_SMTP_RECIPIENT_CHECK_COMPLETE %s)\n",
#ifdef HAVE_SMTP_RECIPIENT_CHECK_COMPLETE
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_SMTP_RECIPIENT_RESET_STATUS %s)\n",
#ifdef HAVE_SMTP_RECIPIENT_RESET_STATUS
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_SMTP_ERRNO %s)\n",
#ifdef HAVE_SMTP_ERRNO
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_SMTP_STRERROR %s)\n",
#ifdef HAVE_SMTP_STRERROR
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_SMTP_SET_APPLICATION_DATA %s)\n",
#ifdef HAVE_SMTP_SET_APPLICATION_DATA
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_SMTP_GET_APPLICATION_DATA %s)\n",
#ifdef HAVE_SMTP_GET_APPLICATION_DATA
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_SMTP_MESSAGE_SET_APPLICATION_DATA %s)\n",
#ifdef HAVE_SMTP_MESSAGE_SET_APPLICATION_DATA
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_SMTP_MESSAGE_GET_APPLICATION_DATA %s)\n",
#ifdef HAVE_SMTP_MESSAGE_GET_APPLICATION_DATA
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_SMTP_RECIPIENT_SET_APPLICATION_DATA %s)\n",
#ifdef HAVE_SMTP_RECIPIENT_SET_APPLICATION_DATA
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_SMTP_RECIPIENT_GET_APPLICATION_DATA %s)\n",
#ifdef HAVE_SMTP_RECIPIENT_GET_APPLICATION_DATA
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_SMTP_OPTION_REQUIRE_ALL_RECIPIENTS %s)\n",
#ifdef HAVE_SMTP_OPTION_REQUIRE_ALL_RECIPIENTS
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_SMTP_AUTH_SET_CONTEXT %s)\n",
#ifdef HAVE_SMTP_AUTH_SET_CONTEXT
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_SMTP_GSASL_SET_CONTEXT %s)\n",
#ifdef HAVE_SMTP_GSASL_SET_CONTEXT
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_SMTP_SET_TIMEOUT %s)\n",
#ifdef HAVE_SMTP_SET_TIMEOUT
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_SMTP_DSN_SET_RET %s)\n",
#ifdef HAVE_SMTP_DSN_SET_RET
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_SMTP_DSN_SET_ENVID %s)\n",
#ifdef HAVE_SMTP_DSN_SET_ENVID
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_SMTP_DSN_SET_NOTIFY %s)\n",
#ifdef HAVE_SMTP_DSN_SET_NOTIFY
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_SMTP_DSN_SET_ORCPT %s)\n",
#ifdef HAVE_SMTP_DSN_SET_ORCPT
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_SMTP_SIZE_SET_ESTIMATE %s)\n",
#ifdef HAVE_SMTP_SIZE_SET_ESTIMATE
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_SMTP_8bitmime_set_body %s)\n",
#ifdef HAVE_SMTP_8bitmime_set_body
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_SMTP_DELIVERBY_SET_MODE %s)\n",
#ifdef HAVE_SMTP_DELIVERBY_SET_MODE
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_SMTP_STARTTLS_ENABLE %s)\n",
#ifdef HAVE_SMTP_STARTTLS_ENABLE
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_SMTP_STARTTLS_SET_CTX %s)\n",
#ifdef HAVE_SMTP_STARTTLS_SET_CTX
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_SMTP_STARTTLS_SET_PASSWORD_CB %s)\n",
#ifdef HAVE_SMTP_STARTTLS_SET_PASSWORD_CB
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_SMTP_ETRN_ADD_NODE %s)\n",
#ifdef HAVE_SMTP_ETRN_ADD_NODE
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_SMTP_ETRN_ENUMERATE_NODES %s)\n",
#ifdef HAVE_SMTP_ETRN_ENUMERATE_NODES
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_SMTP_ETRN_NODE_STATUS %s)\n",
#ifdef HAVE_SMTP_ETRN_NODE_STATUS
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_SMTP_ETRN_SET_APPLICATION_DATA %s)\n",
#ifdef HAVE_SMTP_ETRN_SET_APPLICATION_DATA
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_SMTP_ETRN_GET_APPLICATION_DATA %s)\n",
#ifdef HAVE_SMTP_ETRN_GET_APPLICATION_DATA
  "#t"
#else
  "#f"
#endif
  );

/* ------------------------------------------------------------------ */

printf("(define-inline-constant HAVE_AUTH_CLIENT_INIT %s)\n",
#ifdef HAVE_AUTH_CLIENT_INIT
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_AUTH_CLIENT_EXIT %s)\n",
#ifdef HAVE_AUTH_CLIENT_EXIT
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_AUTH_CREATE_CONTEXT %s)\n",
#ifdef HAVE_AUTH_CREATE_CONTEXT
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_AUTH_DESTROY_CONTEXT %s)\n",
#ifdef HAVE_AUTH_DESTROY_CONTEXT
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_AUTH_SET_MECHANISM_FLAGS %s)\n",
#ifdef HAVE_AUTH_SET_MECHANISM_FLAGS
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_AUTH_SET_MECHANISM_SSF %s)\n",
#ifdef HAVE_AUTH_SET_MECHANISM_SSF
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_AUTH_SET_INTERACT_CB %s)\n",
#ifdef HAVE_AUTH_SET_INTERACT_CB
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_AUTH_CLIENT_ENABLED %s)\n",
#ifdef HAVE_AUTH_CLIENT_ENABLED
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_AUTH_SET_MECHANISM %s)\n",
#ifdef HAVE_AUTH_SET_MECHANISM
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_AUTH_MECHANISM_NAME %s)\n",
#ifdef HAVE_AUTH_MECHANISM_NAME
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_AUTH_RESPONSE %s)\n",
#ifdef HAVE_AUTH_RESPONSE
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_AUTH_GET_SSF %s)\n",
#ifdef HAVE_AUTH_GET_SSF
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_AUTH_ENCODE %s)\n",
#ifdef HAVE_AUTH_ENCODE
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_AUTH_DECODE %s)\n",
#ifdef HAVE_AUTH_DECODE
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_AUTH_SET_EXTERNAL_ID %s)\n",
#ifdef HAVE_AUTH_SET_EXTERNAL_ID
  "#t"
#else
  "#f"
#endif
  );


  printf("\n\
;;;; done\n\
\n\
)\n\
\n\
;;; end of file\n");
  exit(EXIT_SUCCESS);
}

/* end of file */
