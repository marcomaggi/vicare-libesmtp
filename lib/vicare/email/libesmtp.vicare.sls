;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare/Libesmtp
;;;Contents: Libesmtp binding backend
;;;Date: Thu Feb 14, 2013
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2013, 2015, 2017 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!vicare
(library (vicare email libesmtp)
  (options typed-language)
  (foreign-library "vicare-libesmtp")
  (export

    ;; version numbers and strings
    vicare-libesmtp-version-interface-current
    vicare-libesmtp-version-interface-revision
    vicare-libesmtp-version-interface-age
    vicare-libesmtp-version

    smtp-version

    ;; library errors
    smtp-errno
    smtp-strerror

    ;; status data structure
    smtp-status
    make-smtp-status			smtp-status?
    smtp-status-code			set-smtp-status-code!
    smtp-status-text			set-smtp-status-text!
    smtp-status-enh-class		set-smtp-status-enh-class!
    smtp-status-enh-subject		set-smtp-status-enh-subject!
    smtp-status-enh-detail		set-smtp-status-enh-detail!
    smtp-status.vicare-arguments-validation

    ;; session management
    smtp-session
    smtp-session?
    smtp-session?/alive
    smtp-session.vicare-arguments-validation
    smtp-session/alive.vicare-arguments-validation
    false-or-smtp-session.vicare-arguments-validation
    false-or-smtp-session/alive.vicare-arguments-validation

    smtp-create-session
    smtp-destroy-session
    smtp-set-hostname
    smtp-set-server
    smtp-set-timeout
    smtp-set-eventcb
    smtp-set-monitorcb
    smtp-start-session

    ;; message management
    smtp-message
    smtp-message?
    smtp-message?/alive
    smtp-message.vicare-arguments-validation
    smtp-message/alive.vicare-arguments-validation
    false-or-smtp-message.vicare-arguments-validation
    false-or-smtp-message/alive.vicare-arguments-validation

    smtp-add-message
    smtp-enumerate-messages
    smtp-enumerate-messages*
    smtp-set-reverse-path
    smtp-set-messagecb
    smtp-set-message-fp
    smtp-set-message-str
    smtp-message-transfer-status
    smtp-reverse-path-status
    smtp-message-reset-status

    ;; recipient management
    smtp-recipient
    smtp-recipient?
    smtp-recipient?/alive
    smtp-recipient.vicare-arguments-validation
    smtp-recipient/alive.vicare-arguments-validation
    false-or-smtp-recipient.vicare-arguments-validation
    false-or-smtp-recipient/alive.vicare-arguments-validation

    smtp-add-recipient
    smtp-enumerate-recipients
    smtp-enumerate-recipients*
    smtp-option-require-all-recipients
    smtp-recipient-status
    smtp-recipient-check-complete
    smtp-recipient-reset-status

    ;; headers management
    smtp-set-header
    smtp-set-header-option
    smtp-set-resent-headers

    ;; callback makers
    make-smtp-enumerate-messagecb
    make-smtp-enumerate-recipientcb
    make-smtp-messagecb
    make-smtp-eventcb
    make-smtp-monitorcb
    make-smtp-starttls-passwordcb
    make-smtp-etrn-enumerate-nodecb

    ;; constant to symbol conversion
    smtp-event->symbol
    smtp-errno->symbol
    smtp-timeout->symbol
    smtp-cb->symbol
    smtp-hdr->symbol
    smtp-notify->symbol
    smtp-e8bitmime->symbol
    smtp-by->symbol
    smtp-starttls->symbol
    smtp-ret->symbol
    auth->symbol
    auth-plugin->symbol

    ;; application data
    smtp-set-application-data
    smtp-get-application-data
    smtp-message-set-application-data
    smtp-message-get-application-data
    smtp-recipient-set-application-data
    smtp-recipient-get-application-data

    ;; SMTP authentication extension
    smtp-auth-set-context
    smtp-gsasl-set-context

    ;; SMTP StartTLS extension
    smtp-starttls-enable
    smtp-starttls-set-ctx
    smtp-starttls-set-password-cb

    ;; SMTP Deliver By extension
    smtp-deliverby-set-mode

    ;; SMTP Deliver Status Notification extension
    smtp-dsn-set-ret
    smtp-dsn-set-envid
    smtp-dsn-set-notify
    smtp-dsn-set-orcpt

    ;; SMTP Size extension
    smtp-size-set-estimate

    ;; SMTP 8bit-MIME Transport extension
    smtp-8bitmime-set-body

    ;; SMTP Remote Message Queue Starting (ETRN) extension
    smtp-etrn-node
    smtp-etrn-node?
    smtp-etrn-node?/alive
    smtp-etrn-node.vicare-arguments-validation
    smtp-etrn-node/alive.vicare-arguments-validation
    false-or-smtp-etrn-node.vicare-arguments-validation
    false-or-smtp-etrn-node/alive.vicare-arguments-validation

    smtp-etrn-add-node
    smtp-etrn-enumerate-nodes
    smtp-etrn-enumerate-nodes*
    smtp-etrn-node-status
    smtp-etrn-set-application-data
    smtp-etrn-get-application-data

    ;; AUTH module
    auth-context
    auth-context?
    auth-context?/alive
    auth-context.vicare-arguments-validation
    auth-context/alive.vicare-arguments-validation
    false-or-auth-context.vicare-arguments-validation
    false-or-auth-context/alive.vicare-arguments-validation

;;; --------------------------------------------------------------------
;;; still to be implemented

    auth-client-init
    auth-client-exit
    auth-create-context
    auth-destroy-context
    auth-set-mechanism-flags
    auth-set-mechanism-ssf
    auth-set-interact-cb
    auth-client-enabled
    auth-set-mechanism
    auth-mechanism-name
    auth-response
    auth-get-ssf
    auth-encode
    auth-decode
    auth-set-external-id
    )
  (import (vicare (0 4 2017 1 (>= 10)))
    (prefix (vicare system structs) structs::)
    (prefix (vicare email libesmtp unsafe-capi) capi::)
    (vicare arguments validation)
    (vicare arguments general-c-buffers)
    (vicare language-extensions syntaxes)
    (vicare unsafe operations)
    (prefix (vicare ffi) ffi::)
    (prefix (vicare ffi foreign-pointer-wrapper) ffi::)
    (vicare email libesmtp constants))


;;;; arguments validation

(define-argument-validation (smtp-status who obj)
  (smtp-status? obj)
  (assertion-violation who "expected \"smtp-status\" structure as argument" obj))

(define-argument-validation (false-or-smtp-status who obj)
  (or (not obj) (smtp-status? obj))
  (assertion-violation who "expected #f or \"smtp-status\" structure as argument" obj))


;;;; helpers



;;;; version functions

(define (vicare-libesmtp-version-interface-current)
  (capi::vicare-libesmtp-version-interface-current))

(define (vicare-libesmtp-version-interface-revision)
  (capi::vicare-libesmtp-version-interface-revision))

(define (vicare-libesmtp-version-interface-age)
  (capi::vicare-libesmtp-version-interface-age))

(define (vicare-libesmtp-version)
  (ascii->string (capi::vicare-libesmtp-version)))

;;; --------------------------------------------------------------------

(define (smtp-version)
  (define who 'smtp-version)
  (let ((rv (capi::smtp-version)))
    (and rv (ascii->string rv))))


;;;; library errors

(define (smtp-errno)
  (capi::smtp-errno))

(define (smtp-strerror code)
  (define who 'smtp-strerror)
  (with-arguments-validation (who)
      ((signed-int	code))
    (let ((rv (capi::smtp-strerror code)))
      (and rv (ascii->string rv)))))


;;;; data structures

(ffi::define-foreign-pointer-wrapper smtp-session
  (ffi::foreign-destructor	capi::smtp-destroy-session)
  (ffi::collector-struct-type	#f)
  (ffi::collected-struct-type	smtp-message)
  (ffi::collected-struct-type	smtp-etrn-node))

(ffi::define-foreign-pointer-wrapper smtp-message
  (ffi::foreign-destructor	#f)
  (ffi::collector-struct-type	smtp-session)
  (ffi::collected-struct-type	smtp-recipient))

(ffi::define-foreign-pointer-wrapper smtp-recipient
  (ffi::foreign-destructor	#f)
  (ffi::collector-struct-type	smtp-message))

(ffi::define-foreign-pointer-wrapper smtp-etrn-node
  (ffi::foreign-destructor	#f)
  (ffi::collector-struct-type	smtp-session))

(ffi::define-foreign-pointer-wrapper auth-context
  (ffi::foreign-destructor	capi::auth-destroy-context)
  (ffi::collector-struct-type	#f))


;;;; data structures: status

(structs::define-struct smtp-status
  (code
		;Exact  integer in  the  range of  the  C language  type
		;"signed int"; SMTP protocol status code.
   text
		;Scheme string; text from the server.
   enh-class
   enh-subject
   enh-detail
		;Exact  integers in  the range  of the  C language  type
		;"signed int"; RFC 2034 enhanced status code triplet.
   ))

(define (%struct-smtp-status-printer S port sub-printer)
  (define-inline (%display thing)
    (display thing port))
  (define-inline (%write thing)
    (write thing port))
  (%display "#[smtp-status")
  (%display " code=")		(%display ($smtp-status-code        S))
  (%display " text=")		(%write   ($smtp-status-text        S))
  (%display " enh-class=")	(%write   ($smtp-status-enh-class   S))
  (%display " enh-subject=")	(%write   ($smtp-status-enh-subject S))
  (%display " enh-detail=")	(%write   ($smtp-status-enh-detail  S))
  (%display "]"))

(module ()
  (structs::set-struct-type-printer! (type-descriptor smtp-status) %struct-smtp-status-printer))

(define (%make-smtp-status/empty)
  (make-smtp-status #f #f #f #f #f))


;;;; session management

(define (smtp-create-session)
  (let ((rv (capi::smtp-create-session)))
    (and rv (make-smtp-session/owner rv))))

(define (smtp-destroy-session session)
  (define who 'smtp-destroy-session)
  (with-arguments-validation (who)
      ((smtp-session	session))
    ($smtp-session-finalise session)))

;;; --------------------------------------------------------------------

(define (smtp-start-session session)
  (define who 'smtp-start-session)
  (with-arguments-validation (who)
      ((smtp-session/alive	session))
    (capi::smtp-start-session session)))

;;; --------------------------------------------------------------------

(define smtp-set-hostname
  (case-lambda
   ((session)
    (smtp-set-hostname session #f))
   ((session local-hostname)
    ;;Set LOCAL-HOSTNAME  as local  hostname for SESSION;  if successful
    ;;return #t, else return #f.
    ;;
    (define who 'smtp-set-hostname)
    (with-arguments-validation (who)
	((smtp-session/alive		session)
	 (general-c-string/false	local-hostname))
      (with-general-c-strings/false
	  ((hname		local-hostname))
	(capi::smtp-set-hostname session hname))))))

(define (smtp-set-server session remote-server)
  ;;Set  REMOTE-SERVER as  remote server  specification for  SESSION; if
  ;;successful return #t, else return #f.
  ;;
  (define who 'smtp-set-server)
  (with-arguments-validation (who)
      ((smtp-session/alive	session)
       (general-c-string	remote-server))
    (with-general-c-strings
	((rserver	remote-server))
      (capi::smtp-set-server session rserver))))

(define (smtp-set-timeout session which value)
  (define who 'smtp-set-timeout)
  (with-arguments-validation (who)
      ((smtp-session/alive	session)
       (signed-int		which)
       (signed-long		value))
    (capi::smtp-set-timeout session which value)))

(define (smtp-set-eventcb session c-callback)
  ;;Register a callback to notify the application about events.
  ;;
  (define who 'smtp-set-eventcb)
  (with-arguments-validation (who)
      ((smtp-session/alive	session)
       (pointer			c-callback))
    (capi::smtp-set-eventcb session c-callback)))

(define (smtp-set-monitorcb session c-callback headers)
  ;;Set a callback for tracing the SMTP protocol session.
  ;;
  (define who 'smtp-set-monitorcb)
  (with-arguments-validation (who)
      ((smtp-session/alive	session)
       (pointer			c-callback))
    (capi::smtp-set-monitorcb session c-callback headers)))


;;;; message management

(define (smtp-add-message session)
  ;;Build  and  return  a  new  "smtp-message"  instance  associated  to
  ;;SESSION; if an error occurs return #f.
  ;;
  (define who 'smtp-add-message)
  (with-arguments-validation (who)
      ((smtp-session/alive	session))
    (let ((rv (capi::smtp-add-message session)))
      (and rv (make-smtp-message/not-owner rv session)))))

;;; --------------------------------------------------------------------

(define (smtp-enumerate-messages session c-callback)
  ;;For  each "smtp_message_t"  in  the  "smtp_session_t" referenced  by
  ;;SESSION: call the C-CALLBACK function.  Return unspecified values.
  ;;
  ;;C-CALLBACK   must   be    the   return   value   of    a   call   to
  ;;MAKE-SMTP-ENUMERATE-MESSAGECB.
  ;;
  (define who 'smtp-enumerate-messages)
  (with-arguments-validation (who)
      ((smtp-session/alive	session)
       (pointer			c-callback))
    (capi::smtp-enumerate-messages session c-callback)))

(define (smtp-enumerate-messages* session scheme-callback)
  ;;Apply SCHEME-CALLBACK to each  "smtp-message" registered in SESSION;
  ;;return unspecified values.  The order of application is undefined.
  ;;
  ;;The "smtp-message" instances handed  to SCHEME-CALLBACK are the same
  ;;returned by SMTP-ADD-MESSAGE.
  ;;
  (define who 'smtp-enumerate-messages*)
  (with-arguments-validation (who)
      ((smtp-session/alive	session)
       (procedure		scheme-callback))
    (vector-for-each scheme-callback
      ($smtp-session-vector-of-collected-smtp-message session))
    (void)))

;;; --------------------------------------------------------------------

(define smtp-set-reverse-path
  (case-lambda
   ((message)
    (smtp-set-reverse-path message #f))
   ((message mailbox)
    ;;Set  the reverse  path mailbox  for MESSAGE;  this mailbox  is the
    ;;sender address.  If successful return #t, else return #f.
    ;;
    (define who 'smtp-set-reverse-path)
    (with-arguments-validation (who)
	((smtp-message/alive		message)
	 (general-c-string/false	mailbox))
      (with-general-c-strings/false
	  ((mbox		mailbox))
	(capi::smtp-set-reverse-path message mbox))))))

;;; --------------------------------------------------------------------

(define (smtp-set-messagecb message c-callback)
  ;;Set the callback to read the message from an application.
  ;;
  (define who 'smtp-set-messagecb)
  (with-arguments-validation (who)
      ((smtp-message/alive	message)
       (pointer			c-callback))
    (capi::smtp-set-messagecb message c-callback)))

(define (smtp-set-message-fp message file-pointer)
  ;;Select a "FILE *" from which the message will be read.
  ;;
  (define who 'smtp-set-message-fp)
  (with-arguments-validation (who)
      ((smtp-message/alive	message)
       (pointer			file-pointer))
    (capi::smtp-set-message-fp message file-pointer)))

(define (smtp-set-message-str message string)
  ;;Set the message from a general C buffer.
  ;;
  (define who 'smtp-set-message-str)
  (with-arguments-validation (who)
      ((smtp-message/alive	message)
       (general-c-string/false	string))
    (with-general-c-strings/false
	((string.ascii		string))
      (capi::smtp-set-message-str message string.ascii))))

;;; --------------------------------------------------------------------

(define (smtp-message-transfer-status message)
  ;;Return  a  struct  instance  of type  SMTP-STATUS  representing  the
  ;;delivery  status   for  MESSAGE;  if  no   status  informations  are
  ;;available: return #f.
  ;;
  (define who 'smtp-message-transfer-status)
  (with-arguments-validation (who)
      ((smtp-message/alive	message))
    (let ((rv (capi::smtp-message-transfer-status message (%make-smtp-status/empty))))
      (and rv
	   (let ((text ($smtp-status-text rv)))
	     ($set-smtp-status-text! rv (if text
					    (ascii->string text)
					  ""))
	     rv)))))

(define (smtp-reverse-path-status message)
  ;;Retrieve the reverse path status from a previous SMTP session; if no
  ;;status informations are available: return #f.
  ;;
  (define who 'smtp-reverse-path-status)
  (with-arguments-validation (who)
      ((smtp-message/alive	message))
    (let ((rv (capi::smtp-reverse-path-status message (%make-smtp-status/empty))))
      (and rv
	   (let ((text ($smtp-status-text rv)))
	     ($set-smtp-status-text! rv (if text
					    (ascii->string text)
					  ""))
	     rv)))))

;;; --------------------------------------------------------------------

(define (smtp-message-reset-status message)
  ;;Reset  the  message  status  to  the  state  it  would  have  before
  ;;SMTP-START-SESSION is  called for the  first time on  the containing
  ;;session.  If successful return #t, otherwise return #f.
  ;;
  (define who 'smtp-message-reset-status)
  (with-arguments-validation (who)
      ((smtp-message/alive	message))
    (capi::smtp-message-reset-status message)))


;;;; headers management

(define smtp-set-header
  (case-lambda
   ((message header value1)
    (smtp-set-header message header value1 #f))
   ((message header value1 value2)
    ;;Set a header in the message.  If successful return #t, else return
    ;;#f.
    ;;
    (define who 'smtp-set-header)
    (with-arguments-validation (who)
	((smtp-message/alive	message)
	 (string		header))
      (let ((header.bv (string->ascii header)))
	(case header
	  (("Date")
	   (with-arguments-validation (who)
	       ((signed-long	value1))
	     (capi::smtp-set-header message header.bv value1 #f)))
	  (("Message-Id")
	   (with-arguments-validation (who)
	       ((general-c-string	value1))
	     (with-general-c-strings
		 ((value		value1))
	       (capi::smtp-set-header message header.bv value #f))))
	  (("From" "Disposition-Notification-To")
	   (with-arguments-validation (who)
	       ((general-c-string	value1)
		(general-c-string	value2))
	     (with-general-c-strings
		 ((phrase		value1)
		  (mailbox	value2))
	       (capi::smtp-set-header message header.bv phrase mailbox))))
	  (("To" "Cc" "Bcc" "Reply-To" "Sender")
	   (with-arguments-validation (who)
	       ((general-c-string	value1)
		(general-c-string	value2))
	     (with-general-c-strings
		 ((phrase	value1)
		  (address	value2))
	       (capi::smtp-set-header message header.bv phrase address))))
	  (else
	   (with-arguments-validation (who)
	       ((general-c-string	value1))
	     (with-general-c-strings
		 ((value		value1))
	       (string-to-bytevector string->ascii)
	       (capi::smtp-set-header message header.bv value #f))))
	  ))))))

(define (smtp-set-header-option message header option)
  (define who 'smtp-set-header-option)
  (with-arguments-validation (who)
      ((smtp-message/alive	message)
       (general-c-string	header)
       (signed-int		option))
    (with-general-c-strings
	((header.c	header))
      (string-to-bytevector string->ascii)
      (capi::smtp-set-header-option message header.c option))))

(define (smtp-set-resent-headers message onoff)
  (define who 'smtp-set-resent-headers)
  (with-arguments-validation (who)
      ((smtp-message/alive	message))
    (capi::smtp-set-resent-headers message onoff)))


;;;; recipient management

(define (smtp-add-recipient message mailbox)
  ;;Build  and  return a  new  "smtp-recipient"  instance associated  to
  ;;MESSAGE; if an error occurs return #f.
  ;;
  (define who 'smtp-add-recipient)
  (with-arguments-validation (who)
      ((smtp-message/alive	message)
       (general-c-string	mailbox))
    (with-general-c-strings
	((mbox		mailbox))
      (let ((rv (capi::smtp-add-recipient message mbox)))
	(and rv (make-smtp-recipient/not-owner rv message))))))

;;; --------------------------------------------------------------------

(define (smtp-enumerate-recipients message c-callback)
  ;;For each  "smtp_recipient_t" in  the "smtp_message_t"  referenced by
  ;;MESSAGE: call the C-CALLBACK function.  Return unspecified values.
  ;;
  ;;C-CALLBACK   must   be    the   return   value   of    a   call   to
  ;;MAKE-SMTP-ENUMERATE-RECIPIENTCB.
  ;;
  (define who 'smtp-enumerate-recipients)
  (with-arguments-validation (who)
      ((smtp-message/alive	message)
       (pointer			c-callback))
    (capi::smtp-enumerate-recipients message c-callback)))

(define (smtp-enumerate-recipients* message scheme-callback)
  ;;Apply  SCHEME-CALLBACK   to  each  "smtp-recipient"   registered  in
  ;;MESSAGE;  return unspecified  values.  The  order of  application is
  ;;undefined.
  ;;
  ;;The  "smtp-recipient" instances  handed to  SCHEME-CALLBACK are  the
  ;;same returned by SMTP-ADD-RECIPIENT.
  ;;
  (define who 'smtp-enumerate-recipients*)
  (with-arguments-validation (who)
      ((smtp-message/alive	message)
       (procedure		scheme-callback))
    (vector-for-each scheme-callback
      ($smtp-message-vector-of-collected-smtp-recipient message))
    (void)))

(define (smtp-option-require-all-recipients session onoff)
  (define who 'smtp-option-require-all-recipients)
  (with-arguments-validation (who)
      ((smtp-session/alive	session))
    (capi::smtp-option-require-all-recipients session onoff)))

;;; --------------------------------------------------------------------

(define (smtp-recipient-status recipient)
  ;;Return  an  instance  of   SMTP-STATUS  representing  the  recipient
  ;;success/failure status from  a previous SMTP session.   If no status
  ;;informations are available: return #f.
  ;;
  (define who 'smtp-recipient-status)
  (with-arguments-validation (who)
      ((smtp-recipient/alive	recipient))
    (let ((rv (capi::smtp-recipient-status recipient (%make-smtp-status/empty))))
      (and rv
	   (let ((text ($smtp-status-text rv)))
	     ($set-smtp-status-text! rv (if text
					    (ascii->string text)
					  ""))
	     rv)))))

(define (smtp-recipient-check-complete recipient)
  ;;Check whether processing is complete  for the specified recipient of
  ;;the message.  If complete return #t, otherwise return #f.
  ;;
  (define who 'smtp-recipient-check-complete)
  (with-arguments-validation (who)
      ((smtp-recipient/alive	recipient))
    (capi::smtp-recipient-check-complete recipient)))

(define (smtp-recipient-reset-status recipient)
  ;;Reset  the  recipient status  to  the  state  it would  have  before
  ;;SMTP-START-SESSION is  called for the  first time on  the containing
  ;;session.  If successful return #t, otherwise return #f.
  ;;
  (define who 'smtp-recipient-reset-status)
  (with-arguments-validation (who)
      ((smtp-recipient/alive	recipient))
    (capi::smtp-recipient-reset-status recipient)))


;;;; application data

(define (smtp-set-application-data session data-pointer)
  (define who 'smtp-set-application-data)
  (with-arguments-validation (who)
      ((smtp-session/alive	session)
       (pointer			data-pointer))
    (capi::smtp-set-application-data session data-pointer)))

(define (smtp-get-application-data session)
  (define who 'smtp-get-application-data)
  (with-arguments-validation (who)
      ((smtp-session/alive	session))
    (capi::smtp-get-application-data session)))

;;; --------------------------------------------------------------------

(define (smtp-message-set-application-data message data-pointer)
  (define who 'smtp-message-set-application-data)
  (with-arguments-validation (who)
      ((smtp-message/alive	message)
       (pointer			data-pointer))
    (capi::smtp-message-set-application-data message data-pointer)))

(define (smtp-message-get-application-data message)
  (define who 'smtp-message-get-application-data)
  (with-arguments-validation (who)
      ((smtp-message/alive	message))
    (capi::smtp-message-get-application-data message)))

;;; --------------------------------------------------------------------

(define (smtp-recipient-set-application-data recipient data-pointer)
  (define who 'smtp-recipient-set-application-data)
  (with-arguments-validation (who)
      ((smtp-recipient/alive	recipient)
       (pointer			data-pointer))
    (capi::smtp-recipient-set-application-data recipient data-pointer)))

(define (smtp-recipient-get-application-data recipient)
  (define who 'smtp-recipient-get-application-data)
  (with-arguments-validation (who)
      ((smtp-recipient/alive	recipient))
    (capi::smtp-recipient-get-application-data recipient)))


;;;; SMTP authentication extension

(define (smtp-auth-set-context session auth-context)
  (define who 'smtp-auth-set-context)
  (with-arguments-validation (who)
      ((smtp-session/alive		session)
       (false-or-auth-context/alive	auth-context))
    (capi::smtp-auth-set-context session auth-context)))

(define (smtp-gsasl-set-context session gsasl-context)
  (define who 'smtp-gsasl-set-context)
  (with-arguments-validation (who)
      ((smtp-session/alive	session)
       (pointer/false		gsasl-context))
    (capi::smtp-gsasl-set-context session gsasl-context)))


;;;; SMTP StartTLS extension

(define (smtp-starttls-enable session how)
  (define who 'smtp-starttls-enable)
  (with-arguments-validation (who)
      ((smtp-session/alive	session)
       (signed-int		how))
    (capi::smtp-starttls-enable session how)))

(define (smtp-starttls-set-ctx session ssl-context)
  (define who 'smtp-starttls-set-ctx)
  (with-arguments-validation (who)
      ((smtp-session/alive	session)
       (pointer			ssl-context))
    (capi::smtp-starttls-set-ctx session ssl-context)))

(define (smtp-starttls-set-password-cb c-callback)
  (define who 'smtp-starttls-set-password-cb)
  (with-arguments-validation (who)
      ((pointer		c-callback))
    (capi::smtp-starttls-set-password-cb c-callback)))


;;;; SMTP Deliver By extension

(define (smtp-deliverby-set-mode message time by-mode trace)
  (define who 'smtp-deliverby-set-mode)
  (with-arguments-validation (who)
      ((smtp-message/alive	message)
       (signed-long		time)
       (signed-int		by-mode)
       (signed-int		trace))
    (capi::smtp-deliverby-set-mode message time by-mode trace)))


;;;; SMTP Deliver Status Notification extension

(define (smtp-dsn-set-ret message flags)
  (define who 'smtp-dsn-set-ret)
  (with-arguments-validation (who)
      ((smtp-message/alive	message)
       (signed-int		flags))
    (capi::smtp-dsn-set-ret message flags)))

(define (smtp-dsn-set-envid message envelope-identifier)
  (define who 'smtp-dsn-set-envid)
  (with-arguments-validation (who)
      ((smtp-message/alive	message)
       (general-c-string	envelope-identifier))
    (with-general-c-strings
	((envid		envelope-identifier))
      (capi::smtp-dsn-set-envid message envid))))

(define (smtp-dsn-set-notify recipient flags)
  (define who 'smtp-dsn-set-notify)
  (with-arguments-validation (who)
      ((smtp-recipient/alive	recipient)
       (signed-int		flags))
    (capi::smtp-dsn-set-notify recipient flags)))

(define (smtp-dsn-set-orcpt recipient address-type address)
  (define who 'smtp-dsn-set-orcpt)
  (with-arguments-validation (who)
      ((smtp-recipient/alive	recipient)
       (general-c-string	address-type)
       (general-c-string	address))
    (with-general-c-strings
	((address-type^		address-type)
	 (address^		address))
      (capi::smtp-dsn-set-orcpt recipient address-type^ address^))))


;;;; SMTP Size extension

(define (smtp-size-set-estimate message size)
  (define who 'smtp-size-set-estimate)
  (with-arguments-validation (who)
      ((smtp-message/alive	message)
       (unsigned-long		size))
    (capi::smtp-size-set-estimate message size)))


;;;; SMTP 8bit-MIME Transport extension

(define (smtp-8bitmime-set-body message body)
  (define who 'smtp-8bitmime-set-body)
  (with-arguments-validation (who)
      ((smtp-message/alive	message)
       (signed-int		body))
    (capi::smtp-8bitmime-set-body message body)))


;;;; SMTP Remote Message Queue Starting (ETRN) extension

(define (smtp-etrn-add-node session option node)
  (define who 'smtp-etrn-add-node)
  (with-arguments-validation (who)
      ((smtp-session/alive	session)
       (signed-int		option)
       (general-c-string	node))
    (with-general-c-strings
	((node^		node))
      (let ((rv (capi::smtp-etrn-add-node session option node^)))
	(and rv (make-smtp-etrn-node/not-owner rv session))))))

(define (smtp-etrn-enumerate-nodes session c-callback)
  (define who 'smtp-etrn-enumerate-nodes)
  (with-arguments-validation (who)
      ((smtp-session/alive	session)
       (pointer			c-callback))
    (capi::smtp-etrn-enumerate-nodes session c-callback)))

(define (smtp-etrn-enumerate-nodes* session scheme-callback)
  (define who 'smtp-etrn-enumerate-nodes*)
  (with-arguments-validation (who)
      ((smtp-session/alive	session)
       (procedure		scheme-callback))
    (vector-for-each scheme-callback
      ($smtp-session-vector-of-collected-smtp-etrn-node session))))

(define (smtp-etrn-node-status etrn-node)
  (define who 'smtp-etrn-node-status)
  (with-arguments-validation (who)
      ((smtp-etrn-node/alive	etrn-node))
    (let ((rv (capi::smtp-etrn-node-status etrn-node (%make-smtp-status/empty))))
      (and rv
	   (let ((text ($smtp-status-text rv)))
	     ($set-smtp-status-text! rv (if text
					    (ascii->string text)
					  ""))
	     rv)))))

(define (smtp-etrn-set-application-data etrn-node data-pointer)
  (define who 'smtp-etrn-set-application-data)
  (with-arguments-validation (who)
      ((smtp-etrn-node/alive	etrn-node)
       (pointer			data-pointer))
    (capi::smtp-etrn-set-application-data etrn-node data-pointer)))

(define (smtp-etrn-get-application-data etrn-node)
  (define who 'smtp-etrn-get-application-data)
  (with-arguments-validation (who)
      ((smtp-etrn-node/alive	etrn-node))
    (capi::smtp-etrn-get-application-data etrn-node)))


;;;; AUTH module

(define (auth-client-init)
  (define who 'auth-client-init)
  (with-arguments-validation (who)
      ()
    (capi::auth-client-init)))

(define (auth-client-exit)
  (define who 'auth-client-exit)
  (with-arguments-validation (who)
      ()
    (capi::auth-client-exit)))

(define (auth-create-context)
  (define who 'auth-create-context)
  (with-arguments-validation (who)
      ()
    (capi::auth-create-context)))

(define (auth-destroy-context auth-ctx)
  (define who 'auth-destroy-context)
  (with-arguments-validation (who)
      ((auth-context	auth-ctx))
    ($auth-context-finalise auth-ctx)))

(define (auth-set-mechanism-flags)
  (define who 'auth-set-mechanism-flags)
  (with-arguments-validation (who)
      ()
    (capi::auth-set-mechanism-flags)))

(define (auth-set-mechanism-ssf)
  (define who 'auth-set-mechanism-ssf)
  (with-arguments-validation (who)
      ()
    (capi::auth-set-mechanism-ssf)))

(define (auth-set-interact-cb)
  (define who 'auth-set-interact-cb)
  (with-arguments-validation (who)
      ()
    (capi::auth-set-interact-cb)))

(define (auth-client-enabled)
  (define who 'auth-client-enabled)
  (with-arguments-validation (who)
      ()
    (capi::auth-client-enabled)))

(define (auth-set-mechanism)
  (define who 'auth-set-mechanism)
  (with-arguments-validation (who)
      ()
    (capi::auth-set-mechanism)))

(define (auth-mechanism-name)
  (define who 'auth-mechanism-name)
  (with-arguments-validation (who)
      ()
    (capi::auth-mechanism-name)))

(define (auth-response)
  (define who 'auth-response)
  (with-arguments-validation (who)
      ()
    (capi::auth-response)))

(define (auth-get-ssf)
  (define who 'auth-get-ssf)
  (with-arguments-validation (who)
      ()
    (capi::auth-get-ssf)))

(define (auth-encode)
  (define who 'auth-encode)
  (with-arguments-validation (who)
      ()
    (capi::auth-encode)))

(define (auth-decode)
  (define who 'auth-decode)
  (with-arguments-validation (who)
      ()
    (capi::auth-decode)))

(define (auth-set-external-id)
  (define who 'auth-set-external-id)
  (with-arguments-validation (who)
      ()
    (capi::auth-set-external-id)))


;;;; callback makers

(define make-smtp-enumerate-messagecb
  ;; void (*smtp_enumerate_messagecb_t)
  ;;		(smtp_message_t message,
  ;;		 void *arg);
  (let ((maker (ffi::make-c-callback-maker 'void '(pointer pointer))))
    (lambda (user-scheme-callback)
      (maker (lambda (message-pointer custom-data)
	       (guard (E (else
			  #;(pretty-print E (current-error-port))
			  (void)))
		 (user-scheme-callback (make-smtp-message/not-owner message-pointer #f))
		 (void)))))))

(define make-smtp-enumerate-recipientcb
  ;; void (*smtp_enumerate_recipientcb_t)
  ;;		(smtp_recipient_t recipient,
  ;;		 const char *mailbox,
  ;;		 void *arg);
  (let ((maker (ffi::make-c-callback-maker 'void '(pointer pointer pointer))))
    (lambda (user-scheme-callback)
      (maker (lambda (recipient-pointer mailbox custom-data)
	       (guard (E (else
			  #;(pretty-print E (current-error-port))
			  (void)))
		 (user-scheme-callback (make-smtp-recipient/not-owner recipient-pointer #f)
				       mailbox)
		 (void)))))))

(define make-smtp-messagecb
  ;; const char *(*smtp_messagecb_t)
  ;;		(void **ctx,
  ;;		 int *len,
  ;;		 void *arg);
  (let ((maker (ffi::make-c-callback-maker 'pointer '(pointer pointer pointer))))
    (lambda (user-scheme-callback)
      (maker (lambda (optional-buffer-pointer len-pointer unused-custom-data)
	       (guard (E (else
			  #;(pretty-print E (current-error-port))
			  (null-pointer)))
		 (user-scheme-callback optional-buffer-pointer len-pointer)))))))

(define make-smtp-eventcb
  ;; void (*smtp_eventcb_t)
  ;;		(smtp_session_t session,
  ;;		 int event_no,
  ;;		 void *arg,
  ;;		 ...);
  (let ((maker (ffi::make-c-callback-maker 'void '(pointer signed-int pointer))))
    (lambda (user-scheme-callback)
      (maker (lambda (session-pointer event-no custom-data)
	       (guard (E (else
			  #;(pretty-print E (current-error-port))
			  (void)))
		 (user-scheme-callback (make-smtp-session/not-owner session-pointer)
				       event-no)
		 (void)))))))

(define make-smtp-monitorcb
  ;; void (*smtp_monitorcb_t)
  ;;		(const char *buf,
  ;;		 int buflen,
  ;;		 int writing,
  ;;		 void *arg);
  (let ((maker (ffi::make-c-callback-maker 'void '(pointer signed-int signed-int pointer))))
    (lambda (user-scheme-callback)
      (maker (lambda (buf.ptr buf.len writing custom-data)
	       (guard (E (else
			  #;(pretty-print E (current-error-port))
			  (void)))
		 (user-scheme-callback buf.ptr buf.len writing)
		 (void)))))))

(define make-smtp-etrn-enumerate-nodecb
  ;; void (*smtp_etrn_enumerate_nodecb_t)
  ;;		(smtp_etrn_node_t node,
  ;;		 int option,
  ;;		 const char *domain,
  ;;		 void *arg);
  (let ((maker (ffi::make-c-callback-maker 'void '(pointer signed-int pointer pointer))))
    (lambda (user-scheme-callback)
      (maker (lambda (node-pointer option domain custom-data)
	       (guard (E (else
			  #;(pretty-print E (current-error-port))
			  (void)))
		 (user-scheme-callback (make-smtp-etrn-node/not-owner node-pointer #f)
				       option
				       domain)
		 (void)))))))

;;; --------------------------------------------------------------------

(define make-auth-interact
  ;; int (*auth_interact_t)
  ;;		(auth_client_request_t request,
  ;;		 char **result,
  ;;		 int fields,
  ;;		 void *arg);
  (let ((maker (ffi::make-c-callback-maker 'signed-int
					  '(pointer pointer signed-int pointer))))
    (lambda (user-scheme-callback)
      (maker (lambda (request-pointer result-opinter fields custom-data)
	       (guard (E (else
			  #;(pretty-print E (current-error-port))
			  0))
		 (user-scheme-callback )))))))

(define make-auth-response
  ;; const char *(*auth_response_t)
  ;;		(void *ctx,
  ;;		 const char *challenge,
  ;;		 int *len,
  ;;		 auth_interact_t interact,
  ;;		 void *arg);
  (let ((maker (ffi::make-c-callback-maker 'pointer '(pointer pointer pointer pointer pointer))))
    (lambda (user-scheme-callback)
      (maker (lambda (context-pointer challenge length interact custom-data)
	       (guard (E (else
			  #;(pretty-print E (current-error-port))
			  (null-pointer)))
		 (user-scheme-callback )))))))

(define make-auth-recode
  ;; int (*auth_recode_t)
  ;;		(void *ctx,
  ;;		 char **dstbuf,
  ;;		 int *dstlen,
  ;;		 const char *srcbuf,
  ;;		 int srclen);
  (let ((maker (ffi::make-c-callback-maker 'signed-int
					  '(pointer pointer pointer pointer signed-int))))
    (lambda (user-scheme-callback)
      (maker (lambda (context-pointer dst.ptr.ptr dst.len.ptr src.ptr src.len)
	       (guard (E (else
			  #;(pretty-print E (current-error-port))
			  0))
		 (user-scheme-callback context-pointer
				       dst.ptr.ptr dst.len.ptr
				       src.ptr src.len)))))))

;;; --------------------------------------------------------------------

(define make-smtp-starttls-passwordcb
  ;; int (*smtp_starttls_passwordcb_t)
  ;;		(char *buf,
  ;;		 int buflen,
  ;;		 int rwflag,
  ;;		 void *arg);
  (let ((maker (ffi::make-c-callback-maker 'signed-int
					  '(pointer signed-int signed-int pointer))))
    (lambda (user-scheme-callback)
      (maker (lambda (buf.ptr buf.len rwflag custom-data)
	       (guard (E (else
			  #;(pretty-print E (current-error-port))
			  0))
		 (user-scheme-callback buf.ptr buf.len (if rwflag #t #f))))))))


;;;; constant to symbol conversion

(define-exact-integer->symbol-function smtp-event->symbol
  (SMTP_EV_CONNECT
   SMTP_EV_MAILSTATUS
   SMTP_EV_RCPTSTATUS
   SMTP_EV_MESSAGEDATA
   SMTP_EV_MESSAGESENT
   SMTP_EV_DISCONNECT
   SMTP_EV_SYNTAXWARNING

   SMTP_EV_ETRNSTATUS

   SMTP_EV_EXTNA_DSN
   SMTP_EV_EXTNA_8BITMIME
   SMTP_EV_EXTNA_STARTTLS
   SMTP_EV_EXTNA_ETRN
   SMTP_EV_EXTNA_CHUNKING
   SMTP_EV_EXTNA_BINARYMIME

   SMTP_EV_DELIVERBY_EXPIRED

   SMTP_EV_WEAK_CIPHER
   SMTP_EV_STARTTLS_OK
   SMTP_EV_INVALID_PEER_CERTIFICATE
   SMTP_EV_NO_PEER_CERTIFICATE
   SMTP_EV_WRONG_PEER_CERTIFICATE
   SMTP_EV_NO_CLIENT_CERTIFICATE
   SMTP_EV_UNUSABLE_CLIENT_CERTIFICATE
   SMTP_EV_UNUSABLE_CA_LIST
   ))

(define-exact-integer->symbol-function smtp-errno->symbol
  (SMTP_ERR_NOTHING_TO_DO
   SMTP_ERR_DROPPED_CONNECTION
   SMTP_ERR_INVALID_RESPONSE_SYNTAX
   SMTP_ERR_STATUS_MISMATCH
   SMTP_ERR_INVALID_RESPONSE_STATUS
   SMTP_ERR_INVAL
   SMTP_ERR_EXTENSION_NOT_AVAILABLE

   SMTP_ERR_EAI_ADDRFAMILY
   SMTP_ERR_EAI_NODATA
   SMTP_ERR_EAI_FAIL
   SMTP_ERR_EAI_AGAIN
   SMTP_ERR_EAI_MEMORY
   SMTP_ERR_EAI_FAMILY
   SMTP_ERR_EAI_BADFLAGS
   SMTP_ERR_EAI_NONAME
   SMTP_ERR_EAI_SERVICE
   SMTP_ERR_EAI_SOCKTYPE

   SMTP_ERR_UNTERMINATED_RESPONSE
   SMTP_ERR_CLIENT_ERROR))

(define-exact-integer->symbol-function smtp-timeout->symbol
  (Timeout_GREETING
   Timeout_ENVELOPE
   Timeout_DATA
   Timeout_TRANSFER
   Timeout_DATA2))

(define-exact-integer->symbol-function smtp-cb->symbol
  (SMTP_CB_READING
   SMTP_CB_WRITING
   SMTP_CB_HEADERS))

(define-exact-integer->symbol-function smtp-hdr->symbol
  (Hdr_OVERRIDE
   Hdr_PROHIBIT))

(define-exact-integer->symbol-function smtp-notify->symbol
  (Notify_NOTSET
   Notify_NEVER
   Notify_SUCCESS
   Notify_FAILURE
   Notify_DELAY))

(define-exact-integer->symbol-function smtp-e8bitmime->symbol
  (E8bitmime_NOTSET
   E8bitmime_7BIT
   E8bitmime_8BITMIME
   E8bitmime_BINARYMIME))

(define-exact-integer->symbol-function smtp-by->symbol
  (By_NOTSET
   By_NOTIFY
   By_RETURN))

(define-exact-integer->symbol-function smtp-starttls->symbol
  (Starttls_DISABLED
   Starttls_ENABLED
   Starttls_REQUIRED))

(define-exact-integer->symbol-function smtp-ret->symbol
  (Ret_NOTSET
   Ret_FULL
   Ret_HDRS))

(define-exact-integer->symbol-function auth->symbol
  (AUTH_USER
   AUTH_REALM
   AUTH_PASS
   AUTH_CLEARTEXT))

(define-exact-integer->symbol-function auth-plugin->symbol
  (AUTH_PLUGIN_PLAIN
   AUTH_PLUGIN_EXTERNAL))


;;;; done

)

;;; end of file
;; Local Variables:
;; eval: (put '%struct-destructor-application 'scheme-indent-function 1)
;; eval: (put 'ffi::define-foreign-pointer-wrapper 'scheme-indent-function 1)
;; End:
